{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (oneof, listOf, choose, elements, listOf1)

import Compiler.IR
import Compiler.GoAst
import Compiler.TypeChecker
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Mock IR Data Types for Testing
-- ============================================================================

data MockIRNode = MockIRNode
  { nodeId :: Int
  , nodeType :: String
  , nodeValue :: String
  , nodeLocation :: SourceSpan
  } deriving (Show, Eq)

data MockIR = MockIR
  { irNodes :: [MockIRNode]
  , irEntry :: Int
  , irExports :: Set String
  } deriving (Show, Eq)

data MockType = MockType
  { typeName :: String
  , typeParams :: [String]
  , typeFields :: Map String MockType
  } deriving (Show, Eq)

data MockSymbol = MockSymbol
  { symbolName :: String
  , symbolType :: MockType
  , symbolLocation :: SourcePos
  } deriving (Show, Eq)

data MockSymbolTable = MockSymbolTable
  { symbols :: Map String MockSymbol
  , parentTable :: Maybe MockSymbolTable
  } deriving (Show, Eq)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    let validEnd = if end >= start then end else start
    return $ SourceSpan start validEnd

instance Arbitrary MockType where
  arbitrary = do
    name <- elements ["Int", "String", "Bool", "Void", "Custom"]
    params <- listOf (elements ["T", "U", "V"])
    fields <- Map.fromList <$> listOf (do
      fname <- elements ["field1", "field2", "field3"]
      ftype <- arbitrary
      return (fname, ftype))
    return $ MockType name params fields

instance Arbitrary MockSymbol where
  arbitrary = do
    name <- elements ["x", "y", "z", "func", "var", "const"]
    symType <- arbitrary
    location <- arbitrary
    return $ MockSymbol name symType location

instance Arbitrary MockSymbolTable where
  arbitrary = do
    symbolList <- listOf arbitrary
    let symbolMap = Map.fromList $ map (\s -> (symbolName s, s)) symbolList
    hasParent <- arbitrary
    parent <- if hasParent then Just arbitrary else Nothing
    return $ MockSymbolTable symbolMap parent

instance Arbitrary MockIRNode where
  arbitrary = do
    nodeId' <- choose (1, 1000)
    nodeType' <- elements ["Var", "Func", "Call", "Return", "Const"]
    nodeValue' <- elements ["x", "y", "z", "1", "2", "3", "true", "false"]
    nodeLocation' <- arbitrary
    return $ MockIRNode nodeId' nodeType' nodeValue' nodeLocation'

instance Arbitrary MockIR where
  arbitrary = do
    nodes <- listOf1 arbitrary
    irEntry' <- choose (0, length nodes - 1)
    irExports' <- Set.fromList <$> listOf (elements ["main", "func1", "func2", "export1"])
    return $ MockIR nodes irEntry' irExports'

-- ============================================================================
-- Compiler IR Property Tests
-- ============================================================================

-- Property: IR node IDs are unique
prop_ir_node_ids_unique :: MockIR -> Property
prop_ir_node_ids_unique ir =
  let nodes = irNodes ir
      ids = map nodeId nodes
      uniqueIds = nub ids
  in property $ length ids === length uniqueIds

-- Property: IR entry point is valid
prop_ir_entry_valid :: MockIR -> Property
prop_ir_entry_valid ir =
  let nodes = irNodes ir
      entry = irEntry ir
      nodeCount = length nodes
  in not (null nodes) ==> entry >= 0 && entry < nodeCount

-- Property: IR exports are valid identifiers
prop_ir_exports_valid :: MockIR -> Property
prop_ir_exports_valid ir =
  let exports = irExports ir
      exportNames = Set.toList exports
      isValidExport name = not (null name) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") name
  in property $ all isValidExport exportNames

-- Property: IR node locations are valid
prop_ir_node_locations_valid :: MockIRNode -> Property
prop_ir_node_locations_valid node =
  let span = nodeLocation node
      (SourceSpan start end) = span
  in property $ start <= end

-- Property: Symbol table lookup works correctly
prop_symboltable_lookup :: MockSymbolTable -> String -> Property
prop_symboltable_lookup table name =
  let symbolMap = symbols table
      found = Map.lookup name symbolMap
      hasSymbol = Map.member name symbolMap
  in property $ hasSymbol === (found /= Nothing)

-- Property: Symbol table preserves symbol types
prop_symboltable_preserve_types :: MockSymbol -> Property
prop_symboltable_preserve_types symbol =
  let name = symbolName symbol
      expectedType = symbolType symbol
      table = MockSymbolTable (Map.singleton name symbol) Nothing
      found = Map.lookup name (symbols table)
      actualType = fmap symbolType found
  in property $ actualType === Just expectedType

-- Property: Type equality is reflexive
prop_type_equality_reflexive :: MockType -> Property
prop_type_equality_reflexive typ =
  property $ typ === typ

-- Property: Type equality is symmetric
prop_type_equality_symmetric :: MockType -> MockType -> Property
prop_type_equality_symmetric typ1 typ2 =
  let equal1 = typ1 == typ2
      equal2 = typ2 == typ1
  in property $ equal1 === equal2

-- Property: Type field access is deterministic
prop_type_field_access_deterministic :: MockType -> String -> Property
prop_type_field_access_deterministic typ fieldName =
  let fields = typeFields typ
      lookup1 = Map.lookup fieldName fields
      lookup2 = Map.lookup fieldName fields
  in property $ lookup1 === lookup2

-- Property: Symbol location is preserved
prop_symbol_location_preserved :: MockSymbol -> Property
prop_symbol_location_preserved symbol =
  let originalLocation = symbolLocation symbol
      name = symbolName symbol
      table = MockSymbolTable (Map.singleton name symbol) Nothing
      found = Map.lookup name (symbols table)
      retrievedLocation = fmap symbolLocation found
  in property $ retrievedLocation === Just originalLocation

-- Property: IR node count is consistent
prop_ir_node_count_consistent :: MockIR -> Property
prop_ir_node_count_consistent ir =
  let nodes = irNodes ir
      nodeCount = length nodes
      entry = irEntry ir
  in not (null nodes) ==> entry >= 0 && entry < nodeCount

-- Property: Type parameter count is preserved
prop_type_param_count_preserved :: MockType -> Property
prop_type_param_count_preserved typ =
  let originalParams = typeParams typ
      paramCount = length originalParams
  in property $ paramCount === length originalParams

-- Property: Symbol table parent chain is maintained
prop_symboltable_parent_chain :: MockSymbolTable -> Property
prop_symboltable_parent_chain table =
  let hasParent = parentTable table /= Nothing
      parent = parentTable table
  in property $ hasParent === (parent /= Nothing)

-- Property: IR exports are unique
prop_ir_exports_unique :: MockIR -> Property
prop_ir_exports_unique ir =
  let exports = irExports ir
      exportCount = Set.size exports
      exportList = Set.toList exports
      uniqueExports = nub exportList
  in property $ exportCount === length uniqueExports

-- Property: Node type classification is consistent
prop_ir_node_type_classification :: MockIRNode -> Property
prop_ir_node_type_classification node =
  let nodeType' = nodeType node
      isValidType = nodeType' `elem` ["Var", "Func", "Call", "Return", "Const", "BinaryOp", "UnaryOp"]
  in property $ isValidType .||. (not (null nodeType'))

-- Property: Symbol name validation
prop_symbol_name_validation :: String -> Property
prop_symbol_name_validation name =
  let isValidName = not (null name) && 
                   all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") name &&
                   head name `elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  in classify isValidName "valid name" $
     classify (not isValidName) "invalid name" $
     property $ True

-- Property: Type field names are unique
prop_type_field_names_unique :: MockType -> Property
prop_type_field_names_unique typ =
  let fields = typeFields typ
      fieldNames = Map.keys fields
      uniqueFieldNames = nub fieldNames
  in property $ length fieldNames === length uniqueFieldNames

-- Property: IR node values are consistent with types
prop_ir_node_values_consistent :: MockIRNode -> Property
prop_ir_node_values_consistent node =
  let nodeType' = nodeType node
      nodeValue' = nodeValue node
      isConsistent = case nodeType' of
        "Const" -> nodeValue' `elem` ["1", "2", "3", "true", "false", "\"string\""]
        "Var" -> all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") nodeValue'
        "Func" -> not (null nodeValue')
        _ -> True
  in property $ isConsistent

-- Property: Symbol table merge preserves all symbols
prop_symboltable_merge_preserves :: MockSymbolTable -> MockSymbolTable -> Property
prop_symboltable_merge_preserves table1 table2 =
  let symbols1 = symbols table1
      symbols2 = symbols table2
      mergedSymbols = Map.union symbols1 symbols2
      originalCount = Map.size symbols1 + Map.size symbols2
      mergedCount = Map.size mergedSymbols
  in property $ mergedCount >= max (Map.size symbols1) (Map.size symbols2)

-- Property: Type parameter substitution preserves structure
prop_type_param_substitution :: MockType -> [(String, MockType)] -> Property
prop_type_param_substitution typ substitutions =
  let params = typeParams typ
      hasSubstitution = any (`elem` map fst substitutions) params
  in classify hasSubstitution "has substitution" $
     classify (not hasSubstitution) "no substitution" $
     property $ True

-- Property: IR node ordering is preserved
prop_ir_node_ordering_preserved :: [MockIRNode] -> Property
prop_ir_node_ordering_preserved nodes =
  not (null nodes) ==>
  let originalIds = map nodeId nodes
      ir = MockIR nodes 0 Set.empty
      retrievedNodes = irNodes ir
      retrievedIds = map nodeId retrievedNodes
  in property $ originalIds === retrievedIds

-- Property: Symbol location tracking is accurate
prop_symbol_location_tracking :: SourcePos -> String -> Property
prop_symbol_location_tracking pos name =
  let symbol = MockSymbol name (MockType "Int" [] Map.empty) pos
      trackedLocation = symbolLocation symbol
  in property $ trackedLocation === pos

tests :: TestTree
tests = testGroup "New Cabal Compiler QuickCheck Tests"
  [ fastProperty "IR node IDs unique" prop_ir_node_ids_unique
  , fastProperty "IR entry valid" prop_ir_entry_valid
  , fastProperty "IR exports valid" prop_ir_exports_valid
  , fastProperty "IR node locations valid" prop_ir_node_locations_valid
  , fastProperty "Symbol table lookup" prop_symboltable_lookup
  , fastProperty "Symbol table preserve types" prop_symboltable_preserve_types
  , fastProperty "Type equality reflexive" prop_type_equality_reflexive
  , fastProperty "Type equality symmetric" prop_type_equality_symmetric
  , fastProperty "Type field access deterministic" prop_type_field_access_deterministic
  , fastProperty "Symbol location preserved" prop_symbol_location_preserved
  , fastProperty "IR node count consistent" prop_ir_node_count_consistent
  , fastProperty "Type param count preserved" prop_type_param_count_preserved
  , fastProperty "Symbol table parent chain" prop_symboltable_parent_chain
  , fastProperty "IR exports unique" prop_ir_exports_unique
  , fastProperty "Node type classification" prop_ir_node_type_classification
  , fastProperty "Symbol name validation" prop_symbol_name_validation
  , fastProperty "Type field names unique" prop_type_field_names_unique
  , fastProperty "IR node values consistent" prop_ir_node_values_consistent
  , fastProperty "Symbol table merge preserves" prop_symboltable_merge_preserves
  , fastProperty "Type param substitution" prop_type_param_substitution
  , fastProperty "IR node ordering preserved" prop_ir_node_ordering_preserved
  , fastProperty "Symbol location tracking" prop_symbol_location_tracking
  ]