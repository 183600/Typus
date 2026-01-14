{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.SymbolTableAdvancedSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy, find, delete, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- Symbol table types for testing
data SymbolKind = Variable | Function | Type | Parameter | Constant
               deriving (Eq, Ord, Show)

data SymbolType = IntType | BoolType | StringType | FunctionType [SymbolType] SymbolType | CustomType String
               deriving (Eq, Show)

data Symbol = Symbol
  { symbolName :: String
  , symbolKind :: SymbolKind
  , symbolType :: SymbolType
  , symbolScope :: String
  , symbolPosition :: Int
  }
  deriving (Eq, Show)

data SymbolTable = SymbolTable
  { tableSymbols :: Map String Symbol
  , tableScope :: String
  , tableParent :: Maybe SymbolTable
  , tableChildren :: [SymbolTable]
  }
  deriving (Eq, Show)

data SymbolTableError = 
    SymbolAlreadyExists String
  | SymbolNotFound String
  | InvalidScope String
  | TypeMismatch SymbolType SymbolType
  deriving (Eq, Show)

-- Helper generators for symbol table tests
genString :: Gen String
genString = do
  len <- choose (1, 10)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

genSymbolKind :: Gen SymbolKind
genSymbolKind = elements [Variable, Function, Type, Parameter, Constant]

genSymbolType :: Int -> Gen SymbolType
genSymbolType 0 = elements [IntType, BoolType, StringType]
genSymbolType depth = oneof
  [ elements [IntType, BoolType, StringType]
  , do
      name <- genString
      return $ CustomType name
  , do
      numParams <- choose (0, 3) :: Gen Int
      paramTypes <- replicateM numParams (genSymbolType (depth - 1))
      returnType <- genSymbolType (depth - 1)
      return $ FunctionType paramTypes returnType
  ]

genSymbol :: Gen Symbol
genSymbol = do
  name <- genString
  kind <- genSymbolKind
  typ <- genSymbolType 2
  scope <- genString
  position <- choose (1, 100)
  return $ Symbol name kind typ scope position

genSymbolTable :: Int -> Gen SymbolTable
genSymbolTable depth = do
  scope <- genString
  numSymbols <- choose (0, 5)
  symbols <- replicateM numSymbols genSymbol
  let symbolMap = Map.fromList $ map (\s -> (symbolName s, s)) symbols
  
  numChildren <- choose (0, 2) :: Gen Int
  children <- if depth > 0 
              then replicateM numChildren (genSymbolTable (depth - 1))
              else return []
  
  return $ SymbolTable symbolMap scope Nothing children

-- Arbitrary instances
instance Arbitrary SymbolKind where
  arbitrary = genSymbolKind

instance Arbitrary SymbolType where
  arbitrary = genSymbolType 2

instance Arbitrary Symbol where
  arbitrary = genSymbol

instance Arbitrary SymbolTable where
  arbitrary = genSymbolTable 2

-- Test properties for symbol tables

-- Property 1: Symbol lookup is consistent
prop_symbol_lookup_consistent :: SymbolTable -> String -> Bool
prop_symbol_lookup_consistent table name = 
  let lookup1 = lookupSymbol table name
      lookup2 = lookupSymbol table name
  in lookup1 == lookup2

-- Property 2: Symbol insertion preserves existing symbols
prop_symbol_insertion_preserves_existing :: SymbolTable -> Symbol -> Bool
prop_symbol_insertion_preserves_existing table symbol = 
  let existingSymbols = Map.keys (tableSymbols table)
      inserted = insertSymbol table symbol
      preservedSymbols = filter (`elem` existingSymbols) (Map.keys (tableSymbols inserted))
  in length preservedSymbols == length existingSymbols

-- Property 3: Scope resolution respects hierarchy
prop_scope_resolution_respects_hierarchy :: SymbolTable -> String -> Bool
prop_scope_resolution_respects_hierarchy table name = 
  let directLookup = Map.lookup name (tableSymbols table)
      hierarchicalLookup = resolveSymbol table name
  in case directLookup of
       Just _ -> directLookup == hierarchicalLookup
       Nothing -> isJust hierarchicalLookup == hasSymbolInParent table name

-- Property 4: Shadowing works correctly
prop_shadowing_works_correctly :: SymbolTable -> Symbol -> Property
prop_shadowing_works_correctly table symbol = 
  let name = symbolName symbol
      originalSymbol = lookupSymbol table name
      inserted = insertSymbol table symbol
      newSymbol = lookupSymbol inserted name
  in isJust originalSymbol ==> 
     case newSymbol of
       Just sym -> symbolScope sym == tableScope inserted
       Nothing -> False

-- Property 5: Symbol removal maintains integrity
prop_symbol_removal_maintains_integrity :: SymbolTable -> String -> Bool
prop_symbol_removal_maintains_integrity table name = 
  let removed = removeSymbol table name
      symbolsAfterRemoval = Map.keys (tableSymbols removed)
  in not (name `elem` symbolsAfterRemoval)

-- Property 6: Scope traversal respects parent-child relationships
prop_scope_traversal_respects_hierarchy :: SymbolTable -> Bool
prop_scope_traversal_respects_hierarchy table = 
  let allScopes = collectAllScopes table
      hasDuplicates = length allScopes /= length (nub allScopes)
  in not hasDuplicates

-- Property 7: Symbol type checking is consistent
prop_symbol_type_checking_consistent :: Symbol -> SymbolType -> Bool
prop_symbol_type_checking_consistent symbol expectedType = 
  let actualType = symbolType symbol
      isCompatible = checkTypeCompatibility actualType expectedType
  in isCompatible == (actualType == expectedType || isFunctionTypeCompatible actualType expectedType)

-- Property 8: Symbol table merging preserves all symbols
prop_symbol_table_merging_preserves_symbols :: SymbolTable -> SymbolTable -> Bool
prop_symbol_table_merging_preserves_symbols table1 table2 = 
  let merged = mergeSymbolTables table1 table2
      symbols1 = Map.keys (tableSymbols table1)
      symbols2 = Map.keys (tableSymbols table2)
      mergedSymbols = Map.keys (tableSymbols merged)
  in all (`elem` mergedSymbols) symbols1 && 
     all (`elem` mergedSymbols) symbols2

-- Property 9: Symbol table filtering respects criteria
prop_symbol_table_filtering_respects_criteria :: SymbolTable -> SymbolKind -> Bool
prop_symbol_table_filtering_respects_criteria table kind = 
  let filtered = filterByKind table kind
      filteredSymbols = Map.elems (tableSymbols filtered)
  in all (\s -> symbolKind s == kind) filteredSymbols

-- Property 10: Symbol table size calculation is accurate
prop_symbol_table_size_calculation_accurate :: SymbolTable -> Bool
prop_symbol_table_size_calculation_accurate table = 
  let directSize = Map.size (tableSymbols table)
      totalSize = calculateTotalSize table
  in totalSize >= directSize

-- Helper functions for symbol table operations
lookupSymbol :: SymbolTable -> String -> Maybe Symbol
lookupSymbol table name = 
  case Map.lookup name (tableSymbols table) of
    Just symbol -> Just symbol
    Nothing -> case tableParent table of
                 Just parent -> lookupSymbol parent name
                 Nothing -> Nothing

insertSymbol :: SymbolTable -> Symbol -> SymbolTable
insertSymbol table symbol = 
  let newSymbols = Map.insert (symbolName symbol) symbol (tableSymbols table)
  in table { tableSymbols = newSymbols }

removeSymbol :: SymbolTable -> String -> SymbolTable
removeSymbol table name = 
  let newSymbols = Map.delete name (tableSymbols table)
  in table { tableSymbols = newSymbols }

resolveSymbol :: SymbolTable -> String -> Maybe Symbol
resolveSymbol = lookupSymbol

hasSymbolInParent :: SymbolTable -> String -> Bool
hasSymbolInParent table name = 
  case tableParent table of
    Just parent -> isJust (lookupSymbol parent name)
    Nothing -> False

collectAllScopes :: SymbolTable -> [String]
collectAllScopes table = 
  let currentScope = tableScope table
      childScopes = concatMap collectAllScopes (tableChildren table)
  in currentScope : childScopes

checkTypeCompatibility :: SymbolType -> SymbolType -> Bool
checkTypeCompatibility t1 t2 = t1 == t2 || isFunctionTypeCompatible t1 t2

isFunctionTypeCompatible :: SymbolType -> SymbolType -> Bool
isFunctionTypeCompatible (FunctionType params1 ret1) (FunctionType params2 ret2) = 
  length params1 == length params2 && 
  all (uncurry checkTypeCompatibility) (zip params1 params2) && 
  checkTypeCompatibility ret1 ret2
isFunctionTypeCompatible _ _ = False

mergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTables table1 table2 = 
  let mergedSymbols = Map.union (tableSymbols table1) (tableSymbols table2)
      mergedChildren = tableChildren table1 ++ tableChildren table2
  in SymbolTable mergedSymbols (tableScope table1) Nothing mergedChildren

filterByKind :: SymbolTable -> SymbolKind -> SymbolTable
filterByKind table kind = 
  let filteredSymbols = Map.filter (\s -> symbolKind s == kind) (tableSymbols table)
  in table { tableSymbols = filteredSymbols }

calculateTotalSize :: SymbolTable -> Int
calculateTotalSize table = 
  let directSize = Map.size (tableSymbols table)
      childrenSize = sum $ map calculateTotalSize (tableChildren table)
  in directSize + childrenSize

-- Test cases for symbol tables
testSymbolTableAdvanced :: TestTree
testSymbolTableAdvanced = testGroup "Symbol Table Advanced Tests"
  [ testProperties "Symbol Lookup Properties"
    [ ("symbol_lookup_consistent", property prop_symbol_lookup_consistent)
    , ("scope_resolution_respects_hierarchy", property prop_scope_resolution_respects_hierarchy)
    ]
  , testProperties "Symbol Manipulation Properties"
    [ ("symbol_insertion_preserves_existing", property prop_symbol_insertion_preserves_existing)
    , ("shadowing_works_correctly", property prop_shadowing_works_correctly)
    , ("symbol_removal_maintains_integrity", property prop_symbol_removal_maintains_integrity)
    ]
  , testProperties "Scope Management Properties"
    [ ("scope_traversal_respects_hierarchy", property prop_scope_traversal_respects_hierarchy)
    ]
  , testProperties "Type System Properties"
    [ ("symbol_type_checking_consistent", property prop_symbol_type_checking_consistent)
    ]
  , testProperties "Symbol Table Operations Properties"
    [ ("symbol_table_merging_preserves_symbols", property prop_symbol_table_merging_preserves_symbols)
    , ("symbol_table_filtering_respects_criteria", property prop_symbol_table_filtering_respects_criteria)
    , ("symbol_table_size_calculation_accurate", property prop_symbol_table_size_calculation_accurate)
    ]
  , testCase "Basic symbol insertion and lookup" $ do
    let symbol = Symbol "x" Variable IntType "global" 1
    let table = SymbolTable Map.empty "global" Nothing []
    let inserted = insertSymbol table symbol
    let found = lookupSymbol inserted "x"
    assertEqual "Should find inserted symbol" (Just symbol) found
  
  , testCase "Symbol shadowing" $ do
    let parentSymbol = Symbol "x" Variable IntType "global" 1
    let childSymbol = Symbol "x" Variable BoolType "local" 2
    let parentTable = SymbolTable (Map.singleton "x" parentSymbol) "global" Nothing []
    let childTable = SymbolTable Map.empty "local" (Just parentTable) []
    let insertedChild = insertSymbol childTable childSymbol
    let found = lookupSymbol insertedChild "x"
    assertEqual "Should find shadowed symbol" (Just childSymbol) found
  
  , testCase "Symbol removal" $ do
    let symbol = Symbol "x" Variable IntType "global" 1
    let table = SymbolTable (Map.singleton "x" symbol) "global" Nothing []
    let removed = removeSymbol table "x"
    let found = lookupSymbol removed "x"
    assertEqual "Should not find removed symbol" Nothing found
  
  , testCase "Scope resolution" $ do
    let parentSymbol = Symbol "x" Variable IntType "global" 1
    let parentTable = SymbolTable (Map.singleton "x" parentSymbol) "global" Nothing []
    let childTable = SymbolTable Map.empty "local" (Just parentTable) []
    let found = lookupSymbol childTable "x"
    assertEqual "Should find symbol in parent scope" (Just parentSymbol) found
  
  , testCase "Type checking" $ do
    let symbol = Symbol "x" Variable IntType "global" 1
    let isCompatible = checkTypeCompatibility (symbolType symbol) IntType
    assertBool "Should detect compatible types" isCompatible
    
    let isIncompatible = checkTypeCompatibility (symbolType symbol) BoolType
    assertBool "Should detect incompatible types" (not isIncompatible)
  
  , testCase "Symbol table merging" $ do
    let symbol1 = Symbol "x" Variable IntType "scope1" 1
    let symbol2 = Symbol "y" Variable BoolType "scope2" 2
    let table1 = SymbolTable (Map.singleton "x" symbol1) "scope1" Nothing []
    let table2 = SymbolTable (Map.singleton "y" symbol2) "scope2" Nothing []
    let merged = mergeSymbolTables table1 table2
    let foundX = lookupSymbol merged "x"
    let foundY = lookupSymbol merged "y"
    assertEqual "Should find symbol from first table" (Just symbol1) foundX
    assertEqual "Should find symbol from second table" (Just symbol2) foundY
  
  , testCase "Symbol filtering by kind" $ do
    let varSymbol = Symbol "x" Variable IntType "global" 1
    let funcSymbol = Symbol "f" Function (FunctionType [IntType] IntType) "global" 2
    let table = SymbolTable (Map.fromList [("x", varSymbol), ("f", funcSymbol)]) "global" Nothing []
    let filtered = filterByKind table Variable
    let found = lookupSymbol filtered "x"
    let notFound = lookupSymbol filtered "f"
    assertEqual "Should find variable symbol" (Just varSymbol) found
    assertEqual "Should not find function symbol" Nothing notFound
  
  , testCase "Symbol table size calculation" $ do
    let symbol = Symbol "x" Variable IntType "global" 1
    let childTable = SymbolTable (Map.singleton "x" symbol) "child" Nothing []
    let parentTable = SymbolTable Map.empty "parent" Nothing [childTable]
    let totalSize = calculateTotalSize parentTable
    assertEqual "Should calculate total size correctly" 1 totalSize
  ]

-- Export the test
tests :: TestTree
tests = testSymbolTableAdvanced