{-# LANGUAGE CPP #-}

module Test.Unit.SymbolTableQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import TestSupport.ExtendedArbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.))

import Analyzer.SymbolTable
import Analyzer.Types
  ( SymbolInfo(..)
  , SymbolKind(..)
  )
import qualified Dependencies as Dep
import qualified Ownership as Own
import qualified Data.Map.Strict as Map

-- Property: empty symbol table has no symbols
prop_empty_symboltable :: Property
prop_empty_symboltable =
  let symbolTable = Map.empty :: Map.Map String SymbolInfo
  in property $ Map.null symbolTable

-- Property: symbol table with one entry
prop_symboltable_one_entry :: String -> SymbolInfo -> Property
prop_symboltable_one_entry name symbolInfo =
  let symbolTable = Map.singleton name symbolInfo
  in Map.size symbolTable === 1 .&&.
     Map.lookup name symbolTable === Just symbolInfo

-- Property: symbol table with multiple entries
prop_symboltable_multiple_entries :: [(String, SymbolInfo)] -> Property
prop_symboltable_multiple_entries pairs =
  let symbolTable = Map.fromList pairs
  in Map.size symbolTable === length (uniqueKeys pairs)
  where
    uniqueKeys = map fst . Map.toList . Map.fromList

-- Property: symbol table lookup
prop_symboltable_lookup :: [(String, SymbolInfo)] -> String -> Property
prop_symboltable_lookup pairs key =
  let symbolTable = Map.fromList pairs
      result = Map.lookup key symbolTable
  in result === lookup key pairs
  where
    lookup _ [] = Nothing
    lookup k ((k', v):rest)
      | k == k' = Just v
      | otherwise = lookup k rest

-- Property: symbol table insert
prop_symboltable_insert :: [(String, SymbolInfo)] -> String -> SymbolInfo -> Property
prop_symboltable_insert pairs key value =
  let symbolTable = Map.fromList pairs
      newTable = Map.insert key value symbolTable
  in Map.lookup key newTable === Just value .&&.
     Map.size newTable === if Map.member key symbolTable 
                           then Map.size symbolTable 
                           else Map.size symbolTable + 1

-- Property: symbol table delete
prop_symboltable_delete :: [(String, SymbolInfo)] -> String -> Property
prop_symboltable_delete pairs key =
  let symbolTable = Map.fromList pairs
      newTable = Map.delete key symbolTable
      hadKey = Map.member key symbolTable
  in Map.lookup key newTable === Nothing .&&.
     Map.size newTable === if hadKey then Map.size symbolTable - 1 else Map.size symbolTable

-- Property: symbol table member
prop_symboltable_member :: [(String, SymbolInfo)] -> String -> Property
prop_symboltable_member pairs key =
  let symbolTable = Map.fromList pairs
  in Map.member key symbolTable === any ((== key) . fst) pairs

-- Property: symbol table keys
prop_symboltable_keys :: [(String, SymbolInfo)] -> Property
prop_symboltable_keys pairs =
  let symbolTable = Map.fromList pairs
      keys = Map.keys symbolTable
      expectedKeys = uniqueKeys pairs
  in length keys === length expectedKeys .&&.
     property (all (`elem` expectedKeys) keys)
  where
    uniqueKeys = map fst . Map.toList . Map.fromList

-- Property: symbol table values
prop_symboltable_values :: [(String, SymbolInfo)] -> Property
prop_symboltable_values pairs =
  let symbolTable = Map.fromList pairs
      values = Map.elems symbolTable
      expectedValues = map snd (Map.toList (Map.fromList pairs))
  in length values === length expectedValues

-- Property: symbol table union
prop_symboltable_union :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symboltable_union pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      unionTable = Map.union table1 table2
  in Map.size unionTable >= Map.size table1 .&&.
     Map.size unionTable >= Map.size table2 .&&.
     property (all (`Map.member` unionTable) (Map.keys table1)) .&&.
     property (all (`Map.member` unionTable) (Map.keys table2))

-- Property: symbol table intersection
prop_symboltable_intersection :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symboltable_intersection pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      intersectTable = Map.intersection table1 table2
      commonKeys = filter (`Map.member` table2) (Map.keys table1)
  in property $ Map.size intersectTable === length commonKeys .&&.
     all (`Map.member` intersectTable) commonKeys

-- Property: symbol table difference
prop_symboltable_difference :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symboltable_difference pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      diffTable = Map.difference table1 table2
      exclusiveKeys = filter (not . (`Map.member` table2)) (Map.keys table1)
  in property $ Map.size diffTable === length exclusiveKeys .&&.
     all (`Map.member` diffTable) exclusiveKeys

-- Property: symbol table map
prop_symboltable_map :: [(String, SymbolInfo)] -> Property
prop_symboltable_map pairs =
  let symbolTable = Map.fromList pairs
      mappedTable = Map.map (\si -> si { symbolScope = symbolScope si + 1 }) symbolTable
  in property $ Map.size mappedTable === Map.size symbolTable .&&.
     all (\(k, v) -> symbolScope v == symbolScope (Map.findWithDefault undefined k symbolTable) + 1) 
         (Map.toList mappedTable)

-- Property: symbol table filter
prop_symboltable_filter :: [(String, SymbolInfo)] -> Property
prop_symboltable_filter pairs =
  let symbolTable = Map.fromList pairs
      filteredTable = Map.filter (\si -> symbolScope si > 0) symbolTable
      expectedCount = length [si | si <- Map.elems symbolTable, symbolScope si > 0]
  in Map.size filteredTable === expectedCount

-- Property: symbol table with duplicate keys
prop_symboltable_duplicate_keys :: String -> SymbolInfo -> SymbolInfo -> Property
prop_symboltable_duplicate_keys key value1 value2 =
  let table1 = Map.singleton key value1
      table2 = Map.insert key value2 table1
  in Map.size table2 === 1 .&&.
     Map.lookup key table2 === Just value2

-- Property: symbol table with empty key
prop_symboltable_empty_key :: SymbolInfo -> Property
prop_symboltable_empty_key value =
  let table = Map.singleton "" value
  in Map.size table === 1 .&&.
     Map.lookup "" table === Just value

-- Property: symbol table with special character keys
prop_symboltable_special_chars :: SymbolInfo -> Property
prop_symboltable_special_chars value =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      table = Map.singleton specialChars value
  in Map.size table === 1 .&&.
     Map.lookup specialChars table === Just value

-- Property: symbol table with Unicode keys
prop_symboltable_unicode :: SymbolInfo -> Property
prop_symboltable_unicode value =
  let unicode = "测试键名🚀"
      table = Map.singleton unicode value
  in Map.size table === 1 .&&.
     Map.lookup unicode table === Just value

-- Property: symbol table with SymbolKind Variable
prop_symboltable_symbolkind_variable :: String -> Property
prop_symboltable_symbolkind_variable name =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 False False []
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> symbolName si === name
    Nothing -> property False

-- Property: symbol table with SymbolKind Function
prop_symboltable_symbolkind_function :: String -> Property
prop_symboltable_symbolkind_function name =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 False False []
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> symbolName si === name
    Nothing -> property False

-- Property: symbol table with different scopes
prop_symboltable_different_scopes :: String -> [Int] -> Property
prop_symboltable_different_scopes name scopes =
  let symbolInfos = map (\scope -> SymbolInfo name Nothing Nothing scope False False []) scopes
      tables = map (\si -> Map.singleton name si) symbolInfos
  in property $ all (\t -> Map.size t == 1) tables .&&.
     all (\t -> case Map.lookup name t of
                   Just si -> symbolScope si `elem` scopes
                   Nothing -> False) tables

-- Property: symbol table with moved and borrowed flags
prop_symboltable_moved_borrowed :: String -> Bool -> Bool -> Property
prop_symboltable_moved_borrowed name moved borrowed =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 moved borrowed []
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> isMoved si === moved .&&. isBorrowed si === borrowed
    Nothing -> property False

-- Property: symbol table with type information
prop_symboltable_type_info :: String -> Dep.TypeVar -> Property
prop_symboltable_type_info name typeVar =
  let symbolInfo = SymbolInfo name (Just typeVar) Nothing 0 False False []
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> symbolType si === Just typeVar
    Nothing -> property False

-- Property: symbol table with ownership state
prop_symboltable_ownership_state :: String -> Own.OwnershipType -> Property
prop_symboltable_ownership_state name ownershipType =
  let symbolInfo = SymbolInfo name Nothing (Just ownershipType) 0 False False []
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> ownershipState si === Just ownershipType
    Nothing -> property False

-- Property: symbol table with constraints
prop_symboltable_constraints :: String -> [Dep.Constraint] -> Property
prop_symboltable_constraints name constraintList =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 False False constraintList
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> constraints si === constraintList
    Nothing -> property False

-- Property: symbol table with complete symbol info
prop_symboltable_complete :: String -> Dep.TypeVar -> Own.OwnershipType -> Int -> Bool -> Bool -> [Dep.Constraint] -> Property
prop_symboltable_complete name typeVar ownershipType scope moved borrowed constraintList =
  let symbolInfo = SymbolInfo name (Just typeVar) (Just ownershipType) scope moved borrowed constraintList
      table = Map.singleton name symbolInfo
  in case Map.lookup name table of
    Just si -> symbolName si === name .&&.
               symbolType si === Just typeVar .&&.
               ownershipState si === Just ownershipType .&&.
               symbolScope si === scope .&&.
               isMoved si === moved .&&.
               isBorrowed si === borrowed .&&.
               constraints si === constraintList
    Nothing -> property False

-- Property: symbol table fold
prop_symboltable_fold :: [(String, SymbolInfo)] -> Property
prop_symboltable_fold pairs =
  let symbolTable = Map.fromList pairs
      sumScopes = Map.foldl' (\acc si -> acc + symbolScope si) 0 symbolTable
      expectedSum = sum (map (symbolScope . snd) pairs)
  in sumScopes === expectedSum

-- Property: symbol table size with empty list
prop_symboltable_size_empty :: Property
prop_symboltable_size_empty =
  let symbolTable = Map.fromList ([] :: [(String, SymbolInfo)])
  in Map.size symbolTable === 0

-- Property: symbol table size with single entry
prop_symboltable_size_single :: String -> SymbolInfo -> Property
prop_symboltable_size_single name symbolInfo =
  let symbolTable = Map.singleton name symbolInfo
  in property $ Map.size symbolTable === 1

-- Property: symbol table size with multiple entries
prop_symboltable_size_multiple :: [(String, SymbolInfo)] -> Property
prop_symboltable_size_multiple pairs =
  let symbolTable = Map.fromList pairs
  in property $ Map.size symbolTable === length (uniqueKeys pairs)
  where
    uniqueKeys = map fst . Map.toList . Map.fromList

tests :: TestTree
tests = testGroup "SymbolTable QuickCheck tests"
  [ fastProperty "empty symbol table has no symbols" prop_empty_symboltable
  , fastProperty "symbol table with one entry" prop_symboltable_one_entry
  , fastProperty "symbol table with multiple entries" prop_symboltable_multiple_entries
  , fastProperty "symbol table lookup" prop_symboltable_lookup
  , fastProperty "symbol table insert" prop_symboltable_insert
  , fastProperty "symbol table delete" prop_symboltable_delete
  , fastProperty "symbol table member" prop_symboltable_member
  , fastProperty "symbol table keys" prop_symboltable_keys
  , fastProperty "symbol table values" prop_symboltable_values
  , fastProperty "symbol table union" prop_symboltable_union
  , fastProperty "symbol table intersection" prop_symboltable_intersection
  , fastProperty "symbol table difference" prop_symboltable_difference
  , fastProperty "symbol table map" prop_symboltable_map
  , fastProperty "symbol table filter" prop_symboltable_filter
  , fastProperty "symbol table with duplicate keys" prop_symboltable_duplicate_keys
  , fastProperty "symbol table with empty key" prop_symboltable_empty_key
  , fastProperty "symbol table with special character keys" prop_symboltable_special_chars
  , fastProperty "symbol table with Unicode keys" prop_symboltable_unicode
  , fastProperty "symbol table with SymbolKind Variable" prop_symboltable_symbolkind_variable
  , fastProperty "symbol table with SymbolKind Function" prop_symboltable_symbolkind_function
  , fastProperty "symbol table with different scopes" prop_symboltable_different_scopes
  , fastProperty "symbol table with moved and borrowed flags" prop_symboltable_moved_borrowed
  , fastProperty "symbol table with type information" prop_symboltable_type_info
  , fastProperty "symbol table with ownership state" prop_symboltable_ownership_state
  , fastProperty "symbol table with constraints" prop_symboltable_constraints
  , fastProperty "symbol table with complete symbol info" prop_symboltable_complete
  , fastProperty "symbol table fold" prop_symboltable_fold
  , fastProperty "symbol table size with empty list" prop_symboltable_size_empty
  , fastProperty "symbol table size with single entry" prop_symboltable_size_single
  , fastProperty "symbol table size with multiple entries" prop_symboltable_size_multiple
  ]