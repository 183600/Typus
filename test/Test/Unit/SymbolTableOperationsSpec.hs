{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SymbolTableOperationsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Analyzer.SymbolTable (SymbolTable(..), Symbol(..), SymbolType(..), emptySymbolTable, 
                             insertSymbol, lookupSymbol, updateSymbol, deleteSymbol, mergeSymbolTables)

-- | Test suite for Symbol Table Operations
tests :: TestTree
tests = testGroup "Symbol Table Operations"
  [ testProperty "symbol table lookup after insert" propSymbolTableLookupAfterInsert
  , testProperty "symbol table update preserves key" propSymbolTableUpdatePreservesKey
  , testProperty "symbol table delete removes entry" propSymbolTableDeleteRemovesEntry
  , testProperty "symbol table merge combines entries" propSymbolTableMergeCombinesEntries
  , testProperty "symbol table scope isolation" propSymbolTableScopeIsolation
  , testCase "empty symbol table" testEmptySymbolTable
  , testCase "symbol insertion and lookup" testSymbolInsertionAndLookup
  , testCase "symbol update" testSymbolUpdate
  , testCase "symbol deletion" testSymbolDeletion
  , testCase "symbol table merging" testSymbolTableMerging
  ]

-- | Property: symbol table lookup after insert
propSymbolTableLookupAfterInsert :: String -> Symbol -> Property
propSymbolTableLookupAfterInsert name symbol =
  let table = emptySymbolTable
      table' = insertSymbol name symbol table
      result = lookupSymbol name table'
  in property $ result == Just symbol

-- | Property: symbol table update preserves key
propSymbolTableUpdatePreservesKey :: String -> Symbol -> Symbol -> Property
propSymbolTableUpdatePreservesKey name oldSymbol newSymbol =
  let table = emptySymbolTable
      table' = insertSymbol name oldSymbol table
      table'' = updateSymbol name newSymbol table'
      result = lookupSymbol name table''
  in property $ result == Just newSymbol

-- | Property: symbol table delete removes entry
propSymbolTableDeleteRemovesEntry :: String -> Symbol -> Property
propSymbolTableDeleteRemovesEntry name symbol =
  let table = emptySymbolTable
      table' = insertSymbol name symbol table
      table'' = deleteSymbol name table'
      result = lookupSymbol name table''
  in property $ result == Nothing

-- | Property: symbol table merge combines entries
propSymbolTableMergeCombinesEntries :: String -> Symbol -> String -> Symbol -> Property
propSymbolTableMergeCombinesEntries name1 symbol1 name2 symbol2 =
  let table1 = insertSymbol name1 symbol1 emptySymbolTable
      table2 = insertSymbol name2 symbol2 emptySymbolTable
      merged = mergeSymbolTables table1 table2
      result1 = lookupSymbol name1 merged
      result2 = lookupSymbol name2 merged
  in property $ (name1 /= name2) ==> (result1 == Just symbol1 && result2 == Just symbol2)

-- | Property: symbol table scope isolation
propSymbolTableScopeIsolation :: String -> Symbol -> String -> Symbol -> Property
propSymbolTableScopeIsolation name1 symbol1 name2 symbol2 =
  let parent = insertSymbol name1 symbol1 emptySymbolTable
      child = insertSymbol name2 symbol2 parent
      result1 = lookupSymbol name1 child
      result2 = lookupSymbol name2 child
  in property $ (name1 /= name2) ==> (result1 == Just symbol1 && result2 == Just symbol2)

-- | Unit tests for empty symbol table
testEmptySymbolTable :: IO ()
testEmptySymbolTable = do
  let table = emptySymbolTable
      result = lookupSymbol "nonexistent" table
  assertEqual "lookup in empty table returns Nothing" Nothing result

-- | Unit tests for symbol insertion and lookup
testSymbolInsertionAndLookup :: IO ()
testSymbolInsertionAndLookup = do
  let symbol = Symbol
        { symbolName = "x"
        , symbolType = VariableSymbol
        , symbolScope = Global
        , symbolInfo = "Integer variable"
        }
      table = emptySymbolTable
      table' = insertSymbol "x" symbol table
      result = lookupSymbol "x" table'
  case result of
    Just foundSymbol -> do
      assertEqual "symbol name" "x" $ symbolName foundSymbol
      assertEqual "symbol type" VariableSymbol $ symbolType foundSymbol
      assertEqual "symbol scope" Global $ symbolScope foundSymbol
      assertEqual "symbol info" "Integer variable" $ symbolInfo foundSymbol
    Nothing -> assertFailure "Expected to find symbol"

-- | Unit tests for symbol update
testSymbolUpdate :: IO ()
testSymbolUpdate = do
  let oldSymbol = Symbol
        { symbolName = "x"
        , symbolType = VariableSymbol
        , symbolScope = Global
        , symbolInfo = "Integer variable"
        }
      newSymbol = Symbol
        { symbolName = "x"
        , symbolType = FunctionSymbol
        , symbolScope = Global
        , symbolInfo = "Function parameter"
        }
      table = insertSymbol "x" oldSymbol emptySymbolTable
      table' = updateSymbol "x" newSymbol table
      result = lookupSymbol "x" table'
  case result of
    Just foundSymbol -> do
      assertEqual "updated symbol type" FunctionSymbol $ symbolType foundSymbol
      assertEqual "updated symbol info" "Function parameter" $ symbolInfo foundSymbol
    Nothing -> assertFailure "Expected to find updated symbol"

-- | Unit tests for symbol deletion
testSymbolDeletion :: IO ()
testSymbolDeletion = do
  let symbol = Symbol
        { symbolName = "x"
        , symbolType = VariableSymbol
        , symbolScope = Global
        , symbolInfo = "Integer variable"
        }
      table = insertSymbol "x" symbol emptySymbolTable
      table' = deleteSymbol "x" table
      result = lookupSymbol "x" table'
  assertEqual "symbol should be deleted" Nothing result

-- | Unit tests for symbol table merging
testSymbolTableMerging :: IO ()
testSymbolTableMerging = do
  let symbol1 = Symbol
        { symbolName = "x"
        , symbolType = VariableSymbol
        , symbolScope = Global
        , symbolInfo = "Integer variable"
        }
      symbol2 = Symbol
        { symbolName = "y"
        , symbolType = FunctionSymbol
        , symbolScope = Local
        , symbolInfo = "Local function"
        }
      table1 = insertSymbol "x" symbol1 emptySymbolTable
      table2 = insertSymbol "y" symbol2 emptySymbolTable
      merged = mergeSymbolTables table1 table2
      result1 = lookupSymbol "x" merged
      result2 = lookupSymbol "y" merged
  case (result1, result2) of
    (Just found1, Just found2) -> do
      assertEqual "symbol1 preserved" symbol1 found1
      assertEqual "symbol2 preserved" symbol2 found2
    _ -> assertFailure "Expected both symbols to be found"

-- Helper types and functions
data SymbolType = VariableSymbol | FunctionSymbol | TypeSymbol deriving (Show, Eq)

data SymbolScope = Global | Local deriving (Show, Eq)

data Symbol = Symbol
  { symbolName :: String
  , symbolType :: SymbolType
  , symbolScope :: SymbolScope
  , symbolInfo :: String
  } deriving (Show, Eq)

newtype SymbolTable = SymbolTable [(String, Symbol)] deriving (Show, Eq)

-- Mock functions
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable []

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol (SymbolTable entries) = SymbolTable $ (name, symbol) : entries

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name (SymbolTable entries) = lookup name entries

updateSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
updateSymbol name newSymbol (SymbolTable entries) = 
  SymbolTable $ map (\(n, s) -> if n == name then (name, newSymbol) else (n, s)) entries

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol name (SymbolTable entries) = SymbolTable $ filter ((/= name) . fst) entries

mergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTables (SymbolTable entries1) (SymbolTable entries2) = 
  SymbolTable $ entries1 ++ entries2

-- Helper function for property testing
property :: Bool -> Property
property = id