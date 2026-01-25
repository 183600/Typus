{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestSymbolTableOperationsSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck ()

import Parser ()
import SourceLocation
import ErrorHandler ()
import Compiler.IR ()
import Ownership ()
import Dependencies ()
import Utils ()
import qualified Data.Text as T ()
import qualified Data.Map as Map
import TestSupport.Arbitrary ()

-- | Test suite for Symbol Table Operations
testSymbolTableOperations :: TestTree
testSymbolTableOperations = testGroup "Symbol Table Operations Tests"
  [ testCase "SymbolTable: empty table has no symbols" $
      let table = testEmptySymbolTable
      in Map.null (symbols table) @?= True
      
  , testCase "SymbolTable: add symbol to empty table" $
      let table = testEmptySymbolTable
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table' = testAddSymbol symbol table
      in Map.size (symbols table') @?= 1
      
  , testCase "SymbolTable: lookup existing symbol" $
      let table = testEmptySymbolTable
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table' = testAddSymbol symbol table
      in case testLookupSymbol "x" table' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: lookup non-existing symbol" $
      let table = testEmptySymbolTable
      in testLookupSymbol "nonexistent" table @?= Nothing
      
  , testCase "SymbolTable: update existing symbol" $
      let table = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          symbol2 = Symbol "x" IRBool (SourceLocation.posAt 2 1)
          table' = testAddSymbol symbol1 table
          table'' = testAddSymbol symbol2 table'
      in case testLookupSymbol "x" table'' of
           Just s -> symbolType s @?= IRBool
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: remove existing symbol" $
      let table = testEmptySymbolTable
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table' = testAddSymbol symbol table
          table'' = testRemoveSymbol "x" table'
      in Map.size (symbols table'') @?= 0
      
  , testCase "SymbolTable: remove non-existing symbol" $
      let table = testEmptySymbolTable
          table' = testRemoveSymbol "nonexistent" table
      in Map.size (symbols table') @?= 0
      
  , testCase "SymbolTable: list all symbols" $
      let table = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          symbol2 = Symbol "y" IRBool (SourceLocation.posAt 2 1)
          symbol3 = Symbol "z" IRString (SourceLocation.posAt 3 1)
          table' = testAddSymbol symbol1 $ testAddSymbol symbol2 $ testAddSymbol symbol3 table
          symbolList = testListSymbols table'
      in length symbolList @?= 3
         
  , testCase "SymbolTable: filter symbols by type" $
      let table = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          symbol2 = Symbol "y" IRBool (SourceLocation.posAt 2 1)
          symbol3 = Symbol "z" IRInt (SourceLocation.posAt 3 1)
          table' = testAddSymbol symbol1 $ testAddSymbol symbol2 $ testAddSymbol symbol3 table
          intSymbols = testFilterSymbolsByType IRInt table'
      in length intSymbols @?= 2
      
  , testCase "SymbolTable: merge two tables" $
      let table1 = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          symbol2 = Symbol "y" IRBool (SourceLocation.posAt 2 1)
          table1' = testAddSymbol symbol1 $ testAddSymbol symbol2 table1
          table2 = testEmptySymbolTable
          symbol3 = Symbol "z" IRString (SourceLocation.posAt 3 1)
          symbol4 = Symbol "w" IRInt (SourceLocation.posAt 4 1)
          table2' = testAddSymbol symbol3 $ testAddSymbol symbol4 table2
          merged = testMergeSymbolTables table1' table2'
      in Map.size (symbols merged) @?= 4
      
  , testCase "SymbolTable: merge tables with conflicts" $
      let table1 = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table1' = testAddSymbol symbol1 table1
          table2 = testEmptySymbolTable
          symbol2 = Symbol "x" IRBool (SourceLocation.posAt 2 1)
          table2' = testAddSymbol symbol2 table2
          merged = testMergeSymbolTables table1' table2'
      in case testLookupSymbol "x" merged of
           Just s -> symbolType s @?= IRBool  -- Second table should win
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: create scope" $
      let table = testEmptySymbolTable
          table' = testPushScope table
      in testScopeDepth table' @?= 1
      
  , testCase "SymbolTable: nested scopes" $
      let table = testEmptySymbolTable
          table' = testPushScope table
          table'' = testPushScope table'
      in testScopeDepth table'' @?= 2
      
  , testCase "SymbolTable: add symbol to scope" $
      let table = testEmptySymbolTable
          table' = testPushScope table
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table'' = testAddSymbolToScope symbol table'
      in case testLookupSymbol "x" table'' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: lookup in parent scope" $
      let table = testEmptySymbolTable
          table' = testPushScope table
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table'' = testAddSymbol symbol table'
          table''' = testPushScope table''
      in case testLookupSymbol "x" table''' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: shadow symbol in child scope" $
      let table = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table' = testAddSymbol symbol1 table
          table'' = testPushScope table'
          symbol2 = Symbol "x" IRBool (SourceLocation.posAt 2 1)
          table''' = testAddSymbolToScope symbol2 table''
      in case testLookupSymbol "x" table''' of
           Just s -> symbolType s @?= IRBool  -- Child scope should shadow parent
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: pop scope" $
      let table = testEmptySymbolTable
          table' = testPushScope table
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table'' = testAddSymbolToScope symbol table'
          table''' = testPopScope table''
      in testScopeDepth table''' @?= 0
      
  , testCase "SymbolTable: symbols are removed when scope is popped" $
      let table = testEmptySymbolTable
          table' = testPushScope table
          symbol = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table'' = testAddSymbolToScope symbol table'
          table''' = testPopScope table''
      in testLookupSymbol "x" table''' @?= Nothing
      
  , testCase "SymbolTable: complex scope operations" $
      let table = testEmptySymbolTable
          symbol1 = Symbol "x" IRInt (SourceLocation.posAt 1 1)
          table' = testAddSymbol symbol1 table
          table'' = testPushScope table'
          symbol2 = Symbol "y" IRBool (SourceLocation.posAt 2 1)
          table''' = testAddSymbolToScope symbol2 table''
          table'''' = testPushScope table'''
          symbol3 = Symbol "z" IRString (SourceLocation.posAt 3 1)
          table''''' = testAddSymbolToScope symbol3 table''''
          table'''''' = testPopScope table'''''
      in case (testLookupSymbol "x" table'''''', testLookupSymbol "y" table'''''', testLookupSymbol "z" table'''''') of
           (Just s1, Just s2, Nothing) -> do
             symbolName s1 @?= "x"
             symbolName s2 @?= "y"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with function parameters" $
      let table = testEmptySymbolTable
          funcSymbol = Symbol "add" (TypeArrow [IRInt, IRInt] IRInt) (SourceLocation.posAt 1 1)
          table' = testAddSymbol funcSymbol table
          table'' = testPushScope table'
          param1 = Symbol "x" IRInt (SourceLocation.posAt 2 1)
          param2 = Symbol "y" IRInt (SourceLocation.posAt 3 1)
          table''' = testAddSymbolToScope param1 $ testAddSymbolToScope param2 table''
      in case (testLookupSymbol "add" table''', testLookupSymbol "x" table''', testLookupSymbol "y" table''') of
           (Just s1, Just s2, Just s3) -> do
             symbolName s1 @?= "add"
             symbolName s2 @?= "x"
             symbolName s3 @?= "y"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with type definitions" $
      let table = testEmptySymbolTable
          typeSymbol = Symbol "MyType" (TypeConstructor "MyType" []) (SourceLocation.posAt 1 1)
          table' = testAddSymbol typeSymbol table
          instanceSymbol = Symbol "myInstance" (TypeConstructor "MyType" []) (SourceLocation.posAt 2 1)
          table'' = testAddSymbol instanceSymbol table'
      in case (testLookupSymbol "MyType" table'', testLookupSymbol "myInstance" table'') of
           (Just s1, Just s2) -> do
             symbolName s1 @?= "MyType"
             symbolName s2 @?= "myInstance"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with modules" $
      let table = testEmptySymbolTable
          moduleSymbol = Symbol "MyModule" ModuleType (SourceLocation.posAt 1 1)
          table' = testAddSymbol moduleSymbol table
          table'' = testPushScope table'
          moduleFunc = Symbol "myFunction" (TypeArrow [IRInt] IRBool) (SourceLocation.posAt 2 1)
          table''' = testAddSymbolToScope moduleFunc table''
      in case (testLookupSymbol "MyModule" table''', testLookupSymbol "myFunction" table''') of
           (Just s1, Just s2) -> do
             symbolName s1 @?= "MyModule"
             symbolName s2 @?= "myFunction"
           _ -> assertFailure "Unexpected lookup results"
  ]

-- Symbol Table implementation
data SymbolType = IRInt | IRBool | IRString | TypeArrow [SymbolType] SymbolType | 
                   TypeConstructor String [SymbolType] | ModuleType
  deriving (Eq, Show)

data Symbol = Symbol 
  { symbolName :: String
  , symbolType :: SymbolType
  , symbolPosition :: SourcePos
  } deriving (Eq, Show)

data SymbolTable = SymbolTable 
  { symbols :: Map.Map String Symbol
  , scopes :: [Map.Map String Symbol]
  } deriving (Eq, Show)

testEmptySymbolTable :: SymbolTable
testEmptySymbolTable = SymbolTable Map.empty []

testAddSymbol :: Symbol -> SymbolTable -> SymbolTable
testAddSymbol symbol table =
  let currentSymbols = symbols table
      newSymbols = Map.insert (symbolName symbol) symbol currentSymbols
  in table { symbols = newSymbols }

testAddSymbolToScope :: Symbol -> SymbolTable -> SymbolTable
testAddSymbolToScope symbol table =
  case scopes table of
    [] -> testAddSymbol symbol table  -- No current scope, add to global
    (currentScope:restScopes) -> 
      let newScope = Map.insert (symbolName symbol) symbol currentScope
          newScopes = newScope : restScopes
          allSymbols = Map.union newScope (symbols table)
      in table { scopes = newScopes, symbols = allSymbols }

testLookupSymbol :: String -> SymbolTable -> Maybe Symbol
testLookupSymbol name table = 
  case scopes table of
    [] -> Map.lookup name (symbols table)  -- No scopes, check global
    currentScope:_ -> 
      case Map.lookup name currentScope of
        Just symbol -> Just symbol
        Nothing -> Map.lookup name (symbols table)  -- Check global if not in current scope

testRemoveSymbol :: String -> SymbolTable -> SymbolTable
testRemoveSymbol name table = 
  let currentSymbols = symbols table
      newSymbols = Map.delete name currentSymbols
      newScopes = map (Map.delete name) (scopes table)
  in table { symbols = newSymbols, scopes = newScopes }

testListSymbols :: SymbolTable -> [Symbol]
testListSymbols table = Map.elems (symbols table)

testFilterSymbolsByType :: SymbolType -> SymbolTable -> [Symbol]
testFilterSymbolsByType filterType table = 
  filter (\s -> symbolType s == filterType) (testListSymbols table)

testMergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
testMergeSymbolTables table1 table2 = 
  let symbols1 = symbols table1
      symbols2 = symbols table2
      mergedSymbols = Map.union symbols2 symbols1  -- table2 takes precedence
  in SymbolTable mergedSymbols []

testPushScope :: SymbolTable -> SymbolTable
testPushScope table = 
  let currentScopes = scopes table
      newScopes = Map.empty : currentScopes
  in table { scopes = newScopes }

testPopScope :: SymbolTable -> SymbolTable
testPopScope table = 
  case scopes table of
    [] -> table  -- No scopes to pop
    (_:restScopes) -> 
      let remainingSymbols = foldl Map.union Map.empty restScopes
      in table { scopes = restScopes, symbols = remainingSymbols }

testScopeDepth :: SymbolTable -> Int
testScopeDepth table = length (scopes table)

-- Simplified SourceLocation type for testing
data TestSourcePos = TestSourcePos 
  { testPosLine :: Int
  , testPosColumn :: Int
  } deriving (Eq, Show)

testPosAt :: Int -> Int -> TestSourcePos
testPosAt lineNum columnNum = TestSourcePos lineNum columnNum