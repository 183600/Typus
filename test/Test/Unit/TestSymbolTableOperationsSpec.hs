{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestSymbolTableOperationsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import TestSupport.Arbitrary ()

-- | Test suite for Symbol Table Operations
testSymbolTableOperations :: TestTree
testSymbolTableOperations = testGroup "Symbol Table Operations Tests"
  [ testCase "SymbolTable: empty table has no symbols" $
      let table = emptySymbolTable
      in Map.null (symbols table) @?= True
      
  , testCase "SymbolTable: add symbol to empty table" $
      let table = emptySymbolTable
          symbol = Symbol "x" IRInt (posAt 1 1)
          table' = addSymbol symbol table
      in Map.size (symbols table') @?= 1
      
  , testCase "SymbolTable: lookup existing symbol" $
      let table = emptySymbolTable
          symbol = Symbol "x" IRInt (posAt 1 1)
          table' = addSymbol symbol table
      in case lookupSymbol "x" table' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: lookup non-existing symbol" $
      let table = emptySymbolTable
      in lookupSymbol "nonexistent" table @?= Nothing
      
  , testCase "SymbolTable: update existing symbol" $
      let table = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          symbol2 = Symbol "x" IRBool (posAt 2 1)
          table' = addSymbol symbol1 table
          table'' = addSymbol symbol2 table'
      in case lookupSymbol "x" table'' of
           Just s -> symbolType s @?= IRBool
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: remove existing symbol" $
      let table = emptySymbolTable
          symbol = Symbol "x" IRInt (posAt 1 1)
          table' = addSymbol symbol table
          table'' = removeSymbol "x" table'
      in Map.size (symbols table'') @?= 0
      
  , testCase "SymbolTable: remove non-existing symbol" $
      let table = emptySymbolTable
          table' = removeSymbol "nonexistent" table
      in Map.size (symbols table') @?= 0
      
  , testCase "SymbolTable: list all symbols" $
      let table = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          symbol2 = Symbol "y" IRBool (posAt 2 1)
          symbol3 = Symbol "z" IRString (posAt 3 1)
          table' = addSymbol symbol1 $ addSymbol symbol2 $ addSymbol symbol3 table
          symbolList = listSymbols table'
      in length symbolList @?= 3
         
  , testCase "SymbolTable: filter symbols by type" $
      let table = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          symbol2 = Symbol "y" IRBool (posAt 2 1)
          symbol3 = Symbol "z" IRInt (posAt 3 1)
          table' = addSymbol symbol1 $ addSymbol symbol2 $ addSymbol symbol3 table
          intSymbols = filterSymbolsByType IRInt table'
      in length intSymbols @?= 2
      
  , testCase "SymbolTable: merge two tables" $
      let table1 = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          symbol2 = Symbol "y" IRBool (posAt 2 1)
          table1' = addSymbol symbol1 $ addSymbol symbol2 table1
          table2 = emptySymbolTable
          symbol3 = Symbol "z" IRString (posAt 3 1)
          symbol4 = Symbol "w" IRInt (posAt 4 1)
          table2' = addSymbol symbol3 $ addSymbol symbol4 table2
          merged = mergeSymbolTables table1' table2'
      in Map.size (symbols merged) @?= 4
      
  , testCase "SymbolTable: merge tables with conflicts" $
      let table1 = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          table1' = addSymbol symbol1 table1
          table2 = emptySymbolTable
          symbol2 = Symbol "x" IRBool (posAt 2 1)
          table2' = addSymbol symbol2 table2
          merged = mergeSymbolTables table1' table2'
      in case lookupSymbol "x" merged of
           Just s -> symbolType s @?= IRBool  -- Second table should win
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: create scope" $
      let table = emptySymbolTable
          table' = pushScope table
      in scopeDepth table' @?= 1
      
  , testCase "SymbolTable: nested scopes" $
      let table = emptySymbolTable
          table' = pushScope table
          table'' = pushScope table'
      in scopeDepth table'' @?= 2
      
  , testCase "SymbolTable: add symbol to scope" $
      let table = emptySymbolTable
          table' = pushScope table
          symbol = Symbol "x" IRInt (posAt 1 1)
          table'' = addSymbolToScope symbol table'
      in case lookupSymbol "x" table'' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: lookup in parent scope" $
      let table = emptySymbolTable
          table' = pushScope table
          symbol = Symbol "x" IRInt (posAt 1 1)
          table'' = addSymbol symbol table'
          table''' = pushScope table''
      in case lookupSymbol "x" table''' of
           Just s -> symbolName s @?= "x"
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: shadow symbol in child scope" $
      let table = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          table' = addSymbol symbol1 table
          table'' = pushScope table'
          symbol2 = Symbol "x" IRBool (posAt 2 1)
          table''' = addSymbolToScope symbol2 table''
      in case lookupSymbol "x" table''' of
           Just s -> symbolType s @?= IRBool  -- Child scope should shadow parent
           Nothing -> assertFailure "Symbol not found"
           
  , testCase "SymbolTable: pop scope" $
      let table = emptySymbolTable
          table' = pushScope table
          symbol = Symbol "x" IRInt (posAt 1 1)
          table'' = addSymbolToScope symbol table'
          table''' = popScope table''
      in scopeDepth table''' @?= 0
      
  , testCase "SymbolTable: symbols are removed when scope is popped" $
      let table = emptySymbolTable
          table' = pushScope table
          symbol = Symbol "x" IRInt (posAt 1 1)
          table'' = addSymbolToScope symbol table'
          table''' = popScope table''
      in lookupSymbol "x" table''' @?= Nothing
      
  , testCase "SymbolTable: complex scope operations" $
      let table = emptySymbolTable
          symbol1 = Symbol "x" IRInt (posAt 1 1)
          table' = addSymbol symbol1 table
          table'' = pushScope table'
          symbol2 = Symbol "y" IRBool (posAt 2 1)
          table''' = addSymbolToScope symbol2 table''
          table'''' = pushScope table'''
          symbol3 = Symbol "z" IRString (posAt 3 1)
          table''''' = addSymbolToScope symbol3 table''''
          table'''''' = popScope table'''''
      in case (lookupSymbol "x" table'''''', lookupSymbol "y" table'''''', lookupSymbol "z" table'''''') of
           (Just s1, Just s2, Nothing) -> do
             symbolName s1 @?= "x"
             symbolName s2 @?= "y"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with function parameters" $
      let table = emptySymbolTable
          funcSymbol = Symbol "add" (TypeArrow [IRInt, IRInt] IRInt) (posAt 1 1)
          table' = addSymbol funcSymbol table
          table'' = pushScope table'
          param1 = Symbol "x" IRInt (posAt 2 1)
          param2 = Symbol "y" IRInt (posAt 3 1)
          table''' = addSymbolToScope param1 $ addSymbolToScope param2 table''
      in case (lookupSymbol "add" table''', lookupSymbol "x" table''', lookupSymbol "y" table''') of
           (Just s1, Just s2, Just s3) -> do
             symbolName s1 @?= "add"
             symbolName s2 @?= "x"
             symbolName s3 @?= "y"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with type definitions" $
      let table = emptySymbolTable
          typeSymbol = Symbol "MyType" (TypeConstructor "MyType" []) (posAt 1 1)
          table' = addSymbol typeSymbol table
          instanceSymbol = Symbol "myInstance" (TypeConstructor "MyType" []) (posAt 2 1)
          table'' = addSymbol instanceSymbol table'
      in case (lookupSymbol "MyType" table'', lookupSymbol "myInstance" table'') of
           (Just s1, Just s2) -> do
             symbolName s1 @?= "MyType"
             symbolName s2 @?= "myInstance"
           _ -> assertFailure "Unexpected lookup results"
           
  , testCase "SymbolTable: symbol table with modules" $
      let table = emptySymbolTable
          moduleSymbol = Symbol "MyModule" ModuleType (posAt 1 1)
          table' = addSymbol moduleSymbol table
          table'' = pushScope table'
          moduleFunc = Symbol "myFunction" (TypeArrow [IRInt] IRBool) (posAt 2 1)
          table''' = addSymbolToScope moduleFunc table''
      in case (lookupSymbol "MyModule" table''', lookupSymbol "myFunction" table''') of
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

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable Map.empty []

addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol symbol table = 
  let currentSymbols = symbols table
      newSymbols = Map.insert (symbolName symbol) symbol currentSymbols
  in table { symbols = newSymbols }

addSymbolToScope :: Symbol -> SymbolTable -> SymbolTable
addSymbolToScope symbol table = 
  case scopes table of
    [] -> addSymbol symbol table  -- No current scope, add to global
    (currentScope:restScopes) -> 
      let newScope = Map.insert (symbolName symbol) symbol currentScope
          newScopes = newScope : restScopes
          allSymbols = foldl Map.insert (symbols table) newScope
      in table { scopes = newScopes, symbols = allSymbols }

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = 
  case scopes table of
    [] -> Map.lookup name (symbols table)  -- No scopes, check global
    currentScope:_ -> 
      case Map.lookup name currentScope of
        Just symbol -> Just symbol
        Nothing -> Map.lookup name (symbols table)  -- Check global if not in current scope

removeSymbol :: String -> SymbolTable -> SymbolTable
removeSymbol name table = 
  let currentSymbols = symbols table
      newSymbols = Map.delete name currentSymbols
      newScopes = map (Map.delete name) (scopes table)
  in table { symbols = newSymbols, scopes = newScopes }

listSymbols :: SymbolTable -> [Symbol]
listSymbols table = Map.elems (symbols table)

filterSymbolsByType :: SymbolType -> SymbolTable -> [Symbol]
filterSymbolsByType symbolType table = 
  filter (\s -> symbolType s == symbolType) (listSymbols table)

mergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTables table1 table2 = 
  let symbols1 = symbols table1
      symbols2 = symbols table2
      mergedSymbols = Map.union symbols2 symbols1  -- table2 takes precedence
  in SymbolTable mergedSymbols []

pushScope :: SymbolTable -> SymbolTable
pushScope table = 
  let currentScopes = scopes table
      newScopes = Map.empty : currentScopes
  in table { scopes = newScopes }

popScope :: SymbolTable -> SymbolTable
popScope table = 
  case scopes table of
    [] -> table  -- No scopes to pop
    (_:restScopes) -> 
      let remainingSymbols = foldl Map.union Map.empty restScopes
      in table { scopes = restScopes, symbols = remainingSymbols }

scopeDepth :: SymbolTable -> Int
scopeDepth table = length (scopes table)

-- Simplified SourceLocation type for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  } deriving (Eq, Show)

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column