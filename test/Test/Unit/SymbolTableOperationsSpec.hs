module Test.Unit.SymbolTableOperationsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.SymbolTable

-- Test symbol table creation
prop_symbol_table_creation :: Property
prop_symbol_table_creation =
  let table1 = createSymbolTable
      table2 = createSymbolTable
  in property $ isEmptySymbolTable table1 && isEmptySymbolTable table2

-- Test symbol insertion and lookup
prop_symbol_insertion_lookup :: String -> String -> Property
prop_symbol_insertion_lookup symbolName symbolType =
  let table = createSymbolTable
      tableWithSymbol = insertSymbol table symbolName symbolType
      lookupResult = lookupSymbol tableWithSymbol symbolName
  in property $ lookupResult === Just symbolType

-- Test symbol shadowing
prop_symbol_shadowing :: String -> String -> String -> Property
prop_symbol_shadowing symbolName oldType newType =
  let table = createSymbolTable
      table1 = insertSymbol table symbolName oldType
      table2 = insertSymbol table1 symbolName newType
      lookupResult = lookupSymbol table2 symbolName
  in property $ lookupResult === Just newType

-- Test symbol table scoping
prop_symbol_table_scoping :: String -> String -> Property
prop_symbol_table_scoping outerSymbol innerSymbol =
  let outerTable = createSymbolTable
      outerTableWithSymbol = insertSymbol outerTable outerSymbol "outer"
      innerScope = createInnerScope outerTableWithSymbol
      innerTable = insertSymbol innerScope innerSymbol "inner"
  in property $ 
    lookupSymbol innerTable outerSymbol === Just "outer" &&
    lookupSymbol innerTable innerSymbol === Just "inner"

-- Test symbol table merging
prop_symbol_table_merge :: [(String, String)] -> [(String, String)] -> Property
prop_symbol_table_merge symbols1 symbols2 =
  let table1 = foldl (\t (name, typ) -> insertSymbol t name typ) createSymbolTable symbols1
      table2 = foldl (\t (name, typ) -> insertSymbol t name typ) createSymbolTable symbols2
      merged = mergeSymbolTables table1 table2
  in property $ 
    all (\(name, typ) -> lookupSymbol merged name === Just typ) (symbols1 ++ symbols2)

tests :: TestTree
tests = testGroup "SymbolTable Operations Tests"
  [ testProperty "symbol table creation" prop_symbol_table_creation
  , testProperty "symbol insertion and lookup" prop_symbol_insertion_lookup
  , testProperty "symbol shadowing" prop_symbol_shadowing
  , testProperty "symbol table scoping" prop_symbol_table_scoping
  , testProperty "symbol table merge" prop_symbol_table_merge
  ]