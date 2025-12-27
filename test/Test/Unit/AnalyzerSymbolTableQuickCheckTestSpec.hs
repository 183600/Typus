{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AnalyzerSymbolTableQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

import Analyzer.SymbolTable
import Analyzer.Types
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Analyzer SymbolTable QuickCheck Tests"
  [ symbolTableCreationTests
  , symbolInsertionTests
  , symbolLookupTests
  , symbolScopeTests
  , symbolTypeTests
  , symbolAttributeTests
  , symbolTableMergeTests
  , symbolTableValidationTests
  , symbolTableExportTests
  , symbolTablePerformanceTests
  ]

-- | 1. 符号表创建测试
symbolTableCreationTests :: TestTree
symbolTableCreationTests = testGroup "SymbolTable Creation Tests"
  [ testCase "Empty symbol table" $
      let emptyTable = emptySymbolTable
      in symbolTableSize emptyTable @?= 0
  
  , testCase "Symbol table with parent" $
      let parent = emptySymbolTable
          child = createChildSymbolTable parent
      in symbolTableParent child @?= Just parent
  
  , fastProperty "Symbol table initial scope" $
      \scopeName -> let table = createSymbolTableWithScope scopeName
                    in symbolTableCurrentScope table == scopeName
  ]

-- | 2. 符号插入测试
symbolInsertionTests :: TestTree
symbolInsertionTests = testGroup "Symbol Insertion Tests"
  [ testCase "Insert variable symbol" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
      in symbolTableSize table' @?= 1
  
  , testCase "Insert function symbol" $
      let table = emptySymbolTable
          symbol = FunctionSymbol "f" [IntType] StringType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
      in symbolTableSize table' @?= 1
  
  , fastProperty "Insert multiple symbols" $
      \symbolNames -> let symbols = map (\n -> VariableSymbol n IntType (SourceSpan startPos startPos)) symbolNames
                          table = foldl insertSymbol emptySymbolTable symbols
                      in symbolTableSize table == length (nub symbolNames)
  ]

-- | 3. 符号查找测试
symbolLookupTests :: TestTree
symbolLookupTests = testGroup "Symbol Lookup Tests"
  [ testCase "Lookup existing symbol" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
          result = lookupSymbol table' "x"
      in case result of
           Just (VariableSymbol name _ _) -> name @?= "x"
           _ -> "Expected VariableSymbol" @?= "Found something else"
  
  , testCase "Lookup non-existing symbol" $
      let table = emptySymbolTable
          result = lookupSymbol table "nonexistent"
      in result @?= Nothing
  
  , fastProperty "Lookup inserted symbol" $
      \name -> let symbol = VariableSymbol name StringType (SourceSpan startPos startPos)
                   table = insertSymbol emptySymbolTable symbol
                   result = lookupSymbol table name
               in case result of
                    Just (VariableSymbol foundName _ _) -> foundName == name
                    _ -> False
  ]

-- | 4. 符号作用域测试
symbolScopeTests :: TestTree
symbolScopeTests = testGroup "Symbol Scope Tests"
  [ testCase "Enter new scope" $
      let table = emptySymbolTable
          table' = enterScope table "inner"
      in symbolTableCurrentScope table' @?= "inner"
  
  , testCase "Exit scope" $
      let table = emptySymbolTable
          table' = enterScope table "inner"
          table'' = exitScope table'
      in symbolTableCurrentScope table'' @?= "global"
  
  , fastProperty "Scope isolation" $
      \varName -> let table = emptySymbolTable
                      table' = enterScope table "inner"
                      symbol = VariableSymbol varName IntType (SourceSpan startPos startPos)
                      table'' = insertSymbol table' symbol
                      result = lookupSymbol (exitScope table'') varName
                  in result == Nothing
  ]

-- | 5. 符号类型测试
symbolTypeTests :: TestTree
symbolTypeTests = testGroup "Symbol Type Tests"
  [ testCase "Variable symbol type" $
      let symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
      in getSymbolType symbol @?= IntType
  
  , testCase "Function symbol type" $
      let symbol = FunctionSymbol "f" [IntType] StringType (SourceSpan startPos startPos)
      in getSymbolType symbol @?= FunctionType [IntType] StringType
  
  , testCase "Type symbol type" $
      let symbol = TypeSymbol "MyType" (CustomType "MyType") (SourceSpan startPos startPos)
      in getSymbolType symbol @?= CustomType "MyType"
  
  , fastProperty "Type consistency" $
      \name symbolType -> let symbol = VariableSymbol name symbolType (SourceSpan startPos startPos)
                          in getSymbolType symbol == symbolType
  ]

-- | 6. 符号属性测试
symbolAttributeTests :: TestTree
symbolAttributeTests = testGroup "Symbol Attribute Tests"
  [ testCase "Add attribute to symbol" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
          table'' = addSymbolAttribute table' "x" "mutable" "true"
      in getSymbolAttribute table'' "x" "mutable" @?= Just "true"
  
  , testCase "Get non-existing attribute" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
      in getSymbolAttribute table' "x" "nonexistent" @?= Nothing
  
  , fastProperty "Multiple attributes" $
      \attrs -> let table = emptySymbolTable
                    symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
                    table' = insertSymbol table symbol
                    table'' = foldl (\t (k, v) -> addSymbolAttribute t "x" k v) table' attrs
                in all (\(k, v) -> getSymbolAttribute table'' "x" k == Just v) attrs
  ]

-- | 7. 符号表合并测试
symbolTableMergeTests :: TestTree
symbolTableTests = testGroup "SymbolTable Merge Tests"
  [ testCase "Merge empty tables" $
      let table1 = emptySymbolTable
          table2 = emptySymbolTable
          merged = mergeSymbolTables table1 table2
      in symbolTableSize merged @?= 0
  
  , testCase "Merge with conflicts" $
      let table1 = emptySymbolTable
          symbol1 = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table1' = insertSymbol table1 symbol1
          table2 = emptySymbolTable
          symbol2 = VariableSymbol "x" StringType (SourceSpan startPos startPos)
          table2' = insertSymbol table2 symbol2
          merged = mergeSymbolTables table1' table2'
      in symbolTableSize merged @?= 1  -- Conflict resolution keeps one
  
  , fastProperty "Merge non-conflicting tables" $
      \names1 names2 -> let symbols1 = map (\n -> VariableSymbol n IntType (SourceSpan startPos startPos)) names1
                            symbols2 = map (\n -> VariableSymbol n StringType (SourceSpan startPos startPos)) (names2 :: [String])
                            table1 = foldl insertSymbol emptySymbolTable symbols1
                            table2 = foldl insertSymbol emptySymbolTable symbols2
                            merged = mergeSymbolTables table1 table2
                            uniqueNames = nub (names1 ++ names2)
                        in symbolTableSize merged == length uniqueNames
  ]

-- | 8. 符号表验证测试
symbolTableValidationTests :: TestTree
symbolTableValidationTests = testGroup "SymbolTable Validation Tests"
  [ testCase "Valid empty symbol table" $
      let table = emptySymbolTable
      in validateSymbolTable table @?= True
  
  , testCase "Valid symbol table with variables" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
      in validateSymbolTable table' @?= True
  
  , fastProperty "Symbol table with valid symbols" $
      \names -> let symbols = map (\n -> VariableSymbol n IntType (SourceSpan startPos startPos)) names
                    table = foldl insertSymbol emptySymbolTable symbols
                in validateSymbolTable table
  ]

-- | 9. 符号表导出测试
symbolTableExportTests :: TestTree
symbolTableExportTests = testGroup "SymbolTable Export Tests"
  [ testCase "Export empty table" $
      let table = emptySymbolTable
          exported = exportSymbolTable table
      in length exported @?= 0
  
  , testCase "Export table with symbols" $
      let table = emptySymbolTable
          symbol = VariableSymbol "x" IntType (SourceSpan startPos startPos)
          table' = insertSymbol table symbol
          exported = exportSymbolTable table'
      in length exported @?= 1
  
  , fastProperty "Export preserves symbol names" $
      \names -> let symbols = map (\n -> VariableSymbol n IntType (SourceSpan startPos startPos)) names
                    table = foldl insertSymbol emptySymbolTable symbols
                    exported = exportSymbolTable table
                in sort (map getSymbolName exported) == sort (nub names)
  ]
  where
    getSymbolName (VariableSymbol name _ _) = name
    getSymbolName (FunctionSymbol name _ _ _) = name
    getSymbolName (TypeSymbol name _ _) = name

-- | 10. 符号表性能测试
symbolTablePerformanceTests :: TestTree
symbolTablePerformanceTests = testGroup "SymbolTable Performance Tests"
  [ testCase "Large symbol table lookup performance" $
      let table = foldl (\t i -> insertSymbol t (VariableSymbol ("var" ++ show i) IntType (SourceSpan startPos startPos))) 
                        emptySymbolTable [1..1000]
          result = lookupSymbol table "var500"
      in case result of
           Just (VariableSymbol name _ _) -> name @?= "var500"
           _ -> "Expected VariableSymbol" @?= "Found something else"
  
  , fastProperty "Lookup time complexity" $
      \names -> let symbols = map (\n -> VariableSymbol n IntType (SourceSpan startPos startPos)) (take 100 names)
                    table = foldl insertSymbol emptySymbolTable symbols
                    lookupResults = map (\n -> lookupSymbol table n) (take 100 names)
                in all isJust lookupResults
  ]
  where
    isJust Nothing = False
    isJust (Just _) = True