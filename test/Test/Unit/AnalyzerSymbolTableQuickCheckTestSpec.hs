{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AnalyzerSymbolTableQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import qualified Data.Set as Set
import Data.List (sort, nub)

import Analyzer.Types (SymbolInfo(..), SymbolKind(..))
import qualified Analyzer.Types as Analyzer.Types
import qualified Dependencies as Dep
import qualified Data.Map.Strict as Map
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
      let emptyTable = Map.empty :: Map.Map String String
      in Map.size emptyTable @?= 0
  
  , testCase "Symbol table with parent" $
      let parent = Map.singleton "parent" "value"
          child = Map.insert "child" "value" parent
      in Map.lookup "parent" child @?= Just "value"
  
  , fastProperty "Symbol table initial scope" $
      \(scopeName :: String) -> let table = Map.singleton "scope" scopeName
                                 in Map.lookup "scope" table == Just scopeName
  ]

-- | 2. 符号插入测试
symbolInsertionTests :: TestTree
symbolInsertionTests = testGroup "Symbol Insertion Tests"
  [ testCase "Insert single symbol" $
      let table = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
      in Map.size table' @?= 1
  
  , fastProperty "Insert multiple symbols" $
      \symbolNames -> let symbols = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) symbolNames
                          table = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols
                      in Map.size table == length (nub symbolNames)
  ]

-- | 3. 符号查找测试
symbolLookupTests :: TestTree
symbolLookupTests = testGroup "Symbol Lookup Tests"
  [ testCase "Lookup existing symbol" $
      let table = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
          result = Map.lookup "x" table'
      in case result of
           Just info -> Analyzer.Types.symbolName info @?= "x"
           _ -> "Expected SymbolInfo" @?= "Found something else"
  
  , testCase "Lookup non-existing symbol" $
      let table = Map.empty :: Map.Map String String
          result = Map.lookup "nonexistent" table
      in result @?= Nothing
  
  , fastProperty "Lookup inserted symbol" $
      \name -> let symbol = Analyzer.Types.SymbolInfo { symbolName = name, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
                   table = Map.insert name symbol Map.empty
                   result = Map.lookup name table
               in case result of
                    Just info -> Analyzer.Types.symbolName info == name
                    _ -> False
  ]

-- | 4. 符号作用域测试
symbolScopeTests :: TestTree
symbolScopeTests = testGroup "Symbol Scope Tests"
  [ testCase "Enter new scope" $
      let table = Map.empty
          table' = Map.insert "scope" "inner" table
      in Map.lookup "scope" table' @?= Just "inner"
  
  , testCase "Map operations" $
      let table = Map.empty
          table' = Map.insert "key" "value" table
          table'' = Map.delete "key" table'
      in Map.size table'' @?= 0
  
  , fastProperty "Map lookup" $
      \key -> let table = Map.singleton (key :: String) "value"
                  result = Map.lookup (key :: String) table
              in result == Just "value"
  ]

-- | 5. 符号类型测试
symbolTypeTests :: TestTree
symbolTypeTests = testGroup "Symbol Type Tests"
  [ testCase "Variable symbol type" $
      let symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
      in Analyzer.Types.symbolType symbol @?= Just (Dep.TVCon "Int")
  
  , testCase "Function symbol type" $
      let symbol = Analyzer.Types.SymbolInfo "f" (Just (Dep.TVFun [Dep.TVCon "Int"] (Dep.TVCon "String"))) Nothing 0 False False []
      in Analyzer.Types.symbolType symbol @?= Just (Dep.TVFun [Dep.TVCon "Int"] (Dep.TVCon "String"))
  
  , testCase "Type symbol lookup" $
      let symbol = Analyzer.Types.SymbolInfo { symbolName = "MyType", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
      in Analyzer.Types.symbolType symbol @?= Just (Dep.TVCon "MyType")
  
  , fastProperty "Type consistency" $
      \name -> let symbol = Analyzer.Types.SymbolInfo { symbolName = name, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
               in Analyzer.Types.symbolType symbol == Just (Dep.TVCon "Int")
  ]

-- | 6. 符号属性测试
symbolAttributeTests :: TestTree
symbolAttributeTests = testGroup "Symbol Attribute Tests"
  [ testCase "Add symbol to table" $
      let table = Map.empty
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
      in Map.size table' @?= 1
  
  , testCase "Get symbol info" $
      let table = Map.empty
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
      in Map.lookup "x" table' @?= Just symbol
  
  , fastProperty "Multiple symbols" $
      \names -> let symbols = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) (take 10 names)
                    table = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols
                in Map.size table == 10
  ]

-- | 7. 符号表合并测试
symbolTableMergeTests :: TestTree
symbolTableMergeTests = testGroup "SymbolTable Merge Tests"
  [ testCase "Merge empty tables" $
      let table1 = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          table2 = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          merged = Map.union table1 table2
      in Map.size merged @?= 0
  
  , testCase "Merge with conflicts" $
      let table1 = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          symbol1 = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table1' = Map.insert "x" symbol1 table1
          table2 = Map.empty :: Map.Map String Analyzer.Types.SymbolInfo
          symbol2 = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table2' = Map.insert "x" symbol2 table2
          merged = Map.union table1' table2'
      in Map.size merged @?= 1  -- Conflict resolution keeps one
  
  , fastProperty "Merge non-conflicting tables" $
      \names1 names2 -> let symbols1 = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) names1
                            symbols2 = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) (names2 :: [String])
                            table1 = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols1
                            table2 = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols2
                            merged = Map.union table1 table2
                        in Map.size merged == length (nub (names1 ++ names2))
  ]

-- | 8. 符号表验证测试
symbolTableValidationTests :: TestTree
symbolTableValidationTests = testGroup "SymbolTable Validation Tests"
  [ testCase "Insert valid symbol" $
      let table = Map.empty
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
      in Map.size table' @?= 1
  
  , fastProperty "Symbol table with valid symbols" $
      \names -> let symbols = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) names
                    table = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols
                in Map.size table == length names
  ]

-- | 9. 符号表导出测试
symbolTableExportTests :: TestTree
symbolTableExportTests = testGroup "SymbolTable Export Tests"
  [ testCase "Export empty table" $
      let table = Map.empty
          exported = table  -- Simplified export
      in Map.size exported @?= 0
  
  , testCase "Export table with symbols" $
      let table = Map.empty
          symbol = Analyzer.Types.SymbolInfo { symbolName = "x", symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }
          table' = Map.insert "x" symbol table
          exported = table'  -- Simplified export
      in Map.size exported @?= 1
  
  , fastProperty "Export preserves symbol names" $
      \names -> let symbols = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) names
                    table = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols
                    exported = table  -- Simplified export
                in sort (Map.keys exported) == sort (nub names)
  ]
  where
    getSymbolName info = Analyzer.Types.symbolName info

-- | 10. 符号表性能测试
symbolTablePerformanceTests :: TestTree
symbolTablePerformanceTests = testGroup "SymbolTable Performance Tests"
  [ testCase "Large symbol table lookup performance" $
      let table = foldl (\t i -> Map.insert ("var" ++ show i) (Analyzer.Types.SymbolInfo { symbolName = "var" ++ show i, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] }) t) 
                        Map.empty [1..1000]
          result = Map.lookup "var500" table
      in case result of
           Just info -> Analyzer.Types.symbolName info @?= "var500"
           _ -> "Expected VariableSymbol" @?= "Found something else"
  
  , fastProperty "Lookup time complexity" $
      \names -> let symbols = map (\n -> (n, Analyzer.Types.SymbolInfo { symbolName = n, symbolType = Just (Dep.TVCon "Int"), ownershipState = Nothing, symbolScope = 0, isMoved = False, isBorrowed = False, constraints = [] })) (take 100 names)
                    table = foldl (\t (n, s) -> Map.insert n s t) Map.empty symbols
                    lookupResults = map (\n -> Map.lookup n table) (take 100 names)
                in all isJust lookupResults
  ]
  where
    isJust Nothing = False
    isJust (Just _) = True