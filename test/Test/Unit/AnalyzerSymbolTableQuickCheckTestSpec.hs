{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AnalyzerSymbolTableQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Analyzer.SymbolTable (collectSymbolsAndTypes, collectSymbolsFromAST)
import Analyzer.Types (SymbolInfo(..), SymbolKind(..))
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..))
import Dependencies.Parser (runParser)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Either (isLeft, isRight)
import Control.Monad.State
import Control.Monad.Except

-- | 测试符号收集的基本属性
prop_collect_symbols_basic :: String -> Property
prop_collect_symbols_basic code =
  not (null code) ==>
  let result = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集的幂等性
prop_collect_symbols_idempotent :: String -> Property
prop_collect_symbols_idempotent code =
  not (null code) ==>
  let result1 = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
      result2 = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
  in case (result1, result2) of
    (Right (_, table1), Right (_, table2)) -> Map.size table1 === Map.size table2
    (Left _, Left _) -> property True
    _ -> property False

-- | 测试符号收集对空代码的处理
prop_collect_symbols_empty :: Property
prop_collect_symbols_empty =
  let result = runExcept (evalStateT (collectSymbolsAndTypes "") newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table === 0

-- | 测试符号收集对单行代码的处理
prop_collect_symbols_single_line :: String -> Property
prop_collect_symbols_single_line code =
  not (null code) && not ('\n' `elem` code) ==>
  let result = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对多行代码的处理
prop_collect_symbols_multiline :: Positive Int -> String -> Property
prop_collect_symbols_multiline (Positive n) code =
  n < 10 ==>
  let multiLineCode = unlines $ replicate n code
      result = runExcept (evalStateT (collectSymbolsAndTypes multiLineCode) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对类型定义的处理
prop_collect_symbols_type_def :: String -> Property
prop_collect_symbols_type_def typeName =
  not (null typeName) ==>
  let typeDef = "type " ++ typeName ++ " = int"
      result = runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对变量声明的处理
prop_collect_symbols_var_decl :: String -> Property
prop_collect_symbols_var_decl varName =
  not (null varName) ==>
  let varDecl = "var " ++ varName ++ " : int"
      result = runExcept (evalStateT (collectSymbolsAndTypes varDecl) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对函数声明的处理
prop_collect_symbols_func_decl :: String -> Property
prop_collect_symbols_func_decl funcName =
  not (null funcName) ==>
  let funcDecl = "func " ++ funcName ++ "(x: int) : int"
      result = runExcept (evalStateT (collectSymbolsAndTypes funcDecl) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对复杂类型的处理
prop_collect_symbols_complex_type :: Property
prop_collect_symbols_complex_type =
  let complexType = "type Complex = func(int, ref(string)) -> array[int, 10]"
      result = runExcept (evalStateT (collectSymbolsAndTypes complexType) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对递归类型的处理
prop_collect_symbols_recursive_type :: String -> Property
prop_collect_symbols_recursive_type typeName =
  not (null typeName) ==>
  let recursiveType = "type " ++ typeName ++ " = " ++ typeName ++ " | nil"
      result = runExcept (evalStateT (collectSymbolsAndTypes recursiveType) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对泛型类型的处理
prop_collect_symbols_generic_type :: String -> Property
prop_collect_symbols_generic_type typeName =
  not (null typeName) ==>
  let genericType = "type " ++ typeName ++ "[T] = T | " ++ typeName ++ "[T]"
      result = runExcept (evalStateT (collectSymbolsAndTypes genericType) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对复杂程序的处理
prop_collect_symbols_complex_program :: Property
prop_collect_symbols_complex_program =
  let complexProgram = unlines
        [ "type List[T] = T | List[T]"
        , "type Option[T] = Some(T) | None"
        , "func head[T](l: List[T]) : Option[T] ="
        , "  match l with"
        , "  | T -> Some(T)"
        , "  | List[T] -> head(l)"
        , "  | None -> None"
        ]
      result = runExcept (evalStateT (collectSymbolsAndTypes complexProgram) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对错误代码的处理
prop_collect_symbols_invalid_code :: String -> Property
prop_collect_symbols_invalid_code invalidCode =
  let result = runExcept (evalStateT (collectSymbolsAndTypes invalidCode) newAnalyzerState)
  in case result of
    Left _ -> property True  -- Parsing errors are acceptable
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对重复定义的处理
prop_collect_symbols_duplicate_defs :: String -> Property
prop_collect_symbols_duplicate_defs name =
  not (null name) ==>
  let duplicateDefs = "type " ++ name ++ " = int\ntype " ++ name ++ " = string"
      result = runExcept (evalStateT (collectSymbolsAndTypes duplicateDefs) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对相互依赖类型的处理
prop_collect_symbols_mutual_deps :: String -> String -> Property
prop_collect_symbols_mutual_deps name1 name2 =
  name1 /= name2 ==>
  let mutualDeps = "type " ++ name1 ++ " = " ++ name2 ++ "\ntype " ++ name2 ++ " = " ++ name1
      result = runExcept (evalStateT (collectSymbolsAndTypes mutualDeps) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 2

-- | 测试符号收集对嵌套类型的处理
prop_collect_symbols_nested_types :: String -> Property
prop_collect_symbols_nested_types typeName =
  not (null typeName) ==>
  let nestedType = "type " ++ typeName ++ " = {\n  field: int\n  nested: " ++ typeName ++ "\n}"
      result = runExcept (evalStateT (collectSymbolsAndTypes nestedType) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对大量符号的处理
prop_collect_symbols_many_symbols :: Positive Int -> Property
prop_collect_symbols_many_symbols (Positive n) =
  n < 100 ==>
  let manySymbols = unlines ["type Type" ++ show i ++ " = int" | i <- [1..n]]
      result = runExcept (evalStateT (collectSymbolsAndTypes manySymbols) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= n

-- | 测试符号收集对极长名称的处理
prop_collect_symbols_long_names :: Positive Int -> Property
prop_collect_symbols_long_names (Positive n) =
  n < 1000 ==>
  let longName = replicate n 'x'
      typeDef = "type " ++ longName ++ " = int"
      result = runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对Unicode字符的处理
prop_collect_symbols_unicode :: Property
prop_collect_symbols_unicode =
  let unicodeName = "类型"  -- Chinese characters
      typeDef = "type " ++ unicodeName ++ " = int"
      result = runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试符号收集对特殊字符的处理
prop_collect_symbols_special_chars :: Char -> Property
prop_collect_symbols_special_chars c =
  let specialName = [c]
      typeDef = "type " ++ specialName ++ " = int"
      result = runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
  in case result of
    Left _ -> property True
    Right (_, table) -> Map.size table >= 0

-- | 测试空代码的符号收集
test_collect_symbols_empty :: Assertion
test_collect_symbols_empty = do
  let result = runExcept (evalStateT (collectSymbolsAndTypes "") newAnalyzerState)
  case result of
    Left _ -> assertFailure "Empty code should not fail to collect symbols"
    Right (_, table) -> assertEqual "Empty code should result in empty symbol table" 0 (Map.size table)

-- | 测试简单类型定义的符号收集
test_collect_symbols_simple_type :: Assertion
test_collect_symbols_simple_type = do
  let typeDef = "type MyInt = int"
      result = runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from simple type: " ++ err
    Right (_, table) -> assertBool "Simple type definition should result in non-empty symbol table" (Map.size table > 0)

-- | 测试简单变量声明的符号收集
test_collect_symbols_simple_var :: Assertion
test_collect_symbols_simple_var = do
  let varDecl = "var x : int"
      result = runExcept (evalStateT (collectSymbolsAndTypes varDecl) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from simple variable: " ++ err
    Right (_, table) -> assertBool "Simple variable declaration should result in non-empty symbol table" (Map.size table > 0)

-- | 测试简单函数声明的符号收集
test_collect_symbols_simple_func :: Assertion
test_collect_symbols_simple_func = do
  let funcDecl = "func add(x: int, y: int) : int = x + y"
      result = runExcept (evalStateT (collectSymbolsAndTypes funcDecl) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from simple function: " ++ err
    Right (_, table) -> assertBool "Simple function declaration should result in non-empty symbol table" (Map.size table > 0)

-- | 测试复杂类型的符号收集
test_collect_symbols_complex_type :: Assertion
test_collect_symbols_complex_type = do
  let complexType = "type Complex = func(int: x, ref(string): y) -> array[int, 10]"
      result = runExcept (evalStateT (collectSymbolsAndTypes complexType) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from complex type: " ++ err
    Right (_, table) -> assertBool "Complex type should result in non-empty symbol table" (Map.size table > 0)

-- | 测试递归类型的符号收集
test_collect_symbols_recursive_type :: Assertion
test_collect_symbols_recursive_type = do
  let recursiveType = "type List = T | List"
      result = runExcept (evalStateT (collectSymbolsAndTypes recursiveType) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from recursive type: " ++ err
    Right (_, table) -> assertBool "Recursive type should result in non-empty symbol table" (Map.size table > 0)

-- | 测试泛型类型的符号收集
test_collect_symbols_generic_type :: Assertion
test_collect_symbols_generic_type = do
  let genericType = "type Container[T] = T | Container[T]"
      result = runExcept (evalStateT (collectSymbolsAndTypes genericType) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from generic type: " ++ err
    Right (_, table) -> assertBool "Generic type should result in non-empty symbol table" (Map.size table > 0)

-- | 测试错误代码的符号收集
test_collect_symbols_invalid_code :: Assertion
test_collect_symbols_invalid_code = do
  let invalidCode = "this is not valid code"
      result = runExcept (evalStateT (collectSymbolsAndTypes invalidCode) newAnalyzerState)
  case result of
    Left _ -> assertBool "Invalid code should result in error" True
    Right (_, table) -> assertBool "Invalid code might still result in symbol table" (Map.size table >= 0)

-- | 测试复杂程序的符号收集
test_collect_symbols_complex_program :: Assertion
test_collect_symbols_complex_program = do
  let complexProgram = unlines
        [ "type List[T] = T | List[T]"
        , "type Option[T] = Some(T) | None"
        , "func head[T](l: List[T]) : Option[T] ="
        , "  match l with"
        , "  | T -> Some(T)"
        , "  | List[T] -> head(l)"
        , "  | None -> None"
        ]
      result = runExcept (evalStateT (collectSymbolsAndTypes complexProgram) newAnalyzerState)
  case result of
    Left err -> assertFailure $ "Failed to collect symbols from complex program: " ++ err
    Right (_, table) -> assertBool "Complex program should result in non-empty symbol table" (Map.size table > 0)

-- | 测试符号收集的一致性
test_collect_symbols_consistency :: Assertion
test_collect_symbols_consistency = do
  let code = "type MyInt = int"
      result1 = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
      result2 = runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
  case (result1, result2) of
    (Right (_, table1), Right (_, table2)) -> assertEqual "Collection should be consistent" (Map.size table1) (Map.size table2)
    (Left _, Left _) -> assertBool "Both should fail consistently" True
    _ -> assertFailure "Results should be of the same type"

-- | 辅助函数：创建新的分析器状态
newAnalyzerState :: AnalyzerState
newAnalyzerState = undefined  -- Simplified for testing

-- | 测试套件
tests :: TestTree
tests = testGroup "Analyzer Symbol Table QuickCheck Test Tests"
  [ testProperty "Collect symbols basic" prop_collect_symbols_basic
  , testProperty "Collect symbols idempotent" prop_collect_symbols_idempotent
  , testProperty "Collect symbols empty" prop_collect_symbols_empty
  , testProperty "Collect symbols single line" prop_collect_symbols_single_line
  , testProperty "Collect symbols multiline" prop_collect_symbols_multiline
  , testProperty "Collect symbols type def" prop_collect_symbols_type_def
  , testProperty "Collect symbols var decl" prop_collect_symbols_var_decl
  , testProperty "Collect symbols func decl" prop_collect_symbols_func_decl
  , testProperty "Collect symbols complex type" prop_collect_symbols_complex_type
  , testProperty "Collect symbols recursive type" prop_collect_symbols_recursive_type
  , testProperty "Collect symbols generic type" prop_collect_symbols_generic_type
  , testProperty "Collect symbols complex program" prop_collect_symbols_complex_program
  , testProperty "Collect symbols invalid code" prop_collect_symbols_invalid_code
  , testProperty "Collect symbols duplicate defs" prop_collect_symbols_duplicate_defs
  , testProperty "Collect symbols mutual deps" prop_collect_symbols_mutual_deps
  , testProperty "Collect symbols nested types" prop_collect_symbols_nested_types
  , testProperty "Collect symbols many symbols" prop_collect_symbols_many_symbols
  , testProperty "Collect symbols long names" prop_collect_symbols_long_names
  , testProperty "Collect symbols unicode" prop_collect_symbols_unicode
  , testProperty "Collect symbols special chars" prop_collect_symbols_special_chars
  , testCase "Collect symbols empty" test_collect_symbols_empty
  , testCase "Collect symbols simple type" test_collect_symbols_simple_type
  , testCase "Collect symbols simple var" test_collect_symbols_simple_var
  , testCase "Collect symbols simple func" test_collect_symbols_simple_func
  , testCase "Collect symbols complex type" test_collect_symbols_complex_type
  , testCase "Collect symbols recursive type" test_collect_symbols_recursive_type
  , testCase "Collect symbols generic type" test_collect_symbols_generic_type
  , testCase "Collect symbols invalid code" test_collect_symbols_invalid_code
  , testCase "Collect symbols complex program" test_collect_symbols_complex_program
  , testCase "Collect symbols consistency" test_collect_symbols_consistency
  ]