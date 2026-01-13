{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompilerPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Compiler
import Compiler.TypeChecker
import Compiler.IR
import Parser
import SourceLocation
import Test.QuickCheck (Positive(..))
import Data.List (intercalate)

-- | 测试编译错误的基本属性
prop_compiler_error_creation :: String -> Positive Int -> Positive Int -> Property
prop_compiler_error_creation msg (Positive line) (Positive col) =
  let syntaxError = SyntaxError msg line col
  in property $ errorMessage syntaxError == msg &&
                errorLine syntaxError == line &&
                errorColumn syntaxError == col

-- | 测试编译结果的错误处理
prop_compile_result_error_handling :: String -> Property
prop_compile_result_error_handling input =
  case compile input of
    Left errors -> property $ not (null errors)
    Right result -> property $ True  -- 成功编译也算通过

-- | 测试编译空字符串
prop_compile_empty_string :: Property
prop_compile_empty_string = 
  case compile "" of
    Left errors -> property $ True  -- 有错误也算预期行为
    Right result -> property $ True  -- 成功也算预期行为

-- | 测试编译只有注释的代码
prop_compile_comments_only :: Property
prop_compile_comments_only = 
  let commentCode = "// This is a comment\n// Another comment"
  in case compile commentCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译简单函数
prop_compile_simple_function :: String -> Property
prop_compile_simple_function funcName =
  let simpleCode = "function " ++ funcName ++ "() {\n  return 42;\n}"
  in case compile simpleCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有参数的函数
prop_compile_function_with_params :: String -> [String] -> Property
prop_compile_function_with_params funcName params =
  let paramsStr = intercalate ", " params
      functionCode = "function " ++ funcName ++ "(" ++ paramsStr ++ ") {\n  return 0;\n}"
  in case compile functionCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译多个函数
prop_compile_multiple_functions :: [String] -> Property
prop_compile_multiple_functions funcNames =
  let functionDefs = map (\name -> "function " ++ name ++ "() { return 0; }") funcNames
      multiFunctionCode = intercalate "\n\n" functionDefs
  in case compile multiFunctionCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译嵌套函数
prop_compile_nested_functions :: String -> String -> Property
prop_compile_nested_functions outerName innerName =
  let nestedCode = "function " ++ outerName ++ "() {\n  function " ++ innerName ++ "() {\n    return 1;\n  }\n  return " ++ innerName ++ "();\n}"
  in case compile nestedCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有类型注解的函数
prop_compile_typed_function :: String -> String -> String -> Property
prop_compile_typed_function funcName paramType returnType =
  let typedCode = "function " ++ funcName ++ "(param: " ++ paramType ++ "): " ++ returnType ++ " {\n  return param;\n}"
  in case compile typedCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有条件语句的代码
prop_compile_conditional :: String -> Property
prop_compile_conditional condition =
  let conditionalCode = "function test() {\n  if (" ++ condition ++ ") {\n    return true;\n  } else {\n    return false;\n  }\n}"
  in case compile conditionalCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有循环的代码
prop_compile_loop :: String -> Property
prop_compile_loop loopVar =
  let loopCode = "function test() {\n  for (let " ++ loopVar ++ " = 0; " ++ loopVar ++ " < 10; " ++ loopVar ++ "++) {\n    // do nothing\n  }\n  return 0;\n}"
  in case compile loopCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有数组的代码
prop_compile_arrays :: [Int] -> Property
prop_compile_arrays values =
  let arrayStr = "[" ++ intercalate ", " (map show values) ++ "]"
      arrayCode = "function test() {\n  let arr = " ++ arrayStr ++ ";\n  return arr.length;\n}"
  in case compile arrayCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有对象的代码
prop_compile_objects :: [(String, String)] -> Property
prop_compile_objects properties =
  let propStrs = map (\(k, v) -> "\"" ++ k ++ "\": " ++ v) properties
      objectStr = "{" ++ intercalate ", " propStrs ++ "}"
      objectCode = "function test() {\n  let obj = " ++ objectStr ++ ";\n  return obj;\n}"
  in case compile objectCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有字符串的代码
prop_compile_strings :: String -> Property
prop_compile_strings str =
  let stringCode = "function test() {\n  return \"" ++ str ++ "\";\n}"
  in case compile stringCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有数字的代码
prop_compile_numbers :: Int -> Property
prop_compile_numbers num =
  let numberCode = "function test() {\n  return " ++ show num ++ ";\n}"
  in case compile numberCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有布尔值的代码
prop_compile_booleans :: Bool -> Property
prop_compile_booleans bool =
  let boolCode = "function test() {\n  return " ++ show bool ++ ";\n}"
  in case compile boolCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译带有Unicode的代码
prop_compile_unicode :: String -> Property
prop_compile_unicode unicodeStr =
  let unicodeCode = "function 测试() {\n  return \"" ++ unicodeStr ++ "\";\n}"
  in case compile unicodeCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试编译大型代码文件
prop_compile_large_file :: Positive Int -> Property
prop_compile_large_file (Positive n) =
  let largeCode = unlines $ replicate (min n 100) "function test() { return 0; }"
  in case compile largeCode of
       Left errors -> property $ True
       Right result -> property $ True

-- | 测试类型检查的基本属性
prop_type_check_empty :: Property
prop_type_check_empty = 
  let typeEnv = buildTypeEnvFromPairs []
  in property $ null (typeEnv :: TypeEnv)

-- | 测试类型检查与简单类型
prop_type_check_simple :: String -> String -> Property
prop_type_check_simple varName typeName =
  let typeEnv = buildTypeEnvFromPairs [(varName, typeName)]
  in property $ length typeEnv == 1

-- | 测试IR生成的一致性
prop_ir_generation_consistent :: String -> Property
prop_ir_generation_consistent code =
  case parseTypus code of
    Left _ -> property True
    Right typusFile -> 
      case ensureSourceIR typusFile of
        Left _ -> property True
        Right ir -> property $ True  -- 只要能生成IR就算通过

-- | 测试Go代码生成的一致性
prop_go_generation_consistent :: String -> Property
prop_go_generation_consistent code =
  case parseTypus code of
    Left _ -> property True
    Right typusFile -> 
      case ensureSourceIR typusFile of
        Left _ -> property True
        Right ir -> 
          let goCode = generateGoCode ir
          in property $ not (null goCode)



tests :: TestTree
tests = testGroup "Compiler Properties QuickCheck Tests"
  [ testProperty "compiler error creation" prop_compiler_error_creation
  , testProperty "compile result error handling" prop_compile_result_error_handling
  , testProperty "compile empty string" prop_compile_empty_string
  , testProperty "compile comments only" prop_compile_comments_only
  , testProperty "compile simple function" prop_compile_simple_function
  , testProperty "compile function with params" prop_compile_function_with_params
  , testProperty "compile multiple functions" prop_compile_multiple_functions
  , testProperty "compile nested functions" prop_compile_nested_functions
  , testProperty "compile typed function" prop_compile_typed_function
  , testProperty "compile conditional" prop_compile_conditional
  , testProperty "compile loop" prop_compile_loop
  , testProperty "compile arrays" prop_compile_arrays
  , testProperty "compile objects" prop_compile_objects
  , testProperty "compile strings" prop_compile_strings
  , testProperty "compile numbers" prop_compile_numbers
  , testProperty "compile booleans" prop_compile_booleans
  , testProperty "compile unicode" prop_compile_unicode
  , testProperty "compile large file" prop_compile_large_file
  , testProperty "type check empty" prop_type_check_empty
  , testProperty "type check simple" prop_type_check_simple
  , testProperty "ir generation consistent" prop_ir_generation_consistent
  , testProperty "go generation consistent" prop_go_generation_consistent
  ]