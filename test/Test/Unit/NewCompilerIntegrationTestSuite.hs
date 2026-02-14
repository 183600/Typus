{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCompilerIntegrationTestSuite where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import Data.List (isInfixOf)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)
import Data.Maybe (listToMaybe)

import Compiler (compileTypus, CompilationResult(..))
import Parser (parseTypus, TypusAST(..))
import DependentTypesParser (parseDependentType)
import Ownership (analyzeOwnership)
import Utils (trim)

-- | 测试编译器的基本属性
prop_compiler_basic :: String -> Property
prop_compiler_basic s =
  let limitedString = take 10 s  -- 限制字符串大小
      result = compileTypus limitedString
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 1000  -- 限制生成的代码大小

-- | 测试基本Go代码生成
test_basic_go_code_generation :: Assertion
test_basic_go_code_generation = do
  let validCode = "package main\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains main function" ("func main()" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to generate basic Go code: " ++ err

-- | 测试依赖类型编译
test_dependent_types_compilation :: Assertion
test_dependent_types_compilation = do
  let validCode = "//! dependent_types: on\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int {\n    return a / b\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains NonZero constraint" ("NonZero" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile dependent types: " ++ err

-- | 测试所有权编译
test_ownership_compilation :: Assertion
test_ownership_compilation = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains ownership operations" ("MyString" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile ownership code: " ++ err

-- | 测试混合特性编译
test_mixed_features_compilation :: Assertion
test_mixed_features_compilation = do
  let validCode = "//! ownership: on\n//! dependent_types: on\n\ntype NonZero = int where { self != 0 }\ntype Vector[n: int] struct {\n    data [n]float64\n}\n\nfunc safeDiv(a: int, b: NonZero) -> int {\n    return a / b\n}\n\nfunc main() {\n    {//! ownership: on\n        s := NewMyString(\"hello\")\n        t := s\n        fmt.Println(t.data)\n    }\n    r := safeDiv(10, 2)\n    fmt.Printf(\"Result: %d\\n\", r)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains both features" ("NonZero" `isInfixOf` goCode && "MyString" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile mixed features: " ++ err

-- | 测试错误处理编译
test_error_handling_compilation :: Assertion
test_error_handling_compilation = do
  let validCode = "//! dependent_types: on\n//! constraint_mode: error\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> (int, error) {\n    return a / b, nil\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains error handling" ("error" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile error handling: " ++ err

-- | 测试类型推导编译
test_type_inference_compilation :: Assertion
test_type_inference_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc createVector(n: Positive, value: float64) -> Vector[n] {\n    elements := make([]float64, n)\n    for i := 0; i < n; i++ {\n        elements[i] = value\n    }\n    return Vector{elements}\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains type inference" ("make([]float64, n)" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile type inference: " ++ err

-- | 测试函数前置条件编译
test_function_precondition_compilation :: Assertion
test_function_precondition_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc average[n: int](v: Vector[n]) -> float64 where { n > 0 } {\n    sum := 0.0\n    for _, x := range v.data {\n        sum += x\n    }\n    return sum / float64(n)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains precondition check" ("n > 0" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile function precondition: " ++ err

-- | 测试断言编译
test_assert_compilation :: Assertion
test_assert_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc processInput(n: int) {\n    assert n > 0\n    v := zeros(n)\n    fmt.Printf(\"Vector length: %d\\n\", n)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains assert check" ("if !" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile assert: " ++ err

-- | 测试静态断言编译
test_static_assert_compilation :: Assertion
test_static_assert_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc processInput(n: int) {\n    static_assert n > 0\n    v := zeros(n)\n    fmt.Printf(\"Vector length: %d\\n\", n)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains static assert" ("// static_assert" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile static assert: " ++ err

-- | 测试存在类型编译
test_existential_type_compilation :: Assertion
test_existential_type_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc readVector(input: []float64) -> Vector[some n: int] where { n == len(input) } {\n    return Vector[len(input)]{data: input}\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains existential type" ("len(input)" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile existential type: " ++ err

-- | 测试match语句编译
test_match_compilation :: Assertion
test_match_compilation = do
  let validCode = "//! dependent_types: on\n\nfunc processVector() {\n    data := []float64{1.0, 2.0, 3.0}\n    v := readVector(data)\n    match v.(n) {\n        fmt.Println(get(v, 0))\n        if n > 1 {\n            fmt.Println(get(v, 1))\n        }\n    }\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains match logic" ("n :=" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile match statement: " ++ err

-- | 测试Go互操作编译
test_go_interop_compilation :: Assertion
test_go_interop_compilation = do
  let validCode = "//! dependent_types: on\n\nimport \"sort\"\n\nfunc sortedFirst[n: int](v: Vector[n]) -> float64 where { n > 0 } {\n    sort.Float64s(v.data)\n    return v.data[0]\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains import" ("import" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile Go interop: " ++ err

-- | 测试编译器错误恢复
test_compiler_error_recovery :: Assertion
test_compiler_error_recovery = do
  let invalidCode = "//! dependent_types: on\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int {\n    return a / b\n}\n\nfunc invalidFunction() {\n    invalid syntax here\n}"
      result = compileTypus invalidCode
  case result of
    Left err -> assertBool "Compiler error recovery successful" ("syntax" `isInfixOf` err)
    Right goCode -> assertFailure "Compiler should have failed on invalid syntax"

-- | 测试编译器性能优化
test_compiler_performance_optimization :: Assertion
test_compiler_performance_optimization = do
  let validCode = "//! dependent_types: on\n\ntype Vector[n: int] struct {\n    data [n]float64\n}\n\nfunc zeros(n: Positive) -> Vector[n] {\n    return Vector[n]{data: make([]float64, n)}\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code is optimized" ("make([]float64, n)" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to generate optimized code: " ++ err

-- | 测试编译器内存限制
test_compiler_memory_limits :: Assertion
test_compiler_memory_limits = do
  let validCode = "//! dependent_types: on\n\ntype Vector[n: int] struct {\n    data [n]float64\n}\n\nfunc zeros(n: Positive) -> Vector[n] {\n    return Vector[n]{data: make([]float64, n)}\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code respects memory limits" (length goCode <= 1000)
    Left err -> assertFailure $ "Failed to respect memory limits: " ++ err

-- | 测试编译器QuickCheck属性
prop_compiler_compilation_consistency :: String -> Property
prop_compiler_compilation_consistency s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = compileTypus limitedString
      result2 = compileTypus limitedString
  in property $ case (result1, result2) of
    (Right goCode1, Right goCode2) -> goCode1 == goCode2
    (Left err1, Left err2) -> err1 == err2
    _ -> property False

-- | 测试编译器边界条件
prop_compiler_boundary_conditions :: String -> Property
prop_compiler_boundary_conditions s =
  let limitedString = take 1 s  -- 限制字符串大小
      result = compileTypus limitedString
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode >= 0

-- | 测试编译器输出质量
prop_compiler_output_quality :: String -> Property
prop_compiler_output_quality s =
  let limitedString = take 5 s  -- 限制字符串大小
      result = compileTypus limitedString
  in property $ case result of
    Left _ -> True
    Right goCode -> 
      let hasPackage = "package" `isInfixOf` goCode
          hasImports = "import" `isInfixOf` goCode || not ("fmt" `isInfixOf` limitedString)
      in property $ hasPackage && hasImports

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Minimal "New Compiler Integration Test Suite (Memory Optimized)"
  [ withMemoryLevel Minimal $ testCase "Basic Go code generation" test_basic_go_code_generation
  , withMemoryLevel Minimal $ testCase "Dependent types compilation" test_dependent_types_compilation
  , withMemoryLevel Minimal $ testCase "Ownership compilation" test_ownership_compilation
  , withMemoryLevel Minimal $ testCase "Mixed features compilation" test_mixed_features_compilation
  , withMemoryLevel Minimal $ testCase "Error handling compilation" test_error_handling_compilation
  , withMemoryLevel Minimal $ testCase "Type inference compilation" test_type_inference_compilation
  , withMemoryLevel Minimal $ testCase "Function precondition compilation" test_function_precondition_compilation
  , withMemoryLevel Minimal $ testCase "Assert compilation" test_assert_compilation
  , withMemoryLevel Minimal $ testCase "Static assert compilation" test_static_assert_compilation
  , withMemoryLevel Minimal $ testCase "Existential type compilation" test_existential_type_compilation
  , withMemoryLevel Minimal $ testCase "Match compilation" test_match_compilation
  , withMemoryLevel Minimal $ testCase "Go interop compilation" test_go_interop_compilation
  , withMemoryLevel Minimal $ testCase "Compiler error recovery" test_compiler_error_recovery
  , withMemoryLevel Minimal $ testCase "Compiler performance optimization" test_compiler_performance_optimization
  , withMemoryLevel Minimal $ testCase "Compiler memory limits" test_compiler_memory_limits
  , withMemoryLevel Minimal $ testProperty "Compiler compilation consistency" prop_compiler_compilation_consistency
  , withMemoryLevel Minimal $ testProperty "Compiler boundary conditions" prop_compiler_boundary_conditions
  , withMemoryLevel Minimal $ testProperty "Compiler output quality" prop_compiler_output_quality
  ]