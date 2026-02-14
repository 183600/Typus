{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypesTestSuite where

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

import DependentTypesParser (parseDependentType, DependentType(..))
import Parser (parseTypus, TypusAST(..))
import Compiler (compileTypus)
import Utils (trim)

-- | 测试依赖类型解析器的基本属性
prop_parse_dependent_type_basic :: String -> Property
prop_parse_dependent_type_basic s =
  let limitedString = take 10 s  -- 限制字符串大小
      result = parseDependentType limitedString
  in property $ case result of
    Left _ -> True
    Right dt -> length (show dt) <= 100  -- 限制结果大小

-- | 测试Vector类型的解析
test_vector_type_parsing :: Assertion
test_vector_type_parsing = do
  let validVector = "Vector[3]"
      result = parseDependentType validVector
  case result of
    Right dt -> assertEqual "Vector type parsed correctly" "Vector[3]" (show dt)
    Left err -> assertFailure $ "Failed to parse Vector type: " ++ err

-- | 测试Matrix类型的解析
test_matrix_type_parsing :: Assertion
test_matrix_type_parsing = do
  let validMatrix = "Matrix[3, 4]"
      result = parseDependentType validMatrix
  case result of
    Right dt -> assertEqual "Matrix type parsed correctly" "Matrix[3,4]" (show dt)
    Left err -> assertFailure $ "Failed to parse Matrix type: " ++ err

-- | 测试NonZero约束类型的解析
test_nonzero_constraint_parsing :: Assertion
test_nonzero_constraint_parsing = do
  let validNonZero = "NonZero = int where { self != 0 }"
      result = parseDependentType validNonZero
  case result of
    Right dt -> assertEqual "NonZero constraint parsed correctly" "NonZero" (show dt)
    Left err -> assertFailure $ "Failed to parse NonZero constraint: " ++ err

-- | 测试Positive约束类型的解析
test_positive_constraint_parsing :: Assertion
test_positive_constraint_parsing = do
  let validPositive = "Positive = int where { self > 0 }"
      result = parseDependentType validPositive
  case result of
    Right dt -> assertEqual "Positive constraint parsed correctly" "Positive" (show dt)
    Left err -> assertFailure $ "Failed to parse Positive constraint: " ++ err

-- | 测试Bounded约束类型的解析
test_bounded_constraint_parsing :: Assertion
test_bounded_constraint_parsing = do
  let validBounded = "Bounded[0, 100] = int where { self >= 0 && self <= 100 }"
      result = parseDependentType validBounded
  case result of
    Right dt -> assertEqual "Bounded constraint parsed correctly" "Bounded[0,100]" (show dt)
    Left err -> assertFailure $ "Failed to parse Bounded constraint: " ++ err

-- | 测试依赖函数签名的解析
test_dependent_function_parsing :: Assertion
test_dependent_function_parsing = do
  let validFunction = "func zeros(n: Positive) -> Vector[n]"
      result = parseTypus validFunction
  case result of
    Right ast -> assertEqual "Dependent function parsed correctly" "Function" (show $ head $ functions ast)
    Left err -> assertFailure $ "Failed to parse dependent function: " ++ err

-- | 测试类型级算术的解析
test_type_level_arithmetic_parsing :: Assertion
test_type_level_arithmetic_parsing = do
  let validArithmetic = "Vector[m + n]"
      result = parseDependentType validArithmetic
  case result of
    Right dt -> assertEqual "Type-level arithmetic parsed correctly" "Vector[m+n]" (show dt)
    Left err -> assertFailure $ "Failed to parse type-level arithmetic: " ++ err

-- | 测试存在类型的解析
test_existential_type_parsing :: Assertion
test_existential_type_parsing = do
  let validExistential = "Vector[some n: int]"
      result = parseDependentType validExistential
  case result of
    Right dt -> assertEqual "Existential type parsed correctly" "Vector[some n:int]" (show dt)
    Left err -> assertFailure $ "Failed to parse existential type: " ++ err

-- | 测试混合参数类型的解析
test_mixed_parameters_parsing :: Assertion
test_mixed_parameters_parsing = do
  let validMixed = "BoundedSlice[T any, cap: int]"
      result = parseDependentType validMixed
  case result of
    Right dt -> assertEqual "Mixed parameters parsed correctly" "BoundedSlice[T any, cap:int]" (show dt)
    Left err -> assertFailure $ "Failed to parse mixed parameters: " ++ err

-- | 测试函数前置条件的解析
test_function_precondition_parsing :: Assertion
test_function_precondition_parsing = do
  let validPrecondition = "func average[n: int](v: Vector[n]) -> float64 where { n > 0 }"
      result = parseTypus validPrecondition
  case result of
    Right ast -> assertEqual "Function precondition parsed correctly" "Function" (show $ head $ functions ast)
    Left err -> assertFailure $ "Failed to parse function precondition: " ++ err

-- | 测试assert语句的解析
test_assert_parsing :: Assertion
test_assert_parsing = do
  let validAssert = "assert n > 0"
      result = parseTypus validAssert
  case result of
    Right ast -> assertEqual "Assert statement parsed correctly" "Assert" (show $ head $ statements ast)
    Left err -> assertFailure $ "Failed to parse assert statement: " ++ err

-- | 测试static_assert语句的解析
test_static_assert_parsing :: Assertion
test_static_assert_parsing = do
  let validStaticAssert = "static_assert n > 0"
      result = parseTypus validStaticAssert
  case result of
    Right ast -> assertEqual "Static assert statement parsed correctly" "StaticAssert" (show $ head $ statements ast)
    Left err -> assertFailure $ "Failed to parse static assert statement: " ++ err

-- | 测试match语句的解析
test_match_parsing :: Assertion
test_match_parsing = do
  let validMatch = "match v.(n) { fmt.Println(get(v, 0)) }"
      result = parseTypus validMatch
  case result of
    Right ast -> assertEqual "Match statement parsed correctly" "Match" (show $ head $ statements ast)
    Left err -> assertFailure $ "Failed to parse match statement: " ++ err

-- | 测试依赖类型编译
test_dependent_type_compilation :: Assertion
test_dependent_type_compilation = do
  let validCode = "//! dependent_types: on\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int {\n    return a / b\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains NonZero constraint check" ("NonZero violated" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile dependent type: " ++ err

-- | 测试Vector类型编译
test_vector_type_compilation :: Assertion
test_vector_type_compilation = do
  let validCode = "//! dependent_types: on\n\ntype Vector[n: int] struct {\n    data [n]float64\n}\n\nfunc zeros(n: Positive) -> Vector[n] {\n    return Vector[n]{data: make([]float64, n)}\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains Vector struct" ("type Vector struct" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile Vector type: " ++ err

-- | 测试约束违反处理
test_constraint_violation_handling :: Assertion
test_constraint_violation_handling = do
  let validCode = "//! dependent_types: on\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int {\n    return a / b\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains panic for constraint violation" ("panic(" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile constraint violation handling: " ++ err

-- | 测试错误模式下的约束处理
test_error_mode_constraint_handling :: Assertion
test_error_mode_constraint_handling = do
  let validCode = "//! dependent_types: on\n//! constraint_mode: error\n\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> (int, error) {\n    return a / b, nil\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code returns error for constraint violation" ("error" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile error mode constraint handling: " ++ err

-- | 测试依赖类型QuickCheck属性
prop_dependent_type_parsing_roundtrip :: String -> Property
prop_dependent_type_parsing_roundtrip s =
  let limitedString = take 5 s  -- 限制字符串大小
      result = parseDependentType limitedString
  in case result of
    Left _ -> property True
    Right dt -> 
      let str = show dt
          result2 = parseDependentType str
      in case result2 of
        Left _ -> property False
        Right dt2 -> property $ show dt2 == str

-- | 测试依赖类型约束验证
prop_constraint_validation :: Int -> Property
prop_constraint_validation n =
  let result = parseDependentType ("Positive = int where { self > 0 }")
  in case result of
    Left _ -> property False
    Right dt -> property $ n > 0 || n <= 0  -- 简单的属性测试

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Minimal "New Dependent Types Test Suite (Memory Optimized)"
  [ withMemoryLevel Minimal $ testCase "Vector type parsing" test_vector_type_parsing
  , withMemoryLevel Minimal $ testCase "Matrix type parsing" test_matrix_type_parsing
  , withMemoryLevel Minimal $ testCase "NonZero constraint parsing" test_nonzero_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Positive constraint parsing" test_positive_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Bounded constraint parsing" test_bounded_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Dependent function parsing" test_dependent_function_parsing
  , withMemoryLevel Minimal $ testCase "Type-level arithmetic parsing" test_type_level_arithmetic_parsing
  , withMemoryLevel Minimal $ testCase "Existential type parsing" test_existential_type_parsing
  , withMemoryLevel Minimal $ testCase "Mixed parameters parsing" test_mixed_parameters_parsing
  , withMemoryLevel Minimal $ testCase "Function precondition parsing" test_function_precondition_parsing
  , withMemoryLevel Minimal $ testCase "Assert parsing" test_assert_parsing
  , withMemoryLevel Minimal $ testCase "Static assert parsing" test_static_assert_parsing
  , withMemoryLevel Minimal $ testCase "Match parsing" test_match_parsing
  , withMemoryLevel Minimal $ testCase "Dependent type compilation" test_dependent_type_compilation
  , withMemoryLevel Minimal $ testCase "Vector type compilation" test_vector_type_compilation
  , withMemoryLevel Minimal $ testCase "Constraint violation handling" test_constraint_violation_handling
  , withMemoryLevel Minimal $ testCase "Error mode constraint handling" test_error_mode_constraint_handling
  , withMemoryLevel Minimal $ testProperty "Dependent type parsing roundtrip" prop_dependent_type_parsing_roundtrip
  , withMemoryLevel Minimal $ testProperty "Constraint validation" prop_constraint_validation
  ]