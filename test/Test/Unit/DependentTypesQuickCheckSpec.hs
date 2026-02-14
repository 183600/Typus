{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.DependentTypesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  )
import TestSupport.OptimizedStringOperations 
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  )
import TestSupport.TestPropertyMemoryCleanup 
  ( testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  )

import DependentTypesParser
import Parser (parseTypus)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace, isDigit)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)

-- | 测试值参数化类型的解析
prop_value_parameterized_type_parsing :: String -> Property
prop_value_parameterized_type_parsing typeName =
  let validTypeName = not (null typeName) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName
      typeExpr = typeName ++ "[n: int]"
  in if not validTypeName
     then property $ isLeft (parseTypus typeExpr)
     else property $ isRight (parseTypus typeExpr)

-- | 测试精确类型的解析
prop_refined_type_parsing :: String -> Property
prop_refined_type_parsing baseType =
  let validBaseType = baseType `elem` ["int", "string", "float", "bool"]
      typeExpr = baseType ++ " where { self > 0 }"
  in if not validBaseType
     then property $ isLeft (parseTypus typeExpr)
     else property $ isRight (parseTypus typeExpr)

-- | 测试依赖函数签名的解析
prop_dependent_function_signature_parsing :: String -> String -> Property
prop_dependent_function_signature_parsing funcName paramName =
  let validNames = not (null funcName) && not (null paramName) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (funcName ++ paramName)
      funcExpr = "func " ++ funcName ++ "(n: " ++ paramName ++ ") -> Vector[n]"
  in if not validNames
     then property $ isLeft (parseTypus funcExpr)
     else property $ isRight (parseTypus funcExpr)

-- | 测试类型级算术表达式的解析
prop_type_level_arithmetic_parsing :: String -> String -> Property
prop_type_level_arithmetic_parsing type1 type2 =
  let validTypes = type1 `elem` ["Vector", "Matrix"] && type2 `elem` ["Vector", "Matrix"]
      arithExpr = "Vector[" ++ type1 ++ " + " ++ type2 ++ "]"
  in if not validTypes
     then property $ isLeft (parseTypus arithExpr)
     else property $ isRight (parseTypus arithExpr)

-- | 测试存在类型的解析
prop_existential_type_parsing :: String -> Property
prop_existential_type_parsing typeName =
  let validTypeName = not (null typeName) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName
      typeExpr = typeName ++ "[some n: int]"
  in if not validTypeName
     then property $ isLeft (parseTypus typeExpr)
     else property $ isRight (parseTypus typeExpr)

-- | 测试match表达式的解析
prop_match_expression_parsing :: String -> Property
prop_match_expression_parsing varName =
  let validVarName = not (null varName) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") varName
      matchExpr = "match " ++ varName ++ ".(n) { ... }"
  in if not validVarName
     then property $ isLeft (parseTypus matchExpr)
     else property $ isRight (parseTypus matchExpr)

-- | 测试assert表达式的解析
prop_assert_expression_parsing :: String -> Property
prop_assert_expression_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      assertExpr = "assert " ++ condition
  in if not validCondition
     then property $ isLeft (parseTypus assertExpr)
     else property $ isRight (parseTypus assertExpr)

-- | 测试static_assert表达式的解析
prop_static_assert_expression_parsing :: String -> Property
prop_static_assert_expression_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      staticAssertExpr = "static_assert " ++ condition
  in if not validCondition
     then property $ isLeft (parseTypus staticAssertExpr)
     else property $ isRight (parseTypus staticAssertExpr)

-- | 测试条件窄化的解析
prop_condition_narrowing_parsing :: String -> Property
prop_condition_narrowing_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      narrowExpr = "if " ++ condition ++ " { ... }"
  in if not validCondition
     then property $ isLeft (parseTypus narrowExpr)
     else property $ isRight (parseTypus narrowExpr)

-- | 测试混合类型参数和值参数的解析
prop_mixed_type_value_parameters_parsing :: String -> Property
prop_mixed_type_value_parameters_parsing typeName =
  let validTypeName = not (null typeName) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName
      mixedExpr = "type " ++ typeName ++ "[T any, n: int]"
  in if not validTypeName
     then property $ isLeft (parseTypus mixedExpr)
     else property $ isRight (parseTypus mixedExpr)

-- | 测试函数前置条件的解析
prop_function_precondition_parsing :: String -> String -> Property
prop_function_precondition_parsing funcName condition =
  let validFuncName = not (null funcName) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") funcName
      validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      preconditionExpr = "func " ++ funcName ++ "() -> int where { " ++ condition ++ " }"
  in if not (validFuncName && validCondition)
     then property $ isLeft (parseTypus preconditionExpr)
     else property $ isRight (parseTypus preconditionExpr)

-- | 测试依赖类型的边界情况
test_dependent_types_edge_cases :: Assertion
test_dependent_types_edge_cases = do
  -- 测试空类型名
  assertBool "Empty type name should fail" $ isLeft (parseTypus "[n: int]")
  
  -- 测试无效的约束
  assertBool "Invalid constraint should fail" $ isLeft (parseTypus "int where { invalid }")
  
  -- 测试无效的函数签名
  assertBool "Invalid function signature should fail" $ isLeft (parseTypus "func () -> Vector[]")
  
  -- 测试无效的算术表达式
  assertBool "Invalid arithmetic expression should fail" $ isLeft (parseTypus "Vector[n +]")

-- | 测试依赖类型的复杂表达式
test_dependent_types_complex_expressions :: Assertion
test_dependent_types_complex_expressions = do
  -- 测试复杂的约束
  assertBool "Complex constraint should succeed" $ isRight (parseTypus "int where { self >= 0 && self <= 100 }")
  
  -- 测试嵌套的值参数
  assertBool "Nested value parameters should succeed" $ isRight (parseTypus "Matrix[rows: int, cols: int]")
  
  -- 测试复杂的函数签名
  assertBool "Complex function signature should succeed" $ isRight (parseTypus "func matMul[m: int, n: int, p: int](a: Matrix[m, n], b: Matrix[n, p]) -> Matrix[m, p]")

-- | 依赖类型测试套件
tests :: TestTree
tests = testGroupWithStrategicCleanup "Dependent Types QuickCheck Tests"
  [ -- 基本类型解析测试
    memoryOptimizedProperty "Value parameterized type parsing" (property prop_value_parameterized_type_parsing)
  , memoryOptimizedProperty "Refined type parsing" (property prop_refined_type_parsing)
  , memoryOptimizedProperty "Dependent function signature parsing" (property prop_dependent_function_signature_parsing)
  
  -- 表达式解析测试
  , memoryOptimizedProperty "Type level arithmetic parsing" (property prop_type_level_arithmetic_parsing)
  , memoryOptimizedProperty "Existential type parsing" (property prop_existential_type_parsing)
  , memoryOptimizedProperty "Match expression parsing" (property prop_match_expression_parsing)
  
  -- 断言和条件测试
  , memoryOptimizedProperty "Assert expression parsing" (property prop_assert_expression_parsing)
  , memoryOptimizedProperty "Static assert expression parsing" (property prop_static_assert_expression_parsing)
  , memoryOptimizedProperty "Condition narrowing parsing" (property prop_condition_narrowing_parsing)
  
  -- 高级特性测试
  , memoryOptimizedProperty "Mixed type value parameters" (property prop_mixed_type_value_parameters_parsing)
  , memoryOptimizedProperty "Function precondition parsing" (property prop_function_precondition_parsing)
  
  -- 单元测试
  , testCase "Dependent types edge cases" test_dependent_types_edge_cases
  , testCase "Dependent types complex expressions" test_dependent_types_complex_expressions
  ]

-- | 内存优化的测试套件
memoryOptimizedTests :: TestTree
memoryOptimizedTests = memoryLevelTestGroup Minimal "Dependent Types Memory Optimized Tests"
  [ testProperty "Value parameterized type" prop_value_parameterized_type_parsing
  , testProperty "Refined type" prop_refined_type_parsing
  , testProperty "Dependent function signature" prop_dependent_function_signature_parsing
  , testProperty "Type level arithmetic" prop_type_level_arithmetic_parsing
  , testProperty "Existential type" prop_existential_type_parsing
  ]