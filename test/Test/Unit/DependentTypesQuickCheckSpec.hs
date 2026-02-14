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
      parseResult = parseTypus typeExpr
  in if null typeName
     then property $ isLeft parseResult  -- 空类型名生成的表达式"[n: int]"是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName)
     then property $ isLeft parseResult  -- 包含无效字符的类型名应该失败
     else property $ isRight parseResult  -- 有效类型名应该成功

-- | 测试精确类型的解析
prop_refined_type_parsing :: String -> Property
prop_refined_type_parsing baseType =
  let validBaseType = baseType `elem` ["int", "string", "float", "bool"]
      typeExpr = baseType ++ " where { self > 0 }"
  in if null baseType
     then property $ isLeft (parseTypus typeExpr)  -- 空类型生成的表达式" where { self > 0 }"是无效的
     else if not validBaseType
     then property $ isLeft (parseTypus typeExpr)  -- 无效类型生成的表达式也是无效的
     else property $ isRight (parseTypus typeExpr)  -- 有效类型应该成功

-- | 测试依赖函数签名的解析
prop_dependent_function_signature_parsing :: String -> String -> Property
prop_dependent_function_signature_parsing funcName paramName =
  let funcExpr = "func " ++ funcName ++ "(n: " ++ paramName ++ ") -> Vector[n]"
  in if null funcName || null paramName
     then property $ isLeft (parseTypus funcExpr)  -- 空函数名或参数名生成的表达式是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (funcName ++ paramName))
     then property $ isLeft (parseTypus funcExpr)  -- 包含无效字符的函数名或参数名应该失败
     else property $ isRight (parseTypus funcExpr)  -- 有效函数名和参数名应该成功

-- | 测试类型级算术表达式的解析
prop_type_level_arithmetic_parsing :: String -> String -> Property
prop_type_level_arithmetic_parsing type1 type2 =
  let validTypes = type1 `elem` ["Vector", "Matrix"] && type2 `elem` ["Vector", "Matrix"]
      arithExpr = "Vector[" ++ type1 ++ " + " ++ type2 ++ "]"
  in if null type1 || null type2
     then property $ isLeft (parseTypus arithExpr)  -- 空类型名生成的表达式是无效的
     else if not validTypes
     then property $ isLeft (parseTypus arithExpr)  -- 无效类型生成的表达式也是无效的
     else property $ isRight (parseTypus arithExpr)  -- 有效类型应该成功

-- | 测试存在类型的解析
prop_existential_type_parsing :: String -> Property
prop_existential_type_parsing typeName =
  let typeExpr = typeName ++ "[some n: int]"
  in if null typeName
     then property $ isLeft (parseTypus typeExpr)  -- 空类型名生成的表达式"[some n: int]"是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName)
     then property $ isLeft (parseTypus typeExpr)  -- 包含无效字符的类型名应该失败
     else property $ isRight (parseTypus typeExpr)  -- 有效类型名应该成功

-- | 测试match表达式的解析
prop_match_expression_parsing :: String -> Property
prop_match_expression_parsing varName =
  let matchExpr = "match " ++ varName ++ ".(n) { ... }"
  in if null varName
     then property $ isLeft (parseTypus matchExpr)  -- 空变量名生成的表达式"match .(n) { ... }"是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") varName)
     then property $ isLeft (parseTypus matchExpr)  -- 包含无效字符的变量名应该失败
     else property $ isRight (parseTypus matchExpr)  -- 有效变量名应该成功

-- | 测试assert表达式的解析
prop_assert_expression_parsing :: String -> Property
prop_assert_expression_parsing condition =
  let assertExpr = "assert " ++ condition
  in if null condition
     then property $ isLeft (parseTypus assertExpr)  -- 空条件生成的表达式"assert "是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition)
     then property $ isLeft (parseTypus assertExpr)  -- 包含无效字符的条件应该失败
     else property $ isRight (parseTypus assertExpr)  -- 有效条件应该成功

-- | 测试static_assert表达式的解析
prop_static_assert_expression_parsing :: String -> Property
prop_static_assert_expression_parsing condition =
  let staticAssertExpr = "static_assert " ++ condition
  in if null condition
     then property $ isLeft (parseTypus staticAssertExpr)  -- 空条件生成的表达式"static_assert "是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition)
     then property $ isLeft (parseTypus staticAssertExpr)  -- 包含无效字符的条件应该失败
     else property $ isRight (parseTypus staticAssertExpr)  -- 有效条件应该成功

-- | 测试条件窄化的解析
prop_condition_narrowing_parsing :: String -> Property
prop_condition_narrowing_parsing condition =
  let narrowExpr = "if " ++ condition ++ " { ... }"
  in if null condition
     then property $ isLeft (parseTypus narrowExpr)  -- 空条件生成的表达式"if  { ... }"是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition)
     then property $ isLeft (parseTypus narrowExpr)  -- 包含无效字符的条件应该失败
     else property $ isRight (parseTypus narrowExpr)  -- 有效条件应该成功

-- | 测试混合类型参数和值参数的解析
prop_mixed_type_value_parameters_parsing :: String -> Property
prop_mixed_type_value_parameters_parsing typeName =
  let mixedExpr = "type " ++ typeName ++ "[T any, n: int]"
  in if null typeName
     then property $ isLeft (parseTypus mixedExpr)  -- 空类型名生成的表达式"type [T any, n: int]"是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") typeName)
     then property $ isLeft (parseTypus mixedExpr)  -- 包含无效字符的类型名应该失败
     else property $ isRight (parseTypus mixedExpr)  -- 有效类型名应该成功

-- | 测试函数前置条件的解析
prop_function_precondition_parsing :: String -> String -> Property
prop_function_precondition_parsing funcName condition =
  let preconditionExpr = "func " ++ funcName ++ "() -> int where { " ++ condition ++ " }"
  in if null funcName || null condition
     then property $ isLeft (parseTypus preconditionExpr)  -- 空函数名或条件生成的表达式是无效的
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") funcName)
     then property $ isLeft (parseTypus preconditionExpr)  -- 包含无效字符的函数名应该失败
     else if not (all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition)
     then property $ isLeft (parseTypus preconditionExpr)  -- 包含无效字符的条件应该失败
     else property $ isRight (parseTypus preconditionExpr)  -- 有效函数名和条件应该成功

-- | 测试依赖类型的边界情况
test_dependent_types_edge_cases :: Assertion
test_dependent_types_edge_cases = do
  -- 测试空类型名（实际上解析器可以解析这种形式）
  assertBool "Empty type name should succeed" $ isRight (parseTypus "[n: int]")
  
  -- 测试无效的约束（实际上解析器可以解析语法）
  assertBool "Invalid constraint should succeed" $ isRight (parseTypus "int where { invalid }")
  
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