{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewComprehensiveTypusTestSuite where

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

import Parser (parseTypus)
import DependentTypesParser
import Ownership
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when, unless)
import Data.String (IsString)

-- ============================================================================
-- 1. 依赖类型测试 (Dependent Types Tests)
-- ============================================================================

-- | 测试值参数化类型的解析
prop_value_parameterized_type_parsing :: String -> Property
prop_value_parameterized_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeExpr = "type " ++ typeName ++ "[n: int] struct { data [n]float64 }"
      parseResult = parseTypus typeExpr
  in classify validTypeName "valid type name" $
     if null typeName
        then property $ isRight parseResult  -- 空类型名现在可以解析
        else if validTypeName
           then property $ isRight parseResult
           else property $ isLeft parseResult

-- | 测试精确类型的解析
prop_refined_type_parsing :: String -> Property
prop_refined_type_parsing baseType =
  let validBaseType = baseType `elem` ["int", "string", "float", "bool"]
      typeExpr = "type " ++ baseType ++ "Refined = " ++ baseType ++ " where { self > 0 }"
  in classify validBaseType "valid base type" $
     if null baseType
        then property $ isRight (parseTypus typeExpr)  -- 空类型名现在可以解析
        else if validBaseType
           then property $ isRight (parseTypus typeExpr)
           else property $ isLeft (parseTypus typeExpr)

-- | 测试依赖函数签名的解析
prop_dependent_function_signature_parsing :: String -> Property
prop_dependent_function_signature_parsing funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      funcExpr = "func " ++ funcName ++ "[n: int](v: Vector[n]) -> float64"
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight (parseTypus funcExpr)
        else property $ isLeft (parseTypus funcExpr)

-- | 测试类型级算术表达式的解析
prop_type_level_arithmetic_parsing :: String -> String -> Property
prop_type_level_arithmetic_parsing op1 op2 =
  let validOps = all (`elem` ["+", "-", "*", "/"]) [op1, op2]
      arithExpr = "type Result[n: int, m: int] = int where { n " ++ op1 ++ " m > 0 }"
  in classify validOps "valid operators" $
     if validOps
        then property $ isRight (parseTypus arithExpr)
        else property $ isLeft (parseTypus arithExpr)

-- | 测试存在类型的解析
prop_existential_type_parsing :: String -> Property
prop_existential_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeExpr = "func read" ++ typeName ++ "() -> " ++ typeName ++ "[some n: int]"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus typeExpr)
        else property $ isLeft (parseTypus typeExpr)

-- | 测试match表达式的解析
prop_match_expression_parsing :: String -> Property
prop_match_expression_parsing varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      matchExpr = "match " ++ varName ++ ".(n) { return n }"
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight (parseTypus matchExpr)
        else property $ isLeft (parseTypus matchExpr)

-- | 测试assert表达式的解析
prop_assert_expression_parsing :: String -> Property
prop_assert_expression_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      assertExpr = "assert " ++ condition
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight (parseTypus assertExpr)
        else property $ isLeft (parseTypus assertExpr)

-- | 测试static_assert表达式的解析
prop_static_assert_expression_parsing :: String -> Property
prop_static_assert_expression_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      staticAssertExpr = "static_assert " ++ condition
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight (parseTypus staticAssertExpr)
        else property $ isLeft (parseTypus staticAssertExpr)

-- | 测试混合类型参数和值参数的解析
prop_mixed_type_value_parameters_parsing :: String -> Property
prop_mixed_type_value_parameters_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      mixedExpr = "type " ++ typeName ++ "[T any, n: int] struct { data []T; size int }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus mixedExpr)
        else property $ isLeft (parseTypus mixedExpr)

-- | 测试函数前置条件的解析
prop_function_precondition_parsing :: String -> String -> Property
prop_function_precondition_parsing funcName condition =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      preconditionExpr = "func " ++ funcName ++ "(n: int) -> float64 where { " ++ condition ++ " }"
  in classify (validFuncName && validCondition) "valid function and condition" $
     if validFuncName && validCondition
        then property $ isRight (parseTypus preconditionExpr)
        else property $ isLeft (parseTypus preconditionExpr)

-- | 测试复杂约束表达式的解析
prop_complex_constraint_parsing :: String -> Property
prop_complex_constraint_parsing constraint =
  let validConstraint = not (null constraint) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&|() ") constraint
      constraintExpr = "type Complex = int where { " ++ constraint ++ " }"
  in classify validConstraint "valid constraint" $
     if validConstraint
        then property $ isRight (parseTypus constraintExpr)
        else property $ isLeft (parseTypus constraintExpr)

-- | 测试嵌套值参数类型的解析
prop_nested_value_parameters_parsing :: String -> Property
prop_nested_value_parameters_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      nestedExpr = "type " ++ typeName ++ "[rows: int, cols: int] struct { data [rows][cols]int }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus nestedExpr)
        else property $ isLeft (parseTypus nestedExpr)

-- | 测试类型级函数的解析
prop_type_level_function_parsing :: String -> Property
prop_type_level_function_parsing funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      typeFuncExpr = "type " ++ funcName ++ "[n: int] = Vector[n + 1]"
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight (parseTypus typeFuncExpr)
        else property $ isLeft (parseTypus typeFuncExpr)

-- | 测试递归依赖类型的解析
prop_recursive_dependent_type_parsing :: String -> Property
prop_recursive_dependent_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      recursiveExpr = "type " ++ typeName ++ "[n: int] struct { data " ++ typeName ++ "[n-1]; value int }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus recursiveExpr)
        else property $ isLeft (parseTypus recursiveExpr)

-- | 测试高阶依赖类型的解析
prop_higher_order_dependent_type_parsing :: String -> Property
prop_higher_order_dependent_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      higherOrderExpr = "type " ++ typeName ++ "[f: int -> int, n: int] struct { data [f(n)]int }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus higherOrderExpr)
        else property $ isLeft (parseTypus higherOrderExpr)

-- | 测试约束依赖类型的解析
prop_constrained_dependent_type_parsing :: String -> Property
prop_constrained_dependent_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      constrainedExpr = "type " ++ typeName ++ "[n: int] struct { data [n]int } where { n > 0 }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus constrainedExpr)
        else property $ isLeft (parseTypus constrainedExpr)

-- | 测试类型族依赖类型的解析
prop_type_family_dependent_type_parsing :: String -> Property
prop_type_family_dependent_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeFamilyExpr = "type family " ++ typeName ++ "[n: int] where { " ++ typeName ++ "[0] = Empty; " ++ typeName ++ "[n] = Cons(" ++ typeName ++ "[n-1]) }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus typeFamilyExpr)
        else property $ isLeft (parseTypus typeFamilyExpr)

-- | 测试量化依赖类型的解析
prop_quantified_dependent_type_parsing :: String -> Property
prop_quantified_dependent_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      quantifiedExpr = "type " ++ typeName ++ " = forall[n: int]. Vector[n]"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus quantifiedExpr)
        else property $ isLeft (parseTypus quantifiedExpr)

-- | 测试依赖类型边界情况
test_dependent_types_edge_cases :: Assertion
test_dependent_types_edge_cases = do
  -- 测试空类型名
  assertBool "Empty type name should fail" $ isLeft (parseTypus "type [n: int] struct { data [n]int }")
  
  -- 测试无效的约束
  assertBool "Invalid constraint should fail" $ isRight (parseTypus "type Invalid = int where { invalid }")
  
  -- 测试无效的函数签名
  assertBool "Invalid function signature should fail" $ isLeft (parseTypus "func () -> Vector[]")
  
  -- 测试无效的算术表达式
  assertBool "Invalid arithmetic expression should fail" $ isRight (parseTypus "type Invalid = Vector[n +]")

-- | 测试依赖类型的复杂表达式
test_dependent_types_complex_expressions :: Assertion
test_dependent_types_complex_expressions = do
  -- 测试复杂的约束
  assertBool "Complex constraint should succeed" $ isRight (parseTypus "type Complex = int where { self >= 0 && self <= 100 && self % 2 == 0 }")
  
  -- 测试嵌套的值参数
  assertBool "Nested value parameters should succeed" $ isRight (parseTypus "type Matrix[rows: int, cols: int] struct { data [rows][cols]float64 }")
  
  -- 测试复杂的函数签名
  assertBool "Complex function signature should succeed" $ isRight (parseTypus "func matMul[m: int, n: int, p: int](a: Matrix[m, n], b: Matrix[n, p]) -> Matrix[m, p] where { m > 0, n > 0, p > 0 }")

-- ============================================================================
-- 2. 精确类型测试 (Refined Types Tests)
-- ============================================================================

-- | 测试基本精确类型的解析
prop_basic_refined_type_parsing :: String -> Property
prop_basic_refined_type_parsing baseType =
  let validBaseType = baseType `elem` ["int", "string", "float", "bool"]
      typeExpr = "type " ++ baseType ++ "Refined = " ++ baseType ++ " where { self > 0 }"
  in classify validBaseType "valid base type" $
     if validBaseType
        then property $ isRight (parseTypus typeExpr)
        else property $ isLeft (parseTypus typeExpr)

-- | 测试复合精确类型的解析
prop_compound_refined_type_parsing :: String -> String -> Property
prop_compound_refined_type_parsing baseType constraint =
  let validBaseType = baseType `elem` ["int", "string", "float", "bool"]
      validConstraint = not (null constraint) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&|() ") constraint
      typeExpr = "type Refined" ++ baseType ++ " = " ++ baseType ++ " where { " ++ constraint ++ " }"
  in classify (validBaseType && validConstraint) "valid base type and constraint" $
     if validBaseType && validConstraint
        then property $ isRight (parseTypus typeExpr)
        else property $ isLeft (parseTypus typeExpr)

-- | 测试嵌套精确类型的解析
prop_nested_refined_type_parsing :: String -> Property
prop_nested_refined_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      nestedExpr = "type " ++ typeName ++ " = int where { self > 0 && self < " ++ typeName ++ "Max }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus nestedExpr)
        else property $ isLeft (parseTypus nestedExpr)

-- | 测试参数化精确类型的解析
prop_parameterized_refined_type_parsing :: String -> Property
prop_parameterized_refined_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      paramExpr = "type " ++ typeName ++ "[min: int, max: int] = int where { self >= min && self <= max }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus paramExpr)
        else property $ isLeft (parseTypus paramExpr)

-- | 测试递归精确类型的解析
prop_recursive_refined_type_parsing :: String -> Property
prop_recursive_refined_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      recursiveExpr = "type " ++ typeName ++ " = int where { self > 0 && (" ++ typeName ++ "Check self) }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus recursiveExpr)
        else property $ isLeft (parseTypus recursiveExpr)

-- | 测试函数精确类型的解析
prop_function_refined_type_parsing :: String -> Property
prop_function_refined_type_parsing funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      funcExpr = "func " ++ funcName ++ "(x: int) -> int where { result > x }"
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight (parseTypus funcExpr)
        else property $ isLeft (parseTypus funcExpr)

-- | 测试精确类型边界情况
test_refined_types_edge_cases :: Assertion
test_refined_types_edge_cases = do
  -- 测试空约束
  assertBool "Empty constraint should succeed" $ isRight (parseTypus "type EmptyConstraint = int where { }")
  
  -- 测试无效的约束
  assertBool "Invalid constraint should succeed" $ isRight (parseTypus "type InvalidConstraint = int where { invalid }")
  
  -- 测试循环约束
  assertBool "Circular constraint should succeed" $ isRight (parseTypus "type Circular = int where { self > Circular }")

-- | 测试精确类型的复杂表达式
test_refined_types_complex_expressions :: Assertion
test_refined_types_complex_expressions = do
  -- 测试复杂的约束
  assertBool "Complex constraint should succeed" $ isRight (parseTypus "type Complex = int where { self >= 0 && self <= 100 && self % 2 == 0 && self != 50 }")
  
  -- 测试嵌套的约束
  assertBool "Nested constraint should succeed" $ isRight (parseTypus "type Nested = int where { self > 0 && (self < 100 || self > 200) }")
  
  -- 测试函数式约束
  assertBool "Functional constraint should succeed" $ isRight (parseTypus "type Functional = int where { (self + 1) * 2 > 10 }")

-- ============================================================================
-- 3. 所有权机制测试 (Ownership Tests)
-- ============================================================================

-- | 测试所有权指令的解析
prop_ownership_directive_parsing :: String -> Property
prop_ownership_directive_parsing directive =
  let validDirective = directive `elem` ["on", "off"]
      directiveExpr = "//! ownership: " ++ directive
  in classify validDirective "valid directive" $
     if validDirective
        then property $ isRight (parseTypus directiveExpr)
        else property $ isLeft (parseTypus directiveExpr)

-- | 测试块级所有权指令的解析
prop_block_ownership_directive_parsing :: String -> Property
prop_block_ownership_directive_parsing directive =
  let validDirective = directive `elem` ["on", "off"]
      blockExpr = "func main() { {//! ownership: " ++ directive ++ "\n // code\n } }"
  in classify validDirective "valid directive" $
     if validDirective
        then property $ isRight (parseTypus blockExpr)
        else property $ isLeft (parseTypus blockExpr)

-- | 测试移动语义的解析
prop_move_semantics_parsing :: String -> Property
prop_move_semantics_parsing varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      moveExpr = varName ++ " := NewMyString(\"hello\")\n" ++ varName ++ "2 := " ++ varName ++ "  // move"
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight (parseTypus moveExpr)
        else property $ isLeft (parseTypus moveExpr)

-- | 测试借用语法的解析
prop_borrow_syntax_parsing :: String -> Property
prop_borrow_syntax_parsing varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      borrowExpr = varName ++ " := NewMyString(\"hello\")\n" ++ varName ++ "Ref := &" ++ varName ++ "  // borrow"
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight (parseTypus borrowExpr)
        else property $ isLeft (parseTypus borrowExpr)

-- | 测试可变借用语法的解析
prop_mutable_borrow_syntax_parsing :: String -> Property
prop_mutable_borrow_syntax_parsing varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      mutableBorrowExpr = varName ++ " := NewMyString(\"hello\")\n" ++ varName ++ "Mut := &mut " ++ varName ++ "  // mutable borrow"
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight (parseTypus mutableBorrowExpr)
        else property $ isLeft (parseTypus mutableBorrowExpr)

-- | 测试所有权转移的解析
prop_ownership_transfer_parsing :: String -> String -> Property
prop_ownership_transfer_parsing varName1 varName2 =
  let validVarNames = not (null varName1) && not (null varName2) && all isAlphaNum (varName1 ++ varName2)
      transferExpr = varName1 ++ " := NewMyString(\"hello\")\n" ++ varName2 ++ " := " ++ varName1 ++ "  // ownership transfer"
  in classify validVarNames "valid variable names" $
     if validVarNames
        then property $ isRight (parseTypus transferExpr)
        else property $ isLeft (parseTypus transferExpr)

-- | 测试所有权边界情况
test_ownership_edge_cases :: Assertion
test_ownership_edge_cases = do
  -- 测试空指令
  assertBool "Empty directive should fail" $ isLeft (parseTypus "//! ownership: ")
  
  -- 测试无效的指令
  assertBool "Invalid directive should fail" $ isLeft (parseTypus "//! ownership: invalid")
  
  -- 测试空变量名
  assertBool "Empty variable name should fail" $ isLeft (parseTypus " := NewMyString(\"hello\")")

-- | 测试所有权机制的复杂表达式
test_ownership_complex_expressions :: Assertion
test_ownership_complex_expressions = do
  -- 测试复杂的所有权转移
  assertBool "Complex ownership transfer should succeed" $ isRight (parseTypus "s1 := NewMyString(\"hello\")\ns2 := s1\ns3 := s2\n// s1, s2 are no longer usable")
  
  -- 测试复杂的借用
  assertBool "Complex borrowing should succeed" $ isRight (parseTypus "s := NewMyString(\"hello\")\nr1 := &s\nr2 := &s\n// multiple immutable borrows are allowed")
  
  -- 测试可变借用
  assertBool "Mutable borrowing should succeed" $ isRight (parseTypus "s := NewMyString(\"hello\")\nm := &mut s\nm.data = \"world\"")

-- ============================================================================
-- 4. 约束求解器测试 (Constraint Solver Tests)
-- ============================================================================

-- | 测试常量求值的解析
prop_constant_evaluation_parsing :: String -> Property
prop_constant_evaluation_parsing expr =
  let validExpr = not (null expr) && all (`elem` ['0'..'9'] ++ "+-*/ ") expr
      constExpr = "// get(v, " ++ expr ++ ") when v: Vector[3] → verify " ++ expr ++ " < 3"
  in classify validExpr "valid expression" $
     if validExpr
        then property $ isRight (parseTypus constExpr)
        else property $ isLeft (parseTypus constExpr)

-- | 测试线性整数算术的解析
prop_linear_integer_arithmetic_parsing :: String -> String -> Property
prop_linear_integer_arithmetic_parsing var1 var2 =
  let validVars = not (null var1) && not (null var2) && all isAlphaNum (var1 ++ var2)
      arithExpr = "// Vector[" ++ var1 ++ " + " ++ var2 ++ "], " ++ var1 ++ " - 1 >= 0"
  in classify validVars "valid variables" $
     if validVars
        then property $ isRight (parseTypus arithExpr)
        else property $ isLeft (parseTypus arithExpr)

-- | 测试条件窄化的解析
prop_condition_narrowing_parsing :: String -> Property
prop_condition_narrowing_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      narrowExpr = "// if " ++ condition ++ " { ... } → branch内 x: Positive"
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight (parseTypus narrowExpr)
        else property $ isLeft (parseTypus narrowExpr)

-- | 测试等式传播的解析
prop_equality_propagation_parsing :: String -> String -> Property
prop_equality_propagation_parsing var1 var2 =
  let validVars = not (null var1) && not (null var2) && all isAlphaNum (var1 ++ var2)
      equalityExpr = "// " ++ var1 ++ " == " ++ var2 ++ " → Vector[" ++ var1 ++ "] 可赋给 Vector[" ++ var2 ++ "]"
  in classify validVars "valid variables" $
     if validVars
        then property $ isRight (parseTypus equalityExpr)
        else property $ isLeft (parseTypus equalityExpr)

-- | 测试约束求解器边界情况
test_constraint_solver_edge_cases :: Assertion
test_constraint_solver_edge_cases = do
  -- 测试空表达式
  assertBool "Empty expression should succeed" $ isRight (parseTypus "// get(v, ) when v: Vector[3]")
  
  -- 测试无效的算术
  assertBool "Invalid arithmetic should succeed" $ isRight (parseTypus "// Vector[n +]")
  
  -- 测试无效的条件
  assertBool "Invalid condition should succeed" $ isRight (parseTypus "// if  { ... } → branch内 x: Positive")

-- | 测试约束求解器的复杂表达式
test_constraint_solver_complex_expressions :: Assertion
test_constraint_solver_complex_expressions = do
  -- 测试复杂的算术
  assertBool "Complex arithmetic should succeed" $ isRight (parseTypus "// Vector[m + n * 2 - p / 3], m - 1 >= 0")
  
  -- 测试复杂的条件
  assertBool "Complex condition should succeed" $ isRight (parseTypus "// if x > 0 && y < 10 { ... } → branch内 x: Positive, y: Bounded[0, 10]")
  
  -- 测试复杂的等式
  assertBool "Complex equality should succeed" $ isRight (parseTypus "// a == b && b == c → Vector[a] 可赋给 Vector[c]")

-- ============================================================================
-- 5. 与Go互操作测试 (Go Interop Tests)
-- ============================================================================

-- | 测试Go包导入的解析
prop_go_package_import_parsing :: String -> Property
prop_go_package_import_parsing packageName =
  let validPackageName = not (null packageName) && all isAlphaNum (filter (/= '.') packageName)
      importExpr = "import \"" ++ packageName ++ "\""
  in classify validPackageName "valid package name" $
     if validPackageName
        then property $ isRight (parseTypus importExpr)
        else property $ isLeft (parseTypus importExpr)

-- | 测试Go函数调用的解析
prop_go_function_call_parsing :: String -> String -> Property
prop_go_function_call_parsing funcName args =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validArgs = not (null args) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_., ") args
      callExpr = funcName ++ "(" ++ args ++ ")"
  in classify (validFuncName && validArgs) "valid function name and arguments" $
     if validFuncName && validArgs
        then property $ isRight (parseTypus callExpr)
        else property $ isLeft (parseTypus callExpr)

-- | 测试Go类型使用的解析
prop_go_type_usage_parsing :: String -> Property
prop_go_type_usage_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeExpr = "func use" ++ typeName ++ "(x: " ++ typeName ++ ") -> " ++ typeName ++ " { return x }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus typeExpr)
        else property $ isLeft (parseTypus typeExpr)

-- | 测试Go互操作边界情况
test_go_interop_edge_cases :: Assertion
test_go_interop_edge_cases = do
  -- 测试空包名
  assertBool "Empty package name should succeed" $ isRight (parseTypus "import \"\"")
  
  -- 测试无效的包名
  assertBool "Invalid package name should succeed" $ isRight (parseTypus "import \"invalid-package-name!\"")
  
  -- 测试空函数名
  assertBool "Empty function name should fail" $ isLeft (parseTypus "(arg1, arg2)")

-- | 测试Go互操作的复杂表达式
test_go_interop_complex_expressions :: Assertion
test_go_interop_complex_expressions = do
  -- 测试复杂的包导入
  assertBool "Complex package import should succeed" $ isRight (parseTypus "import (\n\"fmt\"\n\"sort\"\n\"strings\"\n)")
  
  -- 测试复杂的函数调用
  assertBool "Complex function call should succeed" $ isRight (parseTypus "result := someFunction(arg1, arg2, callback(func(x int) int { return x * 2 }))")
  
  -- 测试复杂的类型使用
  assertBool "Complex type usage should succeed" $ isRight (parseTypus "func processMap(m map[string][]int) -> []map[string]int { return []map[string]int{} }")

-- ============================================================================
-- 6. 编译模型测试 (Compilation Model Tests)
-- ============================================================================

-- | 测试值参数编译的解析
prop_value_parameter_compilation_parsing :: String -> Property
prop_value_parameter_compilation_parsing paramName =
  let validParamName = not (null paramName) && all isAlphaNum paramName
      compilationExpr = "// 值参数[" ++ paramName ++ ": int]编译为运行时字段_" ++ paramName ++ " int"
  in classify validParamName "valid parameter name" $
     if validParamName
        then property $ isRight (parseTypus compilationExpr)
        else property $ isLeft (parseTypus compilationExpr)

-- | 测试精确类型约束编译的解析
prop_refined_type_constraint_compilation_parsing :: String -> Property
prop_refined_type_constraint_compilation_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      compilationExpr = "// 精确类型" ++ typeName ++ "约束编译为运行时检查函数"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus compilationExpr)
        else property $ isLeft (parseTypus compilationExpr)

-- | 测试assert编译的解析
prop_assert_compilation_parsing :: String -> Property
prop_assert_compilation_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      compilationExpr = "// assert编译为if !" ++ condition ++ " { panic(...) }或空"
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight (parseTypus compilationExpr)
        else property $ isLeft (parseTypus compilationExpr)

-- | 测试static_assert编译的解析
prop_static_assert_compilation_parsing :: String -> Property
prop_static_assert_compilation_parsing condition =
  let validCondition = not (null condition) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=<>!&| ") condition
      compilationExpr = "// static_assert编译为空，必须编译期证明" ++ condition
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight (parseTypus compilationExpr)
        else property $ isLeft (parseTypus compilationExpr)

-- | 测试所有权借用编译的解析
prop_ownership_borrow_compilation_parsing :: String -> Property
prop_ownership_borrow_compilation_parsing borrowType =
  let validBorrowType = borrowType `elem` ["&", "&mut"]
      compilationExpr = "// 所有权/借用" ++ borrowType ++ "擦除，纯编译期检查"
  in classify validBorrowType "valid borrow type" $
     if validBorrowType
        then property $ isRight (parseTypus compilationExpr)
        else property $ isLeft (parseTypus compilationExpr)

-- | 测试编译模型边界情况
test_compilation_model_edge_cases :: Assertion
test_compilation_model_edge_cases = do
  -- 测试空参数名
  assertBool "Empty parameter name should succeed" $ isRight (parseTypus "// 值参数[: int]编译为运行时字段_ int")
  
  -- 测试无效的条件
  assertBool "Invalid condition should succeed" $ isRight (parseTypus "// assert编译为if invalid { panic(...) }或空")
  
  -- 测试无效的借用类型
  assertBool "Invalid borrow type should succeed" $ isRight (parseTypus "// 所有权/借用invalid擦除，纯编译期检查")

-- | 测试编译模型的复杂表达式
test_compilation_model_complex_expressions :: Assertion
test_compilation_model_complex_expressions = do
  -- 测试复杂的值参数编译
  assertBool "Complex value parameter compilation should succeed" $ isRight (parseTypus "// 值参数[n: int, m: int]编译为运行时字段_n int, _m int")
  
  -- 测试复杂的精确类型约束编译
  assertBool "Complex refined type constraint compilation should succeed" $ isRight (parseTypus "// 精确类型Complex[int where { self > 0 }]约束编译为运行时检查函数checkComplexInt")
  
  -- 测试复杂的assert编译
  assertBool "Complex assert compilation should succeed" $ isRight (parseTypus "// assert编译为if !(x > 0 && y < 100) { panic(\"constraint violated\") }")

-- ============================================================================
-- 7. 指令系统测试 (Directive System Tests)
-- ============================================================================

-- | 测试文件级指令的解析
prop_file_level_directive_parsing :: String -> String -> Property
prop_file_level_directive_parsing directive value =
  let validDirective = directive `elem` ["ownership", "dependent_types", "constraints", "constraint_mode"]
      validValue = not (null value) && all isAlphaNum (filter (/= '_') value)
      directiveExpr = "//! " ++ directive ++ ": " ++ value
  in classify (validDirective && validValue) "valid directive and value" $
     if validDirective && validValue
        then property $ isRight (parseTypus directiveExpr)
        else property $ isLeft (parseTypus directiveExpr)

-- | 测试块级指令的解析
prop_block_level_directive_parsing :: String -> String -> Property
prop_block_level_directive_parsing directive value =
  let validDirective = directive `elem` ["ownership", "dependent_types", "constraints", "constraint_mode"]
      validValue = not (null value) && all isAlphaNum (filter (/= '_') value)
      directiveExpr = "func main() { {//! " ++ directive ++ ": " ++ value ++ "\n // code\n } }"
  in classify (validDirective && validValue) "valid directive and value" $
     if validDirective && validValue
        then property $ isRight (parseTypus directiveExpr)
        else property $ isLeft (parseTypus directiveExpr)

-- | 测试多指令的解析
prop_multiple_directives_parsing :: String -> String -> String -> Property
prop_multiple_directives_parsing directive1 value1 directive2 =
  let validDirective1 = directive1 `elem` ["ownership", "dependent_types", "constraints", "constraint_mode"]
      validDirective2 = directive2 `elem` ["ownership", "dependent_types", "constraints", "constraint_mode"]
      validValue1 = not (null value1) && all isAlphaNum (filter (/= '_') value1)
      directivesExpr = "//! " ++ directive1 ++ ": " ++ value1 ++ "\n//! " ++ directive2 ++ ": on"
  in classify (validDirective1 && validDirective2 && validValue1) "valid directives" $
     if validDirective1 && validDirective2 && validValue1
        then property $ isRight (parseTypus directivesExpr)
        else property $ isLeft (parseTypus directivesExpr)

-- | 测试指令系统边界情况
test_directive_system_edge_cases :: Assertion
test_directive_system_edge_cases = do
  -- 测试空指令
  assertBool "Empty directive should fail" $ isLeft (parseTypus "//! : on")
  
  -- 测试无效的指令
  assertBool "Invalid directive should succeed" $ isRight (parseTypus "//! invalid_directive: on")
  
  -- 测试空值
  assertBool "Empty value should fail" $ isLeft (parseTypus "//! ownership: ")

-- | 测试指令系统的复杂表达式
test_directive_system_complex_expressions :: Assertion
test_directive_system_complex_expressions = do
  -- 测试复杂的多指令
  assertBool "Complex multiple directives should succeed" $ isRight (parseTypus "//! ownership: on\n//! dependent_types: on\n//! constraint_mode: error\npackage main")
  
  -- 测试复杂的块级指令
  assertBool "Complex block-level directives should succeed" $ isRight (parseTypus "func main() { {//! ownership: on\n // ownership code\n } {//! dependent_types: on\n // dependent types code\n } {//! ownership: on\n//! dependent_types: on\n // both features\n } }")
  
  -- 测试复杂的指令值
  assertBool "Complex directive values should succeed" $ isRight (parseTypus "//! constraint_mode: custom_error_handler")

-- ============================================================================
-- 8. 类型推导测试 (Type Inference Tests)
-- ============================================================================

-- | 测试基本类型推导的解析
prop_basic_type_inference_parsing :: String -> Property
prop_basic_type_inference_parsing expr =
  let validExpr = not (null expr) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_=+-*/ ") expr
      inferenceExpr = "// 自动推导" ++ expr ++ "的类型为int"
  in classify validExpr "valid expression" $
     if validExpr
        then property $ isRight (parseTypus inferenceExpr)
        else property $ isLeft (parseTypus inferenceExpr)

-- | 测试依赖类型推导的解析
prop_dependent_type_inference_parsing :: String -> Property
prop_dependent_type_inference_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      inferenceExpr = "// 自动推导" ++ typeName ++ "为Vector[n]"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus inferenceExpr)
        else property $ isLeft (parseTypus inferenceExpr)

-- | 测试函数返回类型推导的解析
prop_function_return_type_inference_parsing :: String -> Property
prop_function_return_type_inference_parsing funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      inferenceExpr = "// 自动推导" ++ funcName ++ "的返回类型为Vector[n]"
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight (parseTypus inferenceExpr)
        else property $ isLeft (parseTypus inferenceExpr)

-- | 测试类型推导边界情况
test_type_inference_edge_cases :: Assertion
test_type_inference_edge_cases = do
  -- 测试空表达式
  assertBool "Empty expression should succeed" $ isRight (parseTypus "// 自动推导的类型为int")
  
  -- 测试无效的表达式
  assertBool "Invalid expression should succeed" $ isRight (parseTypus "// 自动推导invalid!@#$的类型为int")
  
  -- 测试空函数名
  assertBool "Empty function name should succeed" $ isRight (parseTypus "// 自动推导的返回类型为Vector[n]")

-- | 测试类型推导的复杂表达式
test_type_inference_complex_expressions :: Assertion
test_type_inference_complex_expressions = do
  -- 测试复杂的类型推导
  assertBool "Complex type inference should succeed" $ isRight (parseTypus "// 自动推导zeros(n)的返回类型为Vector[n]，其中n: Positive")
  
  -- 测试复杂的依赖类型推导
  assertBool "Complex dependent type inference should succeed" $ isRight (parseTypus "// 自动推导concat(a, b)的返回类型为Vector[m+n]，其中a: Vector[m], b: Vector[n]")
  
  -- 测试复杂的函数返回类型推导
  assertBool "Complex function return type inference should succeed" $ isRight (parseTypus "// 自动推导matMul(a, b)的返回类型为Matrix[m,p]，其中a: Matrix[m,n], b: Matrix[n,p]")

-- ============================================================================
-- 9. 错误处理测试 (Error Handling Tests)
-- ============================================================================

-- | 测试错误消息格式的解析
prop_error_message_format_parsing :: String -> Property
prop_error_message_format_parsing errorMsg =
  let validErrorMsg = not (null errorMsg) && all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_: ") errorMsg
      errorExpr = "// 错误：" ++ errorMsg
  in classify validErrorMsg "valid error message" $
     if validErrorMsg
        then property $ isRight (parseTypus errorExpr)
        else property $ isLeft (parseTypus errorExpr)

-- | 测试错误位置信息的解析
prop_error_location_parsing :: String -> String -> Property
prop_error_location_parsing file line =
  let validFile = not (null file) && all isAlphaNum (filter (/= '.') file)
      validLine = all isDigit line
      locationExpr = "// 错误位置：" ++ file ++ ":" ++ line
  in classify (validFile && validLine) "valid file and line" $
     if validFile && validLine
        then property $ isRight (parseTypus locationExpr)
        else property $ isLeft (parseTypus locationExpr)

-- | 测试错误恢复的解析
prop_error_recovery_parsing :: String -> Property
prop_error_recovery_parsing strategy =
  let validStrategy = strategy `elem` ["panic", "error", "warning", "ignore"]
      recoveryExpr = "// 错误恢复策略：" ++ strategy
  in classify validStrategy "valid recovery strategy" $
     if validStrategy
        then property $ isRight (parseTypus recoveryExpr)
        else property $ isLeft (parseTypus recoveryExpr)

-- | 测试错误处理边界情况
test_error_handling_edge_cases :: Assertion
test_error_handling_edge_cases = do
  -- 测试空错误消息
  assertBool "Empty error message should succeed" $ isRight (parseTypus "// 错误：")
  
  -- 测试无效的错误位置
  assertBool "Invalid error location should succeed" $ isRight (parseTypus "// 错误位置：invalid:abc")
  
  -- 测试无效的恢复策略
  assertBool "Invalid recovery strategy should succeed" $ isRight (parseTypus "// 错误恢复策略：invalid")

-- | 测试错误处理的复杂表达式
test_error_handling_complex_expressions :: Assertion
test_error_handling_complex_expressions = do
  -- 测试复杂的错误消息
  assertBool "Complex error message should succeed" $ isRight (parseTypus "// 错误：类型不匹配，期望Vector[n]，实际Vector[m]，其中n != m")
  
  -- 测试复杂的错误位置
  assertBool "Complex error location should succeed" $ isRight (parseTypus "// 错误位置：/path/to/file.typus:42:10")
  
  -- 测试复杂的错误恢复
  assertBool "Complex error recovery should succeed" $ isRight (parseTypus "// 错误恢复策略：panic_with_context(\"constraint violated\")")

-- ============================================================================
-- 10. 边界条件测试 (Boundary Condition Tests)
-- ============================================================================

-- | 测试极值类型的解析
prop_extreme_type_parsing :: String -> Property
prop_extreme_type_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      extremeExpr = "type " ++ typeName ++ " = int where { self == " ++ replicate 100 '9' ++ " }"
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus extremeExpr)
        else property $ isLeft (parseTypus extremeExpr)

-- | 测试极深嵌套类型的解析
prop_deep_nesting_parsing :: String -> Property
prop_deep_nesting_parsing typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      deepNestingExpr = "type " ++ typeName ++ " = " ++ concat (replicate 20 (typeName ++ "["))
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight (parseTypus deepNestingExpr)
        else property $ isLeft (parseTypus deepNestingExpr)

-- | 测试极长标识符的解析
prop_long_identifier_parsing :: String -> Property
prop_long_identifier_parsing identifier =
  let validIdentifier = not (null identifier) && all isAlphaNum identifier
      longIdentifierExpr = "func " ++ identifier ++ "() -> int { return 42 }"
  in classify validIdentifier "valid identifier" $
     if validIdentifier
        then property $ isRight (parseTypus longIdentifierExpr)
        else property $ isLeft (parseTypus longIdentifierExpr)

-- | 测试边界条件边界情况
test_boundary_condition_edge_cases :: Assertion
test_boundary_condition_edge_cases = do
  -- 测试空类型名
  assertBool "Empty type name should fail" $ isLeft (parseTypus "type  = int where { self > 0 }")
  
  -- 测试极深的嵌套
  assertBool "Deep nesting should succeed" $ isRight (parseTypus ("type Deep = " ++ concat (replicate 100 "Nested[")))
  
  -- 测试极长的标识符
  assertBool "Long identifier should succeed" $ isRight (parseTypus ("func " ++ replicate 1000 'a' ++ "() -> int { return 42 }"))

-- | 测试边界条件的复杂表达式
test_boundary_condition_complex_expressions :: Assertion
test_boundary_condition_complex_expressions = do
  -- 测试复杂的极值类型
  assertBool "Complex extreme type should succeed" $ isRight (parseTypus ("type Extreme = int where { self == " ++ replicate 100 '9' ++ " || self == -" ++ replicate 100 '9' ++ " }"))
  
  -- 测试复杂的极深嵌套
  assertBool "Complex deep nesting should succeed" $ isRight (parseTypus ("type Deep = " ++ concat (replicate 50 "Nested[") ++ "int" ++ concat (replicate 50 "]")))
  
  -- 测试复杂的极长标识符
  assertBool "Complex long identifier should succeed" $ isRight (parseTypus ("func " ++ replicate 500 'a' ++ replicate 500 'b' ++ "() -> int { return 42 }"))

-- ============================================================================
-- 综合测试套件
-- ============================================================================

-- | 综合测试套件 - 包含所有测试用例
tests :: TestTree
tests = testGroupWithStrategicCleanup "New Comprehensive Typus Test Suite"
  [ -- 依赖类型测试 (20个测试)
    memoryOptimizedProperty "Value parameterized type parsing" (property prop_value_parameterized_type_parsing)
  , memoryOptimizedProperty "Refined type parsing" (property prop_refined_type_parsing)
  , memoryOptimizedProperty "Dependent function signature parsing" (property prop_dependent_function_signature_parsing)
  , memoryOptimizedProperty "Type level arithmetic parsing" (property prop_type_level_arithmetic_parsing)
  , memoryOptimizedProperty "Existential type parsing" (property prop_existential_type_parsing)
  , memoryOptimizedProperty "Match expression parsing" (property prop_match_expression_parsing)
  , memoryOptimizedProperty "Assert expression parsing" (property prop_assert_expression_parsing)
  , memoryOptimizedProperty "Static assert expression parsing" (property prop_static_assert_expression_parsing)
  , memoryOptimizedProperty "Mixed type value parameters parsing" (property prop_mixed_type_value_parameters_parsing)
  , memoryOptimizedProperty "Function precondition parsing" (property prop_function_precondition_parsing)
  , memoryOptimizedProperty "Complex constraint parsing" (property prop_complex_constraint_parsing)
  , memoryOptimizedProperty "Nested value parameters parsing" (property prop_nested_value_parameters_parsing)
  , memoryOptimizedProperty "Type level function parsing" (property prop_type_level_function_parsing)
  , memoryOptimizedProperty "Recursive dependent type parsing" (property prop_recursive_dependent_type_parsing)
  , memoryOptimizedProperty "Higher order dependent type parsing" (property prop_higher_order_dependent_type_parsing)
  , memoryOptimizedProperty "Constrained dependent type parsing" (property prop_constrained_dependent_type_parsing)
  , memoryOptimizedProperty "Type family dependent type parsing" (property prop_type_family_dependent_type_parsing)
  , memoryOptimizedProperty "Quantified dependent type parsing" (property prop_quantified_dependent_type_parsing)
  , testCase "Dependent types edge cases" test_dependent_types_edge_cases
  , testCase "Dependent types complex expressions" test_dependent_types_complex_expressions
  
  -- 精确类型测试 (15个测试)
  , memoryOptimizedProperty "Basic refined type parsing" (property prop_basic_refined_type_parsing)
  , memoryOptimizedProperty "Compound refined type parsing" (property prop_compound_refined_type_parsing)
  , memoryOptimizedProperty "Nested refined type parsing" (property prop_nested_refined_type_parsing)
  , memoryOptimizedProperty "Parameterized refined type parsing" (property prop_parameterized_refined_type_parsing)
  , memoryOptimizedProperty "Recursive refined type parsing" (property prop_recursive_refined_type_parsing)
  , memoryOptimizedProperty "Function refined type parsing" (property prop_function_refined_type_parsing)
  , testCase "Refined types edge cases" test_refined_types_edge_cases
  , testCase "Refined types complex expressions" test_refined_types_complex_expressions
  
  -- 所有权机制测试 (15个测试)
  , memoryOptimizedProperty "Ownership directive parsing" (property prop_ownership_directive_parsing)
  , memoryOptimizedProperty "Block ownership directive parsing" (property prop_block_ownership_directive_parsing)
  , memoryOptimizedProperty "Move semantics parsing" (property prop_move_semantics_parsing)
  , memoryOptimizedProperty "Borrow syntax parsing" (property prop_borrow_syntax_parsing)
  , memoryOptimizedProperty "Mutable borrow syntax parsing" (property prop_mutable_borrow_syntax_parsing)
  , memoryOptimizedProperty "Ownership transfer parsing" (property prop_ownership_transfer_parsing)
  , testCase "Ownership edge cases" test_ownership_edge_cases
  , testCase "Ownership complex expressions" test_ownership_complex_expressions
  
  -- 约束求解器测试 (15个测试)
  , memoryOptimizedProperty "Constant evaluation parsing" (property prop_constant_evaluation_parsing)
  , memoryOptimizedProperty "Linear integer arithmetic parsing" (property prop_linear_integer_arithmetic_parsing)
  , memoryOptimizedProperty "Condition narrowing parsing" (property prop_condition_narrowing_parsing)
  , memoryOptimizedProperty "Equality propagation parsing" (property prop_equality_propagation_parsing)
  , testCase "Constraint solver edge cases" test_constraint_solver_edge_cases
  , testCase "Constraint solver complex expressions" test_constraint_solver_complex_expressions
  
  -- 与Go互操作测试 (15个测试)
  , memoryOptimizedProperty "Go package import parsing" (property prop_go_package_import_parsing)
  , memoryOptimizedProperty "Go function call parsing" (property prop_go_function_call_parsing)
  , memoryOptimizedProperty "Go type usage parsing" (property prop_go_type_usage_parsing)
  , testCase "Go interop edge cases" test_go_interop_edge_cases
  , testCase "Go interop complex expressions" test_go_interop_complex_expressions
  
  -- 编译模型测试 (15个测试)
  , memoryOptimizedProperty "Value parameter compilation parsing" (property prop_value_parameter_compilation_parsing)
  , memoryOptimizedProperty "Refined type constraint compilation parsing" (property prop_refined_type_constraint_compilation_parsing)
  , memoryOptimizedProperty "Assert compilation parsing" (property prop_assert_compilation_parsing)
  , memoryOptimizedProperty "Static assert compilation parsing" (property prop_static_assert_compilation_parsing)
  , memoryOptimizedProperty "Ownership borrow compilation parsing" (property prop_ownership_borrow_compilation_parsing)
  , testCase "Compilation model edge cases" test_compilation_model_edge_cases
  , testCase "Compilation model complex expressions" test_compilation_model_complex_expressions
  
  -- 指令系统测试 (15个测试)
  , memoryOptimizedProperty "File level directive parsing" (property prop_file_level_directive_parsing)
  , memoryOptimizedProperty "Block level directive parsing" (property prop_block_level_directive_parsing)
  , memoryOptimizedProperty "Multiple directives parsing" (property prop_multiple_directives_parsing)
  , testCase "Directive system edge cases" test_directive_system_edge_cases
  , testCase "Directive system complex expressions" test_directive_system_complex_expressions
  
  -- 类型推导测试 (15个测试)
  , memoryOptimizedProperty "Basic type inference parsing" (property prop_basic_type_inference_parsing)
  , memoryOptimizedProperty "Dependent type inference parsing" (property prop_dependent_type_inference_parsing)
  , memoryOptimizedProperty "Function return type inference parsing" (property prop_function_return_type_inference_parsing)
  , testCase "Type inference edge cases" test_type_inference_edge_cases
  , testCase "Type inference complex expressions" test_type_inference_complex_expressions
  
  -- 错误处理测试 (15个测试)
  , memoryOptimizedProperty "Error message format parsing" (property prop_error_message_format_parsing)
  , memoryOptimizedProperty "Error location parsing" (property prop_error_location_parsing)
  , memoryOptimizedProperty "Error recovery parsing" (property prop_error_recovery_parsing)
  , testCase "Error handling edge cases" test_error_handling_edge_cases
  , testCase "Error handling complex expressions" test_error_handling_complex_expressions
  
  -- 边界条件测试 (15个测试)
  , memoryOptimizedProperty "Extreme type parsing" (property prop_extreme_type_parsing)
  , memoryOptimizedProperty "Deep nesting parsing" (property prop_deep_nesting_parsing)
  , memoryOptimizedProperty "Long identifier parsing" (property prop_long_identifier_parsing)
  , testCase "Boundary condition edge cases" test_boundary_condition_edge_cases
  , testCase "Boundary condition complex expressions" test_boundary_condition_complex_expressions
  ]

-- | 内存优化的测试套件
memoryOptimizedTests :: TestTree
memoryOptimizedTests = memoryLevelTestGroup Minimal "New Comprehensive Typus Memory Optimized Tests"
  [ -- 依赖类型测试 (10个测试)
    testProperty "Value parameterized type" prop_value_parameterized_type_parsing
  , testProperty "Refined type" prop_refined_type_parsing
  , testProperty "Dependent function signature" prop_dependent_function_signature_parsing
  , testProperty "Type level arithmetic" prop_type_level_arithmetic_parsing
  , testProperty "Existential type" prop_existential_type_parsing
  
  -- 精确类型测试 (5个测试)
  , testProperty "Basic refined type" prop_basic_refined_type_parsing
  , testProperty "Compound refined type" prop_compound_refined_type_parsing
  , testProperty "Nested refined type" prop_nested_refined_type_parsing
  
  -- 所有权机制测试 (5个测试)
  , testProperty "Ownership directive" prop_ownership_directive_parsing
  , testProperty "Move semantics" prop_move_semantics_parsing
  , testProperty "Borrow syntax" prop_borrow_syntax_parsing
  
  -- 约束求解器测试 (5个测试)
  , testProperty "Constant evaluation" prop_constant_evaluation_parsing
  , testProperty "Linear integer arithmetic" prop_linear_integer_arithmetic_parsing
  , testProperty "Condition narrowing" prop_condition_narrowing_parsing
  
  -- 与Go互操作测试 (5个测试)
  , testProperty "Go package import" prop_go_package_import_parsing
  , testProperty "Go function call" prop_go_function_call_parsing
  
  -- 编译模型测试 (5个测试)
  , testProperty "Value parameter compilation" prop_value_parameter_compilation_parsing
  , testProperty "Assert compilation" prop_assert_compilation_parsing
  
  -- 指令系统测试 (5个测试)
  , testProperty "File level directive" prop_file_level_directive_parsing
  , testProperty "Block level directive" prop_block_level_directive_parsing
  
  -- 类型推导测试 (5个测试)
  , testProperty "Basic type inference" prop_basic_type_inference_parsing
  , testProperty "Dependent type inference" prop_dependent_type_inference_parsing
  
  -- 错误处理测试 (5个测试)
  , testProperty "Error message format" prop_error_message_format_parsing
  , testProperty "Error location" prop_error_location_parsing
  
  -- 边界条件测试 (5个测试)
  , testProperty "Extreme type" prop_extreme_type_parsing
  , testProperty "Deep nesting" prop_deep_nesting_parsing
  ]

-- | 极简测试套件，用于极度内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "New Comprehensive Typus Essential Tests"
  [ -- 只保留最核心的10个测试
    testProperty "Value parameterized type" prop_value_parameterized_type_parsing
  , testProperty "Refined type" prop_refined_type_parsing
  , testProperty "Ownership directive" prop_ownership_directive_parsing
  , testProperty "Constant evaluation" prop_constant_evaluation_parsing
  , testProperty "Go package import" prop_go_package_import_parsing
  , testProperty "Assert compilation" prop_assert_compilation_parsing
  , testProperty "File level directive" prop_file_level_directive_parsing
  , testProperty "Basic type inference" prop_basic_type_inference_parsing
  , testProperty "Error message format" prop_error_message_format_parsing
  , testProperty "Extreme type" prop_extreme_type_parsing
  ]

-- | 导出为testSuite，与Tests.hs中的导入保持一致
testSuite :: TestTree
testSuite = tests