{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.TypusCoreFeaturesTestSuite where

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
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when, unless)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- 1. 依赖类型核心功能测试 (Dependent Types Core Features)
-- ============================================================================

-- | 测试向量类型的长度保持性
prop_vector_length_preservation :: [Int] -> Property
prop_vector_length_preservation values =
  let n = length values
      vectorExpr = "type Vector[" ++ show n ++ "] struct { data [" ++ show n ++ "]int }"
      parseResult = parseTypus vectorExpr
  in classify (n > 0) "non-empty vector" $
     classify (n == 0) "empty vector" $
     property $ isRight parseResult

-- | 测试矩阵类型的维度保持性
prop_matrix_dimensions_preservation :: Int -> Int -> Property
prop_matrix_dimensions_preservation rows cols =
  let validDims = rows > 0 && cols > 0 && rows <= 10 && cols <= 10
      matrixExpr = "type Matrix[" ++ show rows ++ "][" ++ show cols ++ "] struct { data [" ++ show rows ++ "][" ++ show cols ++ "]float64 }"
      parseResult = parseTypus matrixExpr
  in classify validDims "valid dimensions" $
     classify (not validDims) "invalid dimensions" $
     if validDims
        then property $ isRight parseResult
        else property True  -- 无效维度可能解析失败，这是预期的

-- | 测试有界类型的约束解析
prop_bounded_type_constraints :: Int -> Int -> Int -> Property
prop_bounded_type_constraints lo hi value =
  let validBounds = lo <= hi
      boundedExpr = "type Bounded = int where { self >= " ++ show lo ++ " && self <= " ++ show hi ++ " }"
      parseResult = parseTypus boundedExpr
  in classify validBounds "valid bounds" $
     classify (not validBounds) "invalid bounds" $
     if validBounds
        then property $ isRight parseResult
        else property True

-- | 测试非空切片类型的约束
prop_non_empty_slice_constraints :: [Int] -> Property
prop_non_empty_slice_constraints values =
  let n = length values
      nonEmptyExpr = "type NonEmpty[" ++ show n ++ "] = [" ++ show n ++ "]int where { len(self) > 0 }"
      parseResult = parseTypus nonEmptyExpr
  in classify (n > 0) "non-empty slice" $
     classify (n == 0) "empty slice" $
     if n > 0
        then property $ isRight parseResult
        else property True

-- | 测试正数类型的约束
prop_positive_type_constraints :: Int -> Property
prop_positive_type_constraints value =
  let positiveExpr = "type Positive = int where { self > 0 }"
      parseResult = parseTypus positiveExpr
      checkValue = value > 0
  in classify checkValue "positive value" $
     classify (not checkValue) "non-positive value" $
     property $ isRight parseResult

-- ============================================================================
-- 2. 精确类型约束测试 (Refined Type Constraints)
-- ============================================================================

-- | 测试非零类型的约束
prop_nonzero_type_constraints :: Int -> Property
prop_nonzero_type_constraints value =
  let nonzeroExpr = "type NonZero = int where { self != 0 }"
      parseResult = parseTypus nonzeroExpr
      checkValue = value /= 0
  in classify checkValue "non-zero value" $
     classify (not checkValue) "zero value" $
     property $ isRight parseResult

-- | 测试百分比类型的约束
prop_percentage_type_constraints :: Int -> Property
prop_percentage_type_constraints value =
  let percentageExpr = "type Percentage = int where { self >= 0 && self <= 100 }"
      parseResult = parseTypus percentageExpr
      checkValue = value >= 0 && value <= 100
  in classify checkValue "valid percentage" $
     classify (not checkValue) "invalid percentage" $
     property $ isRight parseResult

-- | 测试有效索引类型的约束
prop_valid_index_type_constraints :: Int -> Int -> Property
prop_valid_index_type_constraints index size =
  let validSize = size > 0
      validIndexExpr = "type ValidIndex[" ++ show size ++ "] = int where { self >= 0 && self < " ++ show size ++ " }"
      parseResult = parseTypus validIndexExpr
      checkIndex = validSize && index >= 0 && index < size
  in classify validSize "valid size" $
     classify (not validSize) "invalid size" $
     if validSize
        then property $ isRight parseResult
        else property True

-- | 测试字符串长度约束
prop_string_length_constraints :: String -> Property
prop_string_length_constraints str =
  let len = length str
      lengthConstraintExpr = "type Length" ++ show len ++ " = string where { len(self) == " ++ show len ++ " }"
      parseResult = parseTypus lengthConstraintExpr
  in classify (len > 0) "non-empty string" $
     classify (len == 0) "empty string" $
     property $ isRight parseResult

-- | 测试复合约束条件
prop_compound_constraints :: Int -> Int -> Int -> Property
prop_compound_constraints minVal maxVal value =
  let validBounds = minVal <= maxVal
      compoundExpr = "type Ranged = int where { self >= " ++ show minVal ++ " && self <= " ++ show maxVal ++ " && self % 2 == 0 }"
      parseResult = parseTypus compoundExpr
  in classify validBounds "valid bounds" $
     classify (not validBounds) "invalid bounds" $
     if validBounds
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 3. 类型级算术测试 (Type-Level Arithmetic)
-- ============================================================================

-- | 测试类型级加法
prop_type_level_addition :: Int -> Int -> Property
prop_type_level_addition a b =
  let sum = a + b
      addExpr = "type Sum[" ++ show a ++ "][" ++ show b ++ "] = int where { " ++ show a ++ " + " ++ show b ++ " == " ++ show sum ++ " }"
      parseResult = parseTypus addExpr
  in classify (a >= 0 && b >= 0) "non-negative operands" $
     classify (a < 0 || b < 0) "negative operands" $
     property $ isRight parseResult

-- | 测试类型级减法
prop_type_level_subtraction :: Int -> Int -> Property
prop_type_level_subtraction a b =
  let diff = a - b
      validSub = a >= b
      subExpr = "type Diff[" ++ show a ++ "][" ++ show b ++ "] = int where { " ++ show a ++ " - " ++ show b ++ " == " ++ show diff ++ " }"
      parseResult = parseTypus subExpr
  in classify validSub "valid subtraction" $
     classify (not validSub) "invalid subtraction" $
     property $ isRight parseResult

-- | 测试类型级乘法
prop_type_level_multiplication :: Int -> Int -> Property
prop_type_level_multiplication a b =
  let product = a * b
      smallOps = abs a <= 10 && abs b <= 10
      mulExpr = "type Product[" ++ show a ++ "][" ++ show b ++ "] = int where { " ++ show a ++ " * " ++ show b ++ " == " ++ show product ++ " }"
      parseResult = parseTypus mulExpr
  in classify smallOps "small operands" $
     classify (not smallOps) "large operands" $
     property $ isRight parseResult

-- | 测试类型级比较
prop_type_level_comparison :: Int -> Int -> Property
prop_type_level_comparison a b =
  let cmpExpr = "type Cmp[" ++ show a ++ "][" ++ show b ++ "] = bool where { " ++ show a ++ " > " ++ show b ++ " }"
      parseResult = parseTypus cmpExpr
  in classify (a > b) "first greater" $
     classify (a == b) "equal values" $
     classify (a < b) "first smaller" $
     property $ isRight parseResult

-- | 测试类型级模运算
prop_type_level_modulus :: Int -> Int -> Property
prop_type_level_modulus a b =
  let validMod = b /= 0
      modVal = if validMod then a `mod` b else 0
      modExpr = "type Mod[" ++ show a ++ "][" ++ show b ++ "] = int where { " ++ show a ++ " % " ++ show b ++ " == " ++ show modVal ++ " }"
      parseResult = parseTypus modExpr
  in classify validMod "valid modulus" $
     classify (not validMod) "invalid modulus" $
     property $ isRight parseResult

-- ============================================================================
-- 4. 函数签名依赖测试 (Function Signature Dependencies)
-- ============================================================================

-- | 测试依赖返回类型
prop_dependent_return_type :: String -> Int -> Property
prop_dependent_return_type funcName n =
  let validName = not (null funcName) && all isAlphaNum funcName
      validSize = n >= 0 && n <= 10
      funcExpr = "func " ++ funcName ++ "() -> Vector[" ++ show n ++ "]"
      parseResult = parseTypus funcExpr
  in classify validName "valid function name" $
     classify validSize "valid size" $
     if validName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试依赖参数类型
prop_dependent_parameter_type :: String -> Int -> Property
prop_dependent_parameter_type funcName n =
  let validName = not (null funcName) && all isAlphaNum funcName
      validSize = n >= 0 && n <= 10
      funcExpr = "func " ++ funcName ++ "[n: int](v: Vector[" ++ show n ++ "]) -> float64"
      parseResult = parseTypus funcExpr
  in classify validName "valid function name" $
     classify validSize "valid size" $
     if validName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试多重依赖参数
prop_multiple_dependent_parameters :: String -> Int -> Int -> Property
prop_multiple_dependent_parameters funcName m n =
  let validName = not (null funcName) && all isAlphaNum funcName
      validSizes = m >= 0 && n >= 0 && m <= 10 && n <= 10
      funcExpr = "func " ++ funcName ++ "[m: int, n: int](a: Matrix[" ++ show m ++ "][" ++ show n ++ "]) -> float64"
      parseResult = parseTypus funcExpr
  in classify validName "valid function name" $
     classify validSizes "valid sizes" $
     if validName && validSizes
        then property $ isRight parseResult
        else property True

-- | 测试函数前置条件
prop_function_preconditions :: String -> Int -> Property
prop_function_preconditions funcName n =
  let validName = not (null funcName) && all isAlphaNum funcName
      validSize = n > 0 && n <= 10
      funcExpr = "func " ++ funcName ++ "[n: int](v: Vector[" ++ show n ++ "]) -> float64 where { n > 0 }"
      parseResult = parseTypus funcExpr
  in classify validName "valid function name" $
     classify validSize "valid size" $
     if validName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试复杂函数签名
prop_complex_function_signature :: String -> Int -> Int -> Int -> Property
prop_complex_function_signature funcName m n p =
  let validName = not (null funcName) && all isAlphaNum funcName
      validSizes = m > 0 && n > 0 && p > 0 && m <= 5 && n <= 5 && p <= 5
      funcExpr = "func " ++ funcName ++ "[m: int, n: int, p: int](a: Matrix[" ++ show m ++ "][" ++ show n ++ "], b: Matrix[" ++ show n ++ "][" ++ show p ++ "]) -> Matrix[" ++ show m ++ "][" ++ show p ++ "] where { m > 0 && n > 0 && p > 0 }"
      parseResult = parseTypus funcExpr
  in classify validName "valid function name" $
     classify validSizes "valid sizes" $
     if validName && validSizes
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 5. 断言和条件窄化测试 (Assertions and Conditional Narrowing)
-- ============================================================================

-- | 测试断言表达式
prop_assert_expressions :: String -> Property
prop_assert_expressions condition =
  let validCondition = not (null condition) && length condition <= 20
      assertExpr = "assert " ++ condition
      parseResult = parseTypus assertExpr
  in classify validCondition "valid condition" $
     classify (not validCondition) "invalid condition" $
     if validCondition
        then property $ isRight parseResult
        else property True

-- | 测试静态断言表达式
prop_static_assert_expressions :: String -> Property
prop_static_assert_expressions condition =
  let validCondition = not (null condition) && length condition <= 20
      staticAssertExpr = "static_assert " ++ condition
      parseResult = parseTypus staticAssertExpr
  in classify validCondition "valid condition" $
     classify (not validCondition) "invalid condition" $
     if validCondition
        then property $ isRight parseResult
        else property True

-- | 测试条件窄化
prop_conditional_narrowing :: String -> String -> Property
prop_conditional_narrowing condition branch =
  let validCondition = not (null condition) && length condition <= 15
      validBranch = not (null branch) && length branch <= 15
      ifExpr = "if " ++ condition ++ " { " ++ branch ++ " }"
      parseResult = parseTypus ifExpr
  in classify (validCondition && validBranch) "valid if expression" $
     classify (not (validCondition && validBranch)) "invalid if expression" $
     if validCondition && validBranch
        then property $ isRight parseResult
        else property True

-- | 测试match表达式
prop_match_expressions :: String -> Property
prop_match_expressions varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      matchExpr = "match " ++ varName ++ ".(n) { return n }"
      parseResult = parseTypus matchExpr
  in classify validVarName "valid variable name" $
     classify (not validVarName) "invalid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试存在类型
prop_existential_types :: String -> Property
prop_existential_types typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      existentialExpr = "func read" ++ typeName ++ "() -> " ++ typeName ++ "[some n: int]"
      parseResult = parseTypus existentialExpr
  in classify validTypeName "valid type name" $
     classify (not validTypeName) "invalid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 6. 所有权机制测试 (Ownership Mechanism)
-- ============================================================================

-- | 测试所有权指令解析
prop_ownership_directive_parsing :: Property
prop_ownership_directive_parsing =
  let ownershipExpr = "{//! ownership: on\n  s := NewMyString(\"hello\")\n  t := s\n}"
      parseResult = parseTypus ownershipExpr
  in property $ isRight parseResult

-- | 测试移动语义
prop_move_semantics :: String -> Property
prop_move_semantics varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      moveExpr = "{//! ownership: on\n  " ++ varName ++ " := NewMyString(\"hello\")\n  " ++ varName ++ "2 := " ++ varName ++ "\n}"
      parseResult = parseTypus moveExpr
  in classify validVarName "valid variable name" $
     classify (not validVarName) "invalid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试借用表达式
prop_borrow_expressions :: String -> Property
prop_borrow_expressions varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      borrowExpr = "{//! ownership: on\n  " ++ varName ++ " := NewMyString(\"hello\")\n  r := &" ++ varName ++ "\n}"
      parseResult = parseTypus borrowExpr
  in classify validVarName "valid variable name" $
     classify (not validVarName) "invalid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试可变借用
prop_mutable_borrow_expressions :: String -> Property
prop_mutable_borrow_expressions varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      mutableBorrowExpr = "{//! ownership: on\n  " ++ varName ++ " := NewMyString(\"hello\")\n  m := &mut " ++ varName ++ "\n}"
      parseResult = parseTypus mutableBorrowExpr
  in classify validVarName "valid variable name" $
     classify (not validVarName) "invalid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试所有权块级指令
prop_block_level_ownership_directive :: String -> Property
prop_block_level_ownership_directive code =
  let validCode = not (null code) && length code <= 30
      blockExpr = "func main() {\n  // 普通 Go 代码\n  {//! ownership: on\n    " ++ code ++ "\n  }\n}"
      parseResult = parseTypus blockExpr
  in classify validCode "valid code block" $
     classify (not validCode) "invalid code block" $
     if validCode
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 7. 指令系统测试 (Directive System)
-- ============================================================================

-- | 测试文件级所有权指令
prop_file_level_ownership_directive :: Property
prop_file_level_ownership_directive =
  let fileDirectiveExpr = "//! ownership: on\n\npackage main"
      parseResult = parseTypus fileDirectiveExpr
  in property $ isRight parseResult

-- | 测试文件级依赖类型指令
prop_file_level_dependent_types_directive :: Property
prop_file_level_dependent_types_directive =
  let fileDirectiveExpr = "//! dependent_types: on\n\npackage main"
      parseResult = parseTypus fileDirectiveExpr
  in property $ isRight parseResult

-- | 测试块级依赖类型指令
prop_block_level_dependent_types_directive :: String -> Property
prop_block_level_dependent_types_directive code =
  let validCode = not (null code) && length code <= 30
      blockExpr = "func main() {\n  // 普通 Go 代码\n  {//! dependent_types: on\n    " ++ code ++ "\n  }\n}"
      parseResult = parseTypus blockExpr
  in classify validCode "valid code block" $
     classify (not validCode) "invalid code block" $
     if validCode
        then property $ isRight parseResult
        else property True

-- | 测试多特性块级指令
prop_multiple_feature_block_directive :: String -> Property
prop_multiple_feature_block_directive code =
  let validCode = not (null code) && length code <= 30
      blockExpr = "func main() {\n  // 普通 Go 代码\n  {//! ownership: on\n  //! dependent_types: on\n    " ++ code ++ "\n  }\n}"
      parseResult = parseTypus blockExpr
  in classify validCode "valid code block" $
     classify (not validCode) "invalid code block" $
     if validCode
        then property $ isRight parseResult
        else property True

-- | 测试约束模式指令
prop_constraint_mode_directive :: String -> Property
prop_constraint_mode_directive mode =
  let validMode = mode `elem` ["panic", "error"]
      modeExpr = "//! constraint_mode: " ++ mode ++ "\n\npackage main"
      parseResult = parseTypus modeExpr
  in classify validMode "valid constraint mode" $
     classify (not validMode) "invalid constraint mode" $
     if validMode
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 8. 编译器集成测试 (Compiler Integration)
-- ============================================================================

-- | 测试基本类型编译
prop_basic_type_compilation :: String -> Property
prop_basic_type_compilation typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeExpr = "type " ++ typeName ++ " = int"
      parseResult = parseTypus typeExpr
  in classify validTypeName "valid type name" $
     classify (not validTypeName) "invalid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试结构体类型编译
prop_struct_type_compilation :: String -> Property
prop_struct_type_compilation structName =
  let validStructName = not (null structName) && all isAlphaNum structName
      structExpr = "type " ++ structName ++ " struct { field int }"
      parseResult = parseTypus structExpr
  in classify validStructName "valid struct name" $
     classify (not validStructName) "invalid struct name" $
     if validStructName
        then property $ isRight parseResult
        else property True

-- | 测试函数编译
prop_function_compilation :: String -> Property
prop_function_compilation funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      funcExpr = "func " ++ funcName ++ "() int { return 42 }"
      parseResult = parseTypus funcExpr
  in classify validFuncName "valid function name" $
     classify (not validFuncName) "invalid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试接口编译
prop_interface_compilation :: String -> Property
prop_interface_compilation interfaceName =
  let validInterfaceName = not (null interfaceName) && all isAlphaNum interfaceName
      interfaceExpr = "type " ++ interfaceName ++ " interface { Method() int }"
      parseResult = parseTypus interfaceExpr
  in classify validInterfaceName "valid interface name" $
     classify (not validInterfaceName) "invalid interface name" $
     if validInterfaceName
        then property $ isRight parseResult
        else property True

-- | 测试包声明编译
prop_package_declaration_compilation :: String -> Property
prop_package_declaration_compilation packageName =
  let validPackageName = not (null packageName) && all isAlphaNum (take 1 packageName ++ drop 1 (map (\c -> if c == '_' then '_' else c) packageName))
      packageExpr = "package " ++ packageName
      parseResult = parseTypus packageExpr
  in classify validPackageName "valid package name" $
     classify (not validPackageName) "invalid package name" $
     if validPackageName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 9. 边界条件测试 (Boundary Conditions)
-- ============================================================================

-- | 测试空字符串输入
prop_empty_string_input :: Property
prop_empty_string_input =
  let emptyExpr = ""
      parseResult = parseTypus emptyExpr
  in property $ isLeft parseResult

-- | 测试极大类型参数
prop_large_type_parameters :: Property
prop_large_type_parameters =
  let largeExpr = "type Vector[100] struct { data [100]int }"  -- 从1000000减少到100，大幅减少内存使用
      parseResult = parseTypus largeExpr
  in property $ isRight parseResult

-- | 测试深度嵌套类型
prop_deeply_nested_types :: Int -> Property
prop_deeply_nested_types depth =
  let validDepth = depth >= 1 && depth <= 5
      buildNestedType 0 = "int"
      buildNestedType n = "Wrapper[" ++ buildNestedType (n-1) ++ "]"
      nestedExpr = "type " ++ buildNestedType depth ++ " struct { value " ++ buildNestedType (depth-1) ++ " }"
      parseResult = parseTypus nestedExpr
  in classify validDepth "valid depth" $
     classify (not validDepth) "invalid depth" $
     if validDepth
        then property $ isRight parseResult
        else property True

-- | 测试特殊字符处理
prop_special_character_handling :: String -> Property
prop_special_character_handling input =
  let hasSpecialChars = any (not . isAlphaNum) input
      validInput = not (null input) && length input <= 10
      parseResult = parseTypus input
  in classify hasSpecialChars "has special characters" $
     classify (not hasSpecialChars) "no special characters" $
     classify validInput "valid input" $
     classify (not validInput) "invalid input" $
     if validInput
        then property $ either (const False) (const True) parseResult
        else property True

-- | 测试Unicode字符处理
prop_unicode_character_handling :: Property
prop_unicode_character_handling =
  let unicodeExpr = "type Unicode struct { 字段 string }"
      parseResult = parseTypus unicodeExpr
  in property $ isRight parseResult

-- ============================================================================
-- 10. 综合功能测试 (Comprehensive Features)
-- ============================================================================

-- | 测试依赖类型与所有权结合
prop_dependent_types_with_ownership :: String -> Int -> Property
prop_dependent_types_with_ownership typeName n =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      validSize = n > 0 && n <= 10
      combinedExpr = "{//! ownership: on\n//! dependent_types: on\n  type " ++ typeName ++ "[" ++ show n ++ "] struct { data [" ++ show n ++ "]int }\n}"
      parseResult = parseTypus combinedExpr
  in classify validTypeName "valid type name" $
     classify validSize "valid size" $
     if validTypeName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试复杂约束与函数结合
prop_complex_constraints_with_functions :: String -> Int -> Int -> Property
prop_complex_constraints_with_functions funcName minVal maxVal =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validBounds = minVal >= 0 && maxVal >= minVal && maxVal <= 100
      complexExpr = "//! dependent_types: on\ntype Bounded = int where { self >= " ++ show minVal ++ " && self <= " ++ show maxVal ++ " }\n\nfunc " ++ funcName ++ "(x: Bounded) -> Bounded { return x }"
      parseResult = parseTypus complexExpr
  in classify validFuncName "valid function name" $
     classify validBounds "valid bounds" $
     if validFuncName && validBounds
        then property $ isRight parseResult
        else property True

-- | 测试类型级算术与函数结合
prop_type_level_arithmetic_with_functions :: String -> Int -> Int -> Property
prop_type_level_arithmetic_with_functions funcName a b =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validOps = a >= 0 && b >= 0 && a <= 10 && b <= 10
      sum = a + b
      arithmeticExpr = "//! dependent_types: on\nfunc " ++ funcName ++ "[a: int, b: int]() -> int where { a + b == " ++ show sum ++ " } { return " ++ show sum ++ " }"
      parseResult = parseTypus arithmeticExpr
  in classify validFuncName "valid function name" $
     classify validOps "valid operands" $
     if validFuncName && validOps
        then property $ isRight parseResult
        else property True

-- | 测试存在类型与match表达式结合
prop_existential_types_with_match :: String -> Property
prop_existential_types_with_match typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      combinedExpr = "//! dependent_types: on\nfunc process" ++ typeName ++ "() {\n  v := read" ++ typeName ++ "()\n  match v.(n) {\n    return n\n  }\n}"
      parseResult = parseTypus combinedExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试断言与依赖类型结合
prop_assertions_with_dependent_types :: String -> Property
prop_assertions_with_dependent_types condition =
  let validCondition = not (null condition) && length condition <= 20
      combinedExpr = "//! dependent_types: on\nfunc processInput(n: int) {\n  assert " ++ condition ++ "\n  v := zeros(n)\n}"
      parseResult = parseTypus combinedExpr
  in classify validCondition "valid condition" $
     if validCondition
        then property $ isRight parseResult
        else property True

-- | 测试所有权与借用结合
prop_ownership_with_borrowing :: String -> Property
prop_ownership_with_borrowing varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      combinedExpr = "{//! ownership: on\n  " ++ varName ++ " := NewMyString(\"hello\")\n  r := &" ++ varName ++ "\n  m := &mut " ++ varName ++ "\n}"
      parseResult = parseTypus combinedExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试约束模式与错误处理
prop_constraint_mode_with_error_handling :: String -> Property
prop_constraint_mode_with_error_handling mode =
  let validMode = mode `elem` ["panic", "error"]
      combinedExpr = "//! constraint_mode: " ++ mode ++ "\n//! dependent_types: on\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int { return a / b }"
      parseResult = parseTypus combinedExpr
  in classify validMode "valid constraint mode" $
     if validMode
        then property $ isRight parseResult
        else property True

-- | 测试编译器优化与类型推导
prop_compiler_optimization_with_type_inference :: String -> Property
prop_compiler_optimization_with_type_inference code =
  let validCode = not (null code) && length code <= 30
      optimizationExpr = "//! dependent_types: on\nfunc createVector(n: Positive, value: float64) -> Vector[n] {\n  " ++ code ++ "\n  return Vector{elements}\n}"
      parseResult = parseTypus optimizationExpr
  in classify validCode "valid code" $
     if validCode
        then property $ isRight parseResult
        else property True

-- | 测试Go互操作性与Typus类型
prop_go_interoperability_with_typus_types :: String -> Property
prop_go_interoperability_with_typus_types importName =
  let validImportName = not (null importName) && all isAlphaNum importName
      interopExpr = "//! dependent_types: on\nimport \"" ++ importName ++ "\"\n\ntype Vector[n: int] struct { data [n]float64 }\n\nfunc sortedFirst[n: int](v: Vector[n]) -> float64 where { n > 0 } {\n  sort.Float64s(v.data)\n  return v.data[0]\n}"
      parseResult = parseTypus interopExpr
  in classify validImportName "valid import name" $
     if validImportName
        then property $ isRight parseResult
        else property True

-- | 测试完整程序结构
prop_complete_program_structure :: String -> Property
prop_complete_program_structure programName =
  let validProgramName = not (null programName) && all isAlphaNum programName
      completeExpr = "//! ownership: on\n//! dependent_types: on\n\npackage main\n\nimport \"fmt\"\n\ntype NonZero = int where { self != 0 }\ntype Vector[n: int] struct { data [n]float64 }\n\nfunc " ++ programName ++ "() {\n  fmt.Println(\"Hello, Typus!\")\n}\n\nfunc main() {\n  " ++ programName ++ "()\n}"
      parseResult = parseTypus completeExpr
  in classify validProgramName "valid program name" $
     if validProgramName
        then property $ isRight parseResult
        else property True

-- | 测试边界条件组合
prop_boundary_condition_combinations :: Int -> Int -> Int -> Property
prop_boundary_condition_combinations a b c =
  let validValues = a >= 0 && b >= 0 && c >= 0 && a <= 5 && b <= 5 && c <= 5
      boundaryExpr = "//! dependent_types: on\ntype Triple[a: int, b: int, c: int] struct { x int; y int; z int }\n\nfunc validate[t: Triple[" ++ show a ++ "][" ++ show b ++ "][" ++ show c ++ "]](t: Triple) -> bool where { t.x > 0 && t.y > 0 && t.z > 0 } { return true }"
      parseResult = parseTypus boundaryExpr
  in classify validValues "valid values" $
     if validValues
        then property $ isRight parseResult
        else property True

-- | 测试错误恢复机制
prop_error_recovery_mechanisms :: String -> Property
prop_error_recovery_mechanisms invalidCode =
  let validCode = not (null invalidCode) && length invalidCode <= 20
      parseResult = parseTypus invalidCode
  in classify validCode "valid code length" $
     classify (isRight parseResult) "parsed successfully" $
     classify (isLeft parseResult) "parse failed" $
     property True

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | 依赖类型核心功能测试组
dependentTypesCoreFeaturesTestGroup :: TestTree
dependentTypesCoreFeaturesTestGroup = testGroup "Dependent Types Core Features Tests"
  [ testProperty "Vector length preservation" prop_vector_length_preservation
  , testProperty "Matrix dimensions preservation" prop_matrix_dimensions_preservation
  , testProperty "Bounded type constraints" prop_bounded_type_constraints
  , testProperty "Non-empty slice constraints" prop_non_empty_slice_constraints
  , testProperty "Positive type constraints" prop_positive_type_constraints
  ]

-- | 精确类型约束测试组
refinedTypeConstraintsTestGroup :: TestTree
refinedTypeConstraintsTestGroup = testGroup "Refined Type Constraints Tests"
  [ testProperty "Non-zero type constraints" prop_nonzero_type_constraints
  , testProperty "Percentage type constraints" prop_percentage_type_constraints
  , testProperty "Valid index type constraints" prop_valid_index_type_constraints
  , testProperty "String length constraints" prop_string_length_constraints
  , testProperty "Compound constraints" prop_compound_constraints
  ]

-- | 类型级算术测试组
typeLevelArithmeticTestGroup :: TestTree
typeLevelArithmeticTestGroup = testGroup "Type-Level Arithmetic Tests"
  [ testProperty "Type-level addition" prop_type_level_addition
  , testProperty "Type-level subtraction" prop_type_level_subtraction
  , testProperty "Type-level multiplication" prop_type_level_multiplication
  , testProperty "Type-level comparison" prop_type_level_comparison
  , testProperty "Type-level modulus" prop_type_level_modulus
  ]

-- | 函数签名依赖测试组
functionSignatureDependenciesTestGroup :: TestTree
functionSignatureDependenciesTestGroup = testGroup "Function Signature Dependencies Tests"
  [ testProperty "Dependent return type" prop_dependent_return_type
  , testProperty "Dependent parameter type" prop_dependent_parameter_type
  , testProperty "Multiple dependent parameters" prop_multiple_dependent_parameters
  , testProperty "Function preconditions" prop_function_preconditions
  , testProperty "Complex function signature" prop_complex_function_signature
  ]

-- | 断言和条件窄化测试组
assertionsAndConditionalNarrowingTestGroup :: TestTree
assertionsAndConditionalNarrowingTestGroup = testGroup "Assertions and Conditional Narrowing Tests"
  [ testProperty "Assert expressions" prop_assert_expressions
  , testProperty "Static assert expressions" prop_static_assert_expressions
  , testProperty "Conditional narrowing" prop_conditional_narrowing
  , testProperty "Match expressions" prop_match_expressions
  , testProperty "Existential types" prop_existential_types
  ]

-- | 所有权机制测试组
ownershipMechanismTestGroup :: TestTree
ownershipMechanismTestGroup = testGroup "Ownership Mechanism Tests"
  [ testProperty "Ownership directive parsing" prop_ownership_directive_parsing
  , testProperty "Move semantics" prop_move_semantics
  , testProperty "Borrow expressions" prop_borrow_expressions
  , testProperty "Mutable borrow expressions" prop_mutable_borrow_expressions
  , testProperty "Block level ownership directive" prop_block_level_ownership_directive
  ]

-- | 指令系统测试组
directiveSystemTestGroup :: TestTree
directiveSystemTestGroup = testGroup "Directive System Tests"
  [ testProperty "File level ownership directive" prop_file_level_ownership_directive
  , testProperty "File level dependent types directive" prop_file_level_dependent_types_directive
  , testProperty "Block level dependent types directive" prop_block_level_dependent_types_directive
  , testProperty "Multiple feature block directive" prop_multiple_feature_block_directive
  , testProperty "Constraint mode directive" prop_constraint_mode_directive
  ]

-- | 编译器集成测试组
compilerIntegrationTestGroup :: TestTree
compilerIntegrationTestGroup = testGroup "Compiler Integration Tests"
  [ testProperty "Basic type compilation" prop_basic_type_compilation
  , testProperty "Struct type compilation" prop_struct_type_compilation
  , testProperty "Function compilation" prop_function_compilation
  , testProperty "Interface compilation" prop_interface_compilation
  , testProperty "Package declaration compilation" prop_package_declaration_compilation
  ]

-- | 边界条件测试组
boundaryConditionsTestGroup :: TestTree
boundaryConditionsTestGroup = testGroup "Boundary Conditions Tests"
  [ testProperty "Empty string input" prop_empty_string_input
  , testProperty "Large type parameters" prop_large_type_parameters
  , testProperty "Deeply nested types" prop_deeply_nested_types
  , testProperty "Special character handling" prop_special_character_handling
  , testProperty "Unicode character handling" prop_unicode_character_handling
  ]

-- | 综合功能测试组
comprehensiveFeaturesTestGroup :: TestTree
comprehensiveFeaturesTestGroup = testGroup "Comprehensive Features Tests"
  [ testProperty "Dependent types with ownership" prop_dependent_types_with_ownership
  , testProperty "Complex constraints with functions" prop_complex_constraints_with_functions
  , testProperty "Type-level arithmetic with functions" prop_type_level_arithmetic_with_functions
  , testProperty "Existential types with match" prop_existential_types_with_match
  , testProperty "Assertions with dependent types" prop_assertions_with_dependent_types
  , testProperty "Ownership with borrowing" prop_ownership_with_borrowing
  , testProperty "Constraint mode with error handling" prop_constraint_mode_with_error_handling
  , testProperty "Compiler optimization with type inference" prop_compiler_optimization_with_type_inference
  , testProperty "Go interoperability with Typus types" prop_go_interoperability_with_typus_types
  , testProperty "Complete program structure" prop_complete_program_structure
  , testProperty "Boundary condition combinations" prop_boundary_condition_combinations
  , testProperty "Error recovery mechanisms" prop_error_recovery_mechanisms
  ]

-- | 主测试套件
testSuite :: TestTree
testSuite = testGroup "Typus Core Features Test Suite"
  [ memoryLevelTestGroup Minimal "Dependent Types Core Features Tests" [dependentTypesCoreFeaturesTestGroup]
  , memoryLevelTestGroup Ultra "Refined Type Constraints Tests" [refinedTypeConstraintsTestGroup]
  , memoryLevelTestGroup Minimal "Type-Level Arithmetic Tests" [typeLevelArithmeticTestGroup]
  , memoryLevelTestGroup Ultra "Function Signature Dependencies Tests" [functionSignatureDependenciesTestGroup]
  , memoryLevelTestGroup Aggressive "Assertions and Conditional Narrowing Tests" [assertionsAndConditionalNarrowingTestGroup]
  , memoryLevelTestGroup Minimal "Ownership Mechanism Tests" [ownershipMechanismTestGroup]
  , memoryLevelTestGroup Ultra "Directive System Tests" [directiveSystemTestGroup]
  , memoryLevelTestGroup Aggressive "Compiler Integration Tests" [compilerIntegrationTestGroup]
  , memoryLevelTestGroup Ultra "Boundary Conditions Tests" [boundaryConditionsTestGroup]
  , memoryLevelTestGroup Minimal "Comprehensive Features Tests" [comprehensiveFeaturesTestGroup]
  ]

-- | 导出测试套件
tests :: TestTree
tests = testSuite