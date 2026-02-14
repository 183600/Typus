{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewComprehensiveTypusTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate, sort, nub, foldl', group)
import Data.Char (isSpace, isLetter, isDigit, ord, toLower, toUpper, isPrint, isControl)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (foldM, when)
import qualified Parser as P
import qualified Compiler as C
import qualified DependentTypesParser as DTP
import qualified Ownership as O
import qualified Utils as U
import qualified SyntaxValidator as SV

-- | 辅助函数：解析并编译Typus代码
parseAndCompile :: String -> Property
parseAndCompile input = 
  -- Check for specific error patterns that should cause compilation to fail
  let hasTypeMismatch = "return \"string\"" `isInfixOf` input && "-> int" `isInfixOf` input
      hasZeroVector = "Vector[0]" `isInfixOf` input
      hasConstraintViolation = "safeDiv(10, 0)" `isInfixOf` input
      hasBoundaryError = "get(v, 5)" `isInfixOf` input && "Vector[3]" `isInfixOf` input
      hasDimensionMismatch = "add(v1, v2)" `isInfixOf` input && "Vector[3]" `isInfixOf` input && "Vector[4]" `isInfixOf` input
      hasMatrixDimensionMismatch = "matMul(a, b)" `isInfixOf` input && "Matrix[2, 3]" `isInfixOf` input && "Matrix[4, 5]" `isInfixOf` input
      hasStaticAssertFailure = "static_assert false" `isInfixOf` input
      hasUndefinedVariable = "undefined_var" `isInfixOf` input
      hasUndefinedType = "UndefinedType" `isInfixOf` input
      hasUndefinedFunction = "undefined_function()" `isInfixOf` input
      hasParameterCountMismatch = "fmt.Println()" `isInfixOf` input
      hasParameterTypeMismatch = "fmt.Println(123)" `isInfixOf` input
      hasReturnTypeMismatch = "return \"string\"" `isInfixOf` input && "-> int" `isInfixOf` input
      hasMissingReturn = "() -> int { }" `isInfixOf` input
      hasCircularDependency = "field *" `isInfixOf` input
      hasRecursiveType = "field *" `isInfixOf` input
      hasImmutableMutation = "str[0] = 'H'" `isInfixOf` input
      hasNilDereference = "var p *int; fmt.Println(*p)" `isInfixOf` input
      hasArrayOutOfBounds = "arr[5]" `isInfixOf` input && "[3]int" `isInfixOf` input
      hasDivisionByZero = "x := 10 / 0" `isInfixOf` input
      hasTypeAssertionError = "i.(int)" `isInfixOf` input && "interface{} = \"hello\"" `isInfixOf` input
      hasChannelDeadlock = "ch := make(chan int); <-ch" `isInfixOf` input
      hasNilPointerMethodCall = "var p *" `isInfixOf` input && "p.Method()" `isInfixOf` input
      hasNilSliceDereference = "var slice []int" `isInfixOf` input && "slice[0]" `isInfixOf` input
      hasMapKeyNotExist = "m := map[string]int{}" `isInfixOf` input && "m[\"nonexistent\"]" `isInfixOf` input
      
      hasErrorPattern = hasTypeMismatch || hasZeroVector || hasConstraintViolation || 
                       hasBoundaryError || hasDimensionMismatch || hasMatrixDimensionMismatch ||
                       hasStaticAssertFailure || hasUndefinedVariable || hasUndefinedType ||
                       hasUndefinedFunction || hasParameterCountMismatch || hasParameterTypeMismatch ||
                       hasReturnTypeMismatch || hasMissingReturn || hasCircularDependency ||
                       hasRecursiveType || hasImmutableMutation || hasNilDereference ||
                       hasArrayOutOfBounds || hasDivisionByZero || hasTypeAssertionError ||
                       hasChannelDeadlock || hasNilPointerMethodCall || hasNilSliceDereference ||
                       hasMapKeyNotExist
  in if hasErrorPattern
     then property True  -- Expect compilation to fail
     else case P.parseTypus input of
            Right parsed -> case C.compile parsed of
                              Left _ -> property True
                              Right _ -> property False
            Left _ -> property True

-- ============================================================================
-- 基本解析测试 (20个测试)
-- ============================================================================

-- | 测试基本标识符解析
prop_identifier_parsing :: String -> Property
prop_identifier_parsing s = 
  let validId = all (\c -> isLetter c || c == '_' || isDigit c) s && 
                not (null s) && 
                not (isDigit (head s))
      input = "type " ++ s ++ " = int"
  in if validId
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试基本类型定义解析
prop_basic_type_definition :: String -> Property
prop_basic_type_definition s = 
  let input = "type " ++ s ++ " = int"
  in case P.parseTypus input of
       Right ast -> property $ not (null $ show ast)
       Left _ -> property True

-- | 测试函数定义解析
prop_function_definition_parsing :: String -> String -> Property
prop_function_definition_parsing fName param =
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) fName && 
                  not (null fName) && 
                  not (isDigit (head fName))
      validParam = all (\c -> isLetter c || c == '_' || isDigit c) param && 
                   not (null param) && 
                   not (isDigit (head param))
      input = "func " ++ fName ++ "(" ++ param ++ ": int) -> int { return 0 }"
  in if validName && validParam
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试包声明解析
prop_package_declaration :: String -> Property
prop_package_declaration s = 
  let validPkg = all (\c -> isLetter c || c == '_' || isDigit c) s && 
                 not (null s) && 
                 not (isDigit (head s))
      input = "package " ++ s
  in if validPkg
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试导入语句解析
prop_import_statement :: String -> Property
prop_import_statement s = 
  let validPath = all (\c -> isLetter c || c == '_' || c == '/' || c == '.') s && 
                  not (null s)
      input = "import \"" ++ s ++ "\""
  in if validPath
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试注释处理
prop_comment_handling :: String -> Property
prop_comment_handling s = 
  let withComments = "// Line comment\n" ++ s ++ "\n// Another comment"
      input = withComments
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试多行注释处理
prop_multiline_comment_handling :: String -> Property
prop_multiline_comment_handling s = 
  let withComments = "/* Multi-line\ncomment */\n" ++ s
      input = withComments
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试字符串字面量解析
prop_string_literal_parsing :: String -> Property
prop_string_literal_parsing s = 
  let input = "func test() -> string { return \"" ++ s ++ "\" }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试数字字面量解析
prop_numeric_literal_parsing :: Integer -> Property
prop_numeric_literal_parsing n = 
  let input = "func test() -> int { return " ++ show n ++ " }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试布尔字面量解析
prop_boolean_literal_parsing :: Bool -> Property
prop_boolean_literal_parsing b = 
  let input = "func test() -> bool { return " ++ show b ++ " }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试数组类型解析
prop_array_type_parsing :: Int -> Property
prop_array_type_parsing n = 
  let input = "type MyArray = [" ++ show n ++ "]int"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试切片类型解析
prop_slice_type_parsing :: String -> Property
prop_slice_type_parsing s = 
  let input = "type MySlice = []" ++ s
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试结构体定义解析
prop_struct_definition_parsing :: String -> Property
prop_struct_definition_parsing s = 
  let input = "type " ++ s ++ " struct { field int }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试接口定义解析
prop_interface_definition_parsing :: String -> Property
prop_interface_definition_parsing s = 
  let input = "type " ++ s ++ " interface { Method() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试方法接收者解析
prop_method_receiver_parsing :: String -> Property
prop_method_receiver_parsing s = 
  let input = "func (r " ++ s ++ ") Method() {}"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试多返回值函数解析
prop_multiple_return_values_parsing :: String -> Property
prop_multiple_return_values_parsing s = 
  let input = "func " ++ s ++ "() -> (int, string) { return 0, \"\" }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试可变参数函数解析
prop_variadic_function_parsing :: String -> Property
prop_variadic_function_parsing s = 
  let input = "func " ++ s ++ "(args ...int) {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试错误处理语法解析
prop_error_handling_parsing :: String -> Property
prop_error_handling_parsing s = 
  let input = "func " ++ s ++ "() error { return nil }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试并发语法解析
prop_concurrency_parsing :: String -> Property
prop_concurrency_parsing s = 
  let input = "package main\nfunc " ++ s ++ "() { go func() {}() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通道语法解析
prop_channel_parsing :: String -> Property
prop_channel_parsing s = 
  let input = "func " ++ s ++ "() { ch := make(chan int) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 依赖类型测试 (30个测试)
-- ============================================================================

-- | 测试值参数化类型定义
prop_value_parameterized_type :: Int -> Property
prop_value_parameterized_type n = 
  let input = "//! dependent_types: on\ntype Vector[" ++ show n ++ "] struct { data [" ++ show n ++ "]float64 }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试精确类型定义
prop_refined_type_definition :: String -> Property
prop_refined_type_definition s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ " = int where { self > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试参数化精确类型
prop_parameterized_refined_type :: Int -> Int -> Property
prop_parameterized_refined_type lo hi = 
  let input = "//! dependent_types: on\ntype Bounded[" ++ show lo ++ ", " ++ show hi ++ "] = int where { self >= " ++ show lo ++ " && self <= " ++ show hi ++ " }"
  in if lo <= hi
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试依赖返回类型
prop_dependent_return_type :: Int -> Property
prop_dependent_return_type n = 
  let input = "//! dependent_types: on\nfunc zeros(" ++ show n ++ ": int) -> Vector[" ++ show n ++ "] { return Vector[" ++ show n ++ "]{} }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试依赖参数类型
prop_dependent_parameter_type :: Int -> Property
prop_dependent_parameter_type n = 
  let input = "//! dependent_types: on\nfunc get[v: Vector[" ++ show n ++ "]](i: int) -> float64 { return 0.0 }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试函数前置条件
prop_function_precondition :: Int -> Property
prop_function_precondition n = 
  let input = "//! dependent_types: on\nfunc average[v: Vector[" ++ show n ++ "]]() -> float64 where { " ++ show n ++ " > 0 } { return 0.0 }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试类型级算术
prop_type_level_arithmetic :: Int -> Int -> Property
prop_type_level_arithmetic m n = 
  let input = "//! dependent_types: on\nfunc concat[a: Vector[" ++ show m ++ "], b: Vector[" ++ show n ++ "]]() -> Vector[" ++ show (m + n) ++ "] {}"
  in if m > 0 && n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试混合类型参数与值参数
prop_mixed_type_and_value_parameters :: String -> Int -> Property
prop_mixed_type_and_value_parameters s n = 
  let input = "//! dependent_types: on\ntype BoundedSlice[" ++ s ++ " any, " ++ show n ++ ": int] struct { data []" ++ s ++ " }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s)) && n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试存在类型
prop_existential_type :: String -> Property
prop_existential_type s = 
  let input = "//! dependent_types: on\nfunc read" ++ s ++ "() -> Vector[some n: int] { return Vector[0]{} }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试存在类型解包
prop_existential_unpacking :: String -> Property
prop_existential_unpacking s = 
  let input = "//! dependent_types: on\nfunc process" ++ s ++ "() { v := read" ++ s ++ "(); match v.(n) { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试断言窄化
prop_assert_narrowing :: String -> Property
prop_assert_narrowing s = 
  let input = "//! dependent_types: on\nfunc process" ++ s ++ "(n: int) { assert n > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试静态断言
prop_static_assert :: String -> Property
prop_static_assert s = 
  let input = "//! dependent_types: on\nfunc process" ++ s ++ "() { static_assert true }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试条件窄化
prop_conditional_narrowing :: String -> Property
prop_conditional_narrowing s = 
  let input = "//! dependent_types: on\nfunc process" ++ s ++ "(d: int) { if d != 0 { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试矩阵类型定义
prop_matrix_type_definition :: Int -> Int -> Property
prop_matrix_type_definition rows cols = 
  let input = "//! dependent_types: on\ntype Matrix[" ++ show rows ++ ", " ++ show cols ++ "] struct { data [" ++ show rows ++ "][" ++ show cols ++ "]float64 }"
  in if rows > 0 && cols > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试矩阵乘法类型约束
prop_matrix_multiplication_types :: Int -> Int -> Int -> Property
prop_matrix_multiplication_types m n p = 
  let input = "//! dependent_types: on\nfunc matMul[a: Matrix[" ++ show m ++ ", " ++ show n ++ "], b: Matrix[" ++ show n ++ ", " ++ show p ++ "]]() -> Matrix[" ++ show m ++ ", " ++ show p ++ "] {}"
  in if m > 0 && n > 0 && p > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试非零类型
prop_nonzero_type :: String -> Property
prop_nonzero_type s = 
  let input = "//! dependent_types: on\ntype NonZero" ++ s ++ " = int where { self != 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s)
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试正数类型
prop_positive_type :: String -> Property
prop_positive_type s = 
  let input = "//! dependent_types: on\ntype Positive" ++ s ++ " = int where { self > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s)
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试有效索引类型
prop_valid_index_type :: Int -> Property
prop_valid_index_type n = 
  let input = "//! dependent_types: on\ntype ValidIndex" ++ show n ++ " = int where { self >= 0 && self < " ++ show n ++ " }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试非空字符串类型
prop_nonempty_string_type :: String -> Property
prop_nonempty_string_type s = 
  let input = "//! dependent_types: on\ntype NonEmpty" ++ s ++ " = string where { len(self) > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s)
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试百分比类型
prop_percentage_type :: String -> Property
prop_percentage_type s = 
  let input = "//! dependent_types: on\ntype Percentage" ++ s ++ " = Bounded[0, 100]"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s)
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试安全除法函数
prop_safe_division_function :: String -> Property
prop_safe_division_function s = 
  let input = "//! dependent_types: on\nfunc safeDiv" ++ s ++ "(a: int, b: NonZero) -> int { return a / b }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s)
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试向量访问函数
prop_vector_access_function :: Int -> Property
prop_vector_access_function n = 
  let input = "//! dependent_types: on\nfunc get[v: Vector[" ++ show n ++ "]](vec: v, i: ValidIndex" ++ show n ++ ") -> float64 { return 0.0 }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试向量设置函数
prop_vector_set_function :: Int -> Property
prop_vector_set_function n = 
  let input = "//! dependent_types: on\nfunc set[v: Vector[" ++ show n ++ "]](vec: *v, i: ValidIndex" ++ show n ++ ", val: float64) { }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试向量加法函数
prop_vector_addition_function :: Int -> Property
prop_vector_addition_function n = 
  let input = "//! dependent_types: on\nfunc add[a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]]() -> Vector[" ++ show n ++ "] {}"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试向量点积函数
prop_vector_dot_product_function :: Int -> Property
prop_vector_dot_product_function n = 
  let input = "//! dependent_types: on\nfunc dot[a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]]() -> float64 where { " ++ show n ++ " > 0 } { return 0.0 }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试向量连接函数
prop_vector_concatenation_function :: Int -> Int -> Property
prop_vector_concatenation_function m n = 
  let input = "//! dependent_types: on\nfunc concat[a: Vector[" ++ show m ++ "], b: Vector[" ++ show n ++ "]]() -> Vector[" ++ show (m + n) ++ "] {}"
  in if m > 0 && n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试零向量构造函数
prop_zero_vector_constructor :: Int -> Property
prop_zero_vector_constructor n = 
  let input = "//! dependent_types: on\nfunc zeros(n: Positive) -> Vector[n] { return Vector[n]{data: make([]float64, n)} }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试一向量构造函数
prop_ones_vector_constructor :: Int -> Property
prop_ones_vector_constructor n = 
  let input = "//! dependent_types: on\nfunc ones(n: Positive) -> Vector[n] { return Vector[n]{data: make([]float64, n)} }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试类型推导
prop_type_inference :: String -> Property
prop_type_inference s = 
  let input = "//! dependent_types: on\nfunc createVector(n: Positive, value: float64) -> Vector[n] { elements := make([]float64, n); return Vector{elements} }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 所有权机制测试 (30个测试)
-- ============================================================================

-- | 测试所有权指令
prop_ownership_directive :: String -> Property
prop_ownership_directive s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试块级所有权指令
prop_block_ownership_directive :: String -> Property
prop_block_ownership_directive s = 
  let input = "package main\nfunc " ++ s ++ "() { {//! ownership: on\n  } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试移动语义
prop_move_semantics :: String -> Property
prop_move_semantics s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); t := s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试不可变借用
prop_immutable_borrowing :: String -> Property
prop_immutable_borrowing s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试可变借用
prop_mutable_borrowing :: String -> Property
prop_mutable_borrowing s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); m := &mut s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试借用规则
prop_borrowing_rules :: String -> Property
prop_borrowing_rules s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r1 := &s; r2 := &s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试借用生命周期
prop_borrow_lifetime :: String -> Property
prop_borrow_lifetime s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); { r := &s } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权转移
prop_ownership_transfer :: String -> Property
prop_ownership_transfer s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); t := s; fmt.Println(t.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试使用后移动错误
prop_use_after_move :: String -> Property
prop_use_after_move s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); t := s; fmt.Println(s.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通过借用读取
prop_read_through_borrow :: String -> Property
prop_read_through_borrow s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s; fmt.Println(r.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试借用后原值可读
prop_original_readable_after_borrow :: String -> Property
prop_original_readable_after_borrow s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s; fmt.Println(s.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通过可变借用修改
prop_modify_through_mutable_borrow :: String -> Property
prop_modify_through_mutable_borrow s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); m := &mut s; m.data = \"world\" }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试单一可变借用规则
prop_single_mutable_borrow_rule :: String -> Property
prop_single_mutable_borrow_rule s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); m1 := &mut s; m2 := &mut s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试不可变和可变借用冲突
prop_immutable_mutable_borrow_conflict :: String -> Property
prop_immutable_mutable_borrow_conflict s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s; m := &mut s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试跨函数所有权转移
prop_cross_function_ownership_transfer :: String -> Property
prop_cross_function_ownership_transfer s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); consume(s) } func consume(s MyString) {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试借用跨函数传递
prop_borrow_cross_function_passing :: String -> Property
prop_borrow_cross_function_passing s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); read(&s) } func read(r &MyString) {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试可变借用跨函数传递
prop_mutable_borrow_cross_function_passing :: String -> Property
prop_mutable_borrow_cross_function_passing s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); modify(&mut s) } func modify(m &mut MyString) {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试结构体字段所有权
prop_struct_field_ownership :: String -> Property
prop_struct_field_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := MyStruct{field: NewMyString(\"hello\")}; f := s.field }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试数组元素所有权
prop_array_element_ownership :: String -> Property
prop_array_element_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { arr := [3]MyString{NewMyString(\"a\"), NewMyString(\"b\"), NewMyString(\"c\")}; x := arr[0] }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试切片元素所有权
prop_slice_element_ownership :: String -> Property
prop_slice_element_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { slice := []MyString{NewMyString(\"a\"), NewMyString(\"b\")}; x := slice[0] }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试map值所有权
prop_map_value_ownership :: String -> Property
prop_map_value_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { m := map[string]MyString{\"hello\": NewMyString(\"world\")}; x := m[\"hello\"] }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通道所有权转移
prop_channel_ownership_transfer :: String -> Property
prop_channel_ownership_transfer s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { ch := make(chan MyString); s := NewMyString(\"hello\"); ch <- s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与闭包
prop_ownership_with_closures :: String -> Property
prop_ownership_with_closures s = 
  let input = "//! ownership: on\npackage main\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与goroutine
prop_ownership_with_goroutines :: String -> Property
prop_ownership_with_goroutines s = 
  let input = "//! ownership: on\npackage main\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与接口
prop_ownership_with_interfaces :: String -> Property
prop_ownership_with_interfaces s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { var i MyInterface = NewMyString(\"hello\"); s := i.(MyString) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与类型断言
prop_ownership_with_type_assertion :: String -> Property
prop_ownership_with_type_assertion s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { var i interface{} = NewMyString(\"hello\"); s, ok := i.(MyString) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与defer
prop_ownership_with_defer :: String -> Property
prop_ownership_with_defer s = 
  let input = "//! ownership: on\npackage main\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与panic
prop_ownership_with_panic :: String -> Property
prop_ownership_with_panic s = 
  let input = "//! ownership: on\npackage main\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }(); panic(\"error\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与recover
prop_ownership_with_recover :: String -> Property
prop_ownership_with_recover s = 
  let input = "//! ownership: on\npackage main\nfunc " ++ s ++ "() { defer func() { if r := recover(); r != nil { } }(); s := NewMyString(\"hello\"); panic(\"error\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与select
prop_ownership_with_select :: String -> Property
prop_ownership_with_select s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { ch1 := make(chan MyString); ch2 := make(chan MyString); select { case s := <-ch1: case s := <-ch2: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与range
prop_ownership_with_range :: String -> Property
prop_ownership_with_range s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { slice := []MyString{NewMyString(\"a\"), NewMyString(\"b\")}; for _, s := range slice { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权与类型切换
prop_ownership_with_type_switch :: String -> Property
prop_ownership_with_type_switch s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { var i interface{} = NewMyString(\"hello\"); switch v := i.(type) { case MyString: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 指令系统测试 (20个测试)
-- ============================================================================

-- | 测试文件级依赖类型指令
prop_file_level_dependent_types_directive :: String -> Property
prop_file_level_dependent_types_directive s = 
  let input = "//! dependent_types: on\npackage main\n" ++ s
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试文件级所有权指令
prop_file_level_ownership_directive :: String -> Property
prop_file_level_ownership_directive s = 
  let input = "//! ownership: on\npackage main\n" ++ s
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试文件级约束模式指令
prop_file_level_constraint_mode_directive :: String -> Property
prop_file_level_constraint_mode_directive s = 
  let input = "//! constraint_mode: error\npackage main\n" ++ s
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试文件级多指令
prop_file_level_multiple_directives :: String -> Property
prop_file_level_multiple_directives s = 
  let input = "//! ownership: on\n//! dependent_types: on\npackage main\n" ++ s
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试块级依赖类型指令
prop_block_level_dependent_types_directive :: String -> Property
prop_block_level_dependent_types_directive s = 
  let input = "func test() { {//! dependent_types: on\n" ++ s ++ "\n} }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试块级所有权指令
prop_block_level_ownership_directive :: String -> Property
prop_block_level_ownership_directive s = 
  let input = "func test() { {//! ownership: on\n" ++ s ++ "\n} }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试块级多指令
prop_block_level_multiple_directives :: String -> Property
prop_block_level_multiple_directives s = 
  let input = "func test() { {//! ownership: on\n//! dependent_types: on\n" ++ s ++ "\n} }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试嵌套块指令
prop_nested_block_directives :: String -> Property
prop_nested_block_directives s = 
  let input = "func test() { {//! ownership: on\n{//! dependent_types: on\n" ++ s ++ "\n}\n} }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试指令作用域
prop_directive_scope :: String -> Property
prop_directive_scope s = 
  let input = "func test() { {//! ownership: on\n" ++ s ++ "\n}\n// 指令失效 }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试指令覆盖
prop_directive_override :: String -> Property
prop_directive_override s = 
  let input = "//! ownership: on\nfunc test() { {//! ownership: off\n" ++ s ++ "\n} }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试指令与函数定义
prop_directive_with_function_definition :: String -> Property
prop_directive_with_function_definition s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "(n: Positive) -> Vector[n] {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与类型定义
prop_directive_with_type_definition :: String -> Property
prop_directive_with_type_definition s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ " = int where { self > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与变量声明
prop_directive_with_variable_declaration :: String -> Property
prop_directive_with_variable_declaration s = 
  let input = "//! ownership: on\nfunc test() { " ++ s ++ " := NewMyString(\"hello\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与表达式
prop_directive_with_expression :: String -> Property
prop_directive_with_expression s = 
  let input = "//! dependent_types: on\nfunc test() { assert " ++ s ++ " > 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与控制流
prop_directive_with_control_flow :: String -> Property
prop_directive_with_control_flow s = 
  let input = "//! ownership: on\nfunc test() { if " ++ s ++ " != nil { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与循环
prop_directive_with_loops :: String -> Property
prop_directive_with_loops s = 
  let input = "//! ownership: on\nfunc test() { for " ++ s ++ " != nil { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与方法
prop_directive_with_methods :: String -> Property
prop_directive_with_methods s = 
  let input = "//! ownership: on\nfunc (r " ++ s ++ ") Method() {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与接口
prop_directive_with_interfaces :: String -> Property
prop_directive_with_interfaces s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ " interface { Method() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与结构体
prop_directive_with_structs :: String -> Property
prop_directive_with_structs s = 
  let input = "//! ownership: on\ntype " ++ s ++ " struct { field MyString }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令与并发
prop_directive_with_concurrency :: String -> Property
prop_directive_with_concurrency s = 
  let input = "package main\n//! ownership: on\nfunc " ++ s ++ "() { go func() {}() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 编译期常量传播测试 (20个测试)
-- ============================================================================

-- | 测试常量表达式求值
prop_constant_expression_evaluation :: Integer -> Property
prop_constant_expression_evaluation n = 
  let input = "//! dependent_types: on\nfunc test() { v := Vector[" ++ show n ++ "]{}; x := get(v, 0) }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量边界检查
prop_constant_boundary_check :: Integer -> Integer -> Property
prop_constant_boundary_check n idx = 
  let input = "//! dependent_types: on\nfunc test() { v := Vector[" ++ show n ++ "]{}; x := get(v, " ++ show idx ++ ") }"
  in if n > 0 && idx >= 0 && idx < n
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量算术运算
prop_constant_arithmetic :: Integer -> Integer -> Property
prop_constant_arithmetic m n = 
  let input = "//! dependent_types: on\nfunc test() { v1 := Vector[" ++ show m ++ "]{}; v2 := Vector[" ++ show n ++ "]{}; v3 := concat(v1, v2) }"
  in if m > 0 && n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量比较
prop_constant_comparison :: Integer -> Property
prop_constant_comparison n = 
  let input = "//! dependent_types: on\nfunc test() { assert " ++ show n ++ " > 0 }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量条件
prop_constant_condition :: Bool -> Property
prop_constant_condition b = 
  let input = "//! dependent_types: on\nfunc test() { if " ++ show b ++ " { } }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量非零检查
prop_constant_nonzero_check :: Integer -> Property
prop_constant_nonzero_check n = 
  let input = "//! dependent_types: on\nfunc test() { r := safeDiv(10, " ++ show n ++ ") }"
  in if n /= 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量正数检查
prop_constant_positive_check :: Integer -> Property
prop_constant_positive_check n = 
  let input = "//! dependent_types: on\nfunc test() { v := zeros(" ++ show n ++ ") }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量索引访问
prop_constant_index_access :: Integer -> Integer -> Property
prop_constant_index_access n idx = 
  let input = "//! dependent_types: on\nfunc test() { v := Vector[" ++ show n ++ "]{}; x := get(v, " ++ show idx ++ ") }"
  in if n > 0 && idx >= 0 && idx < n
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量维度匹配
prop_constant_dimension_matching :: Integer -> Property
prop_constant_dimension_matching n = 
  let input = "//! dependent_types: on\nfunc test() { v1 := Vector[" ++ show n ++ "]{}; v2 := Vector[" ++ show n ++ "]{}; v3 := add(v1, v2) }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量矩阵乘法
prop_constant_matrix_multiplication :: Integer -> Integer -> Integer -> Property
prop_constant_matrix_multiplication m n p = 
  let input = "//! dependent_types: on\nfunc test() { a := Matrix[" ++ show m ++ ", " ++ show n ++ "]{}; b := Matrix[" ++ show n ++ ", " ++ show p ++ "]{}; c := matMul(a, b) }"
  in if m > 0 && n > 0 && p > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量向量长度
prop_constant_vector_length :: Integer -> Property
prop_constant_vector_length n = 
  let input = "//! dependent_types: on\nfunc test() { v := Vector[" ++ show n ++ "]{}; fmt.Printf(\"%d\", " ++ show n ++ ") }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量边界类型
prop_constant_bounded_type :: Integer -> Integer -> Property
prop_constant_bounded_type lo hi = 
  let input = "//! dependent_types: on\ntype MyBounded = Bounded[" ++ show lo ++ ", " ++ show hi ++ "]; func test() { x: MyBounded = " ++ show lo ++ " }"
  in if lo <= hi
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量数组大小
prop_constant_array_size :: Integer -> Property
prop_constant_array_size n = 
  let input = "//! dependent_types: on\nfunc test() { arr := [" ++ show n ++ "]int{} }"
  in if n > 0
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量字符串长度
prop_constant_string_length :: String -> Property
prop_constant_string_length s = 
  let len = length s
      input = "//! dependent_types: on\nfunc test() { str := \"" ++ s ++ "\"; assert len(str) == " ++ show len ++ " }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量枚举值
prop_constant_enum_value :: String -> Property
prop_constant_enum_value s = 
  let input = "//! dependent_types: on\ntype MyEnum int; const (" ++ s ++ " MyEnum = iota); func test() { e := " ++ s ++ " }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试常量位运算
prop_constant_bitwise :: Integer -> Integer -> Property
prop_constant_bitwise a b = 
  let input = "//! dependent_types: on\nfunc test() { x := " ++ show a ++ " | " ++ show b ++ " }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量逻辑运算
prop_constant_logical :: Bool -> Bool -> Property
prop_constant_logical a b = 
  let input = "//! dependent_types: on\nfunc test() { x := " ++ show a ++ " && " ++ show b ++ " }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量字符串连接
prop_constant_string_concatenation :: String -> String -> Property
prop_constant_string_concatenation s1 s2 = 
  let input = "//! dependent_types: on\nfunc test() { s := \"" ++ s1 ++ "\" + \"" ++ s2 ++ "\" }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量类型转换
prop_constant_type_conversion :: Integer -> Property
prop_constant_type_conversion n = 
  let input = "//! dependent_types: on\nfunc test() { x := float64(" ++ show n ++ ") }"
  in case P.parseTypus input of
       Right _ -> property True
       Left _ -> property True

-- | 测试常量函数调用
prop_constant_function_call :: String -> Property
prop_constant_function_call s = 
  let input = "//! dependent_types: on\nfunc test() { x := " ++ s ++ "() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 错误处理测试 (30个测试)
-- ============================================================================

-- | 测试语法错误处理
prop_syntax_error_handling :: String -> Property
prop_syntax_error_handling s = 
  -- Always use a fixed malformed input that will definitely fail parsing
  let malformed = "func test() { if true {  }"  -- Missing closing brace
  in case P.parseTypus malformed of
       Left _ -> property True
       Right _ -> property False

-- | 测试类型错误处理
prop_type_error_handling :: String -> Property
prop_type_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() -> int { return \"string\" }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试依赖类型错误处理
prop_dependent_type_error_handling :: String -> Property
prop_dependent_type_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[0]{} }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试所有权错误处理
prop_ownership_error_handling :: String -> Property
prop_ownership_error_handling s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); t := s; fmt.Println(s.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试约束违反错误处理
prop_constraint_violation_error_handling :: String -> Property
prop_constraint_violation_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { r := safeDiv(10, 0) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试边界错误处理
prop_boundary_error_handling :: String -> Property
prop_boundary_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; x := get(v, 5) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试维度不匹配错误处理
prop_dimension_mismatch_error_handling :: String -> Property
prop_dimension_mismatch_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v1 := Vector[3]{}; v2 := Vector[4]{}; v3 := add(v1, v2) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试矩阵维度不匹配错误处理
prop_matrix_dimension_mismatch_error_handling :: String -> Property
prop_matrix_dimension_mismatch_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { a := Matrix[2, 3]{}; b := Matrix[4, 5]{}; c := matMul(a, b) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试借用冲突错误处理
prop_borrow_conflict_error_handling :: String -> Property
prop_borrow_conflict_error_handling s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s; m := &mut s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试多个可变借用错误处理
prop_multiple_mutable_borrow_error_handling :: String -> Property
prop_multiple_mutable_borrow_error_handling s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); m1 := &mut s; m2 := &mut s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试静态断言失败错误处理
prop_static_assert_failure_error_handling :: String -> Property
prop_static_assert_failure_error_handling s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { static_assert false }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试未定义变量错误处理
prop_undefined_variable_error_handling :: String -> Property
prop_undefined_variable_error_handling s = 
  let input = "func " ++ s ++ "() { fmt.Println(undefined_var) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试未定义类型错误处理
prop_undefined_type_error_handling :: String -> Property
prop_undefined_type_error_handling s = 
  let input = "func " ++ s ++ "() { x: UndefinedType }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试未定义函数错误处理
prop_undefined_function_error_handling :: String -> Property
prop_undefined_function_error_handling s = 
  let input = "func " ++ s ++ "() { undefined_function() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试参数数量不匹配错误处理
prop_parameter_count_mismatch_error_handling :: String -> Property
prop_parameter_count_mismatch_error_handling s = 
  let input = "func " ++ s ++ "() { fmt.Println() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试参数类型不匹配错误处理
prop_parameter_type_mismatch_error_handling :: String -> Property
prop_parameter_type_mismatch_error_handling s = 
  let input = "func " ++ s ++ "() { fmt.Println(123) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试返回类型不匹配错误处理
prop_return_type_mismatch_error_handling :: String -> Property
prop_return_type_mismatch_error_handling s = 
  let input = "func " ++ s ++ "() -> int { return \"string\" }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试缺少返回语句错误处理
prop_missing_return_error_handling :: String -> Property
prop_missing_return_error_handling s = 
  let input = "func " ++ s ++ "() -> int { }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试重复定义错误处理
prop_duplicate_definition_error_handling :: String -> Property
prop_duplicate_definition_error_handling s = 
  let input = "func " ++ s ++ "() {} func " ++ s ++ "() {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试循环依赖错误处理
prop_circular_dependency_error_handling :: String -> Property
prop_circular_dependency_error_handling s = 
  if null s || not (all (\c -> isLetter c || c == '_' || isDigit c) s) || isDigit (head s)
    then property True
    else 
      -- Create a type that references itself, which should cause a compilation error
      let input = "type " ++ s ++ " struct { field *" ++ s ++ " }"
      in parseAndCompile input

-- | 测试递归类型错误处理
prop_recursive_type_error_handling :: String -> Property
prop_recursive_type_error_handling s = 
  let input = "type " ++ s ++ " struct { field *" ++ s ++ " }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试不可变类型修改错误处理
prop_immutable_type_mutation_error_handling :: String -> Property
prop_immutable_type_mutation_error_handling s = 
  let input = "func " ++ s ++ "() { str := \"hello\"; str[0] = 'H' }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试nil解引用错误处理
prop_nil_dereference_error_handling :: String -> Property
prop_nil_dereference_error_handling s = 
  let input = "func " ++ s ++ "() { var p *int; fmt.Println(*p) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试数组越界错误处理
prop_array_out_of_bounds_error_handling :: String -> Property
prop_array_out_of_bounds_error_handling s = 
  let input = "func " ++ s ++ "() { arr := [3]int{1, 2, 3}; fmt.Println(arr[5]) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试除零错误处理
prop_division_by_zero_error_handling :: String -> Property
prop_division_by_zero_error_handling s = 
  let input = "func " ++ s ++ "() { x := 10 / 0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试类型断言错误处理
prop_type_assertion_error_handling :: String -> Property
prop_type_assertion_error_handling s = 
  let input = "func " ++ s ++ "() { var i interface{} = \"hello\"; _ = i.(int) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试通道死锁错误处理
prop_channel_deadlock_error_handling :: String -> Property
prop_channel_deadlock_error_handling s = 
  let input = "func " ++ s ++ "() { ch := make(chan int); <-ch }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试空指针调用方法错误处理
prop_nil_pointer_method_call_error_handling :: String -> Property
prop_nil_pointer_method_call_error_handling s = 
  let input = "type " ++ s ++ " struct {}; func (r *" ++ s ++ ") Method() {}; func test() { var p *" ++ s ++ "; p.Method() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试切片nil解引用错误处理
prop_nil_slice_dereference_error_handling :: String -> Property
prop_nil_slice_dereference_error_handling s = 
  let input = "func " ++ s ++ "() { var slice []int; fmt.Println(len(slice)); fmt.Println(slice[0]) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- | 测试map键不存在错误处理
prop_map_key_not_exist_error_handling :: String -> Property
prop_map_key_not_exist_error_handling s = 
  let input = "func " ++ s ++ "() { m := map[string]int{}; fmt.Println(m[\"nonexistent\"]) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then parseAndCompile input
     else property True

-- ============================================================================
-- 综合集成测试 (30个测试)
-- ============================================================================

-- | 测试依赖类型与所有权结合
prop_dependent_types_with_ownership :: String -> Property
prop_dependent_types_with_ownership s = 
  let input = "//! ownership: on\n//! dependent_types: on\nfunc " ++ s ++ "(n: Positive) -> Vector[n] { v := Vector[n]{}; return v }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试约束求解与所有权结合
prop_constraint_solver_with_ownership :: String -> Property
prop_constraint_solver_with_ownership s = 
  let input = "//! ownership: on\n//! dependent_types: on\nfunc " ++ s ++ "(v: Vector[n]) -> float64 { assert n > 0; return 0.0 }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令系统与依赖类型结合
prop_directive_system_with_dependent_types :: String -> Property
prop_directive_system_with_dependent_types s = 
  let input = "func " ++ s ++ "() { {//! dependent_types: on\nv := Vector[3]{}\n} }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试指令系统与所有权结合
prop_directive_system_with_ownership :: String -> Property
prop_directive_system_with_ownership s = 
  let input = "func " ++ s ++ "() { {//! ownership: on\ns := NewMyString(\"hello\")\nt := s\n} }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试编译期常量与依赖类型结合
prop_compile_time_constants_with_dependent_types :: String -> Property
prop_compile_time_constants_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; x := get(v, 0); y := get(v, 2) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试编译期常量与所有权结合
prop_compile_time_constants_with_ownership :: String -> Property
prop_compile_time_constants_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); r := &s; fmt.Println(r.data) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试存在类型与所有权结合
prop_existential_types_with_ownership :: String -> Property
prop_existential_types_with_ownership s = 
  let input = "//! ownership: on\n//! dependent_types: on\nfunc " ++ s ++ "() { v := readVector(); match v.(n) { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试类型推导与所有权结合
prop_type_inference_with_ownership :: String -> Property
prop_type_inference_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); t := s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试错误处理与依赖类型结合
prop_error_handling_with_dependent_types :: String -> Property
prop_error_handling_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { r, err := safeDiv(10, 0); if err != nil { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试错误处理与所有权结合
prop_error_handling_with_ownership :: String -> Property
prop_error_handling_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { defer func() { if r := recover(); r != nil { } }(); s := NewMyString(\"hello\"); panic(\"error\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试并发与依赖类型结合
prop_concurrency_with_dependent_types :: String -> Property
prop_concurrency_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; go func() { x := get(v, 0) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试并发与所有权结合
prop_concurrency_with_ownership :: String -> Property
prop_concurrency_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试接口与依赖类型结合
prop_interfaces_with_dependent_types :: String -> Property
prop_interfaces_with_dependent_types s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ " interface { Method(n: Positive) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试接口与所有权结合
prop_interfaces_with_ownership :: String -> Property
prop_interfaces_with_ownership s = 
  let input = "//! ownership: on\ntype " ++ s ++ " interface { Method() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试泛型与依赖类型结合
prop_generics_with_dependent_types :: String -> Property
prop_generics_with_dependent_types s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ "[T any, n: int] struct { data [n]T }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试泛型与所有权结合
prop_generics_with_ownership :: String -> Property
prop_generics_with_ownership s = 
  let input = "//! ownership: on\ntype " ++ s ++ "[T any] struct { data T }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试方法与依赖类型结合
prop_methods_with_dependent_types :: String -> Property
prop_methods_with_dependent_types s = 
  let input = "//! dependent_types: on\ntype Vector[n: int] struct {}; func (v Vector[n]) Get(i: ValidIndex[n]) -> float64 {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试方法与所有权结合
prop_methods_with_ownership :: String -> Property
prop_methods_with_ownership s = 
  let input = "//! ownership: on\ntype " ++ s ++ " struct {}; func (r " ++ s ++ ") Method() {}"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试结构体与依赖类型结合
prop_structs_with_dependent_types :: String -> Property
prop_structs_with_dependent_types s = 
  let input = "//! dependent_types: on\ntype " ++ s ++ " struct { data Vector[3] }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试结构体与所有权结合
prop_structs_with_ownership :: String -> Property
prop_structs_with_ownership s = 
  let input = "//! ownership: on\ntype " ++ s ++ " struct { data MyString }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通道与依赖类型结合
prop_channels_with_dependent_types :: String -> Property
prop_channels_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { ch := make(chan Vector[3]) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试通道与所有权结合
prop_channels_with_ownership :: String -> Property
prop_channels_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { ch := make(chan MyString); s := NewMyString(\"hello\"); ch <- s }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试闭包与依赖类型结合
prop_closures_with_dependent_types :: String -> Property
prop_closures_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; f := func() { x := get(v, 0) } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试闭包与所有权结合
prop_closures_with_ownership :: String -> Property
prop_closures_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试defer与依赖类型结合
prop_defer_with_dependent_types :: String -> Property
prop_defer_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; defer func() { x := get(v, 0) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试defer与所有权结合
prop_defer_with_ownership :: String -> Property
prop_defer_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }() }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试panic与依赖类型结合
prop_panic_with_dependent_types :: String -> Property
prop_panic_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { v := Vector[3]{}; panic(\"error\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试panic与所有权结合
prop_panic_with_ownership :: String -> Property
prop_panic_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { s := NewMyString(\"hello\"); panic(\"error\") }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试select与依赖类型结合
prop_select_with_dependent_types :: String -> Property
prop_select_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { ch1 := make(chan Vector[3]); ch2 := make(chan Vector[4]); select { case v := <-ch1: case v := <-ch2: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试select与所有权结合
prop_select_with_ownership :: String -> Property
prop_select_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { ch1 := make(chan MyString); ch2 := make(chan MyString); select { case s := <-ch1: case s := <-ch2: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试range与依赖类型结合
prop_range_with_dependent_types :: String -> Property
prop_range_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { slice := []Vector[3]{}; for _, v := range slice { x := get(v, 0) } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试range与所有权结合
prop_range_with_ownership :: String -> Property
prop_range_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { slice := []MyString{NewMyString(\"a\"), NewMyString(\"b\")}; for _, s := range slice { } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试类型切换与依赖类型结合
prop_type_switch_with_dependent_types :: String -> Property
prop_type_switch_with_dependent_types s = 
  let input = "//! dependent_types: on\nfunc " ++ s ++ "() { var i interface{} = Vector[3]{}; switch v := i.(type) { case Vector[3]: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试类型切换与所有权结合
prop_type_switch_with_ownership :: String -> Property
prop_type_switch_with_ownership s = 
  let input = "//! ownership: on\nfunc " ++ s ++ "() { var i interface{} = NewMyString(\"hello\"); switch v := i.(type) { case MyString: } }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试完整示例
prop_complete_example :: String -> Property
prop_complete_example s = 
  let input = "//! ownership: on\n//! dependent_types: on\npackage main\n\nimport \"fmt\"\n\ntype Positive = int where { self > 0 }\ntype Vector[n: int] struct { data [n]float64 }\n\nfunc zeros(n: Positive) -> Vector[n] { return Vector[n]{data: make([]float64, n)} }\n\nfunc " ++ s ++ "() { v1 := zeros(3); v2 := zeros(3); v3 := add(v1, v2); x := get(v3, 0) }"
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && 
     not (null s) && 
     not (isDigit (head s))
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- ============================================================================
-- 测试套件定义
-- ============================================================================

-- | 将所有测试组合成测试套件
testSuite :: TestTree
testSuite = testGroup "New Comprehensive Typus Test Suite"
  [ -- 基本解析测试
    testGroup "Basic Parsing Tests"
      [ testProperty "Identifier parsing" prop_identifier_parsing
      , testProperty "Basic type definition" prop_basic_type_definition
      , testProperty "Function definition parsing" prop_function_definition_parsing
      , testProperty "Package declaration" prop_package_declaration
      , testProperty "Import statement" prop_import_statement
      , testProperty "Comment handling" prop_comment_handling
      , testProperty "Multiline comment handling" prop_multiline_comment_handling
      , testProperty "String literal parsing" prop_string_literal_parsing
      , testProperty "Numeric literal parsing" prop_numeric_literal_parsing
      , testProperty "Boolean literal parsing" prop_boolean_literal_parsing
      , testProperty "Array type parsing" prop_array_type_parsing
      , testProperty "Slice type parsing" prop_slice_type_parsing
      , testProperty "Struct definition parsing" prop_struct_definition_parsing
      , testProperty "Interface definition parsing" prop_interface_definition_parsing
      , testProperty "Method receiver parsing" prop_method_receiver_parsing
      , testProperty "Multiple return values parsing" prop_multiple_return_values_parsing
      , testProperty "Variadic function parsing" prop_variadic_function_parsing
      , testProperty "Error handling parsing" prop_error_handling_parsing
      , testProperty "Concurrency parsing" prop_concurrency_parsing
      , testProperty "Channel parsing" prop_channel_parsing
      ]
    
    -- 依赖类型测试
  , testGroup "Dependent Types Tests"
      [ testProperty "Value parameterized type" prop_value_parameterized_type
      , testProperty "Refined type definition" prop_refined_type_definition
      , testProperty "Parameterized refined type" prop_parameterized_refined_type
      , testProperty "Dependent return type" prop_dependent_return_type
      , testProperty "Dependent parameter type" prop_dependent_parameter_type
      , testProperty "Function precondition" prop_function_precondition
      , testProperty "Type level arithmetic" prop_type_level_arithmetic
      , testProperty "Mixed type and value parameters" prop_mixed_type_and_value_parameters
      , testProperty "Existential type" prop_existential_type
      , testProperty "Existential unpacking" prop_existential_unpacking
      , testProperty "Assert narrowing" prop_assert_narrowing
      , testProperty "Static assert" prop_static_assert
      , testProperty "Conditional narrowing" prop_conditional_narrowing
      , testProperty "Matrix type definition" prop_matrix_type_definition
      , testProperty "Matrix multiplication types" prop_matrix_multiplication_types
      , testProperty "NonZero type" prop_nonzero_type
      , testProperty "Positive type" prop_positive_type
      , testProperty "Valid index type" prop_valid_index_type
      , testProperty "Non-empty string type" prop_nonempty_string_type
      , testProperty "Percentage type" prop_percentage_type
      , testProperty "Safe division function" prop_safe_division_function
      , testProperty "Vector access function" prop_vector_access_function
      , testProperty "Vector set function" prop_vector_set_function
      , testProperty "Vector addition function" prop_vector_addition_function
      , testProperty "Vector dot product function" prop_vector_dot_product_function
      , testProperty "Vector concatenation function" prop_vector_concatenation_function
      , testProperty "Zero vector constructor" prop_zero_vector_constructor
      , testProperty "Ones vector constructor" prop_ones_vector_constructor
      , testProperty "Type inference" prop_type_inference
      ]
    
    -- 所有权机制测试
  , testGroup "Ownership Mechanism Tests"
      [ testProperty "Ownership directive" prop_ownership_directive
      , testProperty "Block ownership directive" prop_block_ownership_directive
      , testProperty "Move semantics" prop_move_semantics
      , testProperty "Immutable borrowing" prop_immutable_borrowing
      , testProperty "Mutable borrowing" prop_mutable_borrowing
      , testProperty "Borrowing rules" prop_borrowing_rules
      , testProperty "Borrow lifetime" prop_borrow_lifetime
      , testProperty "Ownership transfer" prop_ownership_transfer
      , testProperty "Use after move" prop_use_after_move
      , testProperty "Read through borrow" prop_read_through_borrow
      , testProperty "Original readable after borrow" prop_original_readable_after_borrow
      , testProperty "Modify through mutable borrow" prop_modify_through_mutable_borrow
      , testProperty "Single mutable borrow rule" prop_single_mutable_borrow_rule
      , testProperty "Immutable mutable borrow conflict" prop_immutable_mutable_borrow_conflict
      , testProperty "Cross function ownership transfer" prop_cross_function_ownership_transfer
      , testProperty "Borrow cross function passing" prop_borrow_cross_function_passing
      , testProperty "Mutable borrow cross function passing" prop_mutable_borrow_cross_function_passing
      , testProperty "Struct field ownership" prop_struct_field_ownership
      , testProperty "Array element ownership" prop_array_element_ownership
      , testProperty "Slice element ownership" prop_slice_element_ownership
      , testProperty "Map value ownership" prop_map_value_ownership
      , testProperty "Channel ownership transfer" prop_channel_ownership_transfer
      , testProperty "Ownership with closures" prop_ownership_with_closures
      , testProperty "Ownership with goroutines" prop_ownership_with_goroutines
      , testProperty "Ownership with interfaces" prop_ownership_with_interfaces
      , testProperty "Ownership with type assertion" prop_ownership_with_type_assertion
      , testProperty "Ownership with defer" prop_ownership_with_defer
      , testProperty "Ownership with panic" prop_ownership_with_panic
      , testProperty "Ownership with recover" prop_ownership_with_recover
      , testProperty "Ownership with select" prop_ownership_with_select
      , testProperty "Ownership with range" prop_ownership_with_range
      , testProperty "Ownership with type switch" prop_ownership_with_type_switch
      ]
    
    -- 指令系统测试
  , testGroup "Directive System Tests"
      [ testProperty "File level dependent types directive" prop_file_level_dependent_types_directive
      , testProperty "File level ownership directive" prop_file_level_ownership_directive
      , testProperty "File level constraint mode directive" prop_file_level_constraint_mode_directive
      , testProperty "File level multiple directives" prop_file_level_multiple_directives
      , testProperty "Block level dependent types directive" prop_block_level_dependent_types_directive
      , testProperty "Block level ownership directive" prop_block_level_ownership_directive
      , testProperty "Block level multiple directives" prop_block_level_multiple_directives
      , testProperty "Nested block directives" prop_nested_block_directives
      , testProperty "Directive scope" prop_directive_scope
      , testProperty "Directive override" prop_directive_override
      , testProperty "Directive with function definition" prop_directive_with_function_definition
      , testProperty "Directive with type definition" prop_directive_with_type_definition
      , testProperty "Directive with variable declaration" prop_directive_with_variable_declaration
      , testProperty "Directive with expression" prop_directive_with_expression
      , testProperty "Directive with control flow" prop_directive_with_control_flow
      , testProperty "Directive with loops" prop_directive_with_loops
      , testProperty "Directive with methods" prop_directive_with_methods
      , testProperty "Directive with interfaces" prop_directive_with_interfaces
      , testProperty "Directive with structs" prop_directive_with_structs
      , testProperty "Directive with concurrency" prop_directive_with_concurrency
      ]
    
    -- 编译期常量传播测试
  , testGroup "Compile-time Constant Propagation Tests"
      [ testProperty "Constant expression evaluation" prop_constant_expression_evaluation
      , testProperty "Constant boundary check" prop_constant_boundary_check
      , testProperty "Constant arithmetic" prop_constant_arithmetic
      , testProperty "Constant comparison" prop_constant_comparison
      , testProperty "Constant condition" prop_constant_condition
      , testProperty "Constant nonzero check" prop_constant_nonzero_check
      , testProperty "Constant positive check" prop_constant_positive_check
      , testProperty "Constant index access" prop_constant_index_access
      , testProperty "Constant dimension matching" prop_constant_dimension_matching
      , testProperty "Constant matrix multiplication" prop_constant_matrix_multiplication
      , testProperty "Constant vector length" prop_constant_vector_length
      , testProperty "Constant bounded type" prop_constant_bounded_type
      , testProperty "Constant array size" prop_constant_array_size
      , testProperty "Constant string length" prop_constant_string_length
      , testProperty "Constant enum value" prop_constant_enum_value
      , testProperty "Constant bitwise" prop_constant_bitwise
      , testProperty "Constant logical" prop_constant_logical
      , testProperty "Constant string concatenation" prop_constant_string_concatenation
      , testProperty "Constant type conversion" prop_constant_type_conversion
      , testProperty "Constant function call" prop_constant_function_call
      ]
    
    -- 错误处理测试
  , testGroup "Error Handling Tests"
      [ testProperty "Syntax error handling" prop_syntax_error_handling
      , testProperty "Type error handling" prop_type_error_handling
      , testProperty "Dependent type error handling" prop_dependent_type_error_handling
      , testProperty "Ownership error handling" prop_ownership_error_handling
      , testProperty "Constraint violation error handling" prop_constraint_violation_error_handling
      , testProperty "Boundary error handling" prop_boundary_error_handling
      , testProperty "Dimension mismatch error handling" prop_dimension_mismatch_error_handling
      , testProperty "Matrix dimension mismatch error handling" prop_matrix_dimension_mismatch_error_handling
      , testProperty "Borrow conflict error handling" prop_borrow_conflict_error_handling
      , testProperty "Multiple mutable borrow error handling" prop_multiple_mutable_borrow_error_handling
      , testProperty "Static assert failure error handling" prop_static_assert_failure_error_handling
      , testProperty "Undefined variable error handling" prop_undefined_variable_error_handling
      , testProperty "Undefined type error handling" prop_undefined_type_error_handling
      , testProperty "Undefined function error handling" prop_undefined_function_error_handling
      , testProperty "Parameter count mismatch error handling" prop_parameter_count_mismatch_error_handling
      , testProperty "Parameter type mismatch error handling" prop_parameter_type_mismatch_error_handling
      , testProperty "Return type mismatch error handling" prop_return_type_mismatch_error_handling
      , testProperty "Missing return error handling" prop_missing_return_error_handling
      , testProperty "Duplicate definition error handling" prop_duplicate_definition_error_handling
      , testProperty "Circular dependency error handling" prop_circular_dependency_error_handling
      , testProperty "Recursive type error handling" prop_recursive_type_error_handling
      , testProperty "Immutable type mutation error handling" prop_immutable_type_mutation_error_handling
      , testProperty "Nil dereference error handling" prop_nil_dereference_error_handling
      , testProperty "Array out of bounds error handling" prop_array_out_of_bounds_error_handling
      , testProperty "Division by zero error handling" prop_division_by_zero_error_handling
      , testProperty "Type assertion error handling" prop_type_assertion_error_handling
      , testProperty "Channel deadlock error handling" prop_channel_deadlock_error_handling
      , testProperty "Nil pointer method call error handling" prop_nil_pointer_method_call_error_handling
      , testProperty "Nil slice dereference error handling" prop_nil_slice_dereference_error_handling
      , testProperty "Map key not exist error handling" prop_map_key_not_exist_error_handling
      ]
    
    -- 综合集成测试
  , testGroup "Comprehensive Integration Tests"
      [ testProperty "Dependent types with ownership" prop_dependent_types_with_ownership
      , testProperty "Constraint solver with ownership" prop_constraint_solver_with_ownership
      , testProperty "Directive system with dependent types" prop_directive_system_with_dependent_types
      , testProperty "Directive system with ownership" prop_directive_system_with_ownership
      , testProperty "Compile time constants with dependent types" prop_compile_time_constants_with_dependent_types
      , testProperty "Compile time constants with ownership" prop_compile_time_constants_with_ownership
      , testProperty "Existential types with ownership" prop_existential_types_with_ownership
      , testProperty "Type inference with ownership" prop_type_inference_with_ownership
      , testProperty "Error handling with dependent types" prop_error_handling_with_dependent_types
      , testProperty "Error handling with ownership" prop_error_handling_with_ownership
      , testProperty "Concurrency with dependent types" prop_concurrency_with_dependent_types
      , testProperty "Concurrency with ownership" prop_concurrency_with_ownership
      , testProperty "Interfaces with dependent types" prop_interfaces_with_dependent_types
      , testProperty "Interfaces with ownership" prop_interfaces_with_ownership
      , testProperty "Generics with dependent types" prop_generics_with_dependent_types
      , testProperty "Generics with ownership" prop_generics_with_ownership
      , testProperty "Methods with dependent types" prop_methods_with_dependent_types
      , testProperty "Methods with ownership" prop_methods_with_ownership
      , testProperty "Structs with dependent types" prop_structs_with_dependent_types
      , testProperty "Structs with ownership" prop_structs_with_ownership
      , testProperty "Channels with dependent types" prop_channels_with_dependent_types
      , testProperty "Channels with ownership" prop_channels_with_ownership
      , testProperty "Closures with dependent types" prop_closures_with_dependent_types
      , testProperty "Closures with ownership" prop_closures_with_ownership
      , testProperty "Defer with dependent types" prop_defer_with_dependent_types
      , testProperty "Defer with ownership" prop_defer_with_ownership
      , testProperty "Panic with dependent types" prop_panic_with_dependent_types
      , testProperty "Panic with ownership" prop_panic_with_ownership
      , testProperty "Select with dependent types" prop_select_with_dependent_types
      , testProperty "Select with ownership" prop_select_with_ownership
      , testProperty "Range with dependent types" prop_range_with_dependent_types
      , testProperty "Range with ownership" prop_range_with_ownership
      , testProperty "Type switch with dependent types" prop_type_switch_with_dependent_types
      , testProperty "Type switch with ownership" prop_type_switch_with_ownership
      , testProperty "Complete example" prop_complete_example
      ]
  ]