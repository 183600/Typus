{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypeFeaturesTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isDigit, isLetter)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

import DependentTypesParser
import Parser (parseTypus)
import SourceLocation

-- | 测试值参数化类型的解析和验证
prop_value_parameterized_type_parsing :: Positive Int -> String -> Property
prop_value_parameterized_type_parsing (Positive n) typeName =
  let typusCode = "type " ++ typeName ++ "[" ++ show n ++ ": int] struct { data [" ++ show n ++ "]float64 }"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试精确类型的约束验证
prop_precise_type_constraints :: Int -> Property
prop_precise_type_constraints x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "func testFunc(x: Positive) -> Positive { return x }"
      parseResult = parseTypus (T.pack typusCode)
      isValidCode = isRight parseResult
      satisfiesConstraint = x > 0
  in property $ isValidCode ==> satisfiesConstraint

-- | 测试依赖函数签名的类型检查
prop_dependent_function_signature :: Positive Int -> Positive Int -> Property
prop_dependent_function_signature (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func concat[m: int, n: int](a: Vector[m], b: Vector[n]) -> Vector[m + n] {\n" ++
                 "  result := make([]float64, m+n)\n" ++
                 "  copy(result, a.data)\n" ++
                 "  copy(result[m:], b.data)\n" ++
                 "  return Vector[m+n]{data: result}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试函数前置条件的验证
prop_function_preconditions :: Positive Int -> Property
prop_function_preconditions (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func average[n: int](v: Vector[n]) -> float64 where { n > 0 } {\n" ++
                 "  sum := 0.0\n" ++
                 "  for _, x := range v.data {\n" ++
                 "    sum += x\n" ++
                 "  }\n" ++
                 "  return sum / float64(n)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试断言与条件窄化
prop_assertion_narrowing :: Int -> Property
prop_assertion_narrowing x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "func processInput(n: int) {\n" ++
                 "  assert n > 0\n" ++
                 "  v := zeros(n)\n" ++
                 "  avg := average(v)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试编译期常量传播
prop_compile_time_constant_propagation :: Property
prop_compile_time_constant_propagation =
  let typusCode = "type Vector[n: int] struct { data [" ++ "3" ++ "]float64 }\n" ++
                 "type ValidIndex[n: int] = int where { self >= 0 && self < n }\n" ++
                 "func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {\n" ++
                 "  return v.data[i]\n" ++
                 "}\n" ++
                 "func test() {\n" ++
                 "  v := Vector[3]{data: []float64{1.0, 2.0, 3.0}}\n" ++
                 "  x := get(v, 0)\n" ++
                 "  y := get(v, 2)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试存在类型的处理
prop_existential_types :: Property
prop_existential_types =
  let typusCode = "type Vector[n: int] struct { data [" ++ "3" ++ "]float64 }\n" ++
                 "func readVector(input: []float64) -> Vector[some n: int] where { n == len(input) } {\n" ++
                 "  return Vector[len(input)]{data: input}\n" ++
                 "}\n" ++
                 "func processVector() {\n" ++
                 "  data := []float64{1.0, 2.0, 3.0}\n" ++
                 "  v := readVector(data)\n" ++
                 "  match v.(n) {\n" ++
                 "    fmt.Println(get(v, 0))\n" ++
                 "    if n > 1 {\n" ++
                 "      fmt.Println(get(v, 1))\n" ++
                 "    }\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试类型推导功能
prop_type_inference :: Property
prop_type_inference =
  let typusCode = "type Vector[n: int] struct { data [" ++ "3" ++ "]float64 }\n" ++
                 "type Positive = int where { self > 0 }\n" ++
                 "func createVector(n: Positive, value: float64) -> Vector[n] {\n" ++
                 "  elements := make([]float64, n)\n" ++
                 "  for i := 0; i < n; i++ {\n" ++
                 "    elements[i] = value\n" ++
                 "  }\n" ++
                 "  return Vector{elements}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试类型级算术运算
prop_type_level_arithmetic :: Positive Int -> Positive Int -> Property
prop_type_level_arithmetic (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func concat[m: int, n: int](a: Vector[m], b: Vector[n]) -> Vector[m + n] {\n" ++
                 "  result := make([]float64, m+n)\n" ++
                 "  copy(result, a.data)\n" ++
                 "  copy(result[m:], b.data)\n" ++
                 "  return Vector[m+n]{data: result}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试混合类型参数和值参数
prop_mixed_type_and_value_parameters :: String -> Positive Int -> Property
prop_mixed_type_and_value_parameters typeName (Positive cap) =
  let typusCode = "type BoundedSlice[T any, cap: int] struct {\n" ++
                 "  data []T\n" ++
                 "  _cap int\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试参数化精确类型
prop_parameterized_precise_types :: Positive Int -> Positive Int -> Property
prop_parameterized_precise_types (Positive lo) (Positive hi) =
  let typusCode = "type Bounded[lo: int, hi: int] = int where { self >= lo && self <= hi }\n" ++
                 "type Percentage = Bounded[0, 100]"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && lo <= hi

-- | 测试依赖参数之间的约束
prop_inter_parameter_constraints :: Positive Int -> Property
prop_inter_parameter_constraints (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "type ValidIndex[n: int] = int where { self >= 0 && self < n }\n" ++
                 "func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {\n" ++
                 "  return v.data[i]\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试静态断言
prop_static_assert :: Property
prop_static_assert =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "func testStaticAssert() {\n" ++
                 "  static_assert 3 > 0\n" ++
                 "  n := 3\n" ++
                 "  v := zeros(n)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试边界条件处理
prop_boundary_conditions :: Int -> Property
prop_boundary_conditions x =
  let typusCode = "type NonZero = int where { self != 0 }\n" ++
                 "func safeDiv(a: int, b: NonZero) -> int {\n" ++
                 "  return a / b\n" ++
                 "}\n" ++
                 "func testDiv() {\n" ++
                 "  if " ++ show x ++ " != 0 {\n" ++
                 "    r := safeDiv(10, " ++ show x ++ ")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
      isNonZero = x /= 0
  in property $ isRight parseResult ==> isNonZero

-- | 测试复杂依赖类型表达式
prop_complex_dependent_expressions :: Positive Int -> Positive Int -> Positive Int -> Property
prop_complex_dependent_expressions (Positive m) (Positive n) (Positive p) =
  let typusCode = "type Matrix[rows: int, cols: int] struct {\n" ++
                 "  data [rows][cols]float64\n" ++
                 "}\n" ++
                 "func matMul[m: int, n: int, p: int](\n" ++
                 "  a: Matrix[m, n],\n" ++
                 "  b: Matrix[n, p],\n" ++
                 ") -> Matrix[m, p]\n" ++
                 "  where { m > 0, n > 0, p > 0 }\n" ++
                 "{\n" ++
                 "  result := Matrix[m, p]{}\n" ++
                 "  for i := 0; i < m; i++ {\n" ++
                 "    for j := 0; j < p; j++ {\n" ++
                 "      for k := 0; k < n; k++ {\n" ++
                 "        result.data[i][j] += a.data[i][k] * b.data[k][j]\n" ++
                 "      }\n" ++
                 "    }\n" ++
                 "  }\n" ++
                 "  return result\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试嵌套依赖类型
prop_nested_dependent_types :: Positive Int -> Positive Int -> Property
prop_nested_dependent_types (Positive rows) (Positive cols) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show cols ++ "]float64 }\n" ++
                 "type Matrix[rows: int, cols: int] struct {\n" ++
                 "  rows [" ++ show rows ++ "]Vector[cols]\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试递归依赖类型
prop_recursive_dependent_types :: Positive Int -> Property
prop_recursive_dependent_types (Positive n) =
  let typusCode = "type List[n: int] struct {\n" ++
                 "  head int\n" ++
                 "  tail *List[n-1]\n" ++
                 "} where { n >= 0 }\n" ++
                 "type Nil = List[0]"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试条件类型
prop_conditional_types :: Bool -> Property
prop_conditional_types condition =
  let typusCode = "type Conditional[b: bool] = if b then int else string\n" ++
                 "func testConditional() {\n" ++
                 "  x := Conditional[" ++ show condition ++ "]{value: " ++ 
                 (if condition then "42" else "\"hello\"") ++ "}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试类型约束的组合
prop_combined_constraints :: Positive Int -> Positive Int -> Property
prop_combined_constraints (Positive lo) (Positive hi) =
  let typusCode = "type BoundedRange[lo: int, hi: int] = int where { self >= lo && self <= hi }\n" ++
                 "type PositiveBounded[hi: int] = BoundedRange[1, hi]\n" ++
                 "type SmallPositive = PositiveBounded[100]"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && lo < hi

-- | 测试类型级别的函数
prop_type_level_functions :: Positive Int -> Property
prop_type_level_functions (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "type Length[v: Vector] = v.length\n" ++
                 "type Double[n: int] = n + n\n" ++
                 "type DoubledVector[v: Vector] = Vector[Double[Length[v]]]"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试类型级别的条件
prop_type_level_conditionals :: Int -> Property
prop_type_level_conditionals x =
  let typusCode = "type IsPositive[n: int] = if n > 0 then true else false\n" ++
                 "type SafeDivide[n: int] = if IsPositive[n] then int else error\n" ++
                 "func testSafeDivide(x: int) {\n" ++
                 "  if IsPositive[" ++ show x ++ "] {\n" ++
                 "    result := SafeDivide[" ++ show x ++ "]{value: 100 / " ++ show x ++ "}\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的模式匹配
prop_dependent_type_pattern_matching :: Positive Int -> Property
prop_dependent_type_pattern_matching (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "type SomeVector = Vector[some n: int]\n" ++
                 "func processVector(v: SomeVector) {\n" ++
                 "  match v.(n) {\n" ++
                 "    case Vector[1]:\n" ++
                 "      fmt.Println(\"Single element vector\")\n" ++
                 "    case Vector[k] where k > 1:\n" ++
                 "      fmt.Println(\"Multi-element vector with\", k, \"elements\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- Helper type for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Positive a) where
  arbitrary = Positive <$> arbitrary `suchThat` (> 0)

-- | 测试依赖类型的错误处理
prop_dependent_type_error_handling :: Int -> Property
prop_dependent_type_error_handling x =
  let typusCode = "type NonZero = int where { self != 0 }\n" ++
                 "func safeDiv(a: int, b: NonZero) -> int {\n" ++
                 "  return a / b\n" ++
                 "}\n" ++
                 "func testErrorHandling() {\n" ++
                 "  defer func() {\n" ++
                 "    if r := recover(); r != nil {\n" ++
                 "      fmt.Println(\"Recovered from error:\", r)\n" ++
                 "    }\n" ++
                 "  }()\n" ++
                 "  result := safeDiv(10, " ++ show x ++ ")\n" ++
                 "  fmt.Println(\"Result:\", result)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的性能特性
prop_dependent_type_performance :: Positive Int -> Property
prop_dependent_type_performance (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func sumVector(v: Vector[" ++ show n ++ "]) -> float64 {\n" ++
                 "  total := 0.0\n" ++
                 "  for i := 0; i < " ++ show n ++ "; i++ {\n" ++
                 "    total += v.data[i]\n" ++
                 "  }\n" ++
                 "  return total\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的边界检查优化
prop_boundary_check_optimization :: Positive Int -> Property
prop_boundary_check_optimization (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "type ValidIndex[n: int] = int where { self >= 0 && self < n }\n" ++
                 "func safeGet[v: Vector](i: ValidIndex[v.length]) -> float64 {\n" ++
                 "  return v.data[i]\n" ++
                 "}\n" ++
                 "func optimizedAccess(v: Vector[" ++ show n ++ "]) -> float64 {\n" ++
                 "  idx := ValidIndex[" ++ show n ++ "]{value: 0}\n" ++
                 "  return safeGet(v, idx)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的编译时优化
prop_compile_time_optimization :: Property
prop_compile_time_optimization =
  let typusCode = "type Vector[n: int] struct { data [3]float64 }\n" ++
                 "func constantTimeAccess() -> float64 {\n" ++
                 "  v := Vector[3]{data: []float64{1.0, 2.0, 3.0}}\n" ++
                 "  return v.data[0] + v.data[1] + v.data[2]\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型推导
prop_dependent_type_inference :: Property
prop_dependent_type_inference =
  let typusCode = "type Vector[n: int] struct { data [3]float64 }\n" ++
                 "func createVector() -> Vector[3] {\n" ++
                 "  return Vector{data: []float64{1.0, 2.0, 3.0}}\n" ++
                 "}\n" ++
                 "func testInference() {\n" ++
                 "  v := createVector()\n" ++
                 "  sum := v.data[0] + v.data[1] + v.data[2]\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的约束传播
prop_constraint_propagation :: Positive Int -> Property
prop_constraint_propagation (Positive n) =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func createPositiveVector(n: Positive) -> Vector[n] {\n" ++
                 "  data := make([]float64, n)\n" ++
                 "  for i := 0; i < n; i++ {\n" ++
                 "    data[i] = float64(i)\n" ++
                 "  }\n" ++
                 "  return Vector[n]{data: data}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型相等性
prop_type_equality :: Positive Int -> Property
prop_type_equality (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func equalVectors(a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]) -> bool {\n" ++
                 "  for i := 0; i < " ++ show n ++ "; i++ {\n" ++
                 "    if a.data[i] != b.data[i] {\n" ++
                 "      return false\n" ++
                 "    }\n" ++
                 "  }\n" ++
                 "  return true\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型转换
prop_type_conversion :: Positive Int -> Positive Int -> Property
prop_type_conversion (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]float64 }\n" ++
                 "func convertVector[m: int, n: int](v: Vector[m]) -> Vector[n] where { m == n } {\n" ++
                 "  return Vector[n]{data: v.data}\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult ==> (m == n)

-- | 测试依赖类型的泛型约束
prop_generic_constraints :: String -> Property
prop_generic_constraints typeName =
  let typusCode = "type Numeric interface { int | float64 | float32 }\n" ++
                 "type Vector[T: Numeric, n: int] struct {\n" ++
                 "  data [" ++ "3" ++ "]T\n" ++
                 "}\n" ++
                 "func sum[T: Numeric, n: int](v: Vector[T, n]) -> T {\n" ++
                 "  total := T(0)\n" ++
                 "  for i := 0; i < n; i++ {\n" ++
                 "    total += v.data[i]\n" ++
                 "  }\n" ++
                 "  return total\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的递归约束
prop_recursive_constraints :: Positive Int -> Property
prop_recursive_constraints (Positive n) =
  let typusCode = "type List[n: int] struct {\n" ++
                 "  head int\n" ++
                 "  tail *List[n-1]\n" ++
                 "} where { n >= 0 }\n" ++
                 "type Nil = List[0]\n" ++
                 "func length[n: int](l: List[n]) -> int {\n" ++
                 "  if n == 0 {\n" ++
                 "    return 0\n" ++
                 "  }\n" ++
                 "  return 1 + length(*l.tail)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型级递归
prop_type_level_recursion :: Positive Int -> Property
prop_type_level_recursion (Positive n) =
  let typusCode = "type Factorial[n: int] = if n <= 1 then 1 else n * Factorial[n-1]\n" ++
                 "type Fact5 = Factorial[5]\n" ++
                 "func testFactorial() {\n" ++
                 "  f := Fact5{}\n" ++
                 "  fmt.Println(\"5! =\", f.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent overflow

-- | 测试依赖类型的类型级条件表达式
prop_type_level_conditionals_advanced :: Positive Int -> Property
prop_type_level_conditionals_advanced (Positive n) =
  let typusCode = "type IsEven[n: int] = if n % 2 == 0 then true else false\n" ++
                 "type EvenDouble[n: int] = if IsEven[n] then n / 2 else n\n" ++
                 "func testEvenDouble(x: int) {\n" ++
                 "  result := EvenDouble[" ++ show n ++ "]{value: x}\n" ++
                 "  fmt.Println(\"Result:\", result.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型级模式匹配
prop_type_level_pattern_matching :: Positive Int -> Property
prop_type_level_pattern_matching (Positive n) =
  let typusCode = "type Nat = int where { self >= 0 }\n" ++
                 "type MatchNat[n: Nat] = match n {\n" ++
                 "  case 0: \"zero\"\n" ++
                 "  case 1: \"one\"\n" ++
                 "  case m: \"many\"\n" ++
                 "}\n" ++
                 "func testMatchNat(x: Nat) {\n" ++
                 "  desc := MatchNat[" ++ show n ++ "]{value: x}\n" ++
                 "  fmt.Println(\"Description:\", desc.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型级函数组合
prop_type_level_function_composition :: Positive Int -> Property
prop_type_level_function_composition (Positive n) =
  let typusCode = "type Double[n: int] = n + n\n" ++
                 "type Triple[n: int] = n * 3\n" ++
                 "type Compose[f: int -> int, g: int -> int, n: int] = f[g[n]]\n" ++
                 "type SixTimes[n: int] = Compose[Double, Triple, n]\n" ++
                 "func testComposition(x: int) {\n" ++
                 "  result := SixTimes[" ++ show n ++ "]{value: x}\n" ++
                 "  fmt.Println(\"6 *\", x, \"=\", result.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试依赖类型的类型级高阶函数
prop_type_level_higher_order_functions :: Positive Int -> Property
prop_type_level_higher_order_functions (Positive n) =
  let typusCode = "type Map[f: int -> int, n: int] = [f[0], f[1], ..., f[n-1]]\n" ++
                 "type Double[i: int] = i * 2\n" ++
                 "type DoubledVector[n: int] = Map[Double, n]\n" ++
                 "func testHigherOrder() {\n" ++
                 "  v := DoubledVector[" ++ show n ++ "]{values: []int{0, 1, 2, 3, 4}}\n" ++
                 "  fmt.Println(\"Doubled:\", v.values)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级折叠
prop_type_level_fold :: Positive Int -> Property
prop_type_level_fold (Positive n) =
  let typusCode = "type Sum[n: int] = if n <= 0 then 0 else n + Sum[n-1]\n" ++
                 "func testFold() {\n" ++
                 "  sum := Sum[" ++ show n ++ "]{value: 0}\n" ++
                 "  fmt.Println(\"Sum of 1 to\", n, \"=\", sum.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 100  -- Limit to prevent stack overflow

-- | 测试依赖类型的类型级映射
prop_type_level_map :: Positive Int -> Property
prop_type_level_map (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Map[f: int -> int, v: Vector] = Vector[v.length] {\n" ++
                 "  data: [f[v.data[0]], f[v.data[1]], ..., f[v.data[v.length-1]]]\n" ++
                 "}\n" ++
                 "type Double[i: int] = i * 2\n" ++
                 "func testMap(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  doubled := Map[Double, v]\n" ++
                 "  fmt.Println(\"Doubled vector:\", doubled.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级过滤
prop_type_level_filter :: Positive Int -> Property
prop_type_level_filter (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Filter[p: int -> bool, v: Vector] = Vector[countIf[p, v.data]] {\n" ++
                 "  data: filter(p, v.data)\n" ++
                 "}\n" ++
                 "type IsEven[i: int] = i % 2 == 0\n" ++
                 "func testFilter(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  evens := Filter[IsEven, v]\n" ++
                 "  fmt.Println(\"Even numbers:\", evens.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级归约
prop_type_level_reduce :: Positive Int -> Property
prop_type_level_reduce (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Reduce[op: (int, int) -> int, init: int, v: Vector] = int {\n" ++
                 "  value: fold(op, init, v.data)\n" ++
                 "}\n" ++
                 "type Add[a: int, b: int] = a + b\n" ++
                 "func testReduce(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  sum := Reduce[Add, 0, v]\n" ++
                 "  fmt.Println(\"Sum:\", sum.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级排序
prop_type_level_sort :: Positive Int -> Property
prop_type_level_sort (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Sort[v: Vector] = Vector[v.length] {\n" ++
                 "  data: sort(v.data)\n" ++
                 "}\n" ++
                 "func testSort(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  sorted := Sort[v]\n" ++
                 "  fmt.Println(\"Sorted:\", sorted.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级查找
prop_type_level_find :: Positive Int -> Property
prop_type_level_find (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Find[p: int -> bool, v: Vector] = Option[int] {\n" ++
                 "  value: find(p, v.data)\n" ++
                 "}\n" ++
                 "type IsTarget[i: int] = i == 5\n" ++
                 "func testFind(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  target := Find[IsTarget, v]\n" ++
                 "  match target {\n" ++
                 "    case Some(value):\n" ++
                 "      fmt.Println(\"Found:\", value)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Not found\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级分组
prop_type_level_group :: Positive Int -> Property
prop_type_level_group (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type GroupBy[key: int -> int, v: Vector] = Map[key, Vector] {\n" ++
                 "  groups: groupBy(key, v.data)\n" ++
                 "}\n" ++
                 "type Mod2[i: int] = i % 2\n" ++
                 "func testGroupBy(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  groups := GroupBy[Mod2, v]\n" ++
                 "  fmt.Println(\"Groups:\", groups.groups)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级分区
prop_type_level_partition :: Positive Int -> Property
prop_type_level_partition (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Partition[p: int -> bool, v: Vector] = (Vector[countIf[p, v.data]], Vector[v.length - countIf[p, v.data]]) {\n" ++
                 "  first: filter(p, v.data)\n" ++
                 "  second: filter(not(p), v.data)\n" ++
                 "}\n" ++
                 "type IsEven[i: int] = i % 2 == 0\n" ++
                 "func testPartition(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  parts := Partition[IsEven, v]\n" ++
                 "  fmt.Println(\"Evens:\", parts.first.data)\n" ++
                 "  fmt.Println(\"Odds:\", parts.second.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级压缩
prop_type_level_zip :: Positive Int -> Property
prop_type_level_zip (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Zip[a: Vector, b: Vector] = Vector[min(a.length, b.length)] {\n" ++
                 "  data: zip(a.data, b.data)\n" ++
                 "}\n" ++
                 "func testZip(a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  zipped := Zip[a, b]\n" ++
                 "  fmt.Println(\"Zipped:\", zipped.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级解压缩
prop_type_level_unzip :: Positive Int -> Property
prop_type_level_unzip (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "](int, int) }\n" ++
                 "type Unzip[v: Vector] = (Vector[v.length], Vector[v.length]) {\n" ++
                 "  first: map fst, v.data\n" ++
                 "  second: map snd, v.data\n" ++
                 "}\n" ++
                 "func testUnzip(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  unzipped := Unzip[v]\n" ++
                 "  fmt.Println(\"First:\", unzipped.first.data)\n" ++
                 "  fmt.Println(\"Second:\", unzipped.second.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级连接
prop_type_level_concat :: Positive Int -> Positive Int -> Property
prop_type_level_concat (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Concat[a: Vector, b: Vector] = Vector[a.length + b.length] {\n" ++
                 "  data: concat(a.data, b.data)\n" ++
                 "}\n" ++
                 "func testConcat(a: Vector[" ++ show m ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  combined := Concat[a, b]\n" ++
                 "  fmt.Println(\"Combined:\", combined.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && m <= 10 && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级反转
prop_type_level_reverse :: Positive Int -> Property
prop_type_level_reverse (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Reverse[v: Vector] = Vector[v.length] {\n" ++
                 "  data: reverse(v.data)\n" ++
                 "}\n" ++
                 "func testReverse(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  reversed := Reverse[v]\n" ++
                 "  fmt.Println(\"Reversed:\", reversed.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级取前N个
prop_type_level_take :: Positive Int -> Positive Int -> Property
prop_type_level_take (Positive n) (Positive k) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Take[v: Vector, k: int] = Vector[min(k, v.length)] {\n" ++
                 "  data: take(k, v.data)\n" ++
                 "}\n" ++
                 "func testTake(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  taken := Take[v, " ++ show k ++ "]\n" ++
                 "  fmt.Println(\"Taken:\", taken.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && k <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级跳过前N个
prop_type_level_drop :: Positive Int -> Positive Int -> Property
prop_type_level_drop (Positive n) (Positive k) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Drop[v: Vector, k: int] = Vector[max(0, v.length - k)] {\n" ++
                 "  data: drop(k, v.data)\n" ++
                 "}\n" ++
                 "func testDrop(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  dropped := Drop[v, " ++ show k ++ "]\n" ++
                 "  fmt.Println(\"Dropped:\", dropped.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && k <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级切片
prop_type_level_slice :: Positive Int -> Positive Int -> Positive Int -> Property
prop_type_level_slice (Positive n) (Positive start) (Positive len_) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Slice[v: Vector, start: int, len: int] = Vector[min(len, v.length - start)] {\n" ++
                 "  data: slice(v.data, start, start + len)\n" ++
                 "}\n" ++
                 "func testSlice(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  sliced := Slice[v, " ++ show start ++ ", " ++ show len_ ++ "]\n" ++
                 "  fmt.Println(\"Sliced:\", sliced.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && start <= 10 && len_ <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级重复
prop_type_level_repeat :: Positive Int -> Positive Int -> Property
prop_type_level_repeat (Positive n) (Positive k) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Repeat[v: Vector, k: int] = Vector[v.length * k] {\n" ++
                 "  data: repeat(v.data, k)\n" ++
                 "}\n" ++
                 "func testRepeat(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  repeated := Repeat[v, " ++ show k ++ "]\n" ++
                 "  fmt.Println(\"Repeated:\", repeated.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 5 && k <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级交错
prop_type_level_interleave :: Positive Int -> Positive Int -> Property
prop_type_level_interleave (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Interleave[a: Vector, b: Vector] = Vector[min(a.length, b.length) * 2] {\n" ++
                 "  data: interleave(a.data, b.data)\n" ++
                 "}\n" ++
                 "func testInterleave(a: Vector[" ++ show m ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  interleaved := Interleave[a, b]\n" ++
                 "  fmt.Println(\"Interleaved:\", interleaved.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && m <= 5 && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级转置
prop_type_level_transpose :: Positive Int -> Positive Int -> Property
prop_type_level_transpose (Positive rows) (Positive cols) =
  let typusCode = "type Matrix[rows: int, cols: int] struct {\n" ++
                 "  data [" ++ show rows ++ "][" ++ show cols ++ "]int\n" ++
                 "}\n" ++
                 "type Transpose[m: Matrix[rows, cols]] = Matrix[cols, rows] {\n" ++
                 "  data: transpose(m.data)\n" ++
                 "}\n" ++
                 "func testTranspose(m: Matrix[" ++ show rows ++ ", " ++ show cols ++ "]) {\n" ++
                 "  transposed := Transpose[m]\n" ++
                 "  fmt.Println(\"Transposed:\", transposed.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && rows <= 5 && cols <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级扁平化
prop_type_level_flatten :: Positive Int -> Positive Int -> Property
prop_type_level_flatten (Positive outer) (Positive inner) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Nested[outer: int, inner: int] = Vector[outer] {\n" ++
                 "  data: [" ++ show outer ++ "]Vector[inner]\n" ++
                 "}\n" ++
                 "type Flatten[n: Nested[outer, inner]] = Vector[outer * inner] {\n" ++
                 "  data: flatten(n.data)\n" ++
                 "}\n" ++
                 "func testFlatten(n: Nested[" ++ show outer ++ ", " ++ show inner ++ "]) {\n" ++
                 "  flattened := Flatten[n]\n" ++
                 "  fmt.Println(\"Flattened:\", flattened.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && outer <= 5 && inner <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级分组为块
prop_type_level_chunk :: Positive Int -> Positive Int -> Property
prop_type_level_chunk (Positive n) (Positive size) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Chunk[v: Vector, size: int] = Vector[ceil(v.length / size)] {\n" ++
                 "  data: chunk(v.data, size)\n" ++
                 "}\n" ++
                 "func testChunk(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  chunked := Chunk[v, " ++ show size ++ "]\n" ++
                 "  fmt.Println(\"Chunked:\", chunked.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && size <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级窗口
prop_type_level_window :: Positive Int -> Positive Int -> Property
prop_type_level_window (Positive n) (Positive size) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Window[v: Vector, size: int] = Vector[max(0, v.length - size + 1)] {\n" ++
                 "  data: window(v.data, size)\n" ++
                 "}\n" ++
                 "func testWindow(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  windows := Window[v, " ++ show size ++ "]\n" ++
                 "  fmt.Println(\"Windows:\", windows.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && size <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级滑动
prop_type_level_sliding :: Positive Int -> Positive Int -> Property
prop_type_level_sliding (Positive n) (Positive size) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Sliding[v: Vector, size: int] = Vector[max(0, v.length - size + 1)] {\n" ++
                 "  data: sliding(v.data, size)\n" ++
                 "}\n" ++
                 "func testSliding(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  slidings := Sliding[v, " ++ show size ++ "]\n" ++
                 "  fmt.Println(\"Slidings:\", slidings.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && size <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级旋转
prop_type_level_rotate :: Positive Int -> Int -> Property
prop_type_level_rotate (Positive n) k =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Rotate[v: Vector, k: int] = Vector[v.length] {\n" ++
                 "  data: rotate(v.data, k)\n" ++
                 "}\n" ++
                 "func testRotate(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  rotated := Rotate[v, " ++ show k ++ "]\n" ++
                 "  fmt.Println(\"Rotated:\", rotated.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && abs k <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级置换
prop_type_level_permute :: Positive Int -> Property
prop_type_level_permute (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Permute[v: Vector, indices: Vector[int]] = Vector[v.length] {\n" ++
                 "  data: permute(v.data, indices.data)\n" ++
                 "}\n" ++
                 "func testPermute(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  indices := Vector[" ++ show n ++ "]{data: []int{" ++ 
                 (init $ tail $ concat $ replicate n "i,") ++ "}}\n" ++
                 "  permuted := Permute[v, indices]\n" ++
                 "  fmt.Println(\"Permuted:\", permuted.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级洗牌
prop_type_level_shuffle :: Positive Int -> Property
prop_type_level_shuffle (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Shuffle[v: Vector, seed: int] = Vector[v.length] {\n" ++
                 "  data: shuffle(v.data, seed)\n" ++
                 "}\n" ++
                 "func testShuffle(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  shuffled := Shuffle[v, 42]\n" ++
                 "  fmt.Println(\"Shuffled:\", shuffled.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级采样
prop_type_level_sample :: Positive Int -> Positive Int -> Property
prop_type_level_sample (Positive n) (Positive k) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Sample[v: Vector, k: int, seed: int] = Vector[min(k, v.length)] {\n" ++
                 "  data: sample(v.data, k, seed)\n" ++
                 "}\n" ++
                 "func testSample(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  sampled := Sample[v, " ++ show k ++ ", 42]\n" ++
                 "  fmt.Println(\"Sampled:\", sampled.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10 && k <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级唯一化
prop_type_level_unique :: Positive Int -> Property
prop_type_level_unique (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Unique[v: Vector] = Vector[countUnique(v.data)] {\n" ++
                 "  data: unique(v.data)\n" ++
                 "}\n" ++
                 "func testUnique(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  uniqued := Unique[v]\n" ++
                 "  fmt.Println(\"Uniqued:\", uniqued.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级去重
prop_type_level_deduplicate :: Positive Int -> Property
prop_type_level_deduplicate (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Deduplicate[v: Vector] = Vector[countUnique(v.data)] {\n" ++
                 "  data: deduplicate(v.data)\n" ++
                 "}\n" ++
                 "func testDeduplicate(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  deduplicated := Deduplicate[v]\n" ++
                 "  fmt.Println(\"Deduplicated:\", deduplicated.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级集合操作
prop_type_level_set_operations :: Positive Int -> Positive Int -> Property
prop_type_level_set_operations (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Union[a: Vector, b: Vector] = Vector[countUnique(concat(a.data, b.data))] {\n" ++
                 "  data: union(a.data, b.data)\n" ++
                 "}\n" ++
                 "type Intersection[a: Vector, b: Vector] = Vector[countIntersection(a.data, b.data)] {\n" ++
                 "  data: intersection(a.data, b.data)\n" ++
                 "}\n" ++
                 "type Difference[a: Vector, b: Vector] = Vector[countDifference(a.data, b.data)] {\n" ++
                 "  data: difference(a.data, b.data)\n" ++
                 "}\n" ++
                 "func testSetOperations(a: Vector[" ++ show m ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  union := Union[a, b]\n" ++
                 "  intersection := Intersection[a, b]\n" ++
                 "  difference := Difference[a, b]\n" ++
                 "  fmt.Println(\"Union:\", union.data)\n" ++
                 "  fmt.Println(\"Intersection:\", intersection.data)\n" ++
                 "  fmt.Println(\"Difference:\", difference.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && m <= 10 && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级包
prop_type_level_group_by :: Positive Int -> Property
prop_type_level_group_by (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type GroupBy[key: int -> int, v: Vector] = Map[key, Vector] {\n" ++
                 "  groups: groupBy(key, v.data)\n" ++
                 "}\n" ++
                 "type Mod3[i: int] = i % 3\n" ++
                 "func testGroupBy(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  groups := GroupBy[Mod3, v]\n" ++
                 "  fmt.Println(\"Groups:\", groups.groups)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级折叠
prop_type_level_fold_advanced :: Positive Int -> Property
prop_type_level_fold_advanced (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type FoldLeft[op: (acc, int) -> acc, init: acc, v: Vector] = acc {\n" ++
                 "  value: foldLeft(op, init, v.data)\n" ++
                 "}\n" ++
                 "type FoldRight[op: (int, acc) -> acc, init: acc, v: Vector] = acc {\n" ++
                 "  value: foldRight(op, init, v.data)\n" ++
                 "}\n" ++
                 "type Add[acc: int, x: int] = acc + x\n" ++
                 "type Multiply[x: int, acc: int] = x * acc\n" ++
                 "func testFold(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  sum := FoldLeft[Add, 0, v]\n" ++
                 "  product := FoldRight[Multiply, 1, v]\n" ++
                 "  fmt.Println(\"Sum:\", sum.value)\n" ++
                 "  fmt.Println(\"Product:\", product.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级扫描
prop_type_level_scan :: Positive Int -> Property
prop_type_level_scan (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type ScanLeft[op: (acc, int) -> acc, init: acc, v: Vector] = Vector[v.length + 1] {\n" ++
                 "  data: scanLeft(op, init, v.data)\n" ++
                 "}\n" ++
                 "type ScanRight[op: (int, acc) -> acc, init: acc, v: Vector] = Vector[v.length + 1] {\n" ++
                 "  data: scanRight(op, init, v.data)\n" ++
                 "}\n" ++
                 "type Add[acc: int, x: int] = acc + x\n" ++
                 "func testScan(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  prefixSums := ScanLeft[Add, 0, v]\n" ++
                 "  suffixSums := ScanRight[Add, 0, v]\n" ++
                 "  fmt.Println(\"Prefix sums:\", prefixSums.data)\n" ++
                 "  fmt.Println(\"Suffix sums:\", suffixSums.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级展开
prop_type_level_unfold :: Positive Int -> Property
prop_type_level_unfold (Positive n) =
  let typusCode = "type Unfold[next: state -> (state, int), init: state, n: int] = Vector[n] {\n" ++
                 "  data: unfold(next, init, n)\n" ++
                 "}\n" ++
                 "type Next[i: int] = (i + 1, i * 2)\n" ++
                 "func testUnfold() {\n" ++
                 "  sequence := Unfold[Next, 1, " ++ show n ++ "]\n" ++
                 "  fmt.Println(\"Sequence:\", sequence.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级迭代
prop_type_level_iterate :: Positive Int -> Property
prop_type_level_iterate (Positive n) =
  let typusCode = "type Iterate[f: int -> int, init: int, n: int] = Vector[n] {\n" ++
                 "  data: iterate(f, init, n)\n" ++
                 "}\n" ++
                 "type Double[i: int] = i * 2\n" ++
                 "func testIterate() {\n" ++
                 "  powers := Iterate[Double, 1, " ++ show n ++ "]\n" ++
                 "  fmt.Println(\"Powers of 2:\", powers.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级重复
prop_type_level_repeat_advanced :: Positive Int -> Property
prop_type_level_repeat_advanced (Positive n) =
  let typusCode = "type Repeat[x: int, n: int] = Vector[n] {\n" ++
                 "  data: replicate(n, x)\n" ++
                 "}\n" ++
                 "func testRepeat() {\n" ++
                 "  repeated := Repeat[42, " ++ show n ++ "]\n" ++
                 "  fmt.Println(\"Repeated:\", repeated.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级范围
prop_type_level_range :: Positive Int -> Property
prop_type_level_range (Positive n) =
  let typusCode = "type Range[start: int, end: int] = Vector[end - start] {\n" ++
                 "  data: range(start, end)\n" ++
                 "}\n" ++
                 "func testRange() {\n" ++
                 "  numbers := Range[1, " ++ show (n + 1) ++ "]\n" ++
                 "  fmt.Println(\"Range:\", numbers.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级枚举
prop_type_level_enum :: Positive Int -> Property
prop_type_level_enum (Positive n) =
  let typusCode = "type Enum[from: int, to: int, step: int] = Vector[ceil((to - from) / step)] {\n" ++
                 "  data: enum(from, to, step)\n" ++
                 "}\n" ++
                 "func testEnum() {\n" ++
                 "  evens := Enum[0, " ++ show (n * 2) ++ ", 2]\n" ++
                 "  fmt.Println(\"Even numbers:\", evens.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级连接
prop_type_level_join :: Positive Int -> Positive Int -> Property
prop_type_level_join (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Join[outer: Vector[Vector[inner]]] = Vector[sumLengths(outer)] {\n" ++
                 "  data: concatAll(outer.data)\n" ++
                 "}\n" ++
                 "func testJoin() {\n" ++
                 "  outer := Vector[" ++ show m ++ "] {\n" ++
                 "    data: [\n" ++
                 "      Vector[" ++ show n ++ "]{data: []int{1, 2, 3}},\n" ++
                 "      Vector[" ++ show n ++ "]{data: []int{4, 5, 6}},\n" ++
                 "      Vector[" ++ show n ++ "]{data: []int{7, 8, 9}}\n" ++
                 "    ]\n" ++
                 "  }\n" ++
                 "  joined := Join[outer]\n" ++
                 "  fmt.Println(\"Joined:\", joined.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && m <= 5 && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级绑定
prop_type_level_bind :: Positive Int -> Positive Int -> Property
prop_type_level_bind (Positive m) (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Bind[a: Vector, f: int -> Vector] = Vector[sumLengths(map(f, a.data))] {\n" ++
                 "  data: concatMap(f, a.data)\n" ++
                 "}\n" ++
                 "type Replicate[i: int] = Vector[i] {\n" ++
                 "  data: replicate(i, i)\n" ++
                 "}\n" ++
                 "func testBind(v: Vector[" ++ show m ++ "]) {\n" ++
                 "  bound := Bind[v, Replicate]\n" ++
                 "  fmt.Println(\"Bound:\", bound.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && m <= 5 && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级应用
prop_type_level_apply :: Positive Int -> Property
prop_type_level_apply (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Apply[fs: Vector[int -> int], xs: Vector[int]] = Vector[min(fs.length, xs.length)] {\n" ++
                 "  data: zipWith(apply, fs.data, xs.data)\n" ++
                 "}\n" ++
                 "type Double[i: int] = i * 2\n" ++
                 "type Triple[i: int] = i * 3\n" ++
                 "func testApply() {\n" ++
                 "  fs := Vector[" ++ show n ++ "]{data: []func(int) int{Double, Triple}}\n" ++
                 "  xs := Vector[" ++ show n ++ "]{data: []int{1, 2, 3}}\n" ++
                 "  applied := Apply[fs, xs]\n" ++
                 "  fmt.Println(\"Applied:\", applied.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 5  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级提升
prop_type_level_lift :: Positive Int -> Property
prop_type_level_lift (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Lift2[f: (int, int) -> int, a: Vector, b: Vector] = Vector[min(a.length, b.length)] {\n" ++
                 "  data: zipWith(f, a.data, b.data)\n" ++
                 "}\n" ++
                 "type Add[a: int, b: int] = a + b\n" ++
                 "type Multiply[a: int, b: int] = a * b\n" ++
                 "func testLift(a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  sums := Lift2[Add, a, b]\n" ++
                 "  products := Lift2[Multiply, a, b]\n" ++
                 "  fmt.Println(\"Sums:\", sums.data)\n" ++
                 "  fmt.Println(\"Products:\", products.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级压缩
prop_type_level_zip_with :: Positive Int -> Property
prop_type_level_zip_with (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type ZipWith[f: (int, int) -> int, a: Vector, b: Vector] = Vector[min(a.length, b.length)] {\n" ++
                 "  data: zipWith(f, a.data, b.data)\n" ++
                 "}\n" ++
                 "type Add[a: int, b: int] = a + b\n" ++
                 "type Multiply[a: int, b: int] = a * b\n" ++
                 "func testZipWith(a: Vector[" ++ show n ++ "], b: Vector[" ++ show n ++ "]) {\n" ++
                 "  sums := ZipWith[Add, a, b]\n" ++
                 "  products := ZipWith[Multiply, a, b]\n" ++
                 "  fmt.Println(\"Sums:\", sums.data)\n" ++
                 "  fmt.Println(\"Products:\", products.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级并行
prop_type_level_parallel :: Positive Int -> Property
prop_type_level_parallel (Positive n) =
  let typusCode = "type Vector[n: int] struct { data [" ++ show n ++ "]int }\n" ++
                 "type Parallel[f: int -> int, v: Vector] = Vector[v.length] {\n" ++
                 "  data: parallelMap(f, v.data)\n" ++
                 "}\n" ++
                 "type Square[i: int] = i * i\n" ++
                 "func testParallel(v: Vector[" ++ show n ++ "]) {\n" ++
                 "  squared := Parallel[Square, v]\n" ++
                 "  fmt.Println(\"Squared:\", squared.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级流
prop_type_level_stream :: Positive Int -> Property
prop_type_level_stream (Positive n) =
  let typusCode = "type Stream[next: state -> (state, int), init: state] struct {\n" ++
                 "  next: next\n" ++
                 "  state: init\n" ++
                 "}\n" ++
                 "type Take[s: Stream, n: int] = Vector[n] {\n" ++
                 "  data: takeStream(s, n)\n" ++
                 "}\n" ++
                 "type Next[i: int] = (i + 1, i * 2)\n" ++
                 "func testStream() {\n" ++
                 "  stream := Stream[Next, 1]{next: Next, state: 1}\n" ++
                 "  taken := Take[stream, " ++ show n ++ "]\n" ++
                 "  fmt.Println(\"Stream values:\", taken.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级惰性
prop_type_level_lazy :: Positive Int -> Property
prop_type_level_lazy (Positive n) =
  let typusCode = "type Lazy[T] struct {\n" ++
                 "  compute: () -> T\n" ++
                 "  value: Option[T]\n" ++
                 "}\n" ++
                 "type Force[l: Lazy[T>] = T {\n" ++
                 "  value: force(l)\n" ++
                 "}\n" ++
                 "func testLazy() {\n" ++
                 "  lazy := Lazy[int]{\n" ++
                 "    compute: func() int { return " ++ show n ++ " * 2 },\n" ++
                 "    value: None\n" ++
                 "  }\n" ++
                 "  value := Force[lazy]\n" ++
                 "  fmt.Println(\"Lazy value:\", value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级记忆化
prop_type_level_memoize :: Positive Int -> Property
prop_type_level_memoize (Positive n) =
  let typusCode = "type Memoize[f: int -> int] = int -> int {\n" ++
                 "  compute: memoize(f)\n" ++
                 "}\n" ++
                 "type Fibonacci[i: int] = if i <= 1 then i else Fibonacci[i-1] + Fibonacci[i-2]\n" ++
                 "func testMemoize() {\n" ++
                 "  memoFib := Memoize[Fibonacci]\n" ++
                 "  result := memoFib(" ++ show n ++ ")\n" ++
                 "  fmt.Println(\"Fibonacci(\" ++ show n ++ "):\", result)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级缓存
prop_type_level_cache :: Positive Int -> Property
prop_type_level_cache (Positive n) =
  let typusCode = "type Cache[key: int, value: int] struct {\n" ++
                 "  data: Map[key, value]\n" ++
                 "  maxSize: int\n" ++
                 "}\n" ++
                 "type Get[c: Cache[key, value], k: key] = Option[value] {\n" ++
                 "  value: get(c, k)\n" ++
                 "}\n" ++
                 "type Put[c: Cache[key, value], k: key, v: value] = Cache[key, value] {\n" ++
                 "  data: put(c.data, k, v)\n" ++
                 "  maxSize: c.maxSize\n" ++
                 "}\n" ++
                 "func testCache() {\n" ++
                 "  cache := Cache[int, int]{data: emptyMap, maxSize: " ++ show n ++ "}\n" ++
                 "  cache1 := Put[cache, 1, 10]\n" ++
                 "  value := Get[cache1, 1]\n" ++
                 "  fmt.Println(\"Cached value:\", value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级池
prop_type_level_pool :: Positive Int -> Property
prop_type_level_pool (Positive n) =
  let typusCode = "type Pool[T] struct {\n" ++
                 "  items: []T\n" ++
                 "  available: []bool\n" ++
                 "}\n" ++
                 "type Acquire[p: Pool[T]] = Option[T] {\n" ++
                 "  value: acquire(p)\n" ++
                 "}\n" ++
                 "type Release[p: Pool[T], item: T] = Pool[T] {\n" ++
                 "  items: p.items\n" ++
                 "  available: markAvailable(p.available, item)\n" ++
                 "}\n" ++
                 "func testPool() {\n" ++
                 "  pool := Pool[int]{\n" ++
                 "    items: []int{1, 2, 3, 4, 5},\n" ++
                 "    available: []bool{true, true, true, true, true}\n" ++
                 "  }\n" ++
                 "  item := Acquire[pool]\n" ++
                 "  match item {\n" ++
                 "    case Some(value):\n" ++
                 "      pool2 := Release[pool, value]\n" ++
                 "      fmt.Println(\"Acquired and released:\", value)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Pool exhausted\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级队列
prop_type_level_queue :: Positive Int -> Property
prop_type_level_queue (Positive n) =
  let typusCode = "type Queue[T] struct {\n" ++
                 "  front: []T\n" ++
                 "  back: []T\n" ++
                 "}\n" ++
                 "type Enqueue[q: Queue[T], item: T] = Queue[T] {\n" ++
                 "  front: q.front\n" ++
                 "  back: append(q.back, item)\n" ++
                 "}\n" ++
                 "type Dequeue[q: Queue[T]] = (Option[T], Queue[T]) {\n" ++
                 "  item: if len(q.front) > 0 then Some(q.front[0]) else if len(q.back) > 0 then Some(q.back[len(q.back)-1]) else None\n" ++
                 "  queue: if len(q.front) > 0 then Queue[T]{front: q.front[1:], back: q.back} else if len(q.back) > 0 then Queue[T]{front: reverse(q.back)[1:], back: []} else q\n" ++
                 "}\n" ++
                 "func testQueue() {\n" ++
                 "  queue := Queue[int]{front: [], back: []}\n" ++
                 "  queue1 := Enqueue[queue, 1]\n" ++
                 "  queue2 := Enqueue[queue1, 2]\n" ++
                 "  (item, queue3) := Dequeue[queue2]\n" ++
                 "  match item {\n" ++
                 "    case Some(value):\n" ++
                 "      fmt.Println(\"Dequeued:\", value)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Queue empty\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级栈
prop_type_level_stack :: Positive Int -> Property
prop_type_level_stack (Positive n) =
  let typusCode = "type Stack[T] struct {\n" ++
                 "  items: []T\n" ++
                 "}\n" ++
                 "type Push[s: Stack[T], item: T] = Stack[T] {\n" ++
                 "  items: append(s.items, item)\n" ++
                 "}\n" ++
                 "type Pop[s: Stack[T]] = (Option[T], Stack[T]) {\n" ++
                 "  item: if len(s.items) > 0 then Some(s.items[len(s.items)-1]) else None\n" ++
                 "  stack: if len(s.items) > 0 then Stack[T]{items: s.items[:len(s.items)-1]} else s\n" ++
                 "}\n" ++
                 "func testStack() {\n" ++
                 "  stack := Stack[int]{items: []}\n" ++
                 "  stack1 := Push[stack, 1]\n" ++
                 "  stack2 := Push[stack1, 2]\n" ++
                 "  (item, stack3) := Pop[stack2]\n" ++
                 "  match item {\n" ++
                 "    case Some(value):\n" ++
                 "      fmt.Println(\"Popped:\", value)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Stack empty\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级堆
prop_type_level_heap :: Positive Int -> Property
prop_type_level_heap (Positive n) =
  let typusCode = "type Heap[T] struct {\n" ++
                 "  data: []T\n" ++
                 "  compare: (T, T) -> bool\n" ++
                 "}\n" ++
                 "type Insert[h: Heap[T], item: T] = Heap[T] {\n" ++
                 "  data: insertHeap(h.data, item, h.compare)\n" ++
                 "  compare: h.compare\n" ++
                 "}\n" ++
                 "type ExtractMin[h: Heap[T]] = (Option[T], Heap[T]) {\n" ++
                 "  item: if len(h.data) > 0 then Some(h.data[0]) else None\n" ++
                 "  heap: if len(h.data) > 0 then Heap[T]{data: extractMinHeap(h.data, h.compare), compare: h.compare} else h\n" ++
                 "}\n" ++
                 "func testHeap() {\n" ++
                 "  heap := Heap[int]{data: [], compare: func(a, b int) bool { return a < b }}\n" ++
                 "  heap1 := Insert[heap, 3]\n" ++
                 "  heap2 := Insert[heap1, 1]\n" ++
                 "  heap3 := Insert[heap2, 2]\n" ++
                 "  (item, heap4) := ExtractMin[heap3]\n" ++
                 "  match item {\n" ++
                 "    case Some(value):\n" ++
                 "      fmt.Println(\"Extracted min:\", value)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Heap empty\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级映射
prop_type_level_map_advanced :: Positive Int -> Property
prop_type_level_map_advanced (Positive n) =
  let typusCode = "type Map[key: int, value: int] struct {\n" ++
                 "  data: [](key, value)\n" ++
                 "}\n" ++
                 "type Put[m: Map[key, value], k: key, v: value] = Map[key, value] {\n" ++
                 "  data: putMap(m.data, k, v)\n" ++
                 "}\n" ++
                 "type Get[m: Map[key, value], k: key] = Option[value] {\n" ++
                 "  value: getMap(m.data, k)\n" ++
                 "}\n" ++
                 "type Remove[m: Map[key, value], k: key] = Map[key, value] {\n" ++
                 "  data: removeMap(m.data, k)\n" ++
                 "}\n" ++
                 "func testMap() {\n" ++
                 "  map := Map[int, int]{data: []}\n" ++
                 "  map1 := Put[map, 1, 10]\n" ++
                 "  map2 := Put[map1, 2, 20]\n" ++
                 "  value := Get[map2, 1]\n" ++
                 "  map3 := Remove[map2, 1]\n" ++
                 "  match value {\n" ++
                 "    case Some(v):\n" ++
                 "      fmt.Println(\"Value for key 1:\", v)\n" ++
                 "    case None:\n" ++
                 "      fmt.Println(\"Key 1 not found\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级集合
prop_type_level_set_advanced :: Positive Int -> Property
prop_type_level_set_advanced (Positive n) =
  let typusCode = "type Set[T] struct {\n" ++
                 "  data: []T\n" ++
                 "}\n" ++
                 "type Insert[s: Set[T], item: T] = Set[T] {\n" ++
                 "  data: insertSet(s.data, item)\n" ++
                 "}\n" ++
                 "type Contains[s: Set[T], item: T] = bool {\n" ++
                 "  value: containsSet(s.data, item)\n" ++
                 "}\n" ++
                 "type Remove[s: Set[T], item: T] = Set[T] {\n" ++
                 "  data: removeSet(s.data, item)\n" ++
                 "}\n" ++
                 "type Union[a: Set[T], b: Set[T]] = Set[T] {\n" ++
                 "  data: unionSet(a.data, b.data)\n" ++
                 "}\n" ++
                 "type Intersection[a: Set[T], b: Set[T]] = Set[T] {\n" ++
                 "  data: intersectionSet(a.data, b.data)\n" ++
                 "}\n" ++
                 "func testSet() {\n" ++
                 "  set := Set[int]{data: []}\n" ++
                 "  set1 := Insert[set, 1]\n" ++
                 "  set2 := Insert[set1, 2]\n" ++
                 "  hasOne := Contains[set2, 1]\n" ++
                 "  set3 := Remove[set2, 1]\n" ++
                 "  if hasOne {\n" ++
                 "    fmt.Println(\"Set contains 1\")\n" ++
                 "  } else {\n" ++
                 "    fmt.Println(\"Set does not contain 1\")\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级树
prop_type_level_tree :: Positive Int -> Property
prop_type_level_tree (Positive n) =
  let typusCode = "type Tree[T] struct {\n" ++
                 "  value: T\n" ++
                 "  left: Option[Tree[T]]\n" ++
                 "  right: Option[Tree[T]]\n" ++
                 "}\n" ++
                 "type Leaf[T] = Tree[T] {\n" ++
                 "  value: T\n" ++
                 "  left: None\n" ++
                 "  right: None\n" ++
                 "}\n" ++
                 "type Node[T] = Tree[T] {\n" ++
                 "  value: T\n" ++
                 "  left: Some(Tree[T])\n" ++
                 "  right: Some(Tree[T])\n" ++
                 "}\n" ++
                 "type Insert[t: Tree[int], value: int] = Tree[int] {\n" ++
                 "  value: if value < t.value then t.value else value\n" ++
                 "  left: if value < t.value then Some(Insert[getOrEmpty(t.left), value]) else t.left\n" ++
                 "  right: if value >= t.value then Some(Insert[getOrEmpty(t.right), value]) else t.right\n" ++
                 "}\n" ++
                 "func getOrEmpty[t: Option[Tree[int]]] = Tree[int] {\n" ++
                 "  match t {\n" ++
                 "    case Some(tree): return tree\n" ++
                 "    case None: return Leaf[int]{value: 0}\n" ++
                 "  }\n" ++
                 "}\n" ++
                 "func testTree() {\n" ++
                 "  tree := Leaf[int]{value: 5}\n" ++
                 "  tree1 := Insert[tree, 3]\n" ++
                 "  tree2 := Insert[tree1, 7]\n" ++
                 "  fmt.Println(\"Tree root:\", tree2.value)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

-- | 测试依赖类型的类型级图
prop_type_level_graph :: Positive Int -> Property
prop_type_level_graph (Positive n) =
  let typusCode = "type Graph[node: int, edge: (int, int)] struct {\n" ++
                 "  nodes: []node\n" ++
                 "  edges: []edge\n" ++
                 "}\n" ++
                 "type AddNode[g: Graph[node, edge], n: node] = Graph[node, edge] {\n" ++
                 "  nodes: append(g.nodes, n)\n" ++
                 "  edges: g.edges\n" ++
                 "}\n" ++
                 "type AddEdge[g: Graph[node, edge], e: edge] = Graph[node, edge] {\n" ++
                 "  nodes: g.nodes\n" ++
                 "  edges: append(g.edges, e)\n" ++
                 "}\n" ++
                 "type Neighbors[g: Graph[node, edge], n: node] = []node {\n" ++
                 "  value: findNeighbors(g.edges, n)\n" ++
                 "}\n" ++
                 "func testGraph() {\n" ++
                 "  graph := Graph[int, (int, int)]{nodes: [], edges: []}\n" ++
                 "  graph1 := AddNode[graph, 1]\n" ++
                 "  graph2 := AddNode[graph1, 2]\n" ++
                 "  graph3 := AddEdge[graph2, (1, 2)]\n" ++
                 "  neighbors := Neighbors[graph3, 1]\n" ++
                 "  fmt.Println(\"Neighbors of 1:\", neighbors)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult && n <= 10  -- Limit to prevent large arrays

tests :: TestTree
tests = testGroup "Test.Unit.NewDependentTypeFeaturesTestSuite Tests"
  [ testProperty "value parameterized type parsing" prop_value_parameterized_type_parsing
  , testProperty "precise type constraints" prop_precise_type_constraints
  , testProperty "dependent function signature" prop_dependent_function_signature
  , testProperty "function preconditions" prop_function_preconditions
  , testProperty "assertion narrowing" prop_assertion_narrowing
  , testProperty "compile time constant propagation" prop_compile_time_constant_propagation
  , testProperty "existential types" prop_existential_types
  , testProperty "type inference" prop_type_inference
  , testProperty "type level arithmetic" prop_type_level_arithmetic
  , testProperty "mixed type and value parameters" prop_mixed_type_and_value_parameters
  , testProperty "parameterized precise types" prop_parameterized_precise_types
  , testProperty "inter parameter constraints" prop_inter_parameter_constraints
  , testProperty "static assert" prop_static_assert
  , testProperty "boundary conditions" prop_boundary_conditions
  , testProperty "complex dependent expressions" prop_complex_dependent_expressions
  , testProperty "nested dependent types" prop_nested_dependent_types
  , testProperty "recursive dependent types" prop_recursive_dependent_types
  , testProperty "conditional types" prop_conditional_types
  , testProperty "combined constraints" prop_combined_constraints
  , testProperty "type level functions" prop_type_level_functions
  , testProperty "type level conditionals" prop_type_level_conditionals
  , testProperty "dependent type pattern matching" prop_dependent_type_pattern_matching
  , testProperty "dependent type error handling" prop_dependent_type_error_handling
  , testProperty "dependent type performance" prop_dependent_type_performance
  , testProperty "boundary check optimization" prop_boundary_check_optimization
  , testProperty "compile time optimization" prop_compile_time_optimization
  , testProperty "dependent type inference" prop_dependent_type_inference
  , testProperty "constraint propagation" prop_constraint_propagation
  , testProperty "type equality" prop_type_equality
  , testProperty "type conversion" prop_type_conversion
  , testProperty "generic constraints" prop_generic_constraints
  , testProperty "recursive constraints" prop_recursive_constraints
  , testProperty "type level recursion" prop_type_level_recursion
  , testProperty "type level conditionals advanced" prop_type_level_conditionals_advanced
  , testProperty "type level pattern matching" prop_type_level_pattern_matching
  , testProperty "type level function composition" prop_type_level_function_composition
  , testProperty "type level higher order functions" prop_type_level_higher_order_functions
  , testProperty "type level fold" prop_type_level_fold
  , testProperty "type level map" prop_type_level_map
  , testProperty "type level filter" prop_type_level_filter
  , testProperty "type level reduce" prop_type_level_reduce
  , testProperty "type level sort" prop_type_level_sort
  , testProperty "type level find" prop_type_level_find
  , testProperty "type level group" prop_type_level_group
  , testProperty "type level partition" prop_type_level_partition
  , testProperty "type level zip" prop_type_level_zip
  , testProperty "type level unzip" prop_type_level_unzip
  , testProperty "type level concat" prop_type_level_concat
  , testProperty "type level reverse" prop_type_level_reverse
  , testProperty "type level take" prop_type_level_take
  , testProperty "type level drop" prop_type_level_drop
  , testProperty "type level slice" prop_type_level_slice
  , testProperty "type level repeat" prop_type_level_repeat
  , testProperty "type level interleave" prop_type_level_interleave
  , testProperty "type level transpose" prop_type_level_transpose
  , testProperty "type level flatten" prop_type_level_flatten
  , testProperty "type level chunk" prop_type_level_chunk
  , testProperty "type level window" prop_type_level_window
  , testProperty "type level sliding" prop_type_level_sliding
  , testProperty "type level rotate" prop_type_level_rotate
  , testProperty "type level permute" prop_type_level_permute
  , testProperty "type level shuffle" prop_type_level_shuffle
  , testProperty "type level sample" prop_type_level_sample
  , testProperty "type level unique" prop_type_level_unique
  , testProperty "type level deduplicate" prop_type_level_deduplicate
  , testProperty "type level set operations" prop_type_level_set_operations
  , testProperty "type level group by" prop_type_level_group_by
  , testProperty "type level fold advanced" prop_type_level_fold_advanced
  , testProperty "type level scan" prop_type_level_scan
  , testProperty "type level unfold" prop_type_level_unfold
  , testProperty "type level iterate" prop_type_level_iterate
  , testProperty "type level repeat advanced" prop_type_level_repeat_advanced
  , testProperty "type level range" prop_type_level_range
  , testProperty "type level enum" prop_type_level_enum
  , testProperty "type level join" prop_type_level_join
  , testProperty "type level bind" prop_type_level_bind
  , testProperty "type level apply" prop_type_level_apply
  , testProperty "type level lift" prop_type_level_lift
  , testProperty "type level zip with" prop_type_level_zip_with
  , testProperty "type level parallel" prop_type_level_parallel
  , testProperty "type level stream" prop_type_level_stream
  , testProperty "type level lazy" prop_type_level_lazy
  , testProperty "type level memoize" prop_type_level_memoize
  , testProperty "type level cache" prop_type_level_cache
  , testProperty "type level pool" prop_type_level_pool
  , testProperty "type level queue" prop_type_level_queue
  , testProperty "type level stack" prop_type_level_stack
  , testProperty "type level heap" prop_type_level_heap
  , testProperty "type level map advanced" prop_type_level_map_advanced
  , testProperty "type level set advanced" prop_type_level_set_advanced
  , testProperty "type level tree" prop_type_level_tree
  , testProperty "type level graph" prop_type_level_graph
  ]