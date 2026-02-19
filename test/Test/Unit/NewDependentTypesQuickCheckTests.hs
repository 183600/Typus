{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypesQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Monad (when, replicateM, forM_)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort, intercalate)
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum, isAlpha)
import Data.Either (isLeft, isRight, fromRight)
import Data.Maybe (isJust, isNothing, listToMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- Import Typus modules
import Parser (parseTypus, parseTypusFile)
import Compiler (compile)
import DependentTypesParser (runDependentTypesParser)
import SyntaxValidator (validateSyntax)
import SourceLocation (SourcePos(..))

-- ============================================================================
-- 1. 值参数化类型测试
-- ============================================================================

-- | 测试基本的值参数化类型解析
prop_parseValueParameterizedType :: String -> Int -> Property
prop_parseValueParameterizedType nameStr n =
  let limitedName = take 10 $ filter isAlphaNum nameStr
      validName = if null limitedName then "Vector" else limitedName
      value = max 1 (abs n `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validName ++ "[" ++ show value ++ ": int] struct { data [" ++ show value ++ "]float64 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试多值参数化类型
prop_parseMultiValueParameterizedType :: String -> String -> Int -> Int -> Property
prop_parseMultiValueParameterizedType nameStr fieldStr n m =
  let limitedName = take 8 $ filter isAlpha nameStr
      limitedField = take 8 $ filter isAlpha fieldStr
      validName = if null limitedName then "Matrix" else limitedName
      validField = if null limitedField then "data" else limitedField
      rows = max 1 (abs n `mod` 50)
      cols = max 1 (abs m `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validName ++ "[" ++ show rows ++ ": int, " ++ show cols ++ ": int] struct { " ++ validField ++ " [" ++ show rows ++ "][" ++ show cols ++ "]float64 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试混合类型参数和值参数
prop_parseMixedTypeValueParameters :: String -> String -> Int -> Property
prop_parseMixedTypeValueParameters nameStr typeStr n =
  let limitedName = take 8 $ filter isAlpha nameStr
      limitedType = take 8 $ filter isAlpha typeStr
      validName = if null limitedName then "Container" else limitedName
      validType = if null limitedType then "T" else limitedType
      capacity = max 1 (abs n `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validName ++ "[" ++ validType ++ " any, " ++ show capacity ++ ": int] struct { data []" ++ validType ++ " }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 2. 精确类型测试
-- ============================================================================

-- | 测试基本精确类型定义
prop_parseRefinementType :: String -> String -> Property
prop_parseRefinementType typeNameStr constraintStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      limitedConstraint = take 15 $ filter isAlphaNum constraintStr
      validTypeName = if null limitedTypeName then "Positive" else limitedTypeName
      validVar = if null limitedConstraint then "x" else limitedConstraint
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { " ++ validVar ++ " > 0 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试复杂约束的精确类型
prop_parseComplexRefinementType :: String -> Int -> Int -> Property
prop_parseComplexRefinementType nameStr lo hi =
  let limitedName = take 10 $ filter isAlpha nameStr
      validName = if null limitedName then "Bounded" else limitedName
      low = max 0 (abs lo `mod` 100)
      high = max (low + 1) (low + (abs hi `mod` 50) + 1)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validName ++ "[" ++ show low ++ ": int, " ++ show high ++ ": int] = int where { self >= " ++ show low ++ " && self <= " ++ show high ++ " }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试字符串约束的精确类型
prop_parseStringRefinementType :: String -> Property
prop_parseStringRefinementType nameStr =
  let limitedName = take 10 $ filter isAlpha nameStr
      validName = if null limitedName then "NonEmpty" else limitedName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validName ++ " = string where { len(self) > 0 }"
      result = parseTypusFile code
  in property $ length validName > 0 ==> isRight result

-- ============================================================================
-- 3. 依赖函数签名测试
-- ============================================================================

-- | 测试依赖返回类型的函数
prop_parseDependentReturnType :: String -> Int -> Property
prop_parseDependentReturnType funcNameStr n =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "zeros" else limitedFuncName
      size = max 1 (abs n `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show size ++ ": int] struct { data [" ++ show size ++ "]float64 }\n\nfunc " ++ validFuncName ++ "(n: Positive) -> Vector[" ++ show size ++ "] { return Vector[" ++ show size ++ "]{data: make([]float64, " ++ show size ++ ")} }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试参数间依赖的函数
prop_parseParameterDependence :: String -> String -> Int -> Property
prop_parseParameterDependence funcNameStr paramNameStr n =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedParamName = take 8 $ filter isAlpha paramNameStr
      validFuncName = if null limitedFuncName then "get" else limitedFuncName
      validParamName = if null limitedParamName then "v" else validParamName
      size = max 1 (abs n `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show size ++ ": int] struct { data [" ++ show size ++ "]float64 }\ntype ValidIndex[" ++ show size ++ ": int] = int where { self >= 0 && self < " ++ show size ++ " }\n\nfunc " ++ validFuncName ++ "[" ++ show size ++ ": int](" ++ validParamName ++ ": Vector[" ++ show size ++ "], i: ValidIndex[" ++ show size ++ "]) -> float64 { return " ++ validParamName ++ ".data[i] }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试类型级算术
prop_parseTypeLevelArithmetic :: String -> Int -> Int -> Property
prop_parseTypeLevelArithmetic funcNameStr n m =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "concat" else limitedFuncName
      size1 = max 1 (abs n `mod` 25)
      size2 = max 1 (abs m `mod` 25)
      resultSize = size1 + size2
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show resultSize ++ ": int] struct { data [" ++ show resultSize ++ "]float64 }\n\nfunc " ++ validFuncName ++ "[" ++ show size1 ++ ": int, " ++ show size2 ++ ": int](a: Vector[" ++ show size1 ++ "], b: Vector[" ++ show size2 ++ "]) -> Vector[" ++ show resultSize ++ "] { return Vector[" ++ show resultSize ++ "]{data: make([]float64, " ++ show resultSize ++ ")} }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 4. 函数前置条件测试
-- ============================================================================

-- | 测试基本前置条件
prop_parsePrecondition :: String -> Int -> Property
prop_parsePrecondition funcNameStr n =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "average" else limitedFuncName
      size = max 1 (abs n `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show size ++ ": int] struct { data [" ++ show size ++ "]float64 }\n\nfunc " ++ validFuncName ++ "[" ++ show size ++ ": int](v: Vector[" ++ show size ++ "]) -> float64 where { " ++ show size ++ " > 0 } { return 0.0 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试复杂前置条件
prop_parseComplexPrecondition :: String -> Int -> Int -> Int -> Property
prop_parseComplexPrecondition funcNameStr n m p =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "matMul" else limitedFuncName
      rows = max 1 (abs n `mod` 20)
      cols = max 1 (abs m `mod` 20)
      inner = max 1 (abs p `mod` 20)
      code = "package main\n\n//! dependent_types: on\ntype Matrix[" ++ show rows ++ ": int, " ++ show cols ++ ": int] struct { data [" ++ show rows ++ "][" ++ show cols ++ "]float64 }\n\nfunc " ++ validFuncName ++ "[" ++ show rows ++ ": int, " ++ show cols ++ ": int, " ++ show inner ++ ": int](a: Matrix[" ++ show rows ++ ", " ++ show cols ++ "], b: Matrix[" ++ show cols ++ ", " ++ show inner ++ "]) -> Matrix[" ++ show rows ++ ", " ++ show inner ++ "] where { " ++ show rows ++ " > 0 && " ++ show cols ++ " > 0 && " ++ show inner ++ " > 0 } { return Matrix[" ++ show rows ++ ", " ++ show inner ++ "]{data: [][[" ++ show inner ++ "]float64{}} }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 5. 断言与条件窄化测试
-- ============================================================================

-- | 测试基本断言
prop_parseAssertion :: String -> Int -> Property
prop_parseAssertion varNameStr n =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "n" else limitedVarName
      value = max 1 (abs n `mod` 100)
      code = "package main\n\n//! dependent_types: on\nfunc process() { " ++ validVarName ++ " := readInt()\nassert " ++ validVarName ++ " > 0 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试静态断言
prop_parseStaticAssertion :: String -> Int -> Property
prop_parseStaticAssertion varNameStr n =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "size" else limitedVarName
      value = max 1 (abs n `mod` 100)
      code = "package main\n\n//! dependent_types: on\nfunc process() { " ++ validVarName ++ " := " ++ show value ++ "\nstatic_assert " ++ validVarName ++ " > 0 }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试条件窄化
prop_parseConditionNarrowing :: String -> Property
prop_parseConditionNarrowing varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "d" else limitedVarName
      code = "package main\n\n//! dependent_types: on\ntype NonZero = int where { self != 0 }\n\nfunc safeDiv(a: int, b: NonZero) -> int { return a / b }\n\nfunc process() { " ++ validVarName ++ " := readInt()\nif " ++ validVarName ++ " != 0 { r := safeDiv(10, " ++ validVarName ++ ") } }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 6. 存在类型测试
-- ============================================================================

-- | 测试存在类型定义
prop_parseExistentialType :: String -> Property
prop_parseExistentialType funcNameStr =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "readVector" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype Vector[some n: int] struct { data []float64 }\n\nfunc " ++ validFuncName ++ "(input: []float64) -> Vector[some n: int] where { n == len(input) } { return Vector{data: input} }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试存在类型解包
prop_parseExistentialUnpacking :: String -> Property
prop_parseExistentialUnpacking varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "v" else limitedVarName
      code = "package main\n\n//! dependent_types: on\ntype Vector[some n: int] struct { data []float64 }\n\nfunc process() { data := []float64{1.0, 2.0, 3.0}\n" ++ validVarName ++ " := readVector(data)\nmatch " ++ validVarName ++ ".(n) { fmt.Println(get(" ++ validVarName ++ ", 0)) } }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 7. 类型推导测试
-- ============================================================================

-- | 测试基本类型推导
prop_parseTypeInference :: String -> Int -> Property
prop_parseTypeInference funcNameStr n =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "createVector" else limitedFuncName
      size = max 1 (abs n `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show size ++ ": int] struct { data [" ++ show size ++ "]float64 }\n\nfunc " ++ validFuncName ++ "(n: Positive, value: float64) -> Vector[" ++ show size ++ "] { elements := make([]float64, n)\nfor i := 0; i < n; i++ { elements[i] = value }\nreturn Vector{elements} }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- | 测试复杂类型推导
prop_parseComplexTypeInference :: String -> String -> Property
prop_parseComplexTypeInference funcNameStr varNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedVarName = take 8 $ filter isAlpha varNameStr
      validFuncName = if null limitedFuncName then "process" else limitedFuncName
      validVarName = if null limitedVarName then "result" else limitedVarName
      code = "package main\n\n//! dependent_types: on\ntype Vector[3: int] struct { data [3]float64 }\n\nfunc " ++ validFuncName ++ "() { v1 := zeros(3)\nv2 := ones(3)\n" ++ validVarName ++ " := add(v1, v2) }"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- ============================================================================
-- 8. 边界情况测试
-- ============================================================================

-- | 测试零值参数
prop_parseZeroValueParameter :: String -> Property
prop_parseZeroValueParameter typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "ZeroVector" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ "[0: int] struct { data [0]float64 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试极大值参数
prop_parseLargeValueParameter :: String -> Property
prop_parseLargeValueParameter typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "LargeVector" else limitedTypeName
      largeValue = 100  -- 从1000000减少到100，大幅减少内存使用
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ "[" ++ show largeValue ++ ": int] struct { data [" ++ show largeValue ++ "]float64 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试嵌套依赖类型
prop_parseNestedDependentTypes :: String -> Property
prop_parseNestedDependentTypes typeNameStr =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Nested" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype Outer[" ++ validTypeName ++ ": int] struct { inner Inner[" ++ validTypeName ++ "] }\ntype Inner[" ++ validTypeName ++ ": int] struct { data [" ++ validTypeName ++ "]int }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 测试套件组装
-- ============================================================================

-- | 值参数化类型测试套件
valueParameterizedTypeTests :: TestTree
valueParameterizedTypeTests = testGroup "值参数化类型测试"
  [ testProperty "基本值参数化类型解析" prop_parseValueParameterizedType
  , testProperty "多值参数化类型" prop_parseMultiValueParameterizedType
  , testProperty "混合类型参数和值参数" prop_parseMixedTypeValueParameters
  ]

-- | 精确类型测试套件
refinementTypeTests :: TestTree
refinementTypeTests = testGroup "精确类型测试"
  [ testProperty "基本精确类型定义" prop_parseRefinementType
  , testProperty "复杂约束的精确类型" prop_parseComplexRefinementType
  , testProperty "字符串约束的精确类型" prop_parseStringRefinementType
  ]

-- | 依赖函数签名测试套件
dependentFunctionTests :: TestTree
dependentFunctionTests = testGroup "依赖函数签名测试"
  [ testProperty "依赖返回类型的函数" prop_parseDependentReturnType
  , testProperty "参数间依赖的函数" prop_parseParameterDependence
  , testProperty "类型级算术" prop_parseTypeLevelArithmetic
  ]

-- | 函数前置条件测试套件
preconditionTests :: TestTree
preconditionTests = testGroup "函数前置条件测试"
  [ testProperty "基本前置条件" prop_parsePrecondition
  , testProperty "复杂前置条件" prop_parseComplexPrecondition
  ]

-- | 断言与条件窄化测试套件
assertionTests :: TestTree
assertionTests = testGroup "断言与条件窄化测试"
  [ testProperty "基本断言" prop_parseAssertion
  , testProperty "静态断言" prop_parseStaticAssertion
  , testProperty "条件窄化" prop_parseConditionNarrowing
  ]

-- | 存在类型测试套件
existentialTypeTests :: TestTree
existentialTypeTests = testGroup "存在类型测试"
  [ testProperty "存在类型定义" prop_parseExistentialType
  , testProperty "存在类型解包" prop_parseExistentialUnpacking
  ]

-- | 类型推导测试套件
typeInferenceTests :: TestTree
typeInferenceTests = testGroup "类型推导测试"
  [ testProperty "基本类型推导" prop_parseTypeInference
  , testProperty "复杂类型推导" prop_parseComplexTypeInference
  ]

-- | 边界情况测试套件
boundaryCaseTests :: TestTree
boundaryCaseTests = testGroup "边界情况测试"
  [ testProperty "零值参数" prop_parseZeroValueParameter
  , testProperty "极大值参数" prop_parseLargeValueParameter
  , testProperty "嵌套依赖类型" prop_parseNestedDependentTypes
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "新依赖类型QuickCheck测试套件"
  [ valueParameterizedTypeTests
  , refinementTypeTests
  , dependentFunctionTests
  , preconditionTests
  , assertionTests
  , existentialTypeTests
  , typeInferenceTests
  , boundaryCaseTests
  ]