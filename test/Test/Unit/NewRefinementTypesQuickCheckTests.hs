{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewRefinementTypesQuickCheckTests where

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
-- 1. 基本约束类型测试
-- ============================================================================

-- | 测试非零整数约束
prop_parseNonZeroConstraint :: String -> Property
prop_parseNonZeroConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "NonZero" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self != 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试正数约束
prop_parsePositiveConstraint :: String -> Property
prop_parsePositiveConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Positive" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self > 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试负数约束
prop_parseNegativeConstraint :: String -> Property
prop_parseNegativeConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Negative" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self < 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试非负数约束
prop_parseNonNegativeConstraint :: String -> Property
prop_parseNonNegativeConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "NonNegative" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self >= 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 2. 范围约束测试
-- ============================================================================

-- | 测试基本范围约束
prop_parseRangeConstraint :: String -> Int -> Int -> Property
prop_parseRangeConstraint typeNameStr lo hi =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Bounded" else limitedTypeName
      low = max 0 (abs lo `mod` 100)
      high = max (low + 1) (low + (abs hi `mod` 50) + 1)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self >= " ++ show low ++ " && self <= " ++ show high ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试开区间约束
prop_parseOpenRangeConstraint :: String -> Int -> Int -> Property
prop_parseOpenRangeConstraint typeNameStr lo hi =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "OpenRange" else limitedTypeName
      low = max 0 (abs lo `mod` 100)
      high = max (low + 2) (low + (abs hi `mod` 50) + 2)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self > " ++ show low ++ " && self < " ++ show high ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试半开区间约束
prop_parseHalfOpenRangeConstraint :: String -> Int -> Int -> Property
prop_parseHalfOpenRangeConstraint typeNameStr lo hi =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "HalfOpen" else limitedTypeName
      low = max 0 (abs lo `mod` 100)
      high = max (low + 1) (low + (abs hi `mod` 50) + 1)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self >= " ++ show low ++ " && self < " ++ show high ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 3. 字符串约束测试
-- ============================================================================

-- | 测试非空字符串约束
prop_parseNonEmptyStringConstraint :: String -> Property
prop_parseNonEmptyStringConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "NonEmpty" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = string where { len(self) > 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试字符串长度约束
prop_parseStringLengthConstraint :: String -> Int -> Property
prop_parseStringLengthConstraint typeNameStr maxLen =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "MaxLength" else limitedTypeName
      maxLength = max 1 (abs maxLen `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = string where { len(self) <= " ++ show maxLength ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试字符串范围长度约束
prop_parseStringLengthRangeConstraint :: String -> Int -> Int -> Property
prop_parseStringLengthRangeConstraint typeNameStr minLen maxLen =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "LengthRange" else limitedTypeName
      minLength = max 0 (abs minLen `mod` 50)
      maxLength = max (minLength + 1) (minLength + (abs maxLen `mod` 50) + 1)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = string where { len(self) >= " ++ show minLength ++ " && len(self) <= " ++ show maxLength ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 4. 数组和切片约束测试
-- ============================================================================

-- | 测试数组大小约束
prop_parseArraySizeConstraint :: String -> Int -> Property
prop_parseArraySizeConstraint typeNameStr size =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "FixedArray" else limitedTypeName
      arraySize = max 1 (abs size `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = [" ++ show arraySize ++ "]int where { len(self) == " ++ show arraySize ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试切片长度约束
prop_parseSliceLengthConstraint :: String -> Int -> Property
prop_parseSliceLengthConstraint typeNameStr maxSize =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "BoundedSlice" else limitedTypeName
      maximumSize = max 1 (abs maxSize `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = []int where { len(self) <= " ++ show maximumSize ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试有效索引约束
prop_parseValidIndexConstraint :: String -> Int -> Property
prop_parseValidIndexConstraint typeNameStr size =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "ValidIndex" else limitedTypeName
      arraySize = max 1 (abs size `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self >= 0 && self < " ++ show arraySize ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 5. 复合约束测试
-- ============================================================================

-- | 测试多个条件约束
prop_parseMultipleConditionsConstraint :: String -> Int -> Property
prop_parseMultipleConditionsConstraint typeNameStr value =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "MultiCond" else limitedTypeName
      val = max 1 (abs value `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self > 0 && self < " ++ show (val * 2) ++ " && self % 2 == 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试嵌套约束
prop_parseNestedConstraint :: String -> Property
prop_parseNestedConstraint typeNameStr =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Nested" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self > 0 && (self < 100 || self > 200) }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试函数调用约束
prop_parseFunctionCallConstraint :: String -> Property
prop_parseFunctionCallConstraint typeNameStr =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "FuncCall" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { isPrime(self) }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 6. 参数化约束类型测试
-- ============================================================================

-- | 测试参数化范围约束
prop_parseParameterizedRangeConstraint :: String -> Int -> Int -> Property
prop_parseParameterizedRangeConstraint typeNameStr lo hi =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Range" else limitedTypeName
      low = max 0 (abs lo `mod` 100)
      high = max (low + 1) (low + (abs hi `mod` 50) + 1)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ "[" ++ show low ++ ": int, " ++ show high ++ ": int] = int where { self >= " ++ show low ++ " && self <= " ++ show high ++ " }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试百分比约束
prop_parsePercentageConstraint :: String -> Property
prop_parsePercentageConstraint typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Percentage" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype Bounded[0: int, 100: int] = int where { self >= 0 && self <= 100 }\ntype " ++ validTypeName ++ " = Bounded[0, 100]"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试动态边界约束
prop_parseDynamicBoundaryConstraint :: String -> Int -> Property
prop_parseDynamicBoundaryConstraint typeNameStr maxVal =
  let limitedTypeName = take 8 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Dynamic" else limitedTypeName
      maxValue = max 1 (abs maxVal `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ "[max: int] = int where { self >= 0 && self <= max }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 7. 约束验证测试
-- ============================================================================

-- | 测试约束验证函数
prop_parseConstraintValidation :: String -> Property
prop_parseConstraintValidation funcNameStr =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "validate" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype Positive = int where { self > 0 }\n\nfunc " ++ validFuncName ++ "(x: int) -> Positive { assert x > 0; return x }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- | 测试约束错误处理
prop_parseConstraintErrorHandling :: String -> Property
prop_parseConstraintErrorHandling funcNameStr =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "safeOp" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype NonZero = int where { self != 0 }\n\nfunc " ++ validFuncName ++ "(a: int, b: int) -> int { assert b != 0; return a / b }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- | 测试约束传播
prop_parseConstraintPropagation :: String -> Property
prop_parseConstraintPropagation funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "propagate" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype Positive = int where { self > 0 }\ntype NonZero = int where { self != 0 }\n\nfunc " ++ validFuncName ++ "(x: Positive) -> NonZero { return x }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- ============================================================================
-- 8. 约束求解器测试
-- ============================================================================

-- | 测试常量求值
prop_parseConstantEvaluation :: String -> Int -> Property
prop_parseConstantEvaluation funcNameStr value =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "constEval" else limitedFuncName
      val = max 0 (abs value `mod` 100)
      code = "package main\n\n//! dependent_types: on\ntype ValidIndex = int where { self >= 0 && self < " ++ show val ++ " }\n\nfunc " ++ validFuncName ++ "() { idx := 2; assert idx < " ++ show val ++ " }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- | 测试线性整数算术
prop_parseLinearArithmetic :: String -> Int -> Int -> Property
prop_parseLinearArithmetic funcNameStr a b =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "linArith" else limitedFuncName
      valA = max 0 (abs a `mod` 50)
      valB = max 0 (abs b `mod` 50)
      sum = valA + valB
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show sum ++ ": int] struct { data [" ++ show sum ++ "]float64 }\n\nfunc " ++ validFuncName ++ "() { v1: Vector[" ++ show valA ++ "]; v2: Vector[" ++ show valB ++ "]; v3 := concat(v1, v2) }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- | 测试等式传播
prop_parseEqualityPropagation :: String -> Property
prop_parseEqualityPropagation funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "eqProp" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype Vector[n: int] struct { data [n]float64 }\n\nfunc " ++ validFuncName ++ "(n: int, m: int) where { n == m } { v1: Vector[n]; v2: Vector[m]; v2 = v1 }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- | 测试不等式链
prop_parseInequalityChain :: String -> Property
prop_parseInequalityChain funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "ineqChain" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype Positive = int where { self > 0 }\ntype GreaterThanOne = int where { self > 1 }\n\nfunc " ++ validFuncName ++ "(x: GreaterThanOne) -> Positive { return x }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- ============================================================================
-- 测试套件组装
-- ============================================================================

-- | 基本约束类型测试套件
basicConstraintTests :: TestTree
basicConstraintTests = testGroup "基本约束类型测试"
  [ testProperty "非零整数约束" prop_parseNonZeroConstraint
  , testProperty "正数约束" prop_parsePositiveConstraint
  , testProperty "负数约束" prop_parseNegativeConstraint
  , testProperty "非负数约束" prop_parseNonNegativeConstraint
  ]

-- | 范围约束测试套件
rangeConstraintTests :: TestTree
rangeConstraintTests = testGroup "范围约束测试"
  [ testProperty "基本范围约束" prop_parseRangeConstraint
  , testProperty "开区间约束" prop_parseOpenRangeConstraint
  , testProperty "半开区间约束" prop_parseHalfOpenRangeConstraint
  ]

-- | 字符串约束测试套件
stringConstraintTests :: TestTree
stringConstraintTests = testGroup "字符串约束测试"
  [ testProperty "非空字符串约束" prop_parseNonEmptyStringConstraint
  , testProperty "字符串长度约束" prop_parseStringLengthConstraint
  , testProperty "字符串范围长度约束" prop_parseStringLengthRangeConstraint
  ]

-- | 数组和切片约束测试套件
arraySliceConstraintTests :: TestTree
arraySliceConstraintTests = testGroup "数组和切片约束测试"
  [ testProperty "数组大小约束" prop_parseArraySizeConstraint
  , testProperty "切片长度约束" prop_parseSliceLengthConstraint
  , testProperty "有效索引约束" prop_parseValidIndexConstraint
  ]

-- | 复合约束测试套件
compoundConstraintTests :: TestTree
compoundConstraintTests = testGroup "复合约束测试"
  [ testProperty "多个条件约束" prop_parseMultipleConditionsConstraint
  , testProperty "嵌套约束" prop_parseNestedConstraint
  , testProperty "函数调用约束" prop_parseFunctionCallConstraint
  ]

-- | 参数化约束类型测试套件
parameterizedConstraintTests :: TestTree
parameterizedConstraintTests = testGroup "参数化约束类型测试"
  [ testProperty "参数化范围约束" prop_parseParameterizedRangeConstraint
  , testProperty "百分比约束" prop_parsePercentageConstraint
  , testProperty "动态边界约束" prop_parseDynamicBoundaryConstraint
  ]

-- | 约束验证测试套件
constraintValidationTests :: TestTree
constraintValidationTests = testGroup "约束验证测试"
  [ testProperty "约束验证函数" prop_parseConstraintValidation
  , testProperty "约束错误处理" prop_parseConstraintErrorHandling
  , testProperty "约束传播" prop_parseConstraintPropagation
  ]

-- | 约束求解器测试套件
constraintSolverTests :: TestTree
constraintSolverTests = testGroup "约束求解器测试"
  [ testProperty "常量求值" prop_parseConstantEvaluation
  , testProperty "线性整数算术" prop_parseLinearArithmetic
  , testProperty "等式传播" prop_parseEqualityPropagation
  , testProperty "不等式链" prop_parseInequalityChain
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "新精确类型和约束QuickCheck测试套件"
  [ basicConstraintTests
  , rangeConstraintTests
  , stringConstraintTests
  , arraySliceConstraintTests
  , compoundConstraintTests
  , parameterizedConstraintTests
  , constraintValidationTests
  , constraintSolverTests
  ]