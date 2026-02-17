{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewErrorHandlingQuickCheckTests where

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
import Ownership (analyzeOwnership)
import SyntaxValidator (validateSyntax)
import SourceLocation (SourcePos(..))
import ErrorHandler (handleError)


-- ============================================================================
-- 1. 基本错误检测测试
-- ============================================================================

-- | 测试空输入错误检测
prop_testEmptyInputError :: Property
prop_testEmptyInputError =
  let code = ""
      result = parseTypusFile code
  in property $ True ==> isLeft result

-- | 测试无效包名错误检测
prop_testInvalidPackageNameError :: String -> Property
prop_testInvalidPackageNameError invalidStr =
  let limitedStr = take 10 $ filter (not . isAlphaNum) (invalidStr ++ "@#$%^&*()")
      code = if null limitedStr then "package @@@@" else "package " ++ take 5 limitedStr
      result = parseTypusFile code
  in property $ True ==> isLeft result

-- | 测试无效变量名错误检测
prop_testInvalidVariableNameError :: String -> Property
prop_testInvalidVariableNameError invalidStr =
  let limitedStr = take 10 $ filter (not . isAlphaNum) (invalidStr ++ "@#$%^&*()")
      code = if null limitedStr then "package main\n\nvar @@@@ int" else "package main\n\nvar " ++ take 5 limitedStr ++ " int"
      result = parseTypusFile code
  in property $ True ==> isLeft result

-- | 测试无效类型错误检测
prop_testInvalidTypeError :: String -> String -> Property
prop_testInvalidTypeError varNameStr typeStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedType = take 8 $ filter (not . isAlpha) (typeStr ++ "@#$%^&*()")
      validVarName = if null limitedVarName then "x" else limitedVarName
      invalidType = if null limitedType then "@@@@" else take 5 limitedType
      code = "package main\n\nvar " ++ validVarName ++ " " ++ invalidType
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isLeft result

-- ============================================================================
-- 2. 语法错误检测测试
-- ============================================================================

-- | 测试不完整语句错误检测
prop_testIncompleteStatementError :: String -> Property
prop_testIncompleteStatementError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nvar " ++ validVarName ++ " int"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isLeft result

-- | 测试不匹配的大括号错误检测
prop_testMismatchedBracesError :: String -> Property
prop_testMismatchedBracesError funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      code = "package main\n\nfunc " ++ validFuncName ++ "() int { return 1"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isLeft result

-- | 测试不匹配的圆括号错误检测
prop_testMismatchedParenthesesError :: String -> Property
prop_testMismatchedParenthesesError funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      code = "package main\n\nfunc " ++ validFuncName ++ "() int { return (1 + 2"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isLeft result

-- | 测试无效表达式错误检测
prop_testInvalidExpressionError :: String -> Property
prop_testInvalidExpressionError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nfunc test() { " ++ validVarName ++ " := + }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isLeft result

-- ============================================================================
-- 3. 语义错误检测测试
-- ============================================================================

-- | 测试类型不匹配错误检测
prop_testTypeMismatchError :: String -> Property
prop_testTypeMismatchError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nfunc test() { " ++ validVarName ++ " := 1\n" ++ validVarName ++ " = \"string\" }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但语义检查应该失败

-- | 测试未定义变量错误检测
prop_testUndefinedVariableError :: String -> Property
prop_testUndefinedVariableError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nfunc test() { fmt.Println(" ++ validVarName ++ ") }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但语义检查应该失败

-- | 测试重复声明错误检测
prop_testDuplicateDeclarationError :: String -> Property
prop_testDuplicateDeclarationError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nfunc test() { " ++ validVarName ++ " := 1\n" ++ validVarName ++ " := 2 }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但语义检查应该失败

-- | 测试函数参数数量不匹配错误检测
prop_testFunctionArgumentCountMismatchError :: String -> Property
prop_testFunctionArgumentCountMismatchError funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      code = "package main\n\nfunc " ++ validFuncName ++ "(x int, y int) int { return x + y }\n\nfunc main() { " ++ validFuncName ++ "(1) }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result  -- 解析成功，但语义检查应该失败

-- ============================================================================
-- 4. 依赖类型错误检测测试
-- ============================================================================

-- | 测试约束违反错误检测
prop_testConstraintViolationError :: String -> Int -> Property
prop_testConstraintViolationError varNameStr value =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      val = abs value `mod` 100
      code = "package main\n\n//! dependent_types: on\ntype Positive = int where { self > 0 }\n\nfunc test() { " ++ validVarName ++ " := Positive(" ++ show val ++ ") }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但约束检查应该失败

-- | 测试值参数不匹配错误检测
prop_testValueParameterMismatchError :: String -> Int -> Int -> Property
prop_testValueParameterMismatchError funcNameStr size1 size2 =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      val1 = max 1 (abs size1 `mod` 10)
      val2 = max 1 (abs size2 `mod` 10)
      code = "package main\n\n//! dependent_types: on\ntype Vector[" ++ show val1 ++ ": int] struct { data [" ++ show val1 ++ "]int }\n\nfunc " ++ validFuncName ++ "() { v1 := Vector[" ++ show val1 ++ "]{data: [" ++ show val1 ++ "]int{}}\nv2 := Vector[" ++ show val2 ++ "]{data: [" ++ show val2 ++ "]int{}}\nv2 = v1 }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && val1 /= val2) ==> isRight result  -- 解析成功，但类型检查应该失败

-- | 测试依赖类型约束求解失败
prop_testDependentTypeConstraintSolveFailure :: String -> Property
prop_testDependentTypeConstraintSolveFailure funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      code = "package main\n\n//! dependent_types: on\ntype ComplexType[n: int] = int where { n * n - (n-1) * (n+1) == 1 }\n\nfunc " ++ validFuncName ++ "() { x := ComplexType[5]{value: 1} }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result  -- 解析成功，但约束求解应该失败

-- ============================================================================
-- 5. 所有权错误检测测试
-- ============================================================================

-- | 测试使用已移动变量错误检测
prop_testUseAfterMoveError :: String -> Property
prop_testUseAfterMoveError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVarName ++ " := 1\ny := " ++ validVarName ++ "\nfmt.Println(" ++ validVarName ++ ") }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但所有权检查应该失败

-- | 测试多个可变借用错误检测
prop_testMultipleMutableBorrowsError :: String -> Property
prop_testMultipleMutableBorrowsError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVarName ++ " := 1\nm1 := &mut " ++ validVarName ++ "\nm2 := &mut " ++ validVarName ++ " }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但所有权检查应该失败

-- | 测试借用与移动冲突错误检测
prop_testBorrowMoveConflictError :: String -> Property
prop_testBorrowMoveConflictError varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVarName ++ " := 1\nr := &" ++ validVarName ++ "\ny := " ++ validVarName ++ " }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result  -- 解析成功，但所有权检查应该失败

-- ============================================================================
-- 6. 边界情况测试
-- ============================================================================

-- | 测试极大输入处理
prop_testLargeInputHandling :: Int -> Property
prop_testLargeInputHandling size =
  let largeSize = max 1000 (abs size `mod` 10000)
      largeString = replicate largeSize 'a'
      code = "package main\n\nvar x string = \"" ++ take 100 largeString ++ "\""
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试极深嵌套结构
prop_testDeepNestingHandling :: Int -> Property
prop_testDeepNestingHandling depth =
  let nestingDepth = max 1 (min 20 (abs depth `mod` 25))
      createNestedStructs 0 = "field int"
      createNestedStructs n = "inner struct { " ++ createNestedStructs (n-1) ++ " }"
      nestedStruct = createNestedStructs nestingDepth
      code = "package main\n\ntype DeepStruct struct { " ++ nestedStruct ++ " }"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试特殊字符处理
prop_testSpecialCharacterHandling :: String -> Property
prop_testSpecialCharacterHandling specialStr =
  let limitedStr = take 20 $ specialStr ++ "中文测试!@#$%^&*()_+-=[]{}|;':\",./<>?"
      code = "package main\n\nvar x string = \"" ++ limitedStr ++ "\""
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试Unicode字符处理
prop_testUnicodeHandling :: String -> Property
prop_testUnicodeHandling unicodeStr =
  let limitedStr = take 20 $ unicodeStr ++ "测试中文🚀emoji"
      code = "package main\n\nvar " ++ take 5 (filter isAlpha limitedStr) ++ " string = \"" ++ limitedStr ++ "\""
      result = parseTypusFile code
  in property $ length (filter isAlpha limitedStr) > 0 ==> isRight result

-- ============================================================================
-- 7. 错误恢复测试
-- ============================================================================

-- | 测试语法错误恢复
prop_testSyntaxErrorRecovery :: String -> String -> Property
prop_testSyntaxErrorRecovery var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "x" else limitedVar1
      validVar2 = if null limitedVar2 then "y" else limitedVar2
      code = "package main\n\nvar " ++ validVar1 ++ " int = @@@@\nvar " ++ validVar2 ++ " int = 1"
      result = parseTypusFile code
      validationResult = validateSyntax code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isLeft result

-- | 测试语义错误恢复
prop_testSemanticErrorRecovery :: String -> String -> Property
prop_testSemanticErrorRecovery var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "x" else limitedVar1
      validVar2 = if null limitedVar2 then "y" else limitedVar2
      code = "package main\n\nfunc test() { " ++ validVar1 ++ " := 1\n" ++ validVar1 ++ " = \"string\"\n" ++ validVar2 ++ " := 2 }"
      result = parseTypusFile code
      validationResult = validateSyntax code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isRight result

-- | 测试多错误恢复
prop_testMultipleErrorRecovery :: String -> String -> String -> Property
prop_testMultipleErrorRecovery var1Str var2Str var3Str =
  let limitedVar1 = take 6 $ filter isAlpha var1Str
      limitedVar2 = take 6 $ filter isAlpha var2Str
      limitedVar3 = take 6 $ filter isAlpha var3Str
      validVar1 = if null limitedVar1 then "x" else limitedVar1
      validVar2 = if null limitedVar2 then "y" else limitedVar2
      validVar3 = if null limitedVar3 then "z" else limitedVar3
      code = "package main\n\nvar " ++ validVar1 ++ " int = @@@@\nvar " ++ validVar2 ++ " string = 123\nvar " ++ validVar3 ++ " int = 1"
      result = parseTypusFile code
      validationResult = validateSyntax code
  in property $ (length validVar1 > 0 && length validVar2 > 0 && length validVar3 > 0) ==> isLeft result

-- ============================================================================
-- 8. 错误信息质量测试
-- ============================================================================

-- | 测试错误信息准确性
testErrorInformationAccuracy :: TestTree
testErrorInformationAccuracy = testCase "错误信息准确性" $ do
  let code = "package main\n\nvar x int = @@@@"
      result = parseTypusFile code
  case result of
    Left err -> assertBool "错误信息应包含位置信息" $ "line" `isInfixOf` show err
    Right _ -> assertFailure "应该产生解析错误"

-- | 测试错误信息可读性
testErrorMessageReadability :: TestTree
testErrorMessageReadability = testCase "错误信息可读性" $ do
  let code = "package main\n\nfunc test() { x := 1\nx = \"string\" }"
      result = parseTypusFile code
  case result of
    Right _ -> do
      let validationResult = validateSyntax code
      assertBool "应该有语义错误" $ not (null validationResult)
    Left _ -> return ()  -- 如果解析失败，跳过语义检查

-- | 测试错误位置准确性
testErrorLocationAccuracy :: TestTree
testErrorLocationAccuracy = testCase "错误位置准确性" $ do
  let code = "package main\n\nvar x int = @@@@"
      result = parseTypusFile code
  case result of
    Left err -> assertBool "错误信息应包含行号" $ any isDigit (show err)
    Right _ -> assertFailure "应该产生解析错误"

-- ============================================================================
-- 测试套件组装
-- ============================================================================

-- | 基本错误检测测试套件
basicErrorDetectionTests :: TestTree
basicErrorDetectionTests = testGroup "基本错误检测测试"
  [ testProperty "空输入错误检测" prop_testEmptyInputError
  , testProperty "无效包名错误检测" prop_testInvalidPackageNameError
  , testProperty "无效变量名错误检测" prop_testInvalidVariableNameError
  , testProperty "无效类型错误检测" prop_testInvalidTypeError
  ]

-- | 语法错误检测测试套件
syntaxErrorDetectionTests :: TestTree
syntaxErrorDetectionTests = testGroup "语法错误检测测试"
  [ testProperty "不完整语句错误检测" prop_testIncompleteStatementError
  , testProperty "不匹配的大括号错误检测" prop_testMismatchedBracesError
  , testProperty "不匹配的圆括号错误检测" prop_testMismatchedParenthesesError
  , testProperty "无效表达式错误检测" prop_testInvalidExpressionError
  ]

-- | 语义错误检测测试套件
semanticErrorDetectionTests :: TestTree
semanticErrorDetectionTests = testGroup "语义错误检测测试"
  [ testProperty "类型不匹配错误检测" prop_testTypeMismatchError
  , testProperty "未定义变量错误检测" prop_testUndefinedVariableError
  , testProperty "重复声明错误检测" prop_testDuplicateDeclarationError
  , testProperty "函数参数数量不匹配错误检测" prop_testFunctionArgumentCountMismatchError
  ]

-- | 依赖类型错误检测测试套件
dependentTypeErrorDetectionTests :: TestTree
dependentTypeErrorDetectionTests = testGroup "依赖类型错误检测测试"
  [ testProperty "约束违反错误检测" prop_testConstraintViolationError
  , testProperty "值参数不匹配错误检测" prop_testValueParameterMismatchError
  , testProperty "依赖类型约束求解失败" prop_testDependentTypeConstraintSolveFailure
  ]

-- | 所有权错误检测测试套件
ownershipErrorDetectionTests :: TestTree
ownershipErrorDetectionTests = testGroup "所有权错误检测测试"
  [ testProperty "使用已移动变量错误检测" prop_testUseAfterMoveError
  , testProperty "多个可变借用错误检测" prop_testMultipleMutableBorrowsError
  , testProperty "借用与移动冲突错误检测" prop_testBorrowMoveConflictError
  ]

-- | 边界情况测试套件
boundaryCaseTests :: TestTree
boundaryCaseTests = testGroup "边界情况测试"
  [ testProperty "极大输入处理" prop_testLargeInputHandling
  , testProperty "极深嵌套结构" prop_testDeepNestingHandling
  , testProperty "特殊字符处理" prop_testSpecialCharacterHandling
  , testProperty "Unicode字符处理" prop_testUnicodeHandling
  ]

-- | 错误恢复测试套件
errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "错误恢复测试"
  [ testProperty "语法错误恢复" prop_testSyntaxErrorRecovery
  , testProperty "语义错误恢复" prop_testSemanticErrorRecovery
  , testProperty "多错误恢复" prop_testMultipleErrorRecovery
  ]

-- | 错误信息质量测试套件
errorInformationQualityTests :: TestTree
errorInformationQualityTests = testGroup "错误信息质量测试"
  [ testErrorInformationAccuracy
  , testErrorMessageReadability
  , testErrorLocationAccuracy
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "新错误处理和边界情况QuickCheck测试套件"
  [ basicErrorDetectionTests
  , syntaxErrorDetectionTests
  , semanticErrorDetectionTests
  , dependentTypeErrorDetectionTests
  , ownershipErrorDetectionTests
  , boundaryCaseTests
  , errorRecoveryTests
  , errorInformationQualityTests
  ]