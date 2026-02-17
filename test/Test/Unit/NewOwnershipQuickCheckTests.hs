{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewOwnershipQuickCheckTests where

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
import Ownership (analyzeOwnership)
import SyntaxValidator (validateSyntax)
import SourceLocation (SourcePos(..))

-- ============================================================================
-- 1. 基本所有权语义测试
-- ============================================================================

-- | 测试基本移动语义
prop_parseBasicMoveSemantics :: String -> Property
prop_parseBasicMoveSemantics varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试变量移动
prop_parseVariableMove :: String -> String -> Property
prop_parseVariableMove var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "s" else limitedVar1
      validVar2 = if null limitedVar2 then "t" else limitedVar2
      code = "package main\n\n//! ownership: on\nvar " ++ validVar1 ++ " int = 1\nvar " ++ validVar2 ++ " = " ++ validVar1
      result = parseTypusFile code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isRight result

-- | 测试移动后不可使用
prop_parseMovedVariableUsage :: String -> String -> Property
prop_parseMovedVariableUsage var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "s" else limitedVar1
      validVar2 = if null limitedVar2 then "t" else limitedVar2
      code = "package main\n\n//! ownership: on\nvar " ++ validVar1 ++ " int = 1\nvar " ++ validVar2 ++ " = " ++ validVar1 ++ "\nfmt.Println(" ++ validVar2 ++ ")"
      result = parseTypusFile code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isRight result

-- ============================================================================
-- 2. 借用测试
-- ============================================================================

-- | 测试不可变借用
prop_parseImmutableBorrow :: String -> String -> Property
prop_parseImmutableBorrow varNameStr borrowNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedBorrowName = take 8 $ filter isAlpha borrowNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrowName = if null limitedBorrowName then "r" else limitedBorrowName
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1\nvar " ++ validBorrowName ++ " = &" ++ validVarName
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrowName > 0) ==> isRight result

-- | 测试可变借用
prop_parseMutableBorrow :: String -> String -> Property
prop_parseMutableBorrow varNameStr borrowNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedBorrowName = take 8 $ filter isAlpha borrowNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrowName = if null limitedBorrowName then "m" else limitedBorrowName
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1\nvar " ++ validBorrowName ++ " = &mut " ++ validVarName
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrowName > 0) ==> isRight result

-- | 测试多个不可变借用
prop_parseMultipleImmutableBorrows :: String -> String -> String -> Property
prop_parseMultipleImmutableBorrows varNameStr borrow1Str borrow2Str =
  let limitedVarName = take 6 $ filter isAlpha varNameStr
      limitedBorrow1 = take 6 $ filter isAlpha borrow1Str
      limitedBorrow2 = take 6 $ filter isAlpha borrow2Str
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrow1 = if null limitedBorrow1 then "r1" else limitedBorrow1
      validBorrow2 = if null limitedBorrow2 then "r2" else limitedBorrow2
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1\nvar " ++ validBorrow1 ++ " = &" ++ validVarName ++ "\nvar " ++ validBorrow2 ++ " = &" ++ validVarName
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrow1 > 0 && length validBorrow2 > 0) ==> isRight result

-- ============================================================================
-- 3. 借用规则测试
-- ============================================================================

-- | 测试借用与移动互斥
prop_parseBorrowMoveExclusion :: String -> String -> String -> Property
prop_parseBorrowMoveExclusion varNameStr borrowNameStr moveNameStr =
  let limitedVarName = take 6 $ filter isAlpha varNameStr
      limitedBorrowName = take 6 $ filter isAlpha borrowNameStr
      limitedMoveName = take 6 $ filter isAlpha moveNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrowName = if null limitedBorrowName then "r" else limitedBorrowName
      validMoveName = if null limitedMoveName then "t" else limitedMoveName
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1\nvar " ++ validBorrowName ++ " = &" ++ validVarName ++ "\nfmt.Println(*" ++ validBorrowName ++ ")\nvar " ++ validMoveName ++ " = " ++ validVarName
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrowName > 0 && length validMoveName > 0) ==> isRight result

-- | 测试可变借用独占性
prop_parseMutableBorrowExclusivity :: String -> String -> String -> Property
prop_parseMutableBorrowExclusivity varNameStr borrow1Str borrow2Str =
  let limitedVarName = take 6 $ filter isAlpha varNameStr
      limitedBorrow1 = take 6 $ filter isAlpha borrow1Str
      limitedBorrow2 = take 6 $ filter isAlpha borrow2Str
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrow1 = if null limitedBorrow1 then "m1" else limitedBorrow1
      validBorrow2 = if null limitedBorrow2 then "m2" else limitedBorrow2
      code = "package main\n\n//! ownership: on\nvar " ++ validVarName ++ " int = 1\nvar " ++ validBorrow1 ++ " = &mut " ++ validVarName ++ "\n*" ++ validBorrow1 ++ " = 2"
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrow1 > 0 && length validBorrow2 > 0) ==> isRight result

-- | 测试借用生命周期
prop_parseBorrowLifetime :: String -> String -> Property
prop_parseBorrowLifetime varNameStr borrowNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedBorrowName = take 8 $ filter isAlpha borrowNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrowName = if null limitedBorrowName then "r" else limitedBorrowName
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVarName ++ " := 1\n" ++ validBorrowName ++ " := &" ++ validVarName ++ "\nfmt.Println(*" ++ validBorrowName ++ ") }"
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrowName > 0) ==> isRight result

-- ============================================================================
-- 4. 函数中的所有权测试
-- ============================================================================

-- | 测试函数参数移动
prop_parseFunctionParameterMove :: String -> String -> Property
prop_parseFunctionParameterMove funcNameStr paramStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedParam = take 8 $ filter isAlpha paramStr
      validFuncName = if null limitedFuncName then "consume" else limitedFuncName
      validParam = if null limitedParam then "x" else limitedParam
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "(" ++ validParam ++ " int) { fmt.Println(" ++ validParam ++ ") }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validParam > 0) ==> isRight result

-- | 测试函数参数借用
prop_parseFunctionParameterBorrow :: String -> String -> Property
prop_parseFunctionParameterBorrow funcNameStr paramStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedParam = take 8 $ filter isAlpha paramStr
      validFuncName = if null limitedFuncName then "read" else limitedFuncName
      validParam = if null limitedParam then "x" else limitedParam
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "(" ++ validParam ++ " &int) { fmt.Println(*" ++ validParam ++ ") }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validParam > 0) ==> isRight result

-- | 测试函数参数可变借用
prop_parseFunctionParameterMutableBorrow :: String -> String -> Property
prop_parseFunctionParameterMutableBorrow funcNameStr paramStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedParam = take 8 $ filter isAlpha paramStr
      validFuncName = if null limitedFuncName then "modify" else limitedFuncName
      validParam = if null limitedParam then "x" else limitedParam
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "(" ++ validParam ++ " &mut int) { *" ++ validParam ++ " = 2 }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validParam > 0) ==> isRight result

-- | 测试函数返回值移动
prop_parseFunctionReturnValueMove :: String -> Property
prop_parseFunctionReturnValueMove funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "create" else limitedFuncName
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "() int { return 1 }"
      result = parseTypusFile code
  in property $ length validFuncName > 0 ==> isRight result

-- ============================================================================
-- 5. 结构体中的所有权测试
-- ============================================================================

-- | 测试结构体字段移动
prop_parseStructFieldMove :: String -> String -> Property
prop_parseStructFieldMove structNameStr fieldNameStr =
  let limitedStructName = take 8 $ filter isAlpha structNameStr
      limitedFieldName = take 8 $ filter isAlpha fieldNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      validFieldName = if null limitedFieldName then "value" else limitedFieldName
      code = "package main\n\n//! ownership: on\ntype " ++ validStructName ++ " struct { " ++ validFieldName ++ " int }\n\nfunc process() { s := " ++ validStructName ++ "{" ++ validFieldName ++ ": 1}\nt := s }"
      result = parseTypusFile code
  in property $ (length validStructName > 0 && length validFieldName > 0) ==> isRight result

-- | 测试结构体字段借用
prop_parseStructFieldBorrow :: String -> String -> Property
prop_parseStructFieldBorrow structNameStr fieldNameStr =
  let limitedStructName = take 8 $ filter isAlpha structNameStr
      limitedFieldName = take 8 $ filter isAlpha fieldNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      validFieldName = if null limitedFieldName then "value" else limitedFieldName
      code = "package main\n\n//! ownership: on\ntype " ++ validStructName ++ " struct { " ++ validFieldName ++ " int }\n\nfunc process() { s := " ++ validStructName ++ "{" ++ validFieldName ++ ": 1}\nr := &s." ++ validFieldName ++ " }"
      result = parseTypusFile code
  in property $ (length validStructName > 0 && length validFieldName > 0) ==> isRight result

-- | 测试结构体字段可变借用
prop_parseStructFieldMutableBorrow :: String -> String -> Property
prop_parseStructFieldMutableBorrow structNameStr fieldNameStr =
  let limitedStructName = take 8 $ filter isAlpha structNameStr
      limitedFieldName = take 8 $ filter isAlpha fieldNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      validFieldName = if null limitedFieldName then "value" else limitedFieldName
      code = "package main\n\n//! ownership: on\ntype " ++ validStructName ++ " struct { " ++ validFieldName ++ " int }\n\nfunc process() { s := " ++ validStructName ++ "{" ++ validFieldName ++ ": 1}\nm := &mut s." ++ validFieldName ++ "\n*m = 2 }"
      result = parseTypusFile code
  in property $ (length validStructName > 0 && length validFieldName > 0) ==> isRight result

-- ============================================================================
-- 6. 切片和数组中的所有权测试
-- ============================================================================

-- | 测试切片元素移动
prop_parseSliceElementMove :: String -> Int -> Property
prop_parseSliceElementMove sliceNameStr index =
  let limitedSliceName = take 8 $ filter isAlpha sliceNameStr
      validSliceName = if null limitedSliceName then "slice" else limitedSliceName
      validIndex = max 0 (abs index `mod` 10)
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validSliceName ++ " := []int{1, 2, 3}\nelement := " ++ validSliceName ++ "[" ++ show validIndex ++ "] }"
      result = parseTypusFile code
  in property $ length validSliceName > 0 ==> isRight result

-- | 测试切片元素借用
prop_parseSliceElementBorrow :: String -> Int -> Property
prop_parseSliceElementBorrow sliceNameStr index =
  let limitedSliceName = take 8 $ filter isAlpha sliceNameStr
      validSliceName = if null limitedSliceName then "slice" else limitedSliceName
      validIndex = max 0 (abs index `mod` 10)
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validSliceName ++ " := []int{1, 2, 3}\nref := &" ++ validSliceName ++ "[" ++ show validIndex ++ "] }"
      result = parseTypusFile code
  in property $ length validSliceName > 0 ==> isRight result

-- | 测试切片元素可变借用
prop_parseSliceElementMutableBorrow :: String -> Int -> Property
prop_parseSliceElementMutableBorrow sliceNameStr index =
  let limitedSliceName = take 8 $ filter isAlpha sliceNameStr
      validSliceName = if null limitedSliceName then "slice" else limitedSliceName
      validIndex = max 0 (abs index `mod` 10)
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validSliceName ++ " := []int{1, 2, 3}\nmref := &mut " ++ validSliceName ++ "[" ++ show validIndex ++ "]\n*mref = 2 }"
      result = parseTypusFile code
  in property $ length validSliceName > 0 ==> isRight result

-- ============================================================================
-- 7. 所有权与控制流测试
-- ============================================================================

-- | 测试条件分支中的所有权
prop_parseOwnershipInConditional :: String -> String -> Property
prop_parseOwnershipInConditional varNameStr condVarStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedCondVar = take 8 $ filter isAlpha condVarStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validCondVar = if null limitedCondVar then "cond" else limitedCondVar
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validVarName ++ " := 1\n" ++ validCondVar ++ " := true\nif " ++ validCondVar ++ " { t := " ++ validVarName ++ "\nfmt.Println(t) } }"
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validCondVar > 0) ==> isRight result

-- | 测试循环中的所有权
prop_parseOwnershipInLoop :: String -> Property
prop_parseOwnershipInLoop varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validVarName ++ " := 1\nfor i := 0; i < 3; i++ { t := " ++ validVarName ++ "\nfmt.Println(t) } }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试函数调用中的所有权
prop_parseOwnershipInFunctionCall :: String -> String -> Property
prop_parseOwnershipInFunctionCall funcNameStr varNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedVarName = take 8 $ filter isAlpha varNameStr
      validFuncName = if null limitedFuncName then "consume" else limitedFuncName
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "(x int) { fmt.Println(x) }\n\nfunc process() { " ++ validVarName ++ " := 1\n" ++ validFuncName ++ "(" ++ validVarName ++ ") }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validVarName > 0) ==> isRight result

-- ============================================================================
-- 8. 高级所有权特性测试
-- ============================================================================

-- | 测试所有权转移与返回
prop_parseOwnershipTransferAndReturn :: String -> String -> Property
prop_parseOwnershipTransferAndReturn funcNameStr varNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedVarName = take 8 $ filter isAlpha varNameStr
      validFuncName = if null limitedFuncName then "transfer" else limitedFuncName
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc " ++ validFuncName ++ "(x int) int { return x }\n\nfunc process() { " ++ validVarName ++ " := 1\nt := " ++ validFuncName ++ "(" ++ validVarName ++ ")\nfmt.Println(t) }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validVarName > 0) ==> isRight result

-- | 测试部分移动
prop_parsePartialMove :: String -> String -> Property
prop_parsePartialMove structNameStr fieldNameStr =
  let limitedStructName = take 8 $ filter isAlpha structNameStr
      limitedFieldName = take 8 $ filter isAlpha fieldNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      validFieldName = if null limitedFieldName then "value" else limitedFieldName
      code = "package main\n\n//! ownership: on\ntype " ++ validStructName ++ " struct { " ++ validFieldName ++ " int\nother int }\n\nfunc process() { s := " ++ validStructName ++ "{" ++ validFieldName ++ ": 1, other: 2}\nt := s." ++ validFieldName ++ "\nfmt.Println(t)\nfmt.Println(s.other) }"
      result = parseTypusFile code
  in property $ (length validStructName > 0 && length validFieldName > 0) ==> isRight result

-- | 测试所有权与闭包
prop_parseOwnershipWithClosure :: String -> Property
prop_parseOwnershipWithClosure varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validVarName ++ " := 1\nf := func() { fmt.Println(" ++ validVarName ++ ") }\nf() }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试所有权与Goroutine
prop_parseOwnershipWithGoroutine :: String -> Property
prop_parseOwnershipWithGoroutine varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      code = "package main\n\n//! ownership: on\nfunc process() { " ++ validVarName ++ " := 1\ngo func() { fmt.Println(" ++ validVarName ++ ") }() }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- ============================================================================
-- 测试套件组装
-- ============================================================================

-- | 基本所有权语义测试套件
basicOwnershipTests :: TestTree
basicOwnershipTests = testGroup "基本所有权语义测试"
  [ testProperty "基本移动语义" prop_parseBasicMoveSemantics
  , testProperty "变量移动" prop_parseVariableMove
  , testProperty "移动后不可使用" prop_parseMovedVariableUsage
  ]

-- | 借用测试套件
borrowingTests :: TestTree
borrowingTests = testGroup "借用测试"
  [ testProperty "不可变借用" prop_parseImmutableBorrow
  , testProperty "可变借用" prop_parseMutableBorrow
  , testProperty "多个不可变借用" prop_parseMultipleImmutableBorrows
  ]

-- | 借用规则测试套件
borrowingRulesTests :: TestTree
borrowingRulesTests = testGroup "借用规则测试"
  [ testProperty "借用与移动互斥" prop_parseBorrowMoveExclusion
  , testProperty "可变借用独占性" prop_parseMutableBorrowExclusivity
  , testProperty "借用生命周期" prop_parseBorrowLifetime
  ]

-- | 函数中的所有权测试套件
functionOwnershipTests :: TestTree
functionOwnershipTests = testGroup "函数中的所有权测试"
  [ testProperty "函数参数移动" prop_parseFunctionParameterMove
  , testProperty "函数参数借用" prop_parseFunctionParameterBorrow
  , testProperty "函数参数可变借用" prop_parseFunctionParameterMutableBorrow
  , testProperty "函数返回值移动" prop_parseFunctionReturnValueMove
  ]

-- | 结构体中的所有权测试套件
structOwnershipTests :: TestTree
structOwnershipTests = testGroup "结构体中的所有权测试"
  [ testProperty "结构体字段移动" prop_parseStructFieldMove
  , testProperty "结构体字段借用" prop_parseStructFieldBorrow
  , testProperty "结构体字段可变借用" prop_parseStructFieldMutableBorrow
  ]

-- | 切片和数组中的所有权测试套件
sliceArrayOwnershipTests :: TestTree
sliceArrayOwnershipTests = testGroup "切片和数组中的所有权测试"
  [ testProperty "切片元素移动" prop_parseSliceElementMove
  , testProperty "切片元素借用" prop_parseSliceElementBorrow
  , testProperty "切片元素可变借用" prop_parseSliceElementMutableBorrow
  ]

-- | 所有权与控制流测试套件
controlFlowOwnershipTests :: TestTree
controlFlowOwnershipTests = testGroup "所有权与控制流测试"
  [ testProperty "条件分支中的所有权" prop_parseOwnershipInConditional
  , testProperty "循环中的所有权" prop_parseOwnershipInLoop
  , testProperty "函数调用中的所有权" prop_parseOwnershipInFunctionCall
  ]

-- | 高级所有权特性测试套件
advancedOwnershipTests :: TestTree
advancedOwnershipTests = testGroup "高级所有权特性测试"
  [ testProperty "所有权转移与返回" prop_parseOwnershipTransferAndReturn
  , testProperty "部分移动" prop_parsePartialMove
  , testProperty "所有权与闭包" prop_parseOwnershipWithClosure
  , testProperty "所有权与Goroutine" prop_parseOwnershipWithGoroutine
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "新所有权机制QuickCheck测试套件"
  [ basicOwnershipTests
  , borrowingTests
  , borrowingRulesTests
  , functionOwnershipTests
  , structOwnershipTests
  , sliceArrayOwnershipTests
  , controlFlowOwnershipTests
  , advancedOwnershipTests
  ]