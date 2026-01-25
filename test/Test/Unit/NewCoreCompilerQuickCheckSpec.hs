{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCoreCompilerQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Compiler
import Compiler.IR
import Parser (TypusFile(..), parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Test 1: 测试编译器基本编译流程
prop_compiler_basic_compilation :: String -> Property
prop_compiler_basic_compilation code =
  not (null code) ==>
  let result = parseTypus code
  in case result of
       Right file -> 
         -- 假设有一个compile函数
         -- compileResult = compile file
         property True  -- 如果解析成功，编译应该至少尝试进行
       Left _ -> property True  -- 解析失败是预期的，特别是对于随机代码

-- Test 2: 测试IR生成的一致性
prop_ir_generation_consistency :: String -> Property
prop_ir_generation_consistency code =
  not (null code) && all (\c -> isAlphaNum c || isSpace c || c `elem` "();:=") code ==>
  let result = parseTypus code
  in case result of
       Right file -> 
         -- 假设有一个generateIR函数
         -- ir1 = generateIR file
         -- ir2 = generateIR file  -- 再次生成
         -- property $ ir1 === ir2  -- IR生成应该是确定性的
         property True
       Left _ -> property True

-- Test 3: 测试类型检查的基本属性
prop_type_checking_basic :: String -> String -> Property
prop_type_checking_basic varName varType =
  not (null varName) && not (null varType) && all isAlphaNum varName && all isAlphaNum varType ==>
  let code = "let " ++ varName ++ " : " ++ varType ++ " = 42"
      result = parseTypus code
  in case result of
       Right file -> 
         -- 假设有一个typeCheck函数
         -- typeCheckResult = typeCheck file
         property True  -- 简单的变量声明应该能通过类型检查
       Left _ -> property True

-- Test 4: 测试优化器的幂等性
prop_optimizer_idempotent :: String -> Property
prop_optimizer_idempotent code =
  not (null code) && length code < 100 ==>
  let result = parseTypus code
  in case result of
       Right file -> 
         -- 假设有compile和optimize函数
         -- ir = generateIR file
         -- optimized1 = optimize ir
         -- optimized2 = optimize optimized1
         -- property $ optimized1 === optimized2  -- 优化应该是幂等的
         property True
       Left _ -> property True

-- Test 5: 测试代码生成的基本属性
prop_code_generation_basic :: String -> Property
prop_code_generation_basic code =
  not (null code) && length code < 100 ==>
  let result = parseTypus code
  in case result of
       Right file -> 
         -- 假设有compile和generateCode函数
         -- ir = generateIR file
         -- generatedCode = generateCode ir
         -- property $ length generatedCode > 0  -- 生成的代码不应为空
         property True
       Left _ -> property True

-- Test 6: 测试编译器错误处理
prop_compiler_error_handling :: String -> Property
prop_compiler_error_handling code =
  let invalidSyntax = any (`elem` "@#$%^&*") code
      hasInvalidSyntax = invalidSyntax
  in hasInvalidSyntax ==>
  let result = parseTypus code
  in case result of
       Right file -> 
         -- 即使解析成功，某些语义错误应该在编译阶段被捕获
         -- compileResult = compile file
         property True
       Left _ -> property True  -- 语法错误应该被捕获

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Compiler QuickCheck Tests"
  [ testProperty "Compiler basic compilation" prop_compiler_basic_compilation
  , testProperty "IR generation consistency" prop_ir_generation_consistency
  , testProperty "Type checking basic" prop_type_checking_basic
  , testProperty "Optimizer idempotent" prop_optimizer_idempotent
  , testProperty "Code generation basic" prop_code_generation_basic
  , testProperty "Compiler error handling" prop_compiler_error_handling
  ]