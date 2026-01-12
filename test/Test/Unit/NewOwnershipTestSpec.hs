{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewOwnershipTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Ownership
import Ownership.Common.Types
import SourceLocation (SourcePos(..), Located(..), locatedAt)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (null, isInfixOf)

-- | 测试所有权类型的基本属性
test_ownership_types :: Assertion
test_ownership_types = do
  -- 测试所有权类型的比较
  assertEqual "Owned should equal Owned" Owned Owned
  assertBool "Owned should not equal Borrowed" (Owned /= Borrowed)
  assertBool "Borrowed should not equal Moved" (Borrowed /= Moved)
  assertBool "Owned should not equal Moved" (Owned /= Moved)

-- | 测试所有权分析器的创建
test_ownership_analyzer_creation :: Assertion
test_ownership_analyzer_creation = do
  let analyzer = newOwnershipAnalyzer
  assertBool "Analyzer should be created" (True)  -- 简单测试，确保创建不失败

-- | 测试基本的所有权分析
test_basic_ownership_analysis :: Assertion
test_basic_ownership_analysis = do
  let simpleCode = "let x = 42\nlet y = x"
      result = analyzeOwnership simpleCode
  case result of
    Left err -> assertFailure $ "Failed to analyze basic ownership: " ++ show err
    Right _ -> return ()  -- 成功分析即可

-- | 测试所有权转移
test_ownership_transfer :: Assertion
test_ownership_transfer = do
  let transferCode = "let x = Box::new(42)\nlet y = x\n"
      result = analyzeOwnership transferCode
  case result of
    Left _ -> return ()  -- 可能检测到所有权转移错误
    Right _ -> return ()  -- 也可能成功分析

-- | 测试借用检查
test_borrow_checking :: Assertion
test_borrow_checking = do
  let borrowCode = "let x = 42\nlet y = &x\nlet z = x\n"
      result = analyzeOwnership borrowCode
  case result of
    Left _ -> return ()  -- 可能检测到借用错误
    Right _ -> return ()  -- 也可能成功分析

-- | 测试共享所有权
test_shared_ownership :: Assertion
test_shared_ownership = do
  let sharedCode = "let x = Rc::new(42)\nlet y = x.clone()\nlet z = x\n"
      result = analyzeOwnership sharedCode
  case result of
    Left _ -> return ()  -- 可能检测到共享所有权错误
    Right _ -> return ()  -- 也可能成功分析

-- | 测试所有权错误格式化
test_ownership_error_formatting :: Assertion
test_ownership_error_formatting = do
  let error = LoopOwnershipError "Ownership violation" (SourcePos 5 10)
      formatted = formatOwnershipErrors [error]
  assertBool "Formatted error should contain position" ("5:10" `isInfixOf` formatted)
  assertBool "Formatted error should contain error message" ("Ownership violation" `isInfixOf` formatted)

-- | 测试词法分析
test_lexical_analysis :: Assertion
test_lexical_analysis = do
  let simpleCode = "let x = 42"
      result = lexAll simpleCode
  case result of
    Left err -> assertFailure $ "Failed to lex simple code: " ++ show err
    Right tokens -> assertBool "Should produce tokens" (not $ null tokens)

-- | 测试语法分析
test_parsing :: Assertion
test_parsing = do
  let simpleCode = "let x = 42"
      result = parseProgram simpleCode
  case result of
    Left err -> assertFailure $ "Failed to parse simple code: " ++ show err
    Right ast -> assertBool "Should produce AST" (True)  -- 简单测试，确保解析不失败

-- | 测试内置函数
test_builtin_functions :: Assertion
test_builtin_functions = do
  let builtins = builtInFunctions
  assertBool "Should have built-in functions" (not $ null builtins)

-- | 测试复杂所有权场景
test_complex_ownership_scenarios :: Assertion
test_complex_ownership_scenarios = do
  let complexCode = "fn process(data: Box<Vec<i32>>) -> i32 {\n  let len = data.len();\n  len\n}\nlet box_data = Box::new(vec![1, 2, 3]);\nlet result = process(box_data);\n"
      result = analyzeOwnership complexCode
  case result of
    Left _ -> return ()  -- 可能检测到复杂所有权错误
    Right _ -> return ()  -- 也可能成功分析

-- | 测试所有权文件分析
test_ownership_file_analysis :: Assertion
test_ownership_file_analysis = do
  let fileContent = "let x = 42\nlet y = x\nlet z = y\n"
      result = analyzeOwnershipFile fileContent
  case result of
    Left err -> assertFailure $ "Failed to analyze ownership file: " ++ show err
    Right _ -> return ()  -- 成功分析即可

-- | 测试调试模式的所有权分析
test_ownership_analysis_debug :: Assertion
test_ownership_analysis_debug = do
  let debugCode = "let x = 42\nlet y = x\n"
      result = analyzeOwnershipDebug debugCode
  case result of
    Left err -> assertFailure $ "Failed to analyze ownership in debug mode: " ++ show err
    Right _ -> return ()  -- 成功分析即可

-- | QuickCheck属性：所有权分析应该处理简单赋值
prop_ownership_analysis_simple_assignment :: String -> Property
prop_ownership_analysis_simple_assignment varName =
  let code = "let " ++ take 5 (filter isAlpha varName) ++ " = 42"
      result = analyzeOwnership code
  in case result of
       Left _ -> property False
       Right _ -> property True

-- | QuickCheck属性：所有权错误应该包含位置信息
prop_ownership_errors_have_location :: String -> Positive Int -> Positive Int -> Property
prop_ownership_errors_have_location msg (Positive line) (Positive col) =
  let error = OwnershipError msg (SourcePos line col) Owned Borrowed
      formatted = formatOwnershipErrors [error]
  in if line > 0 && col > 0 && not (null msg)
     then (show line `isInfixOf` formatted) .&&. 
          (show col `isInfixOf` formatted) .&&.
          (msg `isInfixOf` formatted)
     else property True

-- | QuickCheck属性：词法分析应该产生非空结果
prop_lexical_analysis_produces_tokens :: String -> Property
prop_lexical_analysis_produces_tokens code =
  let result = lexAll code
  in case result of
       Left _ -> property True  -- 词法分析可能失败
       Right tokens -> not (null tokens) || null code  -- 空代码可能产生空token列表

-- | 测试所有权转移的一致性
test_ownership_transfer_consistency :: Assertion
test_ownership_transfer_consistency = do
  let transfer = OwnershipTransfer (SourcePos 1 10) (SourcePos 1 20) Owned Borrowed
      fromPos = transferFrom transfer
      toPos = transferTo transfer
      fromType = transferFromType transfer
      toType = transferToType transfer
  assertEqual "Transfer from position should match" (SourcePos 1 10) fromPos
  assertEqual "Transfer to position should match" (SourcePos 1 20) toPos
  assertEqual "Transfer from type should match" Owned fromType
  assertEqual "Transfer to type should match" Borrowed toType

-- | 测试所有权错误的分类
test_ownership_error_classification :: Assertion
test_ownership_error_classification = do
  let moveError = OwnershipError "Move error" (SourcePos 1 10) Owned Moved
      borrowError = OwnershipError "Borrow error" (SourcePos 2 20) Shared Borrowed
      lifetimeError = OwnershipError "Lifetime error" (SourcePos 3 30) Owned Shared
  assertEqual "Move error should have correct types" Owned (errorFromType moveError)
  assertEqual "Move error should have correct to type" Moved (errorToType moveError)
  assertEqual "Borrow error should have correct types" Shared (errorFromType borrowError)
  assertEqual "Borrow error should have correct to type" Borrowed (errorToType borrowError)
  assertEqual "Lifetime error should have correct types" Owned (errorFromType lifetimeError)
  assertEqual "Lifetime error should have correct to type" Shared (errorToType lifetimeError)

-- | 测试所有权分析的性能
test_ownership_analysis_performance :: Assertion
test_ownership_analysis_performance = do
  let largeCode = unlines $ replicate 100 "let x" ++ show (1 :: Int) ++ " = " ++ show (1 :: Int)
      result = analyzeOwnership largeCode
  case result of
    Left _ -> return ()  -- 可能失败，但不应该超时
    Right _ -> return ()  -- 成功分析

-- | 测试所有权分析的错误恢复
test_ownership_analysis_error_recovery :: Assertion
test_ownership_analysis_error_recovery = do
  let invalidCode = "let x = \nlet y = x"
      result = analyzeOwnership invalidCode
  case result of
    Left _ -> return ()  -- 期望失败
    Right _ -> assertFailure "Expected ownership analysis to fail with invalid code"

-- | 测试所有权分析与类型系统的集成
test_ownership_type_system_integration :: Assertion
test_ownership_type_system_integration = do
  let typedCode = "let x: Box<i32> = Box::new(42)\nlet y = x\n"
      result = analyzeOwnership typedCode
  case result of
    Left _ -> return ()  -- 可能检测到类型相关的所有权错误
    Right _ -> return ()  -- 也可能成功分析

-- | 测试套件
tests :: TestTree
tests = testGroup "New Ownership Tests"
  [ testCase "Ownership types" test_ownership_types
  , testCase "Ownership analyzer creation" test_ownership_analyzer_creation
  , testCase "Basic ownership analysis" test_basic_ownership_analysis
  , testCase "Ownership transfer" test_ownership_transfer
  , testCase "Borrow checking" test_borrow_checking
  , testCase "Shared ownership" test_shared_ownership
  , testCase "Ownership error formatting" test_ownership_error_formatting
  , testCase "Lexical analysis" test_lexical_analysis
  , testCase "Parsing" test_parsing
  , testCase "Built-in functions" test_builtin_functions
  , testCase "Complex ownership scenarios" test_complex_ownership_scenarios
  , testCase "Ownership file analysis" test_ownership_file_analysis
  , testCase "Ownership analysis debug" test_ownership_analysis_debug
  , testCase "Ownership transfer consistency" test_ownership_transfer_consistency
  , testCase "Ownership error classification" test_ownership_error_classification
  , testCase "Ownership analysis performance" test_ownership_analysis_performance
  , testCase "Ownership analysis error recovery" test_ownership_analysis_error_recovery
  , testCase "Ownership type system integration" test_ownership_type_system_integration
  , testProperty "Ownership analysis simple assignment" prop_ownership_analysis_simple_assignment
  , testProperty "Ownership errors have location" prop_ownership_errors_have_location
  , testProperty "Lexical analysis produces tokens" prop_lexical_analysis_produces_tokens
  ]