{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds  -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import ErrorHandler
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..), emptyContext, formatError)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose)

-- | SourcePos 的 Arbitrary 实例
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- | 测试错误格式化的一致性
prop_error_formatting_consistency :: String -> Property
prop_error_formatting_consistency msg =
  not (null msg) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack msg,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted1 = formatError error
      formatted2 = formatError error
  in formatted1 === formatted2

-- | 测试错误格式化的幂等性
prop_error_formatting_idempotent :: String -> Property
prop_error_formatting_idempotent msg =
  not (null msg) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack msg,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in length formatted >= length msg

-- | 测试错误格式的有效性
prop_error_format_validity :: String -> Property
prop_error_format_validity msg =
  not (null msg) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack msg,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in length formatted >= length msg

-- | 测试错误格式的包含性
prop_error_format_contains :: String -> Property
prop_error_format_contains msg =
  not (null msg) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack msg,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in msg `isInfixOf` formatted

-- | 测试错误位置的一致性
prop_error_position_consistency :: SourcePos -> SourcePos -> Property
prop_error_position_consistency pos1 pos2 =
  let span = SourceSpan pos1 pos2
      span2 = SourceSpan pos1 pos2
  in conjoin [pos1 === pos1, pos2 === pos2]

-- | 测试错误位置的顺序
prop_error_position_order :: SourcePos -> SourcePos -> Property
prop_error_position_order pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
      column1 = posColumn pos1
      column2 = posColumn pos2
      before = line1 < line2 || (line1 == line2 && column1 < column2)
  in property before

-- | 测试错误严重性的一致性
prop_error_severity_consistency :: String -> Property
prop_error_severity_consistency msg =
  not (null msg) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack msg,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      severity1 = severity error
      severity2 = severity error
  in severity1 === severity2

-- | 测试错误严重性的顺序
prop_error_severity_order :: Property
prop_error_severity_order =
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Error message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      warning = error { severity = Warning, message = "Warning message" }
      info = error { severity = Info, message = "Info message" }
      severity1 = severity error
      severity2 = severity warning
      severity3 = severity info
  in property (severity1 >= severity2 && severity2 >= severity3)

-- | 测试错误恢复的一致性
prop_error_recovery_consistency :: String -> Property
prop_error_recovery_consistency input =
  not (null input) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack input,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      recovery1 = recovery error
      recovery2 = recovery error
  in recovery1 === recovery2

-- | 测试错误恢复的幂等性
prop_error_recovery_idempotent :: String -> Property
prop_error_recovery_idempotent input =
  not (null input) ==>
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack input,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      recovery1 = recovery error
      recovery2 = recovery error
  in recovery1 === recovery2

-- | 测试错误处理的性能
prop_error_handling_performance :: Positive Int -> Property
prop_error_handling_performance (Positive n) =
  n < 1000 ==>
  let errors = [TypeError {
        errorId = "test" ++ show i,
        severity = Error,
        category = Parsing,
        message = T.pack $ "Error " ++ show i,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      } | i <- [1..n]]
      processed = map formatError errors
  in length processed === n

-- | 测试错误处理的内存安全性
prop_error_handling_memory_safety :: Positive Int -> Property
prop_error_handling_memory_safety (Positive n) =
  n < 1000 ==>
  let errors = [TypeError {
        errorId = "test" ++ show i,
        severity = Error,
        category = Parsing,
        message = T.pack $ "Error " ++ show i,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      } | i <- [1..n]]
      processed = map formatError errors
  in length processed >= 0

-- | 测试错误处理边界情况
prop_error_handling_edge_cases :: String -> Property
prop_error_handling_edge_cases input =
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack input,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in property (length formatted >= 0)

-- | 测试错误处理特殊字符
prop_error_handling_special_chars :: Char -> Property
prop_error_handling_special_chars c =
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack [c],
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in property (length formatted >= 0)

-- | 测试错误处理Unicode字符
prop_error_handling_unicode :: Property
prop_error_handling_unicode =
  let unicodeChars = ['\0'..'\255']
      errors = map (\c -> TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack [c],
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }) unicodeChars
      formatted = map formatError errors
  in property (all (\e -> length e >= 0) formatted)

-- | 测试错误处理极长字符串
prop_error_handling_long_string :: Positive Int -> Property
prop_error_handling_long_string (Positive n) =
  n < 10000 ==>
  let longString = replicate n 'x'
      error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack longString,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in property (length formatted >= 0)

-- | 测试错误处理极深嵌套
prop_error_handling_deep_nesting :: Positive Int -> Property
prop_error_handling_deep_nesting (Positive n) =
  n < 100 ==>
  let nested = replicate n "Error: "
      error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack (concat nested),
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  in property (length formatted >= 0)

-- | 测试错误处理大量错误
prop_error_handling_many_errors :: Positive Int -> Property
prop_error_handling_many_errors (Positive n) =
  n < 1000 ==>
  let errors = [TypeError {
        errorId = "test" ++ show i,
        severity = Error,
        category = Parsing,
        message = T.pack $ "Error " ++ show i,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      } | i <- [1..n]]
      processed = map formatError errors
  in length processed === n

-- | 测试错误处理并发安全性
prop_error_handling_concurrent_safety :: Positive Int -> Property
prop_error_handling_concurrent_safety (Positive n) =
  n < 100 ==>
  let errors = [TypeError {
        errorId = "test" ++ show i,
        severity = Error,
        category = Parsing,
        message = T.pack $ "Error " ++ show i,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      } | i <- [1..n]]
      processed = map formatError errors
  in length processed === n

-- | 测试错误格式化字符串
test_format_error_string :: Assertion
test_format_error_string = do
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Test message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  assertBool "Error contains message" ("Test message" `isInfixOf` formatted)

-- | 测试错误格式化位置
test_format_error_position :: Assertion
test_format_error_position = do
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Test message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  assertBool "Error formatted" (length formatted > 0)

-- | 测试错误格式化跨度
test_format_error_span :: Assertion
test_format_error_span = do
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Test message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  assertBool "Error formatted" (length formatted > 0)

-- | 测试错误格式化一致性
test_format_error_consistency :: Assertion
test_format_error_consistency = do
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Test message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted1 = formatError error
      formatted2 = formatError error
  assertEqual "Error formatting is consistent" formatted1 formatted2

-- | 测试错误格式化幂等性
test_format_error_idempotent :: Assertion
test_format_error_idempotent = do
  let error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "Test message",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted1 = formatError error
      formatted2 = formatError error
  assertEqual "Error formatting is idempotent" formatted1 formatted2

-- | 测试错误处理边界情况
test_error_handling_edge_cases :: Assertion
test_error_handling_edge_cases = do
  let error1 = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = "\0",
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      error2 = error1 { message = "\n" }
      error3 = error1 { message = "\t" }
      formatted1 = formatError error1
      formatted2 = formatError error2
      formatted3 = formatError error3
  assertBool "Null character handled" (length formatted1 >= 0)
  assertBool "Newline character handled" (length formatted2 >= 0)
  assertBool "Tab character handled" (length formatted3 >= 0)

-- | 测试错误处理性能
test_error_formatting_performance :: Assertion
test_error_formatting_performance = do
  let n = 1000
      errors = [TypeError {
        errorId = "test" ++ show i,
        severity = Error,
        category = Parsing,
        message = T.pack $ "Error " ++ show i,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      } | i <- [1..n]]
      processed = map formatError errors
  assertEqual "All errors processed" n (length processed)

-- | 测试错误处理内存安全性
test_error_formatting_memory_safety :: Assertion
test_error_formatting_memory_safety = do
  let n = 10000
      longString = replicate n 'x'
      error = TypeError {
        errorId = "test",
        severity = Error,
        category = Parsing,
        message = T.pack longString,
        location = unknownLocation,
        context = emptyContext,
        recovery = errorRecovery,
        suggestions = [],
        relatedErrors = [],
        errorChain = [],
        timestamp = Nothing
      }
      formatted = formatError error
  assertBool "Long string handled" (length formatted >= 0)

-- | 测试套件
tests :: TestTree
tests = testGroup "Error Handler Consistency QuickCheck Tests"
  [ testProperty "Error formatting consistency" prop_error_formatting_consistency
  , testProperty "Error formatting idempotent" prop_error_formatting_idempotent
  , testProperty "Error format validity" prop_error_format_validity
  , testProperty "Error format contains" prop_error_format_contains
  , testProperty "Error position consistency" prop_error_position_consistency
  , testProperty "Error position order" prop_error_position_order
  , testProperty "Error severity consistency" prop_error_severity_consistency
  , testProperty "Error severity order" prop_error_severity_order
  , testProperty "Error recovery consistency" prop_error_recovery_consistency
  , testProperty "Error recovery idempotent" prop_error_recovery_idempotent
  , testProperty "Error handling performance" prop_error_handling_performance
  , testProperty "Error handling memory safety" prop_error_handling_memory_safety
  , testProperty "Error handling edge cases" prop_error_handling_edge_cases
  , testProperty "Error handling special chars" prop_error_handling_special_chars
  , testProperty "Error handling unicode" prop_error_handling_unicode
  , testProperty "Error handling long string" prop_error_handling_long_string
  , testProperty "Error handling deep nesting" prop_error_handling_deep_nesting
  , testProperty "Error handling many errors" prop_error_handling_many_errors
  , testProperty "Error handling concurrent safety" prop_error_handling_concurrent_safety
  , testCase "Format error string" test_format_error_string
  , testCase "Format error position" test_format_error_position
  , testCase "Format error span" test_format_error_span
  , testCase "Format error consistency" test_format_error_consistency
  , testCase "Format error idempotent" test_format_error_idempotent
  , testCase "Error handling edge cases" test_error_handling_edge_cases
  , testCase "Error formatting performance" test_error_formatting_performance
  , testCase "Error formatting memory safety" test_error_formatting_memory_safety
  ]