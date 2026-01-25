{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewErrorHandlerTestSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), Located(..), locatedAt)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (null, isInfixOf, isPrefixOf, find, zip, tails)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.State (evalState, execState)

-- Helper function to format UTCTime
formatTimestampHelper :: UTCTime -> String
formatTimestampHelper = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%3q"

-- | 测试错误收集器的基本功能
test_error_collector_basic :: Assertion
test_error_collector_basic = do
  let location = ErrorLocation Nothing 1 10 Nothing Nothing
      error = errorAt "Test" Error "Test error" location
      errors = execState (addError error) []
  assertEqual "Should have one error" 1 (length errors)
  let error' = case errors of
        (e:_) -> e
        [] -> Prelude.error "No errors found"
  assertEqual "Error message should match" "Test error" (T.unpack $ message error')

-- | 测试警告收集
test_warning_collection :: Assertion
test_warning_collection = do
  let location = ErrorLocation Nothing 2 20 Nothing Nothing
      warning = warningAt "Test" "Test warning" location
      warnings = execState (addWarning warning) []
  assertEqual "Should have one warning" 1 (length warnings)
  let warning' = case warnings of
        (w:_) -> w
        [] -> Prelude.error "No warnings found"
  assertEqual "Warning message should match" "Test warning" (T.unpack $ message warning')

-- | 测试信息收集
test_info_collection :: Assertion
test_info_collection = do
  let location = ErrorLocation Nothing 3 30 Nothing Nothing
      info = infoAt "Test" "Test info" location
      infos = execState (addInfo info) []
  assertEqual "Should have one info message" 1 (length infos)
  let info' = case infos of
        (i:_) -> i
        [] -> Prelude.error "No info messages found"
  assertEqual "Info message should match" "Test info" (T.unpack $ message info')

-- | 测试错误检测
test_error_detection :: Assertion
test_error_detection = do
  let location = ErrorLocation Nothing 1 10 Nothing Nothing
      error = errorAt "Test" Error "Test error" location
      errors = execState (addError error) []
  assertBool "Should detect errors" (hasErrors errors)
  assertBool "Should not detect warnings" (not $ hasWarnings errors)

-- | 测试警告检测
test_warning_detection :: Assertion
test_warning_detection = do
  let location = ErrorLocation Nothing 2 20 Nothing Nothing
      warning = warningAt "Test" "Test warning" location
      warnings = execState (addWarning warning) []
  assertBool "Should detect warnings" (hasWarnings warnings)
  assertBool "Should not detect errors" (not $ hasErrors warnings)

-- | 测试错误格式化
test_error_formatting :: Assertion
test_error_formatting = do
  let location = ErrorLocation Nothing 5 15 Nothing Nothing
      error = errorAt "Test" Error "Test error message" location
      formatted = formatError error
  assertBool "Formatted error should contain line number" ("5:" `isInfixOf` formatted)
  assertBool "Formatted error should contain column number" (":15" `isInfixOf` formatted)
  assertBool "Formatted error should contain error message" ("Test error message" `isInfixOf` formatted)

-- | 测试带位置的错误格式化
test_error_formatting_with_location :: Assertion
test_error_formatting_with_location = do
  let location = ErrorLocation Nothing 10 25 Nothing Nothing
      error = errorAt "Test" Error "Location test error" location
      formatted = formatErrorWithLocation error
  assertBool "Formatted error should contain location info" ("10:25" `isInfixOf` formatted)
  assertBool "Formatted error should contain error message" ("Location test error" `isInfixOf` formatted)

-- | 测试多个错误的格式化
test_multiple_errors_formatting :: Assertion
test_multiple_errors_formatting = do
  let location1 = ErrorLocation Nothing 1 10 Nothing Nothing
      location2 = ErrorLocation Nothing 2 20 Nothing Nothing
      location3 = ErrorLocation Nothing 3 30 Nothing Nothing
      error1 = errorAt "Test" Error "First error" location1
      error2 = errorAt "Test" Error "Second error" location2
      warning = warningAt "Test" "A warning" location3
      errors = execState (addError error1 >> addError error2 >> addWarning warning) []
      formatted = formatErrors errors
  assertBool "Formatted errors should contain first error" ("First error" `isInfixOf` formatted)
  assertBool "Formatted errors should contain second error" ("Second error" `isInfixOf` formatted)
  assertBool "Formatted errors should contain warning" ("A warning" `isInfixOf` formatted)

-- | 测试错误恢复
test_error_recovery :: Assertion
test_error_recovery = do
  let location1 = ErrorLocation Nothing 1 10 Nothing Nothing
      location2 = ErrorLocation Nothing 2 20 Nothing Nothing
      recoverableError = errorAt "Test" Error "Recoverable error" location1
      nonRecoverableError = errorAt "Test" Error "Non-recoverable error" location2
  assertBool "Should be able to recover from recoverable error" (canRecoverFrom recoverableError)
  assertBool "Should continue after recoverable error" (shouldContinueAfter recoverableError)

-- | 测试错误分类
test_error_categorization :: Assertion
test_error_categorization = do
  let location1 = ErrorLocation Nothing 1 10 Nothing Nothing
      location2 = ErrorLocation Nothing 2 20 Nothing Nothing
      location3 = ErrorLocation Nothing 3 30 Nothing Nothing
      syntaxError = errorWithCategory "Test" Parsing "Syntax error" location1
      typeError = errorWithCategory "Test" TypeChecking "Type error" location2
      warning = warningWithCategory "Test" Parsing "Syntax warning" location3
  assertEqual "Syntax error should have correct category" Parsing (category syntaxError)
  assertEqual "Type error should have correct category" TypeChecking (category typeError)
  assertEqual "Syntax warning should have correct category" Parsing (category warning)

-- | 测试错误严重性
test_error_severity :: Assertion
test_error_severity = do
  let location1 = ErrorLocation Nothing 1 10 Nothing Nothing
      location2 = ErrorLocation Nothing 2 20 Nothing Nothing
      location3 = ErrorLocation Nothing 3 30 Nothing Nothing
      error = errorAt "Test" Error "Error" location1
      warning = warningAt "Test" "Warning" location2
      info = infoAt "Test" "Info" location3
  assertEqual "Error should have Error severity" Error (severity error)
  assertEqual "Warning should have Warning severity" Warning (severity warning)
  assertEqual "Info should have Info severity" Info (severity info)

-- | 测试错误上下文
test_error_context :: Assertion
test_error_context = do
  let context = emptyContext
      location = ErrorLocation Nothing 1 10 Nothing Nothing
      error = errorAt "Test" Error "Context test error" location
  assertEqual "Error context should be empty" context (Compiler.Errors.Core.context error)

-- | 测试时间戳错误
test_timestamped_errors :: Assertion
test_timestamped_errors = do
  currentTime <- getCurrentTime
  let location = ErrorLocation Nothing 1 10 Nothing Nothing
      timestampedError = errorAtWithUTCTime currentTime "Test" "Timestamped error" location
      errorTime = timestamp timestampedError
  assertEqual "Error should have correct timestamp" (Just (formatTimestampHelper currentTime)) errorTime

-- | QuickCheck属性：错误收集器应该正确计数错误
prop_error_collector_counts :: [String] -> Property
prop_error_collector_counts messages =
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      errors = execState (mapM_ (\msg -> addError (errorAt "Test" Error (T.pack msg) location)) messages) []
  in length errors === length messages

-- | QuickCheck属性：错误格式化应该包含所有必要信息
prop_error_formatting_contains_info :: String -> Positive Int -> Positive Int -> Property
prop_error_formatting_contains_info msg (Positive line) (Positive col) =
  let location = ErrorLocation Nothing line col Nothing Nothing
      error = errorAt "Test" Error (T.pack msg) location
      formatted = formatError error
  in if line > 0 && col > 0 && not (null msg)
     then (show line `isInfixOf` formatted) .&&. 
          (show col `isInfixOf` formatted) .&&.
          (msg `isInfixOf` formatted)
     else property True

-- | QuickCheck属性：错误恢复应该一致
prop_error_recovery_consistent :: String -> Property
prop_error_recovery_consistent msg =
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = errorAt "Test" Error (T.pack msg) location
      canRecover = canRecoverFrom error
      shouldContinue = shouldContinueAfter error
  in property (canRecover == shouldContinue)  -- 简化假设：可恢复性应该与继续性一致

-- | 测试错误位置提取
test_error_location_extraction :: Assertion
test_error_location_extraction = do
  let location = ErrorLocation Nothing 10 25 Nothing Nothing
      error = errorAt "Test" Error "Location test" location
      line = getErrorLine location
      column = getErrorColumn location
  assertEqual "Should extract correct line" 10 line
  assertEqual "Should extract correct column" 25 column

-- | 测试组合错误
test_combined_errors :: Assertion
test_combined_errors = do
  let location1 = ErrorLocation Nothing 1 10 Nothing Nothing
      location2 = ErrorLocation Nothing 2 20 Nothing Nothing
      error1 = errorAt "Test" Error "First error" location1
      error2 = errorAt "Test" Error "Second error" location2
      combined = combineErrors [error1, error2]
  assertEqual "Combined error should contain both errors" 2 (length combined)

-- | 测试错误排序
test_error_sorting :: Assertion
test_error_sorting = do
  let location1 = ErrorLocation Nothing 2 20 Nothing Nothing
      location2 = ErrorLocation Nothing 1 10 Nothing Nothing
      error1 = errorAt "Test" Error "Later error" location1
      error2 = errorAt "Test" Error "Earlier error" location2
      errors = [error1, error2]
      sortedErrors = formatErrors errors
  assertBool "Earlier error should appear before later error" 
    (indexOf "Earlier error" sortedErrors < indexOf "Later error" sortedErrors)
  where
    indexOf sub str = fromMaybe (-1) $ fmap fst $ find ((sub `isPrefixOf`) . snd) $ zip [0..] (tails str)

-- | 测试错误消息的国际化支持
test_error_message_internationalization :: Assertion
test_error_message_internationalization = do
  let location = ErrorLocation Nothing 1 10 Nothing Nothing
      chineseError = errorAt "Test" Error (T.pack "这是一个错误") location
      formatted = formatError chineseError
  assertBool "Should handle Chinese error messages" ("这是一个错误" `isInfixOf` formatted)

-- | 测试套件
tests :: TestTree
tests = testGroup "New ErrorHandler Tests"
  [ testCase "Error collector basic functionality" test_error_collector_basic
  , testCase "Warning collection" test_warning_collection
  , testCase "Info collection" test_info_collection
  , testCase "Error detection" test_error_detection
  , testCase "Warning detection" test_warning_detection
  , testCase "Error formatting" test_error_formatting
  , testCase "Error formatting with location" test_error_formatting_with_location
  , testCase "Multiple errors formatting" test_multiple_errors_formatting
  , testCase "Error recovery" test_error_recovery
  , testCase "Error categorization" test_error_categorization
  , testCase "Error severity" test_error_severity
  , testCase "Error context" test_error_context
  , testCase "Timestamped errors" test_timestamped_errors
  , testCase "Error location extraction" test_error_location_extraction
  , testCase "Combined errors" test_combined_errors
  , testCase "Error sorting" test_error_sorting
  , testCase "Error message internationalization" test_error_message_internationalization
  , testProperty "Error collector counts" prop_error_collector_counts
  , testProperty "Error formatting contains info" prop_error_formatting_contains_info
  , testProperty "Error recovery consistent" prop_error_recovery_consistent
  ]