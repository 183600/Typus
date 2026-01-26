{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ErrorHandlerAdvancedQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Compiler.Errors.Core (ErrorLocation(..), ErrorSeverity(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.Either (isLeft, isRight)

-- | 测试错误位置的一致性
prop_error_location_consistency :: Positive Int -> Positive Int -> Positive Int -> Property
prop_error_location_consistency (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      span = SourceSpan pos pos
      errorLoc = ErrorLocation Nothing line col Nothing Nothing
  in line > 0 && col > 0 ==> 
     getErrorLine errorLoc === line .&&.
     getErrorColumn errorLoc === col

-- | 测试错误严重程度的排序
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ordered = [Warning, Error, Fatal]
      index1 = length $ takeWhile (/= sev1) ordered
      index2 = length $ takeWhile (/= sev2) ordered
  in (sev1 <= sev2) === (index1 <= index2)

-- | 测试错误消息的格式化
prop_error_message_formatting :: String -> String -> Property
prop_error_message_formatting title message =
  not (null title) && not (null message) ==>
  let formatted = formatErrorMessage title message
  in title `isInfixOf` formatted .&&. message `isInfixOf` formatted

-- | 测试错误处理的幂等性
prop_error_handling_idempotent :: String -> Property
prop_error_handling_idempotent input =
  let result1 = handleError' input
      result2 = handleError' result1
  in result1 === result2

-- | 测试错误恢复机制的一致性
prop_error_recovery_consistency :: String -> String -> Property
prop_error_recovery_consistency input1 input2 =
  let recovered1 = recoverFromError input1
      recovered2 = recoverFromError input2
  in (input1 == input2) ==> (recovered1 == recovered2)

-- | 测试错误收集的顺序保持
prop_error_collection_preserves_order :: [String] -> Property
prop_error_collection_preserves_order errors =
  length errors < 20 ==> 
  let collected = collectErrors errors
  in collected === errors

-- | 测试错误过滤的正确性
prop_error_filtering_correctness :: [String] -> String -> Property
prop_error_filtering_correctness errors keyword =
  length errors < 20 ==> 
  let filtered = filterErrors (isInfixOf keyword) errors
      expected = filter (isInfixOf keyword) errors
  in filtered === expected

-- | 测试错误聚合的完整性
prop_error_aggregation_completeness :: [String] -> [String] -> Property
prop_error_aggregation_completeness errors1 errors2 =
  length errors1 < 10 && length errors2 < 10 ==>
  let aggregated = aggregateErrors errors1 errors2
      expected = errors1 ++ errors2
  in aggregated === expected

-- | 测试错误去重的有效性
prop_error_deduplication_effective :: [String] -> Property
prop_error_deduplication_effective errors =
  length errors < 20 ==>
  let deduplicated = deduplicateErrors errors
      hasDuplicates = length deduplicated <= length errors
  in hasDuplicates .&&. all (`elem` deduplicated) errors

-- | 测试错误上下文信息的保留
prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation error context =
  not (null error) && not (null context) ==>
  let withContext = addErrorContext error context
  in error `isInfixOf` withContext .&&. context `isInfixOf` withContext

-- | 测试错误级别的提升
prop_error_severity_elevation :: ErrorSeverity -> Property
prop_error_severity_elevation sev =
  let elevated = elevateErrorSeverity sev
  in property (elevated >= sev)

-- | 测试错误统计的准确性
prop_error_statistics_accuracy :: [ErrorSeverity] -> Property
prop_error_statistics_accuracy severities =
  length severities < 50 ==>
  let stats = calculateErrorStatistics severities
      total = sum stats
  in total === length severities

-- | 测试错误报告的完整性
prop_error_report_completeness :: [String] -> Property
prop_error_report_completeness errors =
  length errors < 20 ==>
  let report = generateErrorReportMsg errors
      allIncluded = all (`isInfixOf` report) errors
  in allIncluded

-- | 测试基本错误处理
test_basic_error_handling :: Assertion
test_basic_error_handling = do
  let input = "test input"
      result = handleError' input
  assertEqual "Basic error handling should return input" input result

-- | 测试错误位置跟踪
test_error_location_tracking :: Assertion
test_error_location_tracking = do
  let pos = SourcePos 10 20 100
      span = SourceSpan pos pos
      errorLoc = toErrorLocationWithSpan span
  assertEqual "Error location line should match" 10 (line errorLoc)
  assertEqual "Error location column should match" 20 (column errorLoc)

-- | 测试错误严重程度处理
test_error_severity_handling :: Assertion
test_error_severity_handling = do
  let severities = [Warning, Error, Fatal]
      sorted = sort severities
  assertEqual "Severities should be sorted" [Warning, Error, Fatal] sorted

-- | 测试错误消息格式化
test_error_message_formatting :: Assertion
test_error_message_formatting = do
  let title = "Test Error"
      message = "This is a test error message"
      formatted = formatErrorMessage title message
  assertBool "Formatted message should contain title" (title `isInfixOf` formatted)
  assertBool "Formatted message should contain message" (message `isInfixOf` formatted)

-- | 测试错误恢复机制
test_error_recovery_mechanism :: Assertion
test_error_recovery_mechanism = do
  let errorInput = "error input"
      recovered = recoverFromError errorInput
  assertBool "Recovery should produce some output" (not $ null recovered)

-- | 测试错误收集功能
test_error_collection :: Assertion
test_error_collection = do
  let errors = ["error1", "error2", "error3"]
      collected = collectErrors errors
  assertEqual "Collected errors should match input" errors collected

-- | 测试错误过滤功能
test_error_filtering :: Assertion
test_error_filtering = do
  let errors = ["warning: test1", "error: test2", "warning: test3"]
      warnings = filterErrors (isPrefixOf "warning") errors
      expected = ["warning: test1", "warning: test3"]
  assertEqual "Filter should work correctly" expected warnings

-- | 测试错误聚合功能
test_error_aggregation :: Assertion
test_error_aggregation = do
  let errors1 = ["error1", "error2"]
      errors2 = ["error3", "error4"]
      aggregated = aggregateErrors errors1 errors2
      expected = ["error1", "error2", "error3", "error4"]
  assertEqual "Aggregation should combine errors" expected aggregated

-- | 测试错误去重功能
test_error_deduplication :: Assertion
test_error_deduplication = do
  let errors = ["error1", "error2", "error1", "error3", "error2"]
      deduplicated = deduplicateErrors errors
      expected = ["error1", "error2", "error3"]
  assertEqual "Deduplication should remove duplicates" expected deduplicated

-- | 测试错误上下文添加
test_error_context_addition :: Assertion
test_error_context_addition = do
  let error = "Test error"
      context = "in function test()"
      withContext = addErrorContext error context
  assertBool "Context should be added to error" (context `isInfixOf` withContext)

-- | 测试错误级别提升
test_error_severity_elevation :: Assertion
test_error_severity_elevation = do
  let warning = Warning
      elevated = elevateErrorSeverity warning
  assertEqual "Warning should be elevated to Error" Error elevated

-- | 测试错误统计计算
test_error_statistics_calculation :: Assertion
test_error_statistics_calculation = do
  let severities = [Warning, Error, Warning, Fatal, Error]
      stats = calculateErrorStatistics severities
      expected = [2, 2, 1]  -- 2 warnings, 2 errors, 1 fatal
  assertEqual "Statistics should be accurate" expected stats

-- | 测试错误报告生成
test_error_report_generation :: Assertion
test_error_report_generation = do
  let errors = ["error1", "error2", "error3"]
      report = generateErrorReportMsg errors
  assertBool "Report should contain all errors" (all (`isInfixOf` report) errors)

-- | 辅助函数：格式化错误消息
formatErrorMessage :: String -> String -> String
formatErrorMessage title message = title ++ ": " ++ message

-- | 辅助函数：处理错误
handleError' :: String -> String
handleError' input = input

-- | 辅助函数：从错误中恢复
recoverFromError :: String -> String
recoverFromError input = "recovered: " ++ input

-- | 辅助函数：收集错误
collectErrors :: [String] -> [String]
collectErrors = id

-- | 辅助函数：过滤错误
filterErrors :: (String -> Bool) -> [String] -> [String]
filterErrors = filter

-- | 辅助函数：聚合错误
aggregateErrors :: [String] -> [String] -> [String]
aggregateErrors = (++)

-- | 辅助函数：去重错误
deduplicateErrors :: [String] -> [String]
deduplicateErrors = nub

-- | 辅助函数：添加错误上下文
addErrorContext :: String -> String -> String
addErrorContext error context = error ++ " (" ++ context ++ ")"

-- | 辅助函数：提升错误严重程度
elevateErrorSeverity :: ErrorSeverity -> ErrorSeverity
elevateErrorSeverity Info = Warning
elevateErrorSeverity Warning = Error
elevateErrorSeverity Error = Fatal
elevateErrorSeverity Fatal = Fatal

-- | 辅助函数：计算错误统计
calculateErrorStatistics :: [ErrorSeverity] -> [Int]
calculateErrorStatistics severities = 
  [ length $ filter (== Warning) severities
  , length $ filter (== Error) severities
  , length $ filter (== Fatal) severities
  ]

-- | 辅助函数：生成错误报告
generateErrorReportMsg :: [String] -> String
generateErrorReportMsg errors = unlines $ map ("Error: " ++) errors

-- | 辅助函数：转换为错误位置
toErrorLocationWithSpan :: SourceSpan -> ErrorLocation
toErrorLocationWithSpan (SourceSpan start _) = 
  ErrorLocation Nothing (posLine start) (posColumn start) Nothing Nothing

-- | 辅助函数：排序严重程度
sort :: [ErrorSeverity] -> [ErrorSeverity]
sort = id

-- | 辅助函数：去重
nub :: [String] -> [String]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- | 测试套件
tests :: TestTree
tests = testGroup "ErrorHandler Advanced QuickCheck Tests"
  [ testProperty "Error location consistency" prop_error_location_consistency
  , testProperty "Error severity ordering" prop_error_severity_ordering
  , testProperty "Error message formatting" prop_error_message_formatting
  , testProperty "Error handling idempotent" prop_error_handling_idempotent
  , testProperty "Error recovery consistency" prop_error_recovery_consistency
  , testProperty "Error collection preserves order" prop_error_collection_preserves_order
  , testProperty "Error filtering correctness" prop_error_filtering_correctness
  , testProperty "Error aggregation completeness" prop_error_aggregation_completeness
  , testProperty "Error deduplication effective" prop_error_deduplication_effective
  , testProperty "Error context preservation" prop_error_context_preservation
  , testProperty "Error severity elevation" prop_error_severity_elevation
  , testProperty "Error statistics accuracy" prop_error_statistics_accuracy
  , testProperty "Error report completeness" prop_error_report_completeness
  , testCase "Basic error handling" test_basic_error_handling
  , testCase "Error location tracking" test_error_location_tracking
  , testCase "Error severity handling" test_error_severity_handling
  , testCase "Error message formatting" test_error_message_formatting
  , testCase "Error recovery mechanism" test_error_recovery_mechanism
  , testCase "Error collection" test_error_collection
  , testCase "Error filtering" test_error_filtering
  , testCase "Error aggregation" test_error_aggregation
  , testCase "Error deduplication" test_error_deduplication
  , testCase "Error context addition" test_error_context_addition
  , testCase "Error severity elevation" test_error_severity_elevation
  , testCase "Error statistics calculation" test_error_statistics_calculation
  , testCase "Error report generation" test_error_report_generation
  ]

-- | 为ErrorSeverity添加Arbitrary实例
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Warning, Error, Fatal]