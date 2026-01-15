{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EnhancedErrorHandlerQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , formatTimestamp
  , unknownLocation
  , isAtLeast
  , severityPriority
  , compareSeverity
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.Either (isLeft, isRight)
import Data.Time (UTCTime)
import Control.Monad (replicateM)

-- 生成错误消息
genErrorMessage :: Gen String
genErrorMessage = listOf1 $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?-"

-- 生成错误严重性
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Info, Warning, Error, Fatal]

-- 生成错误类别
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [Parsing, TypeChecking, Compilation, Runtime, Internal]

-- 生成错误位置
genErrorLocation :: Gen ErrorLocation
genErrorLocation = oneof
  [ return unknownLocation
  , do
      line <- choose (1, 1000)
      column <- choose (1, 1000)
      return $ ErrorLocation line column Nothing
  ]

-- 生成错误上下文
genErrorContext :: Gen ErrorContext
genErrorContext = do
  messages <- listOf genErrorMessage
  return $ foldl (\ctx msg -> ctx { contextMessages = msg : contextMessages ctx }) emptyContext messages

-- 生成错误恢复策略
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ NoRecovery
  , SkipToNextStatement
  , SkipToNextBlock
  , InsertPlaceholder
  , RetryWithAlternative
  ]

-- 生成基本错误
genBasicError :: Gen CombinedError
genBasicError = do
  msg <- genErrorMessage
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  return $ CombinedError
    { errorMessage = T.pack msg
    , errorSeverity = severity
    , errorCategory = category
    , errorLocation = location
    , errorContext = context
    , errorRecovery = recovery
    , errorTimestamp = Nothing
    , errorSuggestions = []
    , relatedErrors = []
    }

-- 属性1: 新的错误收集器应该是空的
prop_new_error_collector_is_empty :: Property
prop_new_error_collector_is_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) && not (hasWarnings collector)

-- 属性2: 添加错误后应该有错误
prop_add_error_creates_error :: Property
prop_add_error_creates_error = forAll genBasicError $ \error ->
  let collector = addError newErrorCollector error
  in property $ hasErrors collector

-- 属性3: 添加警告后应该有警告
prop_add_warning_creates_warning :: Property
prop_add_warning_creates_warning = forAll genBasicError $ \warning ->
  let warningError = warning { errorSeverity = Warning }
      collector = addWarning newErrorCollector warningError
  in property $ hasWarnings collector

-- 属性4: 添加信息后应该有信息
prop_add_info_creates_info :: Property
prop_add_info_creates_info = forAll genBasicError $ \info ->
  let infoError = info { errorSeverity = Info }
      collector = addInfo newErrorCollector infoError
  in property $ not (null $ getInfo collector)

-- 属性5: 错误格式化应该包含错误消息
prop_format_error_includes_message :: Property
prop_format_error_includes_message = forAll genBasicError $ \error ->
  let formatted = formatError error
  in property $ T.unpack (errorMessage error) `isInfixOf` formatted

-- 属性6: 错误严重性比较应该一致
prop_severity_comparison_consistent :: Property
prop_severity_comparison_consistent = forAll genErrorSeverity $ \severity ->
  property $ isAtLeast severity severity && compareSeverity severity severity === EQ

-- 属性7: 严重性优先级应该是单调的
prop_severity_priority_monotonic :: Property
prop_severity_priority_monotonic = 
  let priorities = map severityPriority [Info, Warning, Error, Fatal]
  in property $ priorities == [1, 2, 3, 4]

-- 属性8: 按类别过滤应该只保留指定类别的错误
prop_filter_by_category :: Property
prop_filter_by_category = 
  forAll (choose (1, 10)) $ \n ->
  forAll (replicateM n genBasicError) $ \errors ->
  forAll genErrorCategory $ \category ->
  let filtered = filterByCategory category errors
  in property $ all (\e -> errorCategory e === category) filtered

-- 属性9: 按严重性过滤应该只保留指定严重性的错误
prop_filter_by_severity :: Property
prop_filter_by_severity = 
  forAll (choose (1, 10)) $ \n ->
  forAll (replicateM n genBasicError) $ \errors ->
  forAll genErrorSeverity $ \severity ->
  let filtered = filterBySeverity severity errors
  in property $ all (\e -> errorSeverity e === severity) filtered

-- 属性10: 组合错误应该使用最高严重性
prop_combine_errors_uses_highest_severity :: Property
prop_combine_errors_uses_highest_severity = 
  forAll (choose (2, 5)) $ \n ->
  forAll (replicateM n genBasicError) $ \errors ->
  let combined = combineErrors errors
      maxSeverity = maximum $ map errorSeverity errors
  in property $ combinedErrorSeverity combined === maxSeverity

-- 属性11: 错误统计应该正确计数
prop_error_statistics_correct_counts :: Property
prop_error_statistics_correct_counts = 
  forAll (choose (1, 10)) $ \n ->
  forAll (replicateM n genBasicError) $ \errors ->
  let stats = getErrorStatistics errors
      errorCount = length $ filter (\e -> errorSeverity e `elem` [Error, Fatal]) errors
      warningCount = length $ filter (\e -> errorSeverity e === Warning) errors
      infoCount = length $ filter (\e -> errorSeverity e === Info) errors
  in property $ stats === (errorCount, warningCount, infoCount)

-- 属性12: 错误报告应该包含所有错误
prop_error_report_includes_all_errors :: Property
prop_error_report_includes_all_errors = 
  forAll (choose (1, 5)) $ \n ->
  forAll (replicateM n genBasicError) $ \errors ->
  let report = generateErrorReport errors
  in property $ length (lines report) >= n

-- 属性13: 包装错误应该保留原始错误
prop_wrap_error_preserves_original :: Property
prop_wrap_error_preserves_original = forAll genBasicError $ \original ->
  let wrapperMsg = "Wrapper error"
      wrapped = wrapError wrapperMsg original
  in property $ original `elem` relatedErrors wrapped

-- 属性14: 添加建议应该保留建议
prop_add_suggestions_preserves_suggestions :: Property
prop_add_suggestions_preserves_suggestions = do
  error <- genBasicError
  suggestions <- listOf1 genErrorMessage
  let withSuggestions = errorWithSuggestions error (map T.pack suggestions)
  in property $ all (`elem` errorSuggestions withSuggestions) (map T.pack suggestions)

-- 属性15: 添加上下文应该保留上下文
prop_add_context_preserves_context :: Property
prop_add_context_preserves_context = do
  error <- genBasicError
  contextMsg <- genErrorMessage
  let withContextMsg = withContext error contextMsg
  in property $ contextMsg `elem` contextMessages (errorContext withContextMsg)

-- 测试套件
tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Properties Tests"
  [ testProperty "New error collector is empty" prop_new_error_collector_is_empty
  , testProperty "Add error creates error" prop_add_error_creates_error
  , testProperty "Add warning creates warning" prop_add_warning_creates_warning
  , testProperty "Add info creates info" prop_add_info_creates_info
  , testProperty "Format error includes message" prop_format_error_includes_message
  , testProperty "Severity comparison consistent" prop_severity_comparison_consistent
  , testProperty "Severity priority monotonic" prop_severity_priority_monotonic
  , testProperty "Filter by category" prop_filter_by_category
  , testProperty "Filter by severity" prop_filter_by_severity
  , testProperty "Combine errors uses highest severity" prop_combine_errors_uses_highest_severity
  , testProperty "Error statistics correct counts" prop_error_statistics_correct_counts
  , testProperty "Error report includes all errors" prop_error_report_includes_all_errors
  , testProperty "Wrap error preserves original" prop_wrap_error_preserves_original
  , testProperty "Add suggestions preserves suggestions" prop_add_suggestions_preserves_suggestions
  , testProperty "Add context preserves context" prop_add_context_preserves_context
  ]