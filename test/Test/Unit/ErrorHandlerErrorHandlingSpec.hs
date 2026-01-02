{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
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
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , infoRecovery
  )
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成错误严重级别
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- 生成错误类别
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ ParseError
  , TypeError
  , NameError
  , ScopeError
  , OwnershipError
  , DependencyError
  , InternalError
  , Warning
  , Info
  ]

-- 生成错误位置
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  endLine <- choose (line, line + 10)
  endColumn <- choose (column, column + 100)
  filePath <- oneof [return Nothing, Just <$> elements ["file1.typus", "file2.typus", "test.typus"]]
  return $ ErrorLocation filePath line column (Just endLine) (Just endColumn)

-- 生成错误消息
genErrorMessage :: Gen String
genErrorMessage = do
  words <- listOf $ elements $ ["error", "in", "type", "checking", "parsing", "compilation"]
  return $ unwords words

-- 生成错误建议
genErrorSuggestions :: Gen [String]
genErrorSuggestions = listOf $ do
  words <- listOf $ elements $ ["fix", "by", "adding", "removing", "changing"]
  return $ unwords words

-- ============================================================================
-- 错误处理属性测试
-- ============================================================================

-- Property: 错误收集器正确收集错误
prop_error_collector_collects_errors :: ErrorSeverity -> String -> Property
prop_error_collector_collects_errors severity message =
  let collector = newErrorCollector
      result = case severity of
        Fatal -> addError collector (fatalError message)
        Error -> addError collector (errorAt "test-id" Nothing Nothing) message)
        Warning -> addWarning collector (warningAt "test-id" Nothing Nothing) message)
        Info -> addInfo collector (infoAt "test-id" Nothing Nothing) message)
  in property $ hasErrors collector === (severity == Fatal || severity == Error)

-- Property: 错误过滤按严重级别工作
prop_filter_by_severity :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_by_severity severities filterSeverity =
  not (null severities) ==>
  let errors = zipWith (\i sev -> errorWithCategory sev ParseError ("error" ++ show i)) [1..] severities
      filtered = filterBySeverity filterSeverity errors
      expected = L.filter (\e -> errorSeverity e >= filterSeverity) errors
  in property $ L.length filtered === L.length expected

-- Property: 错误过滤按类别工作
prop_filter_by_category :: [ErrorCategory] -> ErrorCategory -> Property
prop_filter_by_category categories filterCategory =
  not (null categories) ==>
  let errors = zipWith (\i cat -> errorWithCategory Error cat ("error" ++ show i)) [1..] categories
      filtered = filterByCategory filterCategory errors
      expected = L.filter (\e -> errorCategory e == filterCategory) errors
  in property $ L.length filtered === L.length expected

-- Property: 错误组合保持正确的严重级别
prop_combine_errors_preserves_severity :: ErrorSeverity -> ErrorSeverity -> Property
prop_combine_errors_preserves_severity sev1 sev2 =
  let error1 = errorWithCategory sev1 ParseError "error1"
      error2 = errorWithCategory sev2 ParseError "error2"
      combined = combineErrors [error1, error2]
      expectedSeverity = max sev1 sev2
  in property $ combinedErrorSeverity combined === expectedSeverity

-- Property: 错误恢复策略工作正确
prop_error_recovery_strategy :: ErrorSeverity -> Property
prop_error_recovery_strategy severity =
  let error = errorWithCategory severity ParseError "test error"
      canRecover = canRecoverFrom error
      shouldContinue = shouldContinueAfter error
  in case severity of
    Fatal -> property $ not canRecover .&&. not shouldContinue
    Error -> property $ canRecover .&&. shouldContinue
    Warning -> property $ canRecover .&&. shouldContinue
    Info -> property $ canRecover .&&. shouldContinue

-- Property: 错误格式化包含基本信息
prop_error_formatting_contains_info :: ErrorSeverity -> String -> Property
prop_error_formatting_contains_info severity message =
  not (null message) ==>
  let error = errorWithCategory severity ParseError message
      formatted = formatError error
  in property $ message `L.isInfixOf` formatted

-- Property: 错误位置格式化包含行号
prop_error_location_formatting :: Int -> Int -> Property
prop_error_location_formatting line column =
  line > 0 && column > 0 ==>
  let location = ErrorLocation Nothing line column Nothing Nothing
      error = errorAt "test-id" (null context) ==>
  let baseError = errorWithCategory Error ParseError "base error"
      withCtx = withContext baseError context
      errorContext = errorCodeContext withCtx
  in property $ context `L.isInfixOf` show errorContext

-- Property: 错误建议添加工作正确
prop_error_suggestions_addition :: [String] -> Property
prop_error_suggestions_addition suggestions =
  not (null suggestions) ==>
  let baseError = errorWithCategory Error ParseError "base error"
      withSugg = withSuggestions baseError suggestions
      errorSuggestions = errorSuggestions withSugg
  in property $ L.length errorSuggestions === L.length suggestions

-- Property: 相关错误添加工作正确
prop_related_errors_addition :: [TypeError] -> Property
prop_related_errors_addition relatedErrors =
  not (null relatedErrors) ==>
  let baseError = errorWithCategory Error ParseError "base error"
      withRelated = withRelatedErrors baseError relatedErrors
      related = relatedErrors withRelated
  in property $ L.length related === L.length relatedErrors

-- ============================================================================
-- 错误统计和报告测试
-- ============================================================================

-- Property: 错误统计正确计算
prop_error_statistics_calculation :: [ErrorSeverity] -> Property
prop_error_statistics_calculation severities =
  not (null severities) ==>
  let errors = zipWith (\i sev -> errorWithCategory sev ParseError ("error" ++ show i)) [1..] severities
      stats = getErrorStatistics errors
      fatalCount = L.length $ L.filter (\e -> errorSeverity e == Fatal) errors
      errorCount = L.length $ L.filter (\e -> errorSeverity e == Error) errors
      warningCount = L.length $ L.filter (\e -> errorSeverity e == Warning) errors
      infoCount = L.length $ L.filter (\e -> errorSeverity e == Info) errors
  in property $ stats === (fatalCount, errorCount, warningCount, infoCount)

-- Property: 错误报告包含所有错误
prop_error_report_contains_all :: [String] -> Property
prop_error_report_contains_all messages =
  not (null messages) ==>
  let errors = zipWith (\i msg -> errorWithCategory Error ParseError msg) [1..] messages
      report = generateErrorReport errors
  in property $ L.all (`L.isInfixOf` report) messages

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "ErrorHandler Error Handling Tests"
    [ testGroup "Property Tests"
        [ fastProperty "error collector collects errors" prop_error_collector_collects_errors
        , fastProperty "filter by severity" prop_filter_by_severity
        , fastProperty "filter by category" prop_filter_by_category
        , fastProperty "combine errors preserves severity" prop_combine_errors_preserves_severity
        , fastProperty "error recovery strategy" prop_error_recovery_strategy
        , fastProperty "error formatting contains info" prop_error_formatting_contains_info
        , fastProperty "error location formatting" prop_error_location_formatting
        , fastProperty "error context addition" prop_error_context_addition
        , fastProperty "error suggestions addition" prop_error_suggestions_addition
        , fastProperty "related errors addition" prop_related_errors_addition
        , fastProperty "error statistics calculation" prop_error_statistics_calculation
        , fastProperty "error report contains L.all" prop_error_report_contains_all
        ]
    , testGroup "Unit Tests"
        [ testCase "create L.and use error collector" $ do
            let collector = newErrorCollector
            addError collector (errorAt "test-id" Nothing Nothing) "test error")
            addWarning collector (warningAt "test-id" Nothing Nothing) "test warning")
            addInfo collector (infoAt "test-id" Nothing Nothing) "test info")
            
            hasErrors collector @?= True
            hasWarnings collector @?= True
            L.length (getErrors collector) @?= 1
            L.length (getWarnings collector) @?= 1
            L.length (getInfo collector) @?= 1

        , testCase "error severity comparison" $ do
            Fatal > Error @?= True
            Error > Warning @?= True
            Warning > Info @?= True
            Info < Fatal @?= True

        , testCase "error category filtering" $ do
            let errors = 
                  [ errorWithCategory Error ParseError "parse error"
                  , errorWithCategory Error TypeError "type error"
                  , errorWithCategory Warning ParseError "parse warning"
                  , errorWithCategory Info TypeError "type info"
                  ]
            parseErrors = filterByCategory ParseError errors
            typeErrors = filterByCategory TypeError errors
            
            L.length parseErrors @?= 2
            L.length typeErrors @?= 2

        , testCase "error severity filtering" $ do
            let errors = 
                  [ errorWithCategory Fatal ParseError "fatal error"
                  , errorWithCategory Error TypeError "type error"
                  , errorWithCategory Warning ParseError "parse warning"
                  , errorWithCategory Info TypeError "type info"
                  ]
            errorsAndFatal = filterBySeverity Error errors
            warningsAndAbove = filterBySeverity Warning errors
            
            L.length errorsAndFatal @?= 2
            L.length warningsAndAbove @?= 3

        , testCase "error combination" $ do
            let error1 = errorWithCategory Error ParseError "first error"
                error2 = errorWithCategory Warning TypeError "second warning"
                error3 = errorWithCategory Fatal ParseError "fatal error"
                combined = combineErrors [error1, error2, error3]
            combinedErrorSeverity combined @?= Fatal

        , testCase "error recovery strategies" $ do
            let fatalError = errorWithCategory Fatal ParseError "fatal"
                regularError = errorWithCategory Error ParseError "regular"
                warning = errorWithCategory Warning ParseError "warning"
                info = errorWithCategory Info ParseError "info"
            
            canRecoverFrom fatalError @?= False
            shouldContinueAfter fatalError @?= False
            
            canRecoverFrom regularError @?= True
            shouldContinueAfter regularError @?= True
            
            canRecoverFrom warning @?= True
            shouldContinueAfter warning @?= True
            
            canRecoverFrom info @?= True
            shouldContinueAfter info @?= True

        , testCase "error formatting" $ do
            let location = ErrorLocation (Just "test.typus") 10 5 (Just 10) (Just 15)
                error = errorAt "test-id" "type mismatch") suggestions
                errorSuggestions error @?= suggestions

        , testCase "error with context" $ do
            let context = "while checking function main"
                error = withContext (errorWithCategory Error TypeError "type error") context
                errorCodeContext error @?= context

        , testCase "error wrapping" $ do
            let innerError = errorWithCategory Error TypeError "inner error"
                wrappedError = wrapError "outer context" innerError
            "outer context" `L.isInfixOf` errorMessage wrappedError @?= True
            "inner error" `L.isInfixOf` errorMessage wrappedError @?= True

        , testCase "error statistics" $ do
            let errors = 
                  [ errorWithCategory Fatal ParseError "fatal"
                  , errorWithCategory Error TypeError "error1"
                  , errorWithCategory Error ParseError "error2"
                  , errorWithCategory Warning TypeError "warning1"
                  , errorWithCategory Warning ParseError "warning2"
                  , errorWithCategory Info TypeError "info"
                  ]
                stats = getErrorStatistics errors
            stats @?= (1, 2, 2, 1)

        , testCase "custom recovery strategy" $ do
            let customStrategy = createRecoveryStrategy True True "custom recovery"
                error = errorWithCategory Error ParseError "test error"
                recovery = customRecovery customStrategy error
            recoveryCanContinue recovery @?= True
            recoveryMessage recovery @?= "custom recovery"

        , testCase "multiple error formatting" $ do
            let errors = 
                  [ errorAt "test-id" Nothing Nothing) "first error"
                  , errorAt "test-id" Nothing Nothing) "second error"
                  , warningAt "test-id" Nothing Nothing) "warning"
                  ]
                formatted = formatErrorsWithLocation errors
            "first error" `L.isInfixOf` formatted @?= True
            "second error" `L.isInfixOf` formatted @?= True
            "warning" `L.isInfixOf` formatted @?= True
        ]
    ]