{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
  , withTimestamp
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.State (evalState, get, put)

-- ============================================================================
-- Advanced ErrorHandler Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "ErrorHandler Advanced Tests"
    [ testGroup "Error creation and properties"
        [ fastProperty "errorAt creates error with correct location" prop_errorAt_location
        , fastProperty "errorWithCategory creates error with correct category" prop_errorWithCategory_category
        , fastProperty "errorWithSuggestions preserves suggestions" prop_errorWithSuggestions_preserves
        , fastProperty "fatalError has Fatal severity" prop_fatalError_severity
        ]

    , testGroup "Error combination and merging"
        [ fastProperty "combineErrors preserves all errors" prop_combineErrors_preserves
        , fastProperty "combinedErrorSeverity takes maximum severity" prop_combinedErrorSeverity_max
        , fastProperty "filterCombinedErrorsBySeverity works correctly" prop_filterBySeverity_correct
        ]

    , testGroup "Error recovery strategies"
        [ fastProperty "canRecoverFrom is False for Fatal errors" prop_cannot_recover_from_fatal
        , fastProperty "canRecoverFrom is True for Warning and Info" prop_can_recover_from_warning_info
        , fastProperty "shouldContinueAfter is True for non-fatal errors" prop_continue_non_fatal
        , fastProperty "customRecovery strategies work correctly" prop_custom_recovery
        ]

    , testGroup "Error context and metadata"
        [ fastProperty "withContext adds context information" prop_withContext_adds
        , fastProperty "withSuggestions adds suggestions" prop_withSuggestions_adds
        , fastProperty "withRelatedErrors adds related errors" prop_withRelatedErrors_adds
        , fastProperty "withTimestamp adds timestamp" prop_withTimestamp_adds
        ]

    , testGroup "Error filtering and analysis"
        [ fastProperty "hasCategory detects categories correctly" prop_hasCategory_detection
        , fastProperty "filterByCategory filters correctly" prop_filterByCategory_correct
        , fastProperty "filterBySeverity filters correctly" prop_filterBySeverity_correct
        , fastProperty "getErrorStatistics counts correctly" prop_getErrorStatistics_counts
        ]

    , testGroup "Error formatting and reporting"
        [ fastProperty "formatError includes error message" prop_formatError_includes_message
        , fastProperty "formatErrors includes all errors" prop_formatErrors_includes_all
        , fastProperty "generateErrorReport includes statistics" prop_generateErrorReport_includes_stats
        ]

    , testGroup "Error collector operations"
        [ fastProperty "addError increases error count" prop_addError_increases
        , fastProperty "addWarning increases warning count" prop_addWarning_increases
        , fastProperty "addInfo increases info count" prop_addInfo_increases
        , fastProperty "hasErrors detects errors correctly" prop_hasErrors_detection
        , fastProperty "hasWarnings detects warnings correctly" prop_hasWarnings_detection
        ]

    , testGroup "Edge cases and robustness"
        [ testCase "handles empty error collector" test_empty_collector
        , testCase "handles error with empty message" test_empty_message
        , testCase "handles error with no location" test_no_location
        , testCase "handles error with many suggestions" test_many_suggestions
        , testCase "handles deeply nested error contexts" test_nested_contexts
        ]
    ]

-- ============================================================================
-- Error Creation and Properties
-- ============================================================================

prop_errorAt_location :: Property
prop_errorAt_location =
  forAll arbitrary $ \loc ->
  forAll arbitrary $ \msg ->
    let error = errorAt loc msg
        actualLoc = errorLocation error
    in property $ actualLoc === loc

prop_errorWithCategory_category :: Property
prop_errorWithCategory_category =
  forAll arbitrary $ \cat ->
  forAll arbitrary $ \msg ->
    let error = errorWithCategory cat msg
        actualCat = errorCategory error
    in property $ actualCat === cat

prop_errorWithSuggestions_preserves :: Property
prop_errorWithSuggestions_preserves =
  forAll arbitrary $ \msg ->
  forAll arbitrary $ \suggestions ->
    let error = errorWithSuggestions msg suggestions
        actualSuggestions = errorSuggestions error
    in property $ actualSuggestions === suggestions

prop_fatalError_severity :: Property
prop_fatalError_severity =
  forAll arbitrary $ \msg ->
    let error = fatalError msg
        severity = errorSeverity error
    in property $ severity === Fatal

-- ============================================================================
-- Error Combination and Merging
-- ============================================================================

prop_combineErrors_preserves :: Property
prop_combineErrors_preserves =
  forAll arbitrary $ \errors1 ->
  forAll arbitrary $ \errors2 ->
    let combined = combineErrors errors1 errors2
        allErrors = errors1 ++ errors2
    in property $ length combined >= length allErrors

prop_combinedErrorSeverity_max :: Property
prop_combinedErrorSeverity_max =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \errors2 ->
    let combined = combineErrors errors errors2
        maxSeverity = maximum (map errorSeverity (errors ++ errors2))
        actualSeverity = combinedErrorSeverity combined
    in property $ actualSeverity === maxSeverity

prop_filterBySeverity_correct :: Property
prop_filterBySeverity_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \severity ->
    let filtered = filterCombinedErrorsBySeverity severity errors
        allMatch = all (\e -> errorSeverity e <= severity) filtered
    in property $ allMatch

-- ============================================================================
-- Error Recovery Strategies
-- ============================================================================

prop_cannot_recover_from_fatal :: Property
prop_cannot_recover_from_fatal =
  forAll arbitrary $ \msg ->
    let error = fatalError msg
        canRecover = canRecoverFrom error
    in property $ canRecover === False

prop_can_recover_from_warning_info :: Property
prop_can_recover_from_warning_info =
  forAll arbitrary $ \msg ->
  forAll arbitrary $ \loc ->
    let warning = warningAt loc msg
        info = infoAt loc msg
        canRecoverWarning = canRecoverFrom warning
        canRecoverInfo = canRecoverFrom info
    in property $ canRecoverWarning .&&. canRecoverInfo

prop_continue_non_fatal :: Property
prop_continue_non_fatal =
  forAll arbitrary $ \msg ->
  forAll arbitrary $ \loc ->
    let warning = warningAt loc msg
        info = infoAt loc msg
        continueWarning = shouldContinueAfter warning
        continueInfo = shouldContinueAfter info
    in property $ continueWarning .&&. continueInfo

prop_custom_recovery :: Property
prop_custom_recovery =
  forAll arbitrary $ \shouldContinue ->
  forAll arbitrary $ \canRecover ->
    let strategy = customRecovery shouldContinue canRecover
        error = fatalError "test" `withRecovery` strategy
    in property $ shouldContinueAfter error === shouldContinue .&&.
                    canRecoverFrom error === canRecover

-- ============================================================================
-- Error Context and Metadata
-- ============================================================================

prop_withContext_adds :: Property
prop_withContext_adds =
  forAll arbitrary $ \error ->
  forAll arbitrary $ \context ->
    let withCtx = withContext context error
        actualContext = errorContext withCtx
    in property $ actualContext === context

prop_withSuggestions_adds :: Property
prop_withSuggestions_adds =
  forAll arbitrary $ \error ->
  forAll arbitrary $ \suggestions ->
    let withSugg = withSuggestions suggestions error
        actualSuggestions = errorSuggestions withSugg
    in property $ actualSuggestions === suggestions

prop_withRelatedErrors_adds :: Property
prop_withRelatedErrors_adds =
  forAll arbitrary $ \error ->
  forAll arbitrary $ \related ->
    let withRel = withRelatedErrors related error
        actualRelated = errorRelatedErrors withRel
    in property $ actualRelated === related

prop_withTimestamp_adds :: Property
prop_withTimestamp_adds =
  forAll arbitrary $ \error ->
  forAll arbitrary $ \timestamp ->
    let withTS = withTimestamp timestamp error
        actualTS = errorTimestamp withTS
    in property $ actualTS === Just timestamp

-- ============================================================================
-- Error Filtering and Analysis
-- ============================================================================

prop_hasCategory_detection :: Property
prop_hasCategory_detection =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \category ->
    let hasCat = hasCategory category errors
        anyHasCat = any (\e -> errorCategory e == category) errors
    in property $ hasCat === anyHasCat

prop_filterByCategory_correct :: Property
prop_filterByCategory_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \category ->
    let filtered = filterByCategory category errors
        allMatch = all (\e -> errorCategory e == category) filtered
    in property $ allMatch

prop_filterBySeverity_correct :: Property
prop_filterBySeverity_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \severity ->
    let filtered = filterBySeverity severity errors
        allMatch = all (\e -> errorSeverity e == severity) filtered
    in property $ allMatch

prop_getErrorStatistics_counts :: Property
prop_getErrorStatistics_counts =
  forAll arbitrary $ \errors ->
    let stats = getErrorStatistics errors
        errorCount = length $ filter (\e -> errorSeverity e == Error) errors
        warningCount = length $ filter (\e -> errorSeverity e == Warning) errors
        infoCount = length $ filter (\e -> errorSeverity e == Info) errors
        fatalCount = length $ filter (\e -> errorSeverity e == Fatal) errors
    in property $ statsErrorCount stats == errorCount .&&.
                    statsWarningCount stats == warningCount .&&.
                    statsInfoCount stats == infoCount .&&.
                    statsFatalCount stats == fatalCount

-- ============================================================================
-- Error Formatting and Reporting
-- ============================================================================

prop_formatError_includes_message :: Property
prop_formatError_includes_message =
  forAll arbitrary $ \error ->
    let formatted = formatError error
        message = errorMessage error
    in property $ message `T.isInfixOf` formatted

prop_formatErrors_includes_all :: Property
prop_formatErrors_includes_all =
  forAll arbitrary $ \errors ->
    let formatted = formatErrors errors
        messages = map errorMessage errors
    in property $ all (`T.isInfixOf` formatted) messages

prop_generateErrorReport_includes_stats :: Property
prop_generateErrorReport_includes_stats =
  forAll arbitrary $ \errors ->
    let report = generateErrorReport errors
        hasStats = "Error Statistics" `T.isInfixOf` report
    in property $ hasStats

-- ============================================================================
-- Error Collector Operations
-- ============================================================================

prop_addError_increases :: Property
prop_addError_increases =
  forAll arbitrary $ \error ->
    let collector1 = newErrorCollector
        collector2 = addError error collector1
        errors1 = getErrors collector1
        errors2 = getErrors collector2
    in property $ length errors2 === length errors1 + 1

prop_addWarning_increases :: Property
prop_addWarning_increases =
  forAll arbitrary $ \warning ->
    let collector1 = newErrorCollector
        collector2 = addWarning warning collector1
        warnings1 = getWarnings collector1
        warnings2 = getWarnings collector2
    in property $ length warnings2 === length warnings1 + 1

prop_addInfo_increases :: Property
prop_addInfo_increases =
  forAll arbitrary $ \info ->
    let collector1 = newErrorCollector
        collector2 = addInfo info collector1
        infos1 = getInfo collector1
        infos2 = getInfo collector2
    in property $ length infos2 === length infos1 + 1

prop_hasErrors_detection :: Property
prop_hasErrors_detection =
  forAll arbitrary $ \errors ->
    let collector = foldr addError newErrorCollector errors
        hasErr = hasErrors collector
        actualErrors = getErrors collector
    in property $ hasErr === (not (null actualErrors))

prop_hasWarnings_detection :: Property
prop_hasWarnings_detection =
  forAll arbitrary $ \warnings ->
    let collector = foldr addWarning newErrorCollector warnings
        hasWarn = hasWarnings collector
        actualWarnings = getWarnings collector
    in property $ hasWarn === (not (null actualWarnings))

-- ============================================================================
-- Edge Cases and Robustness Tests
-- ============================================================================

test_empty_collector :: IO ()
test_empty_collector = do
  let collector = newErrorCollector
  getErrors collector @?= []
  getWarnings collector @?= []
  getInfo collector @?= []
  hasErrors collector @?= False
  hasWarnings collector @?= False

test_empty_message :: IO ()
test_empty_message = do
  let error = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) ""
      formatted = formatError error
  "" `T.isInfixOf` formatted @?= True

test_no_location :: IO ()
test_no_location = do
  let error = fatalError "test message"
      loc = errorLocation error
  filePath loc @?= Nothing
  line loc @?= 0
  column loc @?= 0

test_many_suggestions :: IO ()
test_many_suggestions = do
  let suggestions = map (("suggestion " ++) . show) [1..100]
      error = errorWithSuggestions "test error" suggestions
      actualSuggestions = errorSuggestions error
  length actualSuggestions @?= 100

test_nested_contexts :: IO ()
test_nested_contexts = do
  let context1 = emptyContext { contextFunction = "func1" }
      context2 = emptyContext { contextFunction = "func2", contextParent = Just context1 }
      context3 = emptyContext { contextFunction = "func3", contextParent = Just context2 }
      error = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) "test" `withContext` context3
      actualContext = errorContext error
  contextFunction actualContext @?= "func3"
  contextParent actualContext @?= Just context2