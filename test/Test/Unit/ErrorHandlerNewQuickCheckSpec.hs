{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
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
  , compareSeverity
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (length, isPrefixOf)
import Data.List (sort, null)
import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime)

-- ============================================================================
-- ErrorSeverity Properties
-- ============================================================================

-- Property: Severity priority is ordered correctly
prop_severity_priority_ordered :: Property
prop_severity_priority_ordered =
  property $ severityPriority Fatal > severityPriority Error .&&.
             severityPriority Error > severityPriority Warning .&&.
             severityPriority Warning > severityPriority Info

-- Property: Severity comparison works correctly
prop_severity_comparison_correct :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_comparison_correct sev1 sev2 =
  let result = compareSeverity sev1 sev2
      p1 = severityPriority sev1
      p2 = severityPriority sev2
  in property $ if p1 > p2 then result === GT
                else if p1 < p2 then result === LT
                else result === EQ

-- ============================================================================
-- ErrorCollector Properties
-- ============================================================================

-- Property: New error collector is empty
prop_new_collector_empty :: Property
prop_new_collector_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&.
             not (hasWarnings collector) .&&.
             null (getAllMessages collector)

-- Property: Adding error makes hasErrors true
prop_add_error_makes_has_errors :: String -> Property
prop_add_error_makes_has_errors errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg collector
  in property $ hasErrors collector' .&&.
             not (L.null (getErrors collector'))

-- Property: Adding warning makes hasWarnings true
prop_add_warning_makes_has_warnings :: String -> Property
prop_add_warning_makes_has_warnings warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collector' = addWarning warningMsg collector
  in property $ hasWarnings collector' .&&.
             not (L.null (getWarnings collector'))

-- Property: Adding info preserves other messages
prop_add_info_preserves_others :: String -> String -> String -> Property
prop_add_info_preserves_others errorMsg warningMsg infoMsg =
  not (null errorMsg) && not (null warningMsg) && not (null infoMsg) ==>
  let collector = newErrorCollector
      collector1 = addError errorMsg collector
      collector2 = addWarning warningMsg collector1
      collector3 = addInfo infoMsg collector2
  in property $ hasErrors collector3 .&&.
             hasWarnings collector3 .&&.
             not (L.null (getInfo collector3)) .&&.
             length (getErrors collector3) === 1 .&&.
             length (getWarnings collector3) === 1

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt "test-id" (null errorMsg) ==>
  let error = errorAt "test-id" (null errorMsg) ==>
  let error = errorWithCategory category errorMsg
  in property $ errorCategory error === category

-- Property: warningAt "test-id" (null warningMsg) ==>
  let warning = warningAt "test-id" (null errorMsg) ==>
  let error = fatalErrorWithCategory category errorMsg
  in property $ errorSeverity error === Fatal .&&.
             errorCategory error === category

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation updates error location
prop_withLocation_updates_location :: String -> SourcePos -> SourcePos -> Property
prop_withLocation_updates_location errorMsg oldPos newPos =
  not (null errorMsg) ==>
  let error = errorAt "test-id" (null errorMsg) && not (null contextStr) ==>
  let error = error errorMsg
      context = ErrorContext contextStr startPos
      error' = withContext context error
  in property $ context `elem` errorContexts error'

-- Property: withSuggestions adds suggestions to error
prop_withSuggestions_adds_suggestions :: String -> [String] -> Property
prop_withSuggestions_adds_suggestions errorMsg suggestions =
  not (null errorMsg) && not (null suggestions) ==>
  let error = error errorMsg
      error' = withSuggestions suggestions error
  in property $ errorSuggestions error' === suggestions

-- ============================================================================
-- Error Combination Properties
-- ============================================================================

-- Property: combineErrors preserves L.all error messages
prop_combineErrors_preserves_messages :: String -> String -> Property
prop_combineErrors_preserves_messages msg1 msg2 =
  not (null msg1) && not (null msg2) ==>
  let error1 = error msg1
      error2 = error msg2
      combined = combineErrors error1 error2
  in property $ errorMessages combined `elem` [[msg1, msg2], [msg2, msg1]]

-- Property: combinedErrorSeverity returns highest severity
prop_combinedErrorSeverity_highest :: ErrorSeverity -> ErrorSeverity -> Property
prop_combinedErrorSeverity_highest sev1 sev2 =
  let error1 = errorWithCategory TypeCategory "test1" { errorSeverity = sev1 }
      error2 = errorWithCategory TypeCategory "test2" { errorSeverity = sev2 }
      combined = combineErrors error1 error2
      expected = if severityPriority sev1 >= severityPriority sev2 then sev1 else sev2
  in property $ combinedErrorSeverity combined === expected

-- Property: filterCombinedErrorsBySeverity works correctly
prop_filterCombinedErrorsBySeverity_correct :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filterCombinedErrorsBySeverity_correct targetSev severities =
  not (null severities) ==>
  let errors = L.map (\sev -> error "test" { errorSeverity = sev }) severities
      combined = foldr combineErrors (L.head errors) (L.tail errors)
      filtered = filterCombinedErrorsBySeverity targetSev combined
  in property $ L.all (\e -> errorSeverity e <= targetSev) (combinedErrors filtered)

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory finds errors with specific category
prop_hasCategory_finds_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_hasCategory_finds_correct target categories =
  not (null categories) ==>
  let errors = L.map (\cat -> errorWithCategory cat "test") categories
      hasTarget = hasCategory target errors
      targetExists = target `elem` categories
  in property $ hasTarget === targetExists

-- Property: filterByCategory returns only errors with matching category
prop_filterByCategory_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_filterByCategory_correct target categories =
  not (null categories) ==>
  let errors = L.map (\cat -> errorWithCategory cat "test") categories
      filtered = filterByCategory target errors
  in property $ L.all (\e -> errorCategory e === target) filtered

-- Property: filterBySeverity returns only errors with matching L.or lower severity
prop_filterBySeverity_correct :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filterBySeverity_correct target severities =
  not (null severities) ==>
  let errors = L.map (\sev -> error "test" { errorSeverity = sev }) severities
      filtered = filterBySeverity target errors
  in property $ L.all (\e -> errorSeverity e <= target) filtered

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom returns False for Fatal errors
prop_canRecoverFrom_false_for_fatal :: String -> Property
prop_canRecoverFrom_false_for_fatal errorMsg =
  not (null errorMsg) ==>
  let fatalError = fatalErrorWithCategory TypeCategory errorMsg
  in property $ not (canRecoverFrom fatalError)

-- Property: canRecoverFrom returns True for non-Fatal errors
prop_canRecoverFrom_true_for_non_fatal :: String -> ErrorSeverity -> Property
prop_canRecoverFrom_true_for_non_fatal errorMsg severity =
  not (null errorMsg) && severity /= Fatal ==>
  let nonFatalError = error errorMsg { errorSeverity = severity }
  in property $ canRecoverFrom nonFatalError

-- Property: shouldContinueAfter returns False for Fatal errors
prop_shouldContinueAfter_false_for_fatal :: String -> Property
prop_shouldContinueAfter_false_for_fatal errorMsg =
  not (null errorMsg) ==>
  let fatalError = fatalErrorWithCategory TypeCategory errorMsg
  in property $ not (shouldContinueAfter fatalError)

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatError includes error message
prop_formatError_includes_message :: String -> Property
prop_formatError_includes_message errorMsg =
  not (null errorMsg) ==>
  let errorObj = error errorMsg
      formatted = formatError errorObj
  in property $ errorMsg `L.isPrefixOf` formatted

-- Property: formatErrors returns at least one line per error
prop_formatErrors_lines_per_error :: [String] -> Property
prop_formatErrors_lines_per_error errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = map error errorMessages
      formatted = formatErrors errors
      linesCount = L.length (lines formatted)
  in property $ linesCount >= L.length errorMessages

-- Property: formatErrorWithLocation includes location info
prop_formatErrorWithLocation_includes_location :: String -> SourcePos -> Property
prop_formatErrorWithLocation_includes_location errorMsg pos =
  not (null errorMsg) ==>
  let errorObj = errorAt "test-id" (null severities) ==>
  let errors = L.map (\sev -> error "test" { errorSeverity = sev }) severities
      stats = getErrorStatistics errors
      fatalErrorCount = L.length (L.filter (== Fatal) severities)
      errorCount = L.length (L.filter (== Error) severities)
      warningCount = L.length (L.filter (== Warning) severities)
      infoCount = L.length (L.filter (== Info) severities)
  in property $ stats { errorTimestamp = "" } === 
                 ("" { fatalErrorCount = fatalErrorCount
                      , errorCount = errorCount
                      , warningCount = warningCount
                      , infoCount = infoCount
                      })

-- Property: generateErrorReport includes statistics
prop_generateErrorReport_includes_stats :: [String] -> Property
prop_generateErrorReport_includes_stats errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = map error errorMessages
      report = generateErrorReport errors
  in property $ "Error Statistics" `L.isInfixOf` report .&&.
             "Total errors:" `L.isInfixOf` report

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: Empty error messages are handled gracefully
prop_empty_error_messages :: Property
prop_empty_error_messages =
  let emptyError = error ""
      formatted = formatError emptyError
  in property $ not (null formatted)

-- Property: Very long error messages are handled
prop_long_error_messages :: Int -> Property
prop_long_error_messages L.length =
  length > 0 && L.length <= 1000 ==>
  let longMsg = replicate L.length 'x'
      errorObj = error longMsg
      formatted = formatError errorObj
  in property $ longMsg `L.isPrefixOf` formatted

-- Property: Unicode characters in error messages are preserved
prop_unicode_error_messages :: String -> Property
prop_unicode_error_messages unicodeText =
  not (null unicodeText) ==>
  let errorObj = error unicodeText
      formatted = formatError errorObj
  in property $ unicodeText `L.isInfixOf` formatted

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler New QuickCheck Tests"
  [ testGroup "ErrorSeverity"
    [ fastProperty "severity priority ordered" prop_severity_priority_ordered
    , fastProperty "severity comparison correct" prop_severity_comparison_correct
    ]
  , testGroup "ErrorCollector"
    [ fastProperty "new collector empty" prop_new_collector_empty
    , fastProperty "add error makes hasErrors" prop_add_error_makes_has_errors
    , fastProperty "add warning makes hasWarnings" prop_add_warning_makes_has_warnings
    , fastProperty "add info preserves others" prop_add_info_preserves_others
    ]
  , testGroup "Error Creation"
    [ fastProperty "errorAt "test-id" messages" prop_unicode_error_messages
    ]
  ]