{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerNewQuickCheckSpec (tests) where

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
  , severityPriority
  , compareSeverity
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, length, null, isPrefixOf)
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
             not (null (getErrors collector'))

-- Property: Adding warning makes hasWarnings true
prop_add_warning_makes_has_warnings :: String -> Property
prop_add_warning_makes_has_warnings warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collector' = addWarning warningMsg collector
  in property $ hasWarnings collector' .&&.
             not (null (getWarnings collector'))

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
             not (null (getInfo collector3)) .&&.
             length (getErrors collector3) === 1 .&&.
             length (getWarnings collector3) === 1

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates error with correct location
prop_errorAt_correct_location :: String -> SourcePos -> Property
prop_errorAt_correct_location errorMsg pos =
  not (null errorMsg) ==>
  let error = errorAt errorMsg pos
  in property $ errorLocation error === ErrorLocation pos pos

-- Property: errorWithCategory creates error with correct category
prop_errorWithCategory_correct_category :: String -> ErrorCategory -> Property
prop_errorWithCategory_correct_category errorMsg category =
  not (null errorMsg) ==>
  let error = errorWithCategory category errorMsg
  in property $ errorCategory error === category

-- Property: warningAt creates warning with correct severity
prop_warningAt_correct_severity :: String -> SourcePos -> Property
prop_warningAt_correct_severity warningMsg pos =
  not (null warningMsg) ==>
  let warning = warningAt warningMsg pos
  in property $ errorSeverity warning === Warning

-- Property: fatalErrorWithCategory creates fatal error
prop_fatal_error_with_category :: String -> ErrorCategory -> Property
prop_fatal_error_with_category errorMsg category =
  not (null errorMsg) ==>
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
  let error = errorAt errorMsg oldPos
      error' = withLocation newPos error
  in property $ errorLocation error' === ErrorLocation newPos newPos

-- Property: withContext adds context to error
prop_withContext_adds_context :: String -> String -> Property
prop_withContext_adds_context errorMsg contextStr =
  not (null errorMsg) && not (null contextStr) ==>
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

-- Property: combineErrors preserves all error messages
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
  let errors = map (\sev -> error "test" { errorSeverity = sev }) severities
      combined = foldr combineErrors (head errors) (tail errors)
      filtered = filterCombinedErrorsBySeverity targetSev combined
  in property $ all (\e -> errorSeverity e <= targetSev) (combinedErrors filtered)

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory finds errors with specific category
prop_hasCategory_finds_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_hasCategory_finds_correct target categories =
  not (null categories) ==>
  let errors = map (\cat -> errorWithCategory cat "test") categories
      hasTarget = hasCategory target errors
      targetExists = target `elem` categories
  in property $ hasTarget === targetExists

-- Property: filterByCategory returns only errors with matching category
prop_filterByCategory_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_filterByCategory_correct target categories =
  not (null categories) ==>
  let errors = map (\cat -> errorWithCategory cat "test") categories
      filtered = filterByCategory target errors
  in property $ all (\e -> errorCategory e === target) filtered

-- Property: filterBySeverity returns only errors with matching or lower severity
prop_filterBySeverity_correct :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filterBySeverity_correct target severities =
  not (null severities) ==>
  let errors = map (\sev -> error "test" { errorSeverity = sev }) severities
      filtered = filterBySeverity target errors
  in property $ all (\e -> errorSeverity e <= target) filtered

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
  in property $ errorMsg `isPrefixOf` formatted

-- Property: formatErrors returns at least one line per error
prop_formatErrors_lines_per_error :: [String] -> Property
prop_formatErrors_lines_per_error errorMessages =
  not (null errorMessages) && all (not . null) errorMessages ==>
  let errors = map error errorMessages
      formatted = formatErrors errors
      linesCount = length (lines formatted)
  in property $ linesCount >= length errorMessages

-- Property: formatErrorWithLocation includes location info
prop_formatErrorWithLocation_includes_location :: String -> SourcePos -> Property
prop_formatErrorWithLocation_includes_location errorMsg pos =
  not (null errorMsg) ==>
  let errorObj = errorAt errorMsg pos
      formatted = formatErrorWithLocation errorObj
  in property $ "line:" `isInfixOf` formatted .&&.
             "column:" `isInfixOf` formatted

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: getErrorStatistics counts errors correctly
prop_getErrorStatistics_counts_correct :: [ErrorSeverity] -> Property
prop_getErrorStatistics_counts_correct severities =
  not (null severities) ==>
  let errors = map (\sev -> error "test" { errorSeverity = sev }) severities
      stats = getErrorStatistics errors
      fatalErrorCount = length (filter (== Fatal) severities)
      errorCount = length (filter (== Error) severities)
      warningCount = length (filter (== Warning) severities)
      infoCount = length (filter (== Info) severities)
  in property $ stats { errorTimestamp = "" } === 
                 ("" { fatalErrorCount = fatalErrorCount
                      , errorCount = errorCount
                      , warningCount = warningCount
                      , infoCount = infoCount
                      })

-- Property: generateErrorReport includes statistics
prop_generateErrorReport_includes_stats :: [String] -> Property
prop_generateErrorReport_includes_stats errorMessages =
  not (null errorMessages) && all (not . null) errorMessages ==>
  let errors = map error errorMessages
      report = generateErrorReport errors
  in property $ "Error Statistics" `isInfixOf` report .&&.
             "Total errors:" `isInfixOf` report

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
prop_long_error_messages length =
  length > 0 && length <= 1000 ==>
  let longMsg = replicate length 'x'
      errorObj = error longMsg
      formatted = formatError errorObj
  in property $ longMsg `isPrefixOf` formatted

-- Property: Unicode characters in error messages are preserved
prop_unicode_error_messages :: String -> Property
prop_unicode_error_messages unicodeText =
  not (null unicodeText) ==>
  let errorObj = error unicodeText
      formatted = formatError errorObj
  in property $ unicodeText `isInfixOf` formatted

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
    [ fastProperty "errorAt correct location" prop_errorAt_correct_location
    , fastProperty "errorWithCategory correct category" prop_errorWithCategory_correct_category
    , fastProperty "warningAt correct severity" prop_warningAt_correct_severity
    , fastProperty "fatalErrorWithCategory creates fatal" prop_fatal_error_with_category
    ]
  , testGroup "Error Modification"
    [ fastProperty "withLocation updates location" prop_withLocation_updates_location
    , fastProperty "withContext adds context" prop_withContext_adds_context
    , fastProperty "withSuggestions adds suggestions" prop_withSuggestions_adds_suggestions
    ]
  , testGroup "Error Combination"
    [ fastProperty "combineErrors preserves messages" prop_combineErrors_preserves_messages
    , fastProperty "combinedErrorSeverity highest" prop_combinedErrorSeverity_highest
    , fastProperty "filterCombinedErrorsBySeverity correct" prop_filterCombinedErrorsBySeverity_correct
    ]
  , testGroup "Error Filtering"
    [ fastProperty "hasCategory finds correct" prop_hasCategory_finds_correct
    , fastProperty "filterByCategory correct" prop_filterByCategory_correct
    , fastProperty "filterBySeverity correct" prop_filterBySeverity_correct
    ]
  , testGroup "Error Recovery"
    [ fastProperty "canRecoverFrom false for fatal" prop_canRecoverFrom_false_for_fatal
    , fastProperty "canRecoverFrom true for non-fatal" prop_canRecoverFrom_true_for_non_fatal
    , fastProperty "shouldContinueAfter false for fatal" prop_shouldContinueAfter_false_for_fatal
    ]
  , testGroup "Error Formatting"
    [ fastProperty "formatError includes message" prop_formatError_includes_message
    , fastProperty "formatErrors lines per error" prop_formatErrors_lines_per_error
    , fastProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocation_includes_location
    ]
  , testGroup "Error Statistics"
    [ fastProperty "getErrorStatistics counts correct" prop_getErrorStatistics_counts_correct
    , fastProperty "generateErrorReport includes stats" prop_generateErrorReport_includes_stats
    ]
  , testGroup "Edge Cases"
    [ fastProperty "empty error messages" prop_empty_error_messages
    , fastProperty "long error messages" prop_long_error_messages
    , fastProperty "unicode error messages" prop_unicode_error_messages
    ]
  ]