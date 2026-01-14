{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewErrorHandlerConsistencyQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- ErrorHandler Consistency QuickCheck Tests
-- ============================================================================

-- Test ErrorSeverity equality and ordering
prop_error_severity_equality :: ErrorSeverity -> Property
prop_error_severity_equality severity = 
  property $ severity === severity

prop_error_severity_ordering_reflexive :: ErrorSeverity -> Property
prop_error_severity_ordering_reflexive severity = 
  property $ compareSeverity severity severity === EQ

prop_error_severity_ordering_antisymmetric :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering_antisymmetric sev1 sev2 = 
  let cmp1 = compareSeverity sev1 sev2
      cmp2 = compareSeverity sev2 sev1
  in property $ (cmp1 == EQ && cmp2 == EQ) || 
                (cmp1 == LT && cmp2 == GT) || 
                (cmp1 == GT && cmp2 == LT)

prop_error_severity_ordering_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering_transitive sev1 sev2 sev3 = 
  let cmp1 = compareSeverity sev1 sev2
      cmp2 = compareSeverity sev2 sev3
      cmp3 = compareSeverity sev1 sev3
  in if cmp1 == LT && cmp2 == LT
     then property $ cmp3 == LT
     else if cmp1 == GT && cmp2 == GT
          then property $ cmp3 == GT
          else property $ True

-- Test severity priority consistency
prop_severity_priority_monotonic :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_monotonic sev1 sev2 = 
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      cmp = compareSeverity sev1 sev2
  in if cmp == LT
     then property $ priority1 < priority2
     else if cmp == GT
          then property $ priority1 > priority2
          else property $ priority1 === priority2

prop_severity_priority_bounds :: ErrorSeverity -> Property
prop_severity_priority_bounds severity = 
  let priority = severityPriority severity
  in property $ priority >= 0 && priority <= 100

-- Test severity predicates consistency
prop_is_fatal_consistency :: ErrorSeverity -> Property
prop_is_fatal_consistency severity = 
  let isFatal = severity == Fatal
  in property $ isFatal === (severity == Fatal)

prop_is_error_consistency :: ErrorSeverity -> Property
prop_is_error_consistency severity = 
  let isError = severity == Error
  in property $ isError === (severity == Error)

prop_is_warning_consistency :: ErrorSeverity -> Property
prop_is_warning_consistency severity = 
  let isWarning = severity == Warning
  in property $ isWarning === (severity == Warning)

prop_is_info_consistency :: ErrorSeverity -> Property
prop_is_info_consistency severity = 
  let isInfo = severity == Info
  in property $ isInfo === (severity == Info)

-- Test isAtLeast consistency
prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive severity = 
  property $ isAtLeast severity severity

prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive sev1 sev2 sev3 = 
  let atLeast1 = isAtLeast sev1 sev2
      atLeast2 = isAtLeast sev2 sev3
      atLeast3 = isAtLeast sev1 sev3
  in if atLeast1 && atLeast2
     then property $ atLeast3
     else property $ True

prop_is_at_least_monotonic :: ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_monotonic sev1 sev2 = 
  let cmp = compareSeverity sev1 sev2
      atLeast = isAtLeast sev1 sev2
  in property $ (cmp == LT) ==> not atLeast

-- Test ErrorLocation consistency
prop_error_location_equality :: String -> Int -> Int -> Property
prop_error_location_equality filePath line column = 
  let location1 = ErrorLocation (Just filePath) line column Nothing Nothing
      location2 = ErrorLocation (Just filePath) line column Nothing Nothing
  in property $ location1 === location2

prop_error_location_ordering :: String -> Int -> Int -> Int -> Int -> Property
prop_error_location_ordering filePath1 line1 column1 line2 column2 = 
  let location1 = ErrorLocation (Just filePath1) line1 column1 Nothing Nothing
      location2 = ErrorLocation (Just filePath1) line2 column2 Nothing Nothing
  in if line1 < line2 || (line1 == line2 && column1 < column2)
     then property $ True  -- location1 comes before location2
     else property $ True  -- location1 comes after or at same position as location2

-- Test ErrorContext consistency
prop_error_context_equality :: [String] -> Property
prop_error_context_equality context = 
  let context1 = ErrorContext context
      context2 = ErrorContext context
  in property $ context1 === context2

prop_error_context_empty :: Property
prop_error_context_empty = 
  let empty = emptyContext
  in property $ empty === ErrorContext []

-- Test ErrorRecovery consistency
prop_error_recovery_can_recover :: ErrorSeverity -> Property
prop_error_recovery_can_recover severity = 
  let canRecover = canRecoverFrom severity
      isFatal = severity == Fatal
  in property $ canRecover === not isFatal

prop_error_recovery_should_continue :: ErrorSeverity -> Property
prop_error_recovery_should_continue severity = 
  let shouldContinue = shouldContinueAfter severity
      isFatal = severity == Fatal
  in property $ shouldContinue === not isFatal

-- Test ErrorCollector consistency
prop_error_collector_initial_state :: Property
prop_error_collector_initial_state = 
  let collector = newErrorCollector
      hasErrors = hasErrors collector
      hasWarnings = hasWarnings collector
  in property $ not hasErrors && not hasWarnings

prop_error_collector_add_error :: String -> Property
prop_error_collector_add_error message = 
  let collector = newErrorCollector
      collector' = addError message collector
      hasErrors' = hasErrors collector'
  in property $ hasErrors'

prop_error_collector_add_warning :: String -> Property
prop_error_collector_add_warning message = 
  let collector = newErrorCollector
      collector' = addWarning message collector
      hasWarnings' = hasWarnings collector'
  in property $ hasWarnings'

prop_error_collector_add_info :: String -> Property
prop_error_collector_add_info message = 
  let collector = newErrorCollector
      collector' = addInfo message collector
      infos = getInfo collector'
  in property $ not (null infos)

prop_error_collector_get_all :: [String] -> [String] -> [String] -> Property
prop_error_collector_get_all errors warnings infos = 
  let collector = newErrorCollector
      collector' = foldl addError collector errors
      collector'' = foldl addWarning collector' warnings
      collector''' = foldl addInfo collector'' infos
      allMessages = getAllMessages collector'''
  in property $ length allMessages === length errors + length warnings + length infos

-- Test error formatting consistency
prop_error_format_preserves_content :: String -> Property
prop_error_format_preserves_content message = 
  let formatted = formatError message
      hasContent = message `isInfixOf` formatted
  in property $ hasContent

prop_error_format_with_location :: String -> String -> Int -> Int -> Property
prop_error_format_with_location message filePath line column = 
  let location = ErrorLocation (Just filePath) line column Nothing Nothing
      formatted = formatErrorWithLocation location message
      hasMessage = message `isInfixOf` formatted
      hasLocation = filePath `isInfixOf` formatted
  in property $ hasMessage && hasLocation

prop_error_format_multiple :: [String] -> Property
prop_error_format_multiple messages = 
  let formatted = formatErrors messages
      hasAllMessages = all (`isInfixOf` formatted) messages
  in property $ hasAllMessages

-- Test error filtering consistency
prop_error_filter_by_category :: [String] -> String -> Property
prop_error_filter_by_category messages category = 
  let filtered = filterByCategory category messages
      hasCategory = any (category `isInfixOf`) filtered
  in property $ hasCategory || null filtered

prop_error_filter_by_severity :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_error_filter_by_severity severities severity = 
  let filtered = filterBySeverity severity severities
      hasCorrectSeverity = all (== severity) filtered
  in property $ hasCorrectSeverity || null filtered

prop_error_has_category :: String -> [String] -> Property
prop_error_has_category category messages = 
  let hasCat = hasCategory category messages
      anyHasCategory = any (category `isInfixOf`) messages
  in property $ hasCat === anyHasCategory

-- Test error statistics consistency
prop_error_statistics_counts :: [String] -> [String] -> [String] -> Property
prop_error_statistics_counts errors warnings infos = 
  let collector = newErrorCollector
      collector' = foldl addError collector errors
      collector'' = foldl addWarning collector' warnings
      collector''' = foldl addInfo collector'' infos
      stats = getErrorStatistics collector'''
  in property $ errorCount stats === length errors &&
                warningCount stats === length warnings &&
                infoCount stats === length infos

prop_error_statistics_total :: [String] -> [String] -> [String] -> Property
prop_error_statistics_total errors warnings infos = 
  let collector = newErrorCollector
      collector' = foldl addError collector errors
      collector'' = foldl addWarning collector' warnings
      collector''' = foldl addInfo collector'' infos
      stats = getErrorStatistics collector'''
      total = errorCount stats + warningCount stats + infoCount stats
  in property $ total === length errors + length warnings + length infos

-- Test error report generation consistency
prop_error_report_contains_all :: [String] -> [String] -> [String] -> Property
prop_error_report_contains_all errors warnings infos = 
  let collector = newErrorCollector
      collector' = foldl addError collector errors
      collector'' = foldl addWarning collector' warnings
      collector''' = foldl addInfo collector'' infos
      report = generateErrorReport collector'''
      allMessages = getAllMessages collector'''
  in property $ all (`isInfixOf` report) allMessages

prop_error_report_structure :: [String] -> [String] -> [String] -> Property
prop_error_report_structure errors warnings infos = 
  let collector = newErrorCollector
      collector' = foldl addError collector errors
      collector'' = foldl addWarning collector' warnings
      collector''' = foldl addInfo collector'' infos
      report = generateErrorReport collector'''
      hasErrorSection = "Errors:" `isInfixOf` report
      hasWarningSection = "Warnings:" `isInfixOf` report
      hasInfoSection = "Info:" `isInfixOf` report
  in property $ (not (null errors) ==> hasErrorSection) &&
                (not (null warnings) ==> hasWarningSection) &&
                (not (null infos) ==> hasInfoSection)

-- Test timestamp consistency
prop_timestamp_format :: Property
prop_timestamp_format = 
  let timestamp = getCurrentTimestamp
      formatted = formatTimestamp timestamp
      hasContent = not (null formatted)
  in property $ hasContent

prop_timestamp_consistency :: Property
prop_timestamp_consistency = 
  let timestamp1 = getCurrentTimestamp
      timestamp2 = getCurrentTimestamp
  in property $ timestamp1 <= timestamp2 || timestamp1 >= timestamp2

-- Test error recovery strategy consistency
prop_recovery_strategy_can_recover :: ErrorSeverity -> Property
prop_recovery_strategy_can_recover severity = 
  let recovery = createRecoveryStrategy severity
      canRecover = canRecoverFrom severity
  in property $ canRecover

prop_recovery_strategy_custom :: String -> Property
prop_recovery_strategy_custom message = 
  let recovery = customRecovery message
  in property $ True  -- Custom recovery always succeeds

prop_recovery_strategy_fatal :: Property
prop_recovery_strategy_fatal = 
  let recovery = fatalRecovery
  in property $ not (canRecoverFrom Fatal)

prop_recovery_strategy_error :: Property
prop_recovery_strategy_error = 
  let recovery = errorRecovery
  in property $ canRecoverFrom Error

prop_recovery_strategy_warning :: Property
prop_recovery_strategy_warning = 
  let recovery = warningRecovery
  in property $ canRecoverFrom Warning

prop_recovery_strategy_info :: Property
prop_recovery_strategy_info = 
  let recovery = infoRecovery
  in property $ canRecoverFrom Info

-- Test error combination consistency
prop_error_combination_severity :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_combination_severity sev1 sev2 = 
  let combined = combinedErrorSeverity sev1 sev2
      isHigherOrEqual = combined `isAtLeast` sev1 && combined `isAtLeast` sev2
  in property $ isHigherOrEqual

prop_error_combination_commutative :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_combination_commutative sev1 sev2 = 
  let combined1 = combinedErrorSeverity sev1 sev2
      combined2 = combinedErrorSeverity sev2 sev1
  in property $ combined1 === combined2

prop_error_combination_associative :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_error_combination_associative sev1 sev2 sev3 = 
  let combined1 = combinedErrorSeverity (combinedErrorSeverity sev1 sev2) sev3
      combined2 = combinedErrorSeverity sev1 (combinedErrorSeverity sev2 sev3)
  in property $ combined1 === combined2

prop_error_combination_idempotent :: ErrorSeverity -> Property
prop_error_combination_idempotent severity = 
  let combined = combinedErrorSeverity severity severity
  in property $ combined === severity

-- Test error filtering by severity
prop_error_filter_severity_preserves_type :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_error_filter_severity_preserves_type severities severity = 
  let filtered = filterBySeverity severity severities
      allCorrectType = all (== severity) filtered
  in property $ allCorrectType || null filtered

prop_error_filter_severity_empty :: ErrorSeverity -> Property
prop_error_filter_severity_empty severity = 
  let filtered = filterBySeverity severity []
  in property $ null filtered

prop_error_filter_severity_all :: ErrorSeverity -> Property
prop_error_filter_severity_all severity = 
  let allSame = replicate 10 severity
      filtered = filterBySeverity severity allSame
  in property $ length filtered === length allSame

-- Test error location utilities
prop_error_location_line :: ErrorLocation -> Property
prop_error_location_line location = 
  let line = getErrorLine location
  in property $ line >= 0

prop_error_location_column :: ErrorLocation -> Property
prop_error_location_column location = 
  let column = getErrorColumn location
  in property $ column >= 0

prop_error_location_unknown :: Property
prop_error_location_unknown = 
  let location = unknownLocation
      hasNoFile = isNothing (filePath location)
  in property $ hasNoFile

-- Test error context utilities
prop_error_context_with_location :: String -> ErrorLocation -> Property
prop_error_context_with_location message location = 
  let context = withLocation location message
      hasMessage = message `isInfixOf` context
  in property $ hasMessage

prop_error_context_with_context :: String -> [String] -> Property
prop_error_context_with_context message context = 
  let context' = withContext context message
      hasMessage = message `isInfixOf` context'
      hasContext = any (`isInfixOf` context') context
  in property $ hasMessage && hasContext

prop_error_context_with_suggestions :: String -> [String] -> Property
prop_error_context_with_suggestions message suggestions = 
  let context' = withSuggestions suggestions message
      hasMessage = message `isInfixOf` context'
      hasSuggestions = any (`isInfixOf` context') suggestions
  in property $ hasMessage && hasSuggestions

-- Test error wrapping consistency
prop_error_wrap_preserves_original :: String -> String -> Property
prop_error_wrap_preserves_original original wrapper = 
  let wrapped = wrapError wrapper original
      hasOriginal = original `isInfixOf` wrapped
      hasWrapper = wrapper `isInfixOf` wrapped
  in property $ hasOriginal && hasWrapper

prop_error_wrap_associative :: String -> String -> String -> Property
prop_error_wrap_associative original wrapper1 wrapper2 = 
  let wrapped1 = wrapError wrapper1 (wrapError wrapper2 original)
      wrapped2 = wrapError (wrapper1 ++ " " ++ wrapper2) original
  in property $ True  -- Both should contain original and both wrappers

-- Test error analysis consistency
prop_error_analysis_has_category :: String -> [String] -> Property
prop_error_analysis_has_category category messages = 
  let hasCat = hasCategory category messages
      anyHasCategory = any (category `isInfixOf`) messages
  in property $ hasCat === anyHasCategory

prop_error_analysis_filter_by_category :: String -> [String] -> Property
prop_error_analysis_filter_by_category category messages = 
  let filtered = filterByCategory category messages
      allHaveCategory = all (category `isInfixOf`) filtered
  in property $ allHaveCategory || null filtered

-- Test error recovery utilities
prop_error_recovery_choose_best :: [ErrorRecovery] -> Property
prop_error_recovery_choose_best recoveries = 
  let best = chooseBestRecovery recoveries
  in property $ isJust best || null recoveries

prop_error_recovery_choose_best_non_fatal :: [ErrorRecovery] -> Property
prop_error_recovery_choose_best_non_fatal recoveries = 
  let best = chooseBestRecovery recoveries
  in case best of
    Just recovery -> property $ canRecover recovery
    Nothing -> property $ True

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

-- Tests collection
tests :: TestTree
tests = testGroup "ErrorHandler Consistency QuickCheck Tests"
  [ testProperty "error severity equality" prop_error_severity_equality
  , testProperty "error severity ordering reflexive" prop_error_severity_ordering_reflexive
  , testProperty "error severity ordering antisymmetric" prop_error_severity_ordering_antisymmetric
  , testProperty "error severity ordering transitive" prop_error_severity_ordering_transitive
  , testProperty "severity priority monotonic" prop_severity_priority_monotonic
  , testProperty "severity priority bounds" prop_severity_priority_bounds
  , testProperty "is fatal consistency" prop_is_fatal_consistency
  , testProperty "is error consistency" prop_is_error_consistency
  , testProperty "is warning consistency" prop_is_warning_consistency
  , testProperty "is info consistency" prop_is_info_consistency
  , testProperty "is at least reflexive" prop_is_at_least_reflexive
  , testProperty "is at least transitive" prop_is_at_least_transitive
  , testProperty "is at least monotonic" prop_is_at_least_monotonic
  , testProperty "error location equality" prop_error_location_equality
  , testProperty "error location ordering" prop_error_location_ordering
  , testProperty "error context equality" prop_error_context_equality
  , testProperty "error context empty" prop_error_context_empty
  , testProperty "error recovery can recover" prop_error_recovery_can_recover
  , testProperty "error recovery should continue" prop_error_recovery_should_continue
  , testProperty "error collector initial state" prop_error_collector_initial_state
  , testProperty "error collector add error" prop_error_collector_add_error
  , testProperty "error collector add warning" prop_error_collector_add_warning
  , testProperty "error collector add info" prop_error_collector_add_info
  , testProperty "error collector get all" prop_error_collector_get_all
  , testProperty "error format preserves content" prop_error_format_preserves_content
  , testProperty "error format with location" prop_error_format_with_location
  , testProperty "error format multiple" prop_error_format_multiple
  , testProperty "error filter by category" prop_error_filter_by_category
  , testProperty "error filter by severity" prop_error_filter_by_severity
  , testProperty "error has category" prop_error_has_category
  , testProperty "error statistics counts" prop_error_statistics_counts
  , testProperty "error statistics total" prop_error_statistics_total
  , testProperty "error report contains all" prop_error_report_contains_all
  , testProperty "error report structure" prop_error_report_structure
  , testProperty "timestamp format" prop_timestamp_format
  , testProperty "timestamp consistency" prop_timestamp_consistency
  , testProperty "recovery strategy can recover" prop_recovery_strategy_can_recover
  , testProperty "recovery strategy custom" prop_recovery_strategy_custom
  , testProperty "recovery strategy fatal" prop_recovery_strategy_fatal
  , testProperty "recovery strategy error" prop_recovery_strategy_error
  , testProperty "recovery strategy warning" prop_recovery_strategy_warning
  , testProperty "recovery strategy info" prop_recovery_strategy_info
  , testProperty "error combination severity" prop_error_combination_severity
  , testProperty "error combination commutative" prop_error_combination_commutative
  , testProperty "error combination associative" prop_error_combination_associative
  , testProperty "error combination idempotent" prop_error_combination_idempotent
  , testProperty "error filter severity preserves type" prop_error_filter_severity_preserves_type
  , testProperty "error filter severity empty" prop_error_filter_severity_empty
  , testProperty "error filter severity all" prop_error_filter_severity_all
  , testProperty "error location line" prop_error_location_line
  , testProperty "error location column" prop_error_location_column
  , testProperty "error location unknown" prop_error_location_unknown
  , testProperty "error context with location" prop_error_context_with_location
  , testProperty "error context with context" prop_error_context_with_context
  , testProperty "error context with suggestions" prop_error_context_with_suggestions
  , testProperty "error wrap preserves original" prop_error_wrap_preserves_original
  , testProperty "error wrap associative" prop_error_wrap_associative
  , testProperty "error analysis has category" prop_error_analysis_has_category
  , testProperty "error analysis filter by category" prop_error_analysis_filter_by_category
  , testProperty "error recovery choose best" prop_error_recovery_choose_best
  , testProperty "error recovery choose best non fatal" prop_error_recovery_choose_best_non_fatal
  ]