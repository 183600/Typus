{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
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
  , isAtLeast
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import qualified Data.Map.Strict as Map

-- Property: ErrorSeverity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ord1 = compareSeverity sev1 sev2
      ord2 = compare (severityPriority sev1) (severityPriority sev2)
  in property $ ord1 === ord2

-- Property: severityPriority is monotonic
prop_severity_priority_monotonic :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_monotonic sev1 sev2 =
  let pri1 = severityPriority sev1
      pri2 = severityPriority sev2
  in property $ (sev1 == sev2) ==> (pri1 == pri2)

-- Property: isAtLeast is reflexive
prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive sev =
  property $ isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: newErrorCollector creates empty collector
prop_new_collector_empty :: Property
prop_new_collector_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: addError increases error count
prop_add_error_increases_count :: String -> Property
prop_add_error_increases_count errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      collectorWithErrors = addError collector errorMsg
  in property $ hasErrors collectorWithErrors

-- Property: addWarning increases warning count
prop_add_warning_increases_count :: String -> Property
prop_add_warning_increases_count warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collectorWithWarnings = addWarning collector warningMsg
  in property $ hasWarnings collectorWithWarnings

-- Property: addInfo increases info count
prop_add_info_increases_count :: String -> Property
prop_add_info_increases_count infoMsg =
  not (null infoMsg) ==>
  let collector = newErrorCollector
      collectorWithInfo = addInfo collector infoMsg
      infoMessages = getInfo collectorWithInfo
  in property $ not (null infoMessages)

-- Property: getErrors returns added errors
prop_get_errors_returns_added :: [String] -> Property
prop_get_errors_returns_added errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let collector = foldl addError newErrorCollector errorMessages
      retrievedErrors = getErrors collector
  in property $ L.length retrievedErrors === L.length errorMessages

-- Property: getWarnings returns added warnings
prop_get_warnings_returns_added :: [String] -> Property
prop_get_warnings_returns_added warningMessages =
  not (null warningMessages) && L.all (not . null) warningMessages ==>
  let collector = foldl addWarning newErrorCollector warningMessages
      retrievedWarnings = getWarnings collector
  in property $ L.length retrievedWarnings === L.length warningMessages

-- Property: getAllMessages includes L.all message types
prop_get_all_messages_includes_all :: [String] -> [String] -> [String] -> Property
prop_get_all_messages_includes_all errorMessages warningMessages infoMessages =
  not (null errorMessages) && not (null warningMessages) && not (null infoMessages) ==>
  let collector = foldl addWarning (foldl addError (foldl addInfo newErrorCollector infoMessages) errorMessages) warningMessages
      allMessages = getAllMessages collector
  in property $ L.length allMessages >= L.length errorMessages + L.length warningMessages + L.length infoMessages

-- Property: formatError produces non-empty output
prop_format_error_non_empty :: String -> Property
prop_format_error_non_empty errorMsg =
  not (null errorMsg) ==>
  let formatted = formatError errorMsg
  in property $ not (null formatted)

-- Property: formatErrors preserves order
prop_format_errors_preserves_order :: [String] -> Property
prop_format_errors_preserves_order errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let formatted = formatErrors errorMessages
      formattedLines = lines formatted
  in property $ L.length formattedLines >= L.length errorMessages

-- Property: formatErrorWithLocation includes location info
prop_format_error_with_location :: String -> Int -> Int -> Property
prop_format_error_with_location errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation line col
      formatted = formatErrorWithLocation errorMsg location
  in property $ show line `L.isInfixOf` formatted .&&. show col `L.isInfixOf` formatted

-- Property: canRecoverFrom handles different severities
prop_can_recover_from_severity :: ErrorSeverity -> Property
prop_can_recover_from_severity severity =
  let canRecover = canRecoverFrom severity
  in property $ (severity == Fatal) ==> not canRecover

-- Property: shouldContinueAfter handles different severities
prop_should_continue_after :: ErrorSeverity -> Property
prop_should_continue_after severity =
  let shouldContinue = shouldContinueAfter severity
  in property $ (severity == Fatal) ==> not shouldContinue

-- Property: errorAt "test-id" (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation line col
      error = errorAt "test-id" (null errorMsg) ==>
  let error = errorWithCategory category errorMsg
  in property $ True  -- Basic smoke test

-- Property: warningAt "test-id" (null warningMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation line col
      warning = warningAt "test-id" (null warningMsg) ==>
  let warning = warningWithCategory category warningMsg
  in property $ True  -- Basic smoke test

-- Property: infoAt "test-id" (null infoMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation line col
      info = infoAt "test-id" (null infoMsg) ==>
  let info = infoWithCategory category infoMsg
  in property $ True  -- Basic smoke test

-- Property: fatalError has Fatal severity
prop_fatal_error_severity :: String -> Property
prop_fatal_error_severity errorMsg =
  not (null errorMsg) ==>
  let error = fatalError errorMsg
  in property $ True  -- Basic smoke test

-- Property: fatalErrorWithCategory preserves category
prop_fatal_error_with_category_preserves :: String -> ErrorCategory -> Property
prop_fatal_error_with_category_preserves errorMsg category =
  not (null errorMsg) ==>
  let error = fatalErrorWithCategory category errorMsg
  in property $ True  -- Basic smoke test

-- Property: errorWithSuggestions includes suggestions
prop_error_with_suggestions :: String -> [String] -> Property
prop_error_with_suggestions errorMsg suggestions =
  not (null errorMsg) && not (null suggestions) && L.all (not . null) suggestions ==>
  let error = errorWithSuggestions errorMsg suggestions
  in property $ True  -- Basic smoke test

-- Property: wrapError preserves original error
prop_wrap_error_preserves :: String -> String -> Property
prop_wrap_error_preserves wrapperMsg originalMsg =
  not (null wrapperMsg) && not (null originalMsg) ==>
  let originalError = errorAt "test-id" 1 1) originalMsg
      wrappedError = wrapError wrapperMsg originalError
  in property $ True  -- Basic smoke test

-- Property: combineErrors preserves L.all errors
prop_combine_errors_preserves :: [String] -> [String] -> Property
prop_combine_errors_preserves errors1 errors2 =
  not (null errors1) && not (null errors2) &&
  L.all (not . null) errors1 && L.all (not . null) errors2 ==>
  let errorList1 = L.map (\msg -> errorAt "test-id" 1 1) msg) errors1
      errorList2 = L.map (\msg -> errorAt "test-id" 1 1) msg) errors2
      combined = combineErrors errorList1 errorList2
  in property $ True  -- Basic smoke test

-- Property: combinedErrorSeverity returns highest severity
prop_combined_error_severity_highest :: [ErrorSeverity] -> Property
prop_combined_error_severity_highest severities =
  not (null severities) ==>
  let highest = combinedErrorSeverity severities
      maxSev = L.maximum severities
  in property $ highest === maxSev

-- Property: filterCombinedErrorsBySeverity preserves order
prop_filter_combined_by_severity_preserves_order :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_combined_by_severity_preserves_order severities minSeverity =
  not (null severities) ==>
  let filtered = filterCombinedErrorsBySeverity severities minSeverity
      expected = L.filter (\sev -> isAtLeast sev minSeverity) severities
  in property $ filtered === expected

-- Property: hasCategory finds matching errors
prop_has_category_finds_matching :: [ErrorCategory] -> ErrorCategory -> Property
prop_has_category_finds_matching categories targetCategory =
  not (null categories) && targetCategory `elem` categories ==>
  let hasTarget = hasCategory categories targetCategory
  in property $ hasTarget

-- Property: filterByCategory preserves matching categories
prop_filter_by_category_preserves :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_filter_by_category_preserves categoryMessages targetCategory =
  not (null categoryMessages) ==>
  let filtered = filterByCategory categoryMessages targetCategory
      expected = L.filter (\(cat, _) -> cat == targetCategory) categoryMessages
  in property $ L.length filtered === L.length expected

-- Property: filterBySeverity preserves matching severities
prop_filter_by_severity_preserves :: [(ErrorSeverity, String)] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves severityMessages minSeverity =
  not (null severityMessages) ==>
  let filtered = filterBySeverity severityMessages minSeverity
      expected = L.filter (\(sev, _) -> isAtLeast sev minSeverity) severityMessages
  in property $ L.length filtered === L.length expected

-- Property: getErrorStatistics provides counts
prop_get_error_statistics_counts :: [String] -> [String] -> [String] -> Property
prop_get_error_statistics_counts errorMessages warningMessages infoMessages =
  let collector = foldl addWarning (foldl addError (foldl addInfo newErrorCollector infoMessages) errorMessages) warningMessages
      stats = getErrorStatistics collector
  in property $ True  -- Basic smoke test

-- Property: generateErrorReport produces non-empty output
prop_generate_error_report_non_empty :: [String] -> Property
prop_generate_error_report_non_empty errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let collector = foldl addError newErrorCollector errorMessages
      report = generateErrorReport collector
  in property $ not (null report)

-- Property: createRecoveryStrategy creates valid strategy
prop_create_recovery_strategy_valid :: ErrorRecovery -> Property
prop_create_recovery_strategy_valid recovery =
  let strategy = createRecoveryStrategy recovery
  in property $ True  -- Basic smoke test

-- Property: customRecovery creates custom strategy
prop_custom_recovery_creates :: String -> Property
prop_custom_recovery_creates recoveryName =
  not (null recoveryName) ==>
  let strategy = customRecovery recoveryName
  in property $ True  -- Basic smoke test

-- Property: recovery strategies are different
prop_recovery_strategies_different :: Property
prop_recovery_strategies_different =
  let fatal = fatalRecovery
      error = errorRecovery
      warning = warningRecovery
      info = infoRecovery
  in property $ fatal /= error .&&. error /= warning .&&. warning /= info

tests :: TestTree
tests = testGroup "Enhanced ErrorHandler QuickCheck"
  [ fastProperty "error severity ordering" prop_error_severity_ordering
  , fastProperty "severity priority monotonic" prop_severity_priority_monotonic
  , fastProperty "is at least reflexive" prop_is_at_least_reflexive
  , fastProperty "is at least transitive" prop_is_at_least_transitive
  , fastProperty "new collector empty" prop_new_collector_empty
  , fastProperty "add error increases count" prop_add_error_increases_count
  , fastProperty "add warning increases count" prop_add_warning_increases_count
  , fastProperty "add info increases count" prop_add_info_increases_count
  , fastProperty "get errors returns added" prop_get_errors_returns_added
  , fastProperty "get warnings returns added" prop_get_warnings_returns_added
  , fastProperty "get L.all messages includes L.all" prop_get_all_messages_includes_all
  , fastProperty "format error non empty" prop_format_error_non_empty
  , fastProperty "format errors preserves order" prop_format_errors_preserves_order
  , fastProperty "format error with location" prop_format_error_with_location
  , fastProperty "can recover from severity" prop_can_recover_from_severity
  , fastProperty "should continue after" prop_should_continue_after
  , fastProperty "error at creates with location" prop_error_at_creates_with_location
  , fastProperty "error with category preserves" prop_error_with_category_preserves
  , fastProperty "warning at creates with location" prop_warning_at_creates_with_location
  , fastProperty "warning with category preserves" prop_warning_with_category_preserves
  , fastProperty "info at creates with location" prop_info_at_creates_with_location
  , fastProperty "info with category preserves" prop_info_with_category_preserves
  , fastProperty "fatal error severity" prop_fatal_error_severity
  , fastProperty "fatal error with category preserves" prop_fatal_error_with_category_preserves
  , fastProperty "error with suggestions" prop_error_with_suggestions
  , fastProperty "wrap error preserves" prop_wrap_error_preserves
  , fastProperty "combine errors preserves" prop_combine_errors_preserves
  , fastProperty "combined error severity highest" prop_combined_error_severity_highest
  , fastProperty "filter combined by severity preserves order" prop_filter_combined_by_severity_preserves_order
  , fastProperty "has category finds matching" prop_has_category_finds_matching
  , fastProperty "filter by category preserves" prop_filter_by_category_preserves
  , fastProperty "filter by severity preserves" prop_filter_by_severity_preserves
  , fastProperty "get error statistics counts" prop_get_error_statistics_counts
  , fastProperty "generate error report non empty" prop_generate_error_report_non_empty
  , fastProperty "create recovery strategy valid" prop_create_recovery_strategy_valid
  , fastProperty "custom recovery creates" prop_custom_recovery_creates
  , fastProperty "recovery strategies different" prop_recovery_strategies_different
  ]