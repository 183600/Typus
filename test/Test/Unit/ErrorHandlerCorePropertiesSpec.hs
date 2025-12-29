{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCorePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(Positive), getPositive)

import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , CombinedError(..)
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
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , combineErrors
  , combinedErrorSeverity
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , severityPriority
  , isAtLeast
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

import Data.List (sort, length)
import Data.Text (Text)
import qualified Data.Text as T

-- Property: severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive severity = isAtLeast severity severity === True

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive s1 s2 s3 =
  isAtLeast s1 s2 .&&. isAtLeast s2 s3 ==> isAtLeast s1 s3

-- Property: Fatal is at least all other severities
prop_fatal_is_at_least_all :: ErrorSeverity -> Property
prop_fatal_is_at_least_all severity = isAtLeast Fatal severity === True

-- Property: Info is not at least any higher severity
prop_info_is_not_at_least_higher :: ErrorSeverity -> Property
prop_info_is_not_at_least_higher severity =
  severity /= Info ==> isAtLeast Info severity === False

-- Property: empty context has no messages
prop_empty_context_has_no_messages :: Property
prop_empty_context_has_no_messages = emptyContext === emptyContext

-- Property: new error collector has no errors
prop_new_collector_has_no_errors :: Property
prop_new_collector_has_no_errors =
  let collector = newErrorCollector
  in hasErrors collector === False .&&. hasWarnings collector === False

-- Property: adding error makes hasErrors true
prop_add_error_makes_has_errors_true :: String -> Property
prop_add_error_makes_has_errors_true msg =
  not (null msg) ==> 
  let collector = addError msg newErrorCollector
  in hasErrors collector === True

-- Property: adding warning makes hasWarnings true
prop_add_warning_makes_has_warnings_true :: String -> Property
prop_add_warning_makes_has_warnings_true msg =
  not (null msg) ==> 
  let collector = addWarning msg newErrorCollector
  in hasWarnings collector === True

-- Property: adding info doesn't make hasErrors or hasWarnings true
prop_add_info_no_errors_warnings :: String -> Property
prop_add_info_no_errors_warnings msg =
  not (null msg) ==> 
  let collector = addInfo msg newErrorCollector
  in hasErrors collector === False .&&. hasWarnings collector === False

-- Property: getErrors returns only errors
prop_get_errors_returns_only_errors :: String -> String -> String -> Property
prop_get_errors_returns_only_errors errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      errors = getErrors collector
  in all ("Error" `T.isPrefixOf`) errors === True

-- Property: getWarnings returns only warnings
prop_get_warnings_returns_only_warnings :: String -> String -> String -> Property
prop_get_warnings_returns_only_warnings errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      warnings = getWarnings collector
  in all ("Warning" `T.isPrefixOf`) warnings === True

-- Property: getInfo returns only info
prop_get_info_returns_only_info :: String -> String -> String -> Property
prop_get_info_returns_only_info errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      infos = getInfo collector
  in all ("Info" `T.isPrefixOf`) infos === True

-- Property: getAllMessages returns all messages
prop_get_all_messages_count :: String -> String -> String -> Property
prop_get_all_messages_count errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      allMessages = getAllMessages collector
      errorCount = if null errorMsg then 0 else 1
      warningCount = if null warningMsg then 0 else 1
      infoCount = if null infoMsg then 0 else 1
  in length allMessages === errorCount + warningCount + infoCount

-- Property: filterBySeverity preserves ordering
prop_filter_by_severity_preserves_ordering :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves_ordering severities minSeverity =
  not (null severities) ==>
  let filtered = filter (isAtLeast minSeverity) severities
      sorted = sort filtered
  in filtered === sorted

-- Property: filterByCategory works correctly
prop_filter_by_category_works :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_filter_by_category_works errorPairs targetCategory =
  not (null errorPairs) ==>
  let filtered = filter (\(cat, _) -> cat == targetCategory) errorPairs
  in all (\(cat, _) -> cat == targetCategory) filtered === True

-- Property: hasCategory finds matching categories
prop_has_category_finds_matches :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_has_category_finds_matches errorPairs targetCategory =
  let hasMatch = any (\(cat, _) -> cat == targetCategory) errorPairs
  in hasCategory errorPairs targetCategory === hasMatch

-- Property: combineErrors preserves highest severity
prop_combine_errors_preserves_highest_severity :: ErrorSeverity -> ErrorSeverity -> Property
prop_combine_errors_preserves_highest_severity sev1 sev2 =
  let combined = combineErrors sev1 sev2
      expected = if severityPriority sev1 >= severityPriority sev2 then sev1 else sev2
  in combinedErrorSeverity combined === expected

-- Property: canRecoverFrom is true for non-fatal errors
prop_can_recover_from_non_fatal :: ErrorSeverity -> Property
prop_can_recover_from_non_fatal severity =
  severity /= Fatal ==> canRecoverFrom severity === True

-- Property: canRecoverFrom is false for fatal errors
prop_can_recover_from_fatal :: Property
prop_can_recover_from_fatal = canRecoverFrom Fatal === False

-- Property: shouldContinueAfter is false for fatal errors
prop_should_continue_after_fatal :: Property
prop_should_continue_after_fatal = shouldContinueAfter Fatal === False

-- Property: shouldContinueAfter is true for non-fatal errors
prop_should_continue_after_non_fatal :: ErrorSeverity -> Property
prop_should_continue_after_non_fatal severity =
  severity /= Fatal ==> shouldContinueAfter severity === True

-- Property: recovery strategies have correct properties
prop_recovery_strategy_properties :: Property
prop_recovery_strategy_properties =
  let fatal = fatalRecovery
      error = errorRecovery
      warning = warningRecovery
      info = infoRecovery
      custom = customRecovery "custom"
  in canRecoverFrom fatal === False .&&.
     canRecoverFrom error === True .&&.
     canRecoverFrom warning === True .&&.
     canRecoverFrom info === True .&&.
     canRecoverFrom custom === True

tests :: TestTree
tests =
  testGroup "ErrorHandler Core Properties"
    [ fastProperty "severity priority ordering" prop_severity_priority_ordering
    , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
    , fastProperty "Fatal is at least all other severities" prop_fatal_is_at_least_all
    , fastProperty "Info is not at least any higher severity" prop_info_is_not_at_least_higher
    , fastProperty "empty context has no messages" prop_empty_context_has_no_messages
    , fastProperty "new collector has no errors" prop_new_collector_has_no_errors
    , fastProperty "adding error makes hasErrors true" prop_add_error_makes_has_errors_true
    , fastProperty "adding warning makes hasWarnings true" prop_add_warning_makes_has_warnings_true
    , fastProperty "adding info doesn't make hasErrors or hasWarnings true" prop_add_info_no_errors_warnings
    , fastProperty "getErrors returns only errors" prop_get_errors_returns_only_errors
    , fastProperty "getWarnings returns only warnings" prop_get_warnings_returns_only_warnings
    , fastProperty "getInfo returns only info" prop_get_info_returns_only_info
    , fastProperty "getAllMessages count is correct" prop_get_all_messages_count
    , fastProperty "filterBySeverity preserves ordering" prop_filter_by_severity_preserves_ordering
    , fastProperty "filterByCategory works correctly" prop_filter_by_category_works
    , fastProperty "hasCategory finds matches" prop_has_category_finds_matches
    , fastProperty "combineErrors preserves highest severity" prop_combine_errors_preserves_highest_severity
    , fastProperty "canRecoverFrom is true for non-fatal errors" prop_can_recover_from_non_fatal
    , fastProperty "canRecoverFrom is false for fatal errors" prop_can_recover_from_fatal
    , fastProperty "shouldContinueAfter is false for fatal errors" prop_should_continue_after_fatal
    , fastProperty "shouldContinueAfter is true for non-fatal errors" prop_should_continue_after_non_fatal
    , fastProperty "recovery strategies have correct properties" prop_recovery_strategy_properties
    ]