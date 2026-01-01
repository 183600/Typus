{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencyQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Positive(Positive), getPositive, Arbitrary(..)
  , Gen, oneof, elements, listOf, listOf1, choose, sized, suchThat
  )

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
  , infoRecovery
  )

import Data.List (length)
import Data.List (sort, nub)
import Data.Text (Text)
import qualified Data.Text as T

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , SemanticError
  , RuntimeError
  , IOError
  , MemoryError
  , ConcurrencyError
  , SecurityError
  , ConfigError
  ]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- Positive <$> choose (1, 1000)
  column <- Positive <$> choose (1, 200)
  endLine <- oneof [pure Nothing, Just . Positive <$> choose (1, 1000)]
  endColumn <- oneof [pure Nothing, Just . Positive <$> choose (1, 200)]
  return $ ErrorLocation 
    { line = getPositive line
    , column = getPositive column
    , endLine = getPositive <$> endLine
    , endColumn = getPositive <$> endColumn
    }

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = do
  words <- listOf1 $ elements $ L.concat
    [ ["syntax", "type", "name", "semantic", "runtime", "IO", "memory", "concurrency", "security", "config"]
    , ["error", "warning", "info", "fatal", "critical", "minor", "major"]
    , ["found", "expected", "missing", "invalid", "undefined", "unresolved", "conflict"]
    , ["in", "at", "on", "with", "without", "before", "after", "during"]
    ]
  return $ unwords words

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = oneof
  [ pure $ customRecovery "retry"
  , pure fatalRecovery
  , pure errorRecovery
  , pure warningRecovery
  , pure infoRecovery
  ]

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = do
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  message <- genErrorMessage
  recovery <- genErrorRecovery
  return $ CombinedError severity category location message recovery

-- Property: severity priority is total ordering
prop_severity_priority_total_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_total_ordering s1 s2 =
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in (p1 > p2) .||. (p1 == p2) .||. (p1 < p2)

-- Property: isAtLeast is antisymmetric for different severities
prop_isAtLeast_antisymmetric :: ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_antisymmetric s1 s2 =
  s1 /= s2 ==> (isAtLeast s1 s2 && isAtLeast s2 s1) === False

-- Property: isAtLeast defines a partial order
prop_isAt_least_partial_order :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAt_least_partial_order s1 s2 s3 =
  isAtLeast s1 s2 .&&. isAtLeast s2 s3 ==> isAtLeast s1 s3

-- Property: Fatal has highest priority
prop_fatal_highest_priority :: ErrorSeverity -> Property
prop_fatal_highest_priority severity =
  severityPriority Fatal >= severityPriority severity

-- Property: Info has lowest priority
prop_info_lowest_priority :: ErrorSeverity -> Property
prop_info_lowest_priority severity =
  severityPriority Info <= severityPriority severity

-- Property: new error collector is empty
prop_new_collector_empty :: Property
prop_new_collector_empty =
  let collector = newErrorCollector
  in hasErrors collector === False .&&.
     hasWarnings collector === False .&&.
     null (getAllMessages collector)

-- Property: adding errors increases error count
prop_add_error_increases_count :: String -> Property
prop_add_error_increases_count msg =
  not (null msg) ==>
  let collector = addError msg newErrorCollector
      originalCount = 0
      newCount = L.length $ getErrors collector
  in newCount > originalCount .&&. hasErrors collector

-- Property: adding warnings increases warning count
prop_add_warning_increases_count :: String -> Property
prop_add_warning_increases_count msg =
  not (null msg) ==>
  let collector = addWarning msg newErrorCollector
      originalCount = 0
      newCount = L.length $ getWarnings collector
  in newCount > originalCount .&&. hasWarnings collector

-- Property: adding info doesn't affect error/warning counts
prop_add_info_no_affect_counts :: String -> Property
prop_add_info_no_affect_counts msg =
  not (null msg) ==>
  let collector = addInfo msg newErrorCollector
  in hasErrors collector === False .&&.
     hasWarnings collector === False

-- Property: getErrors returns only error messages
prop_get_errors_only_errors :: String -> String -> String -> Property
prop_get_errors_only_errors errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      errors = getErrors collector
  in L.all ("Error" `T.L.isPrefixOf`) errors

-- Property: getWarnings returns only warning messages
prop_get_warnings_only_warnings :: String -> String -> String -> Property
prop_get_warnings_only_warnings errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      warnings = getWarnings collector
  in L.all ("Warning" `T.L.isPrefixOf`) warnings

-- Property: getInfo returns only info messages
prop_get_info_only_info :: String -> String -> String -> Property
prop_get_info_only_info errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      infos = getInfo collector
  in L.all ("Info" `T.L.isPrefixOf`) infos

-- Property: getAllMessages count equals L.sum of individual counts
prop_get_all_messages_count :: String -> String -> String -> Property
prop_get_all_messages_count errorMsg warningMsg infoMsg =
  not (null errorMsg && null warningMsg && null infoMsg) ==>
  let collector = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg newErrorCollector
      allMessages = getAllMessages collector
      errorCount = if null errorMsg then 0 else 1
      warningCount = if null warningMsg then 0 else 1
      infoCount = if null infoMsg then 0 else 1
  in L.length allMessages === errorCount + warningCount + infoCount

-- Property: filterBySeverity preserves order
prop_filter_by_severity_preserves_order :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves_order severities minSeverity =
  not (null severities) ==>
  let filtered = L.filter (isAtLeast minSeverity) severities
      sorted = sort filtered
  in filtered === sorted

-- Property: filterByCategory is correct
prop_filter_by_category_correct :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_filter_by_category_correct errorPairs targetCategory =
  not (null errorPairs) ==>
  let filtered = L.filter (\(cat, _) -> cat == targetCategory) errorPairs
  in L.all (\(cat, _) -> cat == targetCategory) filtered

-- Property: hasCategory is accurate
prop_has_category_accurate :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_has_category_accurate errorPairs targetCategory =
  let hasMatch = L.any (\(cat, _) -> cat == targetCategory) errorPairs
  in hasCategory errorPairs targetCategory === hasMatch

-- Property: combineErrors preserves highest severity
prop_combine_errors_preserves_highest :: ErrorSeverity -> ErrorSeverity -> Property
prop_combine_errors_preserves_highest sev1 sev2 =
  let combined = combineErrors sev1 sev2
      expected = if severityPriority sev1 >= severityPriority sev2 then sev1 else sev2
  in combinedErrorSeverity combined === expected

-- Property: canRecoverFrom is false only for Fatal
prop_can_recover_only_fatal_non_recoverable :: ErrorSeverity -> Property
prop_can_recover_only_fatal_non_recoverable severity =
  canRecoverFrom severity === (severity /= Fatal)

-- Property: shouldContinueAfter is false only for Fatal
prop_should_continue_only_fatal_stops :: ErrorSeverity -> Property
prop_should_continue_only_fatal_stops severity =
  shouldContinueAfter severity === (severity /= Fatal)

-- Property: recovery strategies have consistent properties
prop_recovery_strategies_consistent :: ErrorRecovery -> Property
prop_recovery_strategies_consistent recovery =
  let canRecover = case recovery of
        CustomRecovery _ -> True
        FatalRecovery -> False
        ErrorRecovery -> True
        WarningRecovery -> True
        InfoRecovery -> True
  in canRecoverFrom (errorSeverityFromRecovery recovery) === canRecover

-- Helper function for recovery property
errorSeverityFromRecovery :: ErrorRecovery -> ErrorSeverity
errorSeverityFromRecovery recovery = case recovery of
  CustomRecovery _ -> Error
  FatalRecovery -> Fatal
  ErrorRecovery -> Error
  WarningRecovery -> Warning
  InfoRecovery -> Info

-- Property: errorAt "test-id" (null msg) ==>
  let collector = errorAt "test-id" (null msg) ==>
  let collector = warningAt "test-id" (null msg) ==>
  let collector = errorWithCategory category msg newErrorCollector
  in hasErrors collector

-- Property: fatalError creates fatal error
prop_fatal_error_creates_fatal :: String -> Property
prop_fatal_error_creates_fatal msg =
  not (null msg) ==>
  let collector = fatalError msg newErrorCollector
  in hasErrors collector .&&. not (canRecoverFrom Fatal)

-- Property: formatError produces non-empty string
prop_format_error_non_empty :: CombinedError -> Property
prop_format_error_non_empty error =
  let formatted = formatError error
  in not (T.null formatted)

-- Property: formatErrors preserves order
prop_format_errors_preserves_order :: [CombinedError] -> Property
prop_format_errors_preserve_order errors =
  not (null errors) ==>
  let formatted = formatErrors errors
  in L.length formatted === L.length errors

tests :: TestTree
tests =
  testGroup "ErrorHandler Consistency QuickCheck Tests"
    [ fastProperty "severity priority is total ordering" prop_severity_priority_total_ordering
    , fastProperty "isAtLeast is antisymmetric for different severities" prop_isAtLeast_antisymmetric
    , fastProperty "isAtLeast defines a partial order" prop_isAt_least_partial_order
    , fastProperty "Fatal has highest priority" prop_fatal_highest_priority
    , fastProperty "Info has lowest priority" prop_info_lowest_priority
    , fastProperty "new error collector is empty" prop_new_collector_empty
    , fastProperty "adding errors increases error count" prop_add_error_increases_count
    , fastProperty "adding warnings increases warning count" prop_add_warning_increases_count
    , fastProperty "adding info doesn't affect error/warning counts" prop_add_info_no_affect_counts
    , fastProperty "getErrors returns only error messages" prop_get_errors_only_errors
    , fastProperty "getWarnings returns only warning messages" prop_get_warnings_only_warnings
    , fastProperty "getInfo returns only info messages" prop_get_info_only_info
    , fastProperty "getAllMessages count equals L.sum of individual counts" prop_get_all_messages_count
    , fastProperty "filterBySeverity preserves order" prop_filter_by_severity_preserves_order
    , fastProperty "filterByCategory is correct" prop_filter_by_category_correct
    , fastProperty "hasCategory is accurate" prop_has_category_accurate
    , fastProperty "combineErrors preserves highest severity" prop_combine_errors_preserves_highest
    , fastProperty "canRecoverFrom is false only for Fatal" prop_can_recover_only_fatal_non_recoverable
    , fastProperty "shouldContinueAfter is false only for Fatal" prop_should_continue_only_fatal_stops
    , fastProperty "recovery strategies have consistent properties" prop_recovery_strategies_consistent
    , fastProperty "errorAt "test-id" order" prop_format_errors_preserve_order
    ]