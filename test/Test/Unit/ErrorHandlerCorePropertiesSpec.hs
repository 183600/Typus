{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.ErrorHandlerCorePropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
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
  , isAtLeast
  , severityPriority
  , compareSeverity
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ ParseError
    , TypeError
    , NameError
    , ImportError
    , Warning
    , Error
    , InternalError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- arbitrary
    column <- arbitrary
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextText <- arbitrary
    additionalInfo <- arbitrary
    return $ ErrorContext contextText additionalInfo

instance Arbitrary ErrorRecovery where
  arbitrary = elements 
    [ NoRecovery
    , SkipToken
    , InsertToken
    , ReplaceToken
    , Retry
    , Abort
    ]

instance Arbitrary TypeError where
  arbitrary = do
    message <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- arbitrary
    relatedErrors <- arbitrary
    return $ TypeError message severity category location context recovery suggestions relatedErrors

instance Arbitrary CombinedError where
  arbitrary = do
    primary <- arbitrary
    secondary <- arbitrary
    return $ CombinedError primary secondary

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: severityPriority returns consistent ordering
prop_severity_priority_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_consistent sev1 sev2 = 
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
      cmp = compareSeverity sev1 sev2
  in if sev1 == sev2
     then cmp === EQ .&&. p1 === p2
     else if p1 < p2 then cmp === LT else cmp === GT

-- Property: isAtLeast correctly compares severities
prop_is_at_least_correct :: ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_correct sev1 sev2 = 
  let result = isAtLeast sev1 sev2
      p1 = severityPriority sev1
      p2 = severityPriority sev2
  in result === (p1 >= p2)

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: formatErrorWithLocation includes location information
prop_format_error_with_location_includes_location :: TypeError -> Property
prop_format_error_with_location_includes_location err = 
  let formatted = formatErrorWithLocation err
      location = teLocation err
      hasLineInfo = case line location of
        Nothing -> False
        Just l -> show l `isInfixOf` formatted
  in hasLineInfo .||. T.null (teMessage err)

-- Property: formatErrorsWithLocation formats multiple errors
prop_format_errors_with_location_formats_multiple :: [TypeError] -> Property
prop_format_errors_with_location_formats_multiple errors = 
  let formatted = formatErrorsWithLocation errors
      errorCount = length errors
  in if null errors
     then null formatted
     else errorCount > 0 .&&. not (null formatted)

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

-- Property: newErrorCollector creates empty collector
prop_new_error_collectors_empty :: Property
prop_new_error_collectors_empty = 
  let collector = newErrorCollector
  in not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: addError makes collector have errors
prop_add_error_creates_errors :: TypeError -> Property
prop_add_error_creates_errors err = 
  let collector = newErrorCollector
      collector' = addError err collector
  in hasErrors collector'

-- Property: addWarning makes collector have warnings
prop_add_warning_creates_warnings :: TypeError -> Property
prop_add_warning_creates_warnings err = 
  let collector = newErrorCollector
      warningErr = err { teSeverity = Warning }
      collector' = addWarning warningErr collector
  in hasWarnings collector'

-- Property: getErrors returns added errors
prop_get_errors_returns_added :: TypeError -> Property
prop_get_errors_returns_added err = 
  let collector = newErrorCollector
      collector' = addError err collector
      errors = getErrors collector'
  in err `elem` errors

-- Property: getWarnings returns added warnings
prop_get_warnings_returns_added :: TypeError -> Property
prop_get_warnings_returns_added err = 
  let collector = newErrorCollector
      warningErr = err { teSeverity = Warning }
      collector' = addWarning warningErr collector
      warnings = getWarnings collector'
  in warningErr `elem` warnings

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: filterByCategory returns only errors with specified category
prop_filter_by_category_correct :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category_correct cat errors = 
  let filtered = filterByCategory cat errors
  in all (\e -> teCategory e == cat) filtered

-- Property: filterBySeverity returns only errors with specified severity
prop_filter_by_severity_correct :: ErrorSeverity -> [TypeError] -> Property
prop_filter_by_severity_correct sev errors = 
  let filtered = filterBySeverity sev errors
  in all (\e -> teSeverity e == sev) filtered

-- Property: hasCategory returns True if any error has category
prop_has_category_correct :: ErrorCategory -> [TypeError] -> Property
prop_has_category_correct cat errors = 
  let hasCat = hasCategory cat errors
      anyHasCat = any (\e -> teCategory e == cat) errors
  in hasCat === anyHasCat

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom returns True for recoverable errors
prop_can_recover_from_recoverable :: ErrorRecovery -> Property
prop_can_recover_from_recoverable recovery = 
  let recoverable = recovery `elem` [SkipToken, InsertToken, ReplaceToken, Retry]
  in canRecoverFrom recovery === recoverable

-- Property: shouldContinueAfter returns False for fatal errors
prop_should_continue_after_fatal :: Property
prop_should_continue_after_fatal = 
  not (shouldContinueAfter Fatal)

-- Property: shouldContinueAfter returns True for non-fatal errors
prop_should_continue_after_non_fatal :: ErrorSeverity -> Property
prop_should_continue_after_non_fatal sev = 
  let result = shouldContinueAfter sev
  in if sev == Fatal then not result else result

-- ============================================================================
-- Error Combination Properties
-- ============================================================================

-- Property: combineErrors creates CombinedError
prop_combine_errors_creates_combined :: TypeError -> TypeError -> Property
prop_combine_errors_creates_combined err1 err2 = 
  let combined = combineErrors err1 err2
  in case combined of
    CombinedError primary secondary -> 
      primary === err1 .&&. secondary === err2
    _ -> property False

-- Property: combinedErrorSeverity returns max severity
prop_combined_error_severity_max :: TypeError -> TypeError -> Property
prop_combined_error_severity_max err1 err2 = 
  let combined = combineErrors err1 err2
      maxSeverity = max (teSeverity err1) (teSeverity err2)
  in combinedErrorSeverity combined === maxSeverity

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filter_combined_errors_by_severity :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_errors_by_severity sev combinedErrors = 
  let filtered = filterCombinedErrorsBySeverity sev combinedErrors
  in all (\e -> combinedErrorSeverity e >= sev) filtered

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates error with location
prop_error_at_creates_with_location :: Text -> ErrorLocation -> Property
prop_error_at_creates_with_location message location = 
  let err = errorAt message location
  in teMessage err === message .&&. teLocation err === location

-- Property: errorWithCategory creates error with category
prop_error_with_category_creates_with_category :: Text -> ErrorCategory -> Property
prop_error_with_category_creates_with_category message category = 
  let err = errorWithCategory message category
  in teMessage err === message .&&. teCategory err === category

-- Property: fatalError has Fatal severity
prop_fatal_error_has_fatal_severity :: Text -> Property
prop_fatal_error_has_fatal_severity message = 
  let err = fatalError message
  in teSeverity err === Fatal

-- Property: fatalErrorWithCategory has Fatal severity and category
prop_fatal_error_with_category_has_fatal_and_category :: Text -> ErrorCategory -> Property
prop_fatal_error_with_category_has_fatal_and_category message category = 
  let err = fatalErrorWithCategory message category
  in teSeverity err === Fatal .&&. teCategory err === category

-- Property: errorWithSuggestions includes suggestions
prop_error_with_suggestions_includes_suggestions :: Text -> [Text] -> Property
prop_error_with_suggestions_includes_suggestions message suggestions = 
  let err = errorWithSuggestions message suggestions
  in teSuggestions err === suggestions

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation changes error location
prop_with_location_changes_location :: TypeError -> ErrorLocation -> Property
prop_with_location_changes_location err location = 
  let modified = withLocation location err
  in teLocation modified === location

-- Property: withContext adds context to error
prop_with_context_adds_context :: TypeError -> ErrorContext -> Property
prop_with_context_adds_context err context = 
  let modified = withContext context err
  in teContext modified === context

-- Property: withSuggestions adds suggestions to error
prop_with_suggestions_adds_suggestions :: TypeError -> [Text] -> Property
prop_with_suggestions_adds_suggestions err suggestions = 
  let modified = withSuggestions suggestions err
  in teSuggestions modified === suggestions

-- ============================================================================
-- Recovery Strategy Properties
-- ============================================================================

-- Property: customRecovery creates custom recovery strategy
prop_custom_recovery_creates_strategy :: String -> Property
prop_custom_recovery_creates_strategy name = 
  let strategy = customRecovery name
  in not (null strategy)

-- Property: fatalRecovery creates fatal recovery strategy
prop_fatal_recovery_creates_fatal_strategy :: Property
prop_fatal_recovery_creates_fatal_strategy = 
  let strategy = fatalRecovery
  in not (null strategy)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Core Properties Tests"
  [ testGroup "Error Severity Properties"
    [ testProperty "severityPriority returns consistent ordering" prop_severity_priority_consistent
    , testProperty "isAtLeast correctly compares severities" prop_is_at_least_correct
    ]
  , testGroup "Error Location Properties"
    [ testProperty "formatErrorWithLocation includes location information" prop_format_error_with_location_includes_location
    , testProperty "formatErrorsWithLocation formats multiple errors" prop_format_errors_with_location_formats_multiple
    ]
  , testGroup "Error Collection Properties"
    [ testProperty "newErrorCollector creates empty collector" prop_new_error_collectors_empty
    , testProperty "addError makes collector have errors" prop_add_error_creates_errors
    , testProperty "addWarning makes collector have warnings" prop_add_warning_creates_warnings
    , testProperty "getErrors returns added errors" prop_get_errors_returns_added
    , testProperty "getWarnings returns added warnings" prop_get_warnings_returns_added
    ]
  , testGroup "Error Filtering Properties"
    [ testProperty "filterByCategory returns only errors with specified category" prop_filter_by_category_correct
    , testProperty "filterBySeverity returns only errors with specified severity" prop_filter_by_severity_correct
    , testProperty "hasCategory returns True if any error has category" prop_has_category_correct
    ]
  , testGroup "Error Recovery Properties"
    [ testProperty "canRecoverFrom returns True for recoverable errors" prop_can_recover_from_recoverable
    , testProperty "shouldContinueAfter returns False for fatal errors" prop_should_continue_after_fatal
    , testProperty "shouldContinueAfter returns True for non-fatal errors" prop_should_continue_after_non_fatal
    ]
  , testGroup "Error Combination Properties"
    [ testProperty "combineErrors creates CombinedError" prop_combine_errors_creates_combined
    , testProperty "combinedErrorSeverity returns max severity" prop_combined_error_severity_max
    , testProperty "filterCombinedErrorsBySeverity filters correctly" prop_filter_combined_errors_by_severity
    ]
  , testGroup "Error Creation Properties"
    [ testProperty "errorAt creates error with location" prop_error_at_creates_with_location
    , testProperty "errorWithCategory creates error with category" prop_error_with_category_creates_with_category
    , testProperty "fatalError has Fatal severity" prop_fatal_error_has_fatal_severity
    , testProperty "fatalErrorWithCategory has Fatal severity and category" prop_fatal_error_with_category_has_fatal_and_category
    , testProperty "errorWithSuggestions includes suggestions" prop_error_with_suggestions_includes_suggestions
    ]
  , testGroup "Error Modification Properties"
    [ testProperty "withLocation changes error location" prop_with_location_changes_location
    , testProperty "withContext adds context to error" prop_with_context_adds_context
    , testProperty "withSuggestions adds suggestions to error" prop_with_suggestions_adds_suggestions
    ]
  , testGroup "Recovery Strategy Properties"
    [ testProperty "customRecovery creates custom recovery strategy" prop_custom_recovery_creates_strategy
    , testProperty "fatalRecovery creates fatal recovery strategy" prop_fatal_recovery_creates_fatal_strategy
    ]
  ]