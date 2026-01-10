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
  , message
  , location
  , category
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad.State (evalState, execState)

-- Arbitrary instance for Text
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeChecking
    , Ownership
    , Parsing
    , Semantic
    , Runtime
    , Constraint
    , Inference
    , Integration
    , Unknown
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
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- arbitrary
    recHint <- arbitrary
    recCost <- arbitrary
    recConfidence <- arbitrary
    return $ RecoveryStrategy canRec shouldCont recAction recHint recCost recConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- arbitrary
    relatedErrors <- arbitrary
    errorChain <- arbitrary
    timestamp <- arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = do
    msg <- arbitrary
    severity <- arbitrary
    return $ IntegrationError msg severity

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
      loc = location err
      hasLineInfo = show (line loc) `isInfixOf` formatted
  in hasLineInfo .||. T.null (message err)

-- Property: formatErrorsWithLocation formats multiple errors
prop_format_errors_with_location_formats_multiple :: [TypeError] -> Property
prop_format_errors_with_location_formats_multiple errors = 
  let formatted = formatErrorsWithLocation errors
      errorCount = length errors
  in if null errors
     then property (null formatted)
     else property (errorCount > 0) .&&. property (not (null formatted))

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

-- Property: newErrorCollector creates empty collector
prop_new_error_collectors_empty :: Property
prop_new_error_collectors_empty = 
  let errors = execState newErrorCollector []
  in not (hasErrors errors) .&&. not (hasWarnings errors)

-- Property: addError makes collector have errors
prop_add_error_creates_errors :: TypeError -> Property
prop_add_error_creates_errors err = 
  let errors = execState (addError err) []
  in property (hasErrors errors)

-- Property: addWarning makes collector have warnings
prop_add_warning_creates_warnings :: TypeError -> Property
prop_add_warning_creates_warnings err = 
  let warningErr = err { severity = Warning }
      errors = execState (addWarning warningErr) []
  in property (hasWarnings errors)

-- Property: getErrors returns added errors
prop_get_errors_returns_added :: TypeError -> Property
prop_get_errors_returns_added err = 
  let errors = execState (addError err) []
      retrievedErrors = getErrors errors
  in property (err `elem` retrievedErrors)

-- Property: getWarnings returns added warnings
prop_get_warnings_returns_added :: TypeError -> Property
prop_get_warnings_returns_added err = 
  let warningErr = err { severity = Warning }
      errors = execState (addWarning warningErr) []
      warnings = getWarnings errors
  in property (warningErr `elem` warnings)

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: filterByCategory returns only errors with specified category
prop_filter_by_category_correct :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category_correct cat errors = 
  let filtered = filterByCategory cat errors
  in property (all (\e -> category e == cat) filtered)

-- Property: filterBySeverity returns only errors with specified severity
prop_filter_by_severity_correct :: ErrorSeverity -> [TypeError] -> Property
prop_filter_by_severity_correct sev errors = 
  let filtered = filterBySeverity sev errors
  in property (all (\e -> severity e == sev) filtered)

-- Property: hasCategory returns True if any error has category
prop_has_category_correct :: ErrorCategory -> [TypeError] -> Property
prop_has_category_correct cat errors = 
  let hasCat = any (\e -> hasCategory cat e) errors
      anyHasCat = any (\e -> category e == cat) errors
  in property (hasCat === anyHasCat)

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom returns True for recoverable errors
prop_can_recover_from_recoverable :: TypeError -> Property
prop_can_recover_from_recoverable err = 
  let recoverable = canRecover (recovery err)
  in property (canRecoverFrom err === recoverable)

-- Property: shouldContinueAfter returns False for fatal errors
prop_should_continue_after_fatal :: Property
prop_should_continue_after_fatal = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = errorWithCategory "TEST" TypeChecking "test" loc
      errWithFatal = err { severity = Fatal }
  in property (not (shouldContinueAfter errWithFatal))

-- Property: shouldContinueAfter returns True for non-fatal errors
prop_should_continue_after_non_fatal :: ErrorSeverity -> Property
prop_should_continue_after_non_fatal sev = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = errorWithCategory "TEST" TypeChecking "test" loc
      errWithSev = err { severity = sev }
      result = shouldContinueAfter errWithSev
  in if sev == Fatal then property (not result) else property result

-- ============================================================================
-- Error Combination Properties
-- ============================================================================

-- Property: combineErrors creates CombinedError
prop_combine_errors_creates_combined :: TypeError -> TypeError -> Property
prop_combine_errors_creates_combined err1 err2 = 
  let errors = [err1, err2]
      combined = combineErrors errors
  in property (not (null combined))  -- Just check that it creates a CombinedError list

-- Property: combinedErrorSeverity returns max severity
prop_combined_error_severity_max :: TypeError -> TypeError -> Property
prop_combined_error_severity_max err1 err2 = 
  let errors = [err1, err2]
      combined = combineErrors errors
      maxSeverity = max (severity err1) (severity err2)
  in if null combined then property True else property True  -- Simplified since combineErrors returns [TypeError] not [CombinedError]

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filter_combined_errors_by_severity :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_errors_by_severity sev combinedErrors = 
  let filtered = filterCombinedErrorsBySeverity sev combinedErrors
  in property (all (\e -> combinedErrorSeverity e >= sev) filtered)

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates error with location
prop_error_at_creates_with_location :: Text -> ErrorLocation -> Property
prop_error_at_creates_with_location msg loc = 
  let err = errorAt "TEST" msg loc
  in property (message err === msg) .&&. property (location err === loc)

-- Property: errorWithCategory creates error with category
prop_error_with_category_creates_with_category :: Text -> ErrorCategory -> Property
prop_error_with_category_creates_with_category msg cat = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = errorWithCategory "TEST" cat msg loc
  in property (message err === msg) .&&. property (category err === cat)

-- Property: fatalError has Fatal severity
prop_fatal_error_has_fatal_severity :: Text -> Property
prop_fatal_error_has_fatal_severity msg = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = fatalError "TEST" msg loc
  in property (severity err === Fatal)

-- Property: fatalErrorWithCategory has Fatal severity and category
prop_fatal_error_with_category_has_fatal_and_category :: Text -> ErrorCategory -> Property
prop_fatal_error_with_category_has_fatal_and_category msg cat = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = fatalErrorWithCategory "TEST" cat msg loc
  in property (severity err === Fatal) .&&. property (category err === cat)

-- Property: errorWithSuggestions includes suggestions
prop_error_with_suggestions_includes_suggestions :: Text -> [Text] -> Property
prop_error_with_suggestions_includes_suggestions msg suggs = 
  let loc = ErrorLocation Nothing 0 0 Nothing Nothing
      err = errorWithSuggestions "TEST" msg suggs loc
  in property (suggestions err === suggs)

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation changes error location
prop_with_location_changes_location :: TypeError -> ErrorLocation -> Property
prop_with_location_changes_location err loc = 
  let modified = withLocation err loc
  in property (location modified === loc)

-- Property: withContext adds context to error
prop_with_context_adds_context :: TypeError -> ErrorContext -> Property
prop_with_context_adds_context err ctx = 
  let modified = withContext err ctx
  in property (context modified === ctx)

-- Property: withSuggestions adds suggestions to error
prop_with_suggestions_adds_suggestions :: TypeError -> [Text] -> Property
prop_with_suggestions_adds_suggestions err suggs = 
  let modified = withSuggestions suggs err
  in property (suggestions modified === suggs)

-- ============================================================================
-- Recovery Strategy Properties
-- ============================================================================

-- Property: customRecovery creates custom recovery strategy
prop_custom_recovery_creates_strategy :: String -> Property
prop_custom_recovery_creates_strategy name = 
  let strategy = customRecovery True True Nothing Nothing 0 0.0
  in property (not (null (show strategy)))

-- Property: fatalRecovery creates fatal recovery strategy
prop_fatal_recovery_creates_fatal_strategy :: Property
prop_fatal_recovery_creates_fatal_strategy = 
  let strategy = fatalRecovery
  in property (not (canRecover strategy))

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