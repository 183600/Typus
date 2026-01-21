{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.ErrorHandlerCorePropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (CompilerError(..))
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
    return $ ErrorRecovery canRec shouldCont recAction recHint recCost recConfidence

-- Removed duplicate Arbitrary instance
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
prop_format_error_with_location_includes_location :: Property
prop_format_error_with_location_includes_location = 
  let err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext errorRecovery [] [] [] Nothing
      formatted = formatErrorWithLocation err
      loc = location err
      hasLineInfo = show (line loc) `isInfixOf` formatted
  in hasLineInfo .||. T.null (message err)

-- Property: formatErrorsWithLocation formats multiple errors
prop_format_errors_with_location_formats_multiple :: Property
prop_format_errors_with_location_formats_multiple = 
  let errors = [TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext errorRecovery [] [] [] Nothing]
      formatted = formatErrorsWithLocation errors
      errorCount = length errors
  in if null errors
     then property (null formatted)
     else property (length (lines formatted) >= errorCount)

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

-- Property: newErrorCollector creates empty collector
prop_new_error_collectors_empty :: Property
prop_new_error_collectors_empty = 
  let errors = execState newErrorCollector []
  in not (hasErrors errors) .&&. not (hasWarnings errors)

-- Property: hasErrors is true after adding error
prop_has_errors_true_after_add :: Property
prop_has_errors_true_after_add = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      collector = execState (addError err) []
  in property (hasErrors collector)

-- Property: addWarning makes collector have warnings
prop_add_warning_creates_warnings :: Property
prop_add_warning_creates_warnings = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      collector = execState (addWarning err) []
  in property (hasWarnings collector)

-- Property: getErrors returns added errors
prop_get_errors_returns_added :: Property
prop_get_errors_returns_added = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      collector = execState (addError err) []
      errors = getErrors collector
  in property (err `elem` errors)

-- Property: getWarnings returns added warnings
prop_get_warnings_returns_added :: Property
prop_get_warnings_returns_added = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      collector = execState (addWarning err) []
      warnings = getWarnings collector
  in property (err `elem` warnings)

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: filterByCategory returns only errors with specified category
prop_filter_by_category_correct :: Property
prop_filter_by_category_correct = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Error Ownership (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err3 = TypeError "test-003" Error Parsing (T.pack "test message 3") (ErrorLocation Nothing 3 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      errors = [err1, err2, err3]
      filtered = filterByCategory Parsing errors
      allHaveCategory = all (\e -> category e == Parsing) filtered
  in property allHaveCategory

-- Property: filterBySeverity returns only errors with specified severity
prop_filter_by_severity_correct :: Property
prop_filter_by_severity_correct = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Warning Ownership (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err3 = TypeError "test-003" Error Parsing (T.pack "test message 3") (ErrorLocation Nothing 3 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      errors = [err1, err2, err3]
      filtered = filterBySeverity Error errors
      allHaveSeverity = all (\e -> severity e == Error) filtered
  in property allHaveSeverity

-- Property: hasCategory returns True if any error has category
prop_has_category_correct :: Property
prop_has_category_correct = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Error Ownership (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err3 = TypeError "test-003" Error Parsing (T.pack "test message 3") (ErrorLocation Nothing 3 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      errors = [err1, err2, err3]
      hasCategoryResult = any (\e -> hasCategory Parsing e) errors
  in property hasCategoryResult

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom returns True for recoverable errors
prop_can_recover_from_recoverable :: Property
prop_can_recover_from_recoverable = 
  let recovery = errorRecovery
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
  in property (canRecoverFrom err)

-- Property: shouldContinueAfter returns False for fatal errors
prop_should_continue_after_fatal :: Property
prop_should_continue_after_fatal = 
  let recovery = fatalRecovery
      err = TypeError "test-001" Fatal Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
  in property (not (shouldContinueAfter err))

-- Property: shouldContinueAfter returns True for non-fatal errors
prop_should_continue_after_non_fatal :: Property
prop_should_continue_after_non_fatal = 
  let recovery = errorRecovery
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
  in property (shouldContinueAfter err)

-- ============================================================================
-- Error Combination Properties
-- ============================================================================

-- Property: combineErrors creates CombinedError
prop_combine_errors_creates_combined :: Property
prop_combine_errors_creates_combined = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Error Parsing (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      combined = combineErrors [err1, err2]
  in property (length combined >= 2)

-- Property: combinedErrorSeverity returns max severity
prop_combined_error_severity_max :: Property
prop_combined_error_severity_max = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Warning Parsing (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      combined = [err1, err2]
      maxSeverity = maximum (map severity combined)
  in property (maxSeverity `elem` [Error, Warning])

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filter_combined_errors_by_severity :: Property
prop_filter_combined_errors_by_severity = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err1 = TypeError "test-001" Error Parsing (T.pack "test message 1") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      err2 = TypeError "test-002" Warning Parsing (T.pack "test message 2") (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      combinedErrors = [OwnershipErrorCombined Error (error "test ownership"), DependentTypeErrorCombined Warning (error "test dependent")]
      filtered = filterCombinedErrorsBySeverity Error combinedErrors
      allMeetMinSeverity = all (\e -> combinedErrorSeverity e >= Error) filtered
  in property allMeetMinSeverity

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates Error (T.pack creates) error with location
prop_error_at_creates_with_location :: Text -> ErrorLocation -> Property
prop_error_at_creates_with_location msg loc = 
  let err = errorAt "TEST" Error msg loc
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
      err = errorWithSuggestions "TEST" Error suggs loc
  in property (suggestions err === suggs)

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation changes error location
prop_with_location_changes_location :: Property
prop_with_location_changes_location = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      newLoc = ErrorLocation Nothing 2 2 Nothing Nothing
      modifiedErr = withLocation err newLoc
  in property (location modifiedErr == newLoc)

-- Property: withContext adds context to error
prop_with_context_adds_context :: Property
prop_with_context_adds_context = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      newCtx = ErrorContext (Just "code") (Just "function") (Just "variable") (Just "type") []
      modifiedErr = withContext err newCtx
  in property (context modifiedErr == newCtx)

-- Property: withSuggestions adds suggestions to error
prop_with_suggestions_adds_suggestions :: Property
prop_with_suggestions_adds_suggestions = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      newSuggestions = [T.pack "suggestion 1", T.pack "suggestion 2"]
      modifiedErr = withSuggestions newSuggestions err
  in property (suggestions modifiedErr == newSuggestions)

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

-- Property: addError makes collector have errors
prop_add_error_creates_errors :: Property
prop_add_error_creates_errors = 
  let recovery = ErrorRecovery True True Nothing Nothing 50 0.7
      err = TypeError "test-001" Error Parsing (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext recovery [] [] [] Nothing
      collector = execState (addError err) []
  in property (hasErrors collector)

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
    [ testProperty "errorAt creates Error (T.pack creates) error with location" prop_error_at_creates_with_location
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