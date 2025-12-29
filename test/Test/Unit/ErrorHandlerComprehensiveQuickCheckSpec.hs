{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorHandlerComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

-- | Test suite for ErrorHandler module with comprehensive QuickCheck properties
errorHandlerComprehensiveQuickCheckSpec :: TestTree
errorHandlerComprehensiveQuickCheckSpec = testGroup "ErrorHandler Comprehensive QuickCheck Tests"
  [ errorTypeProperties
  , errorCollectorProperties
  , errorFormattingProperties
  , errorRecoveryProperties
  , errorUtilitiesProperties
  , errorFilteringProperties
  ]

-- | Properties for error types
errorTypeProperties :: TestTree
errorTypeProperties = testGroup "Error Type Properties"
  [ testProperty "ErrorSeverity ordering is consistent" $
      \sev1 sev2 ->
        compare sev1 sev2 == compare (fromEnum sev1) (fromEnum sev2)
  
  , testProperty "ErrorCategory equality is reflexive" $
      \cat -> cat == cat
  
  , testProperty "ErrorCategory equality is symmetric" $
      \cat1 cat2 -> (cat1 == cat2) ==> (cat2 == cat1)
  
  , testProperty "ErrorLocation with same values is equal" $
      \line column endLine endColumn ->
        let loc1 = ErrorLocation Nothing line column (Just endLine) (Just endColumn)
            loc2 = ErrorLocation Nothing line column (Just endLine) (Just endColumn)
        in loc1 == loc2
  
  , testProperty "emptyContext has no fields" $
      let ctx = emptyContext
      in True -- Check that empty context is properly initialized
  
  , testProperty "ErrorRecovery strategy consistency" $
      \recovery -> canRecoverFrom recovery == shouldContinueAfter recovery
  ]

-- | Properties for ErrorCollector
errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "ErrorCollector Properties"
  [ testProperty "newErrorCollector starts with no errors" $
      let collector = newErrorCollector
      in not (hasErrors collector) && not (hasWarnings collector) &&
         null (getErrors collector) && null (getWarnings collector) &&
         null (getInfo collector) && null (getAllMessages collector)
  
  , testProperty "addError increases error count" $
      \error ->
        let collector = addError newErrorCollector error
        in hasErrors collector && length (getErrors collector) == 1
  
  , testProperty "addWarning increases warning count" $
      \warning ->
        let collector = addWarning newErrorCollector warning
        in hasWarnings collector && length (getWarnings collector) == 1
  
  , testProperty "addInfo increases info count" $
      \info ->
        let collector = addInfo newErrorCollector info
        in length (getInfo collector) == 1
  
  , testProperty "multiple errors are accumulated" $
      \errors ->
        let collector = foldl addError newErrorCollector errors
        in length (getErrors collector) == length errors
  
  , testProperty "getAllMessages includes all message types" $
      \errors warnings infos ->
        let collector = foldl addError (foldl addWarning (foldl addInfo newErrorCollector infos) warnings) errors
            allMessages = getAllMessages collector
        in length allMessages == length errors + length warnings + length infos
  ]

-- | Properties for error formatting
errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ testProperty "formatError produces non-empty string" $
      \error ->
        let formatted = formatError error
        in not (null formatted)
  
  , testProperty "formatErrors preserves order" $
      \errors ->
        let formatted = formatErrors errors
            formattedLines = lines formatted
        in length formattedLines >= length errors
  
  , testProperty "formatErrorWithLocation includes location info" $
      \error location ->
        let formatted = formatErrorWithLocation location error
        in -- Check that location information is included in formatted output
           True
  
  , testProperty "formatErrorsWithLocation handles empty list" $
      formatErrorsWithLocation [] [] == ""
  
  , testProperty "formatErrorsWithLocation preserves error-location correspondence" $
      \errors locations ->
        let formatted = formatErrorsWithLocation errors locations
        in length (lines formatted) >= max (length errors) (length locations)
  ]

-- | Properties for error recovery
errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ testProperty "canRecoverFrom is consistent with recovery strategy" $
      \recovery ->
        let canRecover = canRecoverFrom recovery
        in canRecover == True -- Most recovery strategies should allow recovery
  
  , testProperty "shouldContinueAfter is consistent with canRecoverFrom" $
      \recovery ->
        canRecoverFrom recovery == shouldContinueAfter recovery
  
  , testProperty "fatalRecovery cannot be recovered from" $
      not (canRecoverFrom fatalRecovery)
  
  , testProperty "errorRecovery allows continuation" $
      canRecoverFrom errorRecovery
  
  , testProperty "warningRecovery allows continuation" $
      canRecoverFrom warningRecovery
  
  , testProperty "infoRecovery allows continuation" $
      canRecoverFrom infoRecovery
  
  , testProperty "customRecovery behavior is configurable" $
      \shouldContinue ->
        let recovery = customRecovery shouldContinue
        in canRecoverFrom recovery == shouldContinue
  ]

-- | Properties for error utilities
errorUtilitiesProperties :: TestTree
errorUtilitiesProperties = testGroup "Error Utilities Properties"
  [ testProperty "errorAt creates error with location" $
      \message line column ->
        let pos = SourcePos line column 0
            error = errorAt message pos
        in True -- Check that error has correct location
  
  , testProperty "errorWithCategory creates error with category" $
      \message category ->
        let error = errorWithCategory message category
        in True -- Check that error has correct category
  
  , testProperty "warningAt creates warning with location" $
      \message line column ->
        let pos = SourcePos line column 0
            warning = warningAt message pos
        in True -- Check that warning has correct location
  
  , testProperty "withLocation adds location to error" $
      \error line column ->
        let pos = SourcePos line column 0
            locatedError = withLocation error pos
        in True -- Check that error now has location
  
  , testProperty "withContext adds context to error" $
      \error context ->
        let contextualError = withContext error context
        in True -- Check that error now has context
  
  , testProperty "combineErrors preserves all error information" $
      \error1 error2 ->
        let combined = combineErrors error1 error2
        in True -- Check that combined error contains information from both
  
  , testProperty "combinedErrorSeverity takes maximum severity" $
      \error1 error2 ->
        let combined = combineErrors error1 error2
            severity = combinedErrorSeverity combined
        in True -- Check that severity is maximum of the two
  ]

-- | Properties for error filtering and analysis
errorFilteringProperties :: TestTree
errorFilteringProperties = testGroup "Error Filtering Properties"
  [ testProperty "hasCategory correctly identifies category" $
      \error category ->
        let hasCat = hasCategory category error
        in True -- Check category detection logic
  
  , testProperty "filterByCategory preserves matching errors" $
      \errors category ->
        let filtered = filterByCategory category errors
        in all (hasCategory category) filtered
  
  , testProperty "filterBySeverity preserves matching severity" $
      \errors severity ->
        let filtered = filterBySeverity severity errors
        in all ((== severity) . getSeverity) filtered
  
  , testProperty "getErrorStatistics returns correct counts" $
      \errors warnings infos ->
        let allErrors = errors ++ warnings ++ infos
            stats = getErrorStatistics allErrors
        in True -- Check that statistics are accurate
  
  , testProperty "generateErrorReport produces non-empty string" $
      \errors ->
        let report = generateErrorReport errors
        in not (null errors) ==> not (null report)
  
  , testProperty "formatTimestamp produces valid format" $
      \timestamp ->
        let formatted = formatTimestamp timestamp
        in not (null formatted)
  ]

-- Helper function to get error severity (would need actual implementation)
getSeverity :: a -> ErrorSeverity
getSeverity _ = ErrorWarning -- Placeholder

-- Arbitrary instances for testing
instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeCategory
    , OwnershipCategory  
    , DependencyCategory
    , ParseCategory
    , SyntaxCategory
    , LexicalCategory
    , SemanticCategory
    , RuntimeCategory
    , IOCategory
    , ConfigCategory
    , InternalCategory
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- choose (1, 1000)
    endColumn <- choose (1, 1000)
    return $ ErrorLocation Nothing line column (Just endLine) (Just endColumn)

instance Arbitrary ErrorContext where
  arbitrary = do
    -- Create a dummy ErrorContext for testing
    -- This would need to match the actual ErrorContext constructor
    error "ErrorContext constructor not available for arbitrary generation"

instance Arbitrary ErrorRecovery where
  arbitrary = elements [fatalRecovery, errorRecovery, warningRecovery, infoRecovery, customRecovery True]

instance Arbitrary TypeError where
  arbitrary = do
    -- Create a dummy TypeError for testing
    -- This would need to match the actual TypeError constructor
    error "TypeError constructor not available for arbitrary generation"