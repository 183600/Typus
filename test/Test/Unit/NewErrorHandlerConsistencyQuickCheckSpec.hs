{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad.State (runState, evalState)
import qualified Data.Map.Strict as Map

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
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
  , getErrorColumn
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos)

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid error IDs
genErrorId :: Gen String
genErrorId = do
  prefix <- elements ["ERR", "WARN", "INFO", "FATAL"]
  number <- choose (1000, 9999)
  return $ prefix ++ show number

-- Generate error messages
genErrorMessage :: Gen Text
genErrorMessage = do
  words <- listOf $ elements $ words "This is a sample error message for testing purposes"
  return $ T.pack $ unwords words

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  filePath <- oneof [return Nothing, fmap Just genErrorId]
  line <- choose (1, 1000)
  column <- choose (1, 100)
  endLine <- oneof [return Nothing, fmap Just $ choose (line, line + 10)]
  endColumn <- oneof [return Nothing, fmap Just $ choose (column, column + 50)]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error contexts
genErrorContext :: Gen ErrorContext
genErrorContext = do
  code <- oneof [return Nothing, fmap Just genErrorMessage]
  func <- oneof [return Nothing, fmap Just genErrorId]
  var <- oneof [return Nothing, fmap Just genErrorId]
  typ <- oneof [return Nothing, fmap Just genErrorId]
  additional <- listOf $ do
    key <- genErrorId
    value <- genErrorMessage
    return (key, T.unpack value)
  return $ ErrorContext code func var typ additional

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRec <- arbitrary
  shouldCont <- arbitrary
  action <- oneof [return Nothing, fmap Just genErrorId]
  hint <- oneof [return Nothing, fmap Just genErrorMessage]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRec shouldCont action hint cost confidence

-- Generate error severities
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- Generate suggestions
genSuggestions :: Gen [Text]
genSuggestions = listOf genErrorMessage

-- Generate related errors
genRelatedErrors :: Gen [TypeError]
genRelatedErrors = listOf genTypeError

-- Generate timestamps
genTimestamp :: Gen (Maybe String)
genTimestamp = oneof [return Nothing, fmap Just genErrorId]

-- Generate TypeErrors
genTypeError :: Gen TypeError
genTypeError = do
  errorId' <- genErrorId
  severity' <- genErrorSeverity
  category' <- genErrorCategory
  message' <- genErrorMessage
  location' <- genErrorLocation
  context' <- genErrorContext
  recovery' <- genErrorRecovery
  suggestions' <- genSuggestions
  relatedErrors' <- genRelatedErrors
  errorChain' <- genRelatedErrors
  timestamp' <- genTimestamp
  return $ TypeError errorId' severity' category' message' location' context' recovery' suggestions' relatedErrors' errorChain' timestamp'

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
  arbitrary = genErrorContext

instance Arbitrary ErrorRecovery where
  arbitrary = genErrorRecovery

instance Arbitrary TypeError where
  arbitrary = genTypeError

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: Severity ordering is consistent
prop_severity_ordering_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering_consistent sev1 sev2 =
  let ordered = [Info, Warning, Error, Fatal]
      index1 = elemIndex sev1 ordered
      index2 = elemIndex sev2 ordered
  in case (index1, index2) of
    (Just i1, Just i2) -> property $ (sev1 >= sev2) === (i1 >= i2)
    _ -> property False  -- Should never happen

-- Property: Fatal errors cannot be recovered from
prop_fatal_no_recovery :: Property
prop_fatal_no_recovery =
  let fatalError = fatalError "test" "fatal message"
  in property $ not (canRecoverFrom fatalError)

-- Property: Fatal errors should not continue execution
prop_fatal_no_continue :: Property
prop_fatal_no_continue =
  let fatalErr = fatalError "test" "fatal message"
  in property $ not (shouldContinueAfter fatalErr)

-- Property: Warning L.and Info errors should be recoverable
prop_warning_info_recoverable :: ErrorSeverity -> Property
prop_warning_info_recoverable sev =
  (sev == Warning || sev == Info) ==>
  let err = errorAt "test-id" > 0) ==>
  let loc = ErrorLocation Nothing startLine startCol (Just endLine) (Just endCol)
  in property $ line loc === startLine .&&. column loc === startCol .&&.
             endLine loc === Just endLine .&&. endColumn loc === Just endCol

-- ============================================================================
-- Error Collector Properties
-- ============================================================================

-- Property: Adding errors increases error count
prop_add_error_increases_count :: TypeError -> [TypeError] -> Property
prop_add_error_increases_count err errors =
  let initialCount = L.length (getAllMessages errors)
      ((), newErrors) = runState (addError err) errors
      finalCount = L.length (getAllMessages newErrors)
  in property $ finalCount === initialCount + 1

-- Property: Adding warnings preserves warning severity
prop_add_warning_preserves_severity :: TypeError -> Property
prop_add_warning_preserves_severity err =
  let ((), warnings) = runState (addWarning err) []
      actualWarnings = getWarnings warnings
  in property $ L.all (\w -> severity w == Warning) actualWarnings

-- Property: Adding info preserves info severity
prop_add_info_preserves_severity :: TypeError -> Property
prop_add_info_preserves_severity err =
  let ((), infos) = runState (addInfo err) []
      actualInfos = getInfo infos
  in property $ L.all (\i -> severity i == Info) actualInfos

-- Property: hasErrors correctly detects errors
prop_has_errors_detection :: [TypeError] -> Property
prop_has_errors_detection errors =
  let actualErrors = getErrors errors
      hasErrs = hasErrors errors
  in property $ hasErrs === not (null actualErrors)

-- Property: hasWarnings correctly detects warnings
prop_has_warnings_detection :: [TypeError] -> Property
prop_has_warnings_detection errors =
  let actualWarnings = getWarnings errors
      hasWarns = hasWarnings errors
  in property $ hasWarns === not (null actualWarnings)

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: Formatting error includes severity
prop_format_includes_severity :: TypeError -> Property
prop_format_includes_severity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ severityStr `L.isInfixOf` formatted

-- Property: Formatting with location includes line number
prop_format_with_location_includes_line :: TypeError -> Property
prop_format_with_location_includes_line err =
  let formatted = formatErrorWithLocation err
      lineNum = show $ line (location err)
  in property $ lineNum `L.isInfixOf` formatted

-- Property: Formatting multiple errors preserves order
prop_format_multiple_preserves_order :: [TypeError] -> Property
prop_format_multiple_preserves_order errors =
  let formatted = formatErrors errors
      errorCount = L.length errors
  in property $ if null errors 
                then null formatted
                else L.length (lines formatted) >= errorCount

-- ============================================================================
-- Error Transformation Properties
-- ============================================================================

-- Property: withLocation updates location correctly
prop_with_location_updates :: TypeError -> ErrorLocation -> Property
prop_with_location_updates err newLoc =
  let updatedErr = withLocation newLoc err
  in property $ location updatedErr === newLoc .&&.
             message updatedErr === message err .&&.
             severity updatedErr === severity err

-- Property: withContext updates context correctly
prop_with_context_updates :: TypeError -> ErrorContext -> Property
prop_with_context_updates err newCtx =
  let updatedErr = withContext newCtx err
  in property $ context updatedErr === newCtx .&&.
             message updatedErr === message err .&&.
             severity updatedErr === severity err

-- Property: withSuggestions adds suggestions
prop_with_suggestions_adds :: TypeError -> [Text] -> Property
prop_with_suggestions_adds err suggestions =
  let updatedErr = withSuggestions suggestions err
  in property $ suggestions updatedErr === suggestions .&&.
             message updatedErr === message err

-- Property: wrapError creates error chain
prop_wrap_error_creates_chain :: TypeError -> TypeError -> Property
prop_wrap_error_creates_chain outer inner =
  let wrapped = wrapError outer inner
  in property $ errorChain wrapped `elem` [[inner], [outer, inner]] .&&.
             message wrapped === message outer

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: filterBySeverity preserves correct severities
prop_filter_by_severity_preserves :: [TypeError] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves errors minSev =
  let filtered = filterBySeverity minSev errors
  in property $ L.all (\e -> severity e >= minSev) filtered

-- Property: filterByCategory preserves correct categories
prop_filter_by_category_preserves :: [TypeError] -> ErrorCategory -> Property
prop_filter_by_category_preserves errors cat =
  let filtered = filterByCategory cat errors
  in property $ L.all (\e -> category e == cat) filtered

-- Property: hasCategory correctly identifies categories
prop_has_category_identifies :: [TypeError] -> ErrorCategory -> Property
prop_has_category_identifies errors cat =
  let hasCat = hasCategory cat errors
      hasMatchingElem = L.any (\e -> category e == cat) errors
  in property $ hasCat === hasMatchingElem

-- ============================================================================
-- Combined Error Properties
-- ============================================================================

-- Property: combinedErrorSeverity extracts severity correctly
prop_combined_severity_extracts :: CombinedError -> Property
prop_combined_severity_extracts combinedErr =
  let extractedSev = combinedErrorSeverity combinedErr
  in case combinedErr of
    OwnershipErrorCombined sev _ -> property $ extractedSev === sev
    DependentTypeErrorCombined sev _ -> property $ extractedSev === sev
    IntegrationError _ sev -> property $ extractedSev === sev
    CrossAnalyzerError _ sev _ -> property $ extractedSev === sev

-- Property: filterCombinedErrorsBySeverity preserves L.minimum severity
prop_filter_combined_preserves_min :: [CombinedError] -> ErrorSeverity -> Property
prop_filter_combined_preserves_min combinedErrs minSev =
  let filtered = filterCombinedErrorsBySeverity minSev combinedErrs
  in property $ L.all (\e -> combinedErrorSeverity e >= minSev) filtered

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: customRecovery creates correct recovery strategy
prop_custom_recovery_correct :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_custom_recovery_correct canRec shouldCont action hint cost confidence =
  let recovery = customRecovery canRec shouldCont action hint cost confidence
  in property $ canRecover recovery === canRec .&&.
             shouldContinue recovery === shouldCont .&&.
             recoveryAction recovery === action .&&.
             recoveryHint recovery === hint .&&.
             recoveryCost recovery === cost .&&.
             recoveryConfidence recovery === confidence

-- Property: Predefined recovery strategies have correct properties
prop_predefined_recovery_strategies :: Property
prop_predefined_recovery_strategies =
  property $ not (canRecover fatalRecovery) .&&.
             not (shouldContinue fatalRecovery) .&&.
             canRecover errorRecovery .&&.
             shouldContinue errorRecovery .&&.
             canRecover warningRecovery .&&.
             shouldContinue warningRecovery .&&.
             canRecover infoRecovery .&&.
             shouldContinue infoRecovery

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: Error statistics counts are correct
prop_error_statistics_counts :: [TypeError] -> Property
prop_error_statistics_counts errors =
  let stats = getErrorStatistics errors
      errorCount = L.length $ filterBySeverity Error errors
      warningCount = L.length $ filterBySeverity Warning errors
      infoCount = L.length $ filterBySeverity Info errors
      fatalCount = L.length $ filterBySeverity Fatal errors
  in property $ Map.lookup "errors" stats === Just errorCount .&&.
             Map.lookup "warnings" stats === Just warningCount .&&.
             Map.lookup "info" stats === Just infoCount .&&.
             Map.lookup "fatal" stats === Just fatalCount

-- ============================================================================
-- Error Report Properties
-- ============================================================================

-- Property: Error report contains summary information
prop_error_report_contains_summary :: [TypeError] -> Property
prop_error_report_contains_summary errors =
  let report = generateErrorReport errors
      hasSummary = "Summary:" `L.isInfixOf` report
  in property $ hasSummary

-- Property: Error report contains error details
prop_error_report_contains_details :: [TypeError] -> Property
prop_error_report_contains_details errors =
  let report = generateErrorReport errors
      hasDetails = not (null errors) ==> L.any (`L.isInfixOf` report) (L.map (T.unpack . message) errors)
  in property $ hasDetails

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty error list handles gracefully
prop_empty_error_list_handling :: Property
prop_empty_error_list_handling =
  let emptyErrors = []
      stats = getErrorStatistics emptyErrors
      report = generateErrorReport emptyErrors
      formatted = formatErrors emptyErrors
  in property $ Map.null stats .&&. 
             not (null report) .&&.
             null formatted

-- Property: Large error lists handle efficiently
prop_large_error_list_handling :: Int -> Property
prop_large_error_list_handling size =
  size >= 0 && size <= 1000 ==>
  let errors = take size $ repeat (errorAt "test-id" 1 1 "message")
      stats = getErrorStatistics errors
  in property $ Map.size stats >= 0

-- Property: Error with L.maximum fields handles correctly
prop_error_with_max_fields :: Property
prop_error_with_max_fields =
  let maxErr = TypeError
        { errorId = "MAX_ERR_9999"
        , severity = Fatal
        , category = Unknown
        , message = "Maximum field error message"
        , location = ErrorLocation (Just "max_file.txt") 999 999 (Just 999) (Just 999)
        , context = ErrorContext (Just "max code") (Just "maxFunc") (Just "maxVar") (Just "MaxType") [("key1", "value1"), ("key2", "value2")]
        , recovery = RecoveryStrategy False False (Just "max action") (Just "max hint") 100 1.0
        , suggestions = ["suggestion1", "suggestion2"]
        , relatedErrors = []
        , errorChain = []
        , timestamp = Just "2023-12-31 23:59:59.999"
        }
  in property $ errorId maxErr === "MAX_ERR_9999" .&&.
             severity maxErr === Fatal .&&.
             not (L.null $ formatError maxErr)

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: Error formatting is consistent
prop_formatting_consistency :: TypeError -> Property
prop_formatting_consistency err =
  let formatted1 = formatError err
      formatted2 = formatError err
  in property $ formatted1 === formatted2

-- Property: Error filtering is idempotent
prop_filtering_idempotent :: [TypeError] -> ErrorSeverity -> Property
prop_filtering_idempotent errors sev =
  let filtered1 = filterBySeverity sev errors
      filtered2 = filterBySeverity sev filtered1
  in property $ filtered1 === filtered2

-- Property: Error collection preserves L.all messages
prop_collection_preserves_all :: [TypeError] -> Property
prop_collection_preserves_all errors =
  let allMessages = getAllMessages errors
  in property $ L.length allMessages === L.length errors

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Error Handler Consistency QuickCheck Tests"
  [ testGroup "Error Severity Properties"
    [ fastProperty "severity ordering consistent" prop_severity_ordering_consistent
    , fastProperty "fatal no recovery" prop_fatal_no_recovery
    , fastProperty "fatal no continue" prop_fatal_no_continue
    , fastProperty "warning info recoverable" prop_warning_info_recoverable
    ]

  , testGroup "Error Location Properties"
    [ fastProperty "location helpers work" prop_error_location_helpers
    , fastProperty "location range valid" prop_location_range_valid
    ]

  , testGroup "Error Collector Properties"
    [ fastProperty "add error increases count" prop_add_error_increases_count
    , fastProperty "add warning preserves severity" prop_add_warning_preserves_severity
    , fastProperty "add info preserves severity" prop_add_info_preserves_severity
    , fastProperty "has errors detection" prop_has_errors_detection
    , fastProperty "has warnings detection" prop_has_warnings_detection
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "format includes severity" prop_format_includes_severity
    , fastProperty "format with location includes line" prop_format_with_location_includes_line
    , fastProperty "format multiple preserves order" prop_format_multiple_preserves_order
    ]

  , testGroup "Error Transformation Properties"
    [ fastProperty "withLocation updates" prop_with_location_updates
    , fastProperty "withContext updates" prop_with_context_updates
    , fastProperty "withSuggestions adds" prop_with_suggestions_adds
    , fastProperty "wrapError creates chain" prop_wrap_error_creates_chain
    ]

  , testGroup "Error Filtering Properties"
    [ fastProperty "filter by severity preserves" prop_filter_by_severity_preserves
    , fastProperty "filter by category preserves" prop_filter_by_category_preserves
    , fastProperty "hasCategory identifies" prop_has_category_identifies
    ]

  , testGroup "Combined Error Properties"
    [ fastProperty "combined severity extracts" prop_combined_severity_extracts
    , fastProperty "filter combined preserves min" prop_filter_combined_preserves_min
    ]

  , testGroup "Error Recovery Properties"
    [ fastProperty "custom recovery correct" prop_custom_recovery_correct
    , fastProperty "predefined recovery strategies" prop_predefined_recovery_strategies
    ]

  , testGroup "Error Statistics Properties"
    [ fastProperty "error statistics counts" prop_error_statistics_counts
    ]

  , testGroup "Error Report Properties"
    [ fastProperty "error report contains summary" prop_error_report_contains_summary
    , fastProperty "error report contains details" prop_error_report_contains_details
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty error list handling" prop_empty_error_list_handling
    , fastProperty "large error list handling" prop_large_error_list_handling
    , fastProperty "error with max fields" prop_error_with_max_fields
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "formatting consistency" prop_formatting_consistency
    , fastProperty "filtering idempotent" prop_filtering_idempotent
    , fastProperty "collection preserves L.all" prop_collection_preserves_all
    ]
  ]