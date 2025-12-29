{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose, suchThat)
import Compiler.Errors.Core
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..)
  , ErrorContext(..), ErrorRecovery(..), CombinedError(..)
  , emptyContext, errorAt, errorWithCategory, warningAt, infoAt, fatalError
  , formatError, formatErrorWithLocation, formatErrors, formatErrorsWithLocation
  , addError, addWarning, addInfo, getErrors, getWarnings, getInfo, getAllMessages
  , hasErrors, hasWarnings, canRecoverFrom, shouldContinueAfter
  , withLocation, withContext, withSuggestions, withRelatedErrors, withTimestamp
  , filterBySeverity, filterByCategory, hasCategory, combinedErrorSeverity
  , filterCombinedErrorsBySeverity, severityPriority, isAtLeast
  , getErrorLine, getErrorColumn, _atLocation, _atFileLocation, _atRange
  , fatalRecovery, errorRecovery, warningRecovery, infoRecovery
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (sort, nub)
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson (ToJSON, FromJSON)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- oneof [return Nothing, Just <$> arbitrary]
    line <- choose (0, 1000)
    column <- choose (0, 1000)
    endLine <- oneof [return Nothing, Just <$> choose (line, line + 100)]
    endColumn <- oneof [return Nothing, Just <$> choose (column, column + 100)]
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- oneof [return Nothing, Just <$> arbitrary]
    contextFunction <- oneof [return Nothing, Just <$> arbitrary]
    contextVariable <- oneof [return Nothing, Just <$> arbitrary]
    contextType <- oneof [return Nothing, Just <$> arbitrary]
    contextAdditional <- listOf $ arbitrary `suchThat` (\(x, y) -> not (null x || null y))
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover' <- arbitrary
    shouldContinue' <- arbitrary
    recoveryAction <- oneof [return Nothing, Just <$> arbitrary]
    recoveryHint <- oneof [return Nothing, Just <$> arbitrary]
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ ErrorRecovery canRecover' shouldContinue' recoveryAction recoveryHint recoveryCost recoveryConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary `suchThat` (not . null)
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> arbitrary `suchThat` (not . null)
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> arbitrary `suchThat` (not . null))
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, Just <$> arbitrary]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> arbitrary <*> arbitrary
    , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
    ]

-- Generate valid error locations
genValidErrorLocation :: Gen ErrorLocation
genValidErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  filePath <- oneof [return Nothing, Just <$> arbitrary]
  endLine <- oneof [return Nothing, Just <$> choose (line, line + 50)]
  endColumn <- oneof [return Nothing, Just <$> choose (column, column + 50)]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error with specific severity
genErrorWithSeverity :: ErrorSeverity -> Gen TypeError
genErrorWithSeverity sev = do
  errorId <- arbitrary `suchThat` (not . null)
  category <- arbitrary
  message <- T.pack <$> arbitrary `suchThat` (not . null)
  location <- genValidErrorLocation
  context <- arbitrary
  recovery <- arbitrary
  suggestions <- listOf (T.pack <$> arbitrary `suchThat` (not . null))
  relatedErrors <- listOf arbitrary
  errorChain <- listOf arbitrary
  timestamp <- oneof [return Nothing, Just <$> arbitrary]
  return $ TypeError errorId sev category message location context recovery suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- ErrorSeverity QuickCheck Tests
-- ============================================================================

-- Test severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Test isAtLeast function
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev = isAtLeast sev sev

prop_isAt_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAt_least_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- ============================================================================
-- ErrorLocation QuickCheck Tests
-- ============================================================================

-- Test error location helper functions
prop_getErrorLine_returns_line :: ErrorLocation -> Property
prop_getErrorLine_returns_line loc = getErrorLine loc === line loc

prop_getErrorColumn_returns_column :: ErrorLocation -> Property
prop_getErrorColumn_returns_column loc = getErrorColumn loc === column loc

-- Test location creation functions
prop_atLocation_creates_correct_location :: Int -> Int -> Property
prop_atLocation_creates_correct_location lineNum col =
  lineNum > 0 && col > 0 ==>
  let loc = _atLocation lineNum col
  in line loc === lineNum .&&. column loc === col .&&.
     filePath loc === Nothing .&&. endLine loc === Nothing .&&.
     endColumn loc === Nothing

prop_atFileLocation_includes_file :: String -> Int -> Int -> Property
prop_atFileLocation_includes_file file lineNum col =
  not (null file) && lineNum > 0 && col > 0 ==>
  let loc = _atFileLocation file lineNum col
  in filePath loc === Just file .&&. line loc === lineNum .&&. column loc === col

prop_atRange_creates_range :: Int -> Int -> Int -> Int -> Property
prop_atRange_creates_range startLine startCol endLineNum endCol =
  startLine <= endLineNum && (startLine < endLineNum || startCol <= endCol) ==>
  let loc = _atRange startLine startCol endLineNum endCol
  in line loc === startLine .&&. column loc === startCol .&&.
     endLine loc === Just endLineNum .&&. endColumn loc === Just endCol

-- ============================================================================
-- ErrorContext QuickCheck Tests
-- ============================================================================

-- Test empty context
prop_empty_context_has_no_fields :: Property
prop_empty_context_has_no_fields =
  contextCode emptyContext === Nothing .&&.
  contextFunction emptyContext === Nothing .&&.
  contextVariable emptyContext === Nothing .&&.
  contextType emptyContext === Nothing .&&.
  null (contextAdditional emptyContext)

-- ============================================================================
-- ErrorRecovery QuickCheck Tests
-- ============================================================================

-- Test predefined recovery strategies
prop_fatal_recovery_cannot_recover :: Property
prop_fatal_recovery_cannot_recover =
  not (canRecover fatalRecovery) .&&. not (shouldContinue fatalRecovery)

prop_error_recovery_can_recover :: Property
prop_error_recovery_can_recover =
  canRecover errorRecovery .&&. shouldContinue errorRecovery

prop_warning_recovery_can_recover :: Property
prop_warning_recovery_can_recover =
  canRecover warningRecovery .&&. shouldContinue warningRecovery

prop_info_recovery_can_recover :: Property
prop_info_recovery_can_recover =
  canRecover infoRecovery .&&. shouldContinue infoRecovery

-- ============================================================================
-- TypeError QuickCheck Tests
-- ============================================================================

-- Test error creation functions
prop_errorAt_creates_error_with_correct_severity :: String -> Text -> ErrorLocation -> Property
prop_errorAt_creates_error_with_correct_severity errId msg loc =
  let err = errorAt errId msg loc
  in severity err === Error .&&.
     errorId err === errId .&&.
     message err === msg .&&.
     location err === loc .&&.
     category err === Unknown

prop_errorWithCategory_sets_category :: String -> ErrorCategory -> Text -> ErrorLocation -> Property
prop_errorWithCategory_sets_category errId cat msg loc =
  let err = errorWithCategory errId cat msg loc
  in category err === cat .&&. severity err === Error

prop_warningAt_creates_warning :: String -> Text -> ErrorLocation -> Property
prop_warningAt_creates_warning errId msg loc =
  severity (warningAt errId msg loc) === Warning

prop_infoAt_creates_info :: String -> Text -> ErrorLocation -> Property
prop_infoAt_creates_info errId msg loc =
  severity (infoAt errId msg loc) === Info

prop_fatalError_creates_fatal :: String -> Text -> ErrorLocation -> Property
prop_fatalError_creates_fatal errId msg loc =
  severity (fatalError errId msg loc) === Fatal

-- Test error modifier functions
prop_withLocation_updates_location :: TypeError -> ErrorLocation -> Property
prop_withLocation_updates_location err newLoc =
  location (withLocation newLoc err) === newLoc

prop_withContext_updates_context :: TypeError -> ErrorContext -> Property
prop_withContext_updates_context err newCtx =
  context (withContext newCtx err) === newCtx

prop_withSuggestions_adds_suggestions :: TypeError -> [Text] -> Property
prop_withSuggestions_adds_suggestions err newSugs =
  suggestions (withSuggestions newSugs err) === newSugs

prop_withRelatedErrors_adds_related_errors :: TypeError -> [TypeError] -> Property
prop_withRelatedErrors_adds_related_errors err relatedErrs =
  relatedErrors (withRelatedErrors relatedErrs err) === relatedErrs

prop_withTimestamp_adds_timestamp :: TypeError -> String -> Property
prop_withTimestamp_adds_timestamp err ts =
  timestamp (withTimestamp ts err) === Just ts

-- Test error recovery functions
prop_canRecoverFrom_uses_recovery_field :: TypeError -> Property
prop_canRecoverFrom_uses_recovery_field err =
  canRecoverFrom err === canRecover (recovery err)

prop_shouldContinueAfter_uses_recovery_field :: TypeError -> Property
prop_shouldContinueAfter_uses_recovery_field err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- ============================================================================
-- ErrorCollector QuickCheck Tests
-- ============================================================================

-- Test error collection functions
prop_getErrors_filters_by_severity :: [TypeError] -> Property
prop_getErrors_filters_by_severity errs =
  all (\e -> severity e == Error || severity e == Fatal) (getErrors errs)

prop_getWarnings_filters_by_severity :: [TypeError] -> Property
prop_getWarnings_filters_by_severity errs =
  all (\e -> severity e == Warning) (getWarnings errs)

prop_getInfo_filters_by_severity :: [TypeError] -> Property
prop_getInfo_filters_by_severity errs =
  all (\e -> severity e == Info) (getInfo errs)

prop_getAllMessages_returns_all :: [TypeError] -> Property
prop_getAllMessages_returns_all errs =
  getAllMessages errs === errs

prop_hasErrors_detects_errors :: [TypeError] -> Property
prop_hasErrors_detects_errors errs =
  hasErrors errs === not (null (getErrors errs))

prop_hasWarnings_detects_warnings :: [TypeError] -> Property
prop_hasWarnings_detects_warnings errs =
  hasWarnings errs === not (null (getWarnings errs))

-- ============================================================================
-- Error Formatting QuickCheck Tests
-- ============================================================================

-- Test error formatting functions
prop_formatError_includes_severity_and_message :: TypeError -> Property
prop_formatError_includes_severity_and_message err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
      msgStr = T.unpack (message err)
  in severityStr `isInfixOf` formatted .&&. msgStr `isInfixOf` formatted

prop_formatErrorWithLocation_includes_location :: TypeError -> Property
prop_formatErrorWithLocation_includes_location err =
  let formatted = formatErrorWithLocation err
      locationStr = show (line (location err)) ++ ":" ++ show (column (location err))
  in locationStr `isInfixOf` formatted

prop_formatErrors_formats_multiple :: [TypeError] -> Property
prop_formatErrors_formats_multiple errs =
  let formatted = formatErrors errs
      formattedLines = lines formatted
  in length formattedLines >= length errs

-- ============================================================================
-- Error Filtering QuickCheck Tests
-- ============================================================================

-- Test error filtering functions
prop_filterBySeverity_filters_correctly :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_filters_correctly minSev errs =
  let filtered = filterBySeverity minSev errs
  in all (\e -> isAtLeast minSev (severity e)) filtered

prop_filterByCategory_filters_correctly :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_filters_correctly cat errs =
  let filtered = filterByCategory cat errs
  in all (\e -> category e == cat) filtered

prop_hasCategory_detects_category :: ErrorCategory -> [TypeError] -> Property
prop_hasCategory_detects_category cat errs =
  hasCategory cat errs === any (\e -> category e == cat) errs

-- ============================================================================
-- CombinedError QuickCheck Tests
-- ============================================================================

-- Test combined error severity
prop_combinedErrorSeverity_matches_constructor :: CombinedError -> Property
prop_combinedErrorSeverity_matches_constructor combinedErr =
  case combinedErr of
    OwnershipErrorCombined sev _ -> combinedErrorSeverity combinedErr === sev
    DependentTypeErrorCombined sev _ -> combinedErrorSeverity combinedErr === sev
    IntegrationError _ sev -> combinedErrorSeverity combinedErr === sev
    CrossAnalyzerError _ sev _ -> combinedErrorSeverity combinedErr === sev

-- Test combined error filtering
prop_filterCombinedErrorsBySeverity_filters_correctly :: ErrorSeverity -> [CombinedError] -> Property
prop_filterCombinedErrorsBySeverity_filters_correctly minSev combinedErrs =
  let filtered = filterCombinedErrorsBySeverity minSev combinedErrs
  in all (\e -> isAtLeast minSev (combinedErrorSeverity e)) filtered

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- Test error ordering by severity
prop_severity_ordering_consistent :: TypeError -> TypeError -> Property
prop_severity_ordering_consistent err1 err2 =
  let sev1 = severity err1
      sev2 = severity err2
  in (sev1 > sev2) === (severityPriority sev1 > severityPriority sev2)

-- Test error modification preserves invariants
prop_error_modification_preserves_id :: TypeError -> String -> Property
prop_error_modification_preserves_id err newId =
  errorId (withTimestamp "2023-01-01" err) === errorId err

-- Test context formatting
prop_context_with_code_includes_code :: ErrorContext -> Property
prop_context_with_code_includes_code ctx =
  case contextCode ctx of
    Just code -> not (null code) ==> code `isInfixOf` show ctx
    Nothing -> property True

tests :: TestTree
tests = testGroup "New Cabal ErrorHandler QuickCheck Tests"
  [ testGroup "ErrorSeverity tests"
      [ testProperty "severity priority ordering" prop_severity_priority_ordering
      , testProperty "isAtLeast reflexive" prop_isAtLeast_reflexive
      , testProperty "isAtLeast transitive" prop_isAt_least_transitive
      ]
  , testGroup "ErrorLocation tests"
      [ testProperty "getErrorLine returns line" prop_getErrorLine_returns_line
      , testProperty "getErrorColumn returns column" prop_getErrorColumn_returns_column
      , testProperty "atLocation creates correct location" prop_atLocation_creates_correct_location
      , testProperty "atFileLocation includes file" prop_atFileLocation_includes_file
      , testProperty "atRange creates range" prop_atRange_creates_range
      ]
  , testGroup "ErrorContext tests"
      [ testProperty "empty context has no fields" prop_empty_context_has_no_fields
      ]
  , testGroup "ErrorRecovery tests"
      [ testProperty "fatal recovery cannot recover" prop_fatal_recovery_cannot_recover
      , testProperty "error recovery can recover" prop_error_recovery_can_recover
      , testProperty "warning recovery can recover" prop_warning_recovery_can_recover
      , testProperty "info recovery can recover" prop_info_recovery_can_recover
      ]
  , testGroup "TypeError tests"
      [ testProperty "errorAt creates error with correct severity" prop_errorAt_creates_error_with_correct_severity
      , testProperty "errorWithCategory sets category" prop_errorWithCategory_sets_category
      , testProperty "warningAt creates warning" prop_warningAt_creates_warning
      , testProperty "infoAt creates info" prop_infoAt_creates_info
      , testProperty "fatalError creates fatal" prop_fatalError_creates_fatal
      , testProperty "withLocation updates location" prop_withLocation_updates_location
      , testProperty "withContext updates context" prop_withContext_updates_context
      , testProperty "withSuggestions adds suggestions" prop_withSuggestions_adds_suggestions
      , testProperty "withRelatedErrors adds related errors" prop_withRelatedErrors_adds_related_errors
      , testProperty "withTimestamp adds timestamp" prop_withTimestamp_adds_timestamp
      , testProperty "canRecoverFrom uses recovery field" prop_canRecoverFrom_uses_recovery_field
      , testProperty "shouldContinueAfter uses recovery field" prop_shouldContinueAfter_uses_recovery_field
      ]
  , testGroup "ErrorCollector tests"
      [ testProperty "getErrors filters by severity" prop_getErrors_filters_by_severity
      , testProperty "getWarnings filters by severity" prop_getWarnings_filters_by_severity
      , testProperty "getInfo filters by severity" prop_getInfo_filters_by_severity
      , testProperty "getAllMessages returns all" prop_getAllMessages_returns_all
      , testProperty "hasErrors detects errors" prop_hasErrors_detects_errors
      , testProperty "hasWarnings detects warnings" prop_hasWarnings_detects_warnings
      ]
  , testGroup "Error formatting tests"
      [ testProperty "formatError includes severity and message" prop_formatError_includes_severity_and_message
      , testProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocation_includes_location
      , testProperty "formatErrors formats multiple" prop_formatErrors_formats_multiple
      ]
  , testGroup "Error filtering tests"
      [ testProperty "filterBySeverity filters correctly" prop_filterBySeverity_filters_correctly
      , testProperty "filterByCategory filters correctly" prop_filterByCategory_filters_correctly
      , testProperty "hasCategory detects category" prop_hasCategory_detects_category
      ]
  , testGroup "CombinedError tests"
      [ testProperty "combinedErrorSeverity matches constructor" prop_combinedErrorSeverity_matches_constructor
      , testProperty "filterCombinedErrorsBySeverity filters correctly" prop_filterCombinedErrorsBySeverity_filters_correctly
      ]
  , testGroup "Additional property tests"
      [ testProperty "severity ordering consistent" prop_severity_ordering_consistent
      , testProperty "error modification preserves id" prop_error_modification_preserves_id
      , testProperty "context with code includes code" prop_context_with_code_includes_code
      ]
  ]