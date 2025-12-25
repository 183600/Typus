{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime)

import Compiler.Errors.Core
  ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..)
  , ErrorRecovery(..), TypeError(..), CombinedError(..)
  , emptyContext, fatalError, errorAt, warningAt, infoAt
  , errorWithCategory, errorWithSuggestions, withLocation, withContext
  , withSuggestions, withRelatedErrors, wrapError, combineErrors
  , hasCategory, filterByCategory, filterBySeverity, getErrorStatistics
  , formatError, formatErrorWithLocation, formatErrors, formatErrorsWithLocation
  , canRecoverFrom, shouldContinueAfter, combinedErrorSeverity
  , filterCombinedErrorsBySeverity, severityPriority, isAtLeast
  , getErrorLine, getErrorColumn, errorRecovery, warningRecovery, infoRecovery
  , customRecovery, fatalRecovery, createRecoveryStrategy
  , generateErrorReport, generateErrorReportWithTimestamp
  , formatTimestamp, getCurrentTimestamp, errorAtWithTimestamp
  , withTimestamp, withUTCTimestamp, errorAtWithUTCTime
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- oneof [return Nothing, fmap Just arbitrary]
    line <- choose (0, 1000)
    column <- choose (0, 1000)
    endLine <- oneof [return Nothing, fmap Just (choose (line, 1000))]
    endColumn <- oneof [return Nothing, fmap Just (choose (0, 1000))]
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- oneof [return Nothing, fmap Just arbitrary]
    contextFunction <- oneof [return Nothing, fmap Just arbitrary]
    contextVariable <- oneof [return Nothing, fmap Just arbitrary]
    contextType <- oneof [return Nothing, fmap Just arbitrary]
    contextAdditional <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- arbitrary
    shouldContinue <- arbitrary
    recoveryAction <- oneof [return Nothing, fmap Just arbitrary]
    recoveryHint <- oneof [return Nothing, fmap Just arbitrary]
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> arbitrary)
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, fmap Just arbitrary]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> arbitrary <*> arbitrary
    , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
    ]

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: severity priority ordering is consistent
prop_severity_priority_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_ordering sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
  in property $ (sev1 > sev2) === (p1 > p2)

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  isAtLeast sev sev === True

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  (isAtLeast sev1 sev2 && isAtLeast sev2 sev3) ==> isAtLeast sev1 sev3

-- Property: Fatal is the highest severity
prop_fatal_is_highest :: ErrorSeverity -> Property
prop_fatal_is_highest sev =
  isAtLeast sev Fatal === (sev == Fatal)

-- Property: Info is the lowest severity
prop_info_is_lowest :: ErrorSeverity -> Property
prop_info_is_lowest sev =
  isAtLeast Info sev === True

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: getErrorLine returns line from location
prop_getErrorLine_correct :: ErrorLocation -> Property
prop_getErrorLine_correct loc =
  getErrorLine loc === line loc

-- Property: getErrorColumn returns column from location
prop_getErrorColumn_correct :: ErrorLocation -> Property
prop_getErrorColumn_correct loc =
  getErrorColumn loc === column loc

-- Property: locations with valid line/column have positive values
prop_valid_location_positive :: ErrorLocation -> Property
prop_valid_location_positive loc =
  let validLine = line loc > 0
      validColumn = column loc > 0
  in property $ (validLine && validColumn) === (line loc > 0 && column loc > 0)

-- ============================================================================
-- Error Construction Properties
-- ============================================================================

-- Property: errorAt creates error with correct basic fields
prop_errorAt_correct_fields :: String -> Text -> ErrorLocation -> Property
prop_errorAt_correct_fields errId msg loc =
  let err = errorAt errId msg loc
  in property $ errorId err === errId .&&.
             message err === msg .&&.
             location err === loc .&&.
             severity err === Error .&&.
             category err === Unknown

-- Property: fatalError has Fatal severity
prop_fatal_error_severity :: String -> Text -> ErrorLocation -> Property
prop_fatal_error_severity errId msg loc =
  let err = fatalError errId msg loc
  in property $ severity err === Fatal .&&.
             canRecoverFrom err === False .&&.
             shouldContinueAfter err === False

-- Property: warningAt has Warning severity
prop_warning_at_severity :: String -> Text -> ErrorLocation -> Property
prop_warning_at_severity errId msg loc =
  let err = warningAt errId msg loc
  in property $ severity err === Warning

-- Property: infoAt has Info severity
prop_info_at_severity :: String -> Text -> ErrorLocation -> Property
prop_info_at_severity errId msg loc =
  let err = infoAt errId msg loc
  in property $ severity err === Info

-- Property: errorWithCategory sets correct category
prop_error_with_category :: String -> ErrorCategory -> Text -> ErrorLocation -> Property
prop_error_with_category errId cat msg loc =
  let err = errorWithCategory errId cat msg loc
  in property $ category err === cat .&&.
             severity err === Error

-- Property: errorWithSuggestions includes suggestions
prop_error_with_suggestions :: String -> Text -> [Text] -> ErrorLocation -> Property
prop_error_with_suggestions errId msg suggestions loc =
  let err = errorWithSuggestions errId msg suggestions loc
  in property $ suggestions err === suggestions

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation updates location
prop_with_location_updates :: TypeError -> ErrorLocation -> Property
prop_with_location_updates err loc =
  let updated = withLocation err loc
  in property $ location updated === loc .&&.
             message updated === message err .&&.
             severity updated === severity err

-- Property: withContext updates context
prop_with_context_updates :: TypeError -> ErrorContext -> Property
prop_with_context_updates err ctx =
  let updated = withContext err ctx
  in property $ context updated === ctx .&&.
             message updated === message err .&&.
             location updated === location err

-- Property: withSuggestions adds to existing suggestions
prop_with_suggestions_adds :: TypeError -> [Text] -> Property
prop_with_suggestions_adds err newSuggestions =
  let updated = withSuggestions newSuggestions err
  in property $ suggestions updated === newSuggestions ++ suggestions err

-- Property: withRelatedErrors adds to existing related errors
prop_with_related_errors_adds :: TypeError -> [TypeError] -> Property
prop_with_related_errors_adds err newRelated =
  let updated = withRelatedErrors newRelated err
  in property $ relatedErrors updated === newRelated ++ relatedErrors err

-- Property: wrapError prepends wrapper message
prop_wrap_error_prepends :: Text -> TypeError -> Property
prop_wrap_error_prepends wrapper err =
  let wrapped = wrapError wrapper err
  in property $ message wrapped === wrapper <> ": " <> message err .&&.
             errorChain wrapped === err : errorChain err

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory is true for matching category
prop_has_category_matching :: ErrorCategory -> TypeError -> Property
prop_has_category_matching cat err =
  hasCategory cat err === (category err == cat)

-- Property: filterByCategory only returns matching errors
prop_filter_by_category :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category cat errors =
  let filtered = filterByCategory cat errors
  in property $ all (hasCategory cat) filtered

-- Property: filterBySeverity only returns matching severity
prop_filter_by_severity :: ErrorSeverity -> [TypeError] -> Property
prop_filter_by_severity sev errors =
  let filtered = filterBySeverity sev errors
  in property $ all (\e -> severity e == sev) filtered

-- Property: filtering preserves order for matching elements
prop_filter_preserves_order :: ErrorCategory -> [TypeError] -> Property
prop_filter_preserves_order cat errors =
  let filtered = filterByCategory cat errors
      originalOrder = filter (hasCategory cat) errors
  in property $ filtered === originalOrder

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: error statistics count matches total
prop_error_stats_total :: [TypeError] -> Property
prop_error_stats_total errors =
  let stats = getErrorStatistics errors
      totalCount = fromMaybe 0 (Map.lookup "total" stats)
  in property $ totalCount === length errors

-- Property: error statistics severity counts are correct
prop_error_stats_severity_counts :: [TypeError] -> Property
prop_error_stats_severity_counts errors =
  let stats = getErrorStatistics errors
      fatalCount = fromMaybe 0 (Map.lookup "fatal" stats)
      errorCount = fromMaybe 0 (Map.lookup "errors" stats)
      warningCount = fromMaybe 0 (Map.lookup "warnings" stats)
      infoCount = fromMaybe 0 (Map.lookup "info" stats)
      expectedFatal = length $ filterBySeverity Fatal errors
      expectedError = length $ filterBySeverity Error errors
      expectedWarning = length $ filterBySeverity Warning errors
      expectedInfo = length $ filterBySeverity Info errors
  in property $ fatalCount === expectedFatal .&&.
             errorCount === expectedError .&&.
             warningCount === expectedWarning .&&.
             infoCount === expectedInfo

-- Property: error statistics category counts are correct
prop_error_stats_category_counts :: [TypeError] -> Property
prop_error_stats_category_counts errors =
  let stats = getErrorStatistics errors
      typeCheckingCount = fromMaybe 0 (Map.lookup "typeChecking" stats)
      ownershipCount = fromMaybe 0 (Map.lookup "ownership" stats)
      expectedTypeChecking = length $ filterByCategory TypeChecking errors
      expectedOwnership = length $ filterByCategory Ownership errors
  in property $ typeCheckingCount === expectedTypeChecking .&&.
             ownershipCount === expectedOwnership

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatError includes severity string
prop_format_error_includes_severity :: TypeError -> Property
prop_format_error_includes_severity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ severityStr `isInfixOf` formatted

-- Property: formatError includes category
prop_format_error_includes_category :: TypeError -> Property
prop_format_error_includes_category err =
  let formatted = formatError err
      categoryStr = "[" ++ show (category err) ++ "]"
  in property $ categoryStr `isInfixOf` formatted

-- Property: formatErrorWithLocation includes location info
prop_format_error_with_location_includes_location :: TypeError -> Property
prop_format_error_with_location_includes_location err =
  let formatted = formatErrorWithLocation err
      hasLocationInfo = line (location err) > 0 && column (location err) > 0
  in hasLocationInfo ==> property $ 
    let lineStr = show (line (location err))
        colStr = show (column (location err))
    in lineStr `isInfixOf` formatted && colStr `isInfixOf` formatted

-- Property: formatErrors includes all errors
prop_format_errors_includes_all :: [TypeError] -> Property
prop_format_errors_includes_all errors =
  not (null errors) ==>
  let formatted = formatErrors errors
      baseMessages = map (T.unpack . message) errors
  in property $ all (`isInfixOf` formatted) baseMessages

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom matches recovery strategy
prop_can_recover_from_matches :: TypeError -> Property
prop_can_recover_from_matches err =
  canRecoverFrom err === canRecover (recovery err)

-- Property: shouldContinueAfter matches recovery strategy
prop_should_continue_after_matches :: TypeError -> Property
prop_should_continue_after_matches err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- Property: fatalRecovery cannot recover
prop_fatal_recovery_cannot_recover :: Property
prop_fatal_recovery_cannot_recover =
  property $ canRecover fatalRecovery === False .&&.
             shouldContinue fatalRecovery === False

-- Property: errorRecovery can recover
prop_error_recovery_can_recover :: Property
prop_error_recovery_can_recover =
  property $ canRecover errorRecovery === True .&&.
             shouldContinue errorRecovery === True

-- Property: warningRecovery can recover
prop_warning_recovery_can_recover :: Property
prop_warning_recovery_can_recover =
  property $ canRecover warningRecovery === True .&&.
             shouldContinue warningRecovery === True

-- Property: infoRecovery can recover
prop_info_recovery_can_recover :: Property
prop_info_recovery_can_recover =
  property $ canRecover infoRecovery === True .&&.
             shouldContinue infoRecovery === True

-- Property: customRecovery creates strategy with given properties
prop_custom_recovery_correct :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_custom_recovery_correct canRec shouldCont action hint cost confidence =
  let recovery = customRecovery canRec shouldCont action hint cost confidence
  in property $ canRecover recovery === canRec .&&.
             shouldContinue recovery === shouldCont .&&.
             recoveryAction recovery === action .&&.
             recoveryHint recovery === hint .&&.
             recoveryCost recovery === cost .&&.
             recoveryConfidence recovery === confidence

-- ============================================================================
-- Combined Error Properties
-- ============================================================================

-- Property: combinedErrorSeverity extracts severity correctly
prop_combined_error_severity_correct :: CombinedError -> Property
prop_combined_error_severity_correct combinedErr =
  let extractedSev = combinedErrorSeverity combinedErr
      expectedSev = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in property $ extractedSev === expectedSev

-- Property: filterCombinedErrorsBySeverity works correctly
prop_filter_combined_errors_by_severity :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_errors_by_severity minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
  in property $ all (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) filtered

-- ============================================================================
-- Error Report Properties
-- ============================================================================

-- Property: error report includes statistics header
prop_error_report_includes_stats :: [TypeError] -> Property
prop_error_report_includes_stats errors =
  not (null errors) ==>
  let report = generateErrorReport errors
  in property $ "Statistics:" `isInfixOf` report

-- Property: error report includes total count
prop_error_report_includes_total :: [TypeError] -> Property
prop_error_report_includes_total errors =
  let report = generateErrorReport errors
      expectedTotal = "total: " ++ show (length errors)
  in property $ expectedTotal `isInfixOf` report

-- Property: error report with timestamp includes timestamp
prop_error_report_with_timestamp_includes_timestamp :: [TypeError] -> String -> Property
prop_error_report_with_timestamp_includes_timestamp errors timestamp =
  let report = generateErrorReportWithTimestamp (Just timestamp) errors
  in property $ ("Generated at: " ++ timestamp) `isInfixOf` report

-- ============================================================================
-- Timestamp Properties
-- ============================================================================

-- Property: formatTimestamp produces non-empty string
prop_format_timestamp_non_empty :: UTCTime -> Property
prop_format_timestamp_non_empty time =
  let formatted = formatTimestamp time
  in property $ not (null formatted)

-- Property: errorAtWithTimestamp sets timestamp
prop_error_at_with_timestamp_sets :: String -> String -> Text -> ErrorLocation -> Property
prop_error_at_with_timestamp_sets timestamp errId msg loc =
  let err = errorAtWithTimestamp timestamp errId msg loc
  in property $ timestamp err === Just timestamp

-- Property: withTimestamp updates timestamp
prop_with_timestamp_updates :: TypeError -> String -> Property
prop_with_timestamp_updates err timestamp =
  let updated = withTimestamp timestamp err
  in property $ timestamp updated === Just timestamp .&&.
             message updated === message err

-- ============================================================================
-- Complex Properties
-- ============================================================================

-- Property: combineErrors preserves all related errors
prop_combine_errors_preserves_related :: [TypeError] -> Property
prop_combine_errors_preserves_related errors =
  let combined = combineErrors errors
      allRelated = concatMap relatedErrors errors
  in property $ length combined >= length errors .&&.
             allRelated `isSublistOf` map relatedErrors combined

-- Property: error modification is idempotent for some operations
prop_with_location_idempotent :: TypeError -> ErrorLocation -> Property
prop_with_location_idempotent err loc =
  let once = withLocation err loc
      twice = withLocation once loc
  in property $ once === twice

-- Property: filtering by multiple criteria
prop_multiple_filtering :: ErrorCategory -> ErrorSeverity -> [TypeError] -> Property
prop_multiple_filtering cat sev errors =
  let byCategory = filterByCategory cat errors
      bySeverity = filterBySeverity sev byCategory
  in property $ all (\e -> category e == cat && severity e == sev) bySeverity

-- Helper function for sublist check
isSublistOf :: Eq a => [a] -> [[a]] -> Bool
isSublistOf _ [] = False
isSublistOf xs (y:ys) = xs `elem` y || isSublistOf xs ys

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ testGroup "Error Severity Properties"
    [ fastProperty "severity priority ordering is consistent" prop_severity_priority_ordering
    , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
    , fastProperty "Fatal is the highest severity" prop_fatal_is_highest
    , fastProperty "Info is the lowest severity" prop_info_is_lowest
    ]
  , testGroup "Error Location Properties"
    [ fastProperty "getErrorLine returns line from location" prop_getErrorLine_correct
    , fastProperty "getErrorColumn returns column from location" prop_getErrorColumn_correct
    , fastProperty "locations with valid line/column have positive values" prop_valid_location_positive
    ]
  , testGroup "Error Construction Properties"
    [ fastProperty "errorAt creates error with correct basic fields" prop_errorAt_correct_fields
    , fastProperty "fatalError has Fatal severity" prop_fatal_error_severity
    , fastProperty "warningAt has Warning severity" prop_warning_at_severity
    , fastProperty "infoAt has Info severity" prop_info_at_severity
    , fastProperty "errorWithCategory sets correct category" prop_error_with_category
    , fastProperty "errorWithSuggestions includes suggestions" prop_error_with_suggestions
    ]
  , testGroup "Error Modification Properties"
    [ fastProperty "withLocation updates location" prop_with_location_updates
    , fastProperty "withContext updates context" prop_with_context_updates
    , fastProperty "withSuggestions adds to existing suggestions" prop_with_suggestions_adds
    , fastProperty "withRelatedErrors adds to existing related errors" prop_with_related_errors_adds
    , fastProperty "wrapError prepends wrapper message" prop_wrap_error_prepends
    , fastProperty "withLocation is idempotent" prop_with_location_idempotent
    ]
  , testGroup "Error Filtering Properties"
    [ fastProperty "hasCategory is true for matching category" prop_has_category_matching
    , fastProperty "filterByCategory only returns matching errors" prop_filter_by_category
    , fastProperty "filterBySeverity only returns matching severity" prop_filter_by_severity
    , fastProperty "filtering preserves order for matching elements" prop_filter_preserves_order
    , fastProperty "filtering by multiple criteria" prop_multiple_filtering
    ]
  , testGroup "Error Statistics Properties"
    [ fastProperty "error statistics count matches total" prop_error_stats_total
    , fastProperty "error statistics severity counts are correct" prop_error_stats_severity_counts
    , fastProperty "error statistics category counts are correct" prop_error_stats_category_counts
    ]
  , testGroup "Error Formatting Properties"
    [ fastProperty "formatError includes severity string" prop_format_error_includes_severity
    , fastProperty "formatError includes category" prop_format_error_includes_category
    , fastProperty "formatErrorWithLocation includes location info" prop_format_error_with_location_includes_location
    , fastProperty "formatErrors includes all errors" prop_format_errors_includes_all
    ]
  , testGroup "Error Recovery Properties"
    [ fastProperty "canRecoverFrom matches recovery strategy" prop_can_recover_from_matches
    , fastProperty "shouldContinueAfter matches recovery strategy" prop_should_continue_after_matches
    , fastProperty "fatalRecovery cannot recover" prop_fatal_recovery_cannot_recover
    , fastProperty "errorRecovery can recover" prop_error_recovery_can_recover
    , fastProperty "warningRecovery can recover" prop_warning_recovery_can_recover
    , fastProperty "infoRecovery can recover" prop_info_recovery_can_recover
    , fastProperty "customRecovery creates strategy with given properties" prop_custom_recovery_correct
    ]
  , testGroup "Combined Error Properties"
    [ fastProperty "combinedErrorSeverity extracts severity correctly" prop_combined_error_severity_correct
    , fastProperty "filterCombinedErrorsBySeverity works correctly" prop_filter_combined_errors_by_severity
    ]
  , testGroup "Error Report Properties"
    [ fastProperty "error report includes statistics header" prop_error_report_includes_stats
    , fastProperty "error report includes total count" prop_error_report_includes_total
    , fastProperty "error report with timestamp includes timestamp" prop_error_report_with_timestamp_includes_timestamp
    ]
  , testGroup "Timestamp Properties"
    [ fastProperty "formatTimestamp produces non-empty string" prop_format_timestamp_non_empty
    , fastProperty "errorAtWithTimestamp sets timestamp" prop_error_at_with_timestamp_sets
    , fastProperty "withTimestamp updates timestamp" prop_with_timestamp_updates
    ]
  , testGroup "Complex Properties"
    [ fastProperty "combineErrors preserves all related errors" prop_combine_errors_preserves_related
    ]
  ]