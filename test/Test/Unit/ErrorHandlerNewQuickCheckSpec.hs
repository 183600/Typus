{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf)
import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , errorAt
  , errorAtWithTimestamp
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , formatError
  , formatErrorWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , severityPriority
  , isAtLeast
  , createRecoveryStrategy
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , getErrorLine
  , getErrorColumn
  )

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- choose (0, 1000)
    column <- choose (0, 1000)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- listOf $ arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recoveryAction <- arbitrary
    recoveryHint <- arbitrary
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont recoveryAction recoveryHint recoveryCost recoveryConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (pack <$> arbitrary)
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Property: isAtLeast reflexivity
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  isAtLeast sev sev

-- Property: isAtLeast transitivity
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: Fatal is at least all severities
prop_fatal_is_at_least_all :: ErrorSeverity -> Property
prop_fatal_is_at_least_all sev =
  isAtLeast Fatal sev

-- Property: Info is not at least higher severities
prop_info_not_at_least_higher :: ErrorSeverity -> Property
prop_info_not_at_least_higher sev =
  sev /= Info ==> not (isAtLeast Info sev)

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: getErrorLine returns line field
prop_getErrorLine_correct :: ErrorLocation -> Property
prop_getErrorLine_correct loc =
  getErrorLine loc === line loc

-- Property: getErrorColumn returns column field
prop_getErrorColumn_correct :: ErrorLocation -> Property
prop_getErrorColumn_correct loc =
  getErrorColumn loc === column loc

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates error with correct fields
prop_errorAt_correct_fields :: String -> String -> ErrorLocation -> Property
prop_errorAt_correct_fields errId msg loc =
  let err = errorAt errId (pack msg) loc
  in errorId err === errId .&&.
     message err === pack msg .&&.
     location err === loc .&&.
     severity err === Error .&&.
     category err === Unknown .&&.
     context err === emptyContext .&&.
     recovery err === errorRecovery .&&.
     suggestions err === [] .&&.
     relatedErrors err === [] .&&.
     errorChain err === [] .&&.
     timestamp err === Nothing

-- Property: errorWithCategory creates error with correct category
prop_errorWithCategory_correct_category :: String -> ErrorCategory -> String -> ErrorLocation -> Property
prop_errorWithCategory_correct_category errId errCategory msg loc =
  let err = errorWithCategory errId errCategory (pack msg) loc
  in category err === errCategory

-- Property: warningAt creates warning
prop_warningAt_creates_warning :: String -> String -> ErrorLocation -> Property
prop_warningAt_creates_warning errId msg loc =
  let err = warningAt errId (pack msg) loc
  in severity err === Warning

-- Property: infoAt creates info
prop_infoAt_creates_info :: String -> String -> ErrorLocation -> Property
prop_infoAt_creates_info errId msg loc =
  let err = infoAt errId (pack msg) loc
  in severity err === Info

-- Property: fatalError creates fatal error
prop_fatalError_creates_fatal :: String -> String -> ErrorLocation -> Property
prop_fatalError_creates_fatal errId msg loc =
  let err = fatalError errId (pack msg) loc
  in severity err === Fatal .&&.
     recovery err === fatalRecovery

-- Property: errorWithSuggestions creates error with suggestions
prop_errorWithSuggestions_suggestions :: String -> String -> [String] -> ErrorLocation -> Property
prop_errorWithSuggestions_suggestions errId msg suggestions loc =
  let err = errorWithSuggestions errId (pack msg) (map pack suggestions) loc
  in suggestions err === map pack suggestions

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation changes location
prop_withLocation_changes_location :: TypeError -> ErrorLocation -> Property
prop_withLocation_changes_location err newLoc =
  let modifiedErr = withLocation err newLoc
  in location modifiedErr === newLoc

-- Property: withContext changes context
prop_withContext_changes_context :: TypeError -> ErrorContext -> Property
prop_withContext_changes_context err newCtx =
  let modifiedErr = withContext err newCtx
  in context modifiedErr === newCtx

-- Property: withSuggestions adds suggestions
prop_withSuggestions_adds_suggestions :: TypeError -> [String] -> Property
prop_withSuggestions_adds_suggestions err newSuggestions =
  let modifiedErr = withSuggestions (map pack newSuggestions) err
  in all (`elem` suggestions modifiedErr) (map pack newSuggestions)

-- Property: withTimestamp adds timestamp
prop_withTimestamp_adds_timestamp :: TypeError -> String -> Property
prop_withTimestamp_adds_timestamp err timestamp =
  let modifiedErr = withTimestamp timestamp err
  in timestamp modifiedErr === Just timestamp

-- Property: wrapError adds to message and chain
prop_wrapError_adds_to_message_and_chain :: TypeError -> String -> Property
prop_wrapError_adds_to_message_and_chain err wrapperMsg =
  let wrappedErr = wrapError (pack wrapperMsg) err
  in message wrappedErr === pack wrapperMsg <> ": " <> message err .&&.
     err `elem` errorChain wrappedErr

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory correctly identifies category
prop_hasCategory_correct :: ErrorCategory -> TypeError -> Property
prop_hasCategory_correct cat err =
  hasCategory cat err === (category err == cat)

-- Property: filterByCategory returns only errors with specified category
prop_filterByCategory_correct :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_correct cat errors =
  let filtered = filterByCategory cat errors
  in all (\e -> category e == cat) filtered

-- Property: filterBySeverity returns only errors with specified severity
prop_filterBySeverity_correct :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_correct sev errors =
  let filtered = filterBySeverity sev errors
  in all (\e -> severity e == sev) filtered

-- Property: filtering by category preserves count of matching errors
prop_filterByCategory_preserves_count :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_preserves_count cat errors =
  let filtered = filterByCategory cat errors
      expected = length $ filter (\e -> category e == cat) errors
  in length filtered === expected

-- Property: filtering by severity preserves count of matching errors
prop_filterBySeverity_preserves_count :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_preserves_count sev errors =
  let filtered = filterBySeverity sev errors
      expected = length $ filter (\e -> severity e == sev) errors
  in length filtered === expected

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: getErrorStatistics total count matches input
prop_getErrorStatistics_total_count :: [TypeError] -> Property
prop_getErrorStatistics_total_count errors =
  let stats = getErrorStatistics errors
  in Map.findWithDefault 0 "total" stats === length errors

-- Property: getErrorStatistics severity counts are correct
prop_getErrorStatistics_severity_counts :: [TypeError] -> Property
prop_getErrorStatistics_severity_counts errors =
  let stats = getErrorStatistics errors
      fatalCount = Map.findWithDefault 0 "fatal" stats
      errorCount = Map.findWithDefault 0 "errors" stats
      warningCount = Map.findWithDefault 0 "warnings" stats
      infoCount = Map.findWithDefault 0 "info" stats
      expectedFatal = length $ filter (\e -> severity e == Fatal) errors
      expectedError = length $ filter (\e -> severity e == Error) errors
      expectedWarning = length $ filter (\e -> severity e == Warning) errors
      expectedInfo = length $ filter (\e -> severity e == Info) errors
  in fatalCount === expectedFatal .&&.
     errorCount === expectedError .&&.
     warningCount === expectedWarning .&&.
     infoCount === expectedInfo

-- Property: getErrorStatistics category counts are correct
prop_getErrorStatistics_category_counts :: [TypeError] -> Property
prop_getErrorStatistics_category_counts errors =
  let stats = getErrorStatistics errors
      typeCheckingCount = Map.findWithDefault 0 "typeChecking" stats
      ownershipCount = Map.findWithDefault 0 "ownership" stats
      parsingCount = Map.findWithDefault 0 "parsing" stats
      expectedTypeChecking = length $ filter (\e -> category e == TypeChecking) errors
      expectedOwnership = length $ filter (\e -> category e == Ownership) errors
      expectedParsing = length $ filter (\e -> category e == Parsing) errors
  in typeCheckingCount === expectedTypeChecking .&&.
     ownershipCount === expectedOwnership .&&.
     parsingCount === expectedParsing

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom returns recovery canRecover field
prop_canRecoverFrom_correct :: TypeError -> Property
prop_canRecoverFrom_correct err =
  canRecoverFrom err === canRecover (recovery err)

-- Property: shouldContinueAfter returns recovery shouldContinue field
prop_shouldContinueAfter_correct :: TypeError -> Property
prop_shouldContinueAfter_correct err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- Property: fatalRecovery cannot recover
prop_fatalRecovery_cannot_recover :: Property
prop_fatalRecovery_cannot_recover =
  not (canRecover fatalRecovery) && not (shouldContinue fatalRecovery)

-- Property: errorRecovery can recover and continue
prop_errorRecovery_can_recover_continue :: Property
prop_errorRecovery_can_recover_continue =
  canRecover errorRecovery && shouldContinue errorRecovery

-- Property: warningRecovery can recover and continue
prop_warningRecovery_can_recover_continue :: Property
prop_warningRecovery_can_recover_continue =
  canRecover warningRecovery && shouldContinue warningRecovery

-- Property: infoRecovery can recover and continue
prop_infoRecovery_can_recover_continue :: Property
prop_infoRecovery_can_recover_continue =
  canRecover infoRecovery && shouldContinue infoRecovery

-- Property: customRecovery creates recovery with specified values
prop_customRecovery_specified_values :: Bool -> Bool -> String -> String -> Int -> Float -> Property
prop_customRecovery_specified_values canRec shouldCont action hint cost confidence =
  let recovery = customRecovery canRec shouldCont (Just action) (Just hint) cost confidence
  in canRecover recovery === canRec .&&.
     shouldContinue recovery === shouldCont .&&.
     recoveryAction recovery === Just action .&&.
     recoveryHint recovery === Just hint .&&.
     recoveryCost recovery === cost .&&.
     recoveryConfidence recovery === confidence

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatError contains severity string
prop_formatError_contains_severity :: TypeError -> Property
prop_formatError_contains_severity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in severityStr `isInfixOf` formatted

-- Property: formatError contains message
prop_formatError_contains_message :: TypeError -> Property
prop_formatError_contains_message err =
  let formatted = formatError err
  in T.unpack (message err) `isInfixOf` formatted

-- Property: formatErrorWithLocation contains location information
prop_formatErrorWithLocation_contains_location :: TypeError -> Property
prop_formatErrorWithLocation_contains_location err =
  let formatted = formatErrorWithLocation err
      hasLocation = line (location err) > 0 || column (location err) > 0
  in if hasLocation
     then (show $ line (location err)) `isInfixOf` formatted && 
          (show $ column (location err)) `isInfixOf` formatted
     else property True

-- ============================================================================
-- Error Combination Properties
-- ============================================================================

-- Property: combineErrors includes original errors
prop_combineErrors_includes_original :: [TypeError] -> Property
prop_combineErrors_includes_original errors =
  let combined = combineErrors errors
  in all (`elem` combined) errors

-- Property: combineErrors includes related errors
prop_combineErrors_includes_related :: [TypeError] -> Property
prop_combineErrors_includes_related errors =
  let combined = combineErrors errors
      allRelated = concatMap relatedErrors errors
  in all (`elem` combined) allRelated

-- ============================================================================
-- Combined Error Properties
-- ============================================================================

-- Property: combinedErrorSeverity extracts severity correctly
prop_combinedErrorSeverity_correct :: CombinedError -> Property
prop_combinedErrorSeverity_correct combinedErr =
  let extractedSeverity = combinedErrorSeverity combinedErr
      expectedSeverity = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in extractedSeverity === expectedSeverity

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filterCombinedErrorsBySeverity_correct :: ErrorSeverity -> [CombinedError] -> Property
prop_filterCombinedErrorsBySeverity_correct minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
  in all (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) filtered

-- ============================================================================
-- Timestamp Properties
-- ============================================================================

-- Property: errorAtWithTimestamp sets timestamp
prop_errorAtWithTimestamp_sets_timestamp :: String -> String -> String -> ErrorLocation -> Property
prop_errorAtWithTimestamp_sets_timestamp errId timestamp msg loc =
  let err = errorAtWithTimestamp timestamp errId (pack msg) loc
  in timestamp err === Just timestamp

-- Property: withTimestamp overrides existing timestamp
prop_withTimestamp_overrides :: TypeError -> String -> Property
prop_withTimestamp_overrides err newTimestamp =
  let modifiedErr = withTimestamp newTimestamp err
  in timestamp modifiedErr === Just newTimestamp

-- ============================================================================
-- Error Context Properties
-- ============================================================================

-- Property: emptyContext has all fields as Nothing/empty
prop_emptyContext_values :: Property
prop_emptyContext_values =
  contextCode emptyContext === Nothing .&&.
  contextFunction emptyContext === Nothing .&&.
  contextVariable emptyContext === Nothing .&&.
  contextType emptyContext === Nothing .&&.
  contextAdditional emptyContext === []

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ testGroup "Error Severity Properties"
    [ fastProperty "severity priority ordering" prop_severity_priority_ordering
    , fastProperty "isAtLeast reflexivity" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast transitivity" prop_isAtLeast_transitive
    , fastProperty "Fatal is at least all severities" prop_fatal_is_at_least_all
    , fastProperty "Info is not at least higher severities" prop_info_not_at_least_higher
    ]

  , testGroup "Error Location Properties"
    [ fastProperty "getErrorLine returns line field" prop_getErrorLine_correct
    , fastProperty "getErrorColumn returns column field" prop_getErrorColumn_correct
    ]

  , testGroup "Error Creation Properties"
    [ fastProperty "errorAt creates error with correct fields" prop_errorAt_correct_fields
    , fastProperty "errorWithCategory creates error with correct category" prop_errorWithCategory_correct_category
    , fastProperty "warningAt creates warning" prop_warningAt_creates_warning
    , fastProperty "infoAt creates info" prop_infoAt_creates_info
    , fastProperty "fatalError creates fatal error" prop_fatalError_creates_fatal
    , fastProperty "errorWithSuggestions creates error with suggestions" prop_errorWithSuggestions_suggestions
    ]

  , testGroup "Error Modification Properties"
    [ fastProperty "withLocation changes location" prop_withLocation_changes_location
    , fastProperty "withContext changes context" prop_withContext_changes_context
    , fastProperty "withSuggestions adds suggestions" prop_withSuggestions_adds_suggestions
    , fastProperty "withTimestamp adds timestamp" prop_withTimestamp_adds_timestamp
    , fastProperty "wrapError adds to message and chain" prop_wrapError_adds_to_message_and_chain
    ]

  , testGroup "Error Filtering Properties"
    [ fastProperty "hasCategory correctly identifies category" prop_hasCategory_correct
    , fastProperty "filterByCategory returns only errors with specified category" prop_filterByCategory_correct
    , fastProperty "filterBySeverity returns only errors with specified severity" prop_filterBySeverity_correct
    , fastProperty "filtering by category preserves count of matching errors" prop_filterByCategory_preserves_count
    , fastProperty "filtering by severity preserves count of matching errors" prop_filterBySeverity_preserves_count
    ]

  , testGroup "Error Statistics Properties"
    [ fastProperty "getErrorStatistics total count matches input" prop_getErrorStatistics_total_count
    , fastProperty "getErrorStatistics severity counts are correct" prop_getErrorStatistics_severity_counts
    , fastProperty "getErrorStatistics category counts are correct" prop_getErrorStatistics_category_counts
    ]

  , testGroup "Error Recovery Properties"
    [ fastProperty "canRecoverFrom returns recovery canRecover field" prop_canRecoverFrom_correct
    , fastProperty "shouldContinueAfter returns recovery shouldContinue field" prop_shouldContinueAfter_correct
    , fastProperty "fatalRecovery cannot recover" prop_fatalRecovery_cannot_recover
    , fastProperty "errorRecovery can recover and continue" prop_errorRecovery_can_recover_continue
    , fastProperty "warningRecovery can recover and continue" prop_warningRecovery_can_recover_continue
    , fastProperty "infoRecovery can recover and continue" prop_infoRecovery_can_recover_continue
    , fastProperty "customRecovery creates recovery with specified values" prop_customRecovery_specified_values
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatError contains severity string" prop_formatError_contains_severity
    , fastProperty "formatError contains message" prop_formatError_contains_message
    , fastProperty "formatErrorWithLocation contains location information" prop_formatErrorWithLocation_contains_location
    ]

  , testGroup "Error Combination Properties"
    [ fastProperty "combineErrors includes original errors" prop_combineErrors_includes_original
    , fastProperty "combineErrors includes related errors" prop_combineErrors_includes_related
    ]

  , testGroup "Combined Error Properties"
    [ fastProperty "combinedErrorSeverity extracts severity correctly" prop_combinedErrorSeverity_correct
    , fastProperty "filterCombinedErrorsBySeverity filters correctly" prop_filterCombinedErrorsBySeverity_correct
    ]

  , testGroup "Timestamp Properties"
    [ fastProperty "errorAtWithTimestamp sets timestamp" prop_errorAtWithTimestamp_sets_timestamp
    , fastProperty "withTimestamp overrides existing timestamp" prop_withTimestamp_overrides
    ]

  , testGroup "Error Context Properties"
    [ fastProperty "emptyContext has all fields as Nothing/empty" prop_emptyContext_values
    ]
  ]