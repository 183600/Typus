{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, suchThat, listOf1, elements
  , frequency, oneof, sized, resize, Positive(..), NonEmptyList(..)
  , choose, getPositive, vectorOf
  )

import Compiler.Errors.Core
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..)
  , ErrorContext(..), ErrorRecovery(..), CombinedError(..)
  , emptyContext, errorAt, warningAt, infoAt, fatalError
  , errorWithCategory, warningWithCategory, infoWithCategory
  , withLocation, withContext, withSuggestions, withRelatedErrors
  , withTimestamp, withUTCTimestamp
  , errorWithSuggestions, wrapError, combineErrors
  , hasCategory, filterByCategory, filterBySeverity, getErrorStatistics
  , formatError, formatErrorWithLocation, formatErrors, formatErrorsWithLocation
  , canRecoverFrom, shouldContinueAfter
  , severityPriority, isAtLeast, compareSeverity
  , _atLocation, _atFileLocation, _atRange
  , combinedErrorSeverity, filterCombinedErrorsBySeverity
  , generateErrorReport, formatTimestamp
  , getErrorLine, getErrorColumn
  )

import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List (sort, intercalate)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeChecking, Ownership, Parsing, Semantic
    , Runtime, Constraint, Inference, Integration, Unknown
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    maybeFile <- oneof [return Nothing, Just <$> arbitrary]
    line <- getPositive <$> arbitrary
    column <- getPositive <$> arbitrary
    endLine <- oneof [return Nothing, Just . getPositive <$> arbitrary]
    endColumn <- oneof [return Nothing, Just . getPositive <$> arbitrary]
    return $ ErrorLocation maybeFile line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    maybeCode <- oneof [return Nothing, Just <$> arbitrary]
    maybeFunction <- oneof [return Nothing, Just <$> arbitrary]
    maybeVariable <- oneof [return Nothing, Just <$> arbitrary]
    maybeType <- oneof [return Nothing, Just <$> arbitrary]
    additional <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ ErrorContext maybeCode maybeFunction maybeVariable maybeType additional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- oneof [return Nothing, Just <$> arbitrary]
    recHint <- oneof [return Nothing, Just <$> arbitrary]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont recAction recHint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- vectorOf 3 (pack <$> arbitrary)
    relatedErrors <- vectorOf 2 arbitrary
    errorChain <- vectorOf 1 arbitrary
    timestamp <- oneof [return Nothing, Just <$> arbitrary]
    return $ TypeError errId errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: compareSeverity is consistent with priority
prop_compareSeverity_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_compareSeverity_consistent sev1 sev2 =
  let result = compareSeverity sev1 sev2
      p1 = severityPriority sev1
      p2 = severityPriority sev2
  in (result == EQ) === (p1 == p2) .&&.
     (result == LT) === (p1 < p2) .&&.
     (result == GT) === (p1 > p2)

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: _atLocation creates correct location
prop_atLocation_correct :: Positive Int -> Positive Int -> Property
prop_atLocation_correct (Positive line) (Positive col) =
  let loc = _atLocation line col
  in getErrorLine loc === line .&&. getErrorColumn loc === col .&&.
     filePath loc === Nothing .&&.
     endLine loc === Nothing .&&. endColumn loc === Nothing

-- Property: _atFileLocation includes file path
prop_atFileLocation_includes_file :: String -> Positive Int -> Positive Int -> Property
prop_atFileLocation_includes_file file (Positive line) (Positive col) =
  let loc = _atFileLocation file line col
  in filePath loc === Just file .&&.
     getErrorLine loc === line .&&. getErrorColumn loc === col

-- Property: _atRange creates correct range
prop_atRange_correct :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_atRange_correct (Positive startLine) (Positive startCol) (Positive endLine) (Positive endCol) =
  let loc = _atRange startLine startCol endLine endCol
  in getErrorLine loc === startLine .&&.
     getErrorColumn loc === startCol .&&.
     endLine loc === Just endLine .&&.
     endColumn loc === Just endCol

-- ============================================================================
-- Error Construction Properties
-- ============================================================================

-- Property: errorAt "test-id" (L.length newSuggestions) updatedSuggestions === newSuggestions

-- Property: withRelatedErrors prepends related errors
prop_withRelatedErrors_prepends :: TypeError -> [TypeError] -> Property
prop_withRelatedErrors_prepends err newRelated =
  let updated = withRelatedErrors newRelated err
      originalRelated = relatedErrors err
      updatedRelated = relatedErrors updated
  in L.length updatedRelated === L.length newRelated + L.length originalRelated .&&.
     take (L.length newRelated) updatedRelated === newRelated

-- Property: withTimestamp sets timestamp
prop_withTimestamp_sets :: TypeError -> String -> Property
prop_withTimestamp_sets err ts =
  let updated = withTimestamp ts err
  in timestamp updated === Just ts .&&.
     errorId updated === errorId err .&&.
     message updated === message err

-- Property: wrapError adds wrapper message L.and chains error
prop_wrapError_chains :: Text -> TypeError -> Property
prop_wrapError_chains wrapperMsg innerErr =
  let wrapped = wrapError wrapperMsg innerErr
      expectedMsg = wrapperMsg <> ": " <> message innerErr
      expectedChain = innerErr : errorChain innerErr
  in message wrapped === expectedMsg .&&.
     errorChain wrapped === expectedChain

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory is correct for errors
prop_hasCategory_correct :: ErrorCategory -> TypeError -> Property
prop_hasCategory_correct cat err =
  hasCategory cat err === (category err == cat)

-- Property: filterByCategory preserves only matching errors
prop_filterByCategory_preserves :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_preserves cat errors =
  let filtered = filterByCategory cat errors
  in L.all (hasCategory cat) filtered .&&.
     L.length filtered <= L.length errors

-- Property: filterBySeverity preserves only matching errors
prop_filterBySeverity_preserves :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_preserves sev errors =
  let filtered = filterBySeverity sev errors
  in L.all (\e -> severity e == sev) filtered .&&.
     L.length filtered <= L.length errors

-- Property: combineErrors flattens related errors
prop_combineErrors_flattens :: [TypeError] -> Property
prop_combineErrors_flattens errors =
  let combined = combineErrors errors
      originalRelated = concatMap relatedErrors errors
  in L.length combined >= L.length errors .&&.
     L.all (\e -> not (hasCategory Unknown e) || errorId e /= "") combined

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom is consistent with recovery strategy
prop_canRecoverFrom_consistent :: TypeError -> Property
prop_canRecoverFrom_consistent err =
  canRecoverFrom err === canRecover (recovery err)

-- Property: shouldContinueAfter is consistent with recovery strategy
prop_shouldContinueAfter_consistent :: TypeError -> Property
prop_shouldContinueAfter_consistent err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- Property: fatal errors cannot be recovered from
prop_fatal_no_recovery :: String -> Text -> ErrorLocation -> Property
prop_fatal_no_recovery errId msg loc =
  let err = fatalError errId msg loc
  in not (canRecoverFrom err) .&&. not (shouldContinueAfter err)

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: getErrorStatistics total count matches input
prop_getErrorStatistics_total :: [TypeError] -> Property
prop_getErrorStatistics_total errors =
  let stats = getErrorStatistics errors
  in Map.lookup "total" stats === Just (L.length errors)

-- Property: getErrorStatistics severity counts are correct
prop_getErrorStatistics_severity_counts :: [TypeError] -> Property
prop_getErrorStatistics_severity_counts errors =
  let stats = getErrorStatistics errors
      fatalCount = Map.lookup "fatal" stats
      errorCount = Map.lookup "errors" stats
      warningCount = Map.lookup "warnings" stats
      infoCount = Map.lookup "info" stats
      expectedFatal = L.length $ L.filter (\e -> severity e == Fatal) errors
      expectedError = L.length $ L.filter (\e -> severity e == Error) errors
      expectedWarning = L.length $ L.filter (\e -> severity e == Warning) errors
      expectedInfo = L.length $ L.filter (\e -> severity e == Info) errors
  in fatalCount === Just expectedFatal .&&.
     errorCount === Just expectedError .&&.
     warningCount === Just expectedWarning .&&.
     infoCount === Just expectedInfo

-- Property: getErrorStatistics category counts are correct
prop_getErrorStatistics_category_counts :: [TypeError] -> Property
prop_getErrorStatistics_category_counts errors =
  let stats = getErrorStatistics errors
      typeCheckingCount = Map.lookup "typeChecking" stats
      ownershipCount = Map.lookup "ownership" stats
      expectedTypeChecking = L.length $ L.filter (\e -> category e == TypeChecking) errors
      expectedOwnership = L.length $ L.filter (\e -> category e == Ownership) errors
  in typeCheckingCount === Just expectedTypeChecking .&&.
     ownershipCount === Just expectedOwnership

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
  in severityStr `L.isInfixOf` formatted

-- Property: formatError contains category
prop_formatError_contains_category :: TypeError -> Property
prop_formatError_contains_category err =
  let formatted = formatError err
      categoryStr = "[" ++ show (category err) ++ "]"
  in categoryStr `L.isInfixOf` formatted

-- Property: formatErrorWithLocation contains location info
prop_formatErrorWithLocation_contains_location :: TypeError -> Property
prop_formatErrorWithLocation_contains_location err =
  let formatted = formatErrorWithLocation err
      loc = location err
      lineStr = if line loc > 0 then show (line loc) else "?"
      colStr = if column loc > 0 then show (column loc) else "?"
  in lineStr `L.isInfixOf` formatted .&&.
     colStr `L.isInfixOf` formatted

-- Property: formatErrors contains L.all formatted errors
prop_formatErrors_contains_all :: [TypeError] -> Property
prop_formatErrors_contains_all errors =
  let formatted = formatErrors errors
      individualFormatted = map formatError errors
  in L.all (`L.isInfixOf` formatted) individualFormatted

-- ============================================================================
-- Combined Error Properties
-- ============================================================================

-- Property: combinedErrorSeverity extracts correct severity
prop_combinedErrorSeverity_correct :: CombinedError -> Property
prop_combinedErrorSeverity_correct combinedErr =
  let expected = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in combinedErrorSeverity combinedErr === expected

-- Property: filterCombinedErrorsBySeverity preserves order
prop_filterCombinedErrorsBySeverity_preserves_order :: ErrorSeverity -> [CombinedError] -> Property
prop_filterCombinedErrorsBySeverity_preserves_order minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
      originalOrder = L.filter (\e -> isAtLeast minSeverity (combinedErrorSeverity e)) combinedErrors
  in map combinedErrorSeverity filtered === map combinedErrorSeverity originalOrder

-- ============================================================================
-- Error Report Properties
-- ============================================================================

-- Property: generateErrorReport contains statistics header
prop_generateErrorReport_contains_stats :: [TypeError] -> Property
prop_generateErrorReport_contains_stats errors =
  let report = generateErrorReport errors
  in "Statistics:" `L.isInfixOf` report .&&.
     "total:" `L.isInfixOf` report

-- Property: generateErrorReport contains error details
prop_generateErrorReport_contains_details :: [TypeError] -> Property
prop_generateErrorReport_contains_details errors =
  not (null errors) ==>
    let report = generateErrorReport errors
        formattedErrors = formatErrorsWithLocation errors
    in formattedErrors `L.isInfixOf` report

-- ============================================================================
-- Error Context Properties
-- ============================================================================

-- Property: emptyContext has no fields set
prop_emptyContext_empty :: Property
prop_emptyContext_empty =
  contextCode emptyContext === Nothing .&&.
  contextFunction emptyContext === Nothing .&&.
  contextVariable emptyContext === Nothing .&&.
  contextType emptyContext === Nothing .&&.
  L.null (contextAdditional emptyContext)

-- ============================================================================
-- Error Recovery Strategy Properties
-- ============================================================================

-- Property: error recovery has reasonable defaults
prop_error_recovery_defaults :: Property
prop_error_recovery_defaults =
  let recovery = errorRecovery
  in canRecover recovery .&&.
     shouldContinue recovery .&&.
     recoveryCost recovery >= 0 .&&.
     recoveryCost recovery <= 100 .&&.
     recoveryConfidence recovery >= 0.0 .&&.
     recoveryConfidence recovery <= 1.0

-- Property: warning recovery is less costly than error recovery
prop_warning_recovery_less_costly :: Property
prop_warning_recovery_less_costly =
  let errorRec = errorRecovery
      warningRec = warningRecovery
  in recoveryCost warningRec <= recoveryCost errorRec .&&.
     recoveryConfidence warningRec >= recoveryConfidence errorRec

-- Test collection
tests :: TestTree
tests = testGroup "ErrorHandler Core Properties"
  [ testGroup "Error Severity"
    [ fastProperty "severity priority ordering" prop_severity_priority_ordering
    , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
    , fastProperty "compareSeverity is consistent with priority" prop_compareSeverity_consistent
    ]
  , testGroup "Error Location"
    [ fastProperty "_atLocation creates correct location" prop_atLocation_correct
    , fastProperty "_atFileLocation includes file path" prop_atFileLocation_includes_file
    , fastProperty "_atRange creates correct range" prop_atRange_correct
    ]
  , testGroup "Error Construction"
    [ fastProperty "errorAt "test-id" recovery" prop_warning_recovery_less_costly
    ]
  ]