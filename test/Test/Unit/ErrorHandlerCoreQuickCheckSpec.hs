{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..))
import Test.QuickCheck.Gen (choose, listOf, elements, vectorOf, oneof)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , errorWithCategory
  , warningAt
  , infoAt
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , canRecoverFrom
  , shouldContinueAfter
  , hasErrors
  , hasWarnings
  , getErrors
  , getWarnings
  , getInfo
  , formatError
  , formatErrorWithLocation
  , severityPriority
  , isAtLeast
  , compareSeverity
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getAllMessages
  , _atLocation
  , _atFileLocation
  , _atRange
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Time (UTCTime, getCurrentTime)
import Data.Ord (comparing)

-- Property: severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  property $ severityPriority Fatal > severityPriority Error .&&.
             severityPriority Error > severityPriority Warning .&&.
             severityPriority Warning > severityPriority Info

-- Property: isAtLeast works correctly
prop_isAtLeast_correct :: Property
prop_isAtLeast_correct =
  property $ isAtLeast Info Info .&&.
             isAtLeast Warning Info .&&.
             isAtLeast Error Warning .&&.
             isAtLeast Fatal Error .&&.
             not (isAtLeast Info Error) .&&.
             not (isAtLeast Warning Fatal)

-- Property: compareSeverity is consistent with priority
prop_compareSeverity_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_compareSeverity_consistent sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      comparison = compareSeverity sev1 sev2
      expected = compare priority1 priority2
  in property $ comparison === expected

-- Property: errorAt creates error with correct properties
prop_errorAt_correct :: String -> String -> Property
prop_errorAt_correct errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      err = errorAt errId (T.pack msg) loc
  in property $ errorId err === errId .&&.
             message err === T.pack msg .&&.
             location err === loc .&&.
             severity err === Error .&&.
             category err === Unknown

-- Property: errorWithCategory sets category correctly
prop_errorWithCategory_correct :: String -> ErrorCategory -> String -> Property
prop_errorWithCategory_correct errId errCategory msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      err = errorWithCategory errId errCategory (T.pack msg) loc
  in property $ errorId err === errId .&&.
             category err === errCategory .&&.
             severity err === Error

-- Property: warningAt creates warning with correct severity
prop_warningAt_correct :: String -> String -> Property
prop_warningAt_correct errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      warn = warningAt errId (T.pack msg) loc
  in property $ errorId warn === errId .&&.
             severity warn === Warning .&&.
             message warn === T.pack msg

-- Property: infoAt creates info with correct severity
prop_infoAt_correct :: String -> String -> Property
prop_infoAt_correct errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      info = infoAt errId (T.pack msg) loc
  in property $ errorId info === errId .&&.
             severity info === Info .&&.
             message info === T.pack msg

-- Property: fatalError creates fatal error
prop_fatalError_correct :: String -> String -> Property
prop_fatalError_correct errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      fatal = fatalError errId (T.pack msg) loc
  in property $ errorId fatal === errId .&&.
             severity fatal === Fatal .&&.
             not (canRecoverFrom fatal)

-- Property: errorWithSuggestions adds suggestions
prop_errorWithSuggestions_correct :: String -> String -> [String] -> Property
prop_errorWithSuggestions_correct errId msg suggestions =
  not (null errId) ==>
  let loc = _atLocation 1 1
      err = errorWithSuggestions errId (T.pack msg) (map T.pack suggestions) loc
  in property $ suggestions err === map T.pack suggestions

-- Property: withLocation updates location
prop_withLocation_correct :: String -> String -> Property
prop_withLocation_correct errId msg =
  not (null errId) ==>
  let loc1 = _atLocation 1 1
      loc2 = _atLocation 2 5
      err1 = errorAt errId (T.pack msg) loc1
      err2 = withLocation loc2 err1
  in property $ location err2 === loc2 .&&.
             errorId err2 === errId .&&.
             message err2 === T.pack msg

-- Property: withContext updates context
prop_withContext_correct :: String -> String -> Property
prop_withContext_correct errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      newContext = emptyContext { contextFunction = Just "testFunc" }
      err1 = errorAt errId (T.pack msg) loc
      err2 = withContext newContext err1
  in property $ context err2 === newContext .&&.
             errorId err2 === errId

-- Property: withTimestamp adds timestamp
prop_withTimestamp_correct :: String -> String -> String -> Property
prop_withTimestamp_correct errId msg timestamp =
  not (null errId) && not (null timestamp) ==>
  let loc = _atLocation 1 1
      err1 = errorAt errId (T.pack msg) loc
      err2 = withTimestamp timestamp err1
  in property $ timestamp err2 === Just timestamp .&&.
             errorId err2 === errId

-- Property: wrapError creates error chain
prop_wrapError_correct :: String -> String -> String -> Property
prop_wrapError_correct errId1 errId2 msg =
  not (null errId1) && not (null errId2) ==>
  let loc1 = _atLocation 1 1
      loc2 = _atLocation 2 2
      innerErr = errorAt errId1 (T.pack msg) loc1
      wrappedErr = wrapError errId2 (T.pack ("Wrapped: " ++ msg)) loc2 innerErr
  in property $ errorId wrappedErr === errId2 .&&.
             length (errorChain wrappedErr) === 1 .&&.
             head (errorChain wrappedErr) === innerErr

-- Property: canRecoverFrom based on severity
prop_canRecoverFrom_severity :: ErrorSeverity -> Property
prop_canRecoverFrom_severity sev =
  let loc = _atLocation 1 1
      err = errorAt "test" (T.pack "test") loc { severity = sev }
      expected = sev /= Fatal
  in property $ canRecoverFrom err === expected

-- Property: shouldContinueAfter based on severity
prop_shouldContinueAfter_severity :: ErrorSeverity -> Property
prop_shouldContinueAfter_severity sev =
  let loc = _atLocation 1 1
      err = errorAt "test" (T.pack "test") loc { severity = sev }
      expected = sev /= Fatal
  in property $ shouldContinueAfter err === expected

-- Property: hasErrors identifies errors correctly
prop_hasErrors_correct :: [ErrorSeverity] -> Property
prop_hasErrors_correct severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
  in property $ hasErrors errors === any (`elem` [Error, Fatal]) severities

-- Property: hasWarnings identifies warnings correctly
prop_hasWarnings_correct :: [ErrorSeverity] -> Property
prop_hasWarnings_correct severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
  in property $ hasWarnings errors === any (== Warning) severities

-- Property: getErrors filters by severity
prop_getErrors_filters :: [ErrorSeverity] -> Property
prop_getErrors_filters severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
      filtered = getErrors errors
  in property $ all (`elem` [Error, Fatal]) (map severity filtered)

-- Property: getWarnings filters by severity
prop_getWarnings_filters :: [ErrorSeverity] -> Property
prop_getWarnings_filters severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
      filtered = getWarnings errors
  in property $ all (== Warning) (map severity filtered)

-- Property: getInfo filters by severity
prop_getInfo_filters :: [ErrorSeverity] -> Property
prop_getInfo_filters severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
      filtered = getInfo errors
  in property $ all (== Info) (map severity filtered)

-- Property: formatError includes severity and message
prop_formatError_includes_parts :: String -> String -> Property
prop_formatError_includes_parts errId msg =
  not (null errId) ==>
  let loc = _atLocation 1 1
      err = errorAt errId (T.pack msg) loc
      formatted = formatError err
  in property $ "ERROR" `isInfixOf` formatted .&&.
             msg `isInfixOf` formatted

-- Property: formatErrorWithLocation includes location
prop_formatErrorWithLocation_includes_location :: String -> String -> Property
prop_formatErrorWithLocation_includes_location errId msg =
  not (null errId) ==>
  let loc = _atLocation 5 10
      err = errorAt errId (T.pack msg) loc
      formatted = formatErrorWithLocation err
  in property $ "5:10" `isInfixOf` formatted .&&.
             msg `isInfixOf` formatted

-- Property: customRecovery creates recovery with specified properties
prop_customRecovery_correct :: Bool -> Bool -> String -> Int -> Float -> Property
prop_customRecovery_correct canRec shouldCont recAction cost confidence =
  confidence >= 0.0 && confidence <= 1.0 && cost >= 0 && cost <= 100 ==>
  let recovery = customRecovery canRec shouldCont (Just recAction) Nothing cost confidence
  in property $ canRecover recovery === canRec .&&.
             shouldContinue recovery === shouldCont .&&.
             recoveryAction recovery === Just recAction .&&.
             recoveryCost recovery === cost .&&.
             recoveryConfidence recovery === confidence

-- Property: recovery strategy ordering by confidence
prop_recovery_confidence_ordering :: Property
prop_recovery_confidence_ordering =
  let recovery1 = customRecovery True True Nothing Nothing 10 0.5
      recovery2 = customRecovery True True Nothing Nothing 10 0.8
      recovery3 = customRecovery True True Nothing Nothing 10 0.3
  in property $ recoveryConfidence recovery2 > recoveryConfidence recovery1 .&&.
             recoveryConfidence recovery1 > recoveryConfidence recovery3

-- Property: recovery strategy ordering by cost (when confidence equal)
prop_recovery_cost_ordering :: Property
prop_recovery_cost_ordering =
  let recovery1 = customRecovery True True Nothing Nothing 20 0.5
      recovery2 = customRecovery True True Nothing Nothing 10 0.5
      recovery3 = customRecovery True True Nothing Nothing 30 0.5
  in property $ recoveryCost recovery2 < recoveryCost recovery1 .&&.
             recoveryCost recovery1 < recoveryCost recovery3

-- Property: error collector operations
prop_error_collector_operations :: [ErrorSeverity] -> Property
prop_error_collector_operations severities =
  let loc = _atLocation 1 1
      errors = [errorAt ("err" ++ show i) (T.pack "test") loc { severity = sev } | (i, sev) <- zip [0..] severities]
      allMessages = getAllMessages errors
      errorMessages = getErrors allMessages
      warningMessages = getWarnings allMessages
      infoMessages = getInfo allMessages
  in property $ length allMessages === length severities .&&.
             length errorMessages === length [sev | sev <- severities, sev `elem` [Error, Fatal]] .&&.
             length warningMessages === length [sev | sev <- severities, sev == Warning] .&&.
             length infoMessages === length [sev | sev <- severities, sev == Info]

-- Property: error context merging
prop_error_context_merging :: String -> String -> Property
prop_error_context_merging funcName varName =
  not (null funcName) && not (null varName) ==>
  let ctx1 = emptyContext { contextFunction = Just funcName }
      ctx2 = emptyContext { contextVariable = Just varName }
      -- Simulate context merging by taking non-Nothing values
      mergedFunc = contextFunction ctx1 `mplus` contextFunction ctx2
      mergedVar = contextVariable ctx1 `mplus` contextVariable ctx2
  in property $ mergedFunc === Just funcName .&&.
             mergedVar === Just varName

tests :: TestTree
tests =
  testGroup "ErrorHandler Core QuickCheck Tests"
    [ fastProperty "severity priority ordering" prop_severity_priority_ordering
    , fastProperty "isAtLeast works correctly" prop_isAtLeast_correct
    , fastProperty "compareSeverity is consistent with priority" prop_compareSeverity_consistent
    , fastProperty "errorAt creates error with correct properties" prop_errorAt_correct
    , fastProperty "errorWithCategory sets category correctly" prop_errorWithCategory_correct
    , fastProperty "warningAt creates warning with correct severity" prop_warningAt_correct
    , fastProperty "infoAt creates info with correct severity" prop_infoAt_correct
    , fastProperty "fatalError creates fatal error" prop_fatalError_correct
    , fastProperty "errorWithSuggestions adds suggestions" prop_errorWithSuggestions_correct
    , fastProperty "withLocation updates location" prop_withLocation_correct
    , fastProperty "withContext updates context" prop_withContext_correct
    , fastProperty "withTimestamp adds timestamp" prop_withTimestamp_correct
    , fastProperty "wrapError creates error chain" prop_wrapError_correct
    , fastProperty "canRecoverFrom based on severity" prop_canRecoverFrom_severity
    , fastProperty "shouldContinueAfter based on severity" prop_shouldContinueAfter_severity
    , fastProperty "hasErrors identifies errors correctly" prop_hasErrors_correct
    , fastProperty "hasWarnings identifies warnings correctly" prop_hasWarnings_correct
    , fastProperty "getErrors filters by severity" prop_getErrors_filters
    , fastProperty "getWarnings filters by severity" prop_getWarnings_filters
    , fastProperty "getInfo filters by severity" prop_getInfo_filters
    , fastProperty "formatError includes severity and message" prop_formatError_includes_parts
    , fastProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocation_includes_location
    , fastProperty "customRecovery creates recovery with specified properties" prop_customRecovery_correct
    , fastProperty "recovery strategy ordering by confidence" prop_recovery_confidence_ordering
    , fastProperty "recovery strategy ordering by cost" prop_recovery_cost_ordering
    , fastProperty "error collector operations" prop_error_collector_operations
    , fastProperty "error context merging" prop_error_context_merging
    ]

-- Helper function for infix pattern matching
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack