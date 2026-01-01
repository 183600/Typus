{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , severityPriority
  , isAtLeast
  , canRecoverFrom
  , shouldContinueAfter
  , getErrorColumn
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Additional QuickCheck Tests for ErrorHandler Module
-- ============================================================================

-- Property: Severity ordering consistency
prop_severity_ordering_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering_consistent sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      expectedOrdering = compare priority1 priority2
      actualOrdering = compare sev1 sev2
  in property $ (expectedOrdering == EQ) ==> (actualOrdering == EQ)

-- Property: isAtLeast transitivity
prop_isAt_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAt_least_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 .&&. isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: isAtLeast reflexivity
prop_isAt_least_reflexive :: ErrorSeverity -> Property
prop_isAt_least_reflexive sev = isAtLeast sev sev

-- Property: isAtLeast antisymmetry for distinct severities
prop_isAt_least_antisymmetric :: ErrorSeverity -> ErrorSeverity -> Property
prop_isAt_least_antisymmetric sev1 sev2 =
  sev1 /= sev2 ==> (isAtLeast sev1 sev2 .&&. isAtLeast sev2 sev1) === False

-- Property: filterBySeverity preserves ordering
prop_filterBySeverity_preserves_ordering :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_preserves_ordering minSeverity errors =
  let filtered = filterBySeverity minSeverity errors
      severities = map severity filtered
      sortedSeverities = sort severities
  in property $ severities === sortedSeverities

-- Property: filterByCategory preserves category
prop_filterByCategory_preserves_category :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_preserves_category cat errors =
  let filtered = filterByCategory cat errors
      categories = map category filtered
  in property $ L.all (== cat) categories

-- Property: getErrors only returns Error L.or Fatal severities
prop_getErrors_only_error_fatal :: [TypeError] -> Property
prop_getErrors_only_error_fatal errors =
  let errorList = getErrors errors
      severities = map severity errorList
  in property $ L.all (\sev -> sev == Error || sev == Fatal) severities

-- Property: getWarnings only returns Warning severity
prop_getWarnings_only_warning :: [TypeError] -> Property
prop_getWarnings_only_warning errors =
  let warningList = getWarnings errors
      severities = map severity warningList
  in property $ L.all (== Warning) severities

-- Property: getInfo only returns Info severity
prop_getInfo_only_info :: [TypeError] -> Property
prop_getInfo_only_info errors =
  let infoList = getInfo errors
      severities = map severity infoList
  in property $ L.all (== Info) severities

-- Property: hasErrors is consistent with getErrors
prop_hasErrors_consistent :: [TypeError] -> Property
prop_hasErrors_consistent errors =
  let hasErr = hasErrors errors
      errorList = getErrors errors
  in property $ hasErr === (not (null errorList))

-- Property: hasWarnings is consistent with getWarnings
prop_hasWarnings_consistent :: [TypeError] -> Property
prop_hasWarnings_consistent errors =
  let hasWarn = hasWarnings errors
      warningList = getWarnings errors
  in property $ hasWarn === (not (null warningList))

-- Property: errorAt "test-id" (T.pack msg)
  in property $ location err === loc .&&. severity err === Error

-- Property: warningAt "test-id" (T.pack msg)
  in property $ location err === loc .&&. severity err === Warning

-- Property: infoAt "test-id" (T.pack msg)
  in property $ location err === loc .&&. severity err === Info

-- Property: fatalError has Fatal severity
prop_fatalError_severity :: String -> Property
prop_fatalError_severity msg =
  let err = fatalError (T.pack msg)
  in property $ severity err === Fatal

-- Property: errorWithCategory sets correct category
prop_errorWithCategory_category :: ErrorCategory -> String -> Property
prop_errorWithCategory_category cat msg =
  let err = errorWithCategory cat (T.pack msg)
  in property $ category err === cat .&&. severity err === Error

-- Property: warningWithCategory sets correct category
prop_warningWithCategory_category :: ErrorCategory -> String -> Property
prop_warningWithCategory_category cat msg =
  let err = warningWithCategory cat (T.pack msg)
  in property $ category err === cat .&&. severity err === Warning

-- Property: infoWithCategory sets correct category
prop_infoWithCategory_category :: ErrorCategory -> String -> Property
prop_infoWithCategory_category cat msg =
  let err = infoWithCategory cat (T.pack msg)
  in property $ category err === cat .&&. severity err === Info

-- Property: withLocation updates location correctly
prop_withLocation_updates_location :: Int -> Int -> String -> Property
prop_withLocation_updates_location line col msg =
  line > 0 && col > 0 ==> 
  let baseErr = errorWithCategory TypeChecking (T.pack msg)
      newLoc = ErrorLocation Nothing line col Nothing Nothing
      updatedErr = withLocation newLoc baseErr
  in property $ location updatedErr === newLoc .&&. 
     category updatedErr === category baseErr .&&.
     severity updatedErr === severity baseErr

-- Property: withContext updates context correctly
prop_withContext_updates_context :: String -> String -> String -> String -> Property
prop_withContext_updates_context code func var typ =
  let baseErr = errorWithCategory TypeChecking (T.pack "test error")
      newContext = ErrorContext (Just code) (Just func) (Just var) (Just typ) []
      updatedErr = withContext newContext baseErr
  in property $ context updatedErr === newContext .&&.
     category updatedErr === category baseErr .&&.
     severity updatedErr === severity baseErr

-- Property: withSuggestions adds suggestions
prop_withSuggestions_adds_suggestions :: String -> [String] -> Property
prop_withSuggestions_adds_suggestions baseMsg suggs =
  let baseErr = errorWithCategory TypeChecking (T.pack baseMsg)
      textSuggs = map T.pack suggs
      updatedErr = withSuggestions textSuggs baseErr
  in property $ suggestions updatedErr === textSuggs .&&.
     category updatedErr === category baseErr .&&.
     severity updatedErr === severity baseErr

-- Property: withRelatedErrors adds related errors
prop_withRelatedErrors_adds_related :: String -> [String] -> Property
prop_withRelatedErrors_adds_related baseMsg relatedMsgs =
  let baseErr = errorWithCategory TypeChecking (T.pack baseMsg)
      relatedErrs = L.map (\msg -> errorWithCategory Semantic (T.pack msg)) relatedMsgs
      updatedErr = withRelatedErrors relatedErrs baseErr
  in property $ relatedErrors updatedErr === relatedErrs .&&.
     category updatedErr === category baseErr .&&.
     severity updatedErr === severity baseErr

-- Property: formatError contains expected elements
prop_formatError_contains_elements :: ErrorSeverity -> ErrorCategory -> String -> Property
prop_formatError_contains_elements sev cat msg =
  let err = TypeError "test-id" sev cat (T.pack msg) 
                      (ErrorLocation Nothing 1 1 Nothing Nothing) 
                      emptyContext undefined undefined [] [] Nothing
      formatted = formatError err
      sevStr = case sev of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ sevStr `L.isInfixOf` formatted .&&.
     show cat `L.isInfixOf` formatted .&&.
     msg `L.isInfixOf` formatted

-- Property: combinedErrorSeverity matches input severity
prop_combinedErrorSeverity_matches_input :: ErrorSeverity -> String -> Property
prop_combinedErrorSeverity_matches_input sev msg =
  let combined = IntegrationError msg sev
      extracted = combinedErrorSeverity combined
  in property $ extracted === sev

-- Property: filterCombinedErrorsBySeverity works correctly
prop_filterCombinedErrorsBySeverity_correct :: ErrorSeverity -> [ErrorSeverity] -> String -> Property
prop_filterCombinedErrorsBySeverity_correct minSeverity severities msg =
  let combined = L.map (\sev -> IntegrationError (msg ++ show sev) sev) severities
      filtered = filterCombinedErrorsBySeverity minSeverity combined
      expected = L.filter (\sev -> isAtLeast minSeverity sev) severities
      actual = map combinedErrorSeverity filtered
  in property $ sort actual === sort expected

-- Property: combineErrors preserves error information
prop_combineErrors_preserves_info :: [String] -> Property
prop_combineErrors_preserves_info msgs =
  not (null msgs) ==> 
  let baseErr = errorWithCategory TypeChecking (T.pack "base error")
      relatedErrs = L.map (\msg -> errorWithCategory Semantic (T.pack msg)) msgs
      combined = combineErrors baseErr relatedErrs
  in property $ relatedErrors combined === relatedErrs .&&.
     category combined === category baseErr .&&.
     severity combined === severity baseErr

-- Property: canRecoverFrom is consistent with severity
prop_canRecoverFrom_consistent :: ErrorSeverity -> Property
prop_canRecoverFrom_consistent sev =
  let err = errorWithCategory TypeChecking (T.pack "test")
      errWithSev = err { severity = sev }
  in property $ canRecoverFrom errWithSev === (sev /= Fatal)

-- Property: shouldContinueAfter is consistent with severity
prop_shouldContinueAfter_consistent :: ErrorSeverity -> Property
prop_shouldContinueAfter_consistent sev =
  let err = errorWithCategory TypeChecking (T.pack "test")
      errWithSev = err { severity = sev }
  in property $ shouldContinueAfter errWithSev === (sev /= Fatal)

-- Property: getErrorLine returns correct line
prop_getErrorLine_correct :: Int -> Property
prop_getErrorLine_correct line =
  line > 0 ==> 
  let loc = ErrorLocation Nothing line 1 Nothing Nothing
  in property $ getErrorLine loc === line

-- Property: getErrorColumn returns correct column
prop_getErrorColumn_correct :: Int -> Property
prop_getErrorColumn_correct col =
  col > 0 ==> 
  let loc = ErrorLocation Nothing 1 col Nothing Nothing
  in property $ getErrorColumn loc === col

-- Property: Error recovery strategies are consistent
prop_error_recovery_consistent :: ErrorSeverity -> Property
prop_error_recovery_consistent sev =
  let err = errorWithCategory TypeChecking (T.pack "test")
      errWithSev = err { severity = sev }
      recovery = recovery errWithSev
  in property $ canRecover recovery === (sev /= Fatal) .&&.
     shouldContinue recovery === (sev /= Fatal)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional ErrorHandler QuickCheck Tests"
  [ fastProperty "severity ordering consistency" prop_severity_ordering_consistent
  , fastProperty "isAtLeast transitivity" prop_isAt_least_transitive
  , fastProperty "isAtLeast reflexivity" prop_isAt_least_reflexive
  , fastProperty "isAtLeast antisymmetry for distinct severities" prop_isAt_least_antisymmetric
  , fastProperty "filterBySeverity preserves ordering" prop_filterBySeverity_preserves_ordering
  , fastProperty "filterByCategory preserves category" prop_filterByCategory_preserves_category
  , fastProperty "getErrors only returns Error L.or Fatal severities" prop_getErrors_only_error_fatal
  , fastProperty "getWarnings only returns Warning severity" prop_getWarnings_only_warning
  , fastProperty "getInfo only returns Info severity" prop_getInfo_only_info
  , fastProperty "hasErrors is consistent with getErrors" prop_hasErrors_consistent
  , fastProperty "hasWarnings is consistent with getWarnings" prop_hasWarnings_consistent
  , fastProperty "errorAt "test-id" consistent" prop_error_recovery_consistent
  ]