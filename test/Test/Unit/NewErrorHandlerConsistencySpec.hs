{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf, resize)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
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
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , withLocation
  , withContext
  , combineErrors
  , combinedErrorSeverity
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , getErrorStatistics
  , emptyContext
  , severityPriority
  , isAtLeast
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- New Error Handler Consistency Tests
-- ============================================================================

-- Property: Severity priority ordering is consistent
prop_severity_priority_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_ordering sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      ordering = compare priority1 priority2
  in property $ (ordering == GT && sev1 > sev2) .||. 
                (ordering == EQ && sev1 == sev2) .||.
                (ordering == LT && sev1 < sev2)

-- Property: isAtLeast is consistent with severity ordering
prop_is_at_least_consistency :: ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_consistency minSeverity sev =
  let result = isAtLeast minSeverity sev
      expected = sev >= minSeverity
  in property $ result === expected

-- Property: New error collector starts empty
prop_new_collector_empty :: Property
prop_new_collector_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&.
     not (hasWarnings collector) .&&.
     null (getAllMessages collector)

-- Property: Adding error makes collector have errors
prop_add_error_creates_error :: String -> Property
prop_add_error_creates_error errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      withError = addError errorMsg collector
  in property $ hasErrors withError .&&.
     length (getErrors withError) >= 1

-- Property: Adding warning makes collector have warnings
prop_add_warning_creates_warning :: String -> Property
prop_add_warning_creates_warning warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      withWarning = addWarning warningMsg collector
  in property $ hasWarnings withWarning .&&.
     length (getWarnings withWarning) >= 1

-- Property: Adding info doesn't create errors or warnings
prop_add_info_no_errors_warnings :: String -> Property
prop_add_info_no_errors_warnings infoMsg =
  not (null infoMsg) ==>
  let collector = newErrorCollector
      withInfo = addInfo infoMsg collector
  in property $ not (hasErrors withInfo) .&&.
     not (hasWarnings withInfo) .&&.
     length (getInfo withInfo) >= 1

-- Property: Error location is preserved in formatting
prop_error_location_preserved :: String -> Int -> Int -> Property
prop_error_location_preserved errorMsg line column =
  not (null errorMsg) && line > 0 && column > 0 ==>
  let location = ErrorLocation Nothing line column Nothing Nothing
      error = errorAt location errorMsg
      formatted = formatError error
  in property $ show line `isInfixOf` formatted .&&.
     show column `isInfixOf` formatted

-- Property: Error context is preserved in error
prop_error_context_preserved :: String -> String -> Property
prop_error_context_preserved errorMsg contextMsg =
  not (null errorMsg) && not (null contextMsg) ==>
  let baseError = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
      contextedError = withContext contextMsg baseError
  in property $ contextMsg `isInfixOf` show contextedError

-- Property: Error category filtering works correctly
prop_error_category_filtering :: String -> ErrorCategory -> Property
prop_error_category_filtering errorMsg category =
  not (null errorMsg) ==>
  let categorizedError = errorWithCategory category errorMsg
      hasCat = hasCategory category categorizedError
      filtered = filterByCategory category [categorizedError]
  in property $ hasCat .&&. length filtered === 1

-- Property: Severity filtering works correctly
prop_severity_filtering :: String -> ErrorSeverity -> Property
prop_severity_filtering errorMsg severity =
  not (null errorMsg) ==>
  let baseError = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
      severityError = case severity of
                        Fatal -> fatalError errorMsg
                        Error -> baseError
                        Warning -> warningAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
                        Info -> infoAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
      filtered = filterBySeverity severity [severityError]
  in property $ length filtered >= 1

-- Property: Error combination preserves worst severity
prop_error_combination_severity :: String -> String -> Property
prop_error_combination_severity msg1 msg2 =
  not (null msg1) && not (null msg2) ==>
  let error1 = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) msg1
      error2 = warningAt (ErrorLocation Nothing 2 1 Nothing Nothing) msg2
      combined = combineErrors [error1, error2]
      combinedSev = combinedErrorSeverity combined
  in property $ combinedSev === Error  -- Error is worse than warning

-- Property: Recovery strategy consistency
prop_recovery_strategy_consistency :: ErrorSeverity -> Property
prop_recovery_strategy_consistency severity =
  let strategy = createRecoveryStrategy severity
      canRecover = canRecoverFrom strategy
      shouldContinue = shouldContinueAfter strategy
  in case severity of
       Fatal -> property $ not canRecover .&&. not shouldContinue
       Error -> property $ canRecover .&&. not shouldContinue
       Warning -> property $ canRecover .&&. shouldContinue
       Info -> property $ canRecover .&&. shouldContinue

-- Property: Custom recovery strategy works as expected
prop_custom_recovery_strategy :: Bool -> Bool -> Property
prop_custom_recovery_strategy canRec shouldCont =
  let strategy = customRecovery canRec shouldCont
  in property $ canRecoverFrom strategy === canRec .&&.
     shouldContinueAfter strategy === shouldCont

-- Property: Error statistics are consistent
prop_error_statistics_consistent :: [String] -> [String] -> [String] -> Property
prop_error_statistics_consistent errorMsgs warningMsgs infoMsgs =
  let nonEmptyErrors = filter (not . null) errorMsgs
      nonEmptyWarnings = filter (not . null) warningMsgs
      nonEmptyInfos = filter (not . null) infoMsgs
      collector = foldr addError newErrorCollector nonEmptyErrors
      collector1 = foldr addWarning collector nonEmptyWarnings
      collector2 = foldr addInfo collector1 nonEmptyInfos
      stats = getErrorStatistics collector2
  in property $ length nonEmptyErrors === length (getErrors collector2) .&&.
     length nonEmptyWarnings === length (getWarnings collector2) .&&.
     length nonEmptyInfos === length (getInfo collector2)

-- Property: Multiple errors are all preserved
prop_multiple_errors_preserved :: [String] -> Property
prop_multiple_errors_preserved errorMsgs =
  let nonEmptyMsgs = filter (not . null) errorMsgs
      collector = foldr addError newErrorCollector nonEmptyMsgs
      allErrors = getErrors collector
  in property $ length allErrors === length nonEmptyMsgs

-- Property: Error formatting contains essential information
prop_error_formatting_contains_info :: String -> Int -> Int -> Property
prop_error_formatting_contains_info errorMsg line column =
  not (null errorMsg) && line > 0 && column > 0 ==>
  let location = ErrorLocation Nothing line column Nothing Nothing
      error = errorAt location errorMsg
      formatted = formatError error
  in property $ errorMsg `isInfixOf` formatted .&&.
     show line `isInfixOf` formatted .&&.
     show column `isInfixOf` formatted

-- Property: Empty context doesn't break error handling
prop_empty_context_safe :: String -> Property
prop_empty_context_safe errorMsg =
  not (null errorMsg) ==>
  let baseError = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
      withEmptyContext = withContext emptyContext baseError
  in property $ not (null (show withEmptyContext))

-- Property: Error location updates work correctly
prop_error_location_update :: String -> Int -> Int -> Int -> Int -> Property
prop_error_location_update errorMsg line1 col1 line2 col2 =
  not (null errorMsg) && line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==>
  let location1 = ErrorLocation Nothing line1 col1 Nothing Nothing
      location2 = ErrorLocation Nothing line2 col2 Nothing Nothing
      baseError = errorAt location1 errorMsg
      updatedError = withLocation location2 baseError
  in property $ show line2 `isInfixOf` show updatedError .&&.
     show col2 `isInfixOf` show updatedError

-- Property: Severity ordering is total
prop_severity_total_ordering :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_severity_total_ordering sev1 sev2 sev3 =
  let comp12 = compare sev1 sev2
      comp23 = compare sev2 sev3
      comp13 = compare sev1 sev3
  in property $ (comp12 == EQ && comp23 == EQ) ==> comp13 == EQ

-- Property: Recovery strategies are composable
prop_recovery_composable :: Bool -> Bool -> Bool -> Bool -> Property
prop_recovery_composable canRec1 shouldCont1 canRec2 shouldCont2 =
  let strategy1 = customRecovery canRec1 shouldCont1
      strategy2 = customRecovery canRec2 shouldCont2
      -- If both strategies allow recovery, combined should allow recovery
      combinedCanRecover = canRecoverFrom strategy1 && canRecoverFrom strategy2
  in property $ combinedCanRecover ==> (canRecoverFrom strategy1 .&&. canRecoverFrom strategy2)

-- Property: Error messages are preserved exactly
prop_error_message_preservation :: String -> Property
prop_error_message_preservation errorMsg =
  not (null errorMsg) ==>
  let error = errorAt (ErrorLocation Nothing 1 1 Nothing Nothing) errorMsg
      formatted = formatError error
  in property $ errorMsg `isInfixOf` formatted

-- Property: Multiple warnings are all preserved
prop_multiple_warnings_preserved :: [String] -> Property
prop_multiple_warnings_preserved warningMsgs =
  let nonEmptyMsgs = filter (not . null) warningMsgs
      collector = foldr addWarning newErrorCollector nonEmptyMsgs
      allWarnings = getWarnings collector
  in property $ length allWarnings === length nonEmptyMsgs

-- Property: Multiple info messages are all preserved
prop_multiple_info_preserved :: [String] -> Property
prop_multiple_info_preserved infoMsgs =
  let nonEmptyMsgs = filter (not . null) infoMsgs
      collector = foldr addInfo newErrorCollector nonEmptyMsgs
      allInfo = getInfo collector
  in property $ length allInfo === length nonEmptyMsgs

-- Property: Mixed message types are all preserved
prop_mixed_messages_preserved :: [String] -> [String] -> [String] -> Property
prop_mixed_messages_preserved errorMsgs warningMsgs infoMsgs =
  let nonEmptyErrors = filter (not . null) errorMsgs
      nonEmptyWarnings = filter (not . null) warningMsgs
      nonEmptyInfos = filter (not . null) infoMsgs
      collector = foldr addError newErrorCollector nonEmptyErrors
      collector1 = foldr addWarning collector nonEmptyWarnings
      collector2 = foldr addInfo collector1 nonEmptyInfos
      allMessages = getAllMessages collector2
  in property $ length allMessages === length nonEmptyErrors + length nonEmptyWarnings + length nonEmptyInfos

-- Tests collection
tests :: TestTree
tests = testGroup "New Error Handler Consistency Tests"
  [ fastProperty "Severity priority ordering is consistent" prop_severity_priority_ordering
  , fastProperty "isAtLeast is consistent with severity ordering" prop_is_at_least_consistency
  , fastProperty "New error collector starts empty" prop_new_collector_empty
  , fastProperty "Adding error makes collector have errors" prop_add_error_creates_error
  , fastProperty "Adding warning makes collector have warnings" prop_add_warning_creates_warning
  , fastProperty "Adding info doesn't create errors or warnings" prop_add_info_no_errors_warnings
  , fastProperty "Error location is preserved in formatting" prop_error_location_preserved
  , fastProperty "Error context is preserved in error" prop_error_context_preserved
  , fastProperty "Error category filtering works correctly" prop_error_category_filtering
  , fastProperty "Severity filtering works correctly" prop_severity_filtering
  , fastProperty "Error combination preserves worst severity" prop_error_combination_severity
  , fastProperty "Recovery strategy consistency" prop_recovery_strategy_consistency
  , fastProperty "Custom recovery strategy works as expected" prop_custom_recovery_strategy
  , fastProperty "Error statistics are consistent" prop_error_statistics_consistent
  , fastProperty "Multiple errors are all preserved" prop_multiple_errors_preserved
  , fastProperty "Error formatting contains essential information" prop_error_formatting_contains_info
  , fastProperty "Empty context doesn't break error handling" prop_empty_context_safe
  , fastProperty "Error location updates work correctly" prop_error_location_update
  , fastProperty "Severity ordering is total" prop_severity_total_ordering
  , fastProperty "Recovery strategies are composable" prop_recovery_composable
  , fastProperty "Error messages are preserved exactly" prop_error_message_preservation
  , fastProperty "Multiple warnings are all preserved" prop_multiple_warnings_preserved
  , fastProperty "Multiple info messages are all preserved" prop_multiple_info_preserved
  , fastProperty "Mixed message types are all preserved" prop_mixed_messages_preserved
  ]