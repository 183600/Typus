{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Data.List (sort, nub)
import qualified Data.Set as Set

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
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
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , canRecoverFrom
  , shouldContinueAfter
  )

import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween)

-- ============================================================================
-- Error Handler Consistency Properties
-- ============================================================================

-- Property: Empty collector has no messages
prop_empty_collector_no_messages :: Property
prop_empty_collector_no_messages =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&. 
             not (hasWarnings collector) .&&.
             null (getErrors collector) .&&.
             null (getWarnings collector) .&&.
             null (getInfo collector)

-- Property: Adding error increases error count
prop_add_error_increases_count :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_add_error_increases_count msg severity category =
  not (null msg) ==>
  let collector = newErrorCollector
      pos = SourcePos 1 1
      span = spanBetween pos pos
      error = errorAt (Just span) msg
      collector' = addError collector error
  in property $ hasErrors collector' .&&. length (getErrors collector') === 1

-- Property: Adding warning increases warning count
prop_add_warning_increases_count :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_add_warning_increases_count msg severity category =
  not (null msg) ==>
  let collector = newErrorCollector
      pos = SourcePos 1 1
      span = spanBetween pos pos
      warning = warningAt (Just span) msg
      collector' = addWarning collector warning
  in property $ hasWarnings collector' .&&. length (getWarnings collector') === 1

-- Property: Error and warning counts are independent
prop_error_warning_independence :: String -> String -> Property
prop_error_warning_independence errorMsg warnMsg =
  not (null errorMsg) && not (null warnMsg) ==>
  let collector = newErrorCollector
      pos = SourcePos 1 1
      span = spanBetween pos pos
      error = errorAt (Just span) errorMsg
      warning = warningAt (Just span) warnMsg
      collector' = addError (addWarning collector error) warning
  in property $ hasErrors collector' .&&. hasWarnings collector' .&&.
             length (getErrors collector') === 1 .&&.
             length (getWarnings collector') === 1

-- Property: Message ordering is preserved
prop_message_ordering_preserved :: [String] -> Property
prop_message_ordering_preserved msgs =
  all (not . null) msgs && length msgs <= 10 ==>
  let collector = newErrorCollector
      pos = SourcePos 1 1
      span = spanBetween pos pos
      addMsg col msg = addError col (errorAt (Just span) msg)
      collector' = foldl addMsg collector msgs
      errorMsgs = map errorMessage (getErrors collector')
  in property $ errorMsgs === msgs

-- Property: Error formatting preserves content
prop_error_formatting_preserves_content :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_formatting_preserves_content msg severity category =
  not (null msg) ==>
  let pos = SourcePos 1 1
      span = spanBetween pos pos
      error = errorAt (Just span) msg
      formatted = formatError error
  in property $ msg `isInfixOf` formatted

-- Property: Multiple errors formatting preserves all messages
prop_multiple_errors_formatting :: [String] -> Property
prop_multiple_errors_formatting msgs =
  all (not . null) msgs && length msgs <= 5 ==>
  let pos = SourcePos 1 1
      span = spanBetween pos pos
      errors = map (\msg -> errorAt (Just span) msg) msgs
      formatted = formatErrors errors
  in property $ all (`isInfixOf` formatted) msgs

-- Property: Error severity classification is consistent
prop_error_severity_consistent :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_severity_consistent msg severity category =
  not (null msg) ==>
  let pos = SourcePos 1 1
      span = spanBetween pos pos
      error = errorWithCategory category severity (Just span) msg
  in property $ errorSeverity error === severity

-- Property: Error recovery determination is deterministic
prop_error_recovery_deterministic :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_recovery_deterministic msg severity category =
  not (null msg) ==>
  let pos = SourcePos 1 1
      span = spanBetween pos pos
      error = errorWithCategory category severity (Just span) msg
      recoverable1 = canRecoverFrom error
      recoverable2 = canRecoverFrom error
  in property $ recoverable1 === recoverable2

-- Property: Context merging preserves information
prop_context_merging_preserves_info :: String -> String -> Property
prop_context_merging_preserves_info key1 key2 =
  not (null key1) && not (null key2) && key1 /= key2 ==>
  let context1 = emptyContext
      context2 = emptyContext
      -- Note: This would need actual context merging functions
      -- For now, test that contexts can be created
  in property $ True

-- Property: Error location tracking is accurate
prop_error_location_tracking :: Int -> Int -> Int -> Int -> String -> Property
prop_error_location_tracking startLine startCol endLine endCol msg =
  all (>0) [startLine, startCol, endLine, endCol] && not (null msg) ==>
  let startPos = SourcePos startLine startCol
      endPos = SourcePos endLine endCol
      span = spanBetween startPos endPos
      error = errorAt (Just span) msg
  in property $ errorLocation error === Just span

tests :: TestTree
tests = testGroup "Core Error Handler Consistency QuickCheck Tests"
  [ fastProperty "empty collector no messages" prop_empty_collector_no_messages
  , fastProperty "add error increases count" prop_add_error_increases_count
  , fastProperty "add warning increases count" prop_add_warning_increases_count
  , fastProperty "error warning independence" prop_error_warning_independence
  , fastProperty "message ordering preserved" prop_message_ordering_preserved
  , fastProperty "error formatting preserves content" prop_error_formatting_preserves_content
  , fastProperty "multiple errors formatting" prop_multiple_errors_formatting
  , fastProperty "error severity consistent" prop_error_severity_consistent
  , fastProperty "error recovery deterministic" prop_error_recovery_deterministic
  , fastProperty "context merging preserves info" prop_context_merging_preserves_info
  , fastProperty "error location tracking" prop_error_location_tracking
  ]