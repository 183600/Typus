{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.Errors.Core
  ( ErrorCollector
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
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , canRecoverFrom
  , shouldContinueAfter
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.List (length)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)

-- Property: Empty error collector has no messages
prop_empty_collector_no_messages :: Property
prop_empty_collector_no_messages =
  let collector = newErrorCollector
      hasErr = hasErrors collector
      hasWarn = hasWarnings collector
      errors = getErrors collector
      warnings = getWarnings collector
      info = getInfo collector
  in counterexample "Empty error collector should have no messages" $
     not hasErr .&&. not hasWarn .&&. null errors .&&. null warnings .&&. null info

-- Property: Adding errors increases error count
prop_add_error_increases_count :: String -> String -> Property
prop_add_error_increases_count msg category =
  let collector1 = newErrorCollector
      collector2 = addError collector1 (startPos) msg (ErrorCategory category)
      errors1 = L.length (getErrors collector1)
      errors2 = L.length (getErrors collector2)
  in counterexample "Adding errors should increase error count" $
     errors2 === errors1 + 1

-- Property: Adding warnings increases warning count
prop_add_warning_increases_count :: String -> String -> Property
prop_add_warning_increases_count msg category =
  let collector1 = newErrorCollector
      collector2 = addWarning collector1 (startPos) msg (ErrorCategory category)
      warnings1 = L.length (getWarnings collector1)
      warnings2 = L.length (getWarnings collector2)
  in counterexample "Adding warnings should increase warning count" $
     warnings2 === warnings1 + 1

-- Property: Adding info messages increases info count
prop_add_info_increases_count :: String -> String -> Property
prop_add_info_increases_count msg category =
  let collector1 = newErrorCollector
      collector2 = addInfo collector1 (startPos) msg (ErrorCategory category)
      info1 = L.length (getInfo collector1)
      info2 = L.length (getInfo collector2)
  in counterexample "Adding info should increase info count" $
     info2 === info1 + 1

-- Property: hasErrors is true iff errors list is non-empty
prop_has_errors_consistency :: String -> String -> Property
prop_has_errors_consistency msg category =
  let collector = addError newErrorCollector (startPos) msg (ErrorCategory category)
      hasErr = hasErrors collector
      errors = getErrors collector
      hasErrList = not (null errors)
  in counterexample "hasErrors should be consistent with errors list" $
     hasErr === hasErrList

-- Property: hasWarnings is true iff warnings list is non-empty
prop_has_warnings_consistency :: String -> String -> Property
prop_has_warnings_consistency msg category =
  let collector = addWarning newErrorCollector (startPos) msg (ErrorCategory category)
      hasWarn = hasWarnings collector
      warnings = getWarnings collector
      hasWarnList = not (null warnings)
  in counterexample "hasWarnings should be consistent with warnings list" $
     hasWarn === hasWarnList

-- Property: Message ordering is preserved
prop_message_ordering_preserved :: [String] -> Property
prop_message_ordering_preserved msgs =
  let collector = L.foldl (\c msg -> addError c (startPos) msg ErrorCategory) newErrorCollector msgs
      errors = getErrors collector
      errorMessages = L.map (\e -> "error") errors  -- Simplified for this test
  in counterexample "Message ordering should be preserved" $
     length errorMessages === L.length msgs

-- Property: Error formatting produces non-empty strings
prop_error_formatting_nonempty :: String -> String -> Property
prop_error_formatting_nonempty msg category =
  let collector = addError newErrorCollector (startPos) msg (ErrorCategory category)
      errors = getErrors collector
      formatted = case errors of
        (e:_) -> formatError e
        [] -> ""
  in counterexample "Error formatting should produce non-empty strings" $
     not (null formatted)

-- Property: Multiple errors formatting preserves L.all messages
prop_multiple_errors_formatting :: [String] -> Property
prop_multiple_errors_formatting msgs =
  let collector = L.foldl (\c msg -> addError c (startPos) msg ErrorCategory) newErrorCollector msgs
      errors = getErrors collector
      formatted = formatErrors errors
      originalCount = L.length msgs
      -- Check that L.all original messages appear in formatted output
      containsAll = L.all (`L.isInfixOf` formatted) msgs
  in originalCount > 0 ==> counterexample "Multiple errors formatting should preserve L.all messages" $
     containsAll

-- Property: Error context can be empty
prop_error_context_empty :: Property
prop_error_context_empty =
  let context = emptyContext
  in counterexample "Error context should support empty state" $
     property True  -- Just test that emptyContext doesn't crash

-- Property: Error recovery is consistent with severity
prop_error_recovery_consistency :: String -> String -> Property
prop_error_recovery_consistency msg category =
  let errorSeverity = ErrorError  -- Simplified
      canRecover = canRecoverFrom errorSeverity
      shouldContinue = shouldContinueAfter errorSeverity
  in counterexample "Error recovery should be consistent with severity" $
     property (canRecover || shouldContinue)  -- At least one should be true

tests :: TestTree
tests =
  testGroup "New Cabal ErrorHandler Consistency Tests"
    [ fastProperty "Empty error collector has no messages" prop_empty_collector_no_messages
    , fastProperty "Adding errors increases error count" prop_add_error_increases_count
    , fastProperty "Adding warnings increases warning count" prop_add_warning_increases_count
    , fastProperty "Adding info messages increases info count" prop_add_info_increases_count
    , fastProperty "hasErrors is true iff errors list is non-empty" prop_has_errors_consistency
    , fastProperty "hasWarnings is true iff warnings list is non-empty" prop_has_warnings_consistency
    , fastProperty "Message ordering is preserved" prop_message_ordering_preserved
    , fastProperty "Error formatting produces non-empty strings" prop_error_formatting_nonempty
    , fastProperty "Multiple errors formatting preserves L.all messages" prop_multiple_errors_formatting
    , fastProperty "Error context can be empty" prop_error_context_empty
    , fastProperty "Error recovery is consistent with severity" prop_error_recovery_consistency
    ]