{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , hasErrors
  , hasWarnings
  , canRecoverFrom
  , shouldContinueAfter
  , formatError
  , infoAt
  )

import SourceLocation (SourcePos(..), startPos)

import Data.List (sort)

-- Property: New error collector should be empty
prop_new_collector_is_empty :: Property
prop_new_collector_is_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: Adding error makes hasErrors return True
prop_add_error_makes_has_errors :: String -> Property
prop_add_error_makes_has_errors msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithError = addError startPos msg collector
  in property $ hasErrors collectorWithError

-- Property: Adding warning makes hasWarnings return True
prop_add_warning_makes_has_warnings :: String -> Property
prop_add_warning_makes_has_warnings msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithWarning = addWarning startPos msg collector
  in property $ hasWarnings collectorWithWarning

-- Property: Error count should increase when adding errors
prop_error_count_increases :: String -> String -> Property
prop_error_count_increases msg1 msg2 =
  not (null msg1) .&&. not (null msg2) ==>
  let collector = newErrorCollector
      collector1 = addError startPos msg1 collector
      collector2 = addError startPos msg2 collector1
      errors1 = getErrors collector1
      errors2 = getErrors collector2
  in property $ L.length errors2 === L.length errors1 + 1

-- Property: Warning count should increase when adding warnings
prop_warning_count_increases :: String -> String -> Property
prop_warning_count_increases msg1 msg2 =
  not (null msg1) .&&. not (null msg2) ==>
  let collector = newErrorCollector
      collector1 = addWarning startPos msg1 collector
      collector2 = addWarning startPos msg2 collector1
      warnings1 = getWarnings collector1
      warnings2 = getWarnings collector2
  in property $ L.length warnings2 === L.length warnings1 + 1

-- Property: Info messages don't affect error/warning status
prop_info_doesnt_affect_status :: String -> Property
prop_info_doesnt_affect_status msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithInfo = addInfo startPos msg collector
  in property $ not (hasErrors collectorWithInfo) .&&. not (hasWarnings collectorWithInfo)

-- Property: Error formatting should include the message
prop_error_formatting_includes_message :: String -> Property
prop_error_formatting_includes_message msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithError = addError startPos msg collector
      errors = getErrors collectorWithError
      formatted = case errors of
        (e:_) -> formatError e
        [] -> ""
  in property $ msg `L.isInfixOf` formatted

-- Property: Recovery should be possible for non-critical errors
prop_recovery_possible_for_warnings :: String -> Property
prop_recovery_possible_for_warnings msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithWarning = addWarning startPos msg collector
      warnings = getWarnings collectorWithWarning
  in case warnings of
    (w:_) -> property $ canRecoverFrom w
    [] -> property $ True

-- Property: Continuation should be possible after warnings
prop_continuation_possible_after_warnings :: String -> Property
prop_continuation_possible_after_warnings msg =
  not (null msg) ==>
  let collector = newErrorCollector
      collectorWithWarning = addWarning startPos msg collector
      warnings = getWarnings collectorWithWarning
  in case warnings of
    (w:_) -> property $ shouldContinueAfter w
    [] -> property $ True

-- Property: Error L.and warning counts should be independent
prop_error_warning_independence :: String -> String -> Property
prop_error_warning_independence errorMsg warningMsg =
  not (null errorMsg) .&&. not (null warningMsg) ==>
  let collector = newErrorCollector
      collectorWithError = addError startPos errorMsg collector
      collectorWithBoth = addWarning startPos warningMsg collectorWithError
      errorCount = L.length $ getErrors collectorWithBoth
      warningCount = L.length $ getWarnings collectorWithBoth
  in property $ errorCount === 1 .&&. warningCount === 1

-- Helper functions
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (L.length needle) s : substrings xs

tests :: TestTree
tests = testGroup "Error Handling Recovery QuickCheck Tests"
  [ fastProperty "New collector is empty" prop_new_collector_is_empty
  , fastProperty "Adding error makes hasErrors return True" prop_add_error_makes_has_errors
  , fastProperty "Adding warning makes hasWarnings return True" prop_add_warning_makes_has_warnings
  , fastProperty "Error count increases when adding errors" prop_error_count_increases
  , fastProperty "Warning count increases when adding warnings" prop_warning_count_increases
  , fastProperty "Info messages don't affect error/warning status" prop_info_doesnt_affect_status
  , fastProperty "Error formatting includes the message" prop_error_formatting_includes_message
  , fastProperty "Recovery is possible for warnings" prop_recovery_possible_for_warnings
  , fastProperty "Continuation is possible after warnings" prop_continuation_possible_after_warnings
  , fastProperty "Error L.and warning counts are independent" prop_error_warning_independence
  ]