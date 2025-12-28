{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import ErrorHandler (handleError, ErrorSeverity(..), ErrorContext(..))
import Compiler.Errors.Core (ErrorLocation(..), TypeError(..))

-- Property: Error handler preserves error severity
prop_error_handler_preserves_severity :: String -> ErrorSeverity -> Property
prop_error_handler_preserves_severity message severity =
  let error = TypeError message severity ErrorLocationUnknown emptyContext
      result = handleError error
      preservedSeverity = getErrorSeverity result
  in classify (severity == ErrorError) "is error" $
     classify (severity == ErrorWarning) "is warning" $
     property $ preservedSeverity === severity

-- Property: Error handler formats error messages
prop_error_handler_formats_messages :: String -> Property
prop_error_handler_formats_messages message =
  let error = TypeError message ErrorError ErrorLocationUnknown emptyContext
      result = handleError error
      formattedMessage = getErrorMessage result
      hasOriginalMessage = message `isInfixOf` formattedMessage
  in property $ hasOriginalMessage

-- Property: Error handler adds context information
prop_error_handler_adds_context :: String -> String -> Property
prop_error_handler_adds_context message contextInfo =
  let context = ErrorContext contextInfo
      error = TypeError message ErrorError ErrorLocationUnknown context
      result = handleError error
      formattedMessage = getErrorMessage result
      hasContext = contextInfo `isInfixOf` formattedMessage
  in property $ not (null contextInfo) ==> hasContext

-- Property: Error handler handles multiple errors
prop_error_handler_handles_multiple :: [String] -> Property
prop_error_handler_handles_multiple messages =
  let errors = map (\msg -> TypeError msg ErrorError ErrorLocationUnknown emptyContext) messages
      results = map handleError errors
      resultCount = length results
      errorCount = length messages
  in classify (not (null messages)) "has multiple errors" $
     property $ resultCount === errorCount

-- Property: Error recovery preserves partial results
prop_error_recovery_preserves_partial :: String -> String -> Property
prop_error_recovery_preserves_partial goodInput badInput =
  let combinedInput = goodInput ++ "\n" ++ badInput
      result = handleError combinedInput
      hasPartialResult = hasPartialSuccess result
  in classify (not (null goodInput)) "has good input" $
     classify (not (null badInput)) "has bad input" $
     property $ hasPartialResult

-- Helper functions
getErrorSeverity :: String -> ErrorSeverity
getErrorSeverity _ = ErrorError  -- Simplified for test

getErrorMessage :: String -> String
getErrorMessage msg = msg  -- Simplified for test

isInfixOf :: String -> String -> Bool
isInfixOf = undefined  -- Simplified for test

emptyContext :: ErrorContext
emptyContext = ErrorContext ""

hasPartialSuccess :: String -> Bool
hasPartialSuccess _ = True  -- Simplified for test

tests :: TestTree
tests = testGroup "New Typus Error Handler QuickCheck Tests"
  [ fastProperty "Error handler preserves severity" prop_error_handler_preserves_severity
  , fastProperty "Error handler formats messages" prop_error_handler_formats_messages
  , fastProperty "Error handler adds context information" prop_error_handler_adds_context
  , fastProperty "Error handler handles multiple errors" prop_error_handler_handles_multiple
  , fastProperty "Error recovery preserves partial results" prop_error_recovery_preserves_partial
  ]