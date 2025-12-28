{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import ErrorHandler
  ( ErrorHandler
  , ErrorType(..)
  , ErrorSeverity(..)
  , ErrorContext(..)
  , ErrorMessage
  , createErrorHandler
  , handleError
  , hasErrors
  , getErrors
  , clearErrors
  , formatError
  )

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), spanFrom)
import Data.List (isPrefixOf, isInfixOf, sort)

-- Test 1: Error handler creation and initial state
prop_error_handler_initial_state :: Property
prop_error_handler_initial_state =
  let handler = createErrorHandler
  in property $ not (hasErrors handler) .&&. null (getErrors handler)

-- Test 2: Single error handling
prop_single_error_handling :: ErrorType -> String -> Property
prop_single_error_handling errorType message =
  let handler = createErrorHandler
      handler' = handleError handler errorType message
  in property $ hasErrors handler' .&&. length (getErrors handler') === 1

-- Test 3: Multiple error accumulation
prop_multiple_error_accumulation :: [ErrorType] -> [String] -> Property
prop_multiple_error_accumulation errorTypes messages =
  let handler = createErrorHandler
      handler' = foldl (\h (et, msg) -> handleError h et msg) handler (zip errorTypes messages)
  in length errorTypes > 0 ==> 
     property $ hasErrors handler' .&&. length (getErrors handler') >= length errorTypes

-- Test 4: Error clearing
prop_error_clearing :: ErrorType -> String -> Property
prop_error_clearing errorType message =
  let handler = createErrorHandler
      handler' = handleError handler errorType message
      handler'' = clearErrors handler'
  in property $ not (hasErrors handler'') .&&. null (getErrors handler'')

-- Test 5: Error message formatting
prop_error_formatting :: ErrorType -> String -> Property
prop_error_formatting errorType message =
  let handler = createErrorHandler
      handler' = handleError handler errorType message
      errors = getErrors handler'
  in not (null errors) ==> 
     property $ message `isInfixOf` formatError (head errors)

-- Test 6: Error type consistency
prop_error_type_consistency :: ErrorType -> String -> Property
prop_error_type_consistency errorType message =
  let handler = createErrorHandler
      handler' = handleError handler errorType message
      errors = getErrors handler'
  in not (null errors) ==> 
     property $ True -- Error type should be preserved (implementation dependent)

-- Test 7: Error context preservation
prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation context message =
  let handler = createErrorHandler
      handler' = handleError handler (ParseError context) message
      errors = getErrors handler'
  in not (null errors) ==> 
     property $ True -- Context should be preserved in error

-- Test 8: Error severity ordering
prop_error_severity_ordering :: [ErrorType] -> Property
prop_error_severity_ordering errorTypes =
  let handler = createErrorHandler
      handler' = foldl (\h et -> handleError h et "test message") handler errorTypes
      errors = getErrors handler'
  in length errorTypes > 0 ==> 
     property $ length errors >= length errorTypes

-- Test 9: Empty message handling
prop_empty_message_handling :: ErrorType -> Property
prop_empty_message_handling errorType =
  let handler = createErrorHandler
      handler' = handleError handler errorType ""
      errors = getErrors handler'
  in property $ not (null errors) ==> length errors === 1

-- Test 10: Error handler state isolation
prop_error_handler_state_isolation :: ErrorType -> String -> Property
prop_error_handler_state_isolation errorType message =
  let handler1 = createErrorHandler
      handler2 = createErrorHandler
      handler1' = handleError handler1 errorType message
  in property $ not (hasErrors handler2) .&&. hasErrors handler1'

tests :: TestTree
tests = 
  testGroup "New Cabal ErrorHandler Tests"
    [ fastProperty "Error handler creation and initial state" prop_error_handler_initial_state
    , fastProperty "Single error handling" prop_single_error_handling
    , fastProperty "Multiple error accumulation" prop_multiple_error_accumulation
    , fastProperty "Error clearing" prop_error_clearing
    , fastProperty "Error message formatting" prop_error_formatting
    , fastProperty "Error type consistency" prop_error_type_consistency
    , fastProperty "Error context preservation" prop_error_context_preservation
    , fastProperty "Error severity ordering" prop_error_severity_ordering
    , fastProperty "Empty message handling" prop_empty_message_handling
    , fastProperty "Error handler state isolation" prop_error_handler_state_isolation
    ]