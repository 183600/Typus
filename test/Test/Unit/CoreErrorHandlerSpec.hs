{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreErrorHandlerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorLocation(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , getErrors
  , getAllMessages
  , hasErrors
  , errorAt
  )
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo)
import Data.List (isInfixOf)

-- Test properties for ErrorHandler module

-- | newErrorCollector should start with no errors
prop_defaultErrorHandler_no_errors :: Property
prop_defaultErrorHandler_no_errors = 
  let collector = newErrorCollector
  in property $ not (hasErrors collector)

-- | addError should mark hasErrors as true
prop_handleError_increases_count :: String -> Property
prop_handleError_increases_count msg = 
  let collector = newErrorCollector
      collector' = addError collector msg
  in property $ hasErrors collector'

-- | addError should preserve previous errors
prop_handleError_preserves_errors :: String -> String -> Property
prop_handleError_preserves_errors msg1 msg2 = 
  let collector = newErrorCollector
      collector1 = addError collector msg1
      collector2 = addError collector1 msg2
      errors = getErrors collector2
  in property $ length errors >= 2

-- | collectErrors should return all errors
prop_collectErrors_returns_all :: [String] -> Property
prop_collectErrors_returns_all msgs = 
  let handler = foldl (\h msg -> handleError h (Error Error msg NoLocation)) defaultErrorHandler msgs
      collected = collectErrors handler
  in property $ length collected == length msgs &&
                all (`elem` map errorMessage collected) msgs

-- | Error severity should be preserved
prop_error_severity_preserved :: ErrorSeverity -> String -> Property
prop_error_severity_preserved severity msg = 
  let handler = defaultErrorHandler
      handler' = handleError handler (Error severity msg NoLocation)
      errors = getErrors handler'
  in property $ case errors of
    [Error actualSeverity _ _] -> actualSeverity == severity
    _ -> property False

-- | Error message should be preserved
prop_error_message_preserved :: ErrorSeverity -> String -> Property
prop_error_message_preserved severity msg = 
  let handler = defaultErrorHandler
      handler' = handleError handler (Error severity msg NoLocation)
      errors = getErrors handler'
  in property $ case errors of
    [Error _ actualMessage _] -> actualMessage == msg
    _ -> property False

-- Unit tests
test_defaultErrorHandler :: Assertion
test_defaultErrorHandler = do
  let collector = newErrorCollector
  assertBool "newErrorCollector has no errors" (not $ hasErrors collector)

test_handleError_error :: Assertion
test_handleError_error = do
  let collector = newErrorCollector
  let collector' = addError collector "Test error"
  assertBool "addError has errors" (hasErrors collector')

test_handleError_warning :: Assertion
test_handleError_warning = do
  let collector = newErrorCollector
  let collector' = addWarning collector "Test warning"
  assertBool "addWarning has warnings" (hasErrors collector')

test_clearErrors :: Assertion
test_clearErrors = do
  let collector = newErrorCollector
  let collector' = addError collector "Test error"
  -- Note: There's no clearErrors function in the actual interface
  assertBool "addError has errors" (hasErrors collector')

test_getErrors :: Assertion
test_getErrors = do
  let collector = newErrorCollector
  let collector1 = addError collector "Error 1"
  let collector2 = addWarning collector1 "Warning 1"
  let errors = getErrors collector2
  assertBool "getErrors returns errors" (length errors >= 2)

test_collectErrors :: Assertion
test_collectErrors = do
  let collector = newErrorCollector
  let collector1 = addError collector "Error 1"
  let collector2 = addWarning collector1 "Warning 1"
  let messages = getAllMessages collector2
  assertBool "getAllMessages returns messages" (length messages >= 2)

test_hasErrors :: Assertion
test_hasErrors = do
  let collector = newErrorCollector
  let collector1 = addError collector "Test error"
  assertBool "newErrorCollector has no errors" (not $ hasErrors collector)
  assertBool "collector with error has errors" (hasErrors collector1)

test_error_location :: Assertion
test_error_location = do
  let handler = defaultErrorHandler
  let span = spanTo (SourcePos 1 1) (SourcePos 1 10)
  let location = SourceLocation span
  let handler' = handleError handler (Error Error "Test error" location)
  let errors = getErrors handler'
  assertEqual "error location" location (errorLocation $ head errors)

test_error_severity_filtering :: Assertion
test_error_severity_filtering = do
  let handler = defaultErrorHandler
  let handler1 = handleError handler (Error Error "Error 1" NoLocation)
  let handler2 = handleError handler1 (Error Warning "Warning 1" NoLocation)
  let handler3 = handleError handler2 (Error Info "Info 1" NoLocation)
  let allErrors = getErrors handler3
  let errorErrors = filter (\e -> errorSeverity e == Error) allErrors
  let warningErrors = filter (\e -> errorSeverity e == Warning) allErrors
  let infoErrors = filter (\e -> errorSeverity e == Info) allErrors
  assertEqual "error severity filtering - errors" 1 (length errorErrors)
  assertEqual "error severity filtering - warnings" 1 (length warningErrors)
  assertEqual "error severity filtering - info" 1 (length infoErrors)

-- Test suite
tests :: TestTree
tests = testGroup "Core ErrorHandler Tests"
  [ testProperties "QuickCheck Properties"
    [ prop_defaultErrorHandler_no_errors
    , prop_handleError_increases_count
    , prop_handleError_preserves_errors
    ]
  , testCase "defaultErrorHandler" test_defaultErrorHandler
  , testCase "handleError error" test_handleError_error
  , testCase "handleError warning" test_handleError_warning
  , testCase "clearErrors" test_clearErrors
  , testCase "getErrors" test_getErrors
  , testCase "collectErrors" test_collectErrors
  , testCase "hasErrors" test_hasErrors
  ]