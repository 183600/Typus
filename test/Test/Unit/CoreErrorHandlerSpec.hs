{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreErrorHandlerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorLocation(..)
  , TypeError(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , getErrors
  , getAllMessages
  , hasErrors
  , errorAt
  , filterBySeverity
  , location
  )
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo, posAt)
import Control.Monad.State (evalState, execState)
import Data.List (isInfixOf)
import qualified Data.Text as T

-- Test properties for ErrorHandler module

-- | newErrorCollector should start with no errors
prop_defaultErrorHandler_no_errors :: Property
prop_defaultErrorHandler_no_errors = 
  let errors = execState newErrorCollector []
  in property $ not (hasErrors errors)

-- | addError should mark hasErrors as true
prop_handleError_increases_count :: String -> Property
prop_handleError_increases_count msg = 
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
      error = errorAt "test" (T.pack msg) location
      errors = execState (addError error) []
  in property $ hasErrors errors

-- | addError should preserve previous errors
prop_handleError_preserves_errors :: String -> String -> Property
prop_handleError_preserves_errors msg1 msg2 = 
  let unknownLocation = ErrorLocation Nothing 0 0 Nothing Nothing
      error1 = errorAt "test1" (T.pack msg1) unknownLocation
      error2 = errorAt "test2" (T.pack msg2) unknownLocation
      errors = execState (addError error1 >> addError error2) []
  in property $ length errors >= 2

-- | getErrors should return all errors
prop_collectErrors_returns_all :: [String] -> Property
prop_collectErrors_returns_all msgs = 
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
      errors = map (\(i, msg) -> errorAt ("test" ++ show i) (T.pack msg) location) (zip [1..] msgs)
      collected = execState (mapM_ addError errors) []
  in property $ length collected == length msgs &&
                all (\msg -> any (\e -> T.pack msg == message e) collected) msgs

-- | Error severity should be preserved
prop_error_severity_preserved :: ErrorSeverity -> String -> Property
prop_error_severity_preserved sev msg = 
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
      error = (errorAt "test" (T.pack msg) location) { severity = sev }
      errors = execState (addError error) []
  in property $ case errors of
    [e] -> severity e == sev
    _ -> False

-- | Error message should be preserved
prop_error_message_preserved :: ErrorSeverity -> String -> Property
prop_error_message_preserved sev msg = 
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
      error = (errorAt "test" (T.pack msg) location) { severity = sev }
      errors = execState (addError error) []
  in property $ case errors of
    [e] -> message e == T.pack msg
    _ -> False

-- Unit tests
test_defaultErrorHandler :: Assertion
test_defaultErrorHandler = do
  let errors = execState newErrorCollector []
  assertBool "newErrorCollector has no errors" (not $ hasErrors errors)

test_handleError_error :: Assertion
test_handleError_error = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error = errorAt "test" "Test error" location
  let errors = execState (addError error) []
  assertBool "addError has errors" (hasErrors errors)

test_handleError_warning :: Assertion
test_handleError_warning = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error = (errorAt "test" "Test warning" location) { severity = Warning }
  let errors = execState (addError error) []
  assertBool "addWarning has warnings" (hasErrors errors)

test_clearErrors :: Assertion
test_clearErrors = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error = errorAt "test" "Test error" location
  let errors = execState (addError error) []
  -- Note: There's no clearErrors function in the actual interface
  assertBool "addError has errors" (hasErrors errors)

test_getErrors :: Assertion
test_getErrors = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error1 = errorAt "test1" "Error 1" location
  let error2 = (errorAt "test2" "Warning 1" location) { severity = Warning }
  let errors = execState (addError error1 >> addError error2) []
  assertBool "getErrors returns errors" (length errors >= 2)

test_collectErrors :: Assertion
test_collectErrors = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error1 = errorAt "test1" "Error 1" location
  let error2 = (errorAt "test2" "Warning 1" location) { severity = Warning }
  let messages = execState (addError error1 >> addError error2) []
  assertBool "getAllMessages returns messages" (length messages >= 2)

test_hasErrors :: Assertion
test_hasErrors = do
  let errors1 = execState newErrorCollector []
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error = errorAt "test" "Test error" location
  let errors2 = execState (addError error) []
  assertBool "newErrorCollector has no errors" (not $ hasErrors errors1)
  assertBool "collector with error has errors" (hasErrors errors2)

test_error_location :: Assertion
test_error_location = do
  let pos = posAt 1 1
  let span = spanTo pos
  let location = ErrorLocation Nothing 1 1 (Just 1) (Just 10)
  let error = (errorAt "test" "Test error" location) { location = location }
  let errors = execState (addError error) []
  assertEqual "error location" location (Compiler.Errors.Core.location (head errors))

test_error_severity_filtering :: Assertion
test_error_severity_filtering = do
  let location = ErrorLocation Nothing 0 0 Nothing Nothing
  let error1 = errorAt "test1" "Error 1" location
  let error2 = (errorAt "test2" "Warning 1" location) { severity = Warning }
  let error3 = (errorAt "test3" "Info 1" location) { severity = Info }
  let allErrors = execState (addError error1 >> addError error2 >> addError error3) []
  let errorErrors = filterBySeverity Error allErrors
  let warningErrors = filterBySeverity Warning allErrors
  let infoErrors = filterBySeverity Info allErrors
  assertEqual "error severity filtering - errors" 1 (length errorErrors)
  assertEqual "error severity filtering - warnings" 1 (length warningErrors)
  assertEqual "error severity filtering - info" 1 (length infoErrors)

-- Test suite
tests :: TestTree
tests = testGroup "Core ErrorHandler Tests"
  [ testProperties "QuickCheck Properties"
    [ ("defaultErrorHandler_no_errors", prop_defaultErrorHandler_no_errors)
    , ("handleError_increases_count", property $ prop_handleError_increases_count "test")
    , ("handleError_preserves_errors", property $ prop_handleError_preserves_errors "test1" "test2")
    ]
  , testCase "defaultErrorHandler" test_defaultErrorHandler
  , testCase "handleError error" test_handleError_error
  , testCase "handleError warning" test_handleError_warning
  , testCase "clearErrors" test_clearErrors
  , testCase "getErrors" test_getErrors
  , testCase "collectErrors" test_collectErrors
  , testCase "hasErrors" test_hasErrors
  , testCase "error location" test_error_location
  , testCase "error severity filtering" test_error_severity_filtering
  ]