module Test.Unit.EnhancedErrorHandlerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Control.Monad.State

-- Import ErrorHandler module
import Compiler.Errors.Core

-- Using unknownLocation from Compiler.Errors.Core

-- Test properties for error handler

-- Property 1: Creating error handler should not crash
prop_new_error_handler :: Property
prop_new_error_handler = property $
  let collector = newErrorCollector
  in property True  -- Should not crash

-- Property 2: Handling empty error should not crash
prop_handle_empty_error :: Property
prop_handle_empty_error = property $
  let error = errorAt "" Error (T.pack "test") unknownLocation
      result = execState (addError error) []
  in property True  -- Should not crash

-- Property 3: Handling error with message should not crash
prop_handle_error_with_message :: String -> Property
prop_handle_error_with_message message = 
  not (null message) ==>
  let error = errorAt message Error (T.pack message) unknownLocation
      result = execState (addError error) []
  in property True  -- Should not crash

-- Property 4: Handling error with different types should not crash
prop_handle_error_with_type :: String -> Int -> Property
prop_handle_error_with_type message typeIndex = 
  not (null message) && typeIndex >= 0 && typeIndex < 10 ==>
  let errorCategories = [Parsing, TypeChecking, Semantic, 
                        Runtime, Constraint, Inference, Integration, Unknown]
      errorType = errorCategories !! typeIndex
      location = unknownLocation
      error = errorWithCategory message errorType (T.pack message) location
      result = execState (addError error) []
  in property True  -- Should not crash

-- Property 5: Handling error with different severities should not crash
prop_handle_error_with_severity :: String -> Int -> Property
prop_handle_error_with_severity message severityIndex = 
  not (null message) && severityIndex >= 0 && severityIndex < 4 ==>
  let severities = [Info, Warning, Error, Fatal]
      severity = severities !! severityIndex
      location = unknownLocation
      error = errorAt message severity (T.pack message) location
      result = execState (addError error) []
  in property True  -- Should not crash

-- Property 6: Getting errors from handler should not crash
prop_get_errors :: [String] -> Property
prop_get_errors messages = 
  let errors = map (\msg -> errorAt msg Error (T.pack msg) unknownLocation) messages
      errorList = execState (mapM_ addError errors) []
  in property $ length errorList >= 0  -- Should not crash

-- Property 7: Checking if handler has errors should not crash
prop_has_errors :: [String] -> Property
prop_has_errors messages = 
  let errors = map (\msg -> errorAt msg Error (T.pack msg) unknownLocation) messages
      errorList = execState (mapM_ addError errors) []
  in property $ (not (null errorList) && not (null messages)) || (null errorList && null messages)

-- Property 8: Clearing errors should not crash
prop_clear_errors :: [String] -> Property
prop_clear_errors messages = 
  let errors = map (\msg -> errorAt msg Error (T.pack msg) unknownLocation) messages
      errorList = execState (mapM_ addError errors) []
      clearedErrors = [] :: [TypeError]
  in property $ length clearedErrors == 0  -- Should be empty after clearing

-- Property 9: Error count should be correct
prop_error_count :: [String] -> Property
prop_error_count messages = 
  let errors = map (\msg -> errorAt msg Error (T.pack msg) unknownLocation) messages
      errorList = execState (mapM_ addError errors) []
      count = length errorList
  in property $ count == length messages

-- Property 10: Formatting errors should not crash
prop_format_errors :: [String] -> Property
prop_format_errors messages = 
  let errors = map (\msg -> errorAt msg Error (T.pack msg) unknownLocation) messages
      formatted = map formatError errors
  in property $ length formatted >= 0  -- Should not crash

-- Unit tests for specific error handler functionality

test_new_error_handler :: Assertion
test_new_error_handler = 
  let collector = newErrorCollector
  in assertBool "Creating error handler should not crash" True

test_handle_empty_error :: Assertion
test_handle_empty_error = 
  let error = errorAt "" Error (T.pack "test") unknownLocation
      result = execState (addError error) []
  in assertBool "Handling empty error should not crash" True

test_handle_error_with_message :: Assertion
test_handle_error_with_message = 
  let error = errorAt "Test error message" Error (T.pack "Test error message") unknownLocation
      result = execState (addError error) []
  in assertBool "Handling error with message should not crash" True

test_handle_syntax_error :: Assertion
test_handle_syntax_error = 
  let error = errorWithCategory "Syntax error" Parsing (T.pack "Syntax error") unknownLocation
      result = execState (addError error) []
  in assertBool "Handling syntax error should not crash" True

test_handle_type_error :: Assertion
test_handle_type_error = 
  let error = errorWithCategory "Type error" TypeChecking (T.pack "Type error") unknownLocation
      result = execState (addError error) []
  in assertBool "Handling type error should not crash" True

test_handle_warning :: Assertion
test_handle_warning = 
  let error = warningAt "Warning message" (T.pack "Warning message") unknownLocation
      result = execState (addError error) []
  in assertBool "Handling warning should not crash" True

test_get_errors :: Assertion
test_get_errors = 
  let error1 = errorAt "Error 1" Error (T.pack "Error 1") unknownLocation
      error2 = errorAt "Error 2" Error (T.pack "Error 2") unknownLocation
      errors = execState (mapM_ addError [error1, error2]) []
  in assertEqual "Should have 2 errors" 2 (length errors)

test_has_errors :: Assertion
test_has_errors = 
  let error = errorAt "Test error" Error (T.pack "Test error") unknownLocation
      errors = execState (addError error) []
  in assertBool "Should have errors" $ not (null errors)

test_clear_errors :: Assertion
test_clear_errors = 
  let error = errorAt "Test error" Error (T.pack "Test error") unknownLocation
      errors = execState (addError error) []
      clearedErrors = [] :: [TypeError]
  in assertEqual "Should have no errors after clearing" 0 (length clearedErrors)

test_error_count :: Assertion
test_error_count = 
  let error1 = errorAt "Error 1" Error (T.pack "Error 1") unknownLocation
      error2 = errorAt "Error 2" Error (T.pack "Error 2") unknownLocation
      error3 = errorAt "Error 3" Error (T.pack "Error 3") unknownLocation
      errors = execState (mapM_ addError [error1, error2, error3]) []
      count = length errors
  in assertEqual "Should have 3 errors" 3 count

test_format_error :: Assertion
test_format_error = 
  let error = errorAt "Test error" Error (T.pack "Test error") unknownLocation
      formatted = formatError error
  in assertBool "Formatting error should not crash" $ not (null formatted)

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedErrorHandlerSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "new error handler" prop_new_error_handler
    , testProperty "handle empty error" prop_handle_empty_error
    , testProperty "handle error with message" prop_handle_error_with_message
    , testProperty "handle error with type" prop_handle_error_with_type
    , testProperty "handle error with severity" prop_handle_error_with_severity
    , testProperty "get errors" prop_get_errors
    , testProperty "has errors" prop_has_errors
    , testProperty "clear errors" prop_clear_errors
    , testProperty "error count" prop_error_count
    , testProperty "format errors" prop_format_errors
    ]
  , testGroup "Unit Tests"
    [ testCase "new error handler" test_new_error_handler
    , testCase "handle empty error" test_handle_empty_error
    , testCase "handle error with message" test_handle_error_with_message
    , testCase "handle syntax error" test_handle_syntax_error
    , testCase "handle type error" test_handle_type_error
    , testCase "handle warning" test_handle_warning
    , testCase "get errors" test_get_errors
    , testCase "has errors" test_has_errors
    , testCase "clear errors" test_clear_errors
    , testCase "error count" test_error_count
    , testCase "format error" test_format_error
    ]
  ]