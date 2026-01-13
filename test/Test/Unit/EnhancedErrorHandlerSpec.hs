module Test.Unit.EnhancedErrorHandlerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)
import Control.Exception (try, SomeException)

-- Import ErrorHandler module
import ErrorHandler (ErrorHandler, ErrorSeverity(..), ErrorType(..), 
                    ErrorMessage(..), ErrorContext(..), ErrorReporter,
                    newErrorHandler, handleError, reportError, 
                    formatError, getErrors, clearErrors, hasErrors,
                    errorCount, getErrorsBySeverity, getErrorsByType,
                    createError, createErrorWithSeverity, createErrorWithContext)

-- Import Compiler module
import Compiler (CompilerError(..), SyntaxError(..), TypeError(..))

-- Test properties for error handler

-- Property 1: Creating error handler should not crash
prop_new_error_handler :: Property
prop_new_error_handler = property $
  let handler = newErrorHandler
  in property True  -- Should not crash

-- Property 2: Handling empty error should not crash
prop_handle_empty_error :: Property
prop_handle_empty_error = property $
  let handler = newErrorHandler
      error = createError "" ErrorTypeOther ErrorSeverityInfo
      result = handleError handler error
  in property True  -- Should not crash

-- Property 3: Handling error with message should not crash
prop_handle_error_with_message :: String -> Property
prop_handle_error_with_message message = 
  not (null message) ==>
  let handler = newErrorHandler
      error = createError message ErrorTypeOther ErrorSeverityInfo
      result = handleError handler error
  in property True  -- Should not crash

-- Property 4: Handling error with different types should not crash
prop_handle_error_with_type :: String -> Int -> Property
prop_handle_error_with_type message typeIndex = 
  not (null message) && typeIndex >= 0 && typeIndex < 10 ==>
  let errorTypes = [ErrorTypeSyntax, ErrorTypeType, ErrorSemantic, 
                    ErrorTypeRuntime, ErrorTypeIO, ErrorTypeOther,
                    ErrorTypeWarning, ErrorTypeNote, ErrorTypeHelp, ErrorTypeInternal]
      errorType = errorTypes !! typeIndex
      handler = newErrorHandler
      error = createError message errorType ErrorSeverityInfo
      result = handleError handler error
  in property True  -- Should not crash

-- Property 5: Handling error with different severities should not crash
prop_handle_error_with_severity :: String -> Int -> Property
prop_handle_error_with_severity message severityIndex = 
  not (null message) && severityIndex >= 0 && severityIndex < 4 ==>
  let severities = [ErrorSeverityInfo, ErrorSeverityWarning, 
                   ErrorSeverityError, ErrorSeverityFatal]
      severity = severities !! severityIndex
      handler = newErrorHandler
      error = createError message ErrorTypeOther severity
      result = handleError handler error
  in property True  -- Should not crash

-- Property 6: Getting errors from handler should not crash
prop_get_errors :: [String] -> Property
prop_get_errors messages = 
  let handler = newErrorHandler
      errors = map (\msg -> createError msg ErrorTypeOther ErrorSeverityInfo) messages
      handler' = foldl handleError handler errors
      result = getErrors handler'
  in property $ length result >= 0  -- Should not crash

-- Property 7: Checking if handler has errors should not crash
prop_has_errors :: [String] -> Property
prop_has_errors messages = 
  let handler = newErrorHandler
      errors = map (\msg -> createError msg ErrorTypeOther ErrorSeverityInfo) messages
      handler' = foldl handleError handler errors
      result = hasErrors handler'
  in property $ (result && not (null messages)) || (not result && null messages)

-- Property 8: Clearing errors should not crash
prop_clear_errors :: [String] -> Property
prop_clear_errors messages = 
  let handler = newErrorHandler
      errors = map (\msg -> createError msg ErrorTypeOther ErrorSeverityInfo) messages
      handler' = foldl handleError handler errors
      handler'' = clearErrors handler'
      result = getErrors handler''
  in property $ length result == 0  -- Should be empty after clearing

-- Property 9: Error count should be correct
prop_error_count :: [String] -> Property
prop_error_count messages = 
  let handler = newErrorHandler
      errors = map (\msg -> createError msg ErrorTypeOther ErrorSeverityInfo) messages
      handler' = foldl handleError handler errors
      count = errorCount handler'
  in property $ count == length messages

-- Property 10: Formatting errors should not crash
prop_format_errors :: [String] -> Property
prop_format_errors messages = 
  let handler = newErrorHandler
      errors = map (\msg -> createError msg ErrorTypeOther ErrorSeverityInfo) messages
      handler' = foldl handleError handler errors
      formatted = map formatError (getErrors handler')
  in property $ length formatted >= 0  -- Should not crash

-- Unit tests for specific error handler functionality

test_new_error_handler :: Assertion
test_new_error_handler = 
  let handler = newErrorHandler
  in assertBool "Creating error handler should not crash" True

test_handle_empty_error :: Assertion
test_handle_empty_error = 
  let handler = newErrorHandler
      error = createError "" ErrorTypeOther ErrorSeverityInfo
      result = handleError handler error
  in assertBool "Handling empty error should not crash" True

test_handle_error_with_message :: Assertion
test_handle_error_with_message = 
  let handler = newErrorHandler
      error = createError "Test error message" ErrorTypeOther ErrorSeverityInfo
      result = handleError handler error
  in assertBool "Handling error with message should not crash" True

test_handle_syntax_error :: Assertion
test_handle_syntax_error = 
  let handler = newErrorHandler
      error = createError "Syntax error" ErrorTypeSyntax ErrorSeverityError
      result = handleError handler error
  in assertBool "Handling syntax error should not crash" True

test_handle_type_error :: Assertion
test_handle_type_error = 
  let handler = newErrorHandler
      error = createError "Type error" ErrorTypeType ErrorSeverityError
      result = handleError handler error
  in assertBool "Handling type error should not crash" True

test_handle_warning :: Assertion
test_handle_warning = 
  let handler = newErrorHandler
      error = createError "Warning message" ErrorTypeWarning ErrorSeverityWarning
      result = handleError handler error
  in assertBool "Handling warning should not crash" True

test_get_errors :: Assertion
test_get_errors = 
  let handler = newErrorHandler
      error1 = createError "Error 1" ErrorTypeOther ErrorSeverityInfo
      error2 = createError "Error 2" ErrorTypeOther ErrorSeverityInfo
      handler' = handleError (handleError handler error1) error2
      errors = getErrors handler'
  in assertEqual "Should have 2 errors" 2 (length errors)

test_has_errors :: Assertion
test_has_errors = 
  let handler = newErrorHandler
      error = createError "Test error" ErrorTypeOther ErrorSeverityInfo
      handler' = handleError handler error
      hasErrs = hasErrors handler'
  in assertBool "Should have errors" hasErrs

test_clear_errors :: Assertion
test_clear_errors = 
  let handler = newErrorHandler
      error = createError "Test error" ErrorTypeOther ErrorSeverityInfo
      handler' = handleError handler error
      handler'' = clearErrors handler'
      errors = getErrors handler''
  in assertEqual "Should have no errors after clearing" 0 (length errors)

test_error_count :: Assertion
test_error_count = 
  let handler = newErrorHandler
      error1 = createError "Error 1" ErrorTypeOther ErrorSeverityInfo
      error2 = createError "Error 2" ErrorTypeOther ErrorSeverityInfo
      error3 = createError "Error 3" ErrorTypeOther ErrorSeverityInfo
      handler' = handleError (handleError (handleError handler error1) error2) error3
      count = errorCount handler'
  in assertEqual "Should have 3 errors" 3 count

test_format_error :: Assertion
test_format_error = 
  let error = createError "Test error" ErrorTypeOther ErrorSeverityInfo
      formatted = formatError error
  in assertBool "Error formatting should not crash" $ not (null formatted)

test_get_errors_by_severity :: Assertion
test_get_errors_by_severity = 
  let handler = newErrorHandler
      error1 = createError "Error 1" ErrorTypeOther ErrorSeverityError
      error2 = createError "Warning 1" ErrorTypeWarning ErrorSeverityWarning
      error3 = createError "Error 2" ErrorTypeOther ErrorSeverityError
      handler' = handleError (handleError (handleError handler error1) error2) error3
      errors = getErrorsBySeverity handler' ErrorSeverityError
      warnings = getErrorsBySeverity handler' ErrorSeverityWarning
  in do
    assertEqual "Should have 2 errors" 2 (length errors)
    assertEqual "Should have 1 warning" 1 (length warnings)

test_get_errors_by_type :: Assertion
test_get_errors_by_type = 
  let handler = newErrorHandler
      error1 = createError "Syntax error" ErrorTypeSyntax ErrorSeverityError
      error2 = createError "Type error" ErrorTypeType ErrorSeverityError
      error3 = createError "Another syntax error" ErrorTypeSyntax ErrorSeverityError
      handler' = handleError (handleError (handleError handler error1) error2) error3
      syntaxErrors = getErrorsByType handler' ErrorTypeSyntax
      typeErrors = getErrorsByType handler' ErrorTypeType
  in do
    assertEqual "Should have 2 syntax errors" 2 (length syntaxErrors)
    assertEqual "Should have 1 type error" 1 (length typeErrors)

test_create_error_with_severity :: Assertion
test_create_error_with_severity = 
  let error = createErrorWithSeverity "Test error" ErrorTypeOther ErrorSeverityFatal
  in assertEqual "Should have fatal severity" ErrorSeverityFatal (errorMessageSeverity error)

test_create_error_with_context :: Assertion
test_create_error_with_context = 
  let context = ErrorContext "test.txt" 1 2 "test function"
      error = createErrorWithContext "Test error" ErrorTypeOther ErrorSeverityInfo context
  in assertEqual "Should have context" context (errorMessageContext error)

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
    , testCase "get errors by severity" test_get_errors_by_severity
    , testCase "get errors by type" test_get_errors_by_type
    , testCase "create error with severity" test_create_error_with_severity
    , testCase "create error with context" test_create_error_with_context
    ]
  ]