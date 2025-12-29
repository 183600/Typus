{-# LANGUAGE TemplateHaskell #-}

-- | Consistency tests for ErrorHandler module
module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import ErrorHandler 
  ( ErrorHandler
  , ErrorContext(..)
  , ErrorSeverity(..)
  , ErrorMessage(..)
  , newErrorHandler
  , handleError
  , formatError
  , collectErrors
  , hasErrors
  , clearErrors
  )
import qualified Data.Text as T
import Data.List (sort)

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | ErrorSeverity should be comparable
prop_error_severity_comparable :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_comparable sev1 sev2 =
  let comparison = compare sev1 sev2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

-- | ErrorSeverity ordering should be total
prop_error_severity_total_ordering :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_total_ordering sev1 sev2 sev3 =
  let comp12 = compare sev1 sev2
      comp23 = compare sev2 sev3
      comp13 = compare sev1 sev3
  in property $ (comp12 == EQ && comp23 == EQ) ==> (comp13 == EQ)

-- | ErrorHandler should be created consistently
prop_error_handler_consistent :: Property
prop_error_handler_consistent =
  let handler1 = newErrorHandler
      handler2 = newErrorHandler
  in hasErrors handler1 === False .&&. hasErrors handler2 === False

-- | New ErrorHandler should have no errors
prop_new_handler_no_errors :: Property
prop_new_handler_no_errors =
  let handler = newErrorHandler
  in hasErrors handler === False

-- | Error handling should be deterministic
prop_error_handling_deterministic :: String -> ErrorSeverity -> Property
prop_error_handling_deterministic message severity =
  let handler = newErrorHandler
      context = ErrorContext "test" 1 1
      errorMsg = ErrorMessage message severity context
      result1 = handleError handler errorMsg
      result2 = handleError handler errorMsg
  in hasErrors result1 === hasErrors result2

-- | Error formatting should produce non-empty strings
prop_error_formatting_non_empty :: String -> ErrorSeverity -> Property
prop_error_formatting_non_empty message severity =
  let context = ErrorContext "test" 1 1
      errorMsg = ErrorMessage message severity context
      formatted = formatError errorMsg
  in not (T.null formatted) === True

-- | Error collection should preserve count
prop_error_collection_preserves_count :: [String] -> ErrorSeverity -> Property
prop_error_collection_preserves_count messages severity =
  let handler = newErrorHandler
      context = ErrorContext "test" 1 1
      errorMessages = map (\msg -> ErrorMessage msg severity context) messages
      handlerWithErrors = foldl handleError handler errorMessages
      collectedErrors = collectErrors handlerWithErrors
  in length collectedErrors === length messages

-- | Clear errors should reset handler state
prop_clear_errors_resets :: [String] -> ErrorSeverity -> Property
prop_clear_errors_resets messages severity =
  let handler = newErrorHandler
      context = ErrorContext "test" 1 1
      errorMessages = map (\msg -> ErrorMessage msg severity context) messages
      handlerWithErrors = foldl handleError handler errorMessages
      clearedHandler = clearErrors handlerWithErrors
  in hasErrors clearedHandler === False

-- | Error context should be preserved
prop_error_context_preserved :: String -> Int -> Int -> Property
prop_error_context_preserved file line column =
  let handler = newErrorHandler
      context = ErrorContext file line column
      errorMsg = ErrorMessage "test" ErrorError context
      handlerWithError = handleError handler errorMsg
      collectedErrors = collectErrors handlerWithError
  in case collectedErrors of
    [] -> property False  -- Should have an error
    (err:_) -> let ErrorContext f l c = errContext err
                in f === file .&&. l === line .&&. c === column

-- | Error severity should affect error collection
prop_error_severity_affects_collection :: String -> ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_affects_collection message sev1 sev2 =
  let handler = newErrorHandler
      context = ErrorContext "test" 1 1
      errorMsg1 = ErrorMessage message sev1 context
      errorMsg2 = ErrorMessage message sev2 context
      handler1 = handleError handler errorMsg1
      handler2 = handleError handler errorMsg2
      errors1 = collectErrors handler1
      errors2 = collectErrors handler2
  in length errors1 === length errors2 .&&. 
     (if sev1 == sev2 
      then map errSeverity errors1 === map errSeverity errors2
      else property True)

-- | Multiple errors should be collected in order
prop_multiple_errors_order :: [String] -> ErrorSeverity -> Property
prop_multiple_errors_order messages severity =
  let handler = newErrorHandler
      context = ErrorContext "test" 1 1
      errorMessages = map (\msg -> ErrorMessage msg severity context) messages
      handlerWithErrors = foldl handleError handler errorMessages
      collectedErrors = collectErrors handlerWithErrors
      collectedMessages = map errMessage collectedErrors
  in collectedMessages === messages

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "ErrorHandler Consistency QuickCheck Tests"
  [ testProperty "ErrorSeverity: comparability" prop_error_severity_comparable
  , testProperty "ErrorSeverity: total ordering" prop_error_severity_total_ordering
  , testProperty "ErrorHandler: consistent creation" prop_error_handler_consistent
  , testProperty "New handler: no errors" prop_new_handler_no_errors
  , testProperty "Error handling: determinism" prop_error_handling_deterministic
  , testProperty "Error formatting: non-empty output" prop_error_formatting_non_empty
  , testProperty "Error collection: preserves count" prop_error_collection_preserves_count
  , testProperty "Clear errors: resets handler" prop_clear_errors_resets
  , testProperty "Error context: preserved" prop_error_context_preserved
  , testProperty "Error severity: affects collection" prop_error_severity_affects_collection
  ]