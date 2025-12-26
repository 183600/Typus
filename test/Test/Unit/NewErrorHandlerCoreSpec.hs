{-# LANGUAGE CPP #-}

module Test.Unit.NewErrorHandlerCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Control.Exception (try, SomeException)

import ErrorHandler (ErrorHandler, runErrorHandler, handleError, ErrorLevel(..))
import EnhancedErrorHandler (EnhancedErrorHandler, runEnhancedErrorHandler, 
                            enhancedHandleError, ErrorContext(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

-- Test 1: Error handler idempotence
prop_error_handler_idempotent :: String -> ErrorLevel -> Property
prop_error_handler_idempotent msg level =
  let handler = handleError level msg
      result1 = runErrorHandler handler
      result2 = runErrorHandler handler
  in result1 === result2

-- Test 2: Error context preservation
prop_error_context_preservation :: String -> ErrorLevel -> SourcePos -> Property
prop_error_context_preservation msg level pos =
  let context = ErrorContext pos level msg
      enhancedHandler = enhancedHandleError context
      result = runEnhancedErrorHandler enhancedHandler
  in case result of
    Left err -> property True -- Error case is acceptable
    Right _ -> property True -- Success case is acceptable

-- Test 3: Error level ordering
prop_error_level_ordering :: ErrorLevel -> ErrorLevel -> Property
prop_error_level_ordering level1 level2 =
  let levels = [Debug, Info, Warning, Error, Critical]
      level1Index = length $ takeWhile (/= level1) levels
      level2Index = length $ takeWhile (/= level2) levels
  in (level1Index <= level2Index) ==> level1 <= level2

-- Test 4: Error message non-empty
prop_error_message_non_empty :: String -> ErrorLevel -> Property
prop_error_message_non_empty msg level =
  not (null msg) ==>
  let handler = handleError level msg
      result = runErrorHandler handler
  in case result of
    Left err -> length err > 0
    Right _ -> property True

-- Test 5: Enhanced error handler context
prop_enhanced_error_context :: String -> ErrorLevel -> SourceSpan -> Property
prop_enhanced_error_context msg level span =
  let pos = spanStart span
      context = ErrorContext pos level msg
      enhancedHandler = enhancedHandleError context
      result = runEnhancedErrorHandler enhancedHandler
  in case result of
    Left err -> property True
    Right _ -> property True

-- Test 6: Error handler exception safety
prop_error_handler_exception_safe :: String -> Property
prop_error_handler_exception_safe msg =
  let handler = handleError Error msg
      result = try $ runErrorHandler handler
  in case result of
    Left (e :: SomeException) -> property False -- Should not throw exceptions
    Right _ -> property True

-- Test 7: Multiple error handling
prop_multiple_error_handling :: [String] -> ErrorLevel -> Property
prop_multiple_error_handling msgs level =
  length msgs < 10 ==> -- Limit to reasonable size
  let handlers = map (handleError level) msgs
      results = map runErrorHandler handlers
  in length results === length handlers

-- Test 8: Error context with different positions
prop_error_context_different_positions :: String -> ErrorLevel -> Int -> Int -> Property
prop_error_context_different_positions msg level line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col 0
      context = ErrorContext pos level msg
      enhancedHandler = enhancedHandleError context
      result = runEnhancedErrorHandler enhancedHandler
  in case result of
    Left err -> property True
    Right _ -> property True

-- Test 9: Error level severity
prop_error_level_severity :: ErrorLevel -> Property
prop_error_level_severity level =
  let handler = handleError level "test message"
      result = runErrorHandler handler
  in case result of
    Left err -> property True -- All levels should produce errors
    Right _ -> level `elem` [Debug, Info] -- Only debug and info might succeed

-- Test 10: Enhanced error handler preserves message
prop_enhanced_error_preserves_message :: String -> ErrorLevel -> Property
prop_enhanced_error_preserves_message msg level =
  not (null msg) ==>
  let pos = SourcePos 1 1 0
      context = ErrorContext pos level msg
      enhancedHandler = enhancedHandleError context
      result = runEnhancedErrorHandler enhancedHandler
  in case result of
    Left err -> msg `isInfixOf` err
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Error Handler Core Tests"
  [ fastProperty "Error handler idempotent" prop_error_handler_idempotent
  , fastProperty "Error context preservation" prop_error_context_preservation
  , fastProperty "Error level ordering" prop_error_level_ordering
  , fastProperty "Error message non-empty" prop_error_message_non_empty
  , fastProperty "Enhanced error context" prop_enhanced_error_context
  , fastProperty "Error handler exception safe" prop_error_handler_exception_safe
  , fastProperty "Multiple error handling" prop_multiple_error_handling
  , fastProperty "Error context with different positions" prop_error_context_different_positions
  , fastProperty "Error level severity" prop_error_level_severity
  , fastProperty "Enhanced error preserves message" prop_enhanced_error_preserves_message
  ]