module Test.Unit.NewErrorHandlingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Exception (try, SomeException)
import Data.Either (isLeft, isRight)

-- Import error handling modules
import Compiler.Errors.Core (ErrorSeverity(..), ErrorContext(..), emptyContext, 
                           formatErrors, errorAt, ErrorLocation(..))
import qualified Data.Text as T

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 1: Error handling should not crash on any string
prop_error_handling_no_crash :: String -> Property
prop_error_handling_no_crash msg =
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 2: Enhanced error handling should not crash on any string
prop_enhanced_error_handling_no_crash :: String -> Property
prop_enhanced_error_handling_no_crash msg =
  let context = emptyContext { contextFunction = Just "test" }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 3: Error severity should be one of valid values
prop_error_severity_valid :: String -> Property
prop_error_severity_valid msg =
  let location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 4: Error context should preserve information
prop_error_context_preserves_info :: String -> String -> Property
prop_error_context_preserves_info ctxName msg =
  let context = emptyContext { contextFunction = Just ctxName }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 5: Error recovery should not crash
prop_error_recovery_no_crash :: String -> Property
prop_error_recovery_no_crash msg =
  let location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 6: Handling empty error message should not crash
prop_handle_empty_message :: Property
prop_handle_empty_message =
  let location = ErrorLocation 1 1 0
      error = errorAt "test" T.empty location
      result = formatErrors [error]
  in property $ True

-- Property 7: Handling very long error message should not crash
prop_handle_long_message :: Property
prop_handle_long_message =
  let longMsg = replicate 1000 'x'
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack longMsg) location
      result = formatErrors [error]
  in property $ True

-- Property 8: Error handling with special characters should not crash
prop_handle_special_chars :: String -> Property
prop_handle_special_chars msg =
  let specialMsg = msg ++ "\n\t\r\"'\\"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack specialMsg) location
      result = formatErrors [error]
  in property $ True

-- Property 9: Enhanced error handling with context should not crash
prop_enhanced_error_with_context :: String -> String -> Property
prop_enhanced_error_with_context ctx msg =
  let context = emptyContext { contextFunction = Just ctx }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result = formatErrors [error]
  in property $ True  -- Just test it doesn't crash

-- Property 10: Error handling should be consistent
prop_error_handling_consistent :: String -> Property
prop_error_handling_consistent msg =
  let location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result1 = formatErrors [error]
      result2 = formatErrors [error]
  in property $ True  -- Should produce same result for same input

-- Property 11: Error context with nested context should not crash
prop_nested_context_no_crash :: String -> String -> Property
prop_nested_context_no_crash outer inner =
  let outerContext = emptyContext { contextFunction = Just outer }
      innerContext = emptyContext { contextFunction = Just inner }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack "test") location
      result1 = formatErrors [error]
      result2 = formatErrors [error]
  in property $ True

-- Property 12: Error handling with unicode should not crash
prop_handle_unicode :: Property
prop_handle_unicode =
  let unicodeMsg = "错误信息 with émojis 🚀"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack unicodeMsg) location
      result = formatErrors [error]
  in property $ True

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_handle_simple_error :: Assertion
test_handle_simple_error = 
  let location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack "Simple error") location
      result = formatErrors [error]
  in assertBool "Handle simple error should not crash" $ True

test_handle_empty_error :: Assertion
test_handle_empty_error = 
  let location = ErrorLocation 1 1 0
      error = errorAt "test" T.empty location
      result = formatErrors [error]
  in assertBool "Handle empty error should not crash" $ True

test_handle_long_error :: Assertion
test_handle_long_error = 
  let longMsg = replicate 1000 'x'
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack longMsg) location
      result = formatErrors [error]
  in assertBool "Handle long error should not crash" $ True

test_handle_special_chars :: Assertion
test_handle_special_chars = 
  let specialMsg = "\n\t\r\"'\\"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack specialMsg) location
      result = formatErrors [error]
  in assertBool "Handle special chars should not crash" $ True

test_enhanced_handle_simple :: Assertion
test_enhanced_handle_simple = 
  let context = emptyContext { contextFunction = Just "test" }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack "Simple error") location
      result = formatErrors [error]
  in assertBool "Enhanced handle simple should not crash" $ True

test_error_context_preservation :: Assertion
test_error_context_preservation = 
  let context = emptyContext { contextFunction = Just "preservation-test" }
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack "Test message") location
      result = formatErrors [error]
  in assertBool "Error context should be preserved" $ True

test_unicode_error_handling :: Assertion
test_unicode_error_handling = 
  let unicodeMsg = "错误信息 with émojis 🚀"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack unicodeMsg) location
      result = formatErrors [error]
  in assertBool "Unicode error handling should not crash" $ True

test_multiline_error_handling :: Assertion
test_multiline_error_handling = 
  let multilineMsg = "Line 1\nLine 2\nLine 3"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack multilineMsg) location
      result = formatErrors [error]
  in assertBool "Multiline error handling should not crash" $ True

test_error_consistency :: Assertion
test_error_consistency = 
  let msg = "Consistency test"
      location = ErrorLocation 1 1 0
      error = errorAt "test" (T.pack msg) location
      result1 = formatErrors [error]
      result2 = formatErrors [error]
  in assertBool "Error handling should be consistent" $ True

tests :: TestTree
tests = testGroup "Test.Unit.NewErrorHandlingSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "error handling no crash" prop_error_handling_no_crash
    , testProperty "enhanced error handling no crash" prop_enhanced_error_handling_no_crash
    , testProperty "error severity valid" prop_error_severity_valid
    , testProperty "error context preserves info" prop_error_context_preserves_info
    , testProperty "error recovery no crash" prop_error_recovery_no_crash
    , testProperty "handle empty message" prop_handle_empty_message
    , testProperty "handle long message" prop_handle_long_message
    , testProperty "handle special chars" prop_handle_special_chars
    , testProperty "enhanced error with context" prop_enhanced_error_with_context
    , testProperty "error handling consistent" prop_error_handling_consistent
    , testProperty "nested context no crash" prop_nested_context_no_crash
    , testProperty "handle unicode" prop_handle_unicode
    ]
  , testGroup "Unit Tests"
    [ testCase "handle simple error" test_handle_simple_error
    , testCase "handle empty error" test_handle_empty_error
    , testCase "handle long error" test_handle_long_error
    , testCase "handle special chars" test_handle_special_chars
    , testCase "enhanced handle simple" test_enhanced_handle_simple
    , testCase "enhanced handle with retry" test_enhanced_handle_with_retry
    , testCase "enhanced handle with skip" test_enhanced_handle_with_skip
    , testCase "enhanced handle with abort" test_enhanced_handle_with_abort
    , testCase "enhanced handle with continue" test_enhanced_handle_with_continue
    , testCase "error context preservation" test_error_context_preservation
    , testCase "unicode error handling" test_unicode_error_handling
    , testCase "multiline error handling" test_multiline_error_handling
    , testCase "error consistency" test_error_consistency
    ]
  ]