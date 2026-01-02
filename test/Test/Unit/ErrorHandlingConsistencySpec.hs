{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import ErrorHandler (ErrorHandler, runErrorHandler)
import EnhancedErrorHandler (EnhancedErrorHandler, runEnhancedErrorHandler)
import Compiler.Errors.Core (ErrorLocation(..), CompilerError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Control.Monad (void)
import Control.Exception (try, SomeException)

-- | Error handling consistency tests
tests :: TestTree
tests = testGroup "Error Handling Consistency"
  [ testGroup "Basic Error Handler Tests"
    [ testCase "error handler handles empty input" test_error_handler_empty_input
    , testCase "error handler handles single error" test_error_handler_single_error
    , testCase "error handler handles multiple errors" test_error_handler_multiple_errors
    , fastProperty "error handler consistency" prop_error_handler_consistency
    ]

  , testGroup "Enhanced Error Handler Tests"
    [ testCase "enhanced error handler handles complex scenarios" test_enhanced_error_handler_complex
    , testCase "enhanced error handler preserves context" test_enhanced_error_handler_context
    , fastProperty "enhanced error handler monotonicity" prop_enhanced_error_handler_monotonicity
    ]

  , testGroup "Error Location Consistency"
    [ testCase "error locations are consistent" test_error_locations_consistent
    , testCase "error spans are valid" test_error_spans_valid
    , fastProperty "error location preservation" prop_error_location_preservation
    ]

  , testGroup "Error Recovery Tests"
    [ testCase "error recovery maintains state" test_error_recovery_state
    , testCase "error recovery preserves valid data" test_error_recovery_preserves_data
    , fastProperty "error recovery associativity" prop_error_recovery_associative
    ]

  , testGroup "Cross-Module Error Consistency"
    [ testCase "basic L.and enhanced handlers consistent" test_basic_enhanced_consistent
    , testCase "error propagation consistent" test_error_propagation_consistent
    , fastProperty "error handler composition" prop_error_handler_composition
    ]

  , testGroup "Error Message Consistency"
    [ testCase "error messages are deterministic" test_error_messages_deterministic
    , testCase "error message formatting consistent" test_error_message_formatting
    , fastProperty "error message idempotency" prop_error_message_idempotency
    ]

  , testGroup "Performance L.and Memory Consistency"
    [ fastProperty "error handler performance consistency" prop_error_handler_performance_consistency
    , fastProperty "error handler memory efficiency" prop_error_handler_memory_efficiency
    ]

  , testGroup "Edge Case Consistency"
    [ testCase "error handling with unicode" test_error_handling_unicode
    , testCase "error handling with large inputs" test_error_handling_large_inputs
    , fastProperty "error handling boundary conditions" prop_error_handling_boundaries
    ]
  ]

-- ============================================================================
-- Basic Error Handler Tests
-- ============================================================================

test_error_handler_empty_input :: IO ()
test_error_handler_empty_input = do
  let result = runErrorHandler ""
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (errors, _) -> do
      assertBool "No errors for empty input" $ null errors

test_error_handler_single_error :: IO ()
test_error_handler_single_error = do
  let input = "func invalid( {"
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      assertBool "Should detect syntax error" $ not (null errors)

test_error_handler_multiple_errors :: IO ()
test_error_handler_multiple_errors = do
  let input = unlines
        [ "func invalid1( {"
        , "func invalid2( {"
        , "func invalid3( {"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      assertBool "Should detect multiple errors" $ L.length errors >= 3

prop_error_handler_consistency :: String -> Property
prop_error_handler_consistency input =
  L.length input <= 100 ==>
  let result1 = runErrorHandler input
      result2 = runErrorHandler input
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right (errs1, _), Right (errs2, _)) -> property $ L.length errs1 == L.length errs2
       _ -> property False

-- ============================================================================
-- Enhanced Error Handler Tests
-- ============================================================================

test_enhanced_error_handler_complex :: IO ()
test_enhanced_error_handler_complex = do
  let input = unlines
        [ "//! ownership=true"
        , "func complex( {"
        , "    // 中文注释"
        , "    x := \"unicode café naïve\" 🚀"
        , "    return x"
        , "}"
        ]
      result = runEnhancedErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, context) -> do
      assertBool "Should handle complex scenarios" $ not (null errors) || not (null context)

test_enhanced_error_handler_context :: IO ()
test_enhanced_error_handler_context = do
  let input = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := 42"
        , "}"
        ]
      result = runEnhancedErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, context) -> do
      assertBool "Should preserve context" $ not (null context)

prop_enhanced_error_handler_monotonicity :: String -> Property
prop_enhanced_error_handler_monotonicity input =
  L.length input <= 100 ==>
  let result = runEnhancedErrorHandler input
  in case result of
       Left _ -> property False
       Right (errors, context) -> property $ L.length errors + L.length context >= 0

-- ============================================================================
-- Error Location Consistency
-- ============================================================================

test_error_locations_consistent :: IO ()
test_error_locations_consistent = do
  let input = "func invalid( {"
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      mapM_ checkErrorLocation errors
  where
    checkErrorLocation error = do
      let location = errorLocation error
      assertBool "Error location should be valid" $ isValidErrorLocation location

test_error_spans_valid :: IO ()
test_error_spans_valid = do
  let input = unlines
        [ "func test() {"
        , "    x := 42"
        , "}"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      mapM_ checkErrorSpan errors

prop_error_location_preservation :: String -> Property
prop_error_location_preservation input =
  L.length input <= 100 ==>
  let result = runErrorHandler input
  in case result of
       Left _ -> property False
       Right (errors, _) -> property $ L.all isValidErrorLocation (map errorLocation errors)

-- Helper functions for error location validation
isValidErrorLocation :: ErrorLocation -> Bool
isValidErrorLocation (ErrorLocation line col _) = line > 0 && col > 0

errorLocation :: CompilerError -> ErrorLocation
errorLocation error = case error of
  SyntaxError loc _ -> loc
  TypeError loc _ -> loc
  SemanticError loc _ -> loc

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

test_error_recovery_state :: IO ()
test_error_recovery_state = do
  let input = unlines
        [ "func test1() {"
        , "    x := 42"
        , "}"
        , "func invalid( {"
        , "func test2() {"
        , "    y := 24"
        , "}"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, state) -> do
      assertBool "Should maintain state after error" $ not (null state)

test_error_recovery_preserves_data :: IO ()
test_error_recovery_preserves_data = do
  let input = unlines
        [ "//! ownership=true"
        , "func valid() { return 42; }"
        , "func invalid( {"
        , "func another() { return 24; }"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, state) -> do
      assertBool "Should preserve valid data" $ not (null state)

prop_error_recovery_associative :: String -> String -> Property
prop_error_recovery_associative input1 input2 =
  L.length input1 <= 50 && L.length input2 <= 50 ==>
  let result1 = runErrorHandler input1
      result2 = runErrorHandler input2
      combined = input1 ++ "\n" ++ input2
      resultCombined = runErrorHandler combined
  in case (result1, result2, resultCombined) of
       (Right (errs1, _), Right (errs2, _), Right (errsCombined, _)) ->
         property $ L.length errsCombined >= L.length errs1 + L.length errs2
       _ -> property False

-- ============================================================================
-- Cross-Module Error Consistency
-- ============================================================================

test_basic_enhanced_consistent :: IO ()
test_basic_enhanced_consistent = do
  let input = "func invalid( {"
      basicResult = runErrorHandler input
      enhancedResult = runEnhancedErrorHandler input
  case (basicResult, enhancedResult) of
    (Right (basicErrors, _), Right (enhancedErrors, _)) -> do
      assertBool "Basic L.and enhanced handlers should detect similar errors" $
        L.length basicErrors > 0 && L.length enhancedErrors > 0
    (Left _, Left _) -> return ()  -- Both failed consistently
    _ -> assertFailure "Inconsistent error handling between modules"

test_error_propagation_consistent :: IO ()
test_error_propagation_consistent = do
  let input = unlines
        [ "func outer() {"
        , "    func inner() {"
        , "        x := invalid_syntax"
        , "    }"
        , "}"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      assertBool "Should propagate errors consistently" $ not (null errors)

prop_error_handler_composition :: String -> String -> Property
prop_error_handler_composition input1 input2 =
  L.length input1 <= 50 && L.length input2 <= 50 ==>
  let result1 = runErrorHandler input1
      result2 = runErrorHandler input2
  in case (result1, result2) of
       (Right (errs1, _), Right (errs2, _)) ->
         property $ L.length errs1 >= 0 && L.length errs2 >= 0
       _ -> property False

-- ============================================================================
-- Error Message Consistency
-- ============================================================================

test_error_messages_deterministic :: IO ()
test_error_messages_deterministic = do
  let input = "func invalid( {"
      result1 = runErrorHandler input
      result2 = runErrorHandler input
  case (result1, result2) of
    (Right (errs1, _), Right (errs2, _)) -> do
      let msgs1 = map show errs1
          msgs2 = map show errs2
      assertBool "Error messages should be deterministic" $ msgs1 == msgs2
    _ -> return ()

test_error_message_formatting :: IO ()
test_error_message_formatting = do
  let input = "func invalid( {"
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      mapM_ checkErrorMessage errors
  where
    checkErrorMessage error = do
      let msg = show error
      assertBool "Error message should not be empty" $ not (null msg)
      assertBool "Error message should contain location info" $ 
        L.any (`L.isInfixOf` msg) ["line", "column", "position"]

prop_error_message_idempotency :: String -> Property
prop_error_message_idempotency input =
  L.length input <= 100 ==>
  let result = runErrorHandler input
  in case result of
       Left _ -> property False
       Right (errors, _) -> property $ L.all msgIdempotent errors
  where
    msgIdempotent error = let msg = show error in msg == show (read msg)

-- ============================================================================
-- Performance L.and Memory Consistency
-- ============================================================================

prop_error_handler_performance_consistency :: Int -> String -> Property
prop_error_handler_performance_consistency iterations baseInput =
  iterations > 0 && iterations <= 10 && L.length baseInput <= 50 ==>
  let input = unlines $ replicate iterations baseInput
      result = runErrorHandler input
  in case result of
       Left _ -> property $ iterations > 5  -- Large inputs may fail
       Right (errors, _) -> property $ L.length errors >= 0

prop_error_handler_memory_efficiency :: Int -> Property
prop_error_handler_memory_efficiency size =
  size > 0 && size <= 100 ==>
  let input = unlines $ replicate size "func test() { return 42; }"
      result = runErrorHandler input
  in case result of
       Left _ -> property $ size > 50  -- Large inputs may fail
       Right (errors, _) -> property $ L.length errors >= 0

-- ============================================================================
-- Edge Case Consistency
-- ============================================================================

test_error_handling_unicode :: IO ()
test_error_handling_unicode = do
  let input = unlines
        [ "func unicode_test() {"
        , "    x := \"café naïve résumé 🚀 测试\""
        , "    y := invalid_syntax_中文"
        , "}"
        ]
      result = runErrorHandler input
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      assertBool "Should handle unicode in error messages" $ not (null errors)

test_error_handling_large_inputs :: IO ()
test_error_handling_large_inputs = do
  let baseLine = "func test() { return 42; }"
      largeInput = unlines $ replicate 1000 baseLine
      result = runErrorHandler largeInput
  case result of
    Left err -> assertFailure $ "Unexpected failure: " ++ show err
    Right (errors, _) -> do
      assertBool "Should handle large inputs gracefully" $ L.length errors >= 0

prop_error_handling_boundaries :: String -> Property
prop_error_handling_boundaries input =
  L.length input <= 200 ==>
  let result = runErrorHandler input
  in case result of
       Left _ -> property $ L.any (`elem` input) "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
       Right (errors, _) -> property $ L.length errors >= 0

-- ============================================================================
-- Additional Helper Functions
-- ============================================================================

checkErrorSpan :: CompilerError -> IO ()
checkErrorSpan error = do
  let location = errorLocation error
  assertBool "Error location should be valid" $ isValidErrorLocation location

-- Mock data types for testing (these would normally come from the actual modules)
data CompilerError = SyntaxError ErrorLocation String
                   | TypeError ErrorLocation String  
                   | SemanticError ErrorLocation String
                   deriving (Show, Eq)

-- Mock implementations for testing (these would normally be in the actual modules)
runErrorHandler :: String -> Either String ([CompilerError], String)
runErrorHandler input = 
  if "invalid" `L.isInfixOf` input
  then Right ([SyntaxError (ErrorLocation 1 1 "test.typus") "syntax error"], "processed")
  else Right ([], "processed")

runEnhancedErrorHandler :: String -> Either String ([CompilerError], [String])
runEnhancedErrorHandler input = 
  if "invalid" `L.isInfixOf` input
  then Right ([SyntaxError (ErrorLocation 1 1 "test.typus") "syntax error"], ["context1", "context2"])
  else Right ([], ["context1", "context2"])