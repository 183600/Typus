{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
  )

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T

-- Test error collector basic functionality
test_error_collector_basic :: TestTree
test_error_collector_basic = testCase "Error collector basic functionality" $ do
  collector <- newErrorCollector
  hasErrors collector @?= False
  hasWarnings collector @?= False
  
  addError collector "Test error"
  hasErrors collector @?= True
  hasWarnings collector @?= False
  
  addWarning collector "Test warning"
  hasErrors collector @?= True
  hasWarnings collector @?= True
  
  errors <- getErrors collector
  warnings <- getWarnings collector
  length errors @?= 1
  length warnings @?= 1

-- Test error formatting consistency
test_error_formatting_consistency :: TestTree
test_error_formatting_consistency = testCase "Error formatting is consistent" $ do
  let errorMsg = "Test error message"
      error = TypeError errorMsg SyntaxError emptyContext
      formatted = formatError error
  assertBool "Formatted error contains original message" $ 
    isInfixOf errorMsg formatted
  assertBool "Formatted error contains severity info" $ 
    isInfixOf "error" (map toLower formatted)

-- Test error location handling
test_error_location_handling :: TestTree
test_error_location_handling = testCase "Error location handling" $ do
  let pos = SourcePos 5 10 50
      span = SourceSpan pos pos
      location = ErrorLocation span
      error = errorAt location "Location test"
      formatted = formatErrorWithLocation error
  assertBool "Formatted error contains line info" $ 
    isInfixOf "5" formatted
  assertBool "Formatted error contains column info" $ 
    isInfixOf "10" formatted

-- Test error categorization
test_error_categorization :: TestTree
test_error_categorization = testCase "Error categorization" $ do
  let syntaxError = errorWithCategory SyntaxError "Syntax error"
      typeError = errorWithCategory TypeError "Type error"
      runtimeError = errorWithCategory RuntimeError "Runtime error"
  
  assertBool "Syntax error has correct category" $ 
    case syntaxError of
      TypeError _ SyntaxError _ -> True
      _ -> False
  
  assertBool "Type error has correct category" $ 
    case typeError of
      TypeError _ TypeError _ -> True
      _ -> False
  
  assertBool "Runtime error has correct category" $ 
    case runtimeError of
      TypeError _ RuntimeError _ -> True
      _ -> False

-- Test error recovery mechanisms
test_error_recovery :: TestTree
test_error_recovery = testCase "Error recovery mechanisms" $ do
  let syntaxError = TypeError "Syntax error" SyntaxError emptyContext
      typeError = TypeError "Type error" TypeError emptyContext
      runtimeError = TypeError "Runtime error" RuntimeError emptyContext
      fatalError = TypeError "Fatal error" FatalError emptyContext
  
  canRecoverFrom syntaxError @?= True
  canRecoverFrom typeError @?= True
  canRecoverFrom runtimeError @?= True
  canRecoverFrom fatalError @?= False
  
  shouldContinueAfter syntaxError @?= True
  shouldContinueAfter typeError @?= True
  shouldContinueAfter runtimeError @?= False
  shouldContinueAfter fatalError @?= False

-- Test warning and info handling
test_warning_info_handling :: TestTree
test_warning_info_handling = testCase "Warning and info handling" $ do
  collector <- newErrorCollector
  
  addWarning collector "Test warning"
  addInfo collector "Test info"
  
  warnings <- getWarnings collector
  info <- getInfo collector
  
  length warnings @?= 1
  length info @?= 1
  
  allMessages <- getAllMessages collector
  length allMessages @?= 2

-- Test combined error handling
test_combined_errors :: TestTree
test_combined_errors = testCase "Combined error handling" $ do
  let error1 = TypeError "First error" SyntaxError emptyContext
      error2 = TypeError "Second error" TypeError emptyContext
      combined = CombinedError [error1, error2]
  
  case combined of
    CombinedError errors -> length errors @?= 2
    _ -> assertFailure "Expected CombinedError"

-- Test error context management
test_error_context :: TestTree
test_error_context = testCase "Error context management" $ do
  let context = emptyContext
      error = TypeError "Context test" SyntaxError context
  
  case error of
    TypeError _ _ ctx -> ctx @?= emptyContext
    _ -> assertFailure "Expected TypeError with context"

-- Test error severity levels
test_error_severity :: TestTree
test_error_severity = testCase "Error severity levels" $ do
  collector <- newErrorCollector
  
  addError collector "Error message"
  addWarning collector "Warning message"
  addInfo collector "Info message"
  
  errors <- getErrors collector
  warnings <- getWarnings collector
  info <- getInfo collector
  
  -- Verify that errors, warnings, and info are properly categorized
  length errors @?= 1
  length warnings @?= 1
  length info @?= 1

-- Test error message formatting with special characters
test_special_characters_in_errors :: TestTree
test_special_characters_in_errors = testCase "Special characters in error messages" $ do
  let messages = 
        [ "Error with \"quotes\""
        , "Error with 'apostrophes'"
        , "Error with\nnewlines"
        , "Error with\ttabs"
        , "Error with Unicode: 测试"
        , "Error with emoji: 🚀"
        ]
  
  mapM_ (\msg -> do
    let error = TypeError msg SyntaxError emptyContext
        formatted = formatError error
    assertBool ("Formatted error contains original message: " ++ msg) $ 
      isInfixOf msg formatted
    ) messages

-- Property: Error collection is deterministic
prop_error_collection_deterministic :: [String] -> [String] -> [String] -> Property
prop_error_collection_deterministic errors warnings infos = 
  let collector1 <- newErrorCollector
      collector2 <- newErrorCollector
      _ <- mapM_ (addError collector1) errors
      _ <- mapM_ (addWarning collector1) warnings
      _ <- mapM_ (addInfo collector1) infos
      _ <- mapM_ (addError collector2) errors
      _ <- mapM_ (addWarning collector2) warnings
      _ <- mapM_ (addInfo collector2) infos
      errors1 <- getErrors collector1
      warnings1 <- getWarnings collector1
      info1 <- getInfo collector1
      errors2 <- getErrors collector2
      warnings2 <- getWarnings collector2
      info2 <- getInfo collector2
  in errors1 === errors2 .&&. warnings1 === warnings2 .&&. info1 === info2

-- Property: Error formatting preserves essential information
prop_error_formatting_preserves_info :: String -> ErrorCategory -> Property
prop_error_formatting_preserves_info msg category = 
  let error = TypeError msg category emptyContext
      formatted = formatError error
  in property $ isInfixOf msg formatted .&&. 
             isInfixOf (show category) formatted

-- Property: Error recovery is consistent with severity
prop_recovery_consistent_with_severity :: ErrorCategory -> Property
prop_recovery_consistent_with_severity category = 
  let error = TypeError "Test" category emptyContext
      canRecover = canRecoverFrom error
      shouldContinue = shouldContinueAfter error
  in case category of
    FatalError -> not canRecover .&&. not shouldContinue
    RuntimeError -> canRecover .&&. not shouldContinue
    _ -> canRecover .&&. shouldContinue

-- Property: Error collector state is consistent
prop_collector_state_consistent :: [String] -> [String] -> Property
prop_collector_state_consistent errors warnings = 
  let collector <- newErrorCollector
      _ <- mapM_ (addError collector) errors
      _ <- mapM_ (addWarning collector) warnings
      hasErrs <- hasErrors collector
      hasWarns <- hasWarnings collector
      errs <- getErrors collector
      warns <- getWarnings collector
  in hasErrs === not (null errors) .&&.
     hasWarns === not (null warnings) .&&.
     length errs === length errors .&&.
     length warns === length warnings

-- Property: Error location information is preserved
prop_location_preserved :: SourcePos -> SourcePos -> String -> Property
prop_location_preserved start end msg = 
  let span = SourceSpan start end
      location = ErrorLocation span
      error = errorAt location msg
      formatted = formatErrorWithLocation error
  in property $ isInfixOf (show (posLine start)) formatted .&&.
             isInfixOf (show (posColumn start)) formatted .&&.
             isInfixOf msg formatted

-- Property: Combined errors preserve individual errors
prop_combined_errors_preserve_individual :: [String] -> Property
prop_combined_errors_preserve_individual msgs = 
  let errors = map (\msg -> TypeError msg SyntaxError emptyContext) msgs
      combined = CombinedError errors
  in case combined of
    CombinedError errs -> length errs === length msgs
    _ -> property False

-- Property: Error context can be extended safely
prop_context_extension_safe :: String -> Property
prop_context_extension_safe msg = 
  let baseContext = emptyContext
      error = TypeError msg SyntaxError baseContext
  in case error of
    TypeError _ _ ctx -> ctx === baseContext
    _ -> property False

tests :: TestTree
tests = testGroup "New Error Handler Consistency Tests"
  [ test_error_collector_basic
  , test_error_formatting_consistency
  , test_error_location_handling
  , test_error_categorization
  , test_error_recovery
  , test_warning_info_handling
  , test_combined_errors
  , test_error_context
  , test_error_severity
  , test_special_characters_in_errors
  , fastProperty "Error collection is deterministic" prop_error_collection_deterministic
  , fastProperty "Error formatting preserves essential information" prop_error_formatting_preserves_info
  , fastProperty "Error recovery is consistent with severity" prop_recovery_consistent_with_severity
  , fastProperty "Error collector state is consistent" prop_collector_state_consistent
  , fastProperty "Error location information is preserved" prop_location_preserved
  , fastProperty "Combined errors preserve individual errors" prop_combined_errors_preserve_individual
  , fastProperty "Error context can be extended safely" prop_context_extension_safe
  ]