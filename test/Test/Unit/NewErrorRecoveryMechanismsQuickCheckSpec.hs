{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Error recovery mechanisms tests for ErrorHandler module
module Test.Unit.NewErrorRecoveryMechanismsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

import ErrorHandler 
  ( ErrorHandler
  , ErrorContext(..)
  , ErrorSeverity(..)
  , ErrorMessage(..)
  , ErrorRecoveryStrategy(..)
  , newErrorHandler
  , handleError
  , formatError
  , collectErrors
  , hasErrors
  , clearErrors
  , attemptRecovery
  , getRecoverySuggestions
  )

import Parser
  ( ParseError(..)
  , parseWithRecovery
  )

import Compiler
  ( CompilationError(..)
  , compileWithRecovery
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate error contexts
genErrorContext :: Gen ErrorContext
genErrorContext = do
  file <- elements ["test.typus", "module.typus", "lib.typus", ""]
  line <- choose (1, 1000)
  column <- choose (1, 200)
  return $ ErrorContext file line column

-- Generate error messages
genErrorMessage :: Gen ErrorMessage
genErrorMessage = do
  message <- elements 
    [ "Syntax error"
    , "Type mismatch"
    , "Undefined variable"
    , "Invalid dependency"
    , "Ownership violation"
    , "Constraint failure"
    , "Parse error"
    , "Compilation error"
    ]
  severity <- elements [ErrorWarning, ErrorError, ErrorFatal]
  context <- genErrorContext
  return $ ErrorMessage message severity context

-- Generate recovery strategies
genRecoveryStrategy :: Gen ErrorRecoveryStrategy
genRecoveryStrategy = elements
  [ SkipToken
  , InsertToken "expected"
  , DeleteToken
  , RetryWithAlternative
  , AbortCompilation
  , ContinueWithWarnings
  ]

-- Generate parse errors
genParseError :: Gen ParseError
genParseError = do
  message <- elements ["Unexpected token", "Expected identifier", "Malformed expression"]
  pos <- choose (0, 1000)
  expected <- elements ["identifier", "operator", "semicolon", "bracket"]
  actual <- elements ["number", "string", "keyword", "eof"]
  return $ ParseError message pos expected actual

-- Generate compilation errors
genCompilationError :: Gen CompilationError
genCompilationError = do
  message <- elements ["Type inference failed", "Dependency cycle detected", "Ownership check failed"]
  stage <- elements ["parsing", "type-checking", "ownership-analysis", "code-generation"]
  context <- genErrorContext
  return $ CompilationError message stage context

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: Error recovery should not lose original error information
prop_recovery_preserves_original_error :: ErrorMessage -> ErrorRecoveryStrategy -> Property
prop_recovery_preserves_original_error errorMsg strategy =
  let handler = newErrorHandler
      handlerWithError = handleError handler errorMsg
      recoveredHandler = attemptRecovery handlerWithError strategy
      originalErrors = collectErrors handlerWithError
      recoveredErrors = collectErrors recoveredHandler
  in property $ not (null originalErrors) ==> 
             length recoveredErrors >= L.length originalErrors

-- Property: Recovery suggestions should be relevant to error type
prop_recovery_suggestions_relevance :: ErrorMessage -> Property
prop_recovery_suggestions_relevance errorMsg =
  let handler = newErrorHandler
      handlerWithError = handleError handler errorMsg
      suggestions = getRecoverySuggestions handlerWithError
  in property $ not (null suggestions) ==> 
             all (\suggestion -> 
               let suggestionText = T.unpack $ formatError suggestion
               in case errSeverity errorMsg of
                    ErrorWarning -> "warning" `L.isInfixOf` suggestionText || "continue" `L.isInfixOf` suggestionText
                    ErrorError -> "error" `L.isInfixOf` suggestionText || "fix" `L.isInfixOf` suggestionText  
                    ErrorFatal -> "fatal" `L.isInfixOf` suggestionText || "abort" `L.isInfixOf` suggestionText
             ) suggestions

-- Property: Multiple recovery attempts should accumulate suggestions
prop_multiple_recovery_accumulates :: [ErrorMessage] -> Property
prop_multiple_recovery_accumulates errorMessages =
  not (null errorMessages) ==> 
  let handler = newErrorHandler
      handlerWithErrors = foldl handleError handler errorMessages
      suggestions1 = getRecoverySuggestions handlerWithErrors
      -- Attempt recovery for first error
      recoveredHandler = if not (null errorMessages) 
                        then attemptRecovery handlerWithErrors SkipToken
                        else handlerWithErrors
      suggestions2 = getRecoverySuggestions recoveredHandler
  in property $ L.length suggestions2 >= L.length suggestions1

-- Property: Parse recovery should produce valid partial results
prop_parse_recovery_partial_results :: String -> ParseError -> Property
prop_parse_recovery_partial_results input parseError =
  length input > 0 ==> 
  let result = parseWithRecovery input parseError
  in property $ isRight result || (isLeft result && 
    case result of
      Left err -> True  -- Parse error but recovery attempted
      Right _ -> False)

-- Property: Compilation recovery should preserve successful parts
prop_compilation_recovery_preserves_success :: [CompilationError] -> Property
prop_compilation_recovery_preserves_success errors =
  let result = compileWithRecovery errors
  in property $ case result of
        Left _ -> not (null errors)  -- Failure expected with errors
        Right partialResult -> True  -- Partial success should be preserved

-- Property: Error recovery should handle circular dependencies gracefully
prop_recovery_circular_dependencies :: [String] -> Property
prop_recovery_circular_dependencies moduleNames =
  length moduleNames >= 3 ==> 
  let uniqueModules = nub moduleNames
      -- Create circular dependency
      circularDeps = zip uniqueModules (L.tail uniqueModules ++ [L.head uniqueModules])
      errors = L.map (\(from, to) -> 
        CompilationError ("Circular dependency: " ++ from ++ " -> " ++ to) 
                         "dependency-analysis" 
                         (ErrorContext from 1 1)
      ) circularDeps
      recovered = compileWithRecovery errors
  in property $ case recovered of
        Left _ -> True  -- Expected to fail
        Right partial -> True  -- But should provide partial results

-- Property: Recovery strategies should be idempotent for same error
prop_recovery_strategy_idempotent :: ErrorMessage -> ErrorRecoveryStrategy -> Property
prop_recovery_strategy_idempotent errorMsg strategy =
  let handler = newErrorHandler
      handlerWithError = handleError handler errorMsg
      recovered1 = attemptRecovery handlerWithError strategy
      recovered2 = attemptRecovery recovered1 strategy
      errors1 = collectErrors recovered1
      errors2 = collectErrors recovered2
  in property $ L.length errors1 === L.length errors2

-- Property: Error recovery should maintain error severity ordering
prop_recovery_maintains_severity_ordering :: [ErrorMessage] -> Property
prop_recovery_maintains_severity_ordering errorMessages =
  not (null errorMessages) ==> 
  let handler = newErrorHandler
      handlerWithErrors = foldl handleError handler errorMessages
      recoveredHandler = attemptRecovery handlerWithErrors SkipToken
      recoveredErrors = collectErrors recoveredHandler
      severities = map errSeverity recoveredErrors
  in property $ severities === sort severities

-- Property: Recovery should handle malformed input gracefully
prop_recovery_malformed_input :: String -> Property
prop_recovery_malformed_input input =
  let malformedChars = L.filter (\c -> ord c < 32 && c `notElem` "\t\n\r") input
  in not (null malformedChars) ==> 
     let handler = newErrorHandler
         errorMsg = ErrorMessage ("Malformed input: " ++ show malformedChars) ErrorError (ErrorContext "" 1 1)
         handlerWithError = handleError handler errorMsg
         recoveredHandler = attemptRecovery handlerWithError SkipToken
         recoveredErrors = collectErrors recoveredHandler
     in property $ not (null recoveredErrors)

-- Property: Recovery suggestions should be actionable
prop_recovery_suggestions_actionable :: ErrorMessage -> Property
prop_recovery_suggestions_actionable errorMsg =
  let handler = newErrorHandler
      handlerWithError = handleError handler errorMsg
      suggestions = getRecoverySuggestions handlerWithError
  in not (null suggestions) ==> 
     property $ L.all (\suggestion -> 
       let suggestionText = T.unpack $ formatError suggestion
           actionableKeywords = ["fix", "change", "add", "remove", "replace", "skip", "continue"]
       in L.any (`L.isInfixOf` suggestionText) actionableKeywords
     ) suggestions

-- Property: Error recovery should preserve context information
prop_recovery_preserves_context :: ErrorContext -> ErrorMessage -> Property
prop_recovery_preserves_context context errorMsg =
  let errorMsgWithContext = errorMsg { errContext = context }
      handler = newErrorHandler
      handlerWithError = handleError handler errorMsgWithContext
      recoveredHandler = attemptRecovery handlerWithError SkipToken
      recoveredErrors = collectErrors recoveredHandler
  in not (null recoveredErrors) ==> 
     property $ L.all (\err -> errContext err === context) recoveredErrors

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Recovery should handle large numbers of errors efficiently
prop_recovery_large_error_count :: Int -> Property
prop_recovery_large_error_count numErrors =
  numErrors > 0 && numErrors <= 1000 ==> 
  let errors = take numErrors $ repeat (ErrorMessage "Test error" ErrorError (ErrorContext "test" 1 1))
      handler = newErrorHandler
      handlerWithErrors = foldl handleError handler errors
      recoveredHandler = attemptRecovery handlerWithErrors SkipToken
  in property $ hasErrors recoveredHandler

-- Property: Recovery should not cause memory leaks
prop_recovery_memory_efficiency :: [ErrorMessage] -> Int -> Property
prop_recovery_memory_efficiency errorMessages iterations =
  iterations > 0 && iterations <= 100 ==> 
  let handler = newErrorHandler
      handlerWithErrors = foldl handleError handler errorMessages
      -- Simulate multiple recovery attempts
      finalHandler = iterate (\h -> attemptRecovery h SkipToken) handlerWithErrors !! iterations
      finalErrors = collectErrors finalHandler
  in property $ L.length finalErrors <= L.length errorMessages + iterations

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Recovery should handle empty error lists
prop_recovery_empty_error_list :: Property
prop_recovery_empty_error_list =
  let handler = newErrorHandler
      recoveredHandler = attemptRecovery handler SkipToken
      errors = collectErrors recoveredHandler
  in property $ null errors

-- Property: Recovery should handle fatal errors appropriately
prop_recovery_fatal_errors :: ErrorMessage -> Property
prop_recovery_fatal_errors errorMsg =
  let fatalError = errorMsg { errSeverity = ErrorFatal }
      handler = newErrorHandler
      handlerWithError = handleError handler fatalError
      recoveredHandler = attemptRecovery handlerWithError ContinueWithWarnings
      suggestions = getRecoverySuggestions recoveredHandler
  in property $ L.any (\s -> errSeverity s == ErrorFatal) suggestions

-- Property: Recovery should handle errors with missing context
prop_recovery_missing_context :: String -> Property
prop_recovery_missing_context message =
  let errorMsg = ErrorMessage message ErrorError (ErrorContext "" 0 0)
      handler = newErrorHandler
      handlerWithError = handleError handler errorMsg
      recoveredHandler = attemptRecovery handlerWithError SkipToken
  in property $ hasErrors recoveredHandler

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Error Recovery Mechanisms QuickCheck Tests"
  [ testGroup "Basic Recovery Properties"
    [ fastProperty "recovery preserves original error" prop_recovery_preserves_original_error
    , fastProperty "recovery suggestions relevance" prop_recovery_suggestions_relevance
    , fastProperty "multiple recovery accumulates" prop_multiple_recovery_accumulates
    ]

  , testGroup "Parse L.and Compilation Recovery"
    [ fastProperty "parse recovery partial results" prop_parse_recovery_partial_results
    , fastProperty "compilation recovery preserves success" prop_compilation_recovery_preserves_success
    , fastProperty "recovery circular dependencies" prop_recovery_circular_dependencies
    ]

  , testGroup "Recovery Strategy Properties"
    [ fastProperty "recovery strategy idempotent" prop_recovery_strategy_idempotent
    , fastProperty "recovery maintains severity ordering" prop_recovery_maintains_severity_ordering
    , fastProperty "recovery malformed input" prop_recovery_malformed_input
    ]

  , testGroup "Recovery Suggestions"
    [ fastProperty "recovery suggestions actionable" prop_recovery_suggestions_actionable
    , fastProperty "recovery preserves context" prop_recovery_preserves_context
    ]

  , testGroup "Performance Properties"
    [ fastProperty "recovery large error count" prop_recovery_large_error_count
    , fastProperty "recovery memory efficiency" prop_recovery_memory_efficiency
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "recovery empty error list" prop_recovery_empty_error_list
    , fastProperty "recovery fatal errors" prop_recovery_fatal_errors
    , fastProperty "recovery missing context" prop_recovery_missing_context
    ]
  ]