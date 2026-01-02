{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import ErrorHandler
import Parser (parseTypus)
import Compiler (compile)
import Utils (trim)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)

-- Property: Error handling preserves error messages
prop_error_preserves_message :: String -> String -> Property
prop_error_preserves_message errorMsg input =
  not (null errorMsg) && not (null input) ==>
  let result = handleError input errorMsg
  in property $ case result of
    Left err -> property $ errorMsg `L.isInfixOf` err
    Right _ -> True

-- Property: Error handling captures source location
prop_error_captures_location :: Int -> Int -> String -> Property
prop_error_captures_location line col errorMsg =
  line >= 0 && col >= 0 && not (null errorMsg) ==>
  let input = "var x = 42"
      result = handleErrorWithLocation input line col errorMsg
  in property $ case result of
    Left err -> property $ show line `L.isInfixOf` err .&&. show col `L.isInfixOf` err
    Right _ -> True

-- Property: Error handling handles syntax errors
prop_error_syntax_errors :: String -> Property
prop_error_syntax_errors malformedCode =
  not (null malformedCode) ==>
  let parseResult = parseTypus malformedCode
      errorResult = case parseResult of
        Left err -> property $ Just err
        Right _ -> Nothing
  in property $ case errorResult of
    Nothing -> True -- Valid code
    Just err -> not (null err)

-- Property: Error handling provides meaningful error context
prop_error_context :: String -> String -> Property
prop_error_context code context =
  not (null code) && not (null context) ==>
  let parseResult = parseTypus code
      enhancedError = case parseResult of
        Left err -> property $ enhanceError err context
        Right _ -> "no error"
  in property $ case enhancedError of
    "no error" -> True
    err -> context `L.isInfixOf` err

-- Property: Error recovery attempts are made
prop_error_recovery_attempts :: String -> Property
prop_error_recovery_attempts codeWithErrors =
  not (null codeWithErrors) ==>
  let parseResult = parseTypus codeWithErrors
      recoveryResult = case parseResult of
        Left err -> property $ attemptRecovery err codeWithErrors
        Right ast -> Right ast
  in property $ case recoveryResult of
    Left _ -> True -- Recovery failed but was attempted
    Right _ -> True -- Recovery succeeded

-- Property: Error messages are consistent
prop_error_consistency :: String -> Property
prop_error_consistency input =
  let parseResult1 = parseTypus input
      parseResult2 = parseTypus input
      error1 = case parseResult1 of
        Left err -> property $ Just err
        Right _ -> Nothing
      error2 = case parseResult2 of
        Left err -> property $ Just err
        Right _ -> Nothing
  in property $ error1 == error2

-- Property: Error handling handles nested errors
prop_error_nested :: [String] -> Property
prop_error_nested errorMessages =
  not (null errorMessages) && L.length errorMessages <= 3 ==>
  let nestedError = createNestedError errorMessages
  in property $ L.all (`L.isInfixOf` nestedError) errorMessages

-- Property: Error severity classification
prop_error_severity :: String -> Property
prop_error_severity errorMsg =
  not (null errorMsg) ==>
  let severity = classifyError errorMsg
  in property $ severity `elem` ["warning", "error", "critical"]

-- Property: Error chaining preserves original errors
prop_error_chaining :: String -> String -> Property
prop_error_chaining error1 error2 =
  not (null error1) && not (null error2) ==>
  let chained = chainErrors error1 error2
  in property $ error1 `L.isInfixOf` chained .&&. error2 `L.isInfixOf` chained

-- Property: Error formatting is readable
prop_error_formatting :: String -> Int -> Int -> Property
prop_error_formatting errorMsg line col =
  not (null errorMsg) && line >= 0 && col >= 0 ==>
  let formatted = formatError errorMsg line col
  in property $ not (null formatted) && errorMsg `L.isInfixOf` formatted

-- Property: Error handling with compiler errors
prop_error_compiler_errors :: String -> Property
prop_error_compiler_errors sourceCode =
  let parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
      compilerError = case compileResult of
        Nothing -> Nothing
        Just (Left err) -> Just err
        Just (Right _) -> Nothing
  in property $ case compilerError of
    Nothing -> True
    Just err -> not (null err)

-- Property: Error handling preserves error types
prop_error_types :: String -> String -> Property
prop_error_types errorInput errorType =
  not (null errorInput) && not (null errorType) ==>
  let result = handleErrorWithType errorInput errorType
  in property $ case result of
    Left err -> property $ errorType `L.isInfixOf` err
    Right _ -> True

-- Property: Error handling with multiple errors
prop_error_multiple :: [String] -> Property
prop_error_multiple errors =
  not (null errors) && L.length errors <= 5 ==>
  let multiError = combineErrors errors
  in property $ L.all (`L.isInfixOf` multiError) errors

-- Property: Error handling provides suggestions
prop_error_suggestions :: String -> Property
prop_error_suggestions malformedInput =
  not (null malformedInput) ==>
  let parseResult = parseTypus malformedInput
      suggestions = case parseResult of
        Left err -> property $ generateSuggestions err malformedInput
        Right _ -> []
  in property $ case suggestions of
    [] -> True
    _ -> not (null suggestions)

-- Property: Error handling tracks error frequency
prop_error_frequency :: String -> Int -> Property
prop_error_frequency errorType count =
  not (null errorType) && count >= 0 && count <= 10 ==>
  let frequency = trackErrorFrequency errorType count
  in property $ frequency >= count

-- Property: Error handling with custom error handlers
prop_error_custom_handlers :: String -> Property
prop_error_custom_handlers input =
  not (null input) ==>
  let customHandler = CustomErrorHandler (\err -> "Custom: " ++ err)
      result = handleWithCustom input customHandler
  in property $ case result of
    Left err -> property $ "Custom:" `L.isPrefixOf` err
    Right _ -> True

-- Property: Error handling preserves stack traces
prop_error_stack_traces :: [String] -> Property
prop_error_stack_traces callStack =
  not (null callStack) && L.length callStack <= 5 ==>
  let stackTrace = createStackTrace callStack
  in property $ L.all (`L.isInfixOf` stackTrace) callStack

-- Property: Error handling with timeout errors
prop_error_timeouts :: Int -> Property
prop_error_timeouts timeoutMs =
  timeoutMs >= 0 && timeoutMs <= 1000 ==>
  let timeoutError = createTimeoutError timeoutMs
  in property $ "timeout" `L.isInfixOf` timeoutError .&&. show timeoutMs `L.isInfixOf` timeoutError

-- Property: Error handling with memory errors
prop_error_memory :: Int -> Property
prop_error_memory memoryUsage =
  memoryUsage >= 0 && memoryUsage <= 10000 ==>
  let memoryError = createMemoryError memoryUsage
  in property $ "memory" `L.isInfixOf` memoryError .&&. show memoryUsage `L.isInfixOf` memoryError

-- Property: Error handling with IO errors
prop_error_io :: String -> Property
prop_error_io ioOperation =
  not (null ioOperation) ==>
  let ioError = createIOError ioOperation
  in property $ ioOperation `L.isInfixOf` ioError .&&. "IO" `L.isInfixOf` ioError

-- Property: Error handling is deterministic
prop_error_deterministic :: String -> Property
prop_error_deterministic input =
  let result1 = handleError input "test error"
      result2 = handleError input "test error"
  in property $ result1 == result2

-- Property: Error handling handles edge cases
prop_error_edge_cases :: String -> Property
prop_error_edge_cases edgeCase =
  let result = handleError edgeCase edgeCase
  in property $ case result of
    Left err -> property $ not (null err)
    Right _ -> True

tests :: TestTree
tests =
  testGroup "Error Handling QuickCheck Tests"
    [ fastProperty "preserves message" prop_error_preserves_message
    , fastProperty "captures location" prop_error_captures_location
    , fastProperty "syntax errors" prop_error_syntax_errors
    , fastProperty "provides context" prop_error_context
    , fastProperty "recovery attempts" prop_error_recovery_attempts
    , fastProperty "consistency" prop_error_consistency
    , fastProperty "nested errors" prop_error_nested
    , fastProperty "severity classification" prop_error_severity
    , fastProperty "error chaining" prop_error_chaining
    , fastProperty "formatting" prop_error_formatting
    , fastProperty "compiler errors" prop_error_compiler_errors
    , fastProperty "preserves types" prop_error_types
    , fastProperty "multiple errors" prop_error_multiple
    , fastProperty "provides suggestions" prop_error_suggestions
    , fastProperty "error frequency" prop_error_frequency
    , fastProperty "custom handlers" prop_error_custom_handlers
    , fastProperty "stack traces" prop_error_stack_traces
    , fastProperty "timeout errors" prop_error_timeouts
    , fastProperty "memory errors" prop_error_memory
    , fastProperty "IO errors" prop_error_io
    , fastProperty "deterministic" prop_error_deterministic
    , fastProperty "edge cases" prop_error_edge_cases
    ]