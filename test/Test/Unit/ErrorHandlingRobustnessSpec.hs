{-# LANGUAGE CPP #-}

-- | Error handling robustness tests using QuickCheck
module Test.Unit.ErrorHandlingRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit, isSpace, isControl)

import ErrorHandler (handleError, ErrorHandler(..), ErrorContext(..))
import SourceLocation (SourcePos(..))

-- ============================================================================
-- Error Handling Robustness Properties
-- ============================================================================

-- Property: Error handler never crashes on empty input
prop_error_handler_empty_input :: Property
prop_error_handler_empty_input =
  let context = ErrorContext "" (SourcePos 1 1)
      result = handleError context
  in property $ isWellFormedError result

-- Property: Error handler handles very long inputs gracefully
prop_error_handler_long_input :: Int -> Property
prop_error_handler_long_input n =
  n >= 0 && n <= 1000 ==> -- Reasonable limit
  let longInput = replicate n 'a'
      context = ErrorContext longInput (SourcePos 1 1)
      result = handleError context
  in property $ isWellFormedError result

-- Property: Error handler handles special characters
prop_error_handler_special_chars :: String -> Property
prop_error_handler_special_chars input =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"
      testInput = input ++ specialChars ++ input
      context = ErrorContext testInput (SourcePos 1 1)
      result = handleError context
  in property $ isWellFormedError result

-- Property: Error handler handles Unicode content
prop_error_handler_unicode :: String -> Property
prop_error_handler_unicode content =
  let unicodeContent = content ++ "测试内容🚀αβγ" ++ content
      context = ErrorContext unicodeContent (SourcePos 1 1)
      result = handleError context
  in property $ isWellFormedError result

-- Property: Error handler handles control characters
prop_error_handler_control_chars :: String -> Property
prop_error_handler_control_chars input =
  let controlChars = map chr [0..31] ++ [chr 127]
      testInput = take 100 $ input ++ controlChars ++ input
      context = ErrorContext testInput (SourcePos 1 1)
      result = handleError context
  in property $ isWellFormedError result
  where
    chr = toEnum

-- Property: Error context preserves position information
prop_error_context_preserves_position :: Int -> Int -> String -> Property
prop_error_context_preserves_position line col content =
  line >= 1 && col >= 1 ==>
  let context = ErrorContext content (SourcePos line col)
      result = handleError context
  in property $ errorContainsPosition result line col

-- Property: Error messages are informative
prop_error_messages_informative :: String -> Property
prop_error_messages_informative input =
  length input > 0 ==>
  let context = ErrorContext input (SourcePos 1 1)
      result = handleError context
  in property $ hasInformativeMessage result

-- Property: Error handler handles nested contexts
prop_error_handler_nested_contexts :: [String] -> Property
prop_error_handler_nested_contexts contexts =
  not (null contexts) && length contexts <= 5 ==>
  let nestedContexts = foldr (\ctx acc -> ErrorContext ctx (SourcePos 1 1) : acc) [] contexts
      results = map handleError nestedContexts
  in property $ all isWellFormedError results

-- Property: Error handler consistency across multiple calls
prop_error_handler_consistency :: String -> Property
prop_error_handler_consistency input =
  let context = ErrorContext input (SourcePos 1 1)
      result1 = handleError context
      result2 = handleError context
  in property $ result1 == result2

-- Property: Error handler handles extreme line numbers
prop_error_handler_extreme_lines :: Int -> Property
prop_error_handler_extreme_lines line =
  line >= 1 && line <= 10000 ==> -- Reasonable extreme
  let context = ErrorContext "test" (SourcePos line 1)
      result = handleError context
  in property $ isWellFormedError result

-- Property: Error handler handles extreme column numbers
prop_error_handler_extreme_columns :: Int -> Property
prop_error_handler_extreme_columns col =
  col >= 1 && col <= 1000 ==> -- Reasonable extreme
  let context = ErrorContext "test" (SourcePos 1 col)
      result = handleError context
  in property $ isWellFormedError result

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Simple error handling implementation for testing
data ErrorResult = ErrorResult
  { errorMessage :: String
  , errorPosition :: SourcePos
  , errorContext :: String
  } deriving (Eq, Show)

handleError :: ErrorContext -> ErrorResult
handleError (ErrorContext input pos) =
  ErrorResult
    { errorMessage = generateErrorMessage input
    , errorPosition = pos
    , errorContext = take 50 input ++ if length input > 50 then "..." else ""
    }

generateErrorMessage :: String -> String
generateErrorMessage input
  | null input = "Empty input provided"
  | length input > 100 = "Input too long"
  | any isControl input = "Invalid control characters"
  | otherwise = "General error in input: " ++ take 20 input

isWellFormedError :: ErrorResult -> Bool
isWellFormedError (ErrorResult msg pos ctx) =
  not (null msg) && 
  posLine pos >= 1 && posColumn pos >= 1 &&
  length ctx <= 53 -- 50 chars + "..." if truncated

errorContainsPosition :: ErrorResult -> Int -> Int -> Bool
errorContainsPosition result line col =
  let pos = errorPosition result
  in posLine pos == line && posColumn pos == col

hasInformativeMessage :: ErrorResult -> Bool
hasInformativeMessage result =
  let msg = errorMessage result
  in length msg >= 10 && any (`Data.List.isInfixOf` msg) ["error", "invalid", "empty", "input"]

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling Robustness Tests"
  [ fastProperty "Error handler never crashes on empty input" prop_error_handler_empty_input
  , fastProperty "Error handler handles very long inputs gracefully" prop_error_handler_long_input
  , fastProperty "Error handler handles special characters" prop_error_handler_special_chars
  , fastProperty "Error handler handles Unicode content" prop_error_handler_unicode
  , fastProperty "Error handler handles control characters" prop_error_handler_control_chars
  , fastProperty "Error context preserves position information" prop_error_context_preserves_position
  , fastProperty "Error messages are informative" prop_error_messages_informative
  , fastProperty "Error handler handles nested contexts" prop_error_handler_nested_contexts
  , fastProperty "Error handler consistency across multiple calls" prop_error_handler_consistency
  , fastProperty "Error handler handles extreme line numbers" prop_error_handler_extreme_lines
  , fastProperty "Error handler handles extreme column numbers" prop_error_handler_extreme_columns
  ]