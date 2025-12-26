{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  , mkSourcePos
  , mkSourceSpan
  , spanContains
  , spanUnion
  , spanLength
  )

import ErrorHandler
  ( ErrorHandler(..)
  , ErrorContext(..)
  , ErrorSeverity(..)
  , ErrorMessage(..)
  , defaultErrorHandler
  , handleError
  , formatError
  , collectErrors
  )

import Utils
  ( trim
  , splitBy
  , removeComments
  , normalizeIndentation
  )

import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Property: spanUnion creates a span that contains both input spans
prop_span_union_contains_both :: SourceSpan -> SourceSpan -> Property
prop_span_union_contains_both span1 span2 =
  let union = spanUnion span1 span2
      contains1 = spanContains union span1
      contains2 = spanContains union span2
  in property $ contains1 .&&. contains2

-- Property: spanLength is non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let length = spanLength span
  in property $ length >= 0

-- Property: spanContains is reflexive
prop_span_contains_reflexive :: SourceSpan -> Property
prop_span_contains_reflexive span =
  property $ spanContains span span

-- Unit test: spanContains works for nested spans
test_span_contains_nested :: TestTree
test_span_contains_nested = testCase "spanContains works for nested spans" $ do
  let outer = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 5 10)
      inner = mkSourceSpan (mkSourcePos 2 3) (mkSourcePos 4 8)
      outside = mkSourceSpan (mkSourcePos 6 1) (mkSourcePos 7 1)
  assertBool "outer should contain inner" $ spanContains outer inner
  assertBool "outer should not contain outside" $ not $ spanContains outer outside

-- Unit test: spanUnion works for adjacent spans
test_span_union_adjacent :: TestTree
test_span_union_adjacent = testCase "spanUnion works for adjacent spans" $ do
  let span1 = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 5)
      span2 = mkSourceSpan (mkSourcePos 1 6) (mkSourcePos 1 10)
      union = spanUnion span1 span2
      expected = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 10)
  union @?= expected

-- ============================================================================
-- ErrorHandler Tests
-- ============================================================================

-- Property: error messages are non-empty when context is provided
prop_error_message_non_empty :: ErrorContext -> String -> Property
prop_error_message_non_empty context msg =
  not (null msg) ==> null (formatError context msg) /= True

-- Unit test: defaultErrorHandler collects errors correctly
test_default_error_handler :: TestTree
test_default_error_handler = testCase "defaultErrorHandler collects errors correctly" $ do
  let handler = defaultErrorHandler
      context = ErrorContext "test" ErrorError
      errorMsg = "Test error message"
      result = handleError handler context errorMsg
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right (handler', errors) -> do
      length errors @?= 1
      let (ctx, msg) = head errors
      ctx @?= context
      msg @?= errorMsg

-- Unit test: collectErrors aggregates multiple errors
test_collect_errors :: TestTree
test_collect_errors = testCase "collectErrors aggregates multiple errors" $ do
  let errors = 
        [ (ErrorContext "test1" ErrorWarning, "Warning message")
        , (ErrorContext "test2" ErrorError, "Error message")
        , (ErrorContext "test3" ErrorInfo, "Info message")
        ]
      collected = collectErrors errors
  length collected @?= 3
  -- Check that errors are sorted by severity
  let severities = map (\(ctx, _) -> ecSeverity ctx) collected
  severities @?= [ErrorError, ErrorWarning, ErrorInfo]

-- ============================================================================
-- Utils Integration Tests
-- ============================================================================

-- Property: trim and removeComments commute on comment-free code
prop_trim_remove_comments_commute :: String -> Property
prop_trim_remove_comments_commute code =
  let noComments = not (isInfixOf "//" code || isInfixOf "/*" code)
  in noComments ==> 
     let trimThenComments = trim (removeComments code)
         commentsThenTrim = removeComments (trim code)
     in trimThenComments === commentsThenTrim

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative code =
  let normalized = normalizeIndentation code
      lines1 = lines code
      lines2 = lines normalized
      -- Check that non-empty lines maintain their relative indentation
      nonEmpty1 = filter (not . null) lines1
      nonEmpty2 = filter (not . null) lines2
  in length nonEmpty1 == length nonEmpty2 ==>
     let indentDiff1 = zipWith (\l1 l2 -> length (takeWhile isSpace l1) - length (takeWhile isSpace l2)) nonEmpty1 (tail nonEmpty1)
         indentDiff2 = zipWith (\l1 l2 -> length (takeWhile isSpace l1) - length (takeWhile isSpace l2)) nonEmpty2 (tail nonEmpty2)
     in indentDiff1 === indentDiff2
  where
    isSpace = Data.Char.isSpace

-- Unit test: removeComments handles nested comments correctly
test_remove_comments_nested :: TestTree
test_remove_comments_nested = testCase "removeComments handles nested comments correctly" $ do
  let input = "code /* outer /* inner */ still outer */ more code"
      expected = "code  more code"
      result = removeComments input
  result @?= expected

-- Unit test: normalizeIndentation handles mixed tabs and spaces
test_normalize_mixed_indentation :: TestTree
test_normalize_mixed_indentation = testCase "normalizeIndentation handles mixed tabs and spaces" $ do
  let input = unlines
        [ "\tline1"
        , "    line2"
        , "\t    line3"
        , "line4"
        ]
      result = normalizeIndentation input
      resultLines = lines result
  -- Check that all lines start without tabs (normalized to spaces)
  all (not . isPrefixOf "\t") resultLines @?= True
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Unit test: SourceLocation integration with ErrorHandler
test_sourcelocation_error_integration :: TestTree
test_sourcelocation_error_integration = testCase "SourceLocation integration with ErrorHandler" $ do
  let span = mkSourceSpan (mkSourcePos 10 5) (mkSourcePos 10 15)
      located = locatedWithSpan span "test variable"
      context = ErrorContext "parsing" ErrorError
      handler = defaultErrorHandler
      errorMsg = "Undefined variable: " ++ locatedValue located
  case handleError handler context errorMsg of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right (handler', errors) -> do
      length errors @?= 1
      let (ctx, msg) = head errors
      ctx @?= context
      assertBool "Error message should contain variable name" $ 
        locatedValue located `isInfixOf` msg

-- Property: splitBy and trim work together predictably
prop_splitby_trim_integration :: Char -> String -> Property
prop_splitby_trim_integration delim str =
  let split = splitBy delim str
      trimmed = map trim split
      -- After trimming, no segment should start or end with whitespace
      noLeading = all (\s -> null s || not (isSpace (head s))) trimmed
      noTrailing = all (\s -> null s || not (isSpace (last s))) trimmed
  in property $ noLeading .&&. noTrailing
  where
    isSpace = Data.Char.isSpace

-- Unit test: end-to-end error handling with source locations
test_end_to_end_error_handling :: TestTree
test_end_to_end_error_handling = testCase "end-to-end error handling with source locations" $ do
  let code = unlines
        [ "func main() {"
        , "    x := undefined_var"
        , "    return x"
        , "}"
        ]
      span = mkSourceSpan (mkSourcePos 2 5) (mkSourcePos 2 19)
      context = ErrorContext "semantic" ErrorError
      errorMsg = "Use of undefined variable"
  -- Simulate error handling pipeline
  let handler = defaultErrorHandler
      locatedError = locatedWithSpan span errorMsg
  case handleError handler context (locatedValue locatedError) of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right (handler', errors) -> do
      length errors @?= 1
      let formatted = formatError context (locatedValue locatedError)
      assertBool "Formatted error should contain context" $ 
        "semantic" `isInfixOf` formatted
      assertBool "Formatted error should contain message" $ 
        errorMsg `isInfixOf` formatted

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Integration Tests"
  [ testGroup "SourceLocation Properties"
    [ fastProperty "spanUnion contains both input spans" prop_span_union_contains_both
    , fastProperty "spanLength is non-negative" prop_span_length_non_negative
    , fastProperty "spanContains is reflexive" prop_span_contains_reflexive
    , test_span_contains_nested
    , test_span_union_adjacent
    ]
  
  , testGroup "ErrorHandler Properties"
    [ fastProperty "error messages are non-empty when context is provided" prop_error_message_non_empty
    , test_default_error_handler
    , test_collect_errors
    ]
  
  , testGroup "Utils Integration Properties"
    [ fastProperty "trim and removeComments commute on comment-free code" prop_trim_remove_comments_commute
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_preserves_relative
    , test_remove_comments_nested
    , test_normalize_mixed_indentation
    ]
  
  , testGroup "Integration Tests"
    [ test_sourcelocation_error_integration
    , fastProperty "splitBy and trim work together predictably" prop_splitby_trim_integration
    , test_end_to_end_error_handling
    ]
  ]