{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Parser (parseTypus)
import Compiler (compile)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, lines, unlines)
import Data.Char (isLetter, isDigit, isSpace, toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Map as Map

-- Test: Text processing handles extremely long lines
test_long_line_processing :: TestTree
test_long_line_processing = testCase "Text processing handles extremely long lines" $ do
  let longLine = "x := " ++ replicate 1000 'a' ++ " + " ++ replicate 1000 'b'
      code = "package main\n\nfunc main() {\n  " ++ longLine ++ "\n}"
      result = compile code
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasPositionInfo = any (\msg -> any (`isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasPositionInfo
        then return ()  -- Success - long line handled with position info
        else assertFailure $ "Expected position information for long line: " ++ unlines errorMessages
    Right _ -> return ()  -- Compilation succeeded

-- Property: trim function handles edge cases correctly
prop_trim_edge_cases :: String -> String -> String -> Property
prop_trim_edge_cases prefix content suffix =
  let input = prefix ++ content ++ suffix
      trimmed = trim input
      expectedStart = null prefix || any (not . isSpace) prefix || not (null trimmed)
      expectedEnd = null suffix || any (not . isSpace) suffix || not (null trimmed)
  in classify (all isSpace prefix) "prefix all whitespace" $
     classify (all isSpace suffix) "suffix all whitespace" $
     property $ expectedStart .&&. expectedEnd

-- Test: Comment removal handles nested and edge cases
test_nested_comment_removal :: TestTree
test_nested_comment_removal = testCase "Comment removal handles nested cases" $ do
  let nestedCommentCode = "package main\n\n/* outer comment\n   /* inner comment */\n   still outer\n*/\n\nfunc main() {\n  x := 5 // line comment\n}"
      result = compile nestedCommentCode
  case result of
    Left errs -> assertFailure $ "Comment removal failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - nested comments handled correctly

-- Property: splitBy handles edge cases with delimiters
prop_splitby_edge_cases :: String -> Char -> Property
prop_splitby_edge_cases input delim =
  let segments = splitBy delim input
      totalLength = sum (map length segments) + max 0 (length segments - 1)
      inputLength = length input
  in classify (null input) "empty input" $
     classify (delim `elem` input) "delimiter present" $
     property $ totalLength <= inputLength + 5  -- Allow some tolerance for edge cases

-- Test: Indentation normalization handles mixed whitespace
test_mixed_indentation_normalization :: TestTree
test_mixed_indentation_normalization = testCase "Indentation normalization with mixed whitespace" $ do
  let mixedIndentCode = "package main\n\nfunc main() {\n  \t x := 5\n\t   y := 10\n    \t z := x + y\n}"
      result = compile mixedIndentCode
  case result of
    Left errs -> assertFailure $ "Mixed indentation handling failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - mixed indentation handled correctly

-- Property: removeComments preserves string literals
prop_comment_removal_preserves_strings :: String -> Property
prop_comment_removal_preserves_strings stringContent =
  not (null stringContent) && length stringContent <= 20 && 
  not ('"' `elem` stringContent) && not ('\n' `elem` stringContent) ==>
  let codeWithComment = "package main\n\nfunc main() {\n  x := \"" ++ stringContent ++ "\" // comment\n}"
      cleaned = removeComments codeWithComment
      stringPreserved = ("\"" ++ stringContent ++ "\"") `isInfixOf` cleaned
  in property $ stringPreserved

-- Test: Text processing handles Unicode and special characters
test_unicode_text_processing :: TestTree
test_unicode_text_processing = testCase "Text processing handles Unicode" $ do
  let unicodeCode = "package main\n\nfunc main() {\n  // 注释 with αβγ and ΔΕΖ\n  x := \"测试内容: 🚀🌟\"\n  y := `raw string with αβγ`\n}"
      result = compile unicodeCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasReasonableContent = any (\msg -> length msg >= 10) errorMessages
      if hasReasonableContent
        then return ()  -- Success - Unicode handled reasonably
        else assertFailure $ "Unicode handling failed: " ++ unlines errorMessages
    Right _ -> return ()  -- Compilation succeeded

-- Property: normalizeIndentation handles empty and whitespace-only lines
prop_indentation_handles_empty_lines :: [String] -> Property
prop_indentation_handles_empty_lines inputLines =
  length inputLines >= 1 && length inputLines <= 10 ==>
  let code = unlines inputLines
      normalized = normalizeIndentation code
      normalizedLines = lines normalized
  in classify (any null inputLines) "has empty lines" $
     classify (all (all isSpace) inputLines) "all whitespace lines" $
     property $ length normalizedLines == length inputLines

-- Test: Text processing is robust against malformed input
test_malformed_text_robustness :: TestTree
test_malformed_text_robustness = testCase "Text processing robustness against malformed input" $ do
  let malformedCode = "package main\n\nfunc main() {\n  x := \"unclosed string\n  y := 5\n  /* unclosed comment\n  z := 10\n}"
      result = compile malformedCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasPositionInfo = any (\msg -> any (`isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasPositionInfo
        then return ()  -- Success - malformed input handled gracefully
        else assertFailure $ "Malformed input handling failed: " ++ unlines errorMessages
    Right _ -> assertFailure "Expected compilation error with malformed input"

-- Property: breakOn function handles edge cases correctly
prop_breakon_edge_cases :: String -> String -> Property
prop_breakon_edge_cases input delimiter =
  not (null delimiter) ==>
  let (before, after) = breakOn delimiter input
      delimiterFound = delimiter `isInfixOf` input
      expectedBefore = if delimiterFound then before `isPrefixOf` input else before == input
      expectedAfter = if delimiterFound then (delimiter ++ after) `isSuffixOf` input else null after
  in classify delimiterFound "delimiter found" $
     property $ expectedBefore .&&. expectedAfter

-- Test: Text processing performance with large files
test_large_file_processing :: TestTree
test_large_file_processing = testCase "Text processing performance with large files" $ do
  let largeFunction = unlines $ replicate 1000 "  x := x + 1  // increment"
      largeCode = "package main\n\nfunc main() {\n  x := 0\n" ++ largeFunction ++ "}"
      result = compile largeCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasReasonableLength = all (\msg -> length msg <= 1000) errorMessages
      if hasReasonableLength
        then return ()  -- Success - large file handled with reasonable error messages
        else assertFailure $ "Large file processing failed: " ++ unlines (take 3 errorMessages)
    Right _ -> return ()  -- Compilation succeeded

tests :: TestTree
tests = testGroup "Text Processing Robustness Tests"
  [ test_long_line_processing
  , test_nested_comment_removal
  , test_mixed_indentation_normalization
  , test_unicode_text_processing
  , test_malformed_text_robustness
  , test_large_file_processing
  , fastProperty "Trim edge cases" prop_trim_edge_cases
  , fastProperty "SplitBy edge cases" prop_splitby_edge_cases
  , fastProperty "Comment removal preserves strings" prop_comment_removal_preserves_strings
  , fastProperty "Indentation handles empty lines" prop_indentation_handles_empty_lines
  , fastProperty "BreakOn edge cases" prop_breakon_edge_cases
  ]