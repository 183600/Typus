{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import qualified Text.Megaparsec as MP
import Utils (trim)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isNothing, isJust)

-- Property: Parser recovery after syntax error should produce partial results
prop_parser_recovery_partial_results :: String -> String -> Property
prop_parser_recovery_partial_results validPrefix invalidSuffix =
  let input = validPrefix ++ invalidSuffix
      result = parseTypus input
      hasPartialResult = case result of
        Right _ -> True
        Left _ -> False -- For this test, we consider L.any parse as success
  in counterexample "Parser should handle partial input gracefully" $
     property hasPartialResult

-- Property: Empty input should parse to empty file
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
      isEmptyFile = case result of
        Right (TypusFile _ [] _) -> True
        _ -> False
  in counterexample "Empty input should parse to empty file" $
     property isEmptyFile

-- Property: Only whitespace input should parse to empty file
prop_parser_whitespace_input :: String -> Property
prop_parser_whitespace_input ws =
  let allWhitespace = L.all (`elem` " \t\n\r") ws
      result = parseTypus ws
      isEmptyFile = case result of
        Right (TypusFile _ [] _) -> True
        _ -> False
  in allWhitespace ==> counterexample "Whitespace-only input should parse to empty file" $
     property isEmptyFile

-- Property: Parser should handle comments gracefully
prop_parser_comments_handling :: String -> String -> Property
prop_parser_comments_handling codeBefore comment =
  let commentLine = "// " ++ comment
      input = codeBefore ++ "\n" ++ commentLine
      result = parseTypus input
      canParse = case result of
        Right _ -> True
        Left _ -> False
  in counterexample "Parser should handle comments gracefully" $
     property canParse

-- Property: Parser should recover from unmatched braces
prop_parser_unmatched_braces_recovery :: String -> String -> Property
prop_parser_unmatched_braces_recovery before after =
  let input = before ++ "{\n" ++ after
      result = parseTypus input
      hasResult = case result of
        Right _ -> True
        Left _ -> False
  in counterexample "Parser should attempt recovery from unmatched braces" $
     property hasResult

-- Property: Parser should handle very long lines
prop_parser_long_lines :: String -> Int -> Property
prop_parser_long_lines base repeatCount =
  let longLine = L.concat (replicate repeatCount base)
      input = longLine ++ "\n"
      result = parseTypus input
      canHandle = case result of
        Right _ -> True
        Left _ -> False
  in repeatCount >= 0 && repeatCount <= 100 ==> 
     counterexample "Parser should handle very long lines" $
     property canHandle

-- Property: Parser should handle Unicode characters
prop_parser_unicode_handling :: String -> Property
prop_parser_unicode_handling unicodeStr =
  let input = "// Unicode test: " ++ unicodeStr ++ "\n"
      result = parseTypus input
      canHandle = case result of
        Right _ -> True
        Left _ -> False
  in counterexample "Parser should handle Unicode characters" $
     property canHandle

-- Property: Parser should handle nested blocks
prop_parser_nested_blocks :: Int -> Property
prop_parser_nested_blocks depth =
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      input = L.concat openBraces ++ L.concat closeBraces
      result = parseTypus input
      hasResult = case result of
        Right _ -> True
        Left _ -> False
  in depth >= 0 && depth <= 20 ==> 
     counterexample "Parser should handle nested blocks" $
     property hasResult

-- Property: Parser should handle mixed newlines
prop_parser_mixed_newlines :: String -> String -> Property
prop_parser_mixed_newlines part1 part2 =
  let input = part1 ++ "\r\n" ++ part2 ++ "\n" ++ part1 ++ "\r" ++ part2
      result = parseTypus input
      canHandle = case result of
        Right _ -> True
        Left _ -> False
  in counterexample "Parser should handle mixed newline styles" $
     property canHandle

-- Property: Parser should preserve file directives
prop_parser_directives_preservation :: String -> String -> Property
prop_parser_directives_preservation directive content =
  let input = "@ownership: true\n" ++ "@dependent-types: false\n" ++ content
      result = parseTypus input
      hasDirectives = case result of
        Right (TypusFile (FileDirectives _ _ _) _ _) -> True
        _ -> False
  in counterexample "Parser should preserve file directives" $
     property hasDirectives

tests :: TestTree
tests =
  testGroup "New Cabal Parser Error Recovery Tests"
    [ fastProperty "Parser recovery after syntax error should produce partial results" prop_parser_recovery_partial_results
    , fastProperty "Empty input should parse to empty file" prop_parser_empty_input
    , fastProperty "Only whitespace input should parse to empty file" prop_parser_whitespace_input
    , fastProperty "Parser should handle comments gracefully" prop_parser_comments_handling
    , fastProperty "Parser should recover from unmatched braces" prop_parser_unmatched_braces_recovery
    , fastProperty "Parser should handle very long lines" prop_parser_long_lines
    , fastProperty "Parser should handle Unicode characters" prop_parser_unicode_handling
    , fastProperty "Parser should handle nested blocks" prop_parser_nested_blocks
    , fastProperty "Parser should handle mixed newlines" prop_parser_mixed_newlines
    , fastProperty "Parser should preserve file directives" prop_parser_directives_preservation
    ]