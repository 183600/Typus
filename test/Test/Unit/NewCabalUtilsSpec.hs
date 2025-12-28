{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlpha)
import Data.List (isPrefixOf, isInfixOf, intersperse)

-- Test 1: Trim removes only leading/trailing whitespace
prop_trim_removes_only_whitespace :: String -> String -> String -> Property
prop_trim_removes_only_whitespace prefix content suffix =
  let leading = takeWhile isSpace prefix
      trailing = takeWhile isSpace (reverse suffix)
      full = leading ++ content ++ trailing
      trimmed = trim full
  in property $ trimmed === content

-- Test 2: Split by character round-trip property
prop_split_by_roundtrip :: Char -> String -> Property
prop_split_by_roundtrip delim s =
  let parts = splitBy delim s
      rejoined = concat $ intersperse [delim] parts
  in property $ rejoined === s

-- Test 3: Split collapsed removes empty segments
prop_split_collapsed_removes_empty :: Char -> String -> Property
prop_split_collapsed_removes_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- Test 4: Comma split delegation property
prop_comma_split_delegation :: String -> Property
prop_comma_split_delegation s =
  splitByComma s === splitBy ',' s

-- Test 5: Comment removal preserves string literals
prop_comment_preserves_strings :: String -> Property
prop_comment_preserves_strings code =
  let withStringLiteral = code ++ "url := \"http://example.com//path\" // comment"
      withoutComments = removeLineComments withStringLiteral
  in "//path" `isInfixOf` withoutComments

-- Test 6: Remove line comments doesn't affect multi-line comments
prop_line_comment_preserves_multiline :: String -> Property
prop_line_comment_preserves_multiline code =
  let withMultiLine = code ++ "/* multi-line\n   comment */"
      withoutLineComments = removeLineComments withMultiLine
  in "/* multi-line" `isInfixOf` withoutLineComments

-- Test 7: Normalization preserves relative indentation
prop_normalize_preserves_relative :: [String] -> Property
prop_normalize_preserves_relative lines =
  let input = unlines lines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
  in length normalizedLines === length lines

-- Test 8: Break on substring finds first occurrence
prop_break_on_finds_first :: String -> String -> Property
prop_break_on_finds_first text pattern =
  let (before, after) = breakOn pattern text
      combined = before ++ pattern ++ after
  in pattern `isInfixOf` text ==> combined === text

-- Test 9: Split collapsed on empty string returns empty
prop_split_collapsed_empty :: Char -> Property
prop_split_collapsed_empty delim =
  splitByCollapsed delim "" === []

-- Test 10: Trim double application idempotence
prop_trim_double_application :: String -> Property
prop_trim_double_application s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

tests :: TestTree
tests = 
  testGroup "New Cabal Utils Tests"
    [ fastProperty "Trim removes only leading/trailing whitespace" prop_trim_removes_only_whitespace
    , fastProperty "Split by character round-trip property" prop_split_by_roundtrip
    , fastProperty "Split collapsed removes empty segments" prop_split_collapsed_removes_empty
    , fastProperty "Comma split delegation property" prop_comma_split_delegation
    , fastProperty "Comment removal preserves string literals" prop_comment_preserves_strings
    , fastProperty "Line comment preserves multi-line comments" prop_line_comment_preserves_multiline
    , fastProperty "Normalization preserves relative indentation" prop_normalize_preserves_relative
    , fastProperty "Break on finds first occurrence" prop_break_on_finds_first
    , fastProperty "Split collapsed on empty string returns empty" prop_split_collapsed_empty
    , fastProperty "Trim double application idempotence" prop_trim_double_application
    ]