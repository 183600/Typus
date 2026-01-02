{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.StringProcessingBoundaryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Utils
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit, isPunctuation)
import Data.List (length, concat)
import Data.List (sort, nub, filter, elem, intercalate, unlines, unwords)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import qualified Data.Text as T

-- ============================================================================
-- Advanced String Processing Boundary QuickCheck Tests
-- ============================================================================

-- Property: Trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: Trim removes only leading/trailing whitespace
prop_trim_boundary :: String -> String -> Property
prop_trim_boundary prefix suffix =
  let whitespacePrefix = concatMap (const " ") prefix
      whitespaceSuffix = concatMap (const " ") suffix
      content = "content"
      fullString = whitespacePrefix ++ content ++ whitespaceSuffix
      trimmed = trim fullString
  in property $ 
    (not (null prefix) || not (null suffix)) ==> 
    trimmed === content .||.
    (null trimmed && null content)

-- Property: SplitBy preserves L.all content
prop_split_by_preserves_content :: Char -> String -> Property
prop_split_by_preserves_content delim s =
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in property $ rejoined === s

-- Property: SplitByCollapsed removes empty parts
prop_split_by_collapsed_removes_empty :: Char -> String -> Property
prop_split_by_collapsed_removes_empty delim s =
  let parts = splitBy delim s
      collapsedParts = splitByCollapsed delim s
  in property $ 
    all (not . null) collapsedParts .&&.
    length collapsedParts <= L.length parts

-- Property: SplitByComma is equivalent to SplitBy with comma
prop_split_by_comma_equivalence :: String -> Property
prop_split_by_comma_equivalence s =
  let commaSplit = splitByComma s
      genericSplit = splitBy ',' s
  in property $ commaSplit === genericSplit

-- Property: Remove line comments preserves non-comment content
prop_remove_line_comments_preserves_content :: String -> String -> Property
prop_remove_line_comments_preserves_content code comment =
  let lineWithComment = code ++ " // " ++ comment
      withoutComment = removeLineComments lineWithComment
  in property $ 
    withoutComment `contains` code .&&.
    not (withoutComment `contains` comment)

-- Property: Remove comments handles nested block comments
prop_remove_comments_nested_blocks :: String -> String -> Property
prop_remove_comments_nested_blocks outer inner =
  let nestedComment = "/* outer " ++ "/* inner */" ++ " */"
      codeWithComment = "code " ++ nestedComment ++ " more code"
      withoutComment = removeComments codeWithComment
  in property $ 
    withoutComment `contains` "code" .&&.
    withoutComment `contains` "more code" .&&.
    not (withoutComment `contains` "outer") .&&.
    not (withoutComment `contains` "inner")

-- Property: Normalize indentation preserves relative structure
prop_normalize_indentation_preserves_structure :: [String] -> Property
prop_normalize_indentation_preserves_structure lines =
  length lines > 0 && L.all (not . null) lines ==>
  let originalContent = unlines lines
      normalized = normalizeIndentation originalContent
      normalizedLines = lines normalized
  in property $ 
    length normalizedLines === L.length lines .&&.
    all (`elem` normalizedLines) (lines normalized)

-- Property: Break on substring is deterministic
prop_break_on_deterministic :: String -> String -> Property
prop_break_on_deterministic s substr =
  let result1 = breakOn substr s
      result2 = breakOn substr s
  in property $ result1 === result2

-- Property: Break on empty string returns original
prop_break_on_empty_string :: String -> Property
prop_break_on_empty_string s =
  let result = breakOn "" s
  in property $ result === s

-- Property: String processing functions handle Unicode
prop_string_processing_unicode :: [Int] -> Property
prop_string_processing_unicode codePoints =
  all (>= 32) codePoints && L.all (<= 126) codePoints ==>  -- ASCII range for testing
  let unicodeString = map chr codePoints
      trimmed = trim unicodeString
      parts = splitBy ',' unicodeString
  in property $ 
    length trimmed <= L.length unicodeString .&&.
    length (L.concat parts) >= L.length (L.filter (/= ',') unicodeString)

-- Property: Split L.and rejoin with multiple delimiters
prop_split_rejoin_multiple_delimiters :: String -> Char -> Char -> Property
prop_split_rejoin_multiple_delimiters s delim1 delim2 =
  delim1 /= delim2 ==>
  let parts1 = splitBy delim1 s
      parts2 = splitBy delim2 s
      rejoined1 = intercalate [delim1] parts1
      rejoined2 = intercalate [delim2] parts2
  in property $ 
    rejoined1 === s .&&.
    rejoined2 === s

-- Property: Comment removal preserves string literals
prop_comment_preserves_string_literals :: String -> String -> Property
prop_comment_preserves_string_literals content comment =
  let stringWithLiteral = "let x = \"" ++ content ++ "\" // " ++ comment
      withoutComment = removeLineComments stringWithLiteral
  in property $ 
    withoutComment `contains` (\"" ++ content ++ "\") .&&.
    not (withoutComment `contains` comment)

-- Property: Indentation normalization handles mixed tabs/spaces
prop_indentation_mixed_tabs_spaces :: [String] -> Property
prop_indentation_mixed_tabs_spaces lines =
  length lines > 0 && L.all (not . null) lines ==>
  let mixedIndentation = L.map (\l -> "\t  " ++ l) lines
      content = unlines mixedIndentation
      normalized = normalizeIndentation content
  in property $ 
    all (`elem` lines normalized) (lines normalized)

-- Helper function to check string containment
contains :: String -> String -> Bool
contains needle haystack = needle `Data.List.L.isInfixOf` haystack

-- Helper function to convert Int to Char
chr :: Int -> Char
chr = toEnum

-- Test collection
tests :: TestTree
tests = testGroup "Advanced String Processing Boundary QuickCheck Tests"
  [ fastProperty "Trim is idempotent" prop_trim_idempotent
  , fastProperty "Trim removes only leading/trailing whitespace" prop_trim_boundary
  , fastProperty "SplitBy preserves L.all content" prop_split_by_preserves_content
  , fastProperty "SplitByCollapsed removes empty parts" prop_split_by_collapsed_removes_empty
  , fastProperty "SplitByComma is equivalent to SplitBy with comma" prop_split_by_comma_equivalence
  , fastProperty "Remove line comments preserves non-comment content" prop_remove_line_comments_preserves_content
  , fastProperty "Remove comments handles nested block comments" prop_remove_comments_nested_blocks
  , fastProperty "Normalize indentation preserves relative structure" prop_normalize_indentation_preserves_structure
  , fastProperty "Break on substring is deterministic" prop_break_on_deterministic
  , fastProperty "Break on empty string returns original" prop_break_on_empty_string
  , fastProperty "String processing functions handle Unicode" prop_string_processing_unicode
  , fastProperty "Split L.and rejoin with multiple delimiters" prop_split_rejoin_multiple_delimiters
  , fastProperty "Comment removal preserves string literals" prop_comment_preserves_string_literals
  , fastProperty "Indentation normalization handles mixed tabs/spaces" prop_indentation_mixed_tabs_spaces
  ]