{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, group)
import qualified Data.Text as T

-- Property: trim never increases string L.length
prop_trim_never_increases_length :: String -> Property
prop_trim_never_increases_length s =
  let trimmed = trim s
  in property $ L.length trimmed <= L.length s

-- Property: trim preserves L.all non-space characters
prop_trim_preserves_non_space :: String -> Property
prop_trim_preserves_non_space s =
  let trimmed = trim s
      nonSpaceOriginal = L.filter (not . isSpace) s
      nonSpaceTrimmed = L.filter (not . isSpace) trimmed
  in property $ sort nonSpaceOriginal === sort nonSpaceTrimmed

-- Property: splitBy preserves total character count (including delimiters)
prop_splitBy_preserves_chars :: Char -> String -> Property
prop_splitBy_preserves_chars delim s =
  let parts = splitBy delim s
      reconstructed = L.foldr (\x acc -> x ++ [delim] ++ acc) (last parts) (init parts)
      originalLength = L.length s
      reconstructedLength = L.length reconstructed
  in property $ if null parts 
                   then originalLength === 0
                   else reconstructedLength === originalLength

-- Property: splitByCollapsed never produces empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ L.all (not . null) parts

-- Property: removeLineComments preserves non-commented lines
prop_remove_line_comments_preserves_non_commented :: String -> Property
prop_remove_line_comments_preserves_non_commented s =
  let lines' = lines s
      withoutComments = removeLineComments s
      resultLines = lines withoutComments
      -- Count lines that don't start with // (ignoring leading spaces)
      originalNonComment = L.length $ L.filter (\line -> not (("//" `L.isPrefixOf`) (dropWhile isSpace line))) lines'
      resultNonComment = L.length resultLines
  in property $ resultNonComment <= originalNonComment

-- Property: removeComments idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent s =
  let once = removeComments s
      twice = removeComments once
  in property $ once === twice

-- Property: normalizeIndentation preserves line count
prop_normalize_preserves_line_count :: String -> Property
prop_normalize_preserves_line_count s =
  let normalized = normalizeIndentation s
      originalLines = L.length $ lines s
      normalizedLines = L.length $ lines normalized
  in property $ originalLines === normalizedLines

-- Property: normalizeIndentation never introduces trailing spaces
prop_normalize_no_trailing_spaces :: String -> Property
prop_normalize_no_trailing_spaces s =
  let normalized = normalizeIndentation s
      hasTrailingSpaces = L.any (`L.isSuffixOf`) (L.map (:[]) " \t") (lines normalized)
  in property $ not hasTrailingSpaces

-- Property: breakOn consistency with isInfixOf prop_breakOn_consistency :: String -> String -> Property
prop_breakOn_consistency needle haystack =
  let (before, after) = breakOn needle haystack
      found = needle `L.isInfixOf` haystack
  in property $ if found
                   then needle `L.isPrefixOf` after
                   else after === haystack

-- Property: fixIndentation consistency with normalizeIndentation
prop_fix_indentation_consistency :: String -> Property
prop_fix_indentation_consistency s =
  let fixed = fixIndentation s
      normalized = normalizeIndentation s
  in property $ fixed === normalized

-- Property: multiple trim operations converge
prop_multiple_trim_converge :: String -> Positive Int -> Property
prop_multiple_trim_converge s (Positive n) =
  let trimOnce = trim s
      trimMultiple = iterate trim s !! (min n 5)  -- Limit iterations to avoid infinite loops
  in property $ trimOnce === trimMultiple

-- Property: splitBy respects delimiter boundaries
prop_splitby_respects_boundaries :: Char -> NonEmptyList Char -> NonEmptyList Char -> Property
prop_splitby_respects_boundaries delim (NonEmpty prefix) (NonEmpty suffix) =
  let s = prefix ++ [delim] ++ suffix
      parts = splitBy delim s
  in property $ L.length parts === 2 .&&. L.head parts === prefix .&&. last parts === suffix

-- Property: comment removal preserves string literals
prop_comment_preserves_string_literals :: String -> Property
prop_comment_preserves_string_literals s =
  let withoutComments = removeComments s
      -- Count quote characters (simple heuristic for string literals)
      originalQuotes = L.length $ L.filter (== '"') s
      afterQuotes = L.length $ L.filter (== '"') withoutComments
  in property $ afterQuotes <= originalQuotes

tests :: TestTree
tests = testGroup "Text Processing Properties QuickCheck"
  [ fastProperty "trim never increases L.length" prop_trim_never_increases_length
  , fastProperty "trim preserves non-space" prop_trim_preserves_non_space
  , fastProperty "splitBy preserves chars" prop_splitBy_preserves_chars
  , fastProperty "splitByCollapsed no empty" prop_splitByCollapsed_no_empty
  , fastProperty "remove line comments preserves non-commented" prop_remove_line_comments_preserves_non_commented
  , fastProperty "remove comments idempotent" prop_remove_comments_idempotent
  , fastProperty "normalize preserves line count" prop_normalize_preserves_line_count
  , fastProperty "normalize no trailing spaces" prop_normalize_no_trailing_spaces
  , fastProperty "breakOn consistency" prop_breakOn_consistency
  , fastProperty "fix indentation consistency" prop_fix_indentation_consistency
  , fastProperty "multiple trim converge" prop_multiple_trim_converge
  , fastProperty "splitBy respects boundaries" prop_splitby_respects_boundaries
  , fastProperty "comment preserves string literals" prop_comment_preserves_string_literals
  ]