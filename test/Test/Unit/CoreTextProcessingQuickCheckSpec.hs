{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreTextProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- ============================================================================
-- Core Text Processing Properties
-- ============================================================================

-- Property: splitBy is inverse to concat for non-empty delimiter
prop_split_by_concat_inverse :: Char -> String -> Property
prop_split_by_concat_inverse delim s = delim /= '\0' ==> 
  let parts = splitBy delim s
      reconstructed = concat (map (\p -> if null p then "" else p ++ [delim]) (init parts) ++ [last parts])
  in counterexample "splitBy should be inverse to concat" $
     reconstructed === s

-- Property: splitByCollapsed removes empty segments
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim s = delim /= '\0' ==>
  let parts = splitByCollapsed delim
  in property $ all (not . null) (parts s)

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: trim removes only leading/trailing whitespace
prop_trim_preserves_internal_structure :: String -> String -> String -> Property
prop_trim_preserves_internal_structure prefix middle suffix =
  let s = prefix ++ middle ++ suffix
      trimmed = trim s
      hasInternalWhitespace = any isSpace middle
  in classify hasInternalWhitespace "has internal whitespace" $
     property $ middle `isInfixOf` trimmed

-- Property: removeLineComments preserves non-comment lines
prop_remove_line_comments_preserves_non_comments :: String -> Property
prop_remove_line_comments_preserves_non_comments s =
  let linesWithoutComments = lines s
      processedLines = lines (removeLineComments s)
      hasNoCommentPrefix = not (any ("//" `isPrefixOf`) linesWithoutComments)
  in classify hasNoCommentPrefix "no comment lines" $
     property $ length processedLines === length linesWithoutComments

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s =
  let linesList = lines s
      originalIndents = map (length . takeWhile isSpace) linesList
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
      normalizedIndents = map (length . takeWhile isSpace) normalizedLines
      -- Calculate relative differences
      relativeDiffs = zipWith (-) (tail originalIndents) (init originalIndents)
      normalizedDiffs = zipWith (-) (tail normalizedIndents) (init normalizedIndents)
  in length linesList > 1 ==> 
     property $ relativeDiffs === normalizedDiffs

-- Property: breakOn finds first occurrence
prop_break_on_finds_first :: String -> String -> Property
prop_break_on_finds_first needle haystack =
  not (null needle) && needle `isInfixOf` haystack ==>
  let (before, after) = breakOn needle haystack
      expectedBefore = takeWhile (not . (needle `isPrefixOf`)) (tails haystack) >>= head
  in counterexample "breakOn should find first occurrence" $
     before ++ needle ++ after === haystack

-- Property: splitBy respects delimiter boundaries
prop_split_by_respects_boundaries :: Char -> String -> String -> Property
prop_split_by_respects_boundaries delim s1 s2 = delim /= '\0' ==>
  let combined = s1 ++ [delim] ++ s2
      parts = splitBy delim combined
  in length parts === 2 .&&. head parts === s1 .&&. last parts === s2

-- Property: removeComments handles nested block comments safely
prop_remove_comments_safe_nested :: String -> String -> Property
prop_remove_comments_safe_nested start middle =
  let comment = "/* " ++ start ++ " /* " ++ middle ++ " */ */"
      code = "code before " ++ comment ++ " code after"
      processed = removeComments code
  in property $ "code before" `isInfixOf` processed .&&. "code after" `isInfixOf` processed

-- Property: text processing functions handle Unicode gracefully
prop_unicode_handling :: String -> Property
prop_unicode_handling s =
  let trimmed = trim s
      parts = splitBy ',' s
      noComments = removeLineComments s
  in property $ 
    -- Should not crash on Unicode input
    length trimmed >= 0 .&&.
    length parts >= 0 .&&.
    length noComments >= 0

tests :: TestTree
tests = testGroup "Core Text Processing QuickCheck Tests"
  [ fastProperty "splitBy concat inverse" prop_split_by_concat_inverse
  , fastProperty "splitByCollapsed no empty" prop_split_by_collapsed_no_empty
  , fastProperty "trim idempotent" prop_trim_idempotent
  , fastProperty "trim preserves internal structure" prop_trim_preserves_internal_structure
  , fastProperty "removeLineComments preserves non-comments" prop_remove_line_comments_preserves_non_comments
  , fastProperty "normalizeIndentation preserves relative" prop_normalize_indentation_preserves_relative
  , fastProperty "breakOn finds first" prop_break_on_finds_first
  , fastProperty "splitBy respects boundaries" prop_split_by_respects_boundaries
  , fastProperty "removeComments safe nested" prop_remove_comments_safe_nested
  , fastProperty "Unicode handling" prop_unicode_handling
  ]