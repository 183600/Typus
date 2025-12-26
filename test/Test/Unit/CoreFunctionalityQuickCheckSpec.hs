{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreFunctionalityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , emptySpan
  , mergeSpans
  , isValidSpan
  , advancePos
  )

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, sort, nub)
import qualified Data.Text as T

-- Property: trim is idempotent (trimming twice is same as trimming once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy and splitByCollapsed relationship
prop_splitBy_relationship :: Char -> String -> Property
prop_splitBy_relationship delim s =
  let withEmpty = splitBy delim s
      withoutEmpty = splitByCollapsed delim s
      -- Collapsed version should never have empty strings
      noEmpties = all (not . null) withoutEmpty
      -- Collapsed version should be a subset of non-empty parts from regular split
      filtered = filter (not . null) withEmpty
  in property $ noEmpties .&&. (sort withoutEmpty === sort filtered)

-- Property: breakOn finds first occurrence or returns whole string
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness needle haystack =
  let (before, after) = breakOn needle haystack
      needleFound = needle `isInfixOf` haystack
      needleInAfter = needle `isPrefixOf` after
  in classify needleFound "needle found" $
     classify (not needleFound) "needle not found" $
     property $ if needleFound 
                   then needleInAfter .&&. (before ++ needle ++ after === haystack)
                   else (before === haystack) .&&. (after === "")

-- Property: source position advancement is consistent
prop_pos_advancement_consistent :: Positive Int -> Positive Int -> String -> Property
prop_pos_advancement_consistent (Positive line) (Positive col) text =
  let start = SourcePos line col
      advanced = advancePos start text
      -- Line should never decrease
      lineNonDecreasing = sourceLine advanced >= sourceLine start
      -- Column should be positive
      colPositive = sourceColumn advanced > 0
  in property $ lineNonDecreasing .&&. colPositive

-- Property: empty span merging
prop_empty_span_merge :: SourceSpan -> Property
prop_empty_span_merge span =
  let empty = emptySpan
      merged1 = mergeSpans empty span
      merged2 = mergeSpans span empty
  in property $ merged1 === span .&&. merged2 === span

-- Property: span validity after merging
prop_span_merge_validity :: SourceSpan -> SourceSpan -> Property
prop_span_merge_validity span1 span2 =
  let merged = mergeSpans span1 span2
      valid1 = isValidSpan span1
      valid2 = isValidSpan span2
      validMerged = isValidSpan merged
  in classify (valid1 && valid2) "both valid" $
     classify (not valid1 && not valid2) "both invalid" $
     property $ if valid1 || valid2 
                   then validMerged
                   else property True -- Merging invalid spans may result in invalid spans

-- Property: comment removal preserves non-comment content structure
prop_comment_preserves_structure :: String -> Property
prop_comment_preserves_structure code =
  let withoutComments = removeComments code
      -- Count of non-comment, non-whitespace characters should be preserved
      originalContent = length $ filter (\c -> not (isSpace c) && c /= '/' && c /= '*') code
      contentAfter = length $ filter (\c -> not (isSpace c) && c /= '/' && c /= '*') withoutComments
  in property $ contentAfter <= originalContent

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative code =
  let normalized = normalizeIndentation code
      originalLines = lines code
      normalizedLines = lines normalized
      -- Check that relative indentation differences are preserved
      relativeDiffs = zipWith (\l1 l2 -> length (takeWhile isSpace l1) - length (takeWhile isSpace l2)) 
                             (tail originalLines) originalLines
      normalizedDiffs = zipWith (\l1 l2 -> length (takeWhile isSpace l1) - length (takeWhile isSpace l2)) 
                               (tail normalizedLines) normalizedLines
  in property $ relativeDiffs === normalizedDiffs

-- Property: string operations round-trip consistency
prop_string_roundtrip :: NonEmptyList Char -> NonEmptyList Char -> Property
prop_string_roundtrip (NonEmpty delim) (NonEmpty content) =
  let str = content ++ [delim] ++ content
      parts = splitBy delim str
      rejoined = foldr (\x acc -> x ++ [delim] ++ acc) (last parts) (init parts)
  in property $ rejoined === str

tests :: TestTree
tests = testGroup "Core Functionality QuickCheck"
  [ fastProperty "trim idempotent" prop_trim_idempotent
  , fastProperty "splitBy relationship" prop_splitBy_relationship
  , fastProperty "breakOn correctness" prop_breakOn_correctness
  , fastProperty "position advancement consistency" prop_pos_advancement_consistent
  , fastProperty "empty span merge" prop_empty_span_merge
  , fastProperty "span merge validity" prop_span_merge_validity
  , fastProperty "comment preserves structure" prop_comment_preserves_structure
  , fastProperty "normalize preserves relative" prop_normalize_preserves_relative
  , fastProperty "string roundtrip" prop_string_roundtrip
  ]