{-# LANGUAGE CPP #-}

module Test.Unit.NewCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (isInfixOf, tails)

import Utils (trim, splitBy, splitByCollapsed, removeComments, breakOn, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, spanFrom, spanBetween, mergeSpans, isValidSpan, locatedAt, mapLocated)
import TestSupport.Arbitrary ()

-- Property 1: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

-- Property 2: splitBy and splitByCollapsed relationship
prop_splitBy_collapsed :: Char -> String -> Property
prop_splitBy_collapsed delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
      filtered = filter (not . null) normal
  in collapsed === filtered

-- Property 3: breakOn returns correct prefix
prop_breakOn_prefix :: String -> String -> Property
prop_breakOn_prefix pat s =
  not (null pat) ==>
  let (prefix, _) = breakOn pat s
      expected = if pat `isInfixOf` s
                 then take (length s - length (dropWhile (not . isPrefixOf pat) (tails s) !! 0)) s
                 else s
  in prefix === expected
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Property 4: SourcePos advancement is consistent
prop_sourcepos_advancement :: Char -> SourcePos -> Property
prop_sourcepos_advancement c pos =
  let advanced = posAfter c pos
      expectedOffset = posOffset pos + 1
  in posOffset advanced === expectedOffset

-- Property 5: spanBetween creates valid spans
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 =
  let (start, end) = if posOffset pos1 <= posOffset pos2
                    then (pos1, pos2)
                    else (pos2, pos1)
      span = spanBetween start end
  in (posOffset (spanStart span) <= posOffset (spanEnd span)) === True

-- Property 6: mergeSpans is commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property 7: locatedAt creates valid located values
prop_located_at_valid :: Int -> Int -> String -> Property
prop_located_at_valid line col value =
  let pos = SourcePos line col 0
      located = locatedAt pos value
  in locSpan located === spanFrom pos

-- Property 8: mapLocated preserves location
prop_map_located_preserves_location :: Int -> String -> Property
prop_map_located_preserves_location n value =
  let pos = startPos
      located = locatedAt pos value
      mapped = mapLocated (take n) located
  in locSpan mapped === locSpan located

-- Property 9: normalizeIndentation preserves relative structure
prop_normalize_indentation_preserves :: String -> Property
prop_normalize_indentation_preserves s =
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in length normalizedLines === length lines'

-- Property 10: removeComments preserves structure when no comments
prop_remove_comments_preserves :: String -> Property
prop_remove_comments_preserves s =
  not (any (`isInfixOf` s) ["//", "/*"]) ==>
  let result = removeComments s
  in result === s

tests :: TestTree
tests = testGroup "New Core QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitByCollapsed filters empty segments" prop_splitBy_collapsed
  , fastProperty "breakOn returns correct prefix" prop_breakOn_prefix
  , fastProperty "SourcePos advancement is consistent" prop_sourcepos_advancement
  , fastProperty "spanBetween creates valid spans" prop_span_between_valid
  , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
  , fastProperty "locatedAt creates valid located values" prop_located_at_valid
  , fastProperty "mapLocated preserves location" prop_map_located_preserves_location
  , fastProperty "normalizeIndentation preserves line count" prop_normalize_indentation_preserves
  , fastProperty "removeComments preserves non-comment strings" prop_remove_comments_preserves
  ]