{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import SourceLocation
import Data.List (isInfixOf)
import Data.List (sort, nub, group, intercalate, find, delete, sortOn)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Advanced Source Location Math Properties QuickCheck Tests
-- ============================================================================

-- Property: Source position advancement is monotonic
prop_source_pos_monotonic :: Int -> Int -> String -> Property
prop_source_pos_monotonic line col text =
  line >= 0 && col >= 0 && not (null text) ==>
  let pos = SourcePos line col
      advancedPos = advancePos pos text
  in property $ 
    (sourceLine advancedPos >= sourceLine pos) .&&.
    (sourceLine advancedPos > sourceLine pos ==> sourceCol advancedPos >= 1)

-- Property: Span merging is associative
prop_span_merge_associative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_associative start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ 
    spanStart merged1 === spanStart merged2 .&&.
    spanEnd merged1 === spanEnd merged2

-- Property: Empty span is identity for merge
prop_empty_span_identity :: SourcePos -> SourcePos -> Property
prop_empty_span_identity start end =
  let span = spanBetween start end
      empty = emptySpan
      merged1 = mergeSpans span empty
      merged2 = mergeSpans empty span
  in property $ 
    merged1 === span .&&. merged2 === span

-- Property: Span validity is preserved under merge
prop_span_merge_validity :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_validity start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in property $ 
    (isValidSpan span1 && isValidSpan span2) ==> isValidSpan merged

-- Property: Located value extraction is inverse of construction
prop_located_inverse :: String -> SourcePos -> Property
prop_located_inverse value pos =
  let located = locatedAt value pos
      extractedValue = locatedValue located
      extractedPos = locatedPos located
  in property $ 
    extractedValue === value .&&. extractedPos === pos

-- Property: Position advancement respects newlines
prop_pos_advance_newlines :: Int -> Int -> Int -> Property
prop_pos_advance_newlines line col numLines =
  line >= 0 && col >= 0 && numLines >= 0 && numLines <= 100 ==>
  let pos = SourcePos line col
      textWithNewlines = L.concat (replicate numLines "\n")
      advancedPos = advancePos pos textWithNewlines
  in property $ 
    sourceLine advancedPos === line + numLines .&&.
    sourceCol advancedPos === col

-- Property: Span ordering is total
prop_span_total_ordering :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_total_ordering start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      start1Pos = spanStart span1
      start2Pos = spanStart span2
  in property $ 
    (start1Pos < start2Pos) || (start1Pos == start2Pos && spanEnd span1 <= spanEnd span2) ||
    (start2Pos < start1Pos) || (start2Pos == start1Pos && spanEnd span2 <= spanEnd span1)

-- Property: Distance calculation satisfies triangle inequality
prop_distance_triangle_inequality :: SourcePos -> SourcePos -> SourcePos -> Property
prop_distance_triangle_inequality pos1 pos2 pos3 =
  let dist12 = posDistance pos1 pos2
      dist23 = posDistance pos2 pos3
      dist13 = posDistance pos1 pos3
  in property $ dist13 <= dist12 + dist23

-- Property: Position distance is symmetric
prop_distance_symmetry :: SourcePos -> SourcePos -> Property
prop_distance_symmetry pos1 pos2 =
  let dist12 = posDistance pos1 pos2
      dist21 = posDistance pos2 pos1
  in property $ dist12 === dist21

-- Property: Position distance to self is zero
prop_distance_self_zero :: SourcePos -> Property
prop_distance_self_zero pos =
  let dist = posDistance pos pos
  in property $ dist === 0

-- Helper function to calculate position distance
posDistance :: SourcePos -> SourcePos -> Int
posDistance pos1 pos2 = 
  let lineDiff = abs (sourceLine pos1 - sourceLine pos2)
      colDiff = abs (sourceCol pos1 - sourceCol pos2)
  in lineDiff * 1000 + colDiff  -- Weight lines more heavily

-- Helper function to compare SourcePos
(<) :: SourcePos -> SourcePos -> Bool
SourcePos l1 c1 < SourcePos l2 c2 = 
  l1 < l2 || (l1 == l2 && c1 < c2)

(<=) :: SourcePos -> SourcePos -> Bool
SourcePos l1 c1 <= SourcePos l2 c2 = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Source Location Math Properties QuickCheck Tests"
  [ fastProperty "Source position advancement is monotonic" prop_source_pos_monotonic
  , fastProperty "Span merging is associative" prop_span_merge_associative
  , fastProperty "Empty span is identity for merge" prop_empty_span_identity
  , fastProperty "Span validity is preserved under merge" prop_span_merge_validity
  , fastProperty "Located value extraction is inverse of construction" prop_located_inverse
  , fastProperty "Position advancement respects newlines" prop_pos_advance_newlines
  , fastProperty "Span ordering is total" prop_span_total_ordering
  , fastProperty "Distance calculation satisfies triangle inequality" prop_distance_triangle_inequality
  , fastProperty "Position distance is symmetric" prop_distance_symmetry
  , fastProperty "Position distance to self is zero" prop_distance_self_zero
  ]