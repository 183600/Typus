{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreSourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Data.List (sort)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , advancePos
  , advancePosBy
  )

-- ============================================================================
-- Source Location Mathematical Properties
-- ============================================================================

-- Property: Position advancement is consistent
prop_pos_advancement_consistent :: Int -> Int -> String -> Property
prop_pos_advancement_consistent line col text =
  line > 0 && col > 0 && not (null text) ==>
  let start = SourcePos line col
      after = advancePos start text
      lineCount = L.length (L.filter (== '\n') text)
  in property $ sourceLine after >= line .&&. sourceLine after <= line + lineCount + 1

-- Property: Empty span validity
prop_empty_span_validity :: Int -> Int -> Property
prop_empty_span_validity line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      span = emptySpan pos
  in property $ isValidSpan span .&&. spanStart span === pos .&&. spanEnd span === pos

-- Property: Span creation preserves order
prop_span_creation_order :: Int -> Int -> Int -> Int -> Property
prop_span_creation_order startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: Span merging is associative
prop_span_merging_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_merging_associative l1 c1 l2 c2 l3 c3 l4 c4 =
  L.all (>0) [l1, c1, l2, c2, l3, c3, l4, c4] ==>
  let p1 = SourcePos l1 c1
      p2 = SourcePos l2 c2
      p3 = SourcePos l3 c3
      p4 = SourcePos l4 c4
      s1 = spanBetween p1 p2
      s2 = spanBetween p2 p3
      s3 = spanBetween p3 p4
      merge12 = mergeSpans s1 s2
      merge23 = mergeSpans s2 s3
      mergeLeft = mergeSpans merge12 s3
      mergeRight = mergeSpans s1 merge23
  in property $ mergeLeft === mergeRight

-- Property: Position advancement by character count
prop_pos_advancement_by_count :: Int -> Int -> Int -> Property
prop_pos_advancement_by_count line col count =
  line > 0 && col > 0 && count >= 0 && count <= 1000 ==>
  let start = SourcePos line col
      after = advancePosBy start count
  in property $ sourceLine after >= line .&&. sourceColumn after >= col

-- Property: Located values preserve their content
prop_located_preserves_content :: Int -> Int -> String -> Property
prop_located_preserves_content line col value =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      located = locatedAt pos value
  in property $ locatedValue located === value

-- Property: Span validity is transitive
prop_span_validity_transitive :: Int -> Int -> Int -> Int -> Property
prop_span_validity_transitive startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = spanBetween start end
      isValid = isValidSpan span
  in classify isValid "valid span" $
     property $ isValid ==> (spanStart span <= spanEnd span)

-- Property: Multiple position advancements compose correctly
prop_multiple_pos_advancements :: Int -> Int -> String -> String -> Property
prop_multiple_pos_advancements line col text1 text2 =
  line > 0 && col > 0 && not (null text1) && not (null text2) ==>
  let start = SourcePos line col
      after1 = advancePos start text1
      after2 = advancePos after1 text2
      afterCombined = advancePos start (text1 ++ text2)
  in property $ after2 === afterCombined

-- Property: Span containment relationship
prop_span_containment :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_containment outerL outerC innerL innerC offset =
  L.all (>0) [outerL, outerC, innerL, innerC] && offset >= 0 && offset <= 10 ==>
  let outerStart = SourcePos outerL outerC
      innerStart = SourcePos innerL innerC
      innerEnd = advancePosBy innerStart offset
      outerEnd = advancePosBy innerStart (offset + 5)
      outerSpan = spanBetween outerStart outerEnd
      innerSpan = spanBetween innerStart innerEnd
  in property $ isValidSpan outerSpan .&&. isValidSpan innerSpan

-- Property: Source position ordering
prop_source_position_ordering :: Int -> Int -> Int -> Int -> Property
prop_source_position_ordering line1 col1 line2 col2 =
  L.all (>0) [line1, col1, line2, col2] ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      lineCompare = compare line1 line2
      colCompare = compare col1 col2
  in property $ 
    if line1 /= line2 
    then lineCompare === compare pos1 pos2
    else colCompare === compare pos1 pos2

tests :: TestTree
tests = testGroup "Core Source Location Math QuickCheck Tests"
  [ fastProperty "position advancement consistent" prop_pos_advancement_consistent
  , fastProperty "empty span validity" prop_empty_span_validity
  , fastProperty "span creation order" prop_span_creation_order
  , fastProperty "span merging associative" prop_span_merging_associative
  , fastProperty "position advancement by count" prop_pos_advancement_by_count
  , fastProperty "located preserves content" prop_located_preserves_content
  , fastProperty "span validity transitive" prop_span_validity_transitive
  , fastProperty "multiple position advancements" prop_multiple_pos_advancements
  , fastProperty "span containment" prop_span_containment
  , fastProperty "source position ordering" prop_source_position_ordering
  ]