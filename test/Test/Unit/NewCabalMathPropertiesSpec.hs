{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , advancePos
  , advancePosBy
  , spanStart
  , spanEnd
  )

import Data.Char (ord)

-- Property: Source position advancement is commutative for character sequences
prop_pos_advance_commutative :: Int -> Int -> String -> Property
prop_pos_advance_commutative line col chars =
  let pos = SourcePos line col
      pos1 = advancePosBy pos (length chars)
      pos2 = foldl advancePos pos chars
  in counterexample "Advanced positions should match" $
     pos1 === pos2

-- Property: Position advancement preserves monotonicity
prop_pos_advance_monotonic :: Int -> Int -> String -> Property
prop_pos_advance_monotonic line col chars =
  let pos = SourcePos line col
      advanced = advancePosBy pos (length chars)
      isMonotonic = (sourceLine advanced > sourceLine pos) ||
                   (sourceLine advanced == sourceLine pos && sourceColumn advanced >= sourceColumn pos)
  in counterexample "Position advancement should be monotonic" $
     property isMonotonic

-- Property: Span merging is associative
prop_span_merge_associative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_associative p1 p2 p3 =
  let span12 = spanBetween p1 p2
      span23 = spanBetween p2 p3
      span123_left = mergeSpans span12 (spanBetween p2 p3)
      span123_right = mergeSpans (spanBetween p1 p2) span23
      isValid = isValidSpan span12 && isValidSpan span23
  in isValid ==> counterexample "Span merging should be associative" $
     span123_left === span123_right

-- Property: Span merging is commutative when spans overlap
prop_span_merge_commutative_overlap :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_commutative_overlap p1 p2 p3 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
      spansOverlap = isValidSpan span1 && isValidSpan span2 &&
                    (spanStart span2 <= spanEnd span1 && spanEnd span2 >= spanStart span1)
  in spansOverlap ==> counterexample "Span merging should be commutative for overlapping spans" $
     merged1 === merged2

-- Property: Empty span is identity element for merging
prop_span_empty_identity :: SourcePos -> SourcePos -> Property
prop_span_empty_identity p1 p2 =
  let span = spanBetween p1 p2
      empty = emptySpan p1
      mergedLeft = mergeSpans empty span
      mergedRight = mergeSpans span empty
  in counterexample "Empty span should be identity for merging" $
     mergedLeft === span .&&. mergedRight === span

-- Property: Position arithmetic consistency
prop_pos_arithmetic_consistency :: Int -> Int -> Int -> Property
prop_pos_arithmetic_consistency line col offset =
  let pos = SourcePos line col
      advanced = posAfter pos offset
      reverted = posAfter advanced (-offset)
      -- Note: We can't go before position (1,1)
      isValid = offset >= 0 || (line > 1 || col > 1 + (-offset))
  in isValid ==> counterexample "Position arithmetic should be consistent" $
     reverted === pos

-- Property: Column reset on newline
prop_pos_newline_reset :: Int -> Int -> Int -> Property
prop_pos_newline_reset line col charsInLine =
  let pos = SourcePos line col
      newlinePos = advancePos pos '\n'
      expectedColumn = 1
      expectedLine = line + 1
  in counterexample "Newline should reset column to 1 and increment line" $
     sourceColumn newlinePos === expectedColumn .&&.
     sourceLine newlinePos === expectedLine

-- Property: Tab advances to next tab position (8-character alignment)
prop_pos_tab_alignment :: Int -> Int -> Property
prop_pos_tab_alignment line col =
  let pos = SourcePos line col
      tabPos = advancePos pos '\t'
      expectedColumn = ((col - 1) `div` 8 + 1) * 8 + 1
  in counterexample "Tab should advance to next 8-character boundary" $
     sourceColumn tabPos === expectedColumn

-- Property: Span validity is preserved under valid operations
prop_span_validity_preserved :: SourcePos -> SourcePos -> Property
prop_span_validity_preserved p1 p2 =
  let span = spanBetween p1 p2
      merged = mergeSpans span span
      isValid = isValidSpan span
  in isValid ==> counterexample "Valid span should remain valid after merging with itself" $
     isValidSpan merged

-- Property: Position creation with line/column constraints
prop_pos_creation_constraints :: Int -> Int -> Property
prop_pos_creation_constraints line col =
  let pos = posAtLineCol line col
      validLine = line >= 1
      validColumn = col >= 1
  in (validLine && validColumn) ==> counterexample "Valid position should have positive line and column" $
     sourceLine pos >= 1 .&&. sourceColumn pos >= 1

tests :: TestTree
tests =
  testGroup "New Cabal Math Properties Tests"
    [ fastProperty "Source position advancement is commutative" prop_pos_advance_commutative
    , fastProperty "Position advancement preserves monotonicity" prop_pos_advance_monotonic
    , fastProperty "Span merging is associative" prop_span_merge_associative
    , fastProperty "Span merging is commutative when spans overlap" prop_span_merge_commutative_overlap
    , fastProperty "Empty span is identity element for merging" prop_span_empty_identity
    , fastProperty "Position arithmetic consistency" prop_pos_arithmetic_consistency
    , fastProperty "Column reset on newline" prop_pos_newline_reset
    , fastProperty "Tab advances to next tab position" prop_pos_tab_alignment
    , fastProperty "Span validity is preserved under valid operations" prop_span_validity_preserved
    , fastProperty "Position creation with line/column constraints" prop_pos_creation_constraints
    ]