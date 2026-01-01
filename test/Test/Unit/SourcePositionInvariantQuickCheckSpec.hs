{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourcePositionInvariantQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

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
  )

import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- Property: source position components are always positive
prop_source_pos_components_positive :: Positive Int -> Positive Int -> Property
prop_source_pos_components_positive (Positive line) (Positive col) =
  let pos = SourcePos line col
  in property $ sourceLine pos > 0 .&&. sourceColumn pos > 0

-- Property: startPos has L.minimum values
prop_start_pos_minimum :: Property
prop_start_pos_minimum =
  let start = startPos
  in property $ sourceLine start === 1 .&&. sourceColumn start === 1

-- Property: posAt creates position at specified location
prop_pos_at_creates_correct :: Positive Int -> Positive Int -> Property
prop_pos_at_creates_correct (Positive line) (Positive col) =
  let pos = posAt line col
  in property $ sourceLine pos === line .&&. sourceColumn pos === col

-- Property: posAtLineCol is consistent with posAt
prop_pos_at_line_col_consistent :: Positive Int -> Positive Int -> Property
prop_pos_at_line_col_consistent (Positive line) (Positive col) =
  let pos1 = posAt line col
      pos2 = posAtLineCol line col
  in property $ pos1 === pos2

-- Property: posAfter handles newline correctly
prop_pos_after_newline :: Positive Int -> Positive Int -> Property
prop_pos_after_newline (Positive line) (Positive col) =
  let pos = posAt line col
      after = posAfter pos '\n'
  in property $ sourceLine after === line + 1 .&&. sourceColumn after === 1

-- Property: posAfter handles regular character correctly
prop_pos_after_regular_char :: Positive Int -> Positive Int -> Char -> Property
prop_pos_after_regular_char (Positive line) (Positive col) c =
  let pos = posAt line col
      after = posAfter pos c
      isNotNewline = c /= '\n'
  in classify isNotNewline "regular character" $
     property $ if isNotNewline
                   then sourceLine after === line .&&. sourceColumn after === col + 1
                   else True

-- Property: advancePos with empty string returns same position
prop_advance_pos_empty :: Positive Int -> Positive Int -> Property
prop_advance_pos_empty (Positive line) (Positive col) =
  let pos = posAt line col
      advanced = advancePos pos ""
  in property $ advanced === pos

-- Property: advancePosBy with single character behaves like posAfter
prop_advance_pos_by_single_char :: Positive Int -> Positive Int -> Char -> Property
prop_advance_pos_by_single_char (Positive line) (Positive col) c =
  let pos = posAt line col
      after1 = posAfter pos c
      after2 = advancePosBy pos [c]
  in property $ after1 === after2

-- Property: advancePos counts newlines correctly
prop_advance_pos_counts_newlines :: Positive Int -> Positive Int -> String -> Property
prop_advance_pos_counts_newlines (Positive line) (Positive col) text =
  let pos = posAt line col
      advanced = advancePos pos text
      newlineCount = L.length $ L.filter (== '\n') text
  in property $ sourceLine advanced >= line .&&. 
                   sourceLine advanced <= line + newlineCount + 1

-- Property: spanFrom single position creates valid span
prop_span_from_single_pos :: Positive Int -> Positive Int -> Property
prop_span_from_single_pos (Positive line) (Positive col) =
  let pos = posAt line col
      span = spanFrom pos pos
  in property $ isValidSpan span .&&. 
                   spanStart span === pos .&&. 
                   spanEnd span === pos

-- Property: spanBetween maintains correct order
prop_span_between_order :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_between_order (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (sourceLine start <= sourceLine end) .&&. 
                   (sourceLine start < sourceLine end || sourceColumn start <= sourceColumn end)

-- Property: mergeSpans contains both original spans
prop_merge_spans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      valid1 = isValidSpan span1
      valid2 = isValidSpan span2
  in classify (valid1 && valid2) "both valid" $
     property $ if valid1 && valid2
                   then let start1 = spanStart span1
                            end1 = spanEnd span1
                            start2 = spanStart span2
                            end2 = spanEnd span2
                            mergedStart = spanStart merged
                            mergedEnd = spanEnd merged
                        in (sourceLine mergedStart <= min (sourceLine start1) (sourceLine start2)) .&&.
                           (sourceLine mergedEnd >= max (sourceLine end1) (sourceLine end2))
                   else True

-- Property: empty span is invalid
prop_empty_span_invalid :: Property
prop_empty_span_invalid =
  let empty = emptySpan
  in property $ not $ isValidSpan empty

-- Property: position advancement is monotonic for line numbers
prop_pos_advancement_monotonic_line :: Positive Int -> Positive Int -> String -> Property
prop_pos_advancement_monotonic_line (Positive line) (Positive col) text =
  let pos = posAt line col
      advanced = advancePos pos text
  in property $ sourceLine advanced >= sourceLine pos

-- Property: position advancement preserves positivity
prop_pos_advancement_preserves_positivity :: Positive Int -> Positive Int -> String -> Property
prop_pos_advancement_preserves_positivity (Positive line) (Positive col) text =
  let pos = posAt line col
      advanced = advancePos pos text
  in property $ sourceLine advanced > 0 .&&. sourceColumn advanced > 0

tests :: TestTree
tests = testGroup "Source Position Invariant QuickCheck"
  [ fastProperty "source pos components positive" prop_source_pos_components_positive
  , fastProperty "start pos L.minimum" prop_start_pos_minimum
  , fastProperty "pos at creates correct" prop_pos_at_creates_correct
  , fastProperty "pos at line col consistent" prop_pos_at_line_col_consistent
  , fastProperty "pos after newline" prop_pos_after_newline
  , fastProperty "pos after regular char" prop_pos_after_regular_char
  , fastProperty "advance pos empty" prop_advance_pos_empty
  , fastProperty "advance pos by single char" prop_advance_pos_by_single_char
  , fastProperty "advance pos counts newlines" prop_advance_pos_counts_newlines
  , fastProperty "span from single pos" prop_span_from_single_pos
  , fastProperty "span between order" prop_span_between_order
  , fastProperty "merge spans contains both" prop_merge_spans_contains_both
  , fastProperty "empty span invalid" prop_empty_span_invalid
  , fastProperty "pos advancement monotonic line" prop_pos_advancement_monotonic_line
  , fastProperty "pos advancement preserves positivity" prop_pos_advancement_preserves_positivity
  ]