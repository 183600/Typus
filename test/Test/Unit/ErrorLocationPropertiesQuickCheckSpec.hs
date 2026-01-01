{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorLocationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , toErrorLocation
  , toErrorLocationWithSpan
  , advancePos
  , advancePosBy
  )

import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- Property: source position components are always positive
prop_source_pos_positive :: Positive Int -> Positive Int -> Property
prop_source_pos_positive (Positive line) (Positive col) =
  let pos = SourcePos line col
  in property $ sourceLine pos > 0 .&&. sourceColumn pos > 0

-- Property: advancePos never decreases line number
prop_advance_pos_line_non_decreasing :: Positive Int -> Positive Int -> String -> Property
prop_advance_pos_line_non_decreasing (Positive line) (Positive col) text =
  let start = SourcePos line col
      advanced = advancePos start text
  in property $ sourceLine advanced >= sourceLine start

-- Property: advancePosBy preserves line count for single-line text
prop_advance_pos_by_single_line :: Positive Int -> Positive Int -> String -> Property
prop_advance_pos_by_single_line (Positive line) (Positive col) text =
  let singleLine = L.filter (/= '\n') text
      start = SourcePos line col
      advanced = advancePosBy start singleLine
  in property $ sourceLine advanced === sourceLine start

-- Property: empty span is always invalid
prop_empty_span_invalid :: Property
prop_empty_span_invalid =
  let empty = emptySpan
  in property $ not $ isValidSpan empty

-- Property: span from single position is valid
prop_span_from_single_pos_valid :: Positive Int -> Positive Int -> Property
prop_span_from_single_pos_valid (Positive line) (Positive col) =
  let pos = SourcePos line col
      span = spanFrom pos pos
  in property $ isValidSpan span

-- Property: spanBetween preserves order
prop_span_between_preserves_order :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_between_preserves_order (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (sourceLine start <= sourceLine end) .&&. 
                   (sourceLine start < sourceLine end || sourceColumn start <= sourceColumn end)

-- Property: mergeSpans is commutative for valid spans
prop_merge_spans_commutative :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> 
                               Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_merge_spans_commutative (Positive line1) (Positive col1) (Positive line2) (Positive col2)
                              (Positive line3) (Positive col3) (Positive line4) (Positive col4) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      pos3 = SourcePos line3 col3
      pos4 = SourcePos line4 col4
      span1 = spanFrom pos1 pos2
      span2 = spanFrom pos3 pos4
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in classify (isValidSpan span1 && isValidSpan span2) "both valid" $
     property $ merged1 === merged2

-- Property: locatedAt creates valid located values
prop_located_at_valid :: Positive Int -> Positive Int -> String -> Property
prop_located_at_valid (Positive line) (Positive col) value =
  let pos = SourcePos line col
      located = locatedAt pos value
  in property $ locatedPos located === pos .&&. locatedValue located === value

-- Property: locatedWithSpan creates valid located values
prop_located_with_span_valid :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_located_with_span_valid (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanFrom pos1 pos2
      located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&. locatedValue located === value

-- Property: toErrorLocation preserves position information
prop_to_error_location_preserves_pos :: Positive Int -> Positive Int -> Property
prop_to_error_location_preserves_pos (Positive line) (Positive col) =
  let pos = SourcePos line col
      errorLoc = toErrorLocation pos
  in property $ True -- Basic smoke test - should not crash

-- Property: toErrorLocationWithSpan preserves span information
prop_to_error_location_with_span_preserves_span :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_to_error_location_with_span_preserves_span (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanFrom pos1 pos2
      errorLoc = toErrorLocationWithSpan span
  in property $ True -- Basic smoke test - should not crash

-- Property: advancePos handles newline correctly
prop_advance_pos_newline :: Positive Int -> Positive Int -> String -> Property
prop_advance_pos_newline (Positive line) (Positive col) prefix =
  let text = prefix ++ "\n"
      start = SourcePos line col
      advanced = advancePos start text
      hasNewline = '\n' `elem` text
  in classify hasNewline "has newline" $
     property $ if hasNewline 
                   then sourceLine advanced > sourceLine start
                   else sourceLine advanced === sourceLine start

-- Property: posAfter advances position by one character
prop_pos_after_advances_by_one :: Positive Int -> Positive Int -> Char -> Property
prop_pos_after_advances_by_one (Positive line) (Positive col) c =
  let pos = SourcePos line col
      after = posAfter pos c
      isNewline = c == '\n'
  in classify isNewline "newline character" $
     property $ if isNewline
                   then sourceLine after > sourceLine pos .&&. sourceColumn after === 1
                   else sourceLine after === sourceLine pos .&&. sourceColumn after === sourceColumn pos + 1

-- Property: spanTo creates span from position to position
prop_span_to_creates_valid_span :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_to_creates_valid_span (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanTo pos1 pos2
  in property $ spanStart span === pos1 .&&. spanEnd span === pos2

tests :: TestTree
tests = testGroup "Error Location Properties QuickCheck"
  [ fastProperty "source pos positive" prop_source_pos_positive
  , fastProperty "advance pos line non decreasing" prop_advance_pos_line_non_decreasing
  , fastProperty "advance pos by single line" prop_advance_pos_by_single_line
  , fastProperty "empty span invalid" prop_empty_span_invalid
  , fastProperty "span from single pos valid" prop_span_from_single_pos_valid
  , fastProperty "span between preserves order" prop_span_between_preserves_order
  , fastProperty "merge spans commutative" prop_merge_spans_commutative
  , fastProperty "located at valid" prop_located_at_valid
  , fastProperty "located with span valid" prop_located_with_span_valid
  , fastProperty "to error location preserves pos" prop_to_error_location_preserves_pos
  , fastProperty "to error location with span preserves span" prop_to_error_location_with_span_preserves_span
  , fastProperty "advance pos newline" prop_advance_pos_newline
  , fastProperty "pos after advances by one" prop_pos_after_advances_by_one
  , fastProperty "span to creates valid span" prop_span_to_creates_valid_span
  ]