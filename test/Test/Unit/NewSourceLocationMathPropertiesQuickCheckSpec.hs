{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationMathPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Data.Char (isSpace)
import qualified Data.List as Data.List
import Data.List (sort, nub)

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
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 1000)
    let end = start { posOffset = posOffset start + endOffset }
    return $ SourceSpan start end

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos is the minimal position
prop_startPos_is_minimal :: SourcePos -> Property
prop_startPos_is_minimal pos =
  property $ startPos <= pos

-- Property: posAfter newline increments line L.and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter tab aligns to next tab stop (8-char boundary)
prop_posAfter_tab_alignment :: SourcePos -> Property
prop_posAfter_tab_alignment pos =
  let pos' = pos { posColumn = posColumn pos `mod` 8 + 1 } -- Ensure column is in 1-8 range
      newPos = posAfter '\t' pos'
      expectedColumn = ((posColumn pos' - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedColumn .&&.
             posOffset newPos === posOffset pos' + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  char `notElem` "\n\t" ==> 
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates position with L.all fields correct
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. 
             posColumn pos === col .&&.
             posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan creates span with same start L.and end
prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_span_from_creates_empty :: SourcePos -> Property
prop_span_from_creates_empty pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_span_to_creates_empty :: SourcePos -> Property
prop_span_to_creates_empty pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with correct start L.and end
prop_span_between_correct :: SourcePos -> SourcePos -> Property
prop_span_between_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_merge_spans_covers_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_covers_both span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&.
             spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: mergeSpans is commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan checks start <= end
prop_is_valid_span_check :: SourcePos -> SourcePos -> Property
prop_is_valid_span_check start end =
  let span = spanBetween start end
      valid = start <= end
  in property $ isValidSpan span === valid

-- ============================================================================
-- Located Values Properties
-- ============================================================================

-- Property: locatedAt creates located value with empty span
prop_located_at_empty_span :: SourcePos -> Int -> Property
prop_located_at_empty_span pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located value with given span
prop_located_with_span :: SourceSpan -> String -> Property
prop_located_with_span span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated preserves location but transforms value
prop_map_located_preserves_location :: SourceSpan -> Int -> Property
prop_map_located_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedSpan mapped === locatedSpan located .&&.
             locatedPos mapped === locatedPos located .&&.
             locatedValue mapped === value * 2

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos is same as posAfter
prop_advance_pos_equals_posAfter :: SourcePos -> Char -> Property
prop_advance_pos_equals_posAfter pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy with empty string returns same position
prop_advance_pos_by_empty :: SourcePos -> Property
prop_advance_pos_by_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy is equivalent to sequential posAfter
prop_advance_pos_by_sequential :: SourcePos -> String -> Property
prop_advance_pos_by_sequential pos chars =
  let sequential = L.foldl (flip posAfter) pos chars
      direct = advancePosBy chars pos
  in property $ sequential === direct

-- Property: advancePosBy with same character multiple times
prop_advance_pos_by_repeated :: SourcePos -> Char -> Int -> Property
prop_advance_pos_by_repeated pos char count =
  count >= 0 && count <= 100 ==>
  let repeated = replicate count char
      result = advancePosBy repeated pos
  in property $ posOffset result === posOffset pos + count

-- Property: advancePosByLine preserves column L.and increments offset
prop_advance_pos_by_line :: SourcePos -> Int -> Property
prop_advance_pos_by_line pos numLines =
  numLines >= 0 && numLines <= 100 ==>
  let newPos = advancePosByLine numLines pos
  in property $ posLine newPos === posLine pos + numLines .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + numLines

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

-- Property: toErrorLocation creates correct error location
prop_to_error_location_correct :: SourcePos -> Property
prop_to_error_location_correct pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos .&&.
             endLine errLoc === Nothing .&&.
             endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan creates error location with range
prop_to_error_location_with_span :: SourceSpan -> Property
prop_to_error_location_with_span span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&.
             column errLoc === posColumn start .&&.
             endLine errLoc === Just (posLine end) .&&.
             endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Mathematical Properties
-- ============================================================================

-- Property: Position advancement is monotonic in offset
prop_pos_advancement_monotonic :: SourcePos -> String -> Property
prop_pos_advancement_monotonic pos chars =
  let newPos = advancePosBy chars pos
  in property $ posOffset newPos >= posOffset pos

-- Property: Span merging is idempotent for identical spans
prop_merge_identical_spans_idempotent :: SourceSpan -> Property
prop_merge_identical_spans_idempotent span =
  mergeSpans span span === span

-- Property: Span L.length is non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let start = spanStart span
      end = spanEnd span
      L.length = posOffset end - posOffset start
  in property $ L.length >= 0

-- Property: Empty span has zero L.length
prop_empty_span_zero_length :: SourcePos -> Property
prop_empty_span_zero_length pos =
  let span = emptySpan pos
      start = spanStart span
      end = spanEnd span
  in property $ posOffset start === posOffset end

-- Property: Position comparison is consistent with offset
prop_pos_comparison_offset :: SourcePos -> SourcePos -> Property
prop_pos_comparison_offset pos1 pos2 =
  let offsetComparison = compare (posOffset pos1) (posOffset pos2)
      posComparison = compare pos1 pos2
  in property $ offsetComparison === posComparison

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Advancing position by newlines updates line count correctly
prop_advance_by_newlines_line_count :: SourcePos -> Int -> Property
prop_advance_by_newlines_line_count pos numNewlines =
  numNewlines >= 0 && numNewlines <= 50 ==>
  let newlines = replicate numNewlines '\n'
      newPos = advancePosBy newlines pos
  in property $ posLine newPos === posLine pos + numNewlines

-- Property: Tab advancement respects tab boundaries
prop_tab_advancement_boundaries :: SourcePos -> Property
prop_tab_advancement_boundaries pos =
  let pos' = pos { posColumn = 1 } -- Start at column 1
      newPos = posAfter '\t' pos'
  in property $ posColumn newPos === 9 -- First tab stop at column 9

-- Property: Large line L.and column numbers are handled correctly
prop_large_line_column_numbers :: Property
prop_large_line_column_numbers =
  let largePos = posAt 10000 10000
  in property $ posLine largePos === 10000 .&&. posColumn largePos === 10000

-- Property: Zero offset position is valid
prop_zero_offset_valid :: Property
prop_zero_offset_valid =
  let zeroOffsetPos = posAtLineCol 1 1 0
  in property $ posOffset zeroOffsetPos === 0

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Source Location Math Properties QuickCheck Tests"
  [ testGroup "Source Position Properties"
    [ fastProperty "startPos is minimal" prop_startPos_is_minimal
    , fastProperty "posAfter newline behavior" prop_posAfter_newline
    , fastProperty "posAfter tab alignment" prop_posAfter_tab_alignment
    , fastProperty "posAfter regular character" prop_posAfter_regular_char
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    ]

  , testGroup "Source Span Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_empty_span_same_start_end
    , fastProperty "spanFrom creates empty span" prop_span_from_creates_empty
    , fastProperty "spanTo creates empty span" prop_span_to_creates_empty
    , fastProperty "spanBetween creates correct span" prop_span_between_correct
    , fastProperty "mergeSpans covers both spans" prop_merge_spans_covers_both
    , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
    , fastProperty "mergeSpans is associative" prop_merge_spans_associative
    , fastProperty "isValidSpan checks correctly" prop_is_valid_span_check
    ]

  , testGroup "Located Values Properties"
    [ fastProperty "locatedAt creates empty span" prop_located_at_empty_span
    , fastProperty "locatedWithSpan creates correct located value" prop_located_with_span
    , fastProperty "mapLocated preserves location" prop_map_located_preserves_location
    ]

  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos equals posAfter" prop_advance_pos_equals_posAfter
    , fastProperty "advancePosBy with empty string" prop_advance_pos_by_empty
    , fastProperty "advancePosBy is sequential" prop_advance_pos_by_sequential
    , fastProperty "advancePosBy with repeated chars" prop_advance_pos_by_repeated
    , fastProperty "advancePosByLine behavior" prop_advance_pos_by_line
    ]

  , testGroup "Error Location Conversion Properties"
    [ fastProperty "toErrorLocation creates correct location" prop_to_error_location_correct
    , fastProperty "toErrorLocationWithSpan creates range" prop_to_error_location_with_span
    ]

  , testGroup "Mathematical Properties"
    [ fastProperty "position advancement is monotonic" prop_pos_advancement_monotonic
    , fastProperty "merge identical spans is idempotent" prop_merge_identical_spans_idempotent
    , fastProperty "span L.length is non-negative" prop_span_length_non_negative
    , fastProperty "empty span has zero L.length" prop_empty_span_zero_length
    , fastProperty "position comparison consistent with offset" prop_pos_comparison_offset
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "advancing by newlines updates line count" prop_advance_by_newlines_line_count
    , fastProperty "tab advancement respects boundaries" prop_tab_advancement_boundaries
    , fastProperty "large line L.and column numbers" prop_large_line_column_numbers
    , fastProperty "zero offset position is valid" prop_zero_offset_valid
    ]
  ]