{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements)

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

import qualified Data.Text as T
import Data.Char (isSpace)

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
    -- Ensure end is not before start
    lineDiff <- choose (0, 100)
    columnDiff <- choose (0, 100)
    offsetDiff <- choose (0, 10000)
    let end = SourcePos 
          { posLine = posLine start + lineDiff
          , posColumn = if lineDiff == 0 then posColumn start + columnDiff else columnDiff + 1
          , posOffset = posOffset start + offsetDiff
          }
    return $ SourceSpan start end

-- ============================================================================
-- Position Properties
-- ============================================================================

-- Property: startPos has consistent values
prop_start_pos_consistent :: Property
prop_start_pos_consistent =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter newline increments line and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter tab advances to next tab stop
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedColumn .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character increments column
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular char pos =
  char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: posAtLineCol creates position with all fields set
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- Property: advancePos is same as posAfter
prop_advancePos_equals_posAfter :: Char -> SourcePos -> Property
prop_advancePos_equals_posAfter char pos =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy empty string doesn't change position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with repeated posAfter
prop_advancePosBy_consistent :: String -> SourcePos -> Property
prop_advancePosBy_consistent chars pos =
  let advanced = advancePosBy chars pos
      manual = foldl (flip posAfter) pos chars
  in property $ advanced === manual

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistent :: String -> SourcePos -> Property
prop_advancePosByText_consistent text pos =
  advancePosByText (T.pack text) pos === advancePosBy text pos

-- Property: advancePosByLine only changes line and resets column
prop_advancePosByLine_correct :: Int -> SourcePos -> Property
prop_advancePosByLine_correct numLines pos =
  numLines >= 0 ==>
  let newPos = advancePosByLine numLines pos
  in property $ posLine newPos === posLine pos + numLines .&&.
             posColumn newPos === 1

-- ============================================================================
-- Span Properties
-- ============================================================================

-- Property: emptySpan has same start and end
prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos =
  spanFrom pos === emptySpan pos

-- Property: spanTo creates empty span at position
prop_spanTo_equals_emptySpan :: SourcePos -> Property
prop_spanTo_equals_emptySpan pos =
  spanTo pos === emptySpan pos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
             spanEnd merged >= spanEnd span1 .&&.
             spanStart merged <= spanStart span2 .&&.
             spanEnd merged >= spanEnd span2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Property
prop_isValidSpan_correct start end =
  let span = spanBetween start end
  in property $ isValidSpan span === (start <= end)

-- ============================================================================
-- Located Value Properties
-- ============================================================================

-- Property: locatedAt creates located value with empty span
prop_locatedAt_empty_span :: SourcePos -> Int -> Property
prop_locatedAt_empty_span pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located value with given span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated preserves span but changes value
prop_mapLocated_preserves_span :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_span span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedSpan mapped === span .&&.
             locatedValue mapped === value * 2

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: toErrorLocation preserves line and column
prop_toErrorLocation_preserves_pos :: SourcePos -> Property
prop_toErrorLocation_preserves_pos pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan preserves start and end positions
prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === posLine (spanStart span) .&&.
             column errLoc === posColumn (spanStart span) .&&.
             endLine errLoc === Just (posLine (spanEnd span)) .&&.
             endColumn errLoc === Just (posColumn (spanEnd span))

-- ============================================================================
-- Advanced Mathematical Properties
-- ============================================================================

-- Property: Position advancement is monotonic in offset
prop_advancePos_monotonic_offset :: String -> SourcePos -> Property
prop_advancePos_monotonic_offset chars pos =
  let advanced = advancePosBy chars pos
  in property $ posOffset advanced >= posOffset pos

-- Property: Span length is non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let length = posOffset (spanEnd span) - posOffset (spanStart span)
  in property $ length >= 0

-- Property: Merged span length is at least as large as individual spans
prop_mergeSpans_length_increasing :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_length_increasing span1 span2 =
  let merged = mergeSpans span1 span2
      len1 = posOffset (spanEnd span1) - posOffset (spanStart span1)
      len2 = posOffset (spanEnd span2) - posOffset (spanStart span2)
      lenMerged = posOffset (spanEnd merged) - posOffset (spanStart merged)
  in property $ lenMerged >= len1 .&&. lenMerged >= len2

-- Property: Position ordering is consistent with offset ordering
prop_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent pos1 pos2 =
  property $ (pos1 <= pos2) === (posOffset pos1 <= posOffset pos2)

-- Property: Empty spans have zero length
prop_empty_span_zero_length :: SourcePos -> Property
prop_empty_span_zero_length pos =
  let span = emptySpan pos
      length = posOffset (spanEnd span) - posOffset (spanStart span)
  in property $ length === 0

-- Property: Advancing by newline affects line number correctly
prop_advanceBy_newline_affects_line :: Int -> SourcePos -> Property
prop_advanceBy_newline_affects_line numNewlines pos =
  numNewlines >= 0 && numNewlines <= 100 ==>
  let newlines = replicate numNewlines '\n'
      advanced = advancePosBy newlines pos
  in property $ posLine advanced === posLine pos + numNewlines .&&.
             posColumn advanced === 1

-- Property: Located values preserve position ordering
prop_located_preserves_ordering :: SourcePos -> SourcePos -> Int -> Int -> Property
prop_located_preserves_ordering pos1 pos2 val1 val2 =
  let loc1 = locatedAt pos1 val1
      loc2 = locatedAt pos2 val2
  in property $ (pos1 <= pos2) === (locatedPos loc1 <= locatedPos loc2)

-- Property: Span merging with empty span returns original span
prop_mergeSpans_empty_identity :: SourceSpan -> Property
prop_mergeSpans_empty_identity span =
  let empty = emptySpan (spanStart span)
  in property $ mergeSpans span empty === span

-- Property: Position advancement by tabs maintains tab alignment
prop_advance_by_tab_alignment :: SourcePos -> Property
prop_advance_by_tab_alignment pos =
  let afterTab = posAfter '\t' pos
  in property $ posColumn afterTab `mod` 8 === 1 .||. posColumn afterTab === 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Math Properties Tests"
  [ testGroup "Position Properties"
    [ fastProperty "startPos has consistent values" prop_start_pos_consistent
    , fastProperty "posAfter newline increments line and resets column" prop_posAfter_newline
    , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab
    , fastProperty "posAfter regular character increments column" prop_posAfter_regular
    , fastProperty "posAt creates position with correct line and column" prop_posAt_correct
    , fastProperty "posAtLineCol creates position with all fields set" prop_posAtLineCol_correct
    , fastProperty "advancePos is same as posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy empty string doesn't change position" prop_advancePosBy_empty
    , fastProperty "advancePosBy is consistent with repeated posAfter" prop_advancePosBy_consistent
    , fastProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine only changes line and resets column" prop_advancePosByLine_correct
    ]

  , testGroup "Span Properties"
    [ fastProperty "emptySpan has same start and end" prop_empty_span_same_start_end
    , fastProperty "spanFrom creates empty span at position" prop_spanFrom_equals_emptySpan
    , fastProperty "spanTo creates empty span at position" prop_spanTo_equals_emptySpan
    , fastProperty "spanBetween creates span with correct start and end" prop_spanBetween_correct
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan checks start <= end" prop_isValidSpan_correct
    ]

  , testGroup "Located Value Properties"
    [ fastProperty "locatedAt creates located value with empty span" prop_locatedAt_empty_span
    , fastProperty "locatedWithSpan creates located value with given span" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves span but changes value" prop_mapLocated_preserves_span
    ]

  , testGroup "Error Location Properties"
    [ fastProperty "toErrorLocation preserves line and column" prop_toErrorLocation_preserves_pos
    , fastProperty "toErrorLocationWithSpan preserves start and end positions" prop_toErrorLocationWithSpan_preserves_span
    ]

  , testGroup "Advanced Mathematical Properties"
    [ fastProperty "Position advancement is monotonic in offset" prop_advancePos_monotonic_offset
    , fastProperty "Span length is non-negative" prop_span_length_non_negative
    , fastProperty "Merged span length is at least as large as individual spans" prop_mergeSpans_length_increasing
    , fastProperty "Position ordering is consistent with offset ordering" prop_position_ordering_consistent
    , fastProperty "Empty spans have zero length" prop_empty_span_zero_length
    , fastProperty "Advancing by newline affects line number correctly" prop_advanceBy_newline_affects_line
    , fastProperty "Located values preserve position ordering" prop_located_preserves_ordering
    , fastProperty "Span merging with empty span returns original span" prop_mergeSpans_empty_identity
    , fastProperty "Position advancement by tabs maintains tab alignment" prop_advance_by_tab_alignment
    ]
  ]