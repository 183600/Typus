{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewSourceLocationMathPropertiesSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
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

-- ============================================================================
-- SourceLocation Module QuickCheck Property Tests
-- ============================================================================

-- | Test that startPos has line L.and column 1
prop_startPos_properties :: Bool
prop_startPos_properties = 
    let pos = startPos
    in sourceLine pos == 1 && sourceColumn pos == 1

-- | Test that posAfter advances column by 1 for normal characters
prop_posAfter_advances_column :: Char -> SourcePos -> Bool
prop_posAfter_advances_column c pos = 
    if c == '\n' 
    then sourceColumn (posAfter c pos) == 1
    else sourceColumn (posAfter c pos) == sourceColumn pos + 1

-- | Test that posAfter advances line by 1 for newline
prop_posAfter_advances_line_for_newline :: SourcePos -> Bool
prop_posAfter_advances_line_for_newline pos = 
    let newPos = posAfter '\n' pos
    in sourceLine newPos == sourceLine pos + 1

-- | Test that posAt creates position with correct line L.and column
prop_posAt_correct_line_column :: Int -> Int -> Bool
prop_posAt_correct_line_column line col = 
    let pos = posAt line col
    in sourceLine pos == line && sourceColumn pos == col

-- | Test that posAtLineCol is equivalent to posAt
prop_posAtLineCol_equals_posAt :: Int -> Int -> Bool
prop_posAtLineCol_equals_posAt line col = 
    posAtLineCol line col == posAt line col

-- | Test that emptySpan has same start L.and end at startPos
prop_emptySpan_properties :: Bool
prop_emptySpan_properties = 
    let span = emptySpan
    in spanStart span == startPos && spanEnd span == startPos

-- | Test that spanFrom creates span with correct start
prop_spanFrom_correct_start :: SourcePos -> Bool
prop_spanFrom_correct_start start = 
    let span = spanFrom start
    in spanStart span == start

-- | Test that spanTo creates span with correct end
prop_spanTo_correct_end :: SourcePos -> Bool
prop_spanTo_correct_end end = 
    let span = spanTo end
    in spanEnd span == end

-- | Test that spanBetween creates span with correct start L.and end
prop_spanBetween_correct_bounds :: SourcePos -> SourcePos -> Bool
prop_spanBetween_correct_bounds start end = 
    let span = spanBetween start end
    in spanStart span == start && spanEnd span == end

-- | Test that mergeSpans contains both original spans
prop_mergeSpans_contains_originals :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpans_contains_originals start1 end1 start2 end1 = 
    let span1 = spanBetween start1 end1
        span2 = spanBetween start2 end1
        merged = mergeSpans span1 span2
    in spanStart merged `min` spanStart span1 == spanStart merged &&
       spanEnd merged `max` spanEnd span2 == spanEnd merged

-- | Test that advancePos advances position correctly for single character
prop_advancePos_single_char :: Char -> SourcePos -> Bool
prop_advancePos_single_char c pos = 
    advancePos [c] pos == posAfter c pos

-- | Test that advancePosBy advances position correctly for multiple characters
prop_advancePosBy_multiple_chars :: String -> SourcePos -> Bool
prop_advancePosBy_multiple_chars s pos = 
    advancePosBy s (L.length s) pos == advancePos s pos

-- | Test that isValidSpan correctly identifies valid spans
prop_isValidSpan_start_before_end :: SourcePos -> Bool
prop_isValidSpan_start_before_end start = 
    let end = posAfter 'a' start
        validSpan = spanBetween start end
    in isValidSpan validSpan

-- | Test that span created at same position is valid
prop_span_same_position_valid :: SourcePos -> Bool
prop_span_same_position_valid pos = 
    let span = spanBetween pos pos
    in isValidSpan span

-- | Test that advancePos handles empty string correctly
prop_advancePos_empty_string :: SourcePos -> Bool
prop_advancePos_empty_string pos = 
    advancePos "" pos == pos

-- | Test that advancePos handles newline correctly
prop_advancePos_newline :: SourcePos -> Bool
prop_advancePos_newline pos = 
    let newPos = advancePos "\n" pos
    in sourceLine newPos == sourceLine pos + 1 && sourceColumn newPos == 1

-- ============================================================================
-- Test Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "SourceLocation Math Properties QuickCheck Tests"
  [ QC.testProperty "startPos has line L.and column 1" prop_startPos_properties
  , QC.testProperty "posAfter advances column by 1 for normal characters" prop_posAfter_advances_column
  , QC.testProperty "posAfter advances line by 1 for newline" prop_posAfter_advances_line_for_newline
  , QC.testProperty "posAt creates position with correct line L.and column" prop_posAt_correct_line_column
  , QC.testProperty "posAtLineCol equals posAt" prop_posAtLineCol_equals_posAt
  , QC.testProperty "emptySpan has same start L.and end at startPos" prop_emptySpan_properties
  , QC.testProperty "spanFrom creates span with correct start" prop_spanFrom_correct_start
  , QC.testProperty "spanTo creates span with correct end" prop_spanTo_correct_end
  , QC.testProperty "spanBetween creates span with correct start L.and end" prop_spanBetween_correct_bounds
  , QC.testProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_originals
  , QC.testProperty "advancePos advances position correctly for single character" prop_advancePos_single_char
  , QC.testProperty "advancePosBy advances position correctly for multiple characters" prop_advancePosBy_multiple_chars
  , QC.testProperty "isValidSpan correctly identifies valid spans" prop_isValidSpan_start_before_end
  , QC.testProperty "span created at same position is valid" prop_span_same_position_valid
  , QC.testProperty "advancePos handles empty string correctly" prop_advancePos_empty_string
  , QC.testProperty "advancePos handles newline correctly" prop_advancePos_newline
  ]