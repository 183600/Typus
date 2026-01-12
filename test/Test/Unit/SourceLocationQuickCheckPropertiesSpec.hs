module Test.Unit.SourceLocationQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.Char (isSpace)

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos should be at line 1, column 1, offset 0
prop_start_pos_properties :: Property
prop_start_pos_properties = 
  property $ posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

-- Property: posAfter newline should increment line and reset column
prop_pos_after_newline :: Int -> Int -> Int -> Property
prop_pos_after_newline line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in property $ 
    posLine newPos == line + 1 && 
    posColumn newPos == 1 && 
    posOffset newPos == offset + 1

-- Property: posAfter tab should advance to next tab stop
prop_pos_after_tab :: Int -> Int -> Int -> Property
prop_pos_after_tab line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ 
    posLine newPos == line && 
    posColumn newPos == expectedCol && 
    posOffset newPos == offset + 1

-- Property: posAfter regular character should increment column and offset
prop_pos_after_regular :: Int -> Int -> Int -> Char -> Property
prop_pos_after_regular line col offset c = 
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in property $ 
    if c `notElem` ['\n', '\t']
    then posLine newPos == line && 
         posColumn newPos == col + 1 && 
         posOffset newPos == offset + 1
    else True  -- Property doesn't apply to special characters

-- Property: posAt should create position with specified line and column
prop_pos_at_properties :: Int -> Int -> Property
prop_pos_at_properties line col = 
  let pos = posAt line col
  in property $ 
    posLine pos == line && 
    posColumn pos == col && 
    posOffset pos == 0

-- Property: posAtLineCol should create position with specified line, column, and offset
prop_pos_at_line_col_properties :: Int -> Int -> Int -> Property
prop_pos_at_line_col_properties line col offset = 
  let pos = posAtLineCol line col offset
  in property $ 
    posLine pos == line && 
    posColumn pos == col && 
    posOffset pos == offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan should have start and end at the same position
prop_empty_span_properties :: Int -> Int -> Int -> Property
prop_empty_span_properties line col offset = 
  let pos = SourcePos line col offset
      span = emptySpan pos
  in property $ 
    spanStart span == pos && 
    spanEnd span == pos

-- Property: spanFrom should create empty span at position
prop_span_from_properties :: Int -> Int -> Int -> Property
prop_span_from_properties line col offset = 
  let pos = SourcePos line col offset
      span = spanFrom pos
  in property $ 
    spanStart span == pos && 
    spanEnd span == pos

-- Property: spanTo should create empty span at position
prop_span_to_properties :: Int -> Int -> Int -> Property
prop_span_to_properties line col offset = 
  let pos = SourcePos line col offset
      span = spanTo pos
  in property $ 
    spanStart span == pos && 
    spanEnd span == pos

-- Property: spanBetween should create span with specified start and end
prop_span_between_properties :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_between_properties line1 col1 offset1 line2 col2 offset2 = 
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = spanBetween start end
  in property $ 
    spanStart span == start && 
    spanEnd span == end

-- Property: mergeSpans should contain both original spans
prop_merge_spans_contains_both :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains_both line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 = 
  let span1 = SourceSpan (SourcePos line1 col1 offset1) (SourcePos line2 col2 offset2)
      span2 = SourceSpan (SourcePos line3 col3 offset3) (SourcePos line4 col4 offset4)
      merged = mergeSpans span1 span2
  in property $ 
    spanStart merged <= spanStart span1 && 
    spanStart merged <= spanStart span2 && 
    spanEnd merged >= spanEnd span1 && 
    spanEnd merged >= spanEnd span2
  where
    line4 = line3
    col4 = col3
    offset4 = offset3

-- Property: isValidSpan should return True for valid spans
prop_is_valid_span_valid :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_is_valid_span_valid line1 col1 offset1 line2 col2 offset2 = 
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
  in property $ 
    if start <= end then isValidSpan span else not (isValidSpan span)

-- ============================================================================
-- Located Values Properties
-- ============================================================================

-- Property: locatedAt should create located value at position
prop_located_at_properties :: Int -> Int -> Int -> String -> Property
prop_located_at_properties line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in property $ 
    locValue located == value && 
    locPos located == pos && 
    locSpan located == emptySpan pos

-- Property: locatedWithSpan should create located value with span
prop_located_with_span_properties :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span_properties line1 col1 offset1 line2 col2 offset2 value = 
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      located = locatedWithSpan span value
  in property $ 
    locValue located == value && 
    locPos located == start && 
    locSpan located == span

-- Property: mapLocated should apply function to value but preserve location
prop_map_located_properties :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_map_located_properties line1 col1 offset1 line2 col2 offset2 value = 
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      located = locatedWithSpan span value
      mapped = mapLocated length located
  in property $ 
    locValue mapped == length value && 
    locPos mapped == start && 
    locSpan mapped == span

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos should be equivalent to posAfter
prop_advance_pos_equivalence :: Char -> Int -> Int -> Int -> Property
prop_advance_pos_equivalence c line col offset = 
  let pos = SourcePos line col offset
      posAfterResult = posAfter c pos
      advancePosResult = advancePos c pos
  in property $ posAfterResult == advancePosResult

-- Property: advancePosBy should advance by each character in sequence
prop_advance_pos_by_sequence :: String -> Int -> Int -> Int -> Property
prop_advance_pos_by_sequence chars line col offset = 
  let pos = SourcePos line col offset
      advanceByResult = advancePosBy chars pos
      sequentialResult = foldl (flip advancePos) pos chars
  in property $ advanceByResult == sequentialResult

-- Property: advancePosByLine should only change line number and reset column
prop_advance_pos_by_line_properties :: Int -> Int -> Int -> Int -> Property
prop_advance_pos_by_line_properties line col offset numLines = 
  let pos = SourcePos line col offset
      newPos = advancePosByLine numLines pos
  in property $ 
    posLine newPos == line + numLines && 
    posColumn newPos == 1

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Properties Tests"
  [ testProperty "startPos properties" prop_start_pos_properties
  , testProperty "posAfter newline" prop_pos_after_newline
  , testProperty "posAfter tab" prop_pos_after_tab
  , testProperty "posAfter regular character" prop_pos_after_regular
  , testProperty "posAt properties" prop_pos_at_properties
  , testProperty "posAtLineCol properties" prop_pos_at_line_col_properties
  , testProperty "emptySpan properties" prop_empty_span_properties
  , testProperty "spanFrom properties" prop_span_from_properties
  , testProperty "spanTo properties" prop_span_to_properties
  , testProperty "spanBetween properties" prop_span_between_properties
  , testProperty "mergeSpans contains both" prop_merge_spans_contains_both
  , testProperty "isValidSpan valid" prop_is_valid_span_valid
  , testProperty "locatedAt properties" prop_located_at_properties
  , testProperty "locatedWithSpan properties" prop_located_with_span_properties
  , testProperty "mapLocated properties" prop_map_located_properties
  , testProperty "advancePos equivalence" prop_advance_pos_equivalence
  , testProperty "advancePosBy sequence" prop_advance_pos_by_sequence
  , testProperty "advancePosByLine properties" prop_advance_pos_by_line_properties
  ]