module Test.Unit.TestSourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation

-- Test Properties for Source Location Math

-- Property: startPos should be at line 1, column 1
prop_start_pos_properties :: Property
prop_start_pos_properties = property $ 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

-- Property: posAfter should increase offset by 1
prop_pos_after_increases_offset :: Char -> Int -> Int -> Int -> Property
prop_pos_after_increases_offset c line col offset = property $ 
  let pos = SourcePos line col offset
  in posOffset (posAfter c pos) == offset + 1

-- Property: posAfter should handle newline correctly
prop_pos_after_newline :: Int -> Int -> Int -> Property
prop_pos_after_newline line col offset = property $ 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in posLine newPos == line + 1 && posColumn newPos == 1

-- Property: spanBetweenOrdered should have start <= end
prop_span_between_ordered :: Int -> Int -> Int -> Int -> Property
prop_span_between_ordered line1 col1 line2 col2 = property $ 
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      span = spanBetweenOrdered pos1 pos2
  in spanStart span <= spanEnd span

-- Property: mergeSpans should contain both original spans
prop_merge_spans_contains_both :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains_both line1 col1 line2 col2 line3 col3 line4 col4 = property $ 
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      pos3 = SourcePos line3 col3 0
      pos4 = SourcePos line4 col4 0
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2

-- Property: isValidSpan should return False when start > end
prop_invalid_span :: Int -> Int -> Int -> Int -> Property
prop_invalid_span line1 col1 line2 col2 = property $ 
  let pos1 = SourcePos (line1 + 10) (col1 + 10) 0  -- Ensure pos1 > pos2
      pos2 = SourcePos line2 col2 0
      span = spanBetween pos1 pos2
  in not (isValidSpan span)

tests :: TestTree
tests = testGroup "Test.Unit.TestSourceLocationMathSpec Tests"
  [ testProperty "startPos should be at line 1, column 1" prop_start_pos_properties
  , testProperty "posAfter should increase offset by 1" prop_pos_after_increases_offset
  , testProperty "posAfter should handle newline correctly" prop_pos_after_newline
  , testProperty "spanBetweenOrdered should have start <= end" prop_span_between_ordered
  , testProperty "mergeSpans should contain both original spans" prop_merge_spans_contains_both
  , testProperty "isValidSpan should return False when start > end" prop_invalid_span
  ]