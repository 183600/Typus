{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing #-}
module Test.Unit.SourceLocationComprehensiveQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T

-- | Test that posAfter correctly handles newline characters
prop_posAfter_newline :: Int -> Int -> Int -> Property
prop_posAfter_newline line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in property $ 
    posLine newPos == line + 1 && 
    posColumn newPos == 1 && 
    posOffset newPos == offset + 1

-- | Test that posAfter correctly handles tab characters
prop_posAfter_tab :: Int -> Int -> Int -> Property
prop_posAfter_tab line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ 
    posLine newPos == line && 
    posColumn newPos == expectedCol && 
    posOffset newPos == offset + 1

-- | Test that posAfter correctly handles regular characters
prop_posAfter_regular :: Int -> Int -> Int -> Char -> Property
prop_posAfter_regular line col offset c = 
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in property $ 
    c `notElem` ['\n', '\t'] ==>
    (posLine newPos == line && 
     posColumn newPos == col + 1 && 
     posOffset newPos == offset + 1)

-- | Test that emptySpan creates a span with same start and end
prop_empty_span_same_start_end :: Int -> Int -> Int -> Property
prop_empty_span_same_start_end line col offset = 
  let pos = SourcePos line col offset
      span = emptySpan pos
  in property $ 
    spanStart span == pos && 
    spanEnd span == pos

-- | Test that spanBetween preserves order
prop_span_between_preserves_order :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_between_preserves_order line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in property $ 
    spanStart span == pos1 && 
    spanEnd span == pos2

-- | Test that spanBetweenOrdered always has start <= end
prop_span_between_ordered :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_between_ordered line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
  in property $ 
    spanStart span <= spanEnd span

-- | Test that mergeSpans contains both original spans
prop_merge_spans_contains_both :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains_both line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in property $ 
    spanStart merged <= spanStart span1 && 
    spanEnd merged >= spanEnd span2

-- | Test that isValidSpan correctly identifies valid spans
prop_is_valid_span :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_is_valid_span line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in property $ 
    isValidSpan span == (pos1 <= pos2)

-- | Test that locatedAt creates a located value with correct position
prop_located_at_correct_position :: Int -> Int -> Int -> String -> Property
prop_located_at_correct_position line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in property $ 
    locValue located == value && 
    locPos located == pos && 
    locSpan located == emptySpan pos

-- | Test that locatedWithSpan creates a located value with correct span
prop_located_with_span_correct :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span_correct line1 col1 offset1 line2 col2 offset2 value = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      located = locatedWithSpan span value
  in property $ 
    locValue located == value && 
    locPos located == pos1 && 
    locSpan located == span

-- | Test that mapLocated correctly transforms the value
prop_map_located_transforms_value :: Int -> Int -> Int -> String -> String -> Property
prop_map_located_transforms_value line col offset value1 value2 = 
  let pos = SourcePos line col offset
      located1 = locatedAt pos value1
      located2 = mapLocated (const value2) located1
  in property $ 
    locValue located2 == value2 && 
    locPos located2 == pos && 
    locSpan located2 == emptySpan pos

-- | Test that toErrorLocation creates correct error location
prop_to_error_location :: Int -> Int -> Int -> Property
prop_to_error_location lineNum colNum offset = 
  let pos = SourcePos lineNum colNum offset
      errLoc = toErrorLocation pos
  in property $ 
    filePath errLoc == Nothing && 
    line errLoc == lineNum && 
    column errLoc == colNum && 
    endLine errLoc == Nothing && 
    endColumn errLoc == Nothing

-- | Test that toErrorLocationWithSpan creates correct error location with range
prop_to_error_location_with_span :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_to_error_location_with_span line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      errLoc = toErrorLocationWithSpan span
  in property $ 
    filePath errLoc == Nothing && 
    line errLoc == line1 && 
    column errLoc == col1 && 
    endLine errLoc == Just line2 && 
    endColumn errLoc == Just col2

-- | Test that advancePosBy correctly advances position
prop_advance_pos_by :: Int -> Int -> Int -> String -> Property
prop_advance_pos_by line col offset text = 
  let pos = SourcePos line col offset
      newPos = advancePosBy text pos
  in property $ 
    posOffset newPos == offset + length text

-- | Test that advancePosByText correctly advances position
prop_advance_pos_by_text :: Int -> Int -> Int -> String -> Property
prop_advance_pos_by_text line col offset text = 
  let pos = SourcePos line col offset
      newPos = advancePosByText (T.pack text) pos
  in property $ 
    posOffset newPos == offset + length text

-- | Test that advancePosByLine correctly advances line number
prop_advance_pos_by_line :: Int -> Int -> Int -> Int -> Property
prop_advance_pos_by_line line col offset numLines = 
  let pos = SourcePos line col offset
      newPos = advancePosByLine numLines pos
  in property $ 
    posLine newPos == line + numLines && 
    posColumn newPos == 1

tests :: TestTree
tests = testGroup "SourceLocation Comprehensive QuickCheck Tests"
  [ testProperty "posAfter handles newline" prop_posAfter_newline
  , testProperty "posAfter handles tab" prop_posAfter_tab
  , testProperty "posAfter handles regular characters" prop_posAfter_regular
  , testProperty "emptySpan has same start and end" prop_empty_span_same_start_end
  , testProperty "spanBetween preserves order" prop_span_between_preserves_order
  , testProperty "spanBetweenOrdered has start <= end" prop_span_between_ordered
  , testProperty "mergeSpans contains both spans" prop_merge_spans_contains_both
  , testProperty "isValidSpan correctly identifies valid spans" prop_is_valid_span
  , testProperty "locatedAt creates correct position" prop_located_at_correct_position
  , testProperty "locatedWithSpan creates correct span" prop_located_with_span_correct
  , testProperty "mapLocated transforms value" prop_map_located_transforms_value
  , testProperty "toErrorLocation creates correct error location" prop_to_error_location
  , testProperty "toErrorLocationWithSpan creates correct range" prop_to_error_location_with_span
  , testProperty "advancePosBy correctly advances position" prop_advance_pos_by
  , testProperty "advancePosByText correctly advances position" prop_advance_pos_by_text
  , testProperty "advancePosByLine correctly advances line number" prop_advance_pos_by_line
  ]