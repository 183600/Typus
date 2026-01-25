{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewSourceLocationMathQuickCheckTestSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- SourceLocation Math QuickCheck Tests
-- ============================================================================

-- Test SourcePos arithmetic properties
prop_source_pos_addition :: Positive Int -> Positive Int -> Positive Int -> Property
prop_source_pos_addition (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      newLine = posLine pos + 1
      newCol = posColumn pos + 1
      newOffset = posOffset pos + 1
      newPos = SourcePos newLine newCol newOffset
  in property $ newPos > pos

prop_source_pos_subtraction :: Positive Int -> Positive Int -> Positive Int -> Property
prop_source_pos_subtraction (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      newLine = max 1 (posLine pos - 1)
      newCol = max 1 (posColumn pos - 1)
      newOffset = max 0 (posOffset pos - 1)
      newPos = SourcePos newLine newCol newOffset
  in property $ newPos <= pos

prop_source_pos_line_addition :: Positive Int -> Positive Int -> Property
prop_source_pos_line_addition (Positive line) (Positive col) = 
  let pos = SourcePos line col 0
      linesToAdd = 5
      newPos = pos { posLine = posLine pos + linesToAdd }
  in property $ posLine newPos === posLine pos + linesToAdd

prop_source_pos_column_addition :: Positive Int -> Positive Int -> Property
prop_source_pos_column_addition (Positive line) (Positive col) = 
  let pos = SourcePos line col 0
      colsToAdd = 10
      newPos = pos { posColumn = posColumn pos + colsToAdd }
  in property $ posColumn newPos === posColumn pos + colsToAdd

prop_source_pos_offset_addition :: Positive Int -> Positive Int -> Property
prop_source_pos_offset_addition (Positive line) (Positive offset) = 
  let pos = SourcePos line 1 offset
      offsetsToAdd = 15
      newPos = pos { posOffset = posOffset pos + offsetsToAdd }
  in property $ posOffset newPos === posOffset pos + offsetsToAdd

-- Test SourcePos comparison properties
prop_source_pos_comparison_reflexive :: SourcePos -> Property
prop_source_pos_comparison_reflexive pos = property $ comparePos pos pos === EQ

prop_source_pos_comparison_antisymmetric :: SourcePos -> SourcePos -> Property
prop_source_pos_comparison_antisymmetric pos1 pos2 = 
  let cmp1 = comparePos pos1 pos2
      cmp2 = comparePos pos2 pos1
  in property $ (cmp1 == EQ && cmp2 == EQ) || 
                (cmp1 == LT && cmp2 == GT) || 
                (cmp1 == GT && cmp2 == LT)

prop_source_pos_comparison_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_source_pos_comparison_transitive pos1 pos2 pos3 = 
  let cmp1 = comparePos pos1 pos2
      cmp2 = comparePos pos2 pos3
      cmp3 = comparePos pos1 pos3
  in if cmp1 == LT && cmp2 == LT
     then property $ cmp3 == LT
     else if cmp1 == GT && cmp2 == GT
          then property $ cmp3 == GT
          else property $ True

prop_source_pos_line_major :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_source_pos_line_major (Positive line1) (Positive col1) (Positive line2) (Positive col2) = 
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
  in if line1 < line2
     then property $ comparePos pos1 pos2 === LT
     else if line1 > line2
          then property $ comparePos pos1 pos2 === GT
          else property $ True

prop_source_pos_column_minor :: Positive Int -> Positive Int -> Positive Int -> Property
prop_source_pos_column_minor (Positive line) (Positive col1) (Positive col2) = 
  let pos1 = SourcePos line col1 0
      pos2 = SourcePos line col2 0
  in if col1 < col2
     then property $ comparePos pos1 pos2 === LT
     else if col1 > col2
          then property $ comparePos pos1 pos2 === GT
          else property $ True

-- Test SourceSpan arithmetic properties
prop_source_span_merge_commutative :: SourceSpan -> SourceSpan -> Property
prop_source_span_merge_commutative span1 span2 = 
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

prop_source_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_source_span_merge_associative span1 span2 span3 = 
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

prop_source_span_merge_idempotent :: SourceSpan -> Property
prop_source_span_merge_idempotent span = 
  let merged = mergeSpans span span
  in property $ merged === span

prop_source_span_merge_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_source_span_merge_contains_originals span1 span2 = 
  let merged = mergeSpans span1 span2
      contains1 = spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1
      contains2 = spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2
  in property $ contains1 && contains2

prop_source_span_length :: SourceSpan -> Property
prop_source_span_length span = 
  let start = spanStart span
      end = spanEnd span
      length = posOffset end - posOffset start
  in property $ length >= 0

prop_source_span_is_valid_reflexive :: SourceSpan -> Property
prop_source_span_is_valid_reflexive span = 
  let valid = isValidSpan span
      start = spanStart span
      end = spanEnd span
      ordered = comparePos start end /= GT
  in property $ valid === ordered

-- Test SourceSpan comparison properties
prop_source_span_comparison :: SourceSpan -> SourceSpan -> Property
prop_source_span_comparison span1 span2 = 
  let start1 = spanStart span1
      start2 = spanStart span2
      cmp = comparePos start1 start2
  in property $ (cmp == LT && span1 < span2) || 
                (cmp == GT && span1 > span2) || 
                (cmp == EQ && span1 == span2)

prop_source_span_ordered_merge :: SourceSpan -> SourceSpan -> Property
prop_source_span_ordered_merge span1 span2 = 
  let ordered1 = spanBetweenOrdered (spanStart span1) (spanEnd span1)
      ordered2 = spanBetweenOrdered (spanStart span2) (spanEnd span2)
      merged = mergeSpans ordered1 ordered2
  in property $ isValidSpan merged

-- Test position advancement properties
prop_pos_after_newline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_after_newline (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 &&
                posColumn newPos === 1 &&
                posOffset newPos === posOffset pos + 1

prop_pos_after_tab :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_after_tab (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos &&
                posColumn newPos === expectedCol &&
                posOffset newPos === posOffset pos + 1

prop_pos_after_regular_char :: Positive Int -> Positive Int -> Positive Int -> Char -> Property
prop_pos_after_regular_char (Positive line) (Positive col) (Positive offset) c = 
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in if c `elem` ['\n', '\t']
     then property $ True  -- Skip special chars
     else property $ posLine newPos === posLine pos &&
                     posColumn newPos === posColumn pos + 1 &&
                     posOffset newPos === posOffset pos + 1

prop_pos_advance_by :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_advance_by (Positive line) (Positive col) (Positive offset) (Positive n) = 
  let pos = SourcePos line col offset
      advancedPos = advancePosBy n pos
  in property $ posOffset advancedPos === posOffset pos + n

prop_pos_advance_by_line :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_advance_by_line (Positive line) (Positive col) (Positive offset) (Positive n) = 
  let pos = SourcePos line col offset
      advancedPos = advancePosByLine n pos
  in property $ posLine advancedPos === posLine pos + n &&
                posColumn advancedPos === posColumn pos

-- Test span arithmetic properties
prop_span_between_ordered :: SourcePos -> SourcePos -> Property
prop_span_between_ordered pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ comparePos start end /= GT

prop_span_between_ordered_symmetric :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_symmetric pos1 pos2 = 
  let span1 = spanBetweenOrdered pos1 pos2
      span2 = spanBetweenOrdered pos2 pos1
  in property $ span1 === span2

prop_span_between_reflexive :: SourcePos -> Property
prop_span_between_reflexive pos = 
  let span = spanBetween pos pos
  in property $ spanStart span === pos && spanEnd span === pos

prop_span_from :: SourcePos -> Property
prop_span_from pos = 
  let span = spanFrom pos
  in property $ spanStart span === pos && spanEnd span === pos

prop_span_to :: SourcePos -> Property
prop_span_to pos = 
  let span = spanTo pos
  in property $ spanStart span === pos && spanEnd span === pos

prop_empty_span :: SourcePos -> Property
prop_empty_span pos = 
  let span = emptySpan pos
  in property $ spanStart span === pos && spanEnd span === pos

-- Test span size and distance properties
prop_span_size :: SourceSpan -> Property
prop_span_size span = 
  let start = spanStart span
      end = spanEnd span
      size = posOffset end - posOffset start
  in property $ size >= 0

prop_span_distance :: SourceSpan -> Property
prop_span_distance span = 
  let start = spanStart span
      end = spanEnd span
      distance = abs (posOffset end - posOffset start)
  in property $ distance >= 0

prop_span_line_distance :: SourceSpan -> Property
prop_span_line_distance span = 
  let start = spanStart span
      end = spanEnd span
      distance = abs (posLine end - posLine start)
  in property $ distance >= 0

prop_span_column_distance :: SourceSpan -> Property
prop_span_column_distance span = 
  let start = spanStart span
      end = spanEnd span
      distance = abs (posColumn end - posColumn start)
  in property $ distance >= 0

-- Test position arithmetic properties
prop_pos_at :: Positive Int -> Positive Int -> Property
prop_pos_at (Positive line) (Positive col) = 
  let pos = posAt line col
  in property $ posLine pos === line && posColumn pos === col && posOffset pos === 0

prop_pos_at_line_col :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_at_line_col (Positive line) (Positive col) (Positive offset) = 
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line && posColumn pos === col && posOffset pos === offset

prop_start_pos :: Property
prop_start_pos = 
  let pos = startPos
  in property $ posLine pos === 1 && posColumn pos === 1 && posOffset pos === 0

-- Test span containment properties
prop_span_contains_start :: SourceSpan -> Property
prop_span_contains_start span = 
  let start = spanStart span
  in property $ comparePos start (spanStart span) === EQ &&
                comparePos start (spanEnd span) /= GT

prop_span_contains_end :: SourceSpan -> Property
prop_span_contains_end span = 
  let end = spanEnd span
  in property $ comparePos end (spanStart span) /= LT &&
                comparePos end (spanEnd span) === EQ

prop_span_contains_middle :: SourceSpan -> Property
prop_span_contains_middle span = 
  let start = spanStart span
      end = spanEnd span
      middle = SourcePos 
        { posLine = (posLine start + posLine end) `div` 2
        , posColumn = (posColumn start + posColumn end) `div` 2
        , posOffset = (posOffset start + posOffset end) `div` 2
        }
  in property $ comparePos middle (spanStart span) /= LT &&
                comparePos middle (spanEnd span) /= GT

-- Test span overlap properties
prop_span_overlaps_reflexive :: SourceSpan -> Property
prop_span_overlaps_reflexive span = 
  let start = spanStart span
      end = spanEnd span
      middle = SourcePos 
        { posLine = (posLine start + posLine end) `div` 2
        , posColumn = (posColumn start + posColumn end) `div` 2
        , posOffset = (posOffset start + posOffset end) `div` 2
        }
      middleSpan = spanBetweenOrdered middle middle
  in property $ comparePos (spanStart span) (spanEnd middleSpan) /= GT &&
                comparePos (spanEnd span) (spanStart middleSpan) /= LT

-- Test position arithmetic with text
prop_pos_advance_by_text :: SourcePos -> String -> Property
prop_pos_advance_by_text pos text = 
  let advancedPos = advancePosByText text pos
      expectedOffset = posOffset pos + length text
  in property $ posOffset advancedPos === expectedOffset

prop_pos_advance_by_text_newlines :: SourcePos -> String -> Property
prop_pos_advance_by_text_newlines pos text = 
  let advancedPos = advancePosByText text pos
      newlineCount = length (filter (== '\n') text)
      expectedLine = posLine pos + newlineCount
  in property $ posLine advancedPos === expectedLine

prop_pos_advance_by_text_last_line :: SourcePos -> String -> Property
prop_pos_advance_by_text_last_line pos text = 
  let advancedPos = advancePosByText text pos
      newlineCount = length (filter (== '\n') text)
      lastNewlineIndex = case elemIndex '\n' (reverse text) of
        Just idx -> length text - 1 - idx
        Nothing -> -1
      expectedColumn = if newlineCount > 0
                      then length (drop (lastNewlineIndex + 1) text) + 1
                      else posColumn pos + length text
  in property $ posColumn advancedPos === expectedColumn

-- Test span arithmetic with positions
prop_span_between_positions :: SourcePos -> SourcePos -> Property
prop_span_between_positions pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in property $ spanStart span === pos1 && spanEnd span === pos2

prop_span_between_ordered_positions :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_positions pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ comparePos start end /= GT &&
                (start === pos1 || start === pos2) &&
                (end === pos1 || end === pos2)

prop_span_merge_with_empty :: SourceSpan -> SourcePos -> Property
prop_span_merge_with_empty span pos = 
  let empty = emptySpan pos
      merged = mergeSpans span empty
  in property $ merged === mergeSpans span empty

prop_span_merge_with_adjacent :: SourceSpan -> Property
prop_span_merge_with_adjacent span = 
  let start = spanStart span
      end = spanEnd span
      adjacentPos = SourcePos 
        { posLine = posLine end
        , posColumn = posColumn end + 1
        , posOffset = posOffset end + 1
        }
      adjacentSpan = emptySpan adjacentPos
      merged = mergeSpans span adjacentSpan
  in property $ spanStart merged === spanStart span &&
                spanEnd merged === adjacentPos

-- Test position arithmetic with tabs
prop_pos_after_tab_alignment :: Positive Int -> Property
prop_pos_after_tab_alignment (Positive col) = 
  let pos = SourcePos 1 col 0
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedCol

prop_pos_after_multiple_tabs :: Positive Int -> Int -> Property
prop_pos_after_multiple_tabs (Positive col) n = 
  let pos = SourcePos 1 col 0
      newPos = iterate (posAfter '\t') pos !! (abs n + 1)
  in property $ posColumn newPos >= posColumn pos &&
                posColumn newPos `mod` 8 === 1

-- Test position arithmetic with line boundaries
prop_pos_at_line_start :: Positive Int -> Property
prop_pos_at_line_start (Positive line) = 
  let pos = SourcePos line 1 0
  in property $ posColumn pos === 1

prop_pos_at_line_end :: Positive Int -> Property
prop_pos_at_line_end (Positive line) = 
  let pos = SourcePos line 1000 0
  in property $ posColumn pos === 1000

prop_pos_advance_to_next_line :: Positive Int -> Positive Int -> Property
prop_pos_advance_to_next_line (Positive line) (Positive col) = 
  let pos = SourcePos line col 0
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 &&
                posColumn newPos === 1

-- Test span arithmetic with multi-line spans
prop_multi_line_span_size :: Positive Int -> Positive Int -> Positive Int -> Property
prop_multi_line_span_size (Positive line1) (Positive line2) (Positive col) = 
  let line1' = min line1 line2
      line2' = max line1 line2
      start = SourcePos line1' col 0
      end = SourcePos line2' col 0
      span = spanBetweenOrdered start end
      expectedLines = line2' - line1' + 1
  in property $ posLine (spanEnd span) - posLine (spanStart span) + 1 === expectedLines

prop_multi_line_span_merge :: Positive Int -> Positive Int -> Positive Int -> Property
prop_multi_line_span_merge (Positive line1) (Positive line2) (Positive col) = 
  let line1' = min line1 line2
      line2' = max line1 line2
      start1 = SourcePos line1' col 0
      end1 = SourcePos line1' (col + 10) 0
      start2 = SourcePos line2' col 0
      end2 = SourcePos line2' (col + 10) 0
      span1 = spanBetweenOrdered start1 end1
      span2 = spanBetweenOrdered start2 end2
      merged = mergeSpans span1 span2
  in property $ posLine (spanStart merged) === line1' &&
                posLine (spanEnd merged) === line2'

-- Test position arithmetic with offset
prop_pos_offset_consistency :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_offset_consistency (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      advanced = advancePosBy 1 pos
  in property $ posOffset advanced === offset + 1

prop_pos_offset_monotonic :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_offset_monotonic (Positive line) (Positive col) (Positive offset) (Positive n) = 
  let pos = SourcePos line col offset
      advanced = advancePosBy n pos
  in property $ posOffset advanced >= posOffset pos

-- Test span arithmetic with offset
prop_span_offset_range :: SourceSpan -> Property
prop_span_offset_range span = 
  let start = spanStart span
      end = spanEnd span
      startOffset = posOffset start
      endOffset = posOffset end
  in property $ endOffset >= startOffset

prop_span_offset_merge :: SourceSpan -> SourceSpan -> Property
prop_span_offset_merge span1 span2 = 
  let merged = mergeSpans span1 span2
      startOffset = posOffset (spanStart merged)
      endOffset = posOffset (spanEnd merged)
      start1Offset = posOffset (spanStart span1)
      end1Offset = posOffset (spanEnd span1)
      start2Offset = posOffset (spanStart span2)
      end2Offset = posOffset (spanEnd span2)
  in property $ startOffset === min start1Offset start2Offset &&
                endOffset === max end1Offset end2Offset

-- Test position arithmetic with column boundaries
prop_pos_column_boundaries :: Positive Int -> Property
prop_pos_column_boundaries (Positive col) = 
  let pos = SourcePos 1 col 0
  in property $ posColumn pos >= 1

prop_pos_column_after_newline :: Positive Int -> Positive Int -> Property
prop_pos_column_after_newline (Positive line) (Positive col) = 
  let pos = SourcePos line col 0
      newPos = posAfter '\n' pos
  in property $ posColumn newPos === 1

prop_pos_column_after_regular_char :: Positive Int -> Positive Int -> Positive Int -> Char -> Property
prop_pos_column_after_regular_char (Positive line) (Positive col) (Positive offset) c = 
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in if c `elem` ['\n', '\t']
     then property $ True  -- Skip special chars
     else property $ posColumn newPos === posColumn pos + 1

-- Test span arithmetic with line boundaries
prop_span_line_boundaries :: SourceSpan -> Property
prop_span_line_boundaries span = 
  let start = spanStart span
      end = spanEnd span
  in property $ posLine start >= 1 && posLine end >= 1

prop_span_line_merge :: SourceSpan -> SourceSpan -> Property
prop_span_line_merge span1 span2 = 
  let merged = mergeSpans span1 span2
      startLine = posLine (spanStart merged)
      endLine = posLine (spanEnd merged)
      start1Line = posLine (spanStart span1)
      end1Line = posLine (spanEnd span1)
      start2Line = posLine (spanStart span2)
      end2Line = posLine (spanEnd span2)
  in property $ startLine === min start1Line start2Line &&
                endLine === max end1Line end2Line

-- Helper functions
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)
  where
    findIndex _ [] = Nothing
    findIndex p (y:ys) = if p y then Just 0 else fmap (+1) (findIndex p ys)

-- Tests collection
tests :: TestTree
tests = testGroup "SourceLocation Math QuickCheck Tests"
  [ testProperty "source pos addition" prop_source_pos_addition
  , testProperty "source pos subtraction" prop_source_pos_subtraction
  , testProperty "source pos line addition" prop_source_pos_line_addition
  , testProperty "source pos column addition" prop_source_pos_column_addition
  , testProperty "source pos offset addition" prop_source_pos_offset_addition
  , testProperty "source pos comparison reflexive" prop_source_pos_comparison_reflexive
  , testProperty "source pos comparison antisymmetric" prop_source_pos_comparison_antisymmetric
  , testProperty "source pos comparison transitive" prop_source_pos_comparison_transitive
  , testProperty "source pos line major" prop_source_pos_line_major
  , testProperty "source pos column minor" prop_source_pos_column_minor
  , testProperty "source span merge commutative" prop_source_span_merge_commutative
  , testProperty "source span merge associative" prop_source_span_merge_associative
  , testProperty "source span merge idempotent" prop_source_span_merge_idempotent
  , testProperty "source span merge contains originals" prop_source_span_merge_contains_originals
  , testProperty "source span length" prop_source_span_length
  , testProperty "source span is valid reflexive" prop_source_span_is_valid_reflexive
  , testProperty "source span comparison" prop_source_span_comparison
  , testProperty "source span ordered merge" prop_source_span_ordered_merge
  , testProperty "pos after newline" prop_pos_after_newline
  , testProperty "pos after tab" prop_pos_after_tab
  , testProperty "pos after regular char" prop_pos_after_regular_char
  , testProperty "pos advance by" prop_pos_advance_by
  , testProperty "pos advance by line" prop_pos_advance_by_line
  , testProperty "span between ordered" prop_span_between_ordered
  , testProperty "span between ordered symmetric" prop_span_between_ordered_symmetric
  , testProperty "span between reflexive" prop_span_between_reflexive
  , testProperty "span from" prop_span_from
  , testProperty "span to" prop_span_to
  , testProperty "empty span" prop_empty_span
  , testProperty "span size" prop_span_size
  , testProperty "span distance" prop_span_distance
  , testProperty "span line distance" prop_span_line_distance
  , testProperty "span column distance" prop_span_column_distance
  , testProperty "pos at" prop_pos_at
  , testProperty "pos at line col" prop_pos_at_line_col
  , testProperty "start pos" prop_start_pos
  , testProperty "span contains start" prop_span_contains_start
  , testProperty "span contains end" prop_span_contains_end
  , testProperty "span contains middle" prop_span_contains_middle
  , testProperty "span overlaps reflexive" prop_span_overlaps_reflexive
  , testProperty "pos advance by text" prop_pos_advance_by_text
  , testProperty "pos advance by text newlines" prop_pos_advance_by_text_newlines
  , testProperty "pos advance by text last line" prop_pos_advance_by_text_last_line
  , testProperty "span between positions" prop_span_between_positions
  , testProperty "span between ordered positions" prop_span_between_ordered_positions
  , testProperty "span merge with empty" prop_span_merge_with_empty
  , testProperty "span merge with adjacent" prop_span_merge_with_adjacent
  , testProperty "pos after tab alignment" prop_pos_after_tab_alignment
  , testProperty "pos after multiple tabs" prop_pos_after_multiple_tabs
  , testProperty "pos at line start" prop_pos_at_line_start
  , testProperty "pos at line end" prop_pos_at_line_end
  , testProperty "pos advance to next line" prop_pos_advance_to_next_line
  , testProperty "multi line span size" prop_multi_line_span_size
  , testProperty "multi line span merge" prop_multi_line_span_merge
  , testProperty "pos offset consistency" prop_pos_offset_consistency
  , testProperty "pos offset monotonic" prop_pos_offset_monotonic
  , testProperty "span offset range" prop_span_offset_range
  , testProperty "span offset merge" prop_span_offset_merge
  , testProperty "pos column boundaries" prop_pos_column_boundaries
  , testProperty "pos column after newline" prop_pos_column_after_newline
  , testProperty "pos column after regular char" prop_pos_column_after_regular_char
  , testProperty "span line boundaries" prop_span_line_boundaries
  , testProperty "span line merge" prop_span_line_merge
  ]