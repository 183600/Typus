module Test.Unit.SourceLocationArithmeticQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.QuickCheck (fastProperty)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Arbitrary instances are now defined in SourceLocation module

-- Properties for SourcePos arithmetic
prop_pos_line_non_negative :: SourcePos -> Bool
prop_pos_line_non_negative (SourcePos line _ _) = line >= 1

prop_pos_column_non_negative :: SourcePos -> Bool
prop_pos_column_non_negative (SourcePos _ column _) = column >= 1

prop_pos_offset_non_negative :: SourcePos -> Bool
prop_pos_offset_non_negative (SourcePos _ _ offset) = offset >= 0

prop_pos_comparison :: SourcePos -> SourcePos -> Property
prop_pos_comparison pos1 pos2 = 
  property (pos1 == pos2 || pos1 < pos2 || pos1 > pos2)

-- Properties for SourceSpan arithmetic
prop_span_start_before_end :: SourceSpan -> Bool
prop_span_start_before_end (SourceSpan start end) = start <= end

prop_span_length :: SourceSpan -> Bool
prop_span_length (SourceSpan start end) = 
  let lineDiff = sourceLine end - sourceLine start
      colDiff = sourceColumn end - sourceColumn start
  in lineDiff >= 0 && (lineDiff > 0 || colDiff >= 0)

-- Properties for position arithmetic
prop_add_lines :: SourcePos -> Int -> Property
prop_add_lines (SourcePos line col offset) n = 
  n >= 0 ==> 
  let newLine = line + n
      newPos = SourcePos newLine col offset
  in sourceLine newPos >= line

prop_add_columns :: SourcePos -> Int -> Property
prop_add_columns (SourcePos line col offset) n = 
  n >= 0 ==> 
  let newCol = col + n
      newPos = SourcePos line newCol offset
  in sourceColumn newPos >= col

prop_add_offsets :: SourcePos -> Int -> Property
prop_add_offsets (SourcePos line col offset) n = 
  n >= 0 ==> 
  let newOffset = offset + n
      newPos = SourcePos line col newOffset
  in sourceOffset newPos >= offset

-- Properties for span arithmetic
prop_span_width :: SourceSpan -> Property
prop_span_width (SourceSpan start end) = 
  sourceLine start == sourceLine end ==> 
  sourceColumn end - sourceColumn start >= 0

prop_span_height :: SourceSpan -> Bool
prop_span_height (SourceSpan start end) = 
  let height = sourceLine end - sourceLine start + 1
  in height >= 1

prop_span_contains_start :: SourceSpan -> Bool
prop_span_contains_start span@(SourceSpan start _) = 
  span `contains` start
  where
    (SourceSpan s e) `contains` pos = s <= pos && pos <= e

prop_span_contains_end :: SourceSpan -> Bool
prop_span_contains_end span@(SourceSpan _ end) = 
  span `contains` end
  where
    (SourceSpan s e) `contains` pos = s <= pos && pos <= e

-- Properties for position ordering
prop_pos_reflexive :: SourcePos -> Bool
prop_pos_reflexive pos = pos <= pos

prop_pos_antisymmetric :: SourcePos -> SourcePos -> Property
prop_pos_antisymmetric pos1 pos2 = 
  (pos1 <= pos2 && pos2 <= pos1) ==> pos1 == pos2

prop_pos_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_pos_transitive pos1 pos2 pos3 = 
  (pos1 <= pos2 && pos2 <= pos3) ==> pos1 <= pos3

-- Properties for span construction
prop_span_single_line :: SourcePos -> Int -> Property
prop_span_single_line (SourcePos line col offset) n = 
  n >= 0 ==> 
  let endCol = col + n
      endPos = SourcePos line endCol (offset + n)
      span = SourceSpan (SourcePos line col offset) endPos
  in sourceLine (getSpanStart span) == sourceLine (getSpanEnd span)

prop_span_multi_line :: SourcePos -> Int -> Property
prop_span_multi_line (SourcePos line col offset) n = 
  n > 0 ==> 
  let endLine = line + n
      endPos = SourcePos endLine col offset
      span = SourceSpan (SourcePos line col offset) endPos
  in sourceLine (getSpanEnd span) > sourceLine (getSpanStart span)

-- Properties for span merging
prop_span_merge_adjacent :: SourcePos -> Int -> Property
prop_span_merge_adjacent (SourcePos line col offset) n = 
  n >= 0 ==> 
  let midPos = SourcePos line (col + n) (offset + n)
      endPos = SourcePos line (col + 2*n) (offset + 2*n)
      span1 = SourceSpan (SourcePos line col offset) midPos
      span2 = SourceSpan midPos endPos
      merged = mergeSpans span1 span2
  in getSpanStart merged == getSpanStart span1 && getSpanEnd merged == getSpanEnd span2
  where
    mergeSpans (SourceSpan s1 _) (SourceSpan _ e2) = SourceSpan s1 e2

-- Properties for span intersection
prop_span_intersect_overlap :: SourcePos -> Int -> Property
prop_span_intersect_overlap (SourcePos line col offset) n = 
  n > 0 ==> 
  let midPos = SourcePos line (col + n) (offset + n)
      endPos = SourcePos line (col + 2*n) (offset + 2*n)
      span1 = SourceSpan (SourcePos line col offset) midPos
      span2 = SourceSpan midPos endPos
  in spansOverlap span1 span2
  where
    spansOverlap (SourceSpan s1 e1) (SourceSpan s2 e2) = 
      not (e1 < s2 || e2 < s1)

-- Helper functions
getSpanStart :: SourceSpan -> SourcePos
getSpanStart (SourceSpan start _) = start

getSpanEnd :: SourceSpan -> SourcePos
getSpanEnd (SourceSpan _ end) = end

-- Accessor functions for SourcePos
sourceLine :: SourcePos -> Int
sourceLine = posLine

sourceColumn :: SourcePos -> Int
sourceColumn = posColumn

sourceOffset :: SourcePos -> Int
sourceOffset = posOffset

tests :: TestTree
tests = testGroup "Test.Unit.SourceLocationArithmeticQuickCheckSpec Tests"
  [ fastProperty "pos line non negative" prop_pos_line_non_negative
  , fastProperty "pos column non negative" prop_pos_column_non_negative
  , fastProperty "pos offset non negative" prop_pos_offset_non_negative
  , fastProperty "pos comparison" prop_pos_comparison
  , fastProperty "span start before end" prop_span_start_before_end
  , fastProperty "span length" prop_span_length
  , fastProperty "add lines" prop_add_lines
  , fastProperty "add columns" prop_add_columns
  , fastProperty "add offsets" prop_add_offsets
  , fastProperty "span width" prop_span_width
  , fastProperty "span height" prop_span_height
  , fastProperty "span contains start" prop_span_contains_start
  , fastProperty "span contains end" prop_span_contains_end
  , fastProperty "pos reflexive" prop_pos_reflexive
  , fastProperty "pos antisymmetric" prop_pos_antisymmetric
  , fastProperty "pos transitive" prop_pos_transitive
  , fastProperty "span single line" prop_span_single_line
  , fastProperty "span multi line" prop_span_multi_line
  , fastProperty "span merge adjacent" prop_span_merge_adjacent
  , fastProperty "span intersect overlap" prop_span_intersect_overlap
  ]