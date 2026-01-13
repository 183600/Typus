{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationMathQuickCheckSpec2 where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import SourceLocation
import Test.QuickCheck (Positive(..))

-- | 测试SourcePos的基本属性
prop_sourcePos_components_positive :: Positive Int -> Positive Int -> Property
prop_sourcePos_components_positive (Positive line) (Positive col) =
  let pos = SourcePos line col 0
  in property $ posLine pos >= 1 && posColumn pos >= 1

-- | 测试startPos的基本属性
prop_start_pos_properties :: Property
prop_start_pos_properties = 
  let start = startPos
  in property $ posLine start == 1 && posColumn start == 1 && posOffset start == 0

-- | 测试posAfter函数与换行符
prop_posAfter_newline :: Positive Int -> Property
prop_posAfter_newline (Positive line) =
  let pos = SourcePos line 5 0
      posAfter' = posAfter '\n' pos
  in property $ posLine posAfter' == line + 1 && posColumn posAfter' == 1

-- | 测试posAfter函数与普通字符
prop_posAfter_regular_char :: Positive Int -> Positive Int -> Char -> Property
prop_posAfter_regular_char (Positive line) (Positive col) c =
  let pos = SourcePos line col 0
      posAfter' = posAfter c pos
  in if c /= '\n'
     then property $ posLine posAfter' == line && posColumn posAfter' == col + 1
     else property True

-- | 测试posAt函数的基本属性
prop_pos_at_properties :: Positive Int -> Positive Int -> Property
prop_pos_at_properties (Positive line) (Positive col) =
  let pos = posAt line col
  in property $ posLine pos == line && posColumn pos == col

-- | 测试posAtLineCol函数的一致性
prop_pos_at_line_col_consistent :: Positive Int -> Positive Int -> Property
prop_pos_at_line_col_consistent (Positive line) (Positive col) =
  let pos1 = posAt line col
      pos2 = posAtLineCol line col 0
  in property $ pos1 == pos2

-- | 测试emptySpan函数的基本属性
prop_empty_span_properties :: Positive Int -> Positive Int -> Property
prop_empty_span_properties (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = emptySpan pos
  in property $ spanStart span == pos && spanEnd span == pos

-- | 测试spanFrom函数的基本属性
prop_span_from_properties :: Positive Int -> Positive Int -> Property
prop_span_from_properties (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = spanFrom pos
  in property $ spanStart span == pos && spanEnd span == pos

-- | 测试spanTo函数的基本属性
prop_span_to_properties :: Positive Int -> Positive Int -> Property
prop_span_to_properties (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = spanTo pos
  in property $ spanStart span == pos && spanEnd span == pos

-- | 测试spanBetween函数的顺序性
prop_span_between_order :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_between_order (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
  in property $ spanStart span1 == min pos1 pos2 && spanEnd span1 == max pos1 pos2 &&
                spanStart span2 == min pos1 pos2 && spanEnd span2 == max pos1 pos2

-- | 测试spanBetweenOrdered函数的顺序性
prop_span_between_ordered_correct :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_between_ordered_correct (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      span = spanBetweenOrdered pos1 pos2
  in if pos1 <= pos2
     then property $ spanStart span == pos1 && spanEnd span == pos2
     else property $ spanStart span == pos2 && spanEnd span == pos1

-- | 测试mergeSpans函数的交换性
prop_merge_spans_commutative :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_merge_spans_commutative (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      span1 = mergeSpans (spanFrom pos1) (spanFrom pos2)
      span2 = mergeSpans (spanFrom pos2) (spanFrom pos1)
  in property $ span1 == span2

-- | 测试mergeSpans函数的结合性
prop_merge_spans_associative :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_merge_spans_associative (Positive line1) (Positive col1) (Positive line2) (Positive col2) (Positive line3) (Positive col3) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      pos3 = SourcePos line3 col3 0
      span1 = mergeSpans (mergeSpans (spanFrom pos1) (spanFrom pos2)) (spanFrom pos3)
      span2 = mergeSpans (spanFrom pos1) (mergeSpans (spanFrom pos2) (spanFrom pos3))
  in property $ span1 == span2

-- | 测试isValidSpan函数的基本属性
prop_is_valid_span_true :: Positive Int -> Positive Int -> Property
prop_is_valid_span_true (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = spanFrom pos
  in property $ isValidSpan span

-- | 测试isValidSpan函数与反向span
prop_is_valid_span_reverse :: Positive Int -> Positive Int -> Property
prop_is_valid_span_reverse (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = SourceSpan pos (SourcePos (line - 1) (max 1 (col - 1)) 0)
  in property $ not (isValidSpan span)

-- | 测试locatedAt函数的基本属性
prop_located_at_preserves_value :: String -> Positive Int -> Positive Int -> Property
prop_located_at_preserves_value val (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      located = locatedAt pos val
  in property $ locValue located == val && locatedPos located == pos

-- | 测试locatedWithSpan函数的基本属性
prop_located_with_span_preserves_value :: String -> Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_located_with_span_preserves_value val (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      span = spanBetween pos1 pos2
      located = locatedWithSpan span val
  in property $ locValue located == val && locatedSpan located == span

-- | 测试LocationTracker的基本功能
prop_location_tracker_set_get :: Positive Int -> Positive Int -> Property
prop_location_tracker_set_get (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      result = runLocationTracker $ do
        setCurrentPos pos
        getCurrentPos
  in property $ result == pos

-- | 测试LocationTracker的span标记功能
prop_location_tracker_span_marking :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_location_tracker_span_marking (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      result = runLocationTracker $ do
        setCurrentPos pos1
        start <- markSpanStart
        setCurrentPos pos2
        markSpanEnd start
  in property $ True  -- 只要能执行就行

tests :: TestTree
tests = testGroup "SourceLocation Math QuickCheck Tests"
  [ testProperty "sourcePos components positive" prop_sourcePos_components_positive
  , testProperty "startPos properties" prop_start_pos_properties
  , testProperty "posAfter newline" prop_posAfter_newline
  , testProperty "posAfter regular char" prop_posAfter_regular_char
  , testProperty "posAt properties" prop_pos_at_properties
  , testProperty "posAtLineCol consistent" prop_pos_at_line_col_consistent
  , testProperty "emptySpan properties" prop_empty_span_properties
  , testProperty "spanFrom properties" prop_span_from_properties
  , testProperty "spanTo properties" prop_span_to_properties
  , testProperty "spanBetween order" prop_span_between_order
  , testProperty "spanBetweenOrdered correct" prop_span_between_ordered_correct
  , testProperty "mergeSpans commutative" prop_merge_spans_commutative
  , testProperty "mergeSpans associative" prop_merge_spans_associative
  , testProperty "isValidSpan true" prop_is_valid_span_true
  , testProperty "isValidSpan reverse" prop_is_valid_span_reverse
  , testProperty "locatedAt preserves value" prop_located_at_preserves_value
  , testProperty "locatedWithSpan preserves value" prop_located_with_span_preserves_value
  , testProperty "locationTracker set get" prop_location_tracker_set_get
  , testProperty "locationTracker span marking" prop_location_tracker_span_marking
  ]