module Test.Unit.SourceLocationAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, spanBetweenOrdered, mergeSpans
  , isValidSpan, isValidBlockSpan, locatedAt, locatedWithSpan, locatedValue
  , locatedSpan, locatedPos, mapLocated, advancePos, advancePosBy, advancePosByText
  , advancePosByLine, toErrorLocation, toErrorLocationWithSpan
  )
import qualified Data.Text as T (pack)
import Compiler.Errors.Core (ErrorLocation(..))
import Control.DeepSeq (NFData, rnf)

-- | SourcePos 的 Arbitrary 实例
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

-- | SourceSpan 的 Arbitrary 实例
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- | Located a 的 Arbitrary 实例
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span' <- arbitrary
    return $ Located value pos span'

-- | 测试SourcePos的比较属性
prop_source_pos_comparison_reflexive :: SourcePos -> Property
prop_source_pos_comparison_reflexive pos =
  pos == pos .&&. pos <= pos .&&. pos >= pos

prop_source_pos_comparison_antisymmetric :: SourcePos -> SourcePos -> Property
prop_source_pos_comparison_antisymmetric pos1 pos2 =
  (pos1 <= pos2 && pos2 <= pos1) ==> pos1 == pos2

prop_source_pos_comparison_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_source_pos_comparison_transitive pos1 pos2 pos3 =
  (pos1 <= pos2 && pos2 <= pos3) ==> pos1 <= pos3

-- | 测试SourcePos的序关系
prop_source_pos_ordering_line_major :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering_line_major pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
  in if line1 < line2
     then pos1 < pos2
     else if line1 > line2
          then pos1 > pos2
          else property True  -- 同一行，需要比较列

prop_source_pos_ordering_column_minor :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering_column_minor pos1 pos2 =
  posLine pos1 == posLine pos2 ==> 
  let col1 = posColumn pos1
      col2 = posColumn pos2
  in if col1 < col2
     then pos1 < pos2
     else if col1 > col2
          then pos1 > pos2
          else property True  -- 同一列，需要比较偏移量

-- | 测试posAfter函数的属性
prop_pos_after_newline_resets_column :: Int -> Property
prop_pos_after_newline_resets_column line =
  line >= 1 ==> 
  let pos = SourcePos line 10 100
      newPos = posAfter '\n' pos
  in posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === 101

prop_pos_after_tab_alignment :: Int -> Int -> Property
prop_pos_after_tab_alignment column offset =
  column >= 1 && column <= 20 && offset >= 0 ==> 
  let pos = SourcePos 1 column offset
      expectedColumn = ((column - 1) `div` 8 + 1) * 8 + 1
      newPos = posAfter '\t' pos
  in posColumn newPos === expectedColumn .&&.
     posOffset newPos === offset + 1

prop_pos_after_regular_char :: Char -> SourcePos -> Property
prop_pos_after_regular_char c pos =
  c /= '\n' && c /= '\t' ==> 
  let newPos = posAfter c pos
  in posLine newPos === posLine pos .&&.
     posColumn newPos === posColumn pos + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- | 测试SourceSpan的属性
prop_span_between_ordered_preserves_order :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_preserves_order pos1 pos2 =
  let span' = spanBetweenOrdered pos1 pos2
      start = spanStart span'
      end = spanEnd span'
  in start <= end

prop_span_between_ordered_min_max :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_min_max pos1 pos2 =
  let span' = spanBetweenOrdered pos1 pos2
      start = spanStart span'
      end = spanEnd span'
  in (start == min pos1 pos2) .&&. (end == max pos1 pos2)

prop_merge_spans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in mergedStart <= start1 .&&. mergedEnd >= end1 .&&.
     mergedStart <= start2 .&&. mergedEnd >= end2

prop_merge_spans_idempotent :: SourceSpan -> Property
prop_merge_spans_idempotent span =
  let merged = mergeSpans span span
  in merged === span

prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- | 测试isValidSpan的属性
prop_is_valid_span_true_for_ordered :: SourcePos -> SourcePos -> Property
prop_is_valid_span_true_for_ordered pos1 pos2 =
  pos1 <= pos2 ==> isValidSpan (SourceSpan pos1 pos2)

prop_is_valid_span_false_for_unordered :: SourcePos -> SourcePos -> Property
prop_is_valid_span_false_for_unordered pos1 pos2 =
  pos1 > pos2 ==> not (isValidSpan (SourceSpan pos1 pos2))

prop_is_valid_block_span_equals_is_valid_span :: SourceSpan -> Property
prop_is_valid_block_span_equals_is_valid_span span =
  isValidBlockSpan span === isValidSpan span

-- | 测试Located值的属性
prop_located_at_creates_valid_location :: SourcePos -> String -> Property
prop_located_at_creates_valid_location pos value =
  let located = locatedAt pos value
      expectedSpan = emptySpan pos
  in locatedValue located === value .&&.
     locatedPos located === pos .&&.
     locatedSpan located === expectedSpan

prop_located_with_span_preserves_values :: SourceSpan -> String -> Property
prop_located_with_span_preserves_values span value =
  let located = locatedWithSpan span value
  in locatedValue located === value .&&.
     locatedSpan located === span .&&.
     locatedPos located === spanStart span

prop_map_located_preserves_location :: SourceSpan -> String -> Property
prop_map_located_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (++ " mapped") located
  in locatedValue mapped === value ++ " mapped" .&&.
     locatedPos mapped === locatedPos located .&&.
     locatedSpan mapped === locatedSpan located

-- | 测试位置前进函数的属性
prop_advance_pos_by_empty_string :: SourcePos -> Property
prop_advance_pos_by_empty_string pos =
  advancePosBy "" pos === pos

prop_advance_pos_by_consistent_with_single :: String -> SourcePos -> Property
prop_advance_pos_by_consistent_with_single s pos =
  let byString = advancePosBy s pos
      byChars = foldl (flip advancePos) pos s
  in byString === byChars

prop_advance_pos_by_text_consistency :: String -> SourcePos -> Property
prop_advance_pos_by_text_consistency s pos =
  let byString = advancePosBy s pos
      byText = advancePosByText (T.pack s) pos
  in byString === byText

prop_advance_pos_by_line_increments_line :: Int -> Int -> Property
prop_advance_pos_by_line_increments_line line numLines =
  line >= 1 && numLines >= 0 ==> 
  let pos = SourcePos line 5 100
      newPos = advancePosByLine numLines pos
  in posLine newPos === line + numLines .&&.
     posColumn newPos === 1

-- | 测试错误位置转换的属性
prop_to_error_location_preserves_position :: SourcePos -> Property
prop_to_error_location_preserves_position pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos .&&.
     column errLoc === posColumn pos .&&.
     filePath errLoc === Nothing .&&.
     endLine errLoc === Nothing .&&.
     endColumn errLoc === Nothing

prop_to_error_location_with_span_preserves_range :: SourceSpan -> Property
prop_to_error_location_with_span_preserves_range span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errLoc === posLine start .&&.
     column errLoc === posColumn start .&&.
     endLine errLoc === Just (posLine end) .&&.
     endColumn errLoc === Just (posColumn end) .&&.
     filePath errLoc === Nothing

-- | 测试SourcePos的NFData实例
prop_source_pos_deepseq :: SourcePos -> Property
prop_source_pos_deepseq pos =
  let result = rnf pos
  in result === ()

-- | 测试SourceSpan的NFData实例
prop_source_span_deepseq :: SourceSpan -> Property
prop_source_span_deepseq span =
  let result = rnf span
  in result === ()

-- | 测试Located的NFData实例
prop_located_deepseq :: Located String -> Property
prop_located_deepseq located =
  let result = rnf located
  in result === ()

-- | 测试spanBetween与spanBetweenOrdered的一致性
prop_span_between_vs_ordered_consistency :: SourcePos -> SourcePos -> Property
prop_span_between_vs_ordered_consistency pos1 pos2 =
  let regularSpan = spanBetween pos1 pos2
      orderedSpan = spanBetweenOrdered pos1 pos2
      startRegular = spanStart regularSpan
      endRegular = spanEnd regularSpan
      startOrdered = spanStart orderedSpan
      endOrdered = spanEnd orderedSpan
  in if pos1 <= pos2
     then regularSpan === orderedSpan
     else (startRegular === pos1 && endRegular === pos2) .&&.
          (startOrdered === pos2 && endOrdered === pos1)

-- | 测试位置比较的传递性
prop_position_ordering_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_position_ordering_transitive pos1 pos2 pos3 =
  (pos1 <= pos2 && pos2 <= pos3) ==> pos1 <= pos3

-- | 测试span合并的幂等性
prop_merge_spans_idempotent_with_same_span :: SourceSpan -> Property
prop_merge_spans_idempotent_with_same_span span =
  mergeSpans span span === span

-- | 测试span合并的交换律
prop_merge_spans_commutative_property :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative_property span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- | 测试span合并的结合律
prop_merge_spans_associative_property :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative_property span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

tests :: TestTree
tests = testGroup "SourceLocation Advanced QuickCheck Tests"
  [ testProperty "SourcePos comparison reflexive" prop_source_pos_comparison_reflexive
  , testProperty "SourcePos comparison antisymmetric" prop_source_pos_comparison_antisymmetric
  , testProperty "SourcePos comparison transitive" prop_source_pos_comparison_transitive
  , testProperty "SourcePos ordering line major" prop_source_pos_ordering_line_major
  , testProperty "SourcePos ordering column minor" prop_source_pos_ordering_column_minor
  , testProperty "posAfter newline resets column" prop_pos_after_newline_resets_column
  , testProperty "posAfter tab alignment" prop_pos_after_tab_alignment
  , testProperty "posAfter regular char" prop_pos_after_regular_char
  , testProperty "spanBetweenOrdered preserves order" prop_span_between_ordered_preserves_order
  , testProperty "spanBetweenOrdered min max" prop_span_between_ordered_min_max
  , testProperty "mergeSpans contains both" prop_merge_spans_contains_both
  , testProperty "mergeSpans idempotent" prop_merge_spans_idempotent
  , testProperty "mergeSpans commutative" prop_merge_spans_commutative
  , testProperty "isValidSpan true for ordered" prop_is_valid_span_true_for_ordered
  , testProperty "isValidSpan false for unordered" prop_is_valid_span_false_for_unordered
  , testProperty "isValidBlockSpan equals isValidSpan" prop_is_valid_block_span_equals_is_valid_span
  , testProperty "locatedAt creates valid location" prop_located_at_creates_valid_location
  , testProperty "locatedWithSpan preserves values" prop_located_with_span_preserves_values
  , testProperty "mapLocated preserves location" prop_map_located_preserves_location
  , testProperty "advancePosBy empty string" prop_advance_pos_by_empty_string
  , testProperty "advancePosBy consistent with single" prop_advance_pos_by_consistent_with_single
  , testProperty "advancePosByText consistency" prop_advance_pos_by_text_consistency
  , testProperty "advancePosByLine increments line" prop_advance_pos_by_line_increments_line
  , testProperty "toErrorLocation preserves position" prop_to_error_location_preserves_position
  , testProperty "toErrorLocationWithSpan preserves range" prop_to_error_location_with_span_preserves_range
  , testProperty "SourcePos deepseq" prop_source_pos_deepseq
  , testProperty "SourceSpan deepseq" prop_source_span_deepseq
  , testProperty "Located deepseq" prop_located_deepseq
  , testProperty "spanBetween vs ordered consistency" prop_span_between_vs_ordered_consistency
  , testProperty "position ordering transitive" prop_position_ordering_transitive
  , testProperty "mergeSpans idempotent with same span" prop_merge_spans_idempotent_with_same_span
  , testProperty "mergeSpans commutative property" prop_merge_spans_commutative_property
  , testProperty "mergeSpans associative property" prop_merge_spans_associative_property
  ]