module Test.Unit.SourceLocationSpanQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
  ( SourcePos(..), SourceSpan(..), startPos
  , emptySpan, spanFrom, spanTo, spanBetween, spanBetweenOrdered
  , mergeSpans, isValidSpan, isValidBlockSpan
  )

-- | 生成有效的SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

-- | 生成有效的SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- | 测试emptySpan的属性
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let span = emptySpan
  in isValidSpan span === False .&&.
     isValidBlockSpan span === False

-- | 测试spanFrom的属性
prop_span_from_valid :: SourcePos -> Property
prop_span_from_valid pos =
  let span = spanFrom pos
  in spanStart span === pos .&&.
     spanEnd span === pos .&&.
     isValidSpan span === True

-- | 测试spanTo的属性
prop_span_to_valid :: SourcePos -> Property
prop_span_to_valid pos =
  let span = spanTo pos
  in spanStart span === pos .&&.
     spanEnd span === pos .&&.
     isValidSpan span === True

-- | 测试spanBetween的属性
prop_span_between_properties :: SourcePos -> SourcePos -> Property
prop_span_between_properties pos1 pos2 =
  let span = spanBetween pos1 pos2
  in spanStart span === pos1 .&&.
     spanEnd span === pos2

-- | 测试spanBetweenOrdered的属性
prop_span_between_ordered_properties :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_properties pos1 pos2 =
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (start <= end) === True .&&.
     (start === pos1 || start === pos2) === True .&&.
     (end === pos1 || end === pos2) === True

-- | 测试mergeSpans的属性
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === 
  mergeSpans (mergeSpans span1 span2) span3

prop_merge_spans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in (mergedStart <= start1 && mergedEnd >= end1) .&&.
     (mergedStart <= start2 && mergedEnd >= end2)

-- | 测试isValidSpan的属性
prop_valid_span_same_position :: SourcePos -> Property
prop_valid_span_same_position pos =
  let span = SourceSpan pos pos
  in isValidSpan span === True

prop_invalid_span_empty :: Property
prop_invalid_span_empty =
  let span = SourceSpan startPos startPos
  in isValidSpan span === True  -- startPos is a valid position

-- | 测试isValidBlockSpan的属性
prop_valid_block_span_different_positions :: SourcePos -> SourcePos -> Property
prop_valid_block_span_different_positions pos1 pos2 =
  pos1 /= pos2 ==> 
  let span = SourceSpan pos1 pos2
  in isValidBlockSpan span === True

prop_invalid_block_span_same_position :: SourcePos -> Property
prop_invalid_block_span_same_position pos =
  let span = SourceSpan pos pos
  in isValidBlockSpan span === False

tests :: TestTree
tests = testGroup "SourceLocation Span QuickCheck Tests"
  [ testProperty "empty span properties" prop_empty_span_properties
  , testProperty "spanFrom valid" prop_span_from_valid
  , testProperty "spanTo valid" prop_span_to_valid
  , testProperty "spanBetween properties" prop_span_between_properties
  , testProperty "spanBetweenOrdered properties" prop_span_between_ordered_properties
  , testProperty "mergeSpans commutative" prop_merge_spans_commutative
  , testProperty "mergeSpans associative" prop_merge_spans_associative
  , testProperty "mergeSpans contains originals" prop_merge_spans_contains_originals
  , testProperty "valid span same position" prop_valid_span_same_position
  , testProperty "invalid span empty" prop_invalid_span_empty
  , testProperty "valid block span different positions" prop_valid_block_span_different_positions
  , testProperty "invalid block span same position" prop_invalid_block_span_same_position
  ]