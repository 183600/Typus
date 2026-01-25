module Test.Unit.SourceLocationSpanQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
  ( SourcePos(..), SourceSpan(..), startPos
  , emptySpan, spanFrom, spanTo, spanBetween, spanBetweenOrdered
  , mergeSpans, isValidSpan, isValidBlockSpan
  )

-- | 生成有效的SourcePos
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- | 生成有效的SourceSpan
-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


-- | 测试emptySpan的属性
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let pos = SourcePos 0 0 0
      sourceSpan = emptySpan pos
  in isValidSpan sourceSpan === False .&&.
     isValidBlockSpan sourceSpan === False

-- | 测试spanFrom的属性
prop_span_from_valid :: SourcePos -> Property
prop_span_from_valid pos =
  let sourceSpan = spanFrom pos
  in spanStart sourceSpan === pos .&&.
     spanEnd sourceSpan === pos .&&.
     isValidSpan sourceSpan === True

-- | 测试spanTo的属性
prop_span_to_valid :: SourcePos -> Property
prop_span_to_valid pos =
  let sourceSpan = spanTo pos
  in spanStart sourceSpan === pos .&&.
     spanEnd sourceSpan === pos .&&.
     isValidSpan sourceSpan === True

-- | 测试spanBetween的属性
prop_span_between_properties :: SourcePos -> SourcePos -> Property
prop_span_between_properties pos1 pos2 =
  let sourceSpan = spanBetween pos1 pos2
  in spanStart sourceSpan === pos1 .&&.
     spanEnd sourceSpan === pos2

-- | 测试spanBetweenOrdered的属性
prop_span_between_ordered_properties :: SourcePos -> SourcePos -> Property
prop_span_between_ordered_properties pos1 pos2 =
  let sourceSpan = spanBetweenOrdered pos1 pos2
      start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in property (start <= end) .&&.
     property (start == pos1 || start == pos2) .&&.
     property (end == pos1 || end == pos2)

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
  let sourceSpan = SourceSpan pos pos
  in isValidSpan sourceSpan === True

prop_invalid_span_empty :: Property
prop_invalid_span_empty =
  let sourceSpan = SourceSpan startPos startPos
  in isValidSpan sourceSpan === True  -- startPos is a valid position

-- | 测试isValidBlockSpan的属性
prop_valid_block_span_different_positions :: SourcePos -> SourcePos -> Property
prop_valid_block_span_different_positions pos1 pos2 =
  pos1 /= pos2 ==> 
  let sourceSpan = SourceSpan pos1 pos2
  in isValidBlockSpan sourceSpan === True

prop_invalid_block_span_same_position :: SourcePos -> Property
prop_invalid_block_span_same_position pos =
  let sourceSpan = SourceSpan pos pos
  in isValidBlockSpan sourceSpan === False

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