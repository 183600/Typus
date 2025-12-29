module Test.Unit.SourceLocationMathPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, advancePos, advancePosBy)

-- ============================================================================
-- Source Location Mathematical Properties QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Mathematical Properties QuickCheck Tests"
  [ testProperty "position advancement is monotonic" prop_position_advancement_monotonic
  , testProperty "span merging is commutative" prop_span_merging_commutative
  , testProperty "span merging is associative" prop_span_merging_associative
  , testProperty "span between positions is valid" prop_span_between_valid
  , testProperty "merged span contains original spans" prop_merged_span_contains_originals
  , testProperty "position advancement by string is consistent" prop_pos_advancement_consistent
  , testProperty "span validity is preserved under merging" prop_span_validity_preserved
  , testProperty "start position is always <= end position in valid spans" prop_span_start_le_end
  ]

-- | Advancing position should never go backwards in offset
prop_position_advancement_monotonic :: SourcePos -> String -> Property
prop_position_advancement_monotonic pos chars = 
  let advanced = advancePosBy chars pos
  in posOffset advanced >= posOffset pos

-- | Merging two spans should be commutative (result should be same regardless of order)
prop_span_merging_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merging_commutative span1 span2 = 
  mergeSpans span1 span2 === mergeSpans span2 span1

-- | Merging spans should be associative
prop_span_merging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merging_associative span1 span2 span3 = 
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- | Span created between two positions should always be valid if positions are ordered
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 = 
  let span = if pos1 <= pos2 then spanBetween pos1 pos2 else spanBetween pos2 pos1
  in isValidSpan span

-- | Merged span should contain both original spans
prop_merged_span_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merged_span_contains_originals span1 span2 = 
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && 
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && 
     spanEnd merged >= spanEnd span2

-- | Position advancement by string should be consistent with character-by-character advancement
prop_pos_advancement_consistent :: SourcePos -> String -> Property
prop_pos_advancement_consistent pos chars = 
  let byString = advancePosBy chars pos
      byChars = foldl (flip advancePos) pos chars
  in byString === byChars

-- | Merging valid spans should produce valid spans
prop_span_validity_preserved :: SourceSpan -> SourceSpan -> Property
prop_span_validity_preserved span1 span2 = 
  let bothValid = isValidSpan span1 && isValidSpan span2
      merged = mergeSpans span1 span2
  in if bothValid then isValidSpan merged else True  -- No requirement if inputs invalid

-- | In any valid span, start position should be <= end position
prop_span_start_le_end :: SourceSpan -> Property
prop_span_start_le_end span = 
  if isValidSpan span
  then spanStart span <= spanEnd span
  else True  -- Property only applies to valid spans

-- ============================================================================
-- Arbitrary Instances for Source Location Types
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 1000)
    let end = start { posOffset = posOffset start + endOffset }
    return $ SourceSpan start end

-- Helper function for generating test data
choose :: (Int, Int) -> Gen Int
choose = Test.Tasty.QuickCheck.choose