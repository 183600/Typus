{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewAdditionalSourceLocationQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import SourceLocation (SourcePos(..), startPos, posAfter, advancePos, advancePosByText, advancePosBy,
                       SourceSpan(..), emptySpan, spanFrom, spanTo, mergeSpans, isValidSpan)

-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

-- | Test SourcePos properties
prop_start_pos_consistent :: Bool
prop_start_pos_consistent = 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

prop_pos_after_newline :: Positive Int -> Property
prop_pos_after_newline (Positive n) = 
  let pos = startPos { posLine = n, posColumn = 5, posOffset = 100 }
      newPos = posAfter '\n' pos
  in posLine newPos === n + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === 101

prop_pos_after_tab :: Positive Int -> Property
prop_pos_after_tab (Positive n) = 
  let pos = startPos { posLine = n, posColumn = 3, posOffset = 100 }
      newPos = posAfter '\t' pos
      expectedColumn = ((3 - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos === n .&&.
     posColumn newPos === expectedColumn .&&.
     posOffset newPos === 101

prop_pos_after_regular_char :: Positive Int -> Char -> Property
prop_pos_after_regular_char (Positive n) c = 
  c /= '\n' && c /= '\t' ==>
  let pos = startPos { posLine = n, posColumn = 5, posOffset = 100 }
      newPos = posAfter c pos
  in posLine newPos === n .&&.
     posColumn newPos === 6 .&&.
     posOffset newPos === 101

-- | Test advancePos properties
prop_advance_pos_empty :: SourcePos -> Property
prop_advance_pos_empty pos = property $ advancePosBy "" pos === pos

prop_advance_pos_newline :: SourcePos -> Property
prop_advance_pos_newline pos = 
  let newPos = advancePosBy "\n" pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

prop_advance_pos_multiple_chars :: SourcePos -> String -> Property
prop_advance_pos_multiple_chars pos s = 
  not (null s) ==> 
  let newPos = advancePosBy s pos
  in posOffset newPos === posOffset pos + length s

-- | Test advancePosByText properties
prop_advance_pos_by_text_empty :: SourcePos -> Property
prop_advance_pos_by_text_empty pos = property $ advancePosByText (T.pack "") pos === pos

prop_advance_pos_by_text_consistency :: SourcePos -> String -> Property
prop_advance_pos_by_text_consistency pos s = 
  advancePosByText (T.pack s) pos === advancePosBy s pos

-- | Test SourceSpan properties
prop_empty_span_valid :: Bool
prop_empty_span_valid = isValidSpan (emptySpan startPos)

prop_span_from_single_pos :: SourcePos -> Property
prop_span_from_single_pos pos = 
  let span = spanFrom pos
  in isValidSpan span .&&. 
     spanStart span === pos .&&.
     spanEnd span === pos

prop_span_to_single_pos :: SourcePos -> Property
prop_span_to_single_pos pos = 
  let span = spanTo pos
  in isValidSpan span .&&. 
     spanStart span === pos .&&.
     spanEnd span === pos

prop_merge_spans_adjacent :: Positive Int -> Property
prop_merge_spans_adjacent (Positive n) = 
  let pos1 = startPos { posOffset = n }
      pos2 = startPos { posOffset = n + 5 }
      span1 = spanFrom pos1
      span2 = spanFrom pos2
      merged = mergeSpans span1 span2
  in spanStart merged === pos1 .&&.
     spanEnd merged === pos2

prop_merge_spans_order_independent :: SourcePos -> SourcePos -> Property
prop_merge_spans_order_independent pos1 pos2 = 
  let span1 = spanFrom pos1
      span2 = spanFrom pos2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- | Test position ordering properties
prop_pos_ordering_line_major :: Positive Int -> Positive Int -> Property
prop_pos_ordering_line_major (Positive line1) (Positive line2) = 
  line1 /= line2 ==>
  let pos1 = startPos { posLine = line1 }
      pos2 = startPos { posLine = line2 }
      expected = compare line1 line2
  in compare pos1 pos2 === expected

prop_pos_ordering_column_minor :: Positive Int -> Property
prop_pos_ordering_column_minor (Positive col) = 
  let line = 5
      pos1 = startPos { posLine = line, posColumn = col }
      pos2 = startPos { posLine = line, posColumn = col + 1 }
  in property $ pos1 < pos2

prop_pos_ordering_offset_final :: Positive Int -> Property
prop_pos_ordering_offset_final (Positive offset) = 
  let line = 5
      col = 3
      pos1 = startPos { posLine = line, posColumn = col, posOffset = offset }
      pos2 = startPos { posLine = line, posColumn = col, posOffset = offset + 1 }
  in property $ pos1 < pos2

-- | Test span validity properties
prop_span_valid_when_start_before_end :: Positive Int -> Property
prop_span_valid_when_start_before_end (Positive n) = 
  let start = startPos { posOffset = n }
      end = startPos { posOffset = n + 5 }
      span = SourceSpan start end
  in property $ isValidSpan span

prop_span_invalid_when_start_after_end :: Positive Int -> Property
prop_span_invalid_when_start_after_end (Positive n) = 
  let start = startPos { posOffset = n + 5 }
      end = startPos { posOffset = n }
      span = SourceSpan start end
  in property $ not (isValidSpan span)

-- | Test arithmetic properties
prop_offset_monotonic :: SourcePos -> String -> Property
prop_offset_monotonic pos s = 
  let newPos = advancePosBy s pos
  in property $ posOffset newPos >= posOffset pos

prop_line_monotonic_with_newlines :: SourcePos -> String -> Property
prop_line_monotonic_with_newlines pos s = 
  let newlineCount = length (filter (== '\n') s)
      newPos = advancePosBy s pos
  in posLine newPos === posLine pos + newlineCount

-- | Combine all tests
newAdditionalSourceLocationQuickCheckTestSpec :: TestTree
newAdditionalSourceLocationQuickCheckTestSpec = testGroup "New Additional SourceLocation QuickCheck Tests"
  [ testProperty "start position is consistent" prop_start_pos_consistent
  , testProperty "posAfter handles newline" prop_pos_after_newline
  , testProperty "posAfter handles tab" prop_pos_after_tab
  , testProperty "posAfter handles regular character" prop_pos_after_regular_char
  , testProperty "advancePos handles empty string" prop_advance_pos_empty
  , testProperty "advancePos handles newline" prop_advance_pos_newline
  , testProperty "advancePos handles multiple characters" prop_advance_pos_multiple_chars
  , testProperty "advancePosByText handles empty string" prop_advance_pos_by_text_empty
  , testProperty "advancePosByText consistency with advancePos" prop_advance_pos_by_text_consistency
  , testProperty "empty span is valid" prop_empty_span_valid
  , testProperty "spanFrom creates valid span" prop_span_from_single_pos
  , testProperty "spanTo creates valid span" prop_span_to_single_pos
  , testProperty "mergeSpans handles adjacent spans" prop_merge_spans_adjacent
  , testProperty "mergeSpans is order independent" prop_merge_spans_order_independent
  , testProperty "pos ordering is line major" prop_pos_ordering_line_major
  , testProperty "pos ordering is column minor" prop_pos_ordering_column_minor
  , testProperty "pos ordering uses offset as final tiebreaker" prop_pos_ordering_offset_final
  , testProperty "span valid when start before end" prop_span_valid_when_start_before_end
  , testProperty "span invalid when start after end" prop_span_invalid_when_start_after_end
  , testProperty "offset is monotonic" prop_offset_monotonic
  , testProperty "line count increases with newlines" prop_line_monotonic_with_newlines
  ]