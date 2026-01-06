{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCoreSourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , advancePos
  , advancePosBy
  )

-- ============================================================================
-- Test Properties for SourceLocation Module
-- ============================================================================

-- | startPos should be (1, 1)
prop_start_pos_constant :: Bool
prop_start_pos_constant = startPos == SourcePos 1 1

-- | posAfter should advance column by 1 for simple characters
prop_pos_after_simple_char :: SourcePos -> Bool
prop_pos_after_simple_char pos = 
  let newPos = posAfter pos '\n'
  in newPos == case pos of
    SourcePos line col -> SourcePos (line + 1) 1

-- | posAt should create consistent positions
prop_pos_at_consistent :: Int -> Int -> Bool
prop_pos_at_consistent line col = 
  line > 0 && col > 0 ==> posAt line col == SourcePos line col

-- | posAtLineCol should be equivalent to posAt
prop_pos_at_line_col_equals_pos_at :: Int -> Int -> Bool
prop_pos_at_line_col_equals_pos_at line col = 
  line > 0 && col > 0 ==> posAtLineCol line col == posAt line col

-- | emptySpan should have same start L.and end position
prop_empty_span_same_positions :: SourcePos -> Bool
prop_empty_span_same_positions pos = 
  let span = emptySpan pos
  in spanStart span == spanEnd span

-- | spanFrom should create span with same start L.and end
prop_span_from_same_positions :: SourcePos -> Bool
prop_span_from_same_positions pos = 
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- | spanTo should create span with start at startPos
prop_span_to_start_at_start :: SourcePos -> Bool
prop_span_to_start_at_start pos = 
  let span = spanTo pos
  in spanStart span == startPos

-- | spanBetween should create valid span
prop_span_between_valid :: SourcePos -> SourcePos -> Bool
prop_span_between_valid pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in isValidSpan span

-- | mergeSpans should create span that encompasses both
prop_merge_spans_encompassing :: SourcePos -> SourcePos -> Bool
prop_merge_spans_encompassing pos1 pos2 = 
  let merged = mergeSpans (spanFrom pos1) (spanFrom pos2)
      start = spanStart merged
      end = spanEnd merged
  in (start <= pos1 && end >= pos1) && (start <= pos2 && end >= pos2)

-- | locatedAt should create located value with correct position
prop_located_at_correct_position :: SourcePos -> String -> Bool
prop_located_at_correct_position pos value = 
  let located = locatedAt pos value
  in locatedPos located == pos && locatedValue located == value

-- | locatedWithSpan should create located value with correct span
prop_located_with_span_correct :: SourcePos -> String -> Bool
prop_located_with_span_correct pos value = 
  let span = spanFrom pos
      located = locatedWithSpan span value
  in locatedSpan located == span && locatedValue located == value

-- | advancePos should handle newline correctly
prop_advance_pos_newline :: SourcePos -> Bool
prop_advance_pos_newline pos = 
  let newPos = advancePos pos '\n'
  in case newPos of
    SourcePos line col -> line > sourceLine pos && col == 1

-- | advancePosBy should handle multiple characters
prop_advance_pos_by_multiple :: SourcePos -> String -> Bool
prop_advance_pos_by_multiple pos s = 
  let finalPos = advancePosBy pos s
      singleSteps = foldl advancePos pos s
  in finalPos == singleSteps

-- | isValidSpan should validate span constraints
prop_is_valid_span_logic :: SourcePos -> SourcePos -> Bool
prop_is_valid_span_logic pos1 pos2 = 
  let span = spanBetween pos1 pos2
      valid = isValidSpan span
      start = spanStart span
      end = spanEnd span
  in valid == (start <= end)

-- Helper function for position comparison
(<=) :: SourcePos -> SourcePos -> Bool
SourcePos l1 c1 <= SourcePos l2 c2 = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "SourceLocation Module QuickCheck Tests"
  [ testProperty "startPos is (1, 1)" prop_start_pos_constant
  , testProperty "posAfter handles newline" prop_pos_after_simple_char
  , testProperty "posAt creates consistent positions" prop_pos_at_consistent
  , testProperty "posAtLineCol equals posAt" prop_pos_at_line_col_equals_pos_at
  , testProperty "emptySpan has same positions" prop_empty_span_same_positions
  , testProperty "spanFrom creates correct span" prop_span_from_same_positions
  , testProperty "spanTo starts at startPos" prop_span_to_start_at_start
  , testProperty "spanBetween creates valid span" prop_span_between_valid
  , testProperty "mergeSpans encompasses both spans" prop_merge_spans_encompassing
  , testProperty "locatedAt creates correct position" prop_located_at_correct_position
  , testProperty "locatedWithSpan creates correct span" prop_located_with_span_correct
  , testProperty "advancePos handles newline" prop_advance_pos_newline
  , testProperty "advancePosBy handles multiple characters" prop_advance_pos_by_multiple
  , testProperty "isValidSpan logic" prop_is_valid_span_logic
  ]