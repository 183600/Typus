{-# LANGUAGE CPP #-}

module Test.Unit.NewSourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, 
                      spanStart, spanEnd, mergeSpans, spanBetween, isValidSpan)
import TestSupport.Arbitrary ()

-- Test 1: Source position arithmetic
prop_sourcepos_offset_calculation :: Int -> Int -> Int -> Property
prop_sourcepos_offset_calculation line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = SourcePos line col offset
  in posOffset pos === offset .&&. posLine pos === line .&&. posColumn pos === col

-- Test 2: Span validity
prop_span_validity :: SourcePos -> SourcePos -> Property
prop_span_validity start end =
  let span = spanBetween start end
  in posOffset start <= posOffset end ==> isValidSpan span === True

-- Test 3: Span merging is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merge12_23 = (span1 `mergeSpans` span2) `mergeSpans` span3
      merge1_23 = span1 `mergeSpans` (span2 `mergeSpans` span3)
  in spanStart merge12_23 === spanStart merge1_23 .&&. 
     spanEnd merge12_23 === spanEnd merge1_23

-- Test 4: Span merging contains original spans
prop_mergeSpans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_originals span1 span2 =
  let merged = span1 `mergeSpans` span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in posOffset (spanStart merged) <= min (posOffset start1) (posOffset start2) .&&.
     posOffset (spanEnd merged) >= max (posOffset end1) (posOffset end2)

-- Test 5: Span between positions
prop_span_between_positions :: SourcePos -> SourcePos -> Property
prop_span_between_positions start end =
  posOffset start <= posOffset end ==>
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Test 6: Empty span properties
prop_empty_span_properties :: SourcePos -> Property
prop_empty_span_properties pos =
  let empty = spanBetween pos pos
  in spanStart empty === pos .&&. spanEnd empty === pos .&&. isValidSpan empty === True

-- Test 7: Position ordering consistency
prop_position_ordering_consistency :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistency pos1 pos2 =
  let line1 = posLine pos1
      col1 = posColumn pos1
      line2 = posLine pos2
      col2 = posColumn pos2
      offset1 = posOffset pos1
      offset2 = posOffset pos2
  in (line1 < line2 || (line1 == line2 && col1 < col2)) ==> offset1 < offset2

-- Test 8: Span length calculation
prop_span_length_calculation :: SourcePos -> SourcePos -> Property
prop_span_length_calculation start end =
  posOffset start <= posOffset end ==>
  let span = spanBetween start end
      length = posOffset (spanEnd span) - posOffset (spanStart span)
  in length >= 0

-- Test 9: Merge with empty span
prop_merge_with_empty_span :: SourceSpan -> Property
prop_merge_with_empty_span span =
  let start = spanStart span
      empty = spanBetween start start
      merged = span `mergeSpans` empty
  in spanStart merged === spanStart span .&&. spanEnd merged === spanEnd span

-- Test 10: Span ordering by start position
prop_span_ordering :: SourceSpan -> SourceSpan -> Property
prop_span_ordering span1 span2 =
  let start1 = spanStart span1
      start2 = spanStart span2
  in posOffset start1 <= posOffset start2 ==> 
     posOffset (spanStart (span1 `mergeSpans` span2)) === posOffset start1

tests :: TestTree
tests = testGroup "New Source Location Math Tests"
  [ fastProperty "SourcePos offset calculation" prop_sourcepos_offset_calculation
  , fastProperty "Span validity" prop_span_validity
  , fastProperty "MergeSpans is associative" prop_mergeSpans_associative
  , fastProperty "MergeSpans contains original spans" prop_mergeSpans_contains_originals
  , fastProperty "Span between positions" prop_span_between_positions
  , fastProperty "Empty span properties" prop_empty_span_properties
  , fastProperty "Position ordering consistency" prop_position_ordering_consistency
  , fastProperty "Span length calculation" prop_span_length_calculation
  , fastProperty "Merge with empty span" prop_merge_with_empty_span
  , fastProperty "Span ordering by start position" prop_span_ordering
  ]