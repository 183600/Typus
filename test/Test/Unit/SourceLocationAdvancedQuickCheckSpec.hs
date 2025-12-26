{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , sourcePos
  , sourceSpan
  , spanStart
  , spanEnd
  , spanContains
  , spanMerge
  , spanLength
  , spanIsEmpty
  , posCompare
  , posInRange
  , spanBetween
  , spanWithLength
  )

import Data.List (sort)

-- | Advanced property tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Advanced QuickCheck Tests"
    [ fastProperty "SourcePos ordering is transitive" prop_sourcePos_transitive
    , fastProperty "SourceSpan contains its start and end positions" prop_span_contains_start_end
    , fastProperty "Span merge contains both original spans" prop_span_merge_contains_both
    , fastProperty "Span length is non-negative" prop_span_length_non_negative
    , fastProperty "Empty span has zero length" prop_empty_span_zero_length
    , fastProperty "Span between positions contains both positions" prop_span_between_contains_both
    , fastProperty "Span with length has correct end position" prop_span_with_length_correct_end
    , fastProperty "Position range check is inclusive" prop_pos_range_inclusive
    , fastProperty "Span merge is commutative" prop_span_merge_commutative
    , fastProperty "Span merge is associative" prop_span_merge_associative
    , fastProperty "Located values preserve position information" prop_located_preserves_position
    , fastProperty "Span comparison respects start position" prop_span_comparison_respects_start
    , fastProperty "Span length calculation is accurate" prop_span_length_accurate
    , fastProperty "Position offset calculation is consistent" prop_position_offset_consistent
    , fastProperty "Span contains positions within its range" prop_span_contains_range_positions
    ]

-- Property: SourcePos ordering is transitive
prop_sourcePos_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcePos_transitive pos1 pos2 pos3 =
  let cmp12 = posCompare pos1 pos2
      cmp23 = posCompare pos2 pos3
      cmp13 = posCompare pos1 pos3
  in (cmp12 <= 0 && cmp23 <= 0) ==> cmp13 <= 0

-- Property: SourceSpan contains its start and end positions
prop_span_contains_start_end :: SourceSpan -> Property
prop_span_contains_start_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ spanContains span start .&&. spanContains span end

-- Property: Span merge contains both original spans
prop_span_merge_contains_both :: SourceSpan -> SourceSpan -> Property
prop_span_merge_contains_both span1 span2 =
  let merged = spanMerge span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in property $ spanContains merged start1 .&&. spanContains merged end1 .&&.
                spanContains merged start2 .&&. spanContains merged end2

-- Property: Span length is non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let length = spanLength span
  in property $ length >= 0

-- Property: Empty span has zero length
prop_empty_span_zero_length :: SourcePos -> Property
prop_empty_span_zero_length pos =
  let emptySpan = sourceSpan pos pos
  in spanLength emptySpan === 0

-- Property: Span between positions contains both positions
prop_span_between_contains_both :: SourcePos -> SourcePos -> Property
prop_span_between_contains_both pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ spanContains span pos1 .&&. spanContains span pos2

-- Property: Span with length has correct end position
prop_span_with_length_correct_end :: SourcePos -> Int -> Property
prop_span_with_length_correct_end pos length =
  length >= 0 ==>
  let span = spanWithLength pos length
      expectedEnd = pos { sourcePosOffset = sourcePosOffset pos + length }
  in spanEnd span === expectedEnd

-- Property: Position range check is inclusive
prop_pos_range_inclusive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_pos_range_inclusive start middle end =
  posCompare start end <= 0 ==>
  let inRange = posInRange start end middle
      startInRange = posInRange start end start
      endInRange = posInRange start end end
  in property $ (posCompare start middle >= 0 && posCompare middle end <= 0) === inRange .&&.
                startInRange .&&. endInRange

-- Property: Span merge is commutative
prop_span_merge_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merge_commutative span1 span2 =
  let merge12 = spanMerge span1 span2
      merge21 = spanMerge span2 span1
  in merge12 === merge21

-- Property: Span merge is associative
prop_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merge_associative span1 span2 span3 =
  let merge12_3 = spanMerge (spanMerge span1 span2) span3
      merge1_23 = spanMerge span1 (spanMerge span2 span3)
  in merge12_3 === merge1_23

-- Property: Located values preserve position information
prop_located_preserves_position :: SourcePos -> SourceSpan -> String -> Property
prop_located_preserves_position pos span value =
  let located = Located value pos span
  in locatedValue located === value .&&.
     locatedPos located === pos .&&.
     locatedSpan located === span

-- Property: Span comparison respects start position
prop_span_comparison_respects_start :: SourceSpan -> SourceSpan -> Property
prop_span_comparison_respects_start span1 span2 =
  let start1 = spanStart span1
      start2 = spanStart span2
      cmp = posCompare start1 start2
  in (cmp < 0) ==> spanLength (spanMerge span1 span2) >= spanLength span2

-- Property: Span length calculation is accurate
prop_span_length_accurate :: SourcePos -> Int -> Property
prop_span_length_accurate pos length =
  length >= 0 && length <= 1000 ==> -- Limit for reasonable test size
  let span = spanWithLength pos length
      calculatedLength = spanLength span
  in calculatedLength === length

-- Property: Position offset calculation is consistent
prop_position_offset_consistent :: SourcePos -> SourcePos -> Property
prop_position_offset_consistent pos1 pos2 =
  posCompare pos1 pos2 >= 0 ==>
  let offset1 = sourcePosOffset pos1
      offset2 = sourcePosOffset pos2
  in offset2 >= offset1

-- Property: Span contains positions within its range
prop_span_contains_range_positions :: SourceSpan -> SourcePos -> Property
prop_span_contains_range_positions span testPos =
  let start = spanStart span
      end = spanEnd span
      contains = spanContains span testPos
      inRange = posInRange start end testPos
  in contains === inRange

-- Additional advanced properties

-- Property: Multiple position ordering maintains consistency
prop_multiple_position_ordering :: [SourcePos] -> Property
prop_multiple_position_ordering positions =
  not (null positions) ==>
  let sorted = sort positions
      isOrdered = all (\(a, b) -> posCompare a b <= 0) (zip sorted (tail sorted))
  in property $ isOrdered

-- Property: Span merge with empty span preserves original
prop_span_merge_empty_preserves :: SourceSpan -> SourcePos -> Property
prop_span_merge_empty_preserves span pos =
  let emptySpan = sourceSpan pos pos
      merged = spanMerge span emptySpan
  in merged === span

-- Property: Nested spans maintain containment property
prop_nested_spans_containment :: SourcePos -> Int -> Int -> Property
prop_nested_spans_containment pos outerLen innerLen =
  outerLen >= 0 && innerLen >= 0 && innerLen <= outerLen ==> 
  let outerSpan = spanWithLength pos outerLen
      innerStartPos = pos { sourcePosOffset = sourcePosOffset pos + (outerLen - innerLen) `div` 2 }
      innerSpan = spanWithLength innerStartPos innerLen
      innerStart = spanStart innerSpan
      innerEnd = spanEnd innerSpan
  in property $ spanContains outerSpan innerStart .&&. spanContains outerSpan innerEnd

-- Property: Span length equals offset difference
prop_span_length_offset_diff :: SourcePos -> Int -> Property
prop_span_length_offset_diff pos length =
  length >= 0 && length <= 1000 ==>
  let span = spanWithLength pos length
      startOffset = sourcePosOffset (spanStart span)
      endOffset = sourcePosOffset (spanEnd span)
  in spanLength span === (endOffset - startOffset)

-- Property: Position in multiple spans consistency
prop_position_multiple_spans :: SourcePos -> [SourceSpan] -> Property
prop_position_multiple_spans testPos spans =
  not (null spans) ==>
  let containingSpans = filter (spanContains testPos) spans
      mergedSpan = foldl spanMerge (head containingSpans) (tail containingSpans)
  in not (null containingSpans) ==> spanContains mergedSpan testPos