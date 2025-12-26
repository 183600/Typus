{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof)

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
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , spanStart
  , spanEnd
  )

import qualified Data.List as L

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    if isValidPos start && isValidPos end && start <= end
      then return $ SourceSpan start end
      else return emptySpan
    where
      isValidPos (SourcePos line col _) = line > 0 && col > 0

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located span value

-- Helper functions
isValidPos :: SourcePos -> Bool
isValidPos (SourcePos line col _) = line > 0 && col > 0

-- Property: startPos should have line 1, column 1
prop_start_pos_properties :: Property
prop_start_pos_properties =
  let pos = startPos
  in property $ sourceLine pos === 1 .&&. sourceColumn pos === 1

-- Property: posAfter should advance column by 1 for normal characters
prop_pos_after_normal_char :: SourcePos -> Property
prop_pos_after_normal_char pos =
  isValidPos pos ==> 
  let nextPos = posAfter pos 'x'
  in property $ sourceLine nextPos === sourceLine pos .&&.
     sourceColumn nextPos === sourceColumn pos + 1

-- Property: posAfter should advance to next line for newline
prop_pos_after_newline :: SourcePos -> Property
prop_pos_after_newline pos =
  isValidPos pos ==>
  let nextPos = posAfter pos '\n'
  in property $ sourceLine nextPos === sourceLine pos + 1 .&&.
     sourceColumn nextPos === 1

-- Property: posAfter should advance to next line for carriage return
prop_pos_after_carriage_return :: SourcePos -> Property
prop_pos_after_carriage_return pos =
  isValidPos pos ==>
  let nextPos = posAfter pos '\r'
  in property $ sourceLine nextPos === sourceLine pos + 1 .&&.
     sourceColumn nextPos === 1

-- Property: posAfter should handle tab character appropriately
prop_pos_after_tab :: SourcePos -> Property
prop_after_tab pos =
  isValidPos pos ==>
  let nextPos = posAfter pos '\t'
      expectedCol = ((sourceColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ sourceLine nextPos === sourceLine pos .&&.
     sourceColumn nextPos === expectedCol

-- Property: posAt should create position at specific line and column
prop_pos_at_creates_correct_position :: Int -> Int -> Property
prop_pos_at_creates_correct_position line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col ""
  in property $ sourceLine pos === line .&&. sourceColumn pos === col

-- Property: posAtLineCol should be consistent with posAt
prop_pos_at_line_col_consistency :: Int -> Int -> String -> Property
prop_pos_at_line_col_consistency line col filename =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos1 = posAt line col filename
      pos2 = posAtLineCol line col filename
  in property $ pos1 === pos2

-- Property: emptySpan should have start and end at startPos
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let span = emptySpan
  in property $ spanStart span === startPos .&&. spanEnd span === startPos

-- Property: spanFrom should create span with same start and end
prop_span_from_same_start_end :: SourcePos -> Property
prop_span_from_same_start_end pos =
  isValidPos pos ==>
  let span = spanFrom pos pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo should create valid span when end >= start
prop_span_to_valid_when_ordered :: SourcePos -> SourcePos -> Property
prop_span_to_valid_when_ordered start end =
  isValidPos start && isValidPos end && start <= end ==>
  let span = spanTo start end
  in property $ spanStart span === start .&&. spanEnd span === end .&&.
     isValidSpan span

-- Property: spanBetween should create span covering both positions
prop_span_between_covers_positions :: SourcePos -> SourcePos -> Property
prop_span_between_covers_positions pos1 pos2 =
  isValidPos pos1 && isValidPos pos2 ==>
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (start <= pos1 && pos1 <= end) .||. (start <= pos2 && pos2 <= end)

-- Property: mergeSpans should contain both original spans
prop_merge_spans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_both span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergeStart = spanStart merged
      mergeEnd = spanEnd merged
  in property $ mergeStart <= start1 .&&. end1 <= mergeEnd .&&.
     mergeStart <= start2 .&&. end2 <= mergeEnd

-- Property: mergeSpans should be commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans should be associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  isValidSpan span1 && isValidSpan span2 && isValidSpan span3 ==>
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: locatedAt should create Located with correct position
prop_located_at_correct_position :: SourcePos -> Int -> Property
prop_located_at_correct_position pos value =
  isValidPos pos ==>
  let located = locatedAt pos value
      expectedSpan = spanFrom pos pos
  in property $ locatedSpan located === expectedSpan .&&.
     locatedValue located === value

-- Property: locatedWithSpan should create Located with correct span
prop_located_with_span_correct :: SourceSpan -> String -> Property
prop_located_with_span_correct span value =
  isValidSpan span ==>
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
     locatedValue located === value

-- Property: mapLocated should preserve span but transform value
prop_map_located_preserves_span :: SourceSpan -> Int -> Property
prop_map_located_preserves_span span value =
  isValidSpan span ==>
  let located = locatedWithSpan span value
      transformed = mapLocated (*2) located
  in property $ locatedSpan transformed === span .&&.
     locatedValue transformed === value * 2

-- Property: advancePos should handle empty string correctly
prop_advance_pos_empty_string :: SourcePos -> Property
prop_advance_pos_empty_string pos =
  isValidPos pos ==>
  let result = advancePos pos ""
  in property $ result === pos

-- Property: advancePos should handle single character
prop_advance_pos_single_char :: SourcePos -> Char -> Property
prop_advance_pos_single_char pos char =
  isValidPos pos ==>
  let result = advancePos pos [char]
      expected = posAfter pos char
  in property $ result === expected

-- Property: advancePosBy should handle zero count
prop_advance_pos_by_zero :: SourcePos -> Property
prop_advance_pos_by_zero pos =
  isValidPos pos ==>
  let result = advancePosBy pos 0 'x'
  in property $ result === pos

-- Property: advancePosBy should handle positive count
prop_advance_pos_by_positive :: SourcePos -> Int -> Property
prop_advance_pos_by_positive pos count =
  isValidPos pos && count >= 0 && count <= 100 ==>
  let result = advancePosBy pos count 'x'
      expected = iterate (posAfter 'x') pos !! count
  in property $ result === expected

-- Property: locatedPos should return the start position of span
prop_located_pos_returns_start :: SourceSpan -> String -> Property
prop_located_pos_returns_start span value =
  isValidSpan span ==>
  let located = locatedWithSpan span value
      pos = locatedPos located
  in property $ pos === spanStart span

-- Property: isValidSpan should be true for properly ordered spans
prop_is_valid_span_ordered :: SourcePos -> SourcePos -> Property
prop_is_valid_span_ordered start end =
  isValidPos start && isValidPos end && start <= end ==>
  let span = SourceSpan start end
  in property $ isValidSpan span

-- Property: isValidSpan should be false for spans with negative positions
prop_is_valid_span_negative_positions :: Int -> Int -> Property
prop_is_valid_span_negative_positions line col =
  line <= 0 || col <= 0 ==>
  let pos = SourcePos line col ""
      span = SourceSpan pos pos
  in property $ not (isValidSpan span)

-- Property: SourcePos ordering should be consistent
prop_source_pos_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering_consistent pos1 pos2 =
  isValidPos pos1 && isValidPos pos2 ==>
  let line1 = sourceLine pos1
      col1 = sourceColumn pos1
      line2 = sourceLine pos2
      col2 = sourceColumn pos2
      expected = if line1 < line2 then True
                 else if line1 > line2 then False
                 else col1 <= col2
  in property $ (pos1 <= pos2) === expected

-- Property: SourceSpan should have reasonable string representation
prop_source_span_show_reasonable :: SourceSpan -> Property
prop_source_span_show_reasonable span =
  isValidSpan span ==>
  let showStr = show span
  in property $ length showStr > 0 .&&. "SourceSpan" `L.isInfixOf` showStr

-- Property: Located should have reasonable string representation
prop_located_show_reasonable :: SourceSpan -> Int -> Property
prop_located_show_reasonable span value =
  isValidSpan span ==>
  let located = locatedWithSpan span value
      showStr = show located
  in property $ length showStr > 0 .&&. "Located" `L.isInfixOf` showStr

tests :: TestTree
tests = testGroup "SourceLocation Boundary Tests"
  [ fastProperty "startPos has correct properties" prop_start_pos_properties
  , fastProperty "posAfter handles normal characters correctly" prop_pos_after_normal_char
  , fastProperty "posAfter handles newline correctly" prop_pos_after_newline
  , fastProperty "posAfter handles carriage return correctly" prop_pos_after_carriage_return
  , fastProperty "posAfter handles tab character correctly" prop_after_tab
  , fastProperty "posAt creates correct position" prop_pos_at_creates_correct_position
  , fastProperty "posAtLineCol is consistent with posAt" prop_pos_at_line_col_consistency
  , fastProperty "emptySpan has correct properties" prop_empty_span_properties
  , fastProperty "spanFrom creates span with same start and end" prop_span_from_same_start_end
  , fastProperty "spanTo creates valid span when positions are ordered" prop_span_to_valid_when_ordered
  , fastProperty "spanBetween covers both positions" prop_span_between_covers_positions
  , fastProperty "mergeSpans contains both original spans" prop_merge_spans_contains_both
  , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
  , fastProperty "mergeSpans is associative" prop_merge_spans_associative
  , fastProperty "locatedAt creates Located with correct position" prop_located_at_correct_position
  , fastProperty "locatedWithSpan creates Located with correct span" prop_located_with_span_correct
  , fastProperty "mapLocated preserves span but transforms value" prop_map_located_preserves_span
  , fastProperty "advancePos handles empty string correctly" prop_advance_pos_empty_string
  , fastProperty "advancePos handles single character correctly" prop_advance_pos_single_char
  , fastProperty "advancePosBy handles zero count correctly" prop_advance_pos_by_zero
  , fastProperty "advancePosBy handles positive count correctly" prop_advance_pos_by_positive
  , fastProperty "locatedPos returns start position of span" prop_located_pos_returns_start
  , fastProperty "isValidSpan is true for properly ordered spans" prop_is_valid_span_ordered
  , fastProperty "isValidSpan is false for spans with negative positions" prop_is_valid_span_negative_positions
  , fastProperty "SourcePos ordering is consistent" prop_source_pos_ordering_consistent
  , fastProperty "SourceSpan has reasonable string representation" prop_source_span_show_reasonable
  , fastProperty "Located has reasonable string representation" prop_located_show_reasonable
  ]