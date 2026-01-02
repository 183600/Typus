{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( SourceLocation(..)
  , Span(..)
  , Position(..)
  , sourceLine
  , sourceColumn
  , sourceFile
  , sourceSpan
  , mkPosition
  , mkSourceLocation
  , spanStart
  , spanEnd
  , spanLength
  , positionCompare
  , positionWithin
  , mergeSpans
  , spanContains
  , spanOverlaps
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, intercalate)

-- Property: Position creation is consistent
prop_position_creation_consistent :: Int -> Int -> Property
prop_position_creation_consistent line col =
  line >= 1 && col >= 1 && line <= 10000 && col <= 10000 ==>
  let pos = mkPosition line col
  in sourceLine pos === line .&&. sourceColumn pos === col

-- Property: SourceLocation creation preserves file information
prop_sourcelocation_creation_preserves_file :: String -> Int -> Int -> Property
prop_sourcelocation_creation_preserves_file file line col =
  line >= 1 && col >= 1 && line <= 1000 && col <= 1000 && not (null file) ==>
  let loc = mkSourceLocation file line col
      pos = mkPosition line col
  in sourceFile loc === file .&&. sourceLine loc === line .&&. sourceColumn loc === col

-- Property: Span L.length calculation is accurate
prop_span_length_accurate :: Int -> Int -> Int -> Property
prop_span_length_accurate startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let startPos = mkPosition startLine startCol
      endPos = mkPosition endLine endCol
      span = Span startPos endPos
      expectedLength = if startLine == endLine 
                      then endCol - startCol + 1
                      else (1000 - startCol + 1) + (endLine - startLine - 1) * 1000 + endCol
  in spanLength span === expectedLength

-- Property: Position comparison is antisymmetric
prop_position_comparison_antisymmetric :: Int -> Int -> Int -> Int -> Property
prop_position_comparison_antisymmetric line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 ==>
  let pos1 = mkPosition line1 col1
      pos2 = mkPosition line2 col2
      cmp1 = positionCompare pos1 pos2
      cmp2 = positionCompare pos2 pos1
  in (cmp1 == EQ) ==> (cmp2 == EQ) .&&.
     ((cmp1 == LT && cmp2 == GT) || (cmp1 == GT && cmp2 == LT))

-- Property: Position within span is consistent
prop_position_within_span_consistent :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_position_within_span_consistent startLine startCol endLine endCol testLine testCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  testLine >= 1 && testCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 &&
  testLine <= 1000 && testCol <= 1000 ==>
  let startPos = mkPosition startLine startCol
      endPos = mkPosition endLine endCol
      span = Span startPos endPos
      testPos = mkPosition testLine testCol
      within = positionWithin testPos span
      lineInRange = testLine >= startLine && testLine <= endLine
      colInRange = if testLine == startLine then testCol >= startCol else True
      colInRangeEnd = if testLine == endLine then testCol <= endCol else True
      expected = lineInRange && colInRange && colInRangeEnd
  in within === expected

-- Property: Span merging preserves containment
prop_span_merging_preserves_containment :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_merging_preserves_containment start1Line start1Col end1Line end1Col 
                                        start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
      merged = mergeSpans span1 span2
  in spanContains merged span1 .&&. spanContains merged span2

-- Property: Span contains is reflexive
prop_span_contains_reflexive :: Int -> Int -> Int -> Int -> Property
prop_span_contains_reflexive startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let span = Span (mkPosition startLine startCol) (mkPosition endLine endCol)
  in spanContains span span

-- Property: Span overlaps is symmetric
prop_span_overlaps_symmetric :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_overlaps_symmetric start1Line start1Col end1Line end1Col 
                              start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
  in spanOverlaps span1 span2 === spanOverlaps span2 span1

-- Property: Merged span contains both original spans
prop_merged_span_contains_originals :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merged_span_contains_originals start1Line start1Col end1Line end1Col 
                                    start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
      merged = mergeSpans span1 span2
  in spanContains merged span1 .&&. spanContains merged span2

-- Property: Position ordering is transitive
prop_position_ordering_transitive :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_position_ordering_transitive line1 col1 line2 col2 line3 col3 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 && line3 >= 1 && col3 >= 1 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 && line3 <= 1000 && col3 <= 1000 ==>
  let pos1 = mkPosition line1 col1
      pos2 = mkPosition line2 col2
      pos3 = mkPosition line3 col3
      cmp12 = positionCompare pos1 pos2
      cmp23 = positionCompare pos2 pos3
      cmp13 = positionCompare pos1 pos3
  in (cmp12 == LT && cmp23 == LT) ==> cmp13 == LT

-- Property: Span L.length is non-negative
prop_span_length_non_negative :: Int -> Int -> Int -> Int -> Property
prop_span_length_non_negative startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let span = Span (mkPosition startLine startCol) (mkPosition endLine endCol)
  in spanLength span >= 0

-- Property: Source location with same position are equal
prop_sourcelocation_same_position_equal :: String -> Int -> Int -> Property
prop_sourcelocation_same_position_equal file line col =
  line >= 1 && col >= 1 && line <= 1000 && col <= 1000 && not (null file) ==>
  let loc1 = mkSourceLocation file line col
      loc2 = mkSourceLocation file line col
  in loc1 === loc2

-- Property: Merged span start is L.minimum of starts
prop_merged_span_start_is_min :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merged_span_start_is_min start1Line start1Col end1Line end1Col 
                              start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
      merged = mergeSpans span1 span2
      mergedStart = spanStart merged
      start1 = spanStart span1
      start2 = spanStart span2
  in positionCompare mergedStart start1 /= GT .&&. positionCompare mergedStart start2 /= GT

-- Property: Merged span end is L.maximum of ends
prop_merged_span_end_is_max :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merged_span_end_is_max start1Line start1Col end1Line end1Col 
                            start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
      merged = mergeSpans span1 span2
      mergedEnd = spanEnd merged
      end1 = spanEnd span1
      end2 = spanEnd span2
  in positionCompare mergedEnd end1 /= LT .&&. positionCompare mergedEnd end2 /= LT

-- Property: Span contains its start position
prop_span_contains_start :: Int -> Int -> Int -> Int -> Property
prop_span_contains_start startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let span = Span (mkPosition startLine startCol) (mkPosition endLine endCol)
      startPos = spanStart span
  in positionWithin startPos span

-- Property: Span contains its end position
prop_span_contains_end :: Int -> Int -> Int -> Int -> Property
prop_span_contains_end startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= 1 && endCol >= 1 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let span = Span (mkPosition startLine startCol) (mkPosition endLine endCol)
      endPos = spanEnd span
  in positionWithin endPos span

-- Property: Zero-L.length span contains only its position
prop_zero_length_span_contains_only_position :: Int -> Int -> Property
prop_zero_length_span_contains_only_position line col =
  line >= 1 && col >= 1 && line <= 1000 && col <= 1000 ==>
  let pos = mkPosition line col
      span = Span pos pos
  in positionWithin pos span .&&. spanLength span === 1

-- Property: Span L.length calculation with multi-line spans
prop_span_length_multi_line :: Int -> Int -> Int -> Property
prop_span_length_multi_line startLine startCol numLines =
  startLine >= 1 && startCol >= 1 && numLines >= 1 && numLines <= 10 &&
  startLine <= 1000 && startCol <= 1000 ==>
  let endLine = startLine + numLines - 1
      endCol = startCol
      span = Span (mkPosition startLine startCol) (mkPosition endLine endCol)
      expectedLength = (1000 - startCol + 1) + (numLines - 2) * 1000 + endCol
  in spanLength span === expectedLength

-- Advanced properties for complex scenarios

-- Property: Complex span merging maintains order
prop_complex_span_merging_order :: [Int] -> Property
prop_complex_span_merging_order lineNumbers =
  not (null lineNumbers) && L.all (>=1) lineNumbers && L.all (<=1000) lineNumbers ==>
  let sortedLines = sort lineNumbers
      positions = L.map (`mkPosition` 1) sortedLines
      spans = zipWith Span positions (L.tail positions ++ [last positions])
      merged = foldl mergeSpans (L.head spans) (L.tail spans)
  in spanLength merged >= L.length lineNumbers - 1

-- Property: Span overlap detection is accurate
prop_span_overlap_detection_accurate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_overlap_detection_accurate start1Line start1Col end1Line end1Col 
                                      start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= 1 && end1Col >= 1 &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= 1 && end2Col >= 1 &&
  (end1Line > start1Line || (end1Line == start1Line && end1Col >= start1Col)) &&
  (end2Line > start2Line || (end2Line == start2Line && end2Col >= start2Col)) &&
  start1Line <= 1000 && start1Col <= 1000 && end1Line <= 1000 && end1Col <= 1000 &&
  start2Line <= 1000 && start2Col <= 1000 && end2Line <= 1000 && end2Col <= 1000 ==>
  let span1 = Span (mkPosition start1Line start1Col) (mkPosition end1Line end1Col)
      span2 = Span (mkPosition start2Line start2Col) (mkPosition end2Line end2Col)
      overlaps = spanOverlaps span1 span2
      merged = mergeSpans span1 span2
      mergedLength = spanLength merged
      sumLength = spanLength span1 + spanLength span2
  in if overlaps 
     then mergedLength <= sumLength && mergedLength >= max (spanLength span1) (spanLength span2)
     else mergedLength == sumLength

-- Property: Position distance calculation consistency
prop_position_distance_consistency :: Int -> Int -> Int -> Int -> Property
prop_position_distance_consistency line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 ==>
  let pos1 = mkPosition line1 col1
      pos2 = mkPosition line2 col2
      span = Span pos1 pos2
      distance = spanLength span
  in distance >= 0

tests :: TestTree
tests = testGroup "SourceLocation Advanced Tests"
  [ fastProperty "Position creation is consistent" prop_position_creation_consistent
  , fastProperty "SourceLocation creation preserves file information" prop_sourcelocation_creation_preserves_file
  , fastProperty "Span L.length calculation is accurate" prop_span_length_accurate
  , fastProperty "Position comparison is antisymmetric" prop_position_comparison_antisymmetric
  , fastProperty "Position within span is consistent" prop_position_within_span_consistent
  , fastProperty "Span merging preserves containment" prop_span_merging_preserves_containment
  , fastProperty "Span contains is reflexive" prop_span_contains_reflexive
  , fastProperty "Span overlaps is symmetric" prop_span_overlaps_symmetric
  , fastProperty "Merged span contains both original spans" prop_merged_span_contains_originals
  , fastProperty "Position ordering is transitive" prop_position_ordering_transitive
  , fastProperty "Span L.length is non-negative" prop_span_length_non_negative
  , fastProperty "Source location with same position are equal" prop_sourcelocation_same_position_equal
  , fastProperty "Merged span start is L.minimum of starts" prop_merged_span_start_is_min
  , fastProperty "Merged span end is L.maximum of ends" prop_merged_span_end_is_max
  , fastProperty "Span contains its start position" prop_span_contains_start
  , fastProperty "Span contains its end position" prop_span_contains_end
  , fastProperty "Zero-L.length span contains only its position" prop_zero_length_span_contains_only_position
  , fastProperty "Span L.length calculation with multi-line spans" prop_span_length_multi_line
  , fastProperty "Complex span merging maintains order" prop_complex_span_merging_order
  , fastProperty "Span overlap detection is accurate" prop_span_overlap_detection_accurate
  , fastProperty "Position distance calculation consistency" prop_position_distance_consistency
  ]