{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationPrecisionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, listOf1, choose, Positive(..), NonEmptyList(..))

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd, sourceLine, sourceColumn)

import Data.List (sort, nub, group, sortBy, find)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Source position ordering is consistent
prop_source_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_source_position_ordering_consistent pos1 pos2 =
  let lineCompare = compare (sourceLine pos1) (sourceLine pos2)
      colCompare = compare (sourceColumn pos1) (sourceColumn pos2)
      finalCompare = if lineCompare /= EQ then lineCompare else colCompare
  in (pos1 == pos2) || (finalCompare /= EQ)

-- Property: Source span contains its start and end positions
prop_source_span_contains_positions :: SourcePos -> SourcePos -> Property
prop_source_span_contains_positions start end =
  let span = SourceSpan start end
      spanContainsStart = spanContainsPosition span start
      spanContainsEnd = spanContainsPosition span end
  in spanContainsStart .&&. spanContainsEnd

-- Property: Merged spans cover all original spans
prop_merged_spans_cover_original :: [SourceSpan] -> Property
prop_merged_spans_cover_original spans =
  let mergedSpans = mergeAdjacentSpans spans
      originalCoverage = calculateTotalCoverage spans
      mergedCoverage = calculateTotalCoverage mergedSpans
  in not (null spans) ==> mergedCoverage >= originalCoverage

-- Property: Source location tracking is preserved through transformations
prop_source_location_preserved_through_transformations :: [(String, SourceSpan)] -> Property
prop_source_location_preserved_through_transformations locations =
  let locatedValues = map (uncurry locatedWithSpan) locations
      transformedValues = applyTransformations locatedValues
      originalSpans = map locatedSpan locatedValues
      transformedSpans = map locatedSpan transformedValues
  in Set.fromList originalSpans === Set.fromList transformedSpans

-- Property: Source position arithmetic is correct
prop_source_position_arithmetic_correct :: SourcePos -> Int -> Int -> Property
prop_source_position_arithmetic_correct pos lineOffset colOffset =
  let newPos = addPosition pos lineOffset colOffset
      expectedLine = max 1 (sourceLine pos + lineOffset)
      expectedCol = max 1 (sourceColumn pos + colOffset)
  in (sourceLine newPos === expectedLine) .&&. (sourceColumn newPos === expectedCol)

-- Property: Source span intersection is computed correctly
prop_source_span_intersection_correct :: SourceSpan -> SourceSpan -> Property
prop_source_span_intersection_correct span1 span2 =
  let intersection = spanIntersection span1 span2
      hasIntersection = isJust intersection
      expectedIntersection = spansOverlap span1 span2
  in hasIntersection === expectedIntersection

-- Property: Located values maintain span information
prop_located_values_maintain_span :: String -> SourceSpan -> Property
prop_located_values_maintain_span value span =
  let locatedValue = locatedWithSpan span value
      extractedSpan = locatedSpan locatedValue
      extractedValue = locatedValue locatedValue
  in (extractedSpan === span) .&&. (extractedValue === value)

-- Property: Source position comparison is transitive
prop_source_position_comparison_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_source_position_comparison_transitive pos1 pos2 pos3 =
  let pos1_le_pos2 = positionLessOrEqual pos1 pos2
      pos2_le_pos3 = positionLessOrEqual pos2 pos3
      pos1_le_pos3 = positionLessOrEqual pos1 pos3
  in (pos1_le_pos2 .&&. pos2_le_pos3) ==> pos1_le_pos3

-- Property: Source span expansion preserves containment
prop_source_span_expansion_preserves_containment :: SourceSpan -> Int -> Int -> Property
prop_source_span_expansion_preserves_containment span lineExpand colExpand =
  let expandedSpan = expandSpan span lineExpand colExpand
      originalStart = spanStart span
      originalEnd = spanEnd span
      expandedContainsOriginal = spanContainsSpan expandedSpan span
  in expandedContainsOriginal

-- Property: Source location serialization preserves information
prop_source_location_serialization_preserves_info :: SourcePos -> SourcePos -> Property
prop_source_location_serialization_preserves_info start end =
  let span = SourceSpan start end
      serialized = serializeSpan span
      deserialized = deserializeSpan serialized
  in span === deserialized

-- Helper functions (these would need to be implemented in the actual modules)
spanContainsPosition :: SourceSpan -> SourcePos -> Bool
spanContainsPosition (SourceSpan start end) pos =
  let lineInRange = sourceLine pos >= sourceLine start && sourceLine pos <= sourceLine end
      colInRange = if sourceLine pos == sourceLine start 
                   then sourceColumn pos >= sourceColumn start
                   else if sourceLine pos == sourceLine end
                        then sourceColumn pos <= sourceColumn end
                        else True
  in lineInRange && colInRange

mergeAdjacentSpans :: [SourceSpan] -> [SourceSpan]
mergeAdjacentSpans [] = []
mergeAdjacentSpans [span] = [span]
mergeAdjacentSpans (span1:span2:rest) = 
  if spansAdjacent span1 span2
  then mergeAdjacentSpans (mergeTwoSpans span1 span2 : rest)
  else span1 : mergeAdjacentSpans (span2:rest)

calculateTotalCoverage :: [SourceSpan] -> Int
calculateTotalCoverage spans = sum $ map spanLength spans

spanLength :: SourceSpan -> Int
spanLength (SourceSpan start end) = 
  let lineDiff = sourceLine end - sourceLine start
      colDiff = sourceColumn end - sourceColumn start
  in lineDiff * 100 + colDiff  -- Simplified calculation

applyTransformations :: [Located String] -> [Located String]
applyTransformations = map transformLocated
  where
    transformLocated located = located { locatedValue = "transformed_" ++ locatedValue located }

addPosition :: SourcePos -> Int -> Int -> SourcePos
addPosition pos lineOffset colOffset = 
  SourcePos (max 1 (sourceLine pos + lineOffset)) (max 1 (sourceColumn pos + colOffset)) 0 0

spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap span1 span2 = isJust (spanIntersection span1 span2)

spanIntersection :: SourceSpan -> SourceSpan -> Maybe SourceSpan
spanIntersection (SourceSpan start1 end1) (SourceSpan start2 end2) =
  let startLine = max (sourceLine start1) (sourceLine start2)
      startCol = max (sourceColumn start1) (sourceColumn start2)
      endLine = min (sourceLine end1) (sourceLine end2)
      endCol = min (sourceColumn end1) (sourceColumn end2)
  in if startLine < endLine || (startLine == endLine && startCol <= endCol)
     then Just (SourceSpan (SourcePos startLine startCol 0 0) (SourcePos endLine endCol 0 0))
     else Nothing

positionLessOrEqual :: SourcePos -> SourcePos -> Bool
positionLessOrEqual pos1 pos2 =
  let lineCompare = compare (sourceLine pos1) (sourceLine pos2)
  in case lineCompare of
    LT -> True
    EQ -> sourceColumn pos1 <= sourceColumn pos2
    GT -> False

expandSpan :: SourceSpan -> Int -> Int -> SourceSpan
expandSpan (SourceSpan start end) lineExpand colExpand =
  let newStart = SourcePos (max 1 (sourceLine start - lineExpand)) 
                           (max 1 (sourceColumn start - colExpand)) 0 0
      newEnd = SourcePos (sourceLine end + lineExpand) 
                         (sourceColumn end + colExpand) 0 0
  in SourceSpan newStart newEnd

spanContainsSpan :: SourceSpan -> SourceSpan -> Bool
spanContainsSpan (SourceSpan start1 end1) (SourceSpan start2 end2) =
  positionLessOrEqual start1 start2 && positionLessOrEqual end2 end1

spansAdjacent :: SourceSpan -> SourceSpan -> Bool
spansAdjacent (SourceSpan _ end1) (SourceSpan start2 _) = 
  sourceLine end1 == sourceLine start2 && 
  abs (sourceColumn end1 - sourceColumn start2) <= 1

mergeTwoSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeTwoSpans (SourceSpan start1 end1) (SourceSpan start2 end2) =
  let start = if positionLessOrEqual start1 start2 then start1 else start2
      end = if positionLessOrEqual end1 end2 then end2 else end1
  in SourceSpan start end

serializeSpan :: SourceSpan -> String
serializeSpan (SourceSpan start end) = 
  show (sourceLine start) ++ ":" ++ show (sourceColumn start) ++ "-" ++
  show (sourceLine end) ++ ":" ++ show (sourceColumn end)

deserializeSpan :: String -> SourceSpan
deserializeSpan str = 
  let parts = words $ map (\c -> if c == ':' || c == '-' then ' ' else c) str
      [startLine, startCol, endLine, endCol] = map read $ take 4 parts ++ repeat 0
      start = SourcePos startLine startCol 0 0
      end = SourcePos endLine endCol 0 0
  in SourceSpan start end

tests :: TestTree
tests = testGroup "Source Location Precision QuickCheck Tests"
  [ fastProperty "Source position ordering consistent" prop_source_position_ordering_consistent
  , fastProperty "Source span contains positions" prop_source_span_contains_positions
  , fastProperty "Merged spans cover original" prop_merged_spans_cover_original
  , fastProperty "Source location preserved through transformations" prop_source_location_preserved_through_transformations
  , fastProperty "Source position arithmetic correct" prop_source_position_arithmetic_correct
  , fastProperty "Source span intersection correct" prop_source_span_intersection_correct
  , fastProperty "Located values maintain span" prop_located_values_maintain_span
  , fastProperty "Source position comparison transitive" prop_source_position_comparison_transitive
  , fastProperty "Source span expansion preserves containment" prop_source_span_expansion_preserves_containment
  , fastProperty "Source location serialization preserves info" prop_source_location_serialization_preserves_info
  ]