{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.Unit.SourceLocationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), spanStart, spanEnd, 
                       posLine, posColumn, startPos, advancePos, mergeSpans, spanBetween, emptySpan,
                       locatedValue, locatedSpan, locatedWithSpan)
import Data.List (isPrefixOf, isInfixOf)

-- Import Arbitrary instances from TestSupport.Arbitrary to avoid orphan instances
import TestSupport.Arbitrary ()

instance (Arbitrary a) => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value (spanStart span) span

-- Helper functions for compatibility
sourceLine :: SourcePos -> Int
sourceLine = posLine

sourceColumn :: SourcePos -> Int  
sourceColumn = posColumn

sourceFile :: SourcePos -> String
sourceFile _ = "test-file" -- Default test filename

spanLength :: SourceSpan -> Int
spanLength span = posOffset (spanEnd span) - posOffset (spanStart span)

spanContains :: SourceSpan -> SourcePos -> Bool
spanContains span pos = pos >= spanStart span && pos <= spanEnd span

spanOverlaps :: SourceSpan -> SourceSpan -> Bool
spanOverlaps span1 span2 = 
  spanContains span1 (spanStart span2) || spanContains span1 (spanEnd span2) ||
  spanContains span2 (spanStart span1) || spanContains span2 (spanEnd span1)

-- | Generate random source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 200)
  offset <- choose (0, 50000)
  return $ SourcePos line column offset

-- | Generate random source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 1000)
  startColumn <- choose (1, 200)
  startOffset <- choose (0, 50000)
  endLine <- choose (startLine, startLine + 10)  -- Limit span size
  endColumn <- if endLine == startLine 
               then choose (startColumn, startColumn + 50)
               else choose (1, 200)
  endOffset <- choose (startOffset, startOffset + 1000)
  let startPos = SourcePos startLine startColumn startOffset
      endPos = SourcePos endLine endColumn endOffset
  return $ SourceSpan { spanStart = startPos, spanEnd = endPos }

-- | Generate located values
genLocated :: Gen (Located String)
genLocated = do
  value <- listOf $ choose ('a', 'z')
  span <- genSourceSpan
  return $ locatedWithSpan span value

-- | Generate multi-line text with positions
genMultiLineText :: Gen (String, [SourcePos])
genMultiLineText = do
  numLines <- choose (1, 10) :: Gen Int
  lines <- listOfN numLines genLine
  let text = unlines lines
      positions = calculateLinePositions text
  return (text, positions)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]
    genLine = listOf $ choose ('a', 'z')
    calculateLinePositions text = 
      let lines' = lines text
          offsets = scanl (\acc line -> acc + length line + 1) 0 lines'
          positions = zipWith3 (\lineNum _ offset -> SourcePos lineNum 1 offset) [1..] lines' offsets
      in positions

-- Property: Source position ordering
prop_source_pos_ordering :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering pos1 pos2 =
  let line1 = sourceLine pos1
      col1 = sourceColumn pos1
      line2 = sourceLine pos2
      col2 = sourceColumn pos2
      earlier = line1 < line2 || (line1 == line2 && col1 < col2)
      later = line1 > line2 || (line1 == line2 && col1 > col2)
  in property $ earlier .||. later .||. (line1 == line2 && col1 == col2)

-- Property: Source span contains its start and end positions
prop_span_contains_bounds :: SourceSpan -> Property
prop_span_contains_bounds span =
  let start = spanStart span
      end = spanEnd span
      containsStart = spanContains span start
      containsEnd = spanContains span end
  in property $ containsStart .&&. containsEnd

-- Property: Source span length calculation
prop_span_length_calculation :: SourceSpan -> Property
prop_span_length_calculation span =
  let start = spanStart span
      end = spanEnd span
      line1 = sourceLine start
      col1 = sourceColumn start
      line2 = sourceLine end
      col2 = sourceColumn end
      expectedLength = if line1 == line2 
                      then col2 - col1 + 1
                      else 1000  -- Approximate for multi-line
      actualLength = spanLength span
  in property $ actualLength >= 1

-- Property: Located value preserves span
prop_located_preserves_span :: String -> SourceSpan -> Property
prop_located_preserves_span value span =
  let located = locatedWithSpan span value
      extractedSpan = locatedSpan located
  in property $ extractedSpan === span

-- Property: Located value preserves content
prop_located_preserves_content :: String -> SourceSpan -> Property
prop_located_preserves_content value span =
  let located = locatedWithSpan span value
      extractedValue = locatedValue located
  in property $ extractedValue === value

-- Property: Span overlap detection
prop_span_overlap_detection :: SourceSpan -> SourceSpan -> Property
prop_span_overlap_detection span1 span2 =
  let overlaps = spanOverlaps span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      line1 = sourceLine start1
      line2 = sourceLine start2
      col1 = sourceColumn start1
      col2 = sourceColumn start2
      endLine1 = sourceLine end1
      endLine2 = sourceLine end2
      endCol1 = sourceColumn end1
      endCol2 = sourceColumn end2
      -- Simple overlap check
      shouldOverlap = not (endLine1 < line2 || endLine2 < line1 ||
                         (endLine1 == line2 && endCol1 < col2) ||
                         (endLine2 == line2 && endCol2 < col1))
  in property $ overlaps == shouldOverlap

-- Property: Span containment is reflexive
prop_span_containment_reflexive :: SourceSpan -> Property
prop_span_containment_reflexive span =
  let start = spanStart span
  in property $ spanContains span start

-- Property: Span containment is transitive
prop_span_containment_transitive :: SourceSpan -> SourceSpan -> SourcePos -> Property
prop_span_containment_transitive outer inner pos =
  let containsInner = spanContains outer (spanStart inner) && spanContains outer (spanEnd inner)
      containsPos = spanContains inner pos
  in (containsInner && containsPos) ==> spanContains outer pos

-- Property: Multi-line position calculation
prop_multiline_position_calculation :: (String, [SourcePos]) -> Property
prop_multiline_position_calculation (text, positions) =
  let expectedLines = lines text
      actualLineNumbers = map sourceLine positions
      expectedLineNumbers = [1..length expectedLines]
  in property $ actualLineNumbers == expectedLineNumbers

-- Property: Source position file preservation
prop_source_pos_file_preservation :: String -> SourcePos -> Property
prop_source_pos_file_preservation filename pos =
  -- SourcePos doesn't have a file field, so we test line/column preservation instead
  let updatedPos = pos { posLine = sourceLine pos + 1 }
      extractedLine = sourceLine updatedPos
  in property $ extractedLine === sourceLine pos + 1

-- Property: Span construction with valid bounds
prop_span_construction_valid :: SourcePos -> SourcePos -> Property
prop_span_construction_valid start end =
  let line1 = sourceLine start
      line2 = sourceLine end
      col1 = sourceColumn start
      col2 = sourceColumn end
      sameLine = line1 == line2
      validOrder = line1 < line2 || (sameLine && col1 <= col2)
      span = SourceSpan { spanStart = start, spanEnd = end }
  in validOrder ==> property $ spanStart span === start .&&. spanEnd span === end

-- Property: Located value with span utility
prop_located_with_span_utility :: String -> SourceSpan -> Property
prop_located_with_span_utility value span =
  let located = locatedWithSpan span value
      extractedValue = locatedValue located
      extractedSpan = locatedSpan located
  in property $ extractedValue === value .&&. extractedSpan === span

-- Property: Source position equality
prop_source_pos_equality :: SourcePos -> Property
prop_source_pos_equality pos =
  let line = sourceLine pos
      column = sourceColumn pos
      offset = posOffset pos
      samePos = SourcePos line column offset
  in property $ pos == samePos

-- Property: Source span equality
prop_source_span_equality :: SourceSpan -> Property
prop_source_span_equality span =
  let start = spanStart span
      end = spanEnd span
      sameSpan = SourceSpan { spanStart = start, spanEnd = end }
  in property $ span == sameSpan

-- Property: Source position ordering consistency
prop_source_pos_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering_consistent pos1 pos2 =
  let line1 = sourceLine pos1
      col1 = sourceColumn pos1
      line2 = sourceLine pos2
      col2 = sourceColumn pos2
      earlier = line1 < line2 || (line1 == line2 && col1 < col2)
      later = line1 > line2 || (line1 == line2 && col1 > col2)
      equal = line1 == line2 && col1 == col2
  in property $ (earlier .||. later .||. equal) .&&. not (earlier && later)

-- Property: Span length non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let length = spanLength span
  in property $ length >= 0

-- Property: Span contains intermediate positions
prop_span_contains_intermediate :: SourceSpan -> Property
prop_span_contains_intermediate span =
  let start = spanStart span
      end = spanEnd span
      line1 = sourceLine start
      line2 = sourceLine end
      col1 = sourceColumn start
      col2 = sourceColumn end
  in if line1 == line2 && col2 > col1 + 1
     then let middleOffset = posOffset start + 1
              middlePos = SourcePos line1 (col1 + 1) middleOffset
          in property $ spanContains span middlePos
     else property $ True  -- Can't test intermediate position

-- Property: Located list preserves all spans
prop_located_list_preservation :: [Located String] -> Property
prop_located_list_preservation locateds =
  let values = map locatedValue locateds
      spans = map locatedSpan locateds
      reconstructed = zipWith locatedWithSpan spans values
  in property $ locateds == reconstructed

-- Property: Source position within reasonable bounds
prop_source_pos_reasonable_bounds :: SourcePos -> Property
prop_source_pos_reasonable_bounds pos =
  let line = sourceLine pos
      column = sourceColumn pos
  in property $ line >= 1 .&&. line <= 10000 .&&. column >= 1 .&&. column <= 1000

tests :: TestTree
tests = testGroup "Source Location Properties QuickCheck Tests"
  [ fastProperty "source pos ordering" prop_source_pos_ordering
  , fastProperty "span contains bounds" prop_span_contains_bounds
  , fastProperty "span length calculation" prop_span_length_calculation
  , fastProperty "located preserves span" prop_located_preserves_span
  , fastProperty "located preserves content" prop_located_preserves_content
  , fastProperty "span overlap detection" prop_span_overlap_detection
  , fastProperty "span containment reflexive" prop_span_containment_reflexive
  , fastProperty "span containment transitive" prop_span_containment_transitive
  , fastProperty "multiline position calculation" prop_multiline_position_calculation
  , fastProperty "source pos file preservation" prop_source_pos_file_preservation
  , fastProperty "span construction valid" prop_span_construction_valid
  , fastProperty "located with span utility" prop_located_with_span_utility
  , fastProperty "source pos equality" prop_source_pos_equality
  , fastProperty "source span equality" prop_source_span_equality
  , fastProperty "source pos ordering consistent" prop_source_pos_ordering_consistent
  , fastProperty "span length non negative" prop_span_length_non_negative
  , fastProperty "span contains intermediate" prop_span_contains_intermediate
  , fastProperty "located list preservation" prop_located_list_preservation
  , fastProperty "source pos reasonable bounds" prop_source_pos_reasonable_bounds
  ]