{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , advancePos
  , advancePosBy
  , spanStart
  , spanEnd
  )

-- ============================================================================
-- Test Generators
-- ============================================================================

-- Generate valid source positions (line and column >= 1)
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- Generate valid source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    endLine <- choose (startLine, startLine + 50)  -- Ensure end line >= start line
    endCol <- if endLine == startLine 
              then choose (startCol, startCol + 50)  -- Same line: end col >= start col
              else choose (1, 100)  -- Different line: any col
    return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- Generate arbitrary located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value span

-- ============================================================================
-- Mathematical Properties for SourcePos
-- ============================================================================

-- Property: startPos should always be (1, 1)
propStartPosConstant :: Bool
propStartPosConstant = startPos == SourcePos 1 1

-- Property: posAfter should increase column by 1, except at line boundaries
propPosAfterIncreasesColumn :: SourcePos -> String -> Bool
propPosAfterIncreasesColumn pos str = 
  let newPos = posAfter pos str
  in spLine newPos == spLine pos && 
     (if null str then spColumn newPos == spColumn pos
      else spColumn newPos >= spColumn pos)

-- Property: posAt should create position with correct line and column
propPosAtCreatesCorrectPosition :: Int -> Int -> Bool
propPosAtCreatesCorrectPosition line col =
  let pos = posAt line col
  in spLine pos == line && spColumn pos == col

-- Property: advancePos should handle newline correctly
propAdvancePosHandlesNewline :: SourcePos -> Bool
propAdvancePosHandlesNewline pos =
  let newPos = advancePos pos '\n'
  in spLine newPos == spLine pos + 1 && spColumn newPos == 1

-- Property: advancePos should handle tab correctly
propAdvancePosHandlesTab :: SourcePos -> Bool
propAdvancePosHandlesTab pos =
  let newPos = advancePos pos '\t'
      expectedCol = ((spColumn pos - 1) `div` 8 + 1) * 8 + 1
  in spLine newPos == spLine pos && spColumn newPos == expectedCol

-- Property: advancePosBy should sum up character advances
propAdvancePosByIsConsistent :: SourcePos -> String -> Bool
propAdvancePosByIsConsistent pos str =
  let foldAdvance = foldl advancePos pos str
      byAdvance = advancePosBy pos str
  in spLine foldAdvance == spLine byAdvance && 
     spColumn foldAdvance == spColumn byAdvance

-- ============================================================================
-- Mathematical Properties for SourceSpan
-- ============================================================================

-- Property: emptySpan should be invalid
propEmptySpanIsInvalid :: Bool
propEmptySpanIsInvalid = not (isValidSpan emptySpan)

-- Property: spanFrom should create span from single position
propSpanFromCreatesValidSpan :: SourcePos -> Bool
propSpanFromCreatesValidSpan pos =
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos && isValidSpan span

-- Property: spanTo should create span from start to end position
propSpanToCreatesCorrectSpan :: SourcePos -> SourcePos -> Bool
propSpanToCreatesCorrectSpan start end =
  let span = spanTo start end
  in spanStart span == start && spanEnd span == end

-- Property: spanBetween should create span that encompasses both positions
propSpanBetweenEncompassesPositions :: SourcePos -> SourcePos -> Bool
propSpanBetweenEncompassesPositions pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (spLine start <= spLine pos1 || (spLine start == spLine pos1 && spColumn start <= spColumn pos1)) &&
     (spLine end >= spLine pos2 || (spLine end == spLine pos2 && spColumn end >= spColumn pos2))

-- Property: mergeSpans should be commutative
propMergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
propMergeSpansCommutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

-- Property: mergeSpans should be associative
propMergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
propMergeSpansAssociative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

-- Property: mergeSpans should contain both original spans
propMergeSpansContainsOriginals :: SourceSpan -> SourceSpan -> Bool
propMergeSpansContainsOriginals span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in (spLine mergedStart <= spLine start1 || (spLine mergedStart == spLine start1 && spColumn mergedStart <= spColumn start1)) &&
     (spLine mergedEnd >= spLine end1 || (spLine mergedEnd == spLine end1 && spColumn mergedEnd >= spColumn end1)) &&
     (spLine mergedStart <= spLine start2 || (spLine mergedStart == spLine start2 && spColumn mergedStart <= spColumn start2)) &&
     (spLine mergedEnd >= spLine end2 || (spLine mergedEnd == spLine end2 && spColumn mergedEnd >= spColumn end2))

-- ============================================================================
-- Mathematical Properties for Located
-- ============================================================================

-- Property: locatedAt should create located value with spanFrom position
propLocatedAtUsesSpanFrom :: Int -> Int -> String -> Bool
propLocatedAtUsesSpanFrom line col value =
  let pos = posAt line col
      located = locatedAt value pos
  case located of
    Located val span -> val == value && spanStart span == pos && spanEnd span == pos

-- Property: locatedWithSpan should preserve the span
propLocatedWithSpanPreservesSpan :: String -> SourceSpan -> Bool
propLocatedWithSpanPreservesSpan value span =
  let located = locatedWithSpan value span
  case located of
    Located val s -> val == value && s == span

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test position arithmetic edge cases
testPositionArithmeticEdgeCases :: TestTree
testPositionArithmeticEdgeCases = testCase "Position arithmetic edge cases" $ do
  let pos = SourcePos 1 1
  let afterNewline = advancePos pos '\n'
  assertEqual "After newline, line should be 2" 2 (spLine afterNewline)
  assertEqual "After newline, column should be 1" 1 (spColumn afterNewline)
  
  let afterTab = advancePos pos '\t'
  let expectedTabCol = ((1 - 1) `div` 8 + 1) * 8 + 1
  assertEqual "After tab, column should align to next tab stop" expectedTabCol (spColumn afterTab)

-- Test span validity edge cases
testSpanValidityEdgeCases :: TestTree
testSpanValidityEdgeCases = testCase "Span validity edge cases" $ do
  let validSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
  assertBool "Span with end > start should be valid" (isValidSpan validSpan)
  
  let samePosSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 1)
  assertBool "Span with same start and end should be valid" (isValidSpan samePosSpan)

-- Test merge spans edge cases
testMergeSpansEdgeCases :: TestTree
testMergeSpansEdgeCases = testCase "Merge spans edge cases" $ do
  let span1 = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
  let span2 = SourceSpan (SourcePos 2 1) (SourcePos 2 5)
  let merged = mergeSpans span1 span2
  assertEqual "Merged span should start at earliest" (SourcePos 1 1) (spanStart merged)
  assertEqual "Merged span should end at latest" (SourcePos 2 5) (spanEnd merged)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Mathematical Properties Tests"
  [ -- QuickCheck properties for SourcePos
    testProperty "startPos is constant" propStartPosConstant
  , testProperty "posAfter increases column" propPosAfterIncreasesColumn
  , testProperty "posAt creates correct position" propPosAtCreatesCorrectPosition
  , testProperty "advancePos handles newline" propAdvancePosHandlesNewline
  , testProperty "advancePos handles tab" propAdvancePosHandlesTab
  , testProperty "advancePosBy is consistent" propAdvancePosByIsConsistent
  
    -- QuickCheck properties for SourceSpan
  , testProperty "emptySpan is invalid" propEmptySpanIsInvalid
  , testProperty "spanFrom creates valid span" propSpanFromCreatesValidSpan
  , testProperty "spanTo creates correct span" propSpanToCreatesCorrectSpan
  , testProperty "spanBetween encompasses positions" propSpanBetweenEncompassesPositions
  , testProperty "mergeSpans is commutative" propMergeSpansCommutative
  , testProperty "mergeSpans is associative" propMergeSpansAssociative
  , testProperty "mergeSpans contains originals" propMergeSpansContainsOriginals
  
    -- QuickCheck properties for Located
  , testProperty "locatedAt uses spanFrom" propLocatedAtUsesSpanFrom
  , testProperty "locatedWithSpan preserves span" propLocatedWithSpanPreservesSpan
  
    -- Unit tests for edge cases
  , testPositionArithmeticEdgeCases
  , testSpanValidityEdgeCases
  , testMergeSpansEdgeCases
  ]