{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationCoreFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Test suite for SourceLocation core functions
tests :: TestTree
tests = testGroup "SourceLocation Core Functions"
  [ testProperty "position comparison works correctly" propPositionComparison
  , testProperty "span contains position correctly" propSpanContainsPosition
  , testProperty "span intersection works correctly" propSpanIntersection
  , testProperty "source location ordering is transitive" propSourceLocationTransitive
  , testCase "position creation edge cases" testPositionCreation
  , testCase "span creation edge cases" testSpanCreation
  , testCase "source location merging" testSourceLocationMerging
  , testCase "position arithmetic" testPositionArithmetic
  , testCase "span boundaries" testSpanBoundaries
  , testCase "source location formatting" testSourceLocationFormatting
  ]

-- | Property: position comparison is consistent
propPositionComparison :: SourcePos -> SourcePos -> Property
propPositionComparison p1 p2 =
  let cmp = compare p1 p2
  in property $ (cmp == EQ) == (posLine p1 == posLine p2 && posColumn p1 == posColumn p2 && posOffset p1 == posOffset p2)

-- | Property: span contains position correctly
propSpanContainsPosition :: SourceSpan -> SourcePos -> Property
propSpanContainsPosition span pos =
  let contains = _isPosInSpan pos span
      start = spanStart span
      end = spanEnd span
      shouldBeContained = pos >= start && pos <= end
  in property $ contains == shouldBeContained

-- | Property: span intersection works correctly
propSpanIntersection :: SourceSpan -> SourceSpan -> Property
propSpanIntersection span1 span2 =
  let hasOverlap = _doSpansOverlap span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      shouldBeOverlap = start1 <= end2 && end1 >= start2
  in property $ hasOverlap == shouldBeOverlap

-- | Property: source location ordering is transitive
propSourceLocationTransitive :: SourcePos -> SourcePos -> SourcePos -> Property
propSourceLocationTransitive pos1 pos2 pos3 =
  let cmp1 = comparePos pos1 pos2
      cmp2 = comparePos pos2 pos3
      cmp3 = comparePos pos1 pos3
  in property $ (cmp1 == EQ && cmp2 == EQ) ==> (cmp3 == EQ)

-- | Unit tests for position creation edge cases
testPositionCreation :: IO ()
testPositionCreation = do
  let pos1 = SourcePos { posLine = 1, posColumn = 1, posOffset = 0 }
      pos2 = SourcePos { posLine = 0, posColumn = 0, posOffset = 0 }
  assertEqual "position 1,1" (SourcePos 1 1 0) pos1
  assertEqual "position 0,0" (SourcePos 0 0 0) pos2
  assertBool "position equality" $ pos1 == SourcePos 1 1 0
  assertBool "position inequality" $ pos1 /= pos2

-- | Unit tests for span creation edge cases
testSpanCreation :: IO ()
testSpanCreation = do
  let start = SourcePos 1 1 0
      end = SourcePos 2 10 20
      span1 = SourceSpan { spanStart = start, spanEnd = end }
  assertEqual "span creation" (SourceSpan start end) span1
  assertBool "span start correctness" $ spanStart span1 == start
  assertBool "span end correctness" $ spanEnd span1 == end

-- | Unit tests for source location merging
testSourceLocationMerging :: IO ()
testSourceLocationMerging = do
  let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
      span2 = SourceSpan (SourcePos 2 1 10) (SourcePos 2 10 19)
      merged = mergeSpans span1 span2
  assertEqual "merged start" (SourcePos 1 1 0) $ spanStart merged
  assertEqual "merged end" (SourcePos 2 10 19) $ spanEnd merged

-- | Unit tests for position arithmetic
testPositionArithmetic :: IO ()
testPositionArithmetic = do
  let pos1 = SourcePos 1 5 4
      pos2 = SourcePos 1 10 9
      pos3 = SourcePos 2 1 10
  assertBool "column comparison" $ comparePos pos1 pos2 == LT
  assertBool "line comparison" $ comparePos pos2 pos3 == LT
  assertEqual "position difference" 5 $ posOffset pos2 - posOffset pos1

-- | Unit tests for span boundaries
testSpanBoundaries :: IO ()
testSpanBoundaries = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
      inside = SourcePos 1 3 2
      outside = SourcePos 1 6 5
  assertBool "span contains inside position" $ _isPosInSpan inside span
  assertBool "span doesn't contain outside position" $ not $ _isPosInSpan outside span
  assertBool "span contains start position" $ _isPosInSpan (spanStart span) span
  assertBool "span contains end position" $ _isPosInSpan (spanEnd span) span

-- | Unit tests for source location formatting
testSourceLocationFormatting :: IO ()
testSourceLocationFormatting = do
  let pos = SourcePos 5 10 45
      formatted = showPos pos
  assertEqual "format includes line" True $ "5" `L.isInfixOf` formatted
  assertEqual "format includes column" True $ "10" `L.isInfixOf` formatted

-- Helper imports
import qualified Data.List as L

-- Helper function for property testing
property :: Bool -> Property
property = property' where
  property' :: Bool -> Property
  property' = id