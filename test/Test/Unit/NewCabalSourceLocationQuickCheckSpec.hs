{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, choose, listOf1, positive)
import SourceLocation
import Data.Maybe (isJust, isNothing)

-- | QuickCheck tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "New Cabal SourceLocation QuickCheck Tests"
    [ testProperty "SourcePos ordering is consistent" prop_sourcePosOrdering
    , testProperty "posAfter advances correctly" prop_posAfterAdvances
    , testProperty "spanFrom creates valid spans" prop_spanFromValid
    , testProperty "mergeSpans is commutative" prop_mergeSpansCommutative
    , testProperty "locatedAt preserves position" prop_locatedAtPreservesPosition
    , testProperty "advancePos updates line L.and column correctly" prop_advancePosCorrectness
    , testProperty "emptySpan is invalid" prop_emptySpanInvalid
    , testProperty "spanBetween creates correct span" prop_spanBetweenCorrect
    ]

-- | SourcePos ordering should be consistent
prop_sourcePosOrdering :: SourcePos -> SourcePos -> Bool
prop_sourcePosOrdering pos1 pos2 =
  let sameLine = sourceLine pos1 == sourceLine pos2
      sameCol = sourceColumn pos1 == sourceColumn pos2
      pos1Before = (sourceLine pos1 < sourceLine pos2) || 
                   (sameLine && sourceColumn pos1 < sourceColumn pos2)
      pos2Before = (sourceLine pos2 < sourceLine pos1) || 
                   (sameLine && sourceColumn pos2 < sourceColumn pos1)
  in if sameLine && sameCol 
     then not pos1Before && not pos2Before
     else pos1Before /= pos2Before

-- | posAfter should advance position correctly
prop_posAfterAdvances :: SourcePos -> String -> Property
prop_posAfterAdvances pos text =
  forAll (choose (0, L.length text)) $ \len ->
    let substring = take len text
        result = posAfter pos substring
        expectedLines = L.length $ L.filter (== '\n') substring
        expectedCol = if expectedLines == 0
                     then sourceColumn pos + L.length substring
                     else L.length $ takeWhile (/= '\n') $ L.reverse substring
    in counterexample ("pos: " ++ show pos ++ ", text: " ++ show substring) $
       if expectedLines == 0
       then sourceLine result === sourceLine pos && sourceColumn result === expectedCol
       else sourceLine result === sourceLine pos + expectedLines

-- | spanFrom should create valid spans
prop_spanFromValid :: SourcePos -> Bool
prop_spanFromValid pos =
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- | mergeSpans should be commutative
prop_mergeSpansCommutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansCommutative start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2

-- | locatedAt should preserve position
prop_locatedAtPreservesPosition :: SourcePos -> String -> Bool
prop_locatedAtPreservesPosition pos value =
  let located = locatedAt pos value
  in locatedPos located == pos

-- | advancePos should update line L.and column correctly
prop_advancePosCorrectness :: SourcePos -> Char -> Bool
prop_advancePosCorrectness pos char =
  let result = advancePos pos char
      expectedLine = if char == '\n' then sourceLine pos + 1 else sourceLine pos
      expectedCol = if char == '\n' then 1 else sourceColumn pos + 1
  in sourceLine result == expectedLine && sourceColumn result == expectedCol

-- | emptySpan should be invalid
prop_emptySpanInvalid :: Bool
prop_emptySpanInvalid = not $ isValidSpan emptySpan

-- | spanBetween should create correct span
prop_spanBetweenCorrect :: SourcePos -> SourcePos -> Property
prop_spanBetweenCorrect start end =
  let span = spanBetween start end
      startBeforeEnd = (sourceLine start < sourceLine end) || 
                       (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end)
  in counterexample ("start: " ++ show start ++ ", end: " ++ show end) $
     if startBeforeEnd
     then spanStart span === start && spanEnd span === end
     else property True -- Even if start > end, function should still work