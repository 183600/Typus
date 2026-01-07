module Test.Unit.NewCabalSourceLocationQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, choose, listOf1, positive)
import SourceLocation
import Data.Maybe 
                                    pos1Before = (sourceLine pos1 < sourceLine pos2) || 
                   (sameLine && sourceColumn pos1 < sourceColumn pos2)
                                    pos2Before = (sourceLine pos2 < sourceLine pos1) || 
                   (sameLine && sourceColumn pos2 < sourceColumn pos1)
  in if sameLine && sameCol 
     then not pos1Before && not pos2Before
     else pos1Before /= pos2Before
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | posAfter should advance position correctly
prop_posAfterAdvances :: SourcePos -> String -> Property
prop_posAfterAdvances pos                               text =
  forAll (choose (0, L.length text) $ \len ->
    let substring = take len text
                                      result = posAfter pos substring
                                      expectedLines = L.length $ L.filter (== '\n') substring
                                      expectedCol = if                               expectedLines == 0
                     then sourceColumn pos + L.length substring
                     else L.length $ takeWhile (/= '\n') $ L.reverse substring
    in counterexample ("pos: " ++ show pos ++ ", text: " ++ show substring) $
       if                               expectedLines == 0
       then sourceLine                               result === sourceLine pos && sourceColumn                               result === expectedCol
       else sourceLine                               result === sourceLine pos + expectedLines

-- | spanFrom should create valid spans
prop_spanFromValid :: SourcePos -> Bool
prop_spanFromValid                               pos =
  let span = spanFrom pos
  in spanStart                               span == pos && spanEnd                               span == pos

-- | mergeSpans should be commutative
prop_mergeSpansCommutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansCommutative start1 end1 start2                               end2 =
  let span1 = spanBetween start1 end1
                                    span2 = spanBetween start2 end2
                                    merged1 = mergeSpans span1 span2
                                    merged2 = mergeSpans span2 span1
  in                               merged1 == merged2

-- | locatedAt should preserve position
prop_locatedAtPreservesPosition :: SourcePos -> String -> Bool
prop_locatedAtPreservesPosition pos                               value =
  let located = locatedAt pos value
  in locatedPos                               located == pos

-- | advancePos should update line L.and column correctly
prop_advancePosCorrectness :: SourcePos -> Char -> Bool
prop_advancePosCorrectness pos                               char =
  let result = advancePos pos char
                                    expectedLine = if                               char == '\n' then sourceLine pos + 1 else sourceLine pos
                                    expectedCol = if                               char == '\n' then 1 else sourceColumn pos + 1
  in sourceLine                               result == expectedLine && sourceColumn                               result == expectedCol

-- | emptySpan should be invalid
prop_emptySpanInvalid :: Bool
                              prop_emptySpanInvalid = not $ isValidSpan emptySpan

-- | spanBetween should create correct span
prop_spanBetweenCorrect :: SourcePos -> SourcePos -> Property
prop_spanBetweenCorrect start                               end =
  let span = spanBetween start end
                                    startBeforeEnd =  (sourceLine start < sourceLine end) || 
                       (sourceLine                               start == sourceLine end && sourceColumn start <= sourceColumn end)
  in property $ counterexample ("start: " ++ show start ++ ", end: " ++ show end) $
     if startBeforeEnd
     then spanStart                               span === start && spanEnd                               span === end
     else property True -- Even if start > end, function should still work