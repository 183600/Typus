module Test.Unit.NewSourceLocationMathQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), oneof, Gen, Property, (===), counterexample)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, advancePosBy, posAt, posAtLineCol)
import TestSupport.QuickCheck 
      in counterexample ("p1=" ++ show p1 ++ ",                               p2 =" ++ show p2 ++ ",                               c =" ++ show c) $
     if p1 <= p2 then p1' <= p2' else True  -- Only check when p1 <= p2

-- | posAfter '\n' should always increase the line number
prop_posAfterNewlineIncreasesLine :: SourcePos -> Property
prop_posAfterNewlineIncreasesLine                               pos =
  let pos' = posAfter '\n' pos
  in counterexample ("pos=" ++ show pos ++ ", pos'=" ++ show pos') $
     posLine pos' === posLine pos + 1

-- | posAfter '\t' should advance to the next tab stop (multiple of 8 + 1)
prop_posAfterTabAdvancesToTabStop :: SourcePos -> Property  
prop_posAfterTabAdvancesToTabStop                               pos =
  let pos' = posAfter '\t' pos
                                    expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in counterexample ("pos=" ++ show pos ++ ", pos'=" ++ show pos' ++ ",                               expected =" ++ show expectedCol) $
     posColumn pos' === expectedCol

-- | posAfter on regular characters should increase column by 1
prop_posAfterRegularCharIncreasesColumn :: SourcePos -> Char -> Property
prop_posAfterRegularCharIncreasesColumn pos c
  | c `elem` ['\n', '\t'] = property True  -- Skip special chars
  |                               otherwise =
      let pos' = posAfter c pos
      in counterexample ("pos=" ++ show pos ++ ",                               c =" ++ show c ++ ", pos'=" ++ show pos') $
         posColumn pos' === posColumn pos + 1

-- | posAt should create a valid position (positive line L.and column)
prop_posAtCreatesValidPosition :: Int -> Int -> Property
prop_posAtCreatesValidPosition line                               col =
  let pos = posAt (abs line + 1) (abs col + 1)  -- Ensure positive
  in counterexample ("line=" ++ show line ++ ",                               col =" ++ show col ++ ",                               pos =" ++ show pos) $
     posLine pos > 0 && posColumn pos > 0

-- | posAtLineCol should create consistent positions
prop_posAtLineColCreatesConsistentPosition :: Int -> Int -> Int -> Property
prop_posAtLineColCreatesConsistentPosition line col                               offset =
  let pos = posAtLineCol (abs line + 1) (abs col + 1) (abs offset)
  in counterexample ("pos=" ++ show pos) $
     posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

-- ============================================================================
-- SourceSpan Property Tests  
-- ============================================================================

-- | spanBetween should always create a valid span
prop_spanBetweenCreatesValidSpan :: SourcePos -> SourcePos -> Property
prop_spanBetweenCreatesValidSpan p1                               p2 =
  let span = spanBetween p1 p2
  in counterexample ("p1=" ++ show p1 ++ ",                               p2 =" ++ show p2 ++ ",                               span =" ++ show span) $
     isValidSpan span

-- | mergeSpans should be commutative: mergeSpans a                               b == mergeSpans b a
prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansCommutative span1                               span2 =
  let merged1 = mergeSpans span1 span2
                                    merged2 = mergeSpans span2 span1
  in counterexample ("span1=" ++ show span1 ++ ",                               span2 =" ++ show span2) $
                                   merged1 === merged2

-- | mergeSpans should be associative: mergeSpans (mergeSpans a b)                               c == mergeSpans a (mergeSpans b c)
prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpansAssociative span1 span2                               span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
                                    merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in counterexample ("span1=" ++ show span1 ++ ",                               span2 =" ++ show span2 ++ ",                               span3 =" ++ show span3) $
                                   merged1 === merged2

-- | mergeSpans should contain both original spans
prop_mergeSpansContainsBothSpans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansContainsBothSpans span1                               span2 =
  let merged = mergeSpans span1 span2
  in counterexample ("span1=" ++ show span1 ++ ",                               span2 =" ++ show span2 ++ ",                               merged =" ++ show merged) $
     spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2

-- | Empty span should be valid
prop_emptySpanIsValid :: SourcePos -> Property
prop_emptySpanIsValid                               pos =
  let span = spanBetween pos pos
  in counterexample ("pos=" ++ show pos ++ ",                               span =" ++ show span) $
     isValidSpan span

-- ============================================================================
-- Position Advancement Property Tests
-- ============================================================================

-- | advancePosBy should be consistent with repeated posAfter applications
prop_advancePosByConsistent :: String -> SourcePos -> Property
prop_advancePosByConsistent str                               pos =
  let pos1 = advancePosBy str pos
                                    pos2 = L.foldl (flip posAfter) pos str
  in counterexample ("str=" ++ show str ++ ",                               pos =" ++ show pos ++ ",                               pos1 =" ++ show pos1 ++ ",                               pos2 =" ++ show pos2) $
                                   pos1 === pos2

-- | advancePosBy with empty string should return the same position
prop_advancePosByEmptyString :: SourcePos -> Property
prop_advancePosByEmptyString                               pos =
  let pos' = advancePosBy "" pos
  in counterexample ("pos=" ++ show pos ++ ", pos'=" ++ show pos') $
     pos' === pos

-- | advancePosBy should preserve order: if p1 <= p2, then advancePosBy s p1 <= advancePosBy s p2
prop_advancePosByPreservesOrder :: SourcePos -> SourcePos -> String -> Property
prop_advancePosByPreservesOrder p1 p2                               str =
  let p1' = advancePosBy str p1
      p2' = advancePosBy str p2
  in counterexample ("p1=" ++ show p1 ++ ",                               p2 =" ++ show p2 ++ ",                               str =" ++ show str) $
     if p1 <= p2 then p1' <= p2' else True  -- Only check when p1 <= p2

-- ============================================================================
-- Mathematical Invariant Tests
-- ============================================================================

-- | Position comparison should be transitive
prop_positionComparisonTransitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_positionComparisonTransitive p1 p2                               p3 =
  let result = if p1 <= p2 && p2 <= p3 then p1 <= p3 else True
  in counterexample ("p1=" ++ show p1 ++ ",                               p2 =" ++ show p2 ++ ",                               p3 =" ++ show p3) $
     result

-- | Span validity should be preserved under merge
prop_spanValidityPreservedUnderMerge :: SourceSpan -> SourceSpan -> Property
prop_spanValidityPreservedUnderMerge span1                               span2 =
  let merged = mergeSpans span1 span2
  in counterexample ("span1=" ++ show span1 ++ ",                               span2 =" ++ show span2 ++ ",                               merged =" ++ show merged) $
     isValidSpan span1 && isValidSpan                               span2 ==> isValidSpan merged

-- | Span L.length should be non-negative
prop_spanLengthNonNegative :: SourceSpan -> Property  
prop_spanLengthNonNegative                               span =
  let L.length = posOffset (spanEnd span) - posOffset (spanStart span)
  in property $ counterexample ("span=" ++ show span ++ ", L.length=" ++ show L.length) $
     L.length >= 0

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
                                              arbitrary = do
              line <- arbitrary `suchThat` (> 0)
    column <- arbitrary `suchThat` (> 0)
    offset <- arbitrary `suchThat` (>= 0)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    end <- arbitrary
    -- Ensure we have a valid span by ordering the positions
    let startPos = min property $ start end
                                      endPos = max start end
    return $ SourceSpan startPos endPos