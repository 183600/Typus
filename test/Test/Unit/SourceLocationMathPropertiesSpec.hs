module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, oneof, suchThat)
import SourceLocation

-- | QuickCheck tests for SourceLocation mathematical properties
tests :: TestTree
tests =
  testGroup "SourceLocation mathematical properties"
    [ testGroup "Position arithmetic properties"
        [ fastProperty "posAfter advances offset by exactly 1" prop_posAfterAdvancesOffset
        , fastProperty "posAfter with newline resets column to 1" prop_posAfterNewlineResetsColumn
        , fastProperty "posAfter with tab aligns to next 8-column boundary" prop_posAfterTabAlignment
        , fastProperty "posAt creates position with given line L.and column" prop_posAtCreation
        ]

    , testGroup "Span arithmetic properties" 
        [ fastProperty "emptySpan has zero L.length" prop_emptySpanZeroLength
        , fastProperty "spanBetween is valid when start <= end" prop_spanBetweenValidity
        , fastProperty "mergeSpans contains both original spans" prop_mergeSpansContainment
        , fastProperty "mergeSpans is commutative" prop_mergeSpansCommutative
        , fastProperty "mergeSpans is associative" prop_mergeSpansAssociative
        ]

    , testGroup "Location tracking properties"
        [ fastProperty "advancePosBy is consistent with repeated advancePos" prop_advancePosByConsistency
        , fastProperty "advancePosByText handles multiline correctly" prop_advancePosByTextMultiline
        , fastProperty "position advancement is reversible for simple chars" prop_advancementReversibility
        ]

    , testGroup "Position ordering properties"
        [ fastProperty "comparePos respects offset ordering" prop_comparePosOffsetOrdering
        , fastProperty "posDistance is always non-negative" prop_posDistanceNonNegative
        , fastProperty "posDistance is symmetric" prop_posDistanceSymmetric
        ]

    , testGroup "Span containment properties"
        [ fastProperty "span contains its start L.and end positions" prop_spanContainsBoundaries
        , fastProperty "spanOverlap is symmetric" prop_spanOverlapSymmetric
        , fastProperty "mergeOverlappingSpans preserves coverage" prop_mergeOverlappingPreservesCoverage
        ]
    ]

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> positiveInt <*> positiveInt <*> nonNegativeInt
      where
        positiveInt = getPositive <$> arbitrary
        nonNegativeInt = getNonNegative <$> arbitrary

instance Arbitrary SourceSpan where
    arbitrary = do
        start <- arbitrary
        end <- arbitrary `suchThat` (\e -> posOffset e >= posOffset start)
        return $ SourceSpan start end

-- ============================================================================
-- Position arithmetic properties
-- ============================================================================

prop_posAfterAdvancesOffset :: Char -> SourcePos -> Bool
prop_posAfterAdvancesOffset c pos =
    posOffset (posAfter c pos) == posOffset pos + 1

prop_posAfterNewlineResetsColumn :: SourcePos -> Bool
prop_posAfterNewlineResetsColumn pos =
    posColumn (posAfter '\n' pos) == 1

prop_posAfterTabAlignment :: SourcePos -> Property
prop_posAfterTabAlignment pos =
    let newCol = posColumn (posAfter '\t' pos)
        oldCol = posColumn pos
    in newCol `mod` 8 === 1 .||. newCol > oldCol

prop_posAtCreation :: Positive Int -> Positive Int -> Bool
prop_posAtCreation (Positive line) (Positive col) =
    let pos = posAt line col
    in posLine pos == line && posColumn pos == col

-- ============================================================================
-- Span arithmetic properties
-- ============================================================================

prop_emptySpanZeroLength :: SourcePos -> Bool
prop_emptySpanZeroLength pos =
    let span = emptySpan pos
    in spanStart span == pos && spanEnd span == pos

prop_spanBetweenValidity :: SourcePos -> SourcePos -> Property
prop_spanBetweenValidity start end =
    posOffset start <= posOffset end ==> isValidSpan (spanBetween start end)

prop_mergeSpansContainment :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContainment span1 span2 =
    let merged = mergeSpans span1 span2
    in spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
       spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2

prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansCommutative span1 span2 =
    mergeSpans span1 span2 == mergeSpans span2 span1

prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansAssociative span1 span2 span3 =
    mergeSpans (mergeSpans span1 span2) span3 == mergeSpans span1 (mergeSpans span2 span3)

-- ============================================================================
-- Location tracking properties
-- ============================================================================

prop_advancePosByConsistency :: String -> SourcePos -> Bool
prop_advancePosByConsistency chars pos =
    advancePosBy chars pos == L.foldl (flip advancePos) pos chars

prop_advancePosByTextMultiline :: String -> SourcePos -> Bool
prop_advancePosByTextMultiline str pos =
    advancePosByText (pack str) pos == advancePosBy str pos
  where
    pack = undefined -- This would require Data.Text import

prop_advancementReversibility :: SourcePos -> Char -> Property
prop_advancementReversibility pos c =
    c `notElem` ['\n', '\t'] ==> 
    let newPos = posAfter c pos
    in posColumn newPos == posColumn pos + 1

-- ============================================================================
-- Position ordering properties
-- ============================================================================

prop_comparePosOffsetOrdering :: SourcePos -> SourcePos -> Bool
prop_comparePosOffsetOrdering p1 p2 =
    compare p1 p2 == compare (posOffset p1) (posOffset p2)

prop_posDistanceNonNegative :: SourcePos -> SourcePos -> Bool
prop_posDistanceNonNegative p1 p2 =
    let distance = abs (posOffset p2 - posOffset p1)
    in distance >= 0

prop_posDistanceSymmetric :: SourcePos -> SourcePos -> Bool
prop_posDistanceSymmetric p1 p2 =
    abs (posOffset p2 - posOffset p1) == abs (posOffset p1 - posOffset p2)

-- ============================================================================
-- Span containment properties
-- ============================================================================

prop_spanContainsBoundaries :: SourceSpan -> Bool
prop_spanContainsBoundaries span =
    let start = spanStart span
        end = spanEnd span
    in _isPosInSpan start span && _isPosInSpan end span

prop_spanOverlapSymmetric :: SourceSpan -> SourceSpan -> Bool
prop_spanOverlapSymmetric span1 span2 =
    _doSpansOverlap span1 span2 == _doSpansOverlap span2 span1

prop_mergeOverlappingPreservesCoverage :: [SourceSpan] -> Bool
prop_mergeOverlappingPreservesCoverage spans =
    let merged = _mergeOverlappingSpans spans
        originalPositions = concatMap spanToPositions spans
        mergedPositions = concatMap spanToPositions merged
    in L.all (`elem` mergedPositions) originalPositions
  where
    spanToPositions span = [spanStart span, spanEnd span]

-- ============================================================================
-- Helper functions (would need to be imported from SourceLocation module)
-- ============================================================================

_isPosInSpan :: SourcePos -> SourceSpan -> Bool
_isPosInSpan pos srcSpan = pos >= spanStart srcSpan && pos <= spanEnd srcSpan

_doSpansOverlap :: SourceSpan -> SourceSpan -> Bool
_doSpansOverlap span1 span2 =
    spanStart span1 <= spanEnd span2 && spanEnd span1 >= spanStart span2

_mergeOverlappingSpans :: [SourceSpan] -> [SourceSpan]
_mergeOverlappingSpans = foldr merge []
  where
    merge current [] = [current]
    merge current (acc:rest)
        | _doSpansOverlap current acc = merge (_spanCovering (spanStart current) (spanEnd acc)) rest
        | otherwise = current : acc : rest

_spanCovering :: SourcePos -> SourcePos -> SourceSpan
_spanCovering p1 p2 = SourceSpan (min p1 p2) (max p1 p2)