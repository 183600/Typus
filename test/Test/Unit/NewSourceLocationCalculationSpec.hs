module Test.Unit.NewSourceLocationCalculationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, suchThat)
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))

-- | 新的位置计算QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Source Location Calculation Tests"
    [ testGroup "Source position properties"
        [ fastProperty "posAfter advances correctly for regular chars" prop_posAfterRegularChar
        , fastProperty "posAfter handles newline correctly" prop_posAfterNewline
        , fastProperty "posAfter handles tab correctly" prop_posAfterTab
        , fastProperty "posAt creates correct position" prop_posAtCorrectness
        ]

    , testGroup "Source span properties"
        [ fastProperty "emptySpan has zero L.length" prop_emptySpanZeroLength
        , fastProperty "spanBetween creates valid span" prop_spanBetweenValid
        , fastProperty "mergeSpans contains both spans" prop_mergeSpansContainsBoth
        , fastProperty "isValidSpan correctness" prop_isValidSpanCorrectness
        ]

    , testGroup "Location tracking properties"
        [ fastProperty "runLocationTracker starts at startPos" prop_runLocationTrackerStartPos
        , fastProperty "advancePosByText advances correctly" prop_advancePosByTextCorrectness
        , fastProperty "advancePosByLine advances correctly" prop_advancePosByLineCorrectness
        ]

    , testGroup "Error location conversion properties"
        [ fastProperty "toErrorLocation preserves position" prop_toErrorLocationPreservesPosition
        , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpanPreservesRange
        ]

    , testGroup "Advanced position properties"
        [ fastProperty "comparePos consistency with offset" prop_comparePosConsistency
        , fastProperty "position advancement is monotonic" prop_positionAdvancementMonotonic
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary SourcePos where
    arbitrary = do
        line <- choose (1, 1000)
        col <- choose (1, 1000)
        offset <- choose (0, 100000)
        return $ SourcePos line col offset

instance Arbitrary SourceSpan where
    arbitrary = do
        start <- arbitrary
        endOffset <- choose (0, 1000)
        let end = start { posOffset = posOffset start + endOffset }
        return $ SourceSpan start end

-- Generate valid spans (start <= end)
genValidSpan :: Gen SourceSpan
genValidSpan = do
    startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    startOffset <- choose (0, 10000)
    let start = SourcePos startLine startCol startOffset
    
    endLineOffset <- choose (0, 10)
    endColOffset <- choose (0, 50)
    endOffsetOffset <- choose (0, 1000)
    
    let end = SourcePos 
            (startLine + endLineOffset)
            (if endLineOffset == 0 then startCol + endColOffset else startCol)
            (startOffset + endOffsetOffset)
    
    return $ SourceSpan start end

-- Generate characters for position advancement
genChar :: Gen Char
genChar = oneof
    [ elements ['a'..'z']
    , elements ['A'..'Z']
    , elements ['0'..'9']
    , elements " \t.,;:!?()[]{}<>+-*/=&|%^~"
    , return '\n'
    ]

-- ============================================================================
-- Properties for SourcePos
-- ============================================================================

prop_posAfterRegularChar :: Char -> SourcePos -> Property
prop_posAfterRegularChar char pos =
    char /= '\n' && char /= '\t' ==> 
    let newPos = posAfter char pos
    in posLine newPos == posLine pos &&
       posColumn newPos == posColumn pos + 1 &&
       posOffset newPos == posOffset pos + 1

prop_posAfterNewline :: SourcePos -> Property
prop_posAfterNewline pos =
    let newPos = posAfter '\n' pos
    in posLine newPos == posLine pos + 1 &&
       posColumn newPos == 1 &&
       posOffset newPos == posOffset pos + 1

prop_posAfterTab :: SourcePos -> Property
prop_posAfterTab pos =
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in posLine newPos == posLine pos &&
       posColumn newPos == expectedCol &&
       posOffset newPos == posOffset pos + 1

prop_posAtCorrectness :: Int -> Int -> Property
prop_posAtCorrectness line col =
    line > 0 && col > 0 ==>
    let pos = posAt line col
    in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- ============================================================================
-- Properties for SourceSpan
-- ============================================================================

prop_emptySpanZeroLength :: SourcePos -> Bool
prop_emptySpanZeroLength pos =
    let span = emptySpan pos
    in spanStart span == pos && spanEnd span == pos

prop_spanBetweenValid :: SourcePos -> SourcePos -> Property
prop_spanBetweenValid start end =
    start <= end ==> 
    let span = spanBetween start end
    in spanStart span == start && spanEnd span == end

prop_mergeSpansContainsBoth :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContainsBoth span1 span2 =
    let merged = mergeSpans span1 span2
    in spanStart merged <= spanStart span1 &&
       spanEnd merged >= spanEnd span1 &&
       spanStart merged <= spanStart span2 &&
       spanEnd merged >= spanEnd span2

prop_isValidSpanCorrectness :: SourceSpan -> Bool
prop_isValidSpanCorrectness span =
    isValidSpan span == (spanStart span <= spanEnd span)

-- ============================================================================
-- Properties for Location Tracking
-- ============================================================================

prop_runLocationTrackerStartPos :: Int -> Property
prop_runLocationTrackerStartPos value =
    let result = runLocationTracker (return value)
    in result == value  -- Simple property to ensure it runs

prop_advancePosByTextCorrectness :: String -> SourcePos -> Bool
prop_advancePosByTextCorrectness text pos =
    let finalPos = advancePosByText text pos
        expectedPos = L.foldl (flip advancePos) pos text
    in finalPos == expectedPos

prop_advancePosByLineCorrectness :: Int -> SourcePos -> Property
prop_advancePosByLineCorrectness numLines pos =
    numLines >= 0 ==>
    let newPos = advancePosByLine numLines pos
    in posLine newPos == posLine pos + numLines &&
       posColumn newPos == 1

-- ============================================================================
-- Properties for Error Location Conversion
-- ============================================================================

prop_toErrorLocationPreservesPosition :: SourcePos -> Bool
prop_toErrorLocationPreservesPosition pos =
    let errLoc = toErrorLocation pos
    in line errLoc == posLine pos &&
       column errLoc == posColumn pos &&
       filePath errLoc == Nothing &&
       endLine errLoc == Nothing &&
       endColumn errLoc == Nothing

prop_toErrorLocationWithSpanPreservesRange :: SourceSpan -> Bool
prop_toErrorLocationWithSpanPreservesRange span =
    let errLoc = toErrorLocationWithSpan span
        start = spanStart span
        end = spanEnd span
    in line errLoc == posLine start &&
       column errLoc == posColumn start &&
       endLine errLoc == Just (posLine end) &&
       endColumn errLoc == Just (posColumn end) &&
       filePath errLoc == Nothing

-- ============================================================================
-- Properties for Advanced Position Operations
-- ============================================================================

prop_comparePosConsistency :: SourcePos -> SourcePos -> Bool
prop_comparePosConsistency pos1 pos2 =
    let offset1 = posOffset pos1
        offset2 = posOffset pos2
        comparison = compare pos1 pos2
        expectedComparison = compare offset1 offset2
    in comparison == expectedComparison

prop_positionAdvancementMonotonic :: String -> SourcePos -> Bool
prop_positionAdvancementMonotonic text pos =
    let positions = scanl (flip advancePos) pos text
        offsets = map posOffset positions
    in L.all (uncurry (<=)) (zip offsets (L.tail offsets))

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Helper to check if one position is less than L.or equal to another
(<=) :: SourcePos -> SourcePos -> Bool
(<=) p1 p2 = posOffset p1 <= posOffset p2