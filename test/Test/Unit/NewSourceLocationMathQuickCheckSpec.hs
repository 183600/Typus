{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationMathQuickCheckSpec where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import SourceLocation

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid source positions with reasonable constraints
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> 
    choose (1, 1000) <*>
    choose (1, 1000) <*>
    choose (0, 100000)

instance Arbitrary SourcePos where
    arbitrary = genSourcePos

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
    start <- genSourcePos
    endOffset <- choose (0, 1000)
    let end = advancePosBy (replicate endOffset 'a') start
    return $ spanBetween start end

instance Arbitrary SourceSpan where
    arbitrary = genSourceSpan

-- Generate characters for position advancement testing
genChar :: Gen Char
genChar = elements ['\n', '\t', ' ', 'a', 'z', '0', '9', ';', '{', '}', '(', ')']

instance Arbitrary Char where
    arbitrary = genChar

-- ============================================================================
-- Source Position Mathematical Properties
-- ============================================================================

-- Property: Advancing position by newline increments line number and resets column
prop_advanceNewlineIncrementsLine :: SourcePos -> Property
prop_advanceNewlineIncrementsLine pos =
    let newPos = posAfter '\n' pos
    in counterexample ("Line should increment, column should reset to 1")
       (posLine newPos === posLine pos + 1 && posColumn newPos === 1)

-- Property: Advancing position by tab aligns to next tab stop (8-character boundaries)
prop_advanceTabAlignsToTabStop :: SourcePos -> Property
prop_advanceTabAlignsToTabStop pos =
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in counterexample ("Tab should align to next 8-character boundary")
       (posColumn newPos === expectedCol)

-- Property: Advancing position by regular character increments column only
prop_advanceRegularCharIncrementsColumn :: SourcePos -> Char -> Property
prop_advanceRegularCharIncrementsColumn pos char
    | char `elem` ['\n', '\t'] = property True  -- Skip special chars
    | otherwise = 
        let newPos = posAfter char pos
        in counterexample ("Regular character should increment column by 1")
           (posLine newPos === posLine pos && posColumn newPos === posColumn pos + 1)

-- Property: Position offset always increases when advancing by any character
prop_offsetAlwaysIncreases :: SourcePos -> Char -> Property
prop_offsetAlwaysIncreases pos char =
    let newPos = posAfter char pos
    in counterexample ("Offset should always increase")
       (posOffset newPos > posOffset pos === True)

-- Property: Advancing by multiple characters is equivalent to sequential advancement
prop_advanceByMultipleChars :: SourcePos -> String -> Property
prop_advanceByMultipleChars pos chars =
    let newPos1 = advancePosBy chars pos
        newPos2 = foldl (flip posAfter) pos chars
    in counterexample ("Batch advancement should equal sequential advancement")
       (newPos1 === newPos2)

-- Property: Position comparison is consistent with offset comparison
prop_positionComparisonConsistentWithOffset :: SourcePos -> SourcePos -> Property
prop_positionComparisonConsistentWithOffset pos1 pos2 =
    let posComparison = compare pos1 pos2
        offsetComparison = compare (posOffset pos1) (posOffset pos2)
    in counterexample ("Position comparison should match offset comparison")
       (posComparison === offsetComparison)

-- ============================================================================
-- Source Span Mathematical Properties
-- ============================================================================

-- Property: Empty span has same start and end position
prop_emptySpanHasSameStartEnd :: SourcePos -> Property
prop_emptySpanHasSameStartEnd pos =
    let span = emptySpan pos
    in counterexample ("Empty span should have same start and end")
       (spanStart span === spanEnd span)

-- Property: Span between positions maintains correct order
prop_spanBetweenMaintainsOrder :: SourcePos -> SourcePos -> Property
prop_spanBetweenMaintainsOrder pos1 pos2 =
    let span = spanBetween pos1 pos2
        minPos = if pos1 <= pos2 then pos1 else pos2
        maxPos = if pos1 >= pos2 then pos1 else pos2
    in counterexample ("Span should maintain correct order")
       (spanStart span === minPos && spanEnd span === maxPos)

-- Property: Merging spans creates span that covers both original spans
prop_mergeSpansCoversBoth :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansCoversBoth span1 span2 =
    let merged = mergeSpans span1 span2
        start1 = spanStart span1
        end1 = spanEnd span1
        start2 = spanStart span2
        end2 = spanEnd span2
        expectedStart = min start1 start2
        expectedEnd = max end1 end2
    in counterexample ("Merged span should cover both original spans")
       (spanStart merged === expectedStart && spanEnd merged === expectedEnd)

-- Property: Span validity check works correctly
prop_spanValidityCheck :: SourcePos -> SourcePos -> Property
prop_spanValidityCheck pos1 pos2 =
    let span = spanBetween pos1 pos2
        isValid = isValidSpan span
        shouldBeValid = spanStart span <= spanEnd span
    in counterexample ("Span validity should be consistent")
       (isValid === shouldBeValid)

-- ============================================================================
-- Text Advancement Properties
-- ============================================================================

-- Property: Advancing by text is equivalent to advancing by unpacked string
prop_advanceByTextEquivalence :: SourcePos -> String -> Property
prop_advanceByTextEquivalence pos str =
    let text = T.pack str
        posByText = advancePosByText text pos
        posByString = advancePosBy str pos
    in counterexample ("Text advancement should equal string advancement")
       (posByText === posByString)

-- Property: Advancing by empty text/string doesn't change position
prop_advanceByEmptyNoChange :: SourcePos -> Property
prop_advanceByEmptyNoChange pos =
    let posByText = advancePosByText T.empty pos
        posByString = advancePosBy "" pos
    in counterexample ("Advancing by empty should not change position")
       (posByText === pos && posByString === pos)

-- Property: Line advancement only changes line number and resets column
prop_advanceLineOnlyChangesLine :: SourcePos -> Int -> Property
prop_advanceLineOnlyChangesLine pos numLines
    | numLines <= 0 = property True
    | otherwise =
        let newPos = advancePosByLine numLines pos
        in counterexample ("Line advancement should only change line and reset column")
           (posLine newPos === posLine pos + numLines && posColumn newPos === 1)

-- ============================================================================
-- Mathematical Operations Properties
-- ============================================================================

-- Property: Position distance is symmetric
prop_positionDistanceSymmetric :: SourcePos -> SourcePos -> Property
prop_positionDistanceSymmetric pos1 pos2 =
    let dist1 = abs (posOffset pos1 - posOffset pos2)
        dist2 = abs (posOffset pos2 - posOffset pos1)
    in counterexample ("Position distance should be symmetric")
       (dist1 === dist2)

-- Property: Line distance is non-negative
prop_lineDistanceNonNegative :: SourcePos -> SourcePos -> Property
prop_lineDistanceNonNegative pos1 pos2 =
    let lineDist = abs (posLine pos1 - posLine pos2)
    in counterexample ("Line distance should be non-negative")
       (lineDist >= 0 === True)

-- Property: Creating position at specific coordinates matches expected values
prop_positionAtCoordinates :: Int -> Int -> Property
prop_positionAtCoordinates line col
    | line <= 0 || col <= 0 = property True
    | otherwise =
        let pos = posAt line col
        in counterexample ("Position at coordinates should match")
           (posLine pos === line && posColumn pos === col)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Source Location Math QuickCheck Tests"
    [ testProperty "Advance newline increments line and resets column" prop_advanceNewlineIncrementsLine
    , testProperty "Advance tab aligns to tab stop" prop_advanceTabAlignsToTabStop
    , testProperty "Advance regular char increments column" prop_advanceRegularCharIncrementsColumn
    , testProperty "Offset always increases when advancing" prop_offsetAlwaysIncreases
    , testProperty "Advance by multiple chars equals sequential advancement" prop_advanceByMultipleChars
    , testProperty "Position comparison consistent with offset comparison" prop_positionComparisonConsistentWithOffset
    , testProperty "Empty span has same start and end" prop_emptySpanHasSameStartEnd
    , testProperty "Span between maintains correct order" prop_spanBetweenMaintainsOrder
    , testProperty "Merge spans covers both original spans" prop_mergeSpansCoversBoth
    , testProperty "Span validity check works correctly" prop_spanValidityCheck
    , testProperty "Advance by text equals advance by string" prop_advanceByTextEquivalence
    , testProperty "Advance by empty doesn't change position" prop_advanceByEmptyNoChange
    , testProperty "Line advancement only changes line and resets column" prop_advanceLineOnlyChangesLine
    , testProperty "Position distance is symmetric" prop_positionDistanceSymmetric
    , testProperty "Line distance is non-negative" prop_lineDistanceNonNegative
    , testProperty "Position at coordinates matches expected values" prop_positionAtCoordinates
    ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Import required for choose and elements
import Test.QuickCheck (choose, elements, property)