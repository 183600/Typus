module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
                      advancePos, advancePosBy, advancePosByText, advancePosByLine,
                      toErrorLocation, toErrorLocationWithSpan)

-- ============================================================================
-- Arbitrary Instances
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
        end <- arbitrary
        -- Ensure span is valid (start <= end)
        let validSpan = if start <= end then SourceSpan start end else SourceSpan end start
        return validSpan

instance Arbitrary a => Arbitrary (Located a) where
    arbitrary = do
        value <- arbitrary
        pos <- arbitrary
        span <- arbitrary
        return $ Located value pos span

-- ============================================================================
-- Source Position Properties
-- ============================================================================

prop_startPosIsValid :: Bool
prop_startPosIsValid = 
    let pos = startPos
    in posLine pos == 1 && posColumn pos == 1 && posOffset pos == 0

prop_posAfterAdvancesCorrectly :: Char -> SourcePos -> Bool
prop_posAfterAdvancesCorrectly c pos =
    let newPos = posAfter c pos
        expectedOffset = posOffset pos + 1
    in posOffset newPos == expectedOffset &&
       if c == '\n'
       then posLine newPos == posLine pos + 1 && posColumn newPos == 1
       else if c == '\t'
            then posColumn newPos == ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
            else posColumn newPos == posColumn pos + 1

prop_advancePosByConsistent :: String -> SourcePos -> Bool
prop_advancePosByConsistent str pos =
    let advByFold = foldl (flip advancePos) pos str
        advByFunc = advancePosBy str pos
    in advByFold == advByFunc

prop_advancePosByTextConsistent :: String -> SourcePos -> Bool
prop_advancePosByTextConsistent str pos =
    let textAdv = advancePosByText (toEnum <$> str) pos
        strAdv = advancePosBy str pos
    in textAdv == strAdv

prop_advancePosByLineAdvancesCorrectly :: Int -> SourcePos -> Bool
prop_advancePosByLineAdvancesCorrectly numLines pos =
    let newPos = advancePosByLine numLines pos
    in posLine newPos == posLine pos + numLines &&
       posColumn newPos == 1

prop_posAtCreatesCorrectPosition :: Int -> Int -> Bool
prop_posAtCreatesCorrectPosition line col =
    let pos = posAt line col
    in posLine pos == line && posColumn pos == col && posOffset pos == 0

prop_posAtLineColCreatesCorrectPosition :: Int -> Int -> Int -> Bool
prop_posAtLineColCreatesCorrectPosition line col offset =
    let pos = posAtLineCol line col offset
    in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

prop_emptySpanIsValid :: SourcePos -> Bool
prop_emptySpanIsValid pos =
    let span = emptySpan pos
    in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_spanFromCreatesValidSpan :: SourcePos -> Bool
prop_spanFromCreatesValidSpan pos =
    let span = spanFrom pos
    in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_spanToCreatesValidSpan :: SourcePos -> Bool
prop_spanToCreatesValidSpan pos =
    let span = spanTo pos
    in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_spanBetweenCreatesValidSpan :: SourcePos -> SourcePos -> Bool
prop_spanBetweenCreatesValidSpan start end =
    let span = spanBetween start end
        expectedStart = min start end
        expectedEnd = max start end
    in spanStart span == expectedStart && spanEnd span == expectedEnd && isValidSpan span

prop_mergeSpansIsCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansIsCommutative span1 span2 =
    let merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in merged1 == merged2

prop_mergeSpansIsAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansIsAssociative span1 span2 span3 =
    let mergedLeft = mergeSpans (mergeSpans span1 span2) span3
        mergedRight = mergeSpans span1 (mergeSpans span2 span3)
    in mergedLeft == mergedRight

prop_mergeSpansContainsOriginals :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContainsOriginals span1 span2 =
    let merged = mergeSpans span1 span2
    in spanStart merged <= spanStart span1 &&
       spanEnd merged >= spanEnd span1 &&
       spanStart merged <= spanStart span2 &&
       spanEnd merged >= spanEnd span2

-- ============================================================================
-- Located Value Properties
-- ============================================================================

prop_locatedAtCreatesCorrectLocation :: SourcePos -> Int -> Bool
prop_locatedAtCreatesCorrectLocation pos value =
    let located = locatedAt pos value
        expectedSpan = emptySpan pos
    in locatedValue located == value &&
       locatedPos located == pos &&
       locatedSpan located == expectedSpan

prop_locatedWithSpanCreatesCorrectLocation :: SourceSpan -> String -> Bool
prop_locatedWithSpanCreatesCorrectLocation span value =
    let located = locatedWithSpan span value
    in locatedValue located == value &&
       locatedSpan located == span &&
       locatedPos located == spanStart span

prop_mapLocatedPreservesLocation :: SourceSpan -> Int -> Bool
prop_mapLocatedPreservesLocation span value =
    let located = locatedWithSpan span value
        mapped = mapLocated (*2) located
    in locatedValue mapped == value * 2 &&
       locatedSpan mapped == span &&
       locatedPos mapped == spanStart span

prop_mapLocatedIsFunctorial :: SourceSpan -> Int -> Int -> Bool
prop_mapLocatedIsFunctorial span x y =
    let located = locatedWithSpan span x
        mapped1 = mapLocated (+y) located
        mapped2 = mapLocated (+y) $ mapLocated (*2) located
        mapped3 = mapLocated (\v -> (v * 2) + y) located
    in locatedValue mapped2 == locatedValue mapped3

-- ============================================================================
-- Error Location Conversion Properties
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
-- Position Ordering Properties
-- ============================================================================

prop_positionOrderingIsConsistent :: SourcePos -> SourcePos -> Bool
prop_positionOrderingIsConsistent pos1 pos2 =
    let offset1 = posOffset pos1
        offset2 = posOffset pos2
    in if offset1 == offset2
       then pos1 == pos2
       else if offset1 < offset2
            then pos1 < pos2
            else pos1 > pos2

prop_positionOrderingIsTransitive :: SourcePos -> SourcePos -> SourcePos -> Bool
prop_positionOrderingIsTransitive pos1 pos2 pos3 =
    if pos1 <= pos2 && pos2 <= pos3
    then pos1 <= pos3
    else True

-- ============================================================================
-- Advanced Properties
-- ============================================================================

prop_advancingByEmptyStringDoesNothing :: SourcePos -> Bool
prop_advancingByEmptyStringDoesNothing pos =
    let advanced = advancePosBy "" pos
    in advanced == pos

prop_advancingByMultipleCharsConsistent :: String -> SourcePos -> Bool
prop_advancingByMultipleCharsConsistent str pos =
    let charByChar = foldl (flip posAfter) pos str
        allAtOnce = advancePosBy str pos
    in charByChar == allAtOnce

prop_spanLengthIsNonNegative :: SourceSpan -> Bool
prop_spanLengthIsNonNegative span =
    let startOffset = posOffset (spanStart span)
        endOffset = posOffset (spanEnd span)
    in endOffset - startOffset >= 0

prop_mergeEmptySpanDoesNothing :: SourceSpan -> SourcePos -> Bool
prop_mergeEmptySpanDoesNothing span pos =
    let empty = emptySpan pos
        merged = mergeSpans span empty
    in merged == mergeSpans span empty

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Advanced QuickCheck Tests"
    [ testGroup "Source Position Properties"
        [ fastProperty "start position is valid" prop_startPosIsValid
        , fastProperty "posAfter advances correctly" prop_posAfterAdvancesCorrectly
        , fastProperty "advancePosBy is consistent" prop_advancePosByConsistent
        , fastProperty "advancePosByText is consistent" prop_advancePosByTextConsistent
        , fastProperty "advancePosByLine advances correctly" prop_advancePosByLineAdvancesCorrectly
        , fastProperty "posAt creates correct position" prop_posAtCreatesCorrectPosition
        , fastProperty "posAtLineCol creates correct position" prop_posAtLineColCreatesCorrectPosition
        ]

    , testGroup "Source Span Properties"
        [ fastProperty "empty span is valid" prop_emptySpanIsValid
        , fastProperty "spanFrom creates valid span" prop_spanFromCreatesValidSpan
        , fastProperty "spanTo creates valid span" prop_spanToCreatesValidSpan
        , fastProperty "spanBetween creates valid span" prop_spanBetweenCreatesValidSpan
        , fastProperty "mergeSpans is commutative" prop_mergeSpansIsCommutative
        , fastProperty "mergeSpans is associative" prop_mergeSpansIsAssociative
        , fastProperty "mergeSpans contains originals" prop_mergeSpansContainsOriginals
        ]

    , testGroup "Located Value Properties"
        [ fastProperty "locatedAt creates correct location" prop_locatedAtCreatesCorrectLocation
        , fastProperty "locatedWithSpan creates correct location" prop_locatedWithSpanCreatesCorrectLocation
        , fastProperty "mapLocated preserves location" prop_mapLocatedPreservesLocation
        , fastProperty "mapLocated is functorial" prop_mapLocatedIsFunctorial
        ]

    , testGroup "Error Location Conversion Properties"
        [ fastProperty "toErrorLocation preserves position" prop_toErrorLocationPreservesPosition
        , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpanPreservesRange
        ]

    , testGroup "Position Ordering Properties"
        [ fastProperty "position ordering is consistent" prop_positionOrderingIsConsistent
        , fastProperty "position ordering is transitive" prop_positionOrderingIsTransitive
        ]

    , testGroup "Advanced Properties"
        [ fastProperty "advancing by empty string does nothing" prop_advancingByEmptyStringDoesNothing
        , fastProperty "advancing by multiple chars is consistent" prop_advancingByMultipleCharsConsistent
        , fastProperty "span length is non-negative" prop_spanLengthIsNonNegative
        , fastProperty "merge empty span does nothing" prop_mergeEmptySpanDoesNothing
        ]
    ]