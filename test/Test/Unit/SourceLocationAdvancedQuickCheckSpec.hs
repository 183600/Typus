module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, Positive(..), NonNegative(..))
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, posAtLineCol, 
                     emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                     locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
                     advancePos, advancePosBy, advancePosByText, advancePosByLine,
                     toErrorLocation, toErrorLocationWithSpan)

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
        end <- arbitrary
        if isValidSpan $ SourceSpan start end
           then return $ SourceSpan start end
           else return $ SourceSpan start start

instance Arbitrary a => Arbitrary (Located a) where
    arbitrary = do
        value <- arbitrary
        span <- arbitrary
        return $ Located value span

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Advanced QuickCheck Tests"
    [ testGroup "SourcePos Properties"
        [ testProperty "posAfter newline increments line and resets column" $
            fastProperty prop_posAfterNewline
        
        , testProperty "posAfter non-newline increments column" $
            fastProperty prop_posAfterNonNewline
        
        , testProperty "posAt creates consistent positions" $
            fastProperty prop_posAtConsistency
        
        , testProperty "advancePosByText handles multi-line text correctly" $
            fastProperty prop_advancePosByTextMultiline
        ]

    , testGroup "SourceSpan Properties"
        [ testProperty "emptySpan has zero length" $
            fastProperty prop_emptySpanZeroLength
        
        , testProperty "spanFrom creates valid spans" $
            fastProperty prop_spanFromValid
        
        , testProperty "spanTo creates valid spans" $
            fastProperty prop_spanToValid
        
        , testProperty "mergeSpans is associative" $
            fastProperty prop_mergeSpansAssociative
        
        , testProperty "mergeSpans is commutative" $
            fastProperty prop_mergeSpansCommutative
        ]

    , testGroup "Located Properties"
        [ testProperty "locatedAt preserves position" $
            fastProperty prop_locatedAtPreservesPosition
        
        , testProperty "locatedWithSpan preserves span" $
            fastProperty prop_locatedWithSpanPreservesSpan
        
        , testProperty "mapLocated preserves location" $
            fastProperty prop_mapLocatedPreservesLocation
        ]

    , testGroup "Position Advancement Properties"
        [ testProperty "advancePos is consistent with posAfter" $
            fastProperty prop_advancePosConsistency
        
        , testProperty "advancePosBy handles zero correctly" $
            fastProperty prop_advancePosByZero
        
        , testProperty "advancePosByLine handles zero lines correctly" $
            fastProperty prop_advancePosByLineZero
        ]

    , testGroup "Error Location Properties"
        [ testProperty "toErrorLocation preserves line and column" $
            fastProperty prop_toErrorLocationPreservesLineCol
        
        , testProperty "toErrorLocationWithSpan preserves span information" $
            fastProperty prop_toErrorLocationWithSpanPreservesSpan
        ]
    ]

-- ============================================================================
-- SourcePos Property Definitions
-- ============================================================================

prop_posAfterNewline :: SourcePos -> Bool
prop_posAfterNewline pos =
    let newPos = posAfter '\n' pos
    in posLine newPos == posLine pos + 1 && posColumn newPos == 1

prop_posAfterNonNewline :: SourcePos -> Char -> Bool
prop_posAfterNonNewline pos char
    | char == '\n' = True  -- handled by prop_posAfterNewline
    | otherwise = 
        let newPos = posAfter char pos
        in posLine newPos == posLine pos && 
           posColumn newPos == posColumn pos + 1

prop_posAtConsistency :: Int -> Int -> Int -> Bool
prop_posAtConsistency line column offset
    | line <= 0 || column <= 0 || offset < 0 = True  -- invalid inputs are handled gracefully
    | otherwise =
        let pos = posAt line column offset
        in posLine pos == line && posColumn pos == column && posOffset pos == offset

prop_advancePosByTextMultiline :: SourcePos -> String -> Bool
prop_advancePosByTextMultiline pos text =
    let finalPos = advancePosByText pos text
        expectedLine = posLine pos + length (filter (== '\n') text)
        lastLineStart = if '\n' `elem` text
                       then length (takeWhile (/= '\n') (reverse text))
                       else posColumn pos + length text
    in posLine finalPos == expectedLine &&
       (if '\n' `elem` text then posColumn finalPos == lastLineStart + 1
        else posColumn finalPos == lastLineStart)

-- ============================================================================
-- SourceSpan Property Definitions
-- ============================================================================

prop_emptySpanZeroLength :: Bool
prop_emptySpanZeroLength =
    let span = emptySpan
    in spanStart span == spanEnd span

prop_spanFromValid :: SourcePos -> Bool
prop_spanFromValid pos =
    let span = spanFrom pos
    in spanStart span == pos && spanEnd span == pos

prop_spanToValid :: SourcePos -> Bool
prop_spanToValid pos =
    let span = spanTo pos
    in spanStart span == pos && spanEnd span == pos

prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansAssociative span1 span2 span3 =
    let merged12 = mergeSpans span1 span2
        merged23 = mergeSpans span2 span3
        result1 = mergeSpans merged12 span3
        result2 = mergeSpans span1 merged23
    in result1 == result2

prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansCommutative span1 span2 =
    let result1 = mergeSpans span1 span2
        result2 = mergeSpans span2 span1
    in result1 == result2

-- ============================================================================
-- Located Property Definitions
-- ============================================================================

prop_locatedAtPreservesPosition :: Int -> String -> Bool
prop_locatedAtPreservesPosition line value =
    let pos = posAt line 1 0
        located = locatedAt pos value
    in locatedPos located == pos

prop_locatedWithSpanPreservesSpan :: Int -> String -> Bool
prop_locatedWithSpanPreservesSpan line value =
    let pos = posAt line 1 0
        span = spanFrom pos
        located = locatedWithSpan span value
    in locatedSpan located == span

prop_mapLocatedPreservesLocation :: Int -> String -> Bool
prop_mapLocatedPreservesLocation line value =
    let pos = posAt line 1 0
        located = locatedAt pos value
        mapped = mapLocated (length) located
    in locatedPos mapped == locatedPos located &&
       locatedSpan mapped == locatedSpan located

-- ============================================================================
-- Position Advancement Property Definitions
-- ============================================================================

prop_advancePosConsistency :: SourcePos -> Char -> Bool
prop_advancePosConsistency pos char =
    let pos1 = advancePos pos char
        pos2 = posAfter char pos
    in pos1 == pos2

prop_advancePosByZero :: SourcePos -> Bool
prop_advancePosByZero pos =
    let pos1 = advancePosBy pos 0
    in pos1 == pos

prop_advancePosByLineZero :: SourcePos -> Bool
prop_advancePosByLineZero pos =
    let pos1 = advancePosByLine pos 0
    in pos1 == pos

-- ============================================================================
-- Error Location Property Definitions
-- ============================================================================

prop_toErrorLocationPreservesLineCol :: SourcePos -> Bool
prop_toErrorLocationPreservesLineCol pos =
    let errLoc = toErrorLocation pos
    in errorLine errLoc == posLine pos && errorColumn errLoc == posColumn pos

prop_toErrorLocationWithSpanPreservesSpan :: SourceSpan -> Bool
prop_toErrorLocationWithSpanPreservesSpan span =
    let errLoc = toErrorLocationWithSpan span
        start = spanStart span
        end = spanEnd span
    in errorLine errLoc == posLine start &&
       errorColumn errLoc == posColumn start &&
       errorEndLine errLoc == posLine end &&
       errorEndColumn errLoc == posColumn end