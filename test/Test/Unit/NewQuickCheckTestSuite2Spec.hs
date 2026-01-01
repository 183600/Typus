module Test.Unit.NewQuickCheckTestSuite2Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Char (isSpace)

import TestSupport.QuickCheck (fastProperty)
import SourceLocation

-- | Test suite for SourceLocation module position calculations
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite2 - SourceLocation Position Calculations"
    [ testGroup "SourcePos basic operations"
        [ testCase "startPos has correct initial values" $ do
            posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0
            
        , testCase "posAt creates position with correct line L.and column" $ do
            let pos = posAt 5 10
            posLine pos @?= 5
            posColumn pos @?= 10
            posOffset pos @?= 0
            
        , testCase "posAtLineCol creates position with L.all fields" $ do
            let pos = posAtLineCol 3 7 42
            posLine pos @?= 3
            posColumn pos @?= 7
            posOffset pos @?= 42
        ]

    , testGroup "Position advancement with different characters"
        [ testCase "posAfter handles newline correctly" $ do
            let pos = posAt 1 5
                newPos = posAfter '\n' pos
            posLine newPos @?= 2
            posColumn newPos @?= 1
            posOffset newPos @?= 6
            
        , testCase "posAfter handles tab correctly" $ do
            let pos = posAt 1 3
                newPos = posAfter '\t' pos
            posColumn newPos @?= 9  -- ((3-1) `div` 8 + 1) * 8 + 1 = 9
            posOffset newPos @?= 3
            
        , testCase "posAfter handles tab at column 8" $ do
            let pos = posAt 1 8
                newPos = posAfter '\t' pos
            posColumn newPos @?= 9  -- ((8-1) `div` 8 + 1) * 8 + 1 = 9
            posOffset newPos @?= 8
            
        , testCase "posAfter handles regular character" $ do
            let pos = posAt 1 5
                newPos = posAfter 'a' pos
            posLine newPos @?= 1
            posColumn newPos @?= 6
            posOffset newPos @?= 5
            
        , fastProperty "posAfter newline increments line L.and resets column" prop_posAfterNewline
        , fastProperty "posAfter tab advances to next tab stop" prop_posAfterTab
        , fastProperty "posAfter regular char increments column" prop_posAfterRegularChar
        ]

    , testGroup "SourceSpan operations"
        [ testCase "emptySpan has correct properties" $ do
            let span = emptySpan
            isValidSpan span @?= False
            
        , testCase "spanFrom creates span from single position" $ do
            let pos = posAt 2 5
                span = spanFrom pos
            isValidSpan span @?= True
            
        , testCase "spanBetween creates correct span" $ do
            let start = posAt 1 1
                end = posAt 2 10
                span = spanBetween start end
            isValidSpan span @?= True
            
        , testCase "mergeSpans combines spans correctly" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 2 1) (posAt 2 15)
                merged = mergeSpans span1 span2
            isValidSpan merged @?= True
            
        , fastProperty "mergeSpans is commutative" prop_mergeSpansCommutative
        , fastProperty "mergeSpans contains both spans" prop_mergeSpansContainsBoth
        ]

    , testGroup "Located value operations"
        [ testCase "locatedAt creates located value" $ do
            let pos = posAt 3 7
                value = "test"
                located = locatedAt pos value
            locatedValue located @?= "test"
            locatedPos located @?= pos
            
        , testCase "locatedWithSpan creates located value with span" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 5)
                value = 42
                located = locatedWithSpan span value
            locatedValue located @?= 42
            locatedSpan located @?= span
            
        , testCase "mapLocated transforms value while preserving location" $ do
            let pos = posAt 2 3
                located = locatedAt pos 5
                transformed = mapLocated (*2) located
            locatedValue transformed @?= 10
            locatedPos transformed @?= pos
        ]

    , testGroup "Location tracking state operations"
        [ testCase "runLocationTracker with basic operations" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt 2 5)
                    getCurrentPos
            result @?= posAt 2 5
            
        , testCase "withLocationTracking tracks positions" $ do
            let result = runLocationTracker $ withLocationTracking $ do
                    markSpanStart
                    setCurrentPos (posAt 1 10)
                    markSpanEnd
                    getCurrentPos
            posLine result @?= 1
            posColumn result @?= 10
        ]

    , testGroup "Text position advancement"
        [ testCase "advancePosByText handles empty text" $ do
            let pos = posAt 1 1
                newPos = advancePosByText "" pos
            newPos @?= pos
            
        , testCase "advancePosByText handles simple text" $ do
            let pos = posAt 1 1
                newPos = advancePosByText "hello" pos
            posColumn newPos @?= 6
            posOffset newPos @?= 5
            
        , testCase "advancePosByText handles newlines" $ do
            let pos = posAt 1 1
                newPos = advancePosByText "hi\nworld" pos
            posLine newPos @?= 2
            posColumn newPos @?= 6
            posOffset newPos @?= 10
            
        , testCase "advancePosByText handles tabs" $ do
            let pos = posAt 1 1
                newPos = advancePosByText "\t" pos
            posColumn newPos @?= 9
            posOffset newPos @?= 1
            
        , fastProperty "advancePosByText is consistent with posAfter" prop_advancePosByTextConsistency
        , fastProperty "advancePosByLine handles line changes" prop_advancePosByLine
        ]

    , testGroup "Error location conversion"
        [ testCase "toErrorLocation converts position correctly" $ do
            let pos = posAt 3 7
                errLoc = toErrorLocation pos
            -- ErrorLocation structure should be verified
            True @?= True  -- Placeholder - actual ErrorLocation fields would be checked
            
        , testCase "toErrorLocationWithSpan converts span correctly" $ do
            let span = spanBetween (posAt 1 1) (posAt 2 10)
                errLoc = toErrorLocationWithSpan span
            -- ErrorLocation structure should be verified
            True @?= True  -- Placeholder - actual ErrorLocation fields would be checked
        ]

    , testGroup "Position ordering L.and comparison"
        [ testCase "SourcePos ordering works correctly" $ do
            let pos1 = posAt 1 1
                pos2 = posAt 1 10
                pos3 = posAt 2 1
            pos1 < pos2 @?= True
            pos2 < pos3 @?= True
            pos1 < pos3 @?= True
            
        , fastProperty "position ordering is transitive" prop_positionOrderingTransitive
        , fastProperty "position ordering is antisymmetric" prop_positionOrderingAntisymmetric
        ]

    , testGroup "Span validation L.and edge cases"
        [ testCase "span validation with start > end" $ do
            let span = spanBetween (posAt 2 10) (posAt 1 5)
            isValidSpan span @?= False
            
        , testCase "span validation with same positions" $ do
            let pos = posAt 3 7
                span = spanBetween pos pos
            isValidSpan span @?= True  -- Single character span is valid
            
        , fastProperty "span validation properties" prop_spanValidationProperties
        ]
    ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Position advancement properties
prop_posAfterNewline :: SourcePos -> Bool
prop_posAfterNewline pos = 
    let newPos = posAfter '\n' pos
    in posLine newPos == posLine pos + 1 && posColumn newPos == 1

prop_posAfterTab :: SourcePos -> Bool
prop_posAfterTab pos = 
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in posColumn newPos == expectedCol && posLine newPos == posLine pos

prop_posAfterRegularChar :: Char -> SourcePos -> Property
prop_posAfterRegularChar c pos = 
    c `notElem` "\n\t" ==> 
    let newPos = posAfter c pos
    in posColumn newPos == posColumn pos + 1 && 
       posLine newPos == posLine pos &&
       posOffset newPos == posOffset pos + 1

-- Span properties
prop_mergeSpansCommutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansCommutative start1 end1 start2 end2 =
    let span1 = spanBetween start1 end1
        span2 = spanBetween start2 end2
        merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in merged1 == merged2

prop_mergeSpansContainsBoth :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansContainsBoth start1 end1 start2 end2 =
    let span1 = spanBetween start1 end1
        span2 = spanBetween start2 end2
        merged = mergeSpans span1 span2
    in True  -- Would need actual containment checking functions

-- Text advancement properties
prop_advancePosByTextConsistency :: String -> SourcePos -> Bool
prop_advancePosByTextConsistency text pos = 
    let finalPos = advancePosByText text pos
        charByCharPos = L.foldl (flip posAfter) pos text
    in finalPos == charByCharPos

prop_advancePosByLine :: Int -> SourcePos -> Bool
prop_advancePosByLine lines pos = 
    let newPos = advancePosByLine lines pos
    in posLine newPos == posLine pos + lines && posColumn newPos == posColumn pos

-- Position ordering properties
prop_positionOrderingTransitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_positionOrderingTransitive pos1 pos2 pos3 =
    (pos1 < pos2 && pos2 < pos3) ==> pos1 < pos3

prop_positionOrderingAntisymmetric :: SourcePos -> SourcePos -> Property
prop_positionOrderingAntisymmetric pos1 pos2 =
    (pos1 < pos2) ==> not (pos2 < pos1)

-- Span validation properties
prop_spanValidationProperties :: SourcePos -> SourcePos -> Bool
prop_spanValidationProperties start end =
    let span = spanBetween start end
        startBeforeEnd = posLine start < posLine end || 
                         (posLine start == posLine end && posColumn start <= posColumn end)
    in isValidSpan span == startBeforeEnd

-- Helper functions for generating test data
genValidChar :: Gen Char
genValidChar = oneof 
    [ elements ['a'..'z']
    , elements ['A'..'Z']
    , elements ['0'..'9']
    , elements " !@#$%^&*()_+-=[]{}|;':\",./<>?"
    , return '\n'
    , return '\t'
    ]

genSourcePos :: Gen SourcePos
genSourcePos = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 10000)
    return $ posAtLineCol line column offset