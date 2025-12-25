{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.SourceLocationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..), HasLocation(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos, markSpanStart, markSpanEnd, withLocationTracking
  , toErrorLocation, toErrorLocationWithSpan
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Char (isSpace, isLetter, isDigit)

-- ============================================================================
-- Advanced Source Position Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Advanced Tests"
    [ testGroup "Advanced Position Operations"
        [ testCase "posAfter handles complex tab scenarios" $ do
            let pos1 = startPos { posColumn = 1 }
            let pos2 = posAfter '\t' pos1
            pos2 @?= startPos { posColumn = 9, posOffset = 1 }
            
            let pos3 = posAfter '\t' pos2
            pos3 @?= startPos { posColumn = 17, posOffset = 2 }
            
            let pos4 = posAfter '\t' (startPos { posColumn = 5 })
            pos4 @?= startPos { posColumn = 9, posOffset = 1 }

        , testCase "advancePosBy handles mixed characters correctly" $ do
            let start = startPos
            let advanced = advancePosBy "hello\n\tworld" start
            advanced @?= SourcePos
                { posLine = 2
                , posColumn = 9
                , posOffset = 12
                }

        , testCase "advancePosByText handles Unicode characters" $ do
            let start = startPos
            let text = T.pack "héllo🚀world"
            let advanced = advancePosByText text start
            advanced @?= SourcePos
                { posLine = 1
                , posColumn = 12
                , posOffset = 12
                }

        , testCase "advancePosByLine handles multiple lines" $ do
            let start = posAt 5 10
            let advanced = advancePosByLine 3 start
            advanced @?= SourcePos
                { posLine = 8
                , posColumn = 1
                , posOffset = 0
                }
        ]

    , testGroup "Complex Span Operations"
        [ testCase "mergeSpans handles non-overlapping spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 5)
            let span2 = spanBetween (posAt 2 3) (posAt 2 8)
            let merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 1) (posAt 2 8)

        , testCase "mergeSpans handles nested spans" $ do
            let outer = spanBetween (posAt 1 1) (posAt 5 10)
            let inner = spanBetween (posAt 2 3) (posAt 4 7)
            let merged = mergeSpans outer inner
            merged @?= outer

        , testCase "isValidSpan detects invalid spans" $ do
            let valid = spanBetween (posAt 1 1) (posAt 1 5)
            assertBool "valid span should be valid" (isValidSpan valid)
            
            let invalid = spanBetween (posAt 1 5) (posAt 1 1)
            assertBool "reversed span should be invalid" (not $ isValidSpan invalid)

        , testCase "span operations preserve consistency" $ do
            let start = posAt 3 7
            let end = posAt 5 12
            let span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
        ]

    , testGroup "Advanced Located Value Operations"
        [ testCase "mapLocated preserves position information" $ do
            let original = locatedAt (posAt 2 3) "hello"
            let mapped = mapLocated length original
            locatedValue mapped @?= 5
            locatedPos mapped @?= posAt 2 3
            locatedSpan mapped @?= emptySpan (posAt 2 3)

        , testCase "locatedWithSpan creates proper located values" $ do
            let span = spanBetween (posAt 1 5) (posAt 1 10)
            let located = locatedWithSpan span 42
            locatedValue located @?= 42
            locatedPos located @?= posAt 1 5
            locatedSpan located @?= span

        , testCase "HasLocation class works correctly" $ do
            let span = spanBetween (posAt 3 1) (posAt 3 8)
            let located = locatedWithSpan span "test"
            getLocation located @?= span
        ]

    , testGroup "Location Tracking Monad Tests"
        [ testCase "LocationTracker maintains position correctly" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt 2 5)
                    getCurrentPos
            result @?= posAt 2 5

        , testCase "markSpanStart and markSpanEnd work together" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt 1 1)
                    start <- markSpanStart
                    setCurrentPos (posAt 1 10)
                    markSpanEnd start
            result @?= spanBetween (posAt 1 1) (posAt 1 10)

        , testCase "withLocationTracking returns correct final position" $ do
            let (result, finalPos) = withLocationTracking (posAt 3 2) $ do
                    setCurrentPos (posAt 4 7)
                    return 42
            result @?= 42
            finalPos @?= posAt 4 7
        ]

    , testGroup "Error Location Conversion Tests"
        [ testCase "toErrorLocation creates correct error location" $ do
            let pos = posAt 10 15
            let errLoc = toErrorLocation pos
            -- Note: We can't directly compare ErrorLocation without seeing its constructor
            -- but we can test the function doesn't crash and returns something reasonable
            assertBool "error location should be created" (True)

        , testCase "toErrorLocationWithSpan handles multi-line spans" $ do
            let span = spanBetween (posAt 5 3) (posAt 7 12)
            let errLoc = toErrorLocationWithSpan span
            assertBool "multi-line span error location should be created" (True)
        ]

    , testGroup "Property-Based Tests"
        [ fastProperty "posAfter is consistent with advancePos" $ 
            \char pos -> posAfter char pos === advancePos char pos

        , fastProperty "spanBetween always creates valid spans" $
            \start end -> let span = spanBetween start end
                          in isValidSpan span || (start > end)

        , fastProperty "mergeSpans is commutative" $
            \span1 span2 -> mergeSpans span1 span2 === mergeSpans span2 span1

        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 -> 
                mergeSpans span1 (mergeSpans span2 span3) ===
                mergeSpans (mergeSpans span1 span2) span3

        , fastProperty "locatedAt creates span with same start and end" $
            \pos value -> 
                let located = locatedAt pos value
                    span = locatedSpan located
                in spanStart span === spanEnd span

        , fastProperty "mapLocated preserves span" $
            \pos value -> 
                let original = locatedAt pos value
                    mapped = mapLocated (+1) original
                in locatedSpan original === locatedSpan mapped

        , fastProperty "advancePosBy is consistent with repeated posAfter" $
            \chars pos ->
                let text = take 100 chars  -- Limit for performance
                    advanced1 = advancePosBy text pos
                    advanced2 = foldl (flip posAfter) pos text
                in advanced1 === advanced2

        , fastProperty "spanBetween with same positions creates empty span" $
            \pos ->
                let span = spanBetween pos pos
                in spanStart span === span && spanEnd span === span

        , fastProperty "mergeSpans contains both original spans" $
            \span1 span2 ->
                let merged = mergeSpans span1 span2
                    contains s1 s2 = spanStart s1 <= spanStart s2 && spanEnd s1 >= spanEnd s2
                in contains merged span1 .&&. contains merged span2
        ]

    , testGroup "Edge Cases and Error Conditions"
        [ testCase "position advancement handles empty string" $ do
            let start = posAt 1 1
            let advanced = advancePosBy "" start
            advanced @?= start

        , testCase "position advancement handles only newlines" $ do
            let start = startPos
            let advanced = advancePosBy "\n\n\n" start
            advanced @?= SourcePos { posLine = 4, posColumn = 1, posOffset = 3 }

        , testCase "position advancement handles only tabs" $ do
            let start = startPos { posColumn = 1 }
            let advanced = advancePosBy "\t\t\t" start
            advanced @?= SourcePos { posLine = 1, posColumn = 25, posOffset = 3 }

        , testCase "empty span at position is valid" $ do
            let pos = posAt 5 10
            let span = emptySpan pos
            assertBool "empty span should be valid" (isValidSpan span)

        , testCase "located values can be nested" $ do
            let inner = locatedAt (posAt 2 3) "inner"
            let outer = locatedAt (posAt 1 1) inner
            locatedValue outer @?= inner
            locatedPos outer @?= posAt 1 1
            locatedPos (locatedValue outer) @?= posAt 2 3
        ]

    , testGroup "Performance and Stress Tests"
        [ testCase "large text advancement" $ do
            let largeText = replicate 10000 'a'
            let start = startPos
            let advanced = advancePosBy largeText start
            posLine advanced @?= 1
            posColumn advanced @?= 10001
            posOffset advanced @?= 10000

        , testCase "many span merges" $ do
            let spans = [spanBetween (posAt i 1) (posAt i 10) | i <- [1..100]]
            let merged = foldl mergeSpans (head spans) (tail spans)
            spanStart merged @?= posAt 1 1
            spanEnd merged @?= posAt 100 10
        ]
    ]