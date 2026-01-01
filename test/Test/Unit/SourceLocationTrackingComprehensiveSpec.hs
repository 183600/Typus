{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationTrackingComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

import SourceLocation 
    ( SourcePos(..)
    , SourceSpan(..)
    , Located(..)
    , HasLocation(..)
    , startPos
    , posAfter
    , posAt
    , posAtLineCol
    , emptySpan
    , spanFrom
    , spanTo
    , spanBetween
    , mergeSpans
    , isValidSpan
    , locatedAt
    , locatedWithSpan
    , locatedValue
    , locatedSpan
    , locatedPos
    , mapLocated
    , LocationTracker
    , runLocationTracker
    , getCurrentPos
    , setCurrentPos
    , markSpanStart
    , markSpanEnd
    , withLocationTracking
    , toErrorLocation
    , toErrorLocationWithSpan
    , advancePos
    , advancePosBy
    )

import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when)

-- | Comprehensive tests for source location tracking
tests :: TestTree
tests =
  testGroup "Source Location Tracking Comprehensive"
    [ testGroup "SourcePos Basic Operations"
        [ testCase "startPos creates valid position" $ do
            let pos = startPos "test.typus"
            assertEqual "Start position should be line 1" 1 (sourceLine pos)
            assertEqual "Start position should be column 1" 1 (sourceColumn pos)

        , testCase "posAfter advances position correctly" $ do
            let start = startPos "test.typus"
                after = posAfter start 'a'
            assertEqual "Line should remain the same" 1 (sourceLine after)
            assertEqual "Column should advance by 1" 2 (sourceColumn after)

        , testCase "posAfter handles newline correctly" $ do
            let start = startPos "test.typus"
                after = posAfter start '\n'
            assertEqual "Line should advance by 1" 2 (sourceLine after)
            assertEqual "Column should reset to 1" 1 (sourceColumn after)

        , testCase "posAfter handles tab correctly" $ do
            let start = posAtLineCol "test.typus" 1 3
                after = posAfter start '\t'
            assertEqual "Line should remain the same" 1 (sourceLine after)
            assertEqual "Column should advance to next tab stop" 8 (sourceColumn after)

        , testCase "posAt creates specific position" $ do
            let pos = posAt "test.typus" 5 10
            assertEqual "Line should be correct" 5 (sourceLine pos)
            assertEqual "Column should be correct" 10 (sourceColumn pos)

        , testCase "posAtLineCol creates specific position" $ do
            let pos = posAtLineCol "test.typus" 3 7
            assertEqual "Line should be correct" 3 (sourceLine pos)
            assertEqual "Column should be correct" 7 (sourceColumn pos)
        ]

    , testGroup "SourceSpan Operations"
        [ testCase "emptySpan creates valid empty span" $ do
            let span = emptySpan "test.typus"
                start = spanStart span
                end = spanEnd span
            assertEqual "Empty span start line should be 1" 1 (sourceLine start)
            assertEqual "Empty span start column should be 1" 1 (sourceColumn start)
            assertEqual "Empty span end line should be 1" 1 (sourceLine end)
            assertEqual "Empty span end column should be 1" 1 (sourceColumn end)

        , testCase "spanFrom creates span from position" $ do
            let pos = posAt "test.typus" 2 5
                span = spanFrom pos
            assertEqual "Span start should match input position" pos (spanStart span)
            assertEqual "Span end should match input position" pos (spanEnd span)

        , testCase "spanTo creates span to position" $ do
            let start = posAt "test.typus" 1 3
                end = posAt "test.typus" 1 8
                span = spanTo start end
            assertEqual "Span start should be correct" start (spanStart span)
            assertEqual "Span end should be correct" end (spanEnd span)

        , testCase "spanBetween creates correct span" $ do
            let start = posAt "test.typus" 2 4
                end = posAt "test.typus" 4 7
                span = spanBetween start end
            assertEqual "Span start should be correct" start (spanStart span)
            assertEqual "Span end should be correct" end (spanEnd span)

        , testCase "isValidSpan validates correctly" $ do
            let validSpan = spanBetween (posAt "test.typus" 1 1) (posAt "test.typus" 1 5)
                invalidSpan = spanBetween (posAt "test.typus" 2 5) (posAt "test.typus" 1 3)
                samePosSpan = spanFrom (posAt "test.typus" 3 2)
            assertBool "Valid span should be recognized" (isValidSpan validSpan)
            assertBool "Invalid span (end before start) should be rejected" (not (isValidSpan invalidSpan))
            assertBool "Single position span should be valid" (isValidSpan samePosSpan)

        , testCase "mergeSpans combines spans correctly" $ do
            let span1 = spanBetween (posAt "test.typus" 1 3) (posAt "test.typus" 2 5)
                span2 = spanBetween (posAt "test.typus" 2 8) (posAt "test.typus" 4 2)
                merged = mergeSpans span1 span2
            assertEqual "Merged span start should be min of starts" 
                (spanStart span1) (spanStart merged)
            assertEqual "Merged span end should be max of ends" 
                (spanEnd span2) (spanEnd merged)
        ]

    , testGroup "Located Values"
        [ testCase "locatedAt creates located value" $ do
            let pos = posAt "test.typus" 3 7
                value = "test string"
                located = locatedAt pos value
            assertEqual "Located value should be correct" value (locatedValue located)
            assertEqual "Located position should be correct" pos (locatedPos located)

        , testCase "locatedWithSpan creates located value with span" $ do
            let span = spanBetween (posAt "test.typus" 1 2) (posAt "test.typus" 1 6)
                value = 42
                located = locatedWithSpan span value
            assertEqual "Located value should be correct" value (locatedValue located)
            assertEqual "Located span should be correct" span (locatedSpan located)

        , testCase "mapLocated transforms located values" $ do
            let pos = posAt "test.typus" 2 4
                original = locatedAt pos "hello"
                transformed = mapLocated (++ " world") original
            assertEqual "Transformed value should be correct" 
                "hello world" (locatedValue transformed)
            assertEqual "Position should be preserved" pos (locatedPos transformed)

        , testCase "Located values maintain location information" $ do
            let span = spanBetween (posAt "test.typus" 3 1) (posAt "test.typus" 3 10)
                located = locatedWithSpan span "test"
            case located of
                Located s v -> do
                    assertEqual "Span should be preserved" span s
                    assertEqual "Value should be preserved" "test" v
        ]

    , testGroup "LocationTracker Monad"
        [ testCase "LocationTracker tracks position correctly" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt "test.typus" 2 5)
                    getCurrentPos
            assertEqual "Position should be tracked correctly" 
                (posAt "test.typus" 2 5) result

        , testCase "LocationTracker tracks spans correctly" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt "test.typus" 1 3)
                    markSpanStart
                    setCurrentPos (posAt "test.typus" 1 8)
                    markSpanEnd
                    getCurrentPos
            assertEqual "Final position should be correct" 
                (posAt "test.typus" 1 8) result

        , testCase "withLocationTracking preserves context" $ do
            let initialPos = posAt "test.typus" 1 1
                result = runLocationTracker $ withLocationTracking initialPos $ do
                    advancePos 'a'
                    advancePos 'b'
                    getCurrentPos
            assertEqual "Context should be preserved" 
                (posAt "test.typus" 1 3) result

        , testCase "LocationTracker handles complex navigation" $ do
            let result = runLocationTracker $ do
                    setCurrentPos (posAt "test.typus" 1 1)
                    advancePos 'h'
                    advancePos 'e'
                    advancePos '\n'
                    advancePos 'w'
                    advancePos 'o'
                    advancePos 'r'
                    advancePos 'l'
                    advancePos 'd'
                    getCurrentPos
            assertEqual "Complex navigation should work" 
                (posAt "test.typus" 2 5) result
        ]

    , testGroup "Position Advancement"
        [ testCase "advancePos handles regular characters" $ do
            let start = posAt "test.typus" 1 5
                after = advancePos start 'x'
            assertEqual "Line should remain the same" 1 (sourceLine after)
            assertEqual "Column should advance by 1" 6 (sourceColumn after)

        , testCase "advancePos handles multiple newlines" $ do
            let start = posAt "test.typus" 2 10
                after1 = advancePos start '\n'
                after2 = advancePos after1 '\n'
                after3 = advancePos after2 'a'
            assertEqual "First newline should advance line" 3 (sourceLine after1)
            assertEqual "First newline should reset column" 1 (sourceColumn after1)
            assertEqual "Second newline should advance line" 4 (sourceLine after2)
            assertEqual "Second newline should reset column" 1 (sourceColumn after2)
            assertEqual "Character after newlines should be on correct line" 4 (sourceLine after3)
            assertEqual "Character after newlines should be on correct column" 2 (sourceColumn after3)

        , testCase "advancePosBy handles strings correctly" $ do
            let start = posAt "test.typus" 1 1
                after = advancePosBy start "hello\nworld"
            assertEqual "Should advance to correct line" 2 (sourceLine after)
            assertEqual "Should advance to correct column" 6 (sourceColumn after)

        , testCase "advancePosBy handles empty string" $ do
            let start = posAt "test.typus" 3 4
                after = advancePosBy start ""
            assertEqual "Empty string should not change position" start after

        , testCase "advancePosBy handles tabs correctly" $ do
            let start = posAt "test.typus" 1 3
                after = advancePosBy start "\t"
            assertEqual "Tab should advance to next tab stop" 8 (sourceColumn after)
        ]

    , testGroup "Error Location Conversion"
        [ testCase "toErrorLocation formats position correctly" $ do
            let pos = posAt "test.typus" 5 12
                errorLoc = toErrorLocation pos
            assertBool "Error location should contain line number" 
                ("5" `L.isInfixOf` errorLoc)
            assertBool "Error location should contain column number" 
                ("12" `L.isInfixOf` errorLoc)
            assertBool "Error location should contain filename" 
                ("test.typus" `L.isInfixOf` errorLoc)

        , testCase "toErrorLocationWithSpan formats span correctly" $ do
            let span = spanBetween (posAt "test.typus" 2 3) (posAt "test.typus" 2 8)
                errorLoc = toErrorLocationWithSpan span
            assertBool "Error location should contain line number" 
                ("2" `L.isInfixOf` errorLoc)
            assertBool "Error location should contain column range" 
                ("3-8" `L.isInfixOf` errorLoc)

        , testCase "toErrorLocationWithSpan handles multi-line spans" $ do
            let span = spanBetween (posAt "test.typus" 1 5) (posAt "test.typus" 3 2)
                errorLoc = toErrorLocationWithSpan span
            assertBool "Error location should contain start line" 
                ("1" `L.isInfixOf` errorLoc)
            assertBool "Error location should contain end line" 
                ("3" `L.isInfixOf` errorLoc)
        ]

    , testGroup "Edge Cases L.and Boundary Conditions"
        [ testCase "SourcePos handles zero values" $ do
            let zeroPos = SourcePos "test.typus" 0 0
            assertEqual "Zero line should be preserved" 0 (sourceLine zeroPos)
            assertEqual "Zero column should be preserved" 0 (sourceColumn zeroPos)

        , testCase "SourcePos handles very large values" $ do
            let largePos = SourcePos "test.typus" 999999 999999
            assertEqual "Large line should be preserved" 999999 (sourceLine largePos)
            assertEqual "Large column should be preserved" 999999 (sourceColumn largePos)

        , testCase "SourceSpan handles single character spans" $ do
            let singleCharSpan = spanBetween (posAt "test.typus" 2 3) (posAt "test.typus" 2 3)
            assertBool "Single character span should be valid" (isValidSpan singleCharSpan)

        , testCase "SourceSpan handles very large spans" $ do
            let largeSpan = spanBetween (posAt "test.typus" 1 1) (posAt "test.typus" 999999 999999)
            assertBool "Large span should be valid" (isValidSpan largeSpan)

        , testCase "Located values handle complex data types" $ do
            let complexValue = Just [1, 2, 3, 4, 5]
                pos = posAt "test.typus" 1 1
                located = locatedAt pos complexValue
            assertEqual "Complex values should be preserved" complexValue (locatedValue located)
        ]

    , testGroup "QuickCheck Properties"
        [ fastProperty "posAfter is consistent for same character" $
            \pos char -> posAfter pos char === posAfter pos char

        , fastProperty "spanFrom preserves position" $
            \pos -> let span = spanFrom pos
                    in spanStart span === pos && spanEnd span === pos

        , fastProperty "locatedAt preserves value L.and position" $
            \pos value -> let located = locatedAt pos value
                          in locatedValue located === value && locatedPos located === pos

        , fastProperty "mapLocated preserves position" $
            \pos value f -> let original = locatedAt pos value
                                transformed = mapLocated f original
                            in locatedPos transformed === locatedPos original

        , fastProperty "advancePosBy on empty string returns same position" $
            \pos -> advancePosBy pos "" === pos

        , fastProperty "mergeSpans is commutative for valid spans" $
            \span1 span2 -> isValidSpan span1 && isValidSpan span2 ==>
                let merged1 = mergeSpans span1 span2
                    merged2 = mergeSpans span2 span1
                in spanStart merged1 === spanStart merged2 &&
                   spanEnd merged1 === spanEnd merged2
        ]
    ]

-- Helper functions for testing
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- QuickCheck arbitraries for source location types
instance Arbitrary SourcePos where
    arbitrary = do
        line <- choose (0, 1000)
        col <- choose (0, 1000)
        return $ SourcePos "test.typus" line col

instance Arbitrary SourceSpan where
    arbitrary = do
        startLine <- choose (1, 100)
        startCol <- choose (1, 100)
        endLine <- choose (startLine, startLine + 10)
        endCol <- if endLine == startLine 
                  then choose (startCol, startCol + 50)
                  else choose (1, 100)
        let start = SourcePos "test.typus" startLine startCol
            end = SourcePos "test.typus" endLine endCol
        return $ SourceSpan start end

arbitraryChar :: Gen Char
arbitraryChar = oneof
    [ choose ('a', 'z')
    , choose ('A', 'Z')
    , choose ('0', '9')
    , elements [' ', '\t', '\n', '\r']
    , elements ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')']
    ]