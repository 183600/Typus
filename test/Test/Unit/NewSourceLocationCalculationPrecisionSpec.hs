{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationCalculationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation (
    SourcePos(..),
    SourceSpan(..),
    Located(..),
    HasLocation(..),
    startPos,
    posAfter,
    posAt,
    posAtLineCol,
    emptySpan,
    spanFrom,
    spanTo,
    spanBetween,
    mergeSpans,
    isValidSpan,
    locatedAt,
    locatedWithSpan,
    locatedValue,
    locatedSpan,
    locatedPos,
    mapLocated,
    advancePos,
    advancePosBy,
    advancePosByText,
    advancePosByLine
 )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isControl)

-- | Source location calculation precision tests
tests :: TestTree
tests =
  testGroup "New Source Location Calculation Precision Tests"
    [ testGroup "Position arithmetic precision"
        [ testCase "posAfter handles tab stops correctly" $ do
            let start = posAt 1 1
                pos1 = posAfter '\t' start
                pos2 = posAfter 'a' pos1
                pos3 = posAfter '\t' pos2
            posColumn pos1 @?= 9  -- First tab to column 9
            posColumn pos2 @?= 10
            posColumn pos3 @?= 17 -- Second tab to column 17
            
        , testCase "posAfter handles multiple newlines correctly" $ do
            let start = posAt 5 10
                pos1 = posAfter '\n' start
                pos2 = posAfter '\n' pos1
                pos3 = posAfter 'x' pos2
            posLine pos1 @?= 6
            posColumn pos1 @?= 1
            posLine pos2 @?= 7
            posColumn pos2 @?= 1
            posLine pos3 @?= 7
            posColumn pos3 @?= 2
            
        , fastProperty "posAfter offset calculation is consistent" $
            \char startPos ->
                let result = posAfter char startPos
                    expectedOffset = posOffset startPos + 1
                in posOffset result === expectedOffset
        ]
        
    , testGroup "Span merging precision"
        [ testCase "mergeSpans handles nested spans correctly" $ do
            let outer = spanBetween (posAt 1 1) (posAt 10 20)
                inner = spanBetween (posAt 2 5) (posAt 8 15)
                merged = mergeSpans outer inner
            spanStart merged @?= spanStart outer
            spanEnd merged @?= spanEnd outer
            
        , testCase "mergeSpans handles L.reverse order spans" $ do
            let span1 = spanBetween (posAt 5 10) (posAt 8 15)
                span2 = spanBetween (posAt 2 3) (posAt 12 25)
                merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span2
            spanEnd merged @?= spanEnd span2
            
        , fastProperty "merged span is always valid" $
            \span1 span2 ->
                let merged = mergeSpans span1 span2
                in property $ isValidSpan merged
        ]
        
    , testGroup "Large file position handling"
        [ testCase "position calculation at large line numbers" $ do
            let largeLinePos = posAt 1000000 50
                afterNewline = posAfter '\n' largeLinePos
            posLine afterNewline @?= 1000001
            posColumn afterNewline @?= 1
            posOffset afterNewline @?= posOffset largeLinePos + 1
            
        , testCase "span validation with large offsets" $ do
            let start = posAtLineCol 1 1 0
                end = posAtLineCol 1000 1000 1000000
                largeSpan = spanBetween start end
            isValidSpan largeSpan @?= True
        ]
        
    , testGroup "Unicode L.and multi-byte character handling"
        [ testCase "position tracking with Unicode characters" $ do
            let start = startPos
                pos1 = advancePosByText "你好" start
                pos2 = advancePosByText "世界" pos1
            -- Each Unicode character should advance position by 1 column
            posColumn pos1 @?= 3  -- 2 characters + start position
            posColumn pos2 @?= 5  -- 4 characters total
            
        , testCase "advancePosByText handles mixed content correctly" $ do
            let start = startPos
                text = "hello\n\t世界"
                result = advancePosByText text start
            posLine result @?= 2
            posColumn result @?= 9  -- After tab L.and Unicode chars
        ]
        
    , testGroup "Edge case position calculations"
        [ testCase "empty span at position" $ do
            let pos = posAt 10 20
                empty = emptySpan pos
            spanStart empty @?= pos
            spanEnd empty @?= pos
            isValidSpan empty @?= True
            
        , testCase "span validity with equal positions" $ do
            let pos = posAt 5 5
                span = spanBetween pos pos
            isValidSpan span @?= True
            
        , fastProperty "spanBetween creates valid spans" $
            \start end ->
                let span = spanBetween start end
                in property $ isValidSpan
        ]
        
    , testGroup "Located value precision"
        [ testCase "locatedAt preserves position information" $ do
            let pos = posAt 3 7
                value = "test"
                located = locatedAt pos value
            locatedPos located @?= pos
            locatedValue located @?= value
            spanStart (locatedSpan located) @?= pos
            spanEnd (locatedSpan located) @?= pos
            
        , testCase "mapLocated preserves location information" $ do
            let pos = posAt 2 4
                span = spanFrom pos
                original = locatedWithSpan span 42
                transformed = mapLocated (*2) original
            locatedPos transformed @?= locatedPos original
            locatedSpan transformed @?= locatedSpan original
            locatedValue transformed @?= 84
        ]
        
    , testGroup "Position advancement precision"
        [ testCase "advancePosBy handles zero L.length" $ do
            let start = posAt 5 10
                result = advancePosBy 0 start
            result @?= start
            
        , testCase "advancePosByLine handles line boundaries" $ do
            let start = posAt 3 5
                result = advancePosByLine 2 start
            posLine result @?= 5
            posColumn result @?= 5  -- Column should be preserved
        ]
        
    , testGroup "Complex text position scenarios"
        [ testCase "position tracking with tabs L.and Unicode mix" $ do
            let start = startPos
                text = "\t你好\tworld"
                finalPos = advancePosByText text start
            posLine finalPos @?= 1
            -- Tab to column 9, then 2 Unicode chars, then tab to next tab stop
            posColumn finalPos @?= 17
            
        , fastProperty "position advancement is monotonic" $
            \text start ->
                let pos1 = advancePosByText text start
                    pos2 = advancePosByText "x" pos1
                in property $ posOffset pos2 > posOffset pos1
        ]
    ]