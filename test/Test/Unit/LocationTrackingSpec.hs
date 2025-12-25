{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.LocationTrackingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements)
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, mapLocated
  , LocationTracker, runLocationTracker, getCurrentPos, setCurrentPos
  , markSpanStart, markSpanEnd, withLocationTracking
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )
import qualified Data.Text as T
import qualified Control.Monad.State as State
import Compiler.Errors.Core (ErrorLocation(..))

tests :: TestTree
tests = testGroup "Location Tracking Tests"
  [ testGroup "LocationTracker monad"
    [ testCase "starts at startPos" $
        runLocationTracker getCurrentPos @?= startPos
    , testCase "setCurrentPos changes position" $
        let newPos = posAt 5 10
        in runLocationTracker (setCurrentPos newPos >> getCurrentPos) @?= newPos
    , testCase "markSpanStart and markSpanEnd work together" $
        let action = do
              start <- markSpanStart
              setCurrentPos (posAt 2 5)
              markSpanEnd start
        in runLocationTracker action @?= spanBetween startPos (posAt 2 5)
    , testCase "withLocationTracking returns correct result" $
        let start = posAt 3 7
            action = do
              setCurrentPos (posAt 3 10)
              getCurrentPos
        in withLocationTracking start action @?= (posAt 3 10, posAt 3 10)
    ]
  , testGroup "Position advancement"
    [ testCase "advancePos handles multiline text" $
        let text = "hello\nworld"
            start = startPos
            result = advancePosBy text start
        in do
          posLine result @?= 2
          posColumn result @?= 6  -- "world" length + 1
    , testCase "advancePosByText handles empty text" $
        advancePosByText T.empty startPos @?= startPos
    , testCase "advancePosByText handles single character" $
        let result = advancePosByText (T.pack "a") startPos
        in do
          posLine result @?= 1
          posColumn result @?= 2
          posOffset result @?= 1
    , testCase "advancePosByLine updates line correctly" $
        let result = advancePosByLine 5 startPos
        in do
          posLine result @?= 6
          posColumn result @?= 1
    ]
  , testGroup "Span operations"
    [ testCase "spanFrom creates empty span" $
        let pos = posAt 3 5
            span = spanFrom pos
        in do
          spanStart span @?= pos
          spanEnd span @?= pos
    , testCase "spanTo creates empty span" $
        let pos = posAt 3 5
            span = spanTo pos
        in do
          spanStart span @?= pos
          spanEnd span @?= pos
    , testCase "mergeSpans handles overlapping spans" $
        let span1 = spanBetween (posAt 1 1) (posAt 1 10)
            span2 = spanBetween (posAt 1 5) (posAt 1 15)
            merged = mergeSpans span1 span2
        in do
          spanStart merged @?= posAt 1 1
          spanEnd merged @?= posAt 1 15
    , testCase "isValidSpan identifies invalid spans" $
        let invalidSpan = spanBetween (posAt 2 10) (posAt 1 5)
        in assertBool "Should identify invalid span" (not $ isValidSpan invalidSpan)
    ]
  , testGroup "Located values"
    [ testCase "mapLocated preserves location" $
        let value = "original"
            located = locatedAt startPos value
            transformed = mapLocated (++ " modified") located
        in do
          locatedValue transformed @?= "original modified"
          locatedSpan transformed @?= locatedSpan located
    , testCase "locatedAt creates span at position" $
        let value = 42
            located = locatedAt (posAt 3 7) value
        in do
          locatedValue located @?= 42
          locatedPos located @?= posAt 3 7
    ]
  , testGroup "Error location conversion"
    [ testCase "toErrorLocation creates correct ErrorLocation" $
        let pos = posAt 5 12
            errLoc = toErrorLocation pos
        in do
          line errLoc @?= 5
          column errLoc @?= 12
          filePath errLoc @?= Nothing
          endLine errLoc @?= Nothing
          endColumn errLoc @?= Nothing
    , testCase "toErrorLocationWithSpan includes range" $
        let span = spanBetween (posAt 3 5) (posAt 4 10)
            errLoc = toErrorLocationWithSpan span
        in do
          line errLoc @?= 3
          column errLoc @?= 5
          endLine errLoc @?= Just 4
          endColumn errLoc @?= Just 10
    ]
  , testGroup "Complex scenarios"
    [ testCase "tracking through Go code" $
        let goCode = "package main\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}"
            positions = scanl (\pos char -> advancePos char pos) startPos goCode
            lineBreaks = map fst $ filter ((== '\n') . snd) $ zip [0..] goCode
        in length lineBreaks @?= 3  -- Should have 3 newlines
    , testCase "span covering multiple lines" $
        let start = posAt 1 5
            end = posAt 3 10
            span = spanBetween start end
        in assertBool "Multi-line span should be valid" (isValidSpan span)
    , testCase "nested span merging" $
        let spans = [ spanBetween (posAt 1 1) (posAt 1 5)
                    , spanBetween (posAt 1 3) (posAt 1 8)
                    , spanBetween (posAt 1 6) (posAt 1 10)
                    ]
            merged = foldl mergeSpans (head spans) (tail spans)
        in do
          spanStart merged @?= posAt 1 1
          spanEnd merged @?= posAt 1 10
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "advancePos is consistent with posAfter" $
        \s pos -> advancePosBy s pos == foldl (flip posAfter) pos s
    , testProperty "mergeSpans is associative" $
        \span1 span2 span3 -> 
          mergeSpans span1 (mergeSpans span2 span3) == 
          mergeSpans (mergeSpans span1 span2) span3
    , testProperty "locatedAt . locatedValue = identity (up to location)" $
        \pos value -> locatedValue (locatedAt pos value) == value
    , testProperty "spanBetween start end = spanBetween end start when start = end" $
        \pos -> spanBetween pos pos == spanBetween pos pos
    , testProperty "isValidSpan spanBetween start end = start <= end" $
        \start end -> isValidSpan (spanBetween start end) == (start <= end)
    ]
  ]

-- Helper functions
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = Prelude.scanl

-- Enhanced Arbitrary instances
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (posLine start, posLine start + 10)
    endColumn <- if endLine == posLine start 
                 then choose (posColumn start, posColumn start + 50)
                 else choose (1, 100)
    endOffset <- choose (posOffset start, posOffset start + 1000)
    let end = SourcePos endLine endColumn endOffset
    return $ SourceSpan start end