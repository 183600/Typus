{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf, Positive(..), NonNegative(..))

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
  , runLocationTracker
  , getCurrentPos
  , setCurrentPos
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | 新的QuickCheck属性测试，针对SourceLocation模块的边界条件
tests :: TestTree
tests =
  testGroup "New Cabal SourceLocation QuickCheck Tests"
    [ testGroup "SourcePos properties"
        [ fastProperty "startPos has correct values" $
            startPos === SourcePos 1 1 0

        , fastProperty "posAt creates correct position" $
            \line col ->
              let pos = posAt line col
              in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

        , fastProperty "posAtLineCol creates correct position" $
            \line col offset ->
              let pos = posAtLineCol line col offset
              in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

        , fastProperty "posAfter newline increments line and resets column" $
            \line col offset ->
              let pos = SourcePos line col offset
                  newPos = posAfter '\n' pos
              in posLine newPos === line + 1 .&&. posColumn newPos === 1 .&&. posOffset newPos === offset + 1

        , fastProperty "posAfter tab aligns to next tab stop" $
            \line col offset ->
              let pos = SourcePos line col offset
                  newPos = posAfter '\t' pos
                  expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
              in posColumn newPos === expectedCol .&&. posOffset newPos === offset + 1

        , fastProperty "posAfter regular char increments column" $
            \line col offset ch ->
              not (ch `elem` ['\n', '\t']) ==>
              let pos = SourcePos line col offset
                  newPos = posAfter ch pos
              in posLine newPos === line .&&. posColumn newPos === col + 1 .&&. posOffset newPos === offset + 1

        , fastProperty "SourcePos ordering is correct" $
            \line1 col1 line2 col2 ->
              let pos1 = posAt line1 col1
                  pos2 = posAt line2 col2
              in (line1 < line2 || (line1 == line2 && col1 < col2)) <=> (pos1 < pos2)
        ]

    , testGroup "SourceSpan properties"
        [ fastProperty "emptySpan has same start and end" $
            \pos ->
              let span = emptySpan pos
              in spanStart span === pos .&&. spanEnd span === pos

        , fastProperty "spanFrom creates empty span" $
            \pos ->
              spanFrom pos === emptySpan pos

        , fastProperty "spanTo creates empty span" $
            \pos ->
              spanTo pos === emptySpan pos

        , fastProperty "spanBetween creates correct span" $
            \startPos endPos ->
              let span = spanBetween startPos endPos
              in spanStart span === startPos .&&. spanEnd span === endPos

        , fastProperty "mergeSpans contains both original spans" $
            \span1 span2 ->
              let merged = mergeSpans span1 span2
              in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
                 spanEnd merged === max (spanEnd span1) (spanEnd span2)

        , fastProperty "mergeSpans is commutative" $
            \span1 span2 ->
              mergeSpans span1 span2 === mergeSpans span2 span1

        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 ->
              mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

        , fastProperty "isValidSpan checks start <= end" $
            \startPos endPos ->
              let span = spanBetween startPos endPos
              in isValidSpan span === (startPos <= endPos)
        ]

    , testGroup "Located properties"
        [ fastProperty "locatedAt creates correct located value" $
            \pos value ->
              let located = locatedAt pos value
              in locatedValue located === value .&&. locatedPos located === pos .&&. 
                 locatedSpan located === emptySpan pos

        , fastProperty "locatedWithSpan creates correct located value" $
            \span value ->
              let located = locatedWithSpan span value
              in locatedValue located === value .&&. locatedSpan located === span .&&.
                 locatedPos located === spanStart span

        , fastProperty "mapLocated preserves location" $
            \span value ->
              let located = locatedWithSpan span value
                  mapped = mapLocated (*2) located
              in locatedSpan mapped === locatedSpan located .&&.
                 locatedPos mapped === locatedPos located .&&.
                 locatedValue mapped === locatedValue located * 2

        , fastProperty "HasLocation instance works correctly" $
            \span value ->
              let located = locatedWithSpan span value
              in getLocation located === span
        ]

    , testGroup "LocationTracker properties"
        [ fastProperty "runLocationTracker starts at startPos" $
            runLocationTracker getCurrentPos === startPos

        , fastProperty "setCurrentPos and getCurrentPos are consistent" $
            \pos ->
              runLocationTracker (setCurrentPos pos >> getCurrentPos) === pos

        , fastProperty "LocationTracker state updates correctly" $
            \pos1 pos2 ->
              runLocationTracker (setCurrentPos pos1 >> setCurrentPos pos2 >> getCurrentPos) === pos2
        ]

    , testGroup "Text advancement properties"
        [ fastProperty "advancePos by empty text doesn't change position" $
            \pos ->
              advancePosByText T.empty pos === pos

        , fastProperty "advancePos by single character matches posAfter" $
            \pos ch ->
              advancePosByText (T.singleton ch) pos === posAfter ch pos

        , fastProperty "advancePosBy is consistent with advancePosByText" $
            \pos text ->
              advancePosByText text pos === advancePosBy (T.length text) pos

        , fastProperty "advancePosByLine increments line and resets column" $
            \pos lines ->
              let newPos = advancePosByLine lines pos
              in posLine newPos === posLine pos + lines .&&. posColumn newPos === 1

        , fastProperty "advancePosByText handles newlines correctly" $
            \pos text ->
              let lines = T.count (T.pack "\n") text
                  posAfterText = advancePosByText text pos
                  expectedLine = posLine pos + lines
              in posLine posAfterText === expectedLine

        , fastProperty "position advancement is monotonic" $
            \pos text ->
              let newPos = advancePosByText text pos
              in posOffset newPos >= posOffset pos
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "Position at line 1 column 1" $ do
            let pos = posAt 1 1
            posLine pos @?= 1
            posColumn pos @?= 1
            posOffset pos @?= 0

        , testCase "Empty span validity" $ do
            let pos = posAt 5 10
                span = emptySpan pos
            isValidSpan span @?= True
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "Span with invalid range" $ do
            let start = posAt 5 10
                end = posAt 3 15
                span = spanBetween start end
            isValidSpan span @?= False

        , testCase "Multi-line text advancement" $ do
            let text = T.pack "Hello\nWorld\nTest"
                start = startPos
                end = advancePosByText text start
            posLine end @?= 3
            posColumn end @?= 5

        , testCase "Tab handling in text advancement" $ do
            let text = T.pack "\tHello"
                start = posAt 1 1
                afterTab = advancePosByText text start
            posColumn afterTab @?= 9  -- Next tab stop

        , testCase "Complex text with mixed characters" $ do
            let text = T.pack "Hello\t\nWorld\tTest"
                start = startPos
                end = advancePosByText text start
            posLine end @?= 2
            posColumn end @?= 13  -- After tab alignment

        , testCase "Located value chain operations" $ do
            let pos1 = posAt 1 5
                pos2 = posAt 2 10
                span1 = emptySpan pos1
                span2 = emptySpan pos2
                value1 = 42
                value2 = "test"
                located1 = locatedAt pos1 value1
                located2 = locatedWithSpan span2 value2
                merged = mergeSpans (locatedSpan located1) (locatedSpan located2)
            spanStart merged @?= pos1
            spanEnd merged @?= pos2
        ]

    , testGroup "Performance and stress tests"
        [ fastProperty "Large text advancement performance" $
            \pos (Positive size) ->
              let largeText = T.replicate size "a"
                  endPos = advancePosByText largeText pos
              in posOffset endPos === posOffset pos + size

        , fastProperty "Deep span merging" $
            \pos (Positive n) ->
              let spans = replicate n (emptySpan pos)
                  merged = foldl mergeSpans (head spans) (tail spans)
              in merged === emptySpan pos

        , fastProperty "Complex located value transformations" $
            \span value ->
              let located = locatedWithSpan span value
                  transformed = mapLocated show . mapLocated (*2) $ located
              in locatedValue transformed === show (value * 2)
        ]
    ]

-- Helper function for conditional property
(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
_ <=> _ = False
