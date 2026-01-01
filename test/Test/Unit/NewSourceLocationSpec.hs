{-# LANGUAGE CPP #-}
module Test.Unit.NewSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

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
  , advancePosByText
  , advancePosByLine
  )
import Compiler.Errors.Core (ErrorLocation(..))

tests :: TestTree
tests =
  testGroup "New Source Location Tests"
    [ testCase "creates source position" $ do
        let pos = posAt 5 10
        posLine pos @?= 5
        posColumn pos @?= 10
        posOffset pos @?= 0

    , testCase "creates source position with offset" $ do
        let pos = posAtLineCol 3 7 42
        posLine pos @?= 3
        posColumn pos @?= 7
        posOffset pos @?= 42

    , testCase "advances position for regular character" $ do
        let pos = posAt 1 1
            newPos = posAfter 'a' pos
        posLine newPos @?= 1
        posColumn newPos @?= 2
        posOffset newPos @?= 1

    , testCase "advances position for newline" $ do
        let pos = posAt 1 5
            newPos = posAfter '\n' pos
        posLine newPos @?= 2
        posColumn newPos @?= 1
        posOffset newPos @?= 1

    , testCase "advances position for tab" $ do
        let pos = posAt 1 1
            newPos = posAfter '\t' pos
        posLine newPos @?= 1
        posColumn newPos @?= 9  -- Next tab stop (8-aligned)
        posOffset newPos @?= 1

    , testCase "advances position for tab at non-tab boundary" $ do
        let pos = posAt 1 5
            newPos = posAfter '\t' pos
        posLine newPos @?= 1
        posColumn newPos @?= 9  -- Next tab stop
        posOffset newPos @?= 1

    , testCase "creates empty span" $ do
        let span = emptySpan
        spanStart span @?= startPos
        spanEnd span @?= startPos

    , testCase "creates span from position" $ do
        let pos = posAt 3 7
            span = spanFrom pos
        spanStart span @?= pos
        spanEnd span @?= pos

    , testCase "creates span to position" $ do
        let start = posAt 2 4
            end = posAt 2 8
            span = spanTo end start
        spanStart span @?= start
        spanEnd span @?= end

    , testCase "creates span between positions" $ do
        let start = posAt 1 5
            end = posAt 3 2
            span = spanBetween start end
        spanStart span @?= start
        spanEnd span @?= end

    , testCase "merges spans" $ do
        let span1 = spanBetween (posAt 1 1) (posAt 1 5)
            span2 = spanBetween (posAt 2 3) (posAt 2 8)
            merged = mergeSpans span1 span2
        spanStart merged @?= spanStart span1
        spanEnd merged @?= spanEnd span2

    , testCase "validates spans" $ do
        let validSpan = spanBetween (posAt 1 1) (posAt 1 5)
            invalidSpan = spanBetween (posAt 2 5) (posAt 1 5)
        assertBool "valid span should be valid" (isValidSpan validSpan)
        assertBool "invalid span should not be valid" (not $ isValidSpan invalidSpan)

    , testCase "creates located values" $ do
        let pos = posAt 3 7
            located = locatedAt pos "test value"
        locatedValue located @?= "test value"
        locatedPos located @?= pos

    , testCase "creates located values with span" $ do
        let span = spanBetween (posAt 1 1) (posAt 1 10)
            located = locatedWithSpan span "test value"
        locatedValue located @?= "test value"
        locatedSpan located @?= span

    , testCase "maps located values" $ do
        let located = locatedAt (posAt 2 3) 42
            mapped = mapLocated (*2) located
        locatedValue mapped @?= 84
        locatedPos mapped @?= locatedPos located

    , testCase "uses location tracker" $ do
        let result = runLocationTracker $ do
                setCurrentPos (posAt 1 5)
                getCurrentPos
        result @?= posAt 1 5

    , testCase "tracks span with location tracker" $ do
        let result = runLocationTracker $ do
                setCurrentPos (posAt 1 1)
                markSpanStart
                setCurrentPos (posAt 1 10)
                markSpanEnd
                getCurrentPos
        result @?= posAt 1 10

    , testCase "uses withLocationTracking" $ do
        let result = withLocationTracking $ do
                return "tracked value"
        case result of
          (value, span) -> do
            value @?= "tracked value"
            assertBool "span should be valid" (isValidSpan span)

    , testCase "converts to error location" $ do
        let pos = posAt 5 10
            errorLoc = toErrorLocation pos
        line errorLoc @?= 5
        column errorLoc @?= 10

    , testCase "converts span to error location" $ do
        let span = spanBetween (posAt 3 5) (posAt 3 15)
            errorLoc = toErrorLocationWithSpan span
        line errorLoc @?= 3
        column errorLoc @?= 5

    , testCase "advances position by character" $ do
        let pos = posAt 1 1
            newPos = advancePos 'a' pos
        posLine newPos @?= 1
        posColumn newPos @?= 2
        posOffset newPos @?= 1

    , testCase "advances position by multiple characters" $ do
        let pos = posAt 1 1
            newPos = advancePosBy "hello" pos
        posLine newPos @?= 1
        posColumn newPos @?= 6
        posOffset newPos @?= 5

    , testCase "advances position by text with newlines" $ do
        let pos = posAt 1 1
            text = "hello\nworld"
            newPos = advancePosByText text pos
        posLine newPos @?= 2
        posColumn newPos @?= 6
        posOffset newPos @?= 11

    , testCase "advances position by line" $ do
        let pos = posAt 1 5
            newPos = advancePosByLine pos
        posLine newPos @?= 2
        posColumn newPos @?= 1
        posOffset newPos @?= 1

    , testCase "handles complex text advancement" $ do
        let pos = posAt 1 1
            text = "line1\n\tline2\nline3"
            newPos = advancePosByText text pos
        posLine newPos @?= 3
        posColumn newPos @?= 6
        posOffset newPos @?= L.length text

    , testCase "creates nested located values" $ do
        let inner = locatedAt (posAt 2 3) "inner"
            outer = locatedAt (posAt 1 1) inner
        locatedValue outer @?= inner
        locatedPos outer @?= posAt 1 1

    , testCase "handles large spans" $ do
        let start = posAt 1 1
            end = posAt 1000 50
            largeSpan = spanBetween start end
        spanStart largeSpan @?= start
        spanEnd largeSpan @?= end
        assertBool "large span should be valid" (isValidSpan largeSpan)

    , testCase "compares positions correctly" $ do
        let pos1 = posAt 1 1
            pos2 = posAt 1 2
            pos3 = posAt 2 1
        assertBool "same line, greater column is greater" (pos2 > pos1)
        assertBool "greater line is greater regardless of column" (pos3 > pos2)
        assertBool "same positions are equal" (pos1 == posAt 1 1)

    , testCase "handles edge cases in position advancement" $ do
        let pos = posAt 1 8
            afterTab = posAfter '\t' pos
        posLine afterTab @?= 1
        posColumn afterTab @?= 9  -- Should jump to next tab stop
        posOffset afterTab @?= 1

    , testCase "creates spans with same start L.and end" $ do
        let pos = posAt 5 10
            span = spanBetween pos pos
        spanStart span @?= pos
        spanEnd span @?= pos
        assertBool "zero-L.length span is valid" (isValidSpan span)

    , testCase "converts located value to error location" $ do
        let located = locatedAt (posAt 7 15) "test"
            errorLoc = toErrorLocation (locatedPos located)
        line errorLoc @?= 7
        column errorLoc @?= 15

    , testCase "handles unicode text advancement" $ do
        let pos = posAt 1 1
            text = "héllo"  -- Contains unicode character
            newPos = advancePosByText text pos
        posLine newPos @?= 1
        posColumn newPos @?= 6  -- Should count characters, not bytes
        posOffset newPos @?= L.length text
    ]