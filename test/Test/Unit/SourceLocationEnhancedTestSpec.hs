module Test.Unit.SourceLocationEnhancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
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
  , advancePos
  , advancePosByText
  )

import Data.Text (Text)
import qualified Data.Text as T

-- | Enhanced unit tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Enhanced Tests"
    [ testGroup "Source position operations"
        [ testCase "posAfter handles tab stops correctly" $ do
            let start = posAt 1 1
            let afterTab = posAfter '\t' start
            posColumn afterTab @?= 9  -- Next tab stop (8-char alignment)

        , testCase "posAfter handles multiple tabs" $ do
            let start = posAt 1 5
            let afterTab = posAfter '\t' start
            posColumn afterTab @?= 9  -- Next tab stop

        , testCase "posAfter handles newline correctly" $ do
            let start = posAt 3 10
            let afterNewline = posAfter '\n' start
            posLine afterNewline @?= 4
            posColumn afterNewline @?= 1

        , testCase "posAfter handles regular characters" $ do
            let start = posAt 2 5
            let afterChar = posAfter 'a' start
            posLine afterChar @?= 2
            posColumn afterChar @?= 6

        , testCase "posAtLineCol creates positions correctly" $ do
            let pos = posAtLineCol 10 20 100
            posLine pos @?= 10
            posColumn pos @?= 20
            posOffset pos @?= 100
        ]

    , testGroup "Source span operations"
        [ testCase "emptySpan has invalid span" $ do
            isValidSpan emptySpan @?= False

        , testCase "spanFrom creates valid span" $ do
            let pos = posAt 1 1
            let span = spanFrom pos
            isValidSpan span @?= True
            spanStart span @?= pos

        , testCase "spanBetween calculates correct span" $ do
            let start = posAt 1 1
            let end = posAt 2 5
            let span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

        , testCase "mergeSpans combines spans correctly" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
            let span2 = spanBetween (posAt 2 1) (posAt 2 20)
            let merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2
        ]

    , testGroup "Located value operations"
        [ testCase "locatedAt creates located values" $ do
            let pos = posAt 3 7
            let value = "test"
            let located = locatedAt pos value
            locatedValue located @?= value
            locatedPos located @?= pos

        , testCase "locatedWithSpan creates spanned values" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 5)
            let value = "hello"
            let located = locatedWithSpan span value
            locatedValue located @?= value
            locatedSpan located @?= span

        , testCase "mapLocated transforms values preserving location" $ do
            let pos = posAt 2 3
            let located = locatedAt pos 42
            let transformed = mapLocated (*2) located
            locatedValue transformed @?= 84
            locatedPos transformed @?= pos
        ]

    , testGroup "Text position tracking"
        [ testCase "advancePosByText handles simple text" $ do
            let start = startPos
            let text = "hello"
            let endPos = advancePosByText start text
            posLine endPos @?= 1
            posColumn endPos @?= 6

        , testCase "advancePosByText handles newlines" $ do
            let start = startPos
            let text = "hello\nworld"
            let endPos = advancePosByText start text
            posLine endPos @?= 2
            posColumn endPos @?= 6

        , testCase "advancePosByText handles tabs" $ do
            let start = posAt 1 5
            let text = "\t"
            let endPos = advancePosByText start text
            posColumn endPos @?= 9  -- Next tab stop

        , testCase "advancePosByText handles mixed content" $ do
            let start = startPos
            let text = "hi\tthere\nworld"
            let endPos = advancePosByText start text
            posLine endPos @?= 2
            posColumn endPos @?= 6
        ]

    , testGroup "Edge cases L.and error conditions"
        [ testCase "spanBetween handles same start L.and end" $ do
            let pos = posAt 1 1
            let span = spanBetween pos pos
            spanStart span @?= pos
            spanEnd span @?= pos
            isValidSpan span @?= True

        , testCase "mergeSpans handles identical spans" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 10)
            let merged = mergeSpans span span
            merged @?= span

        , testCase "advancePosByText handles empty text" $ do
            let start = posAt 5 10
            let endPos = advancePosByText start ""
            endPos @?= start

        , testCase "locatedAt handles different value types" $ do
            let intLocated = locatedAt (posAt 1 1) 42
            let stringLocated = locatedAt (posAt 2 2) "test"
            locatedValue intLocated @?= 42
            locatedValue stringLocated @?= "test"
        ]
    ]