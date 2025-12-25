{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.SourceLocationInvariantSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof)
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan
  , advancePos, advancePosBy, advancePosByText
  )
import qualified Data.Text as T
import Data.Char (isSpace)

tests :: TestTree
tests = testGroup "Source Location Invariant Tests"
  [ testGroup "SourcePos invariants"
    [ testCase "startPos has correct initial values" $
        do
          posLine startPos @?= 1
          posColumn startPos @?= 1
          posOffset startPos @?= 0
    , testCase "posAfter newline updates line and column" $
        let pos = posAfter '\n' startPos
        in do
          posLine pos @?= 2
          posColumn pos @?= 1
          posOffset pos @?= 1
    , testCase "posAfter tab updates column to next tab stop" $
        let pos = posAfter '\t' (posAt 1 3)
        in do
          posLine pos @?= 1
          posColumn pos @?= 9  -- Next tab stop after column 3
    , testCase "posAfter regular char increments column" $
        let pos = posAfter 'a' startPos
        in do
          posLine pos @?= 1
          posColumn pos @?= 2
          posOffset pos @?= 1
    ]
  , testGroup "SourceSpan invariants"
    [ testCase "emptySpan has same start and end" $
        let span = emptySpan startPos
        in do
          spanStart span @?= spanEnd span
          assertBool "emptySpan should be valid" (isValidSpan span)
    , testCase "spanBetween creates valid span" $
        let start = posAt 1 5
            end = posAt 2 10
            span = spanBetween start end
        in do
          spanStart span @?= start
          spanEnd span @?= end
          assertBool "spanBetween should be valid" (isValidSpan span)
    , testCase "mergeSpans creates minimal covering span" $
        let span1 = spanBetween (posAt 1 5) (posAt 1 10)
            span2 = spanBetween (posAt 1 8) (posAt 1 15)
            merged = mergeSpans span1 span2
        in do
          spanStart merged @?= posAt 1 5
          spanEnd merged @?= posAt 1 15
    , testCase "mergeSpans handles non-overlapping spans" $
        let span1 = spanBetween (posAt 1 1) (posAt 1 5)
            span2 = spanBetween (posAt 2 1) (posAt 2 5)
            merged = mergeSpans span1 span2
        in do
          spanStart merged @?= posAt 1 1
          spanEnd merged @?= posAt 2 5
    ]
  , testGroup "Located values invariants"
    [ testCase "locatedAt creates value with empty span" $
        let value = "test"
            located = locatedAt startPos value
        in do
          locatedValue located @?= value
          locatedPos located @?= startPos
          locatedSpan located @?= emptySpan startPos
    , testCase "locatedWithSpan preserves span" $
        let value = "test"
            span = spanBetween (posAt 1 1) (posAt 1 5)
            located = locatedWithSpan span value
        in do
          locatedValue located @?= value
          locatedSpan located @?= span
    ]
  , testGroup "Position advancement invariants"
    [ testCase "advancePosBy empty string returns original position" $
        let pos = posAt 5 10
        in advancePosBy "" pos @?= pos
    , testCase "advancePosByText preserves Text length" $
        let text = T.pack "hello\nworld"
            start = posAt 1 1
            end = advancePosByText text start
        in posOffset end - posOffset start @?= T.length text
    , testCase "advancePosByText handles newlines correctly" $
        let text = T.pack "hi\nthere"
            start = posAt 1 1
            end = advancePosByText text start
        in do
          posLine end @?= 2
          posColumn end @?= 6  -- "there" length + 1
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "posAfter preserves monotonicity of offset" $
        \c pos -> posOffset (posAfter c pos) >= posOffset pos
    , testProperty "spanBetween always creates valid span" $
        \start end -> isValidSpan (spanBetween start end)
    , testProperty "mergeSpans is commutative" $
        \span1 span2 -> mergeSpans span1 span2 == mergeSpans span2 span1
    , testProperty "mergeSpans is idempotent" $
        \span -> mergeSpans span span == span
    , testProperty "advancePosBy is additive" $
        \s1 s2 pos -> advancePosBy (s1 ++ s2) pos == advancePosBy s2 (advancePosBy s1 pos)
    ]
  ]

-- Arbitrary instances for QuickCheck
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
    return $ SourceSpan start end