{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SourceLocationMathPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    , Positive(..), NonNegative(..)
    )

import SourceLocation
    ( SourcePos(..), SourceSpan(..), Located(..)
    , startPos, posAfter, posAt, posAtLineCol
    , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
    , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos
    , advancePos, advancePosBy
    )

import Data.List (sort)

-- | QuickCheck property tests for SourceLocation mathematical properties
tests :: TestTree
tests =
  testGroup "SourceLocation Mathematical Properties QuickCheck Tests"
    [ testGroup "SourcePos Properties"
        [ fastProperty "position advancement is monotonic" $
            \pos (Positive n) ->
              let advanced = advancePosBy pos n
              in advancedLine pos advanced >= sourceLine pos .&&.
                 advancedLine advanced advanced >= sourceLine advanced pos advanced
              
        , fastProperty "position advancement preserves line monotonicity" $
            \pos (Positive n) ->
              let advanced = advancePosBy pos n
              in sourceLine advanced >= sourceLine pos
              
        , fastProperty "position advancement is cumulative" $
            \pos (Positive n) (Positive m) ->
              let step1 = advancePosBy pos n
                  step2 = advancePosBy step1 m
                  direct = advancePosBy pos (n + m)
              in sourceLine step2 === sourceLine direct .&&.
                 sourceColumn step2 === sourceColumn direct
              
        , fastProperty "start position is minimal" $
            \pos ->
              sourceLine (startPos) <= sourceLine pos .&&.
             (sourceLine (startPos) == sourceLine pos ==> sourceColumn (startPos) <= sourceColumn pos)
        ]

    , testGroup "SourceSpan Properties"
        [ fastProperty "empty span has zero L.length" $
            \pos ->
              let span = emptySpan pos
              in spanStart span === spanEnd span
              
        , fastProperty "span merge is commutative" $
            \span1 span2 ->
              let merged1 = mergeSpans span1 span2
                  merged2 = mergeSpans span2 span1
              in spanStart merged1 === spanStart merged2 .&&.
                 spanEnd merged1 === spanEnd merged2
              
        , fastProperty "span merge is associative" $
            \span1 span2 span3 ->
              let merged1 = mergeSpans (mergeSpans span1 span2) span3
                  merged2 = mergeSpans span1 (mergeSpans span2 span3)
              in spanStart merged1 === spanStart merged2 .&&.
                 spanEnd merged1 === spanEnd merged2
              
        , fastProperty "span merge contains L.all original spans" $
            \span1 span2 ->
              let merged = mergeSpans span1 span2
                  start1 = spanStart span1
                  end1 = spanEnd span1
                  start2 = spanStart span2
                  end2 = spanEnd span2
                  mergedStart = spanStart merged
                  mergedEnd = spanEnd merged
              in posLE mergedStart start1 .&&. posLE end1 mergedEnd .&&.
                 posLE mergedStart start2 .&&. posLE end2 mergedEnd
              
        , fastProperty "span between creates valid span" $
            \pos1 pos2 ->
              let span = spanBetween pos1 pos2
              in isValidSpan span
        ]

    , testGroup "Located Value Properties"
        [ fastProperty "located value preserves original value" $
            \value pos ->
              let located = locatedAt pos value
              in locatedValue located === value
              
        , fastProperty "located span contains position" $
            \value pos ->
              let located = locatedAt pos value
                  span = locatedSpan located
              in spanStart span === pos .&&. spanEnd span === pos
              
        , fastProperty "map located preserves location" $
            \value pos f ->
              let located = locatedAt pos value
                  mapped = fmap f located
              in locatedSpan mapped === locatedSpan located
        ]

    , testGroup "Position Comparison Properties"
        [ fastProperty "position comparison is transitive" $
            \pos1 pos2 pos3 ->
              let le12 = posLE pos1 pos2
                  le23 = posLE pos2 pos3
                  le13 = posLE pos1 pos3
              in (le12 .&&. le23) ==> le13
              
        , fastProperty "position comparison is reflexive" $
            \pos ->
              posLE pos pos
              
        , fastProperty "position comparison is antisymmetric" $
            \pos1 pos2 ->
              let le12 = posLE pos1 pos2
                  le21 = posLE pos2 pos1
              in (le12 .&&. le21) ==> (pos1 === pos2)
        ]

    , testGroup "Position Arithmetic Properties"
        [ fastProperty "position advancement by zero is identity" $
            \pos ->
              advancePosBy pos 0 === pos
              
        , fastProperty "position advancement preserves ordering" $
            \pos1 pos2 (Positive n) ->
              posLE pos1 pos2 ==>
              let advanced1 = advancePosBy pos1 n
                  advanced2 = advancePosBy pos2 n
              in posLE advanced1 advanced2
              
        , fastProperty "position advancement is injective for positive steps" $
            \pos1 pos2 (Positive n) ->
              pos1 /= pos2 ==> 
              let advanced1 = advancePosBy pos1 n
                  advanced2 = advancePosBy pos2 n
              in advanced1 /= advanced2
        ]

    , testGroup "Span Construction Properties"
        [ fastProperty "span from position is valid" $
            \pos ->
              let span = spanFrom pos
              in isValidSpan span .&&. spanStart span === pos
              
        , fastProperty "span to position is valid" $
            \pos ->
              let span = spanTo pos
              in isValidSpan span .&&. spanEnd span === pos
              
        , fastProperty "span between ordered positions is valid" $
            \pos1 pos2 ->
              let span = spanBetween pos1 pos2
                  start = spanStart span
                  end = spanEnd span
              in posLE start end
        ]

    , testGroup "Location Tracking Properties"
        [ fastProperty "position advancement respects newlines" $
            \pos (NonNegative lineCount) ->
              let advanced = advancePosBy pos (lineCount * 80) -- Assume 80 chars per line
                  expectedLine = sourceLine pos + lineCount
              in sourceLine advanced >= expectedLine
              
        , fastProperty "position at line column is consistent" $
            \line (NonNegative col) ->
              line > 0 ==> 
              let pos = posAtLineCol line col
              in sourceLine pos === line .&&. sourceColumn pos === col + 1
        ]
    ]

-- Helper function to compare positions
posLE :: SourcePos -> SourcePos -> Bool
posLE pos1 pos2
  | sourceLine pos1 < sourceLine pos2 = True
  | sourceLine pos1 > sourceLine pos2 = False
  | otherwise = sourceColumn pos1 <= sourceColumn pos2

-- Helper function to get line after advancement
advancedLine :: SourcePos -> SourcePos -> Int
advancedLine original advanced = sourceLine advanced - sourceLine original