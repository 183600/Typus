{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SourceLocationBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements)

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
  , advancePosBy
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Int (Int32, Int64)

-- | Test boundary conditions for SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Boundary Tests"
    [ testGroup "Position boundary tests"
        [ testCase "startPos should be (1,1)" $
            startPos @?= SourcePos 1 1

        , testCase "posAfter handles line overflow" $ do
            let pos = SourcePos 1 1000000
                result = posAfter pos '\n'
            assertBool "posAfter should handle large column numbers" $
              case result of
                SourcePos line col -> line > 1 && col == 1

        , testCase "posAtLineCol handles extreme values" $ do
            let pos1 = posAtLineCol 1 1
                pos2 = posAtLineCol 999999 999999
                pos3 = posAtLineCol (-1) (-1)
            pos1 @?= SourcePos 1 1
            pos2 @?= SourcePos 999999 999999
            pos3 @?= SourcePos (-1) (-1)

        , fastProperty "advancePosBy with zero count returns same position" $
            \pos -> advancePosBy pos 0 === pos

        , fastProperty "advancePosBy with positive count increases position" $
            \pos count -> count >= 0 ==> 
            let newPos = advancePosBy pos count
                SourcePos newLine newCol = newPos
                SourcePos oldLine oldCol = pos
            in (newLine > oldLine) || (newLine == oldLine && newCol >= oldCol)
        ]

    , testGroup "Span boundary tests"
        [ testCase "emptySpan should be valid but empty" $ do
            let span = emptySpan
            assertBool "emptySpan should be valid" $ isValidSpan span
            let SourcePos startLine startCol = spanStart span
            let SourcePos endLine endCol = spanEnd span
            (startLine, startCol) @?= (endLine, endCol)

        , testCase "spanFrom creates valid single-character span" $ do
            let pos = SourcePos 5 10
                span = spanFrom pos
            assertBool "spanFrom should create valid span" $ isValidSpan span
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "mergeSpans handles extreme positions" $ do
            let span1 = SourceSpan (SourcePos 1 1) (SourcePos 1000000 1000000)
                span2 = SourceSpan (SourcePos 500000 500000) (SourcePos 2000000 2000000)
                merged = mergeSpans span1 span2
            spanStart merged @?= SourcePos 1 1
            spanEnd merged @?= SourcePos 2000000 2000000

        , fastProperty "mergeSpans is commutative" $
            \span1 span2 -> 
            let merged1 = mergeSpans span1 span2
                merged2 = mergeSpans span2 span1
            in merged1 === merged2

        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 ->
            let merged1 = mergeSpans (mergeSpans span1 span2) span3
                merged2 = mergeSpans span1 (mergeSpans span2 span3)
            in merged1 === merged2
        ]

    , testGroup "Located value boundary tests"
        [ testCase "locatedAt creates proper location" $ do
            let pos = SourcePos 10 20
                value = "test"
                located = locatedAt pos value
            locatedPos located @?= pos
            locatedValue located @?= value

        , testCase "mapLocated preserves location" $ do
            let pos = SourcePos 5 15
                original = locatedAt pos 42
                transformed = mapLocated (*2) original
            locatedPos transformed @?= locatedPos original
            locatedValue transformed @?= 84

        , fastProperty "mapLocated with id returns original" $
            \located -> mapLocated id located === located

        , fastProperty "mapLocated composition works" $
            \located f g ->
            let composed = mapLocated (f . g) located
                separate = mapLocated f (mapLocated g located)
            in composed === separate
        ]

    , testGroup "Error location boundary tests"
        [ testCase "toErrorLocation handles extreme positions" $ do
            let pos = SourcePos 999999 999999
                errLoc = toErrorLocation pos
            assertBool "toErrorLocation should handle extreme positions" $
              not (null errLoc)

        , testCase "toErrorLocationWithSpan handles large spans" $ do
            let span = SourceSpan (SourcePos 1 1) (SourcePos 1000000 1000000)
                errLoc = toErrorLocationWithSpan span
            assertBool "toErrorLocationWithSpan should handle large spans" $
              not (null errLoc)

        , fastProperty "toErrorLocation is deterministic" $
            \pos -> toErrorLocation pos === toErrorLocation pos

        , fastProperty "toErrorLocationWithSpan is deterministic" $
            \span -> toErrorLocationWithSpan span === toErrorLocationWithSpan span
        ]

    , testGroup "QuickCheck property tests for boundary conditions"
        [ fastProperty "isValidSpan correctly identifies invalid spans" $
            \startLine startCol endLine endCol ->
            let start = SourcePos startLine startCol
                end = SourcePos endLine endCol
                span = SourceSpan start end
                shouldBeValid = (startLine < endLine) || 
                               (startLine == endLine && startCol <= endCol)
            in isValidSpan span === shouldBeValid

        , fastProperty "spanBetween contains both endpoints" $
            \pos1 pos2 ->
            let span = spanBetween pos1 pos2
                start = spanStart span
                end = spanEnd span
            in (start == pos1 || start == pos2) && (end == pos1 || end == pos2)

        , fastProperty "advancePos preserves ordering for same character" $
            \pos ->
            let advanced = advancePos pos 'a'
                SourcePos line1 col1 = pos
                SourcePos line2 col2 = advanced
            in (line2 > line1) || (line2 == line1 && col2 >= col1)

        , fastProperty "locatedWithSpan creates consistent located values" $
            \span value ->
            let located = locatedWithSpan span value
            in locatedSpan located === span && locatedValue located === value
        ]
  ]