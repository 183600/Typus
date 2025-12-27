{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewCabalCoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.Text as T
import Data.Char (isSpace, isDigit, isLetter)
import qualified Data.List as L

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , emptySpan
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , mapLocated
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | Core functionality tests for essential Typus compiler components
tests :: TestTree
tests =
  testGroup "NewCabalCoreFunctionality"
    [ testGroup "SourceLocation core operations"
        [ testCase "position advancement with mixed characters" $ do
            let initial = SourcePos 1 1 0
                advanced = advancePosBy "Hello\tWorld\nTest" initial
            advanced @?= SourcePos 2 6 17

        , testCase "span merging with overlapping ranges" $ do
            let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
                span2 = SourceSpan (SourcePos 1 5 4) (SourcePos 1 15 14)
                merged = mergeSpans span1 span2
            merged @?= SourceSpan (SourcePos 1 1 0) (SourcePos 1 15 14)

        , testCase "located value transformation preserves location" $ do
            let pos = SourcePos 3 7 20
                original = locatedAt pos [1,2,3]
                transformed = mapLocated (map (*2)) original
            locatedValue transformed @?= [2,4,6]
            locatedPos transformed @?= pos

        , testCase "error location conversion with span ranges" $ do
            let span = SourceSpan (SourcePos 2 3 10) (SourcePos 4 7 30)
                errLoc = toErrorLocationWithSpan span
            line errLoc @?= 2
            column errLoc @?= 3
            endLine errLoc @?= Just 4
            endColumn errLoc @?= Just 7
        ]

    , testGroup "Utils string processing"
        [ testCase "complex trim with mixed whitespace" $ do
            let input = "\t\n  Hello World  \r\n\t"
                trimmed = trim input
            trimmed @?= "Hello World"

        , testCase "splitBy with Unicode content" $ do
            let input = "测试,Hello,世界,World"
                parts = splitBy ',' input
            parts @?= ["测试", "Hello", "世界", "World"]

        , testCase "splitByCollapsed removes empty segments" $ do
            let input = "a,,,b,,c"
                parts = splitByCollapsed ',' input
            parts @?= ["a", "b", "c"]

        , testCase "removeComments handles complex string literals" $ do
            let input = "var s = \"// not a comment\"; /* real comment */ var x = 42;"
                cleaned = removeComments input
            cleaned @?= "var s = \"// not a comment\";  var x = 42;"

        , testCase "normalizeIndentation preserves relative structure" $ do
            let input = "    line1\n      line2\n    line3"
                normalized = normalizeIndentation input
                lines' = lines normalized
            lines' @?= ["line1", "  line2", "line3"]

        , testCase "breakOn with multiple occurrences" $ do
            let input = "prefix-middle-suffix-end"
                (before, after) = breakOn "-" input
            before @?= "prefix"
            after @?= "middle-suffix-end"
        ]

    , testGroup "Edge case handling"
        [ testCase "empty string processing" $ do
            trim "" @?= ""
            splitBy ',' "" @?= [""]
            splitByCollapsed ',' "" @?= []

        , testCase "single character inputs" $ do
            splitBy 'a' "a" @?= ["", ""]
            splitByCollapsed 'a' "a" @?= []
            trim "a" @?= "a"

        , testCase "position at file boundaries" $ do
            let pos = advancePosBy "single line" startPos
            pos @?= SourcePos 1 12 11

        , testCase "span with same start and end" $ do
            let pos = SourcePos 5 3 15
                span = emptySpan pos
            isValidSpan span @?= True
            spanStart span @?= pos
            spanEnd span @?= pos
        ]

    , testGroup "Performance-critical operations"
        [ testCase "large text processing" $ do
            let largeText = T.unlines $ replicate 1000 "line with some content"
                finalPos = advancePosByText largeText startPos
            posLine finalPos @?= 1001
            posColumn finalPos @?= 1

        , testCase "repeated span merging" $ do
            let spans = [SourceSpan (SourcePos 1 i (i-1)) (SourcePos 1 (i+5) (i+4)) | i <- [1,10..100]]
                merged = foldl1 mergeSpans spans
            spanStart merged @?= SourcePos 1 1 0
            spanEnd merged @?= SourcePos 1 105 104
        ]

    , testGroup "Data integrity validation"
        [ testCase "position consistency after operations" $ do
            let pos1 = advancePosBy "hello" startPos
                pos2 = advancePosBy "world" pos1
                span = spanBetween startPos pos2
            spanStart span @?= startPos
            spanEnd span @?= pos2

        , testCase "string processing roundtrip" $ do
            let original = "  test content  "
                trimmed = trim original
                restored = "  " ++ trimmed ++ "  "
            restored @?= original

        , testCase "split and join consistency" $ do
            let input = "a,b,c,d"
                parts = splitBy ',' input
                rejoined = L.intercalate "," parts
            rejoined @?= input
        ]
    ]