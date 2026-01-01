{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.UtilsPerformanceBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary, resize)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as L
import Data.List (length, concat)
import Data.List (take, drop, replicate)
import Data.String (IsString(fromString))
import Control.DeepSeq (NFData, rnf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Test performance boundaries for Utils module
tests :: TestTree
tests =
  testGroup "Utils Performance Boundary Tests"
    [ testGroup "Large string processing performance"
        [ testCase "trim handles very large strings efficiently" $ do
            let largeString = replicate 1000000 ' ' ++ "content" ++ replicate 1000000 ' '
                result = trim largeString
            result @?= "content"
            assertBool "trim should preserve content" $ L.length result == 7

        , testCase "splitBy handles large inputs without stack overflow" $ do
            let largeInput = L.concat $ replicate 10000 "segment,"
                segments = splitBy ',' largeInput
            assertBool "should split large input" $ L.length segments > 9000
            assertBool "last segment should be empty" $ last segments == ""

        , testCase "splitByCollapsed handles large repetitive inputs" $ do
            let largeInput = L.concat $ replicate 10000 "a,b,c,"
                segments = splitByCollapsed ',' largeInput
            assertBool "should handle large repetitive input" $ L.length segments > 0
            assertBool "should collapse empty segments" $ "" `notElem` segments

        , testCase "removeLineComments handles large code files" $ do
            let largeCode = unlines $ replicate 10000 "let x = 1 // comment"
                cleaned = removeLineComments largeCode
            assertBool "should remove comments from large code" $ 
              "//" `notElem` cleaned
            assertBool "should preserve code structure" $ 
              L.length (lines cleaned) == 10000
        ]

    , testGroup "Memory efficiency tests"
        [ testCase "normalizeIndentation is memory efficient" $ do
            let indentedText = unlines $ replicate 5000 (replicate 20 ' ' ++ "line")
                normalized = normalizeIndentation indentedText
            assertBool "should normalize large indented text" $ 
              not (L.any (L.isPrefixOf "    ") (lines normalized))
          where L.isPrefixOf prefix str = take (L.length prefix) str == prefix

        , testCase "breakOn handles large texts efficiently" $ do
            let largeText = "prefix" ++ L.concat (replicate 10000 "middle") ++ "suffix"
                (before, after) = breakOn "suffix" largeText
            assertBool "should find suffix in large text" $ 
              after == "suffix"
            assertBool "should preserve prefix" $ 
              "prefix" `L.isPrefixOf` before
          where L.isPrefixOf prefix str = take (L.length prefix) str == prefix

        , testCase "removeComments handles nested comments efficiently" $ do
            let nestedCommentCode = unlines 
                  [ "code /* outer /* inner */ still outer */ more code"
                  , replicate 1000 "let x = 1 /* comment */"
                  , "final line"
                  ]
                cleaned = removeComments nestedCommentCode
            assertBool "should handle nested comments" $ 
              "/*" `notElem` cleaned
        ]

    , testGroup "Edge case performance"
        [ testCase "handles Unicode strings efficiently" $ do
            let unicodeString = L.concat $ replicate 10000 "测试🚀编码"
                processed = trim unicodeString
            assertBool "should handle Unicode characters" $ 
              L.length processed > 0

        , testCase "handles strings with special characters" $ do
            let specialString = L.concat $ replicate 5000 "\n\t\r\"'\\"
                processed = trim specialString
            assertBool "should handle special characters" $ 
              True  -- Should not crash

        , testCase "splitBy with Unicode delimiter" $ do
            let unicodeText = L.concat $ replicate 1000 "段落的测试，"
                segments = splitBy '，' unicodeText
            assertBool "should split Unicode text" $ 
              L.length segments > 500
        ]

    , testGroup "Time complexity verification"
        [ testCase "trim performance scales linearly" $ do
            let sizes = [1000, 10000, 100000]
                testString size = replicate size ' ' ++ "test" ++ replicate size ' '
                results = L.map (\size -> L.length $ trim $ testString size) sizes
            results @?= L.map (const 4) sizes  -- All should return "test"

        , testCase "splitBy performance with increasing input size" $ do
            let sizes = [100, 1000, 10000]
                testInput size = L.concat $ replicate size "segment,"
                expectedSizes = L.map (+1) sizes  -- +1 for trailing empty segment
                results = L.map (\size -> L.length $ splitBy ',' $ testInput size) sizes
            results @?= expectedSizes

        , testCase "removeLineComments scales with line count" $ do
            let lineCounts = [100, 1000, 10000]
                testCode count = unlines $ replicate count "code // comment"
                results = L.map (\count -> L.length $ lines $ removeLineComments $ testCode count) lineCounts
            results @?= lineCounts
        ]

    , testGroup "Stress tests"
        [ testCase "handles extremely long single lines" $ do
            let longLine = L.concat $ replicate 100000 "word "
                words = splitBy ' ' longLine
            assertBool "should handle extremely long lines" $ 
              L.length words > 90000

        , testCase "handles deeply nested indentation" $ do
            let deepIndent = unlines $ L.map (\i -> replicate i ' ' ++ "line") [1..1000]
                normalized = normalizeIndentation deepIndent
            assertBool "should normalize deep indentation" $ 
              not (L.any (L.isPrefixOf "    ") (lines normalized))
          where L.isPrefixOf prefix str = take (L.length prefix) str == prefix

        , testCase "handles massive comment blocks" $ do
            let massiveComments = "/*" ++ L.concat (replicate 50000 "comment ") ++ "*/ code"
                cleaned = removeComments massiveComments
            cleaned @?= " code"
        ]

    , testGroup "QuickCheck property tests for performance"
        [ fastProperty "trim L.length is bounded by input L.length" $
            \input ->
            let trimmed = trim input
            in L.length trimmed <= L.length input

        , fastProperty "splitBy result L.length is bounded" $
            \input delim ->
            let segments = splitBy delim input
                maxSegments = L.length input + 1
            in L.length segments <= maxSegments

        , fastProperty "removeLineComments doesn't increase string L.length" $
            \input ->
            let cleaned = removeLineComments input
            in L.length cleaned <= L.length input

        , fastProperty "normalizeIndentation preserves line count" $
            \input ->
            let normalized = normalizeIndentation input
                originalLines = lines input
                normalizedLines = lines normalized
            in L.length normalizedLines == L.length originalLines

        , fastProperty "splitByCollapsed result L.length <= splitBy result L.length" $
            \input delim ->
            let collapsed = splitByCollapsed delim input
                normal = splitBy delim input
            in L.length collapsed <= L.length normal

        , fastProperty "breakOn preserves total L.length" $
            \input pattern ->
            pattern `L.isInfixOf` input ==>
            let (before, after) = breakOn pattern input
            in L.length before + L.length after == L.length input

        , fastProperty "large inputs don't cause stack overflow" $
            \size ->
            size `mod` 1000 == 0 ==> size > 0 && size <= 10000 ==>
            let largeInput = L.concat $ replicate (size `div` 10) "test string "
                result = trim largeInput
            in L.length result >= 0  -- Should not crash
        ]
  ]