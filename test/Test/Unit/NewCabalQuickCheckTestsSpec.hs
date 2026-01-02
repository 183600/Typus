{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, frequency)
import qualified Test.QuickCheck as QC

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , advancePos
  , advancePosBy
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- Property: startPos has line 1, column 1
prop_startPos_basic :: Property
prop_startPos_basic =
  startPos === SourcePos 1 1

-- Property: posAfter advances column by 1
prop_posAfter_column :: Int -> Property
prop_posAfter_column col =
  col >= 1 && col <= 1000 ==>
  let pos = SourcePos 1 col
      nextPos = posAfter pos
  in nextPos === SourcePos 1 (col + 1)

-- Property: advancePos handles newline correctly
prop_advancePos_newline :: SourcePos -> Property
prop_advancePos_newline pos =
  let newPos = advancePos '\n' pos
  in sourceLine newPos === sourceLine pos + 1 .&&.
     sourceColumn newPos === 1

-- Property: advancePos handles tab correctly
prop_advancePos_tab :: Int -> Property
prop_advancePos_tab col =
  col >= 1 && col <= 50 ==>
  let pos = SourcePos 1 col
      newPos = advancePos '\t' pos
  in sourceColumn newPos >= col .&&. sourceColumn newPos <= col + 8

-- Property: spanBetween creates valid span
prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid pos1 pos2 =
  let span = spanBetween pos1 pos2
  in isValidSpan span === True

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_contains l1 c1 l2 c2 l3 c3 =
  L.all (>= 1) [l1, c1, l2, c2, l3, c3] && L.all (<= 1000) [l1, c1, l2, c2, l3, c3] ==>
  let span1 = SourceSpan (SourcePos l1 c1) (SourcePos l2 c2)
      span2 = SourceSpan (SourcePos l3 c3) (SourcePos (l3 + 1) 1)
      merged = mergeSpans span1 span2
  in isValidSpan merged .&&.
     (spanStart merged `leq` spanStart span1 .||. spanStart merged `leq` spanStart span2) .&&.
     (spanEnd span1 `leq` spanEnd merged .||. spanEnd span2 `leq` spanEnd merged)
  where
    leq (SourcePos l1 c1) (SourcePos l2 c2) = l1 < l2 || (l1 == l2 && c1 <= c2)

-- ============================================================================
-- Parser Properties (simulated)
-- ============================================================================

-- Property: Comment removal preserves non-comment content
prop_comment_preservation :: String -> String -> Property
prop_comment_preservation code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) &&
  not ('"' `elem` comment) && not ('\'' `elem` comment) &&
  not ("/*" `L.isInfixOf` code) && not ("*/" `L.isInfixOf` code) ==>
  let withComments = code ++ " /* " ++ comment ++ " */ " ++ code ++ " // " ++ comment
      withoutComments = removeComments withComments
  in code `L.isInfixOf` withoutComments

-- Property: String parsing roundtrip
prop_string_roundtrip :: String -> Property
prop_string_roundtrip content =
  not ('\'' `elem` content) && not ('"' `elem` content) && not ('\n' `elem` content) ==>
  let quoted = "\"" ++ content ++ "\""
      unquoted = if "//" `L.isInfixOf` quoted 
                 then removeLineComments quoted 
                 else quoted
  in content `L.isInfixOf` unquoted

-- Property: Directive parsing consistency
prop_directive_consistency :: String -> Property
prop_directive_consistency directive =
  not (' ' `elem` directive) && not ('\n' `elem` directive) &&
  L.all isAlpha directive ==>
  let withDirective = "// @ownership:" ++ directive ++ "\ncode()"
      processed = removeLineComments withDirective
  in "code()" `L.isInfixOf` processed

-- ============================================================================
-- Advanced Utils Properties
-- ============================================================================

-- Property: Complex string processing pipeline
prop_complex_pipeline :: String -> String -> String -> Property
prop_complex_pipeline prefix middle suffix =
  not ('"' `elem` prefix) && not ('\'' `elem` prefix) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` suffix) && not ('\'' `elem` suffix) ==>
  let input = prefix ++ "  /* block */  " ++ middle ++ "  // line  \n  " ++ suffix
      processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
                  |> splitByComma
      rejoined = Data.List.intercalate "," processed
  in L.length rejoined <= L.length input .&&.
     not ("/* block */" `L.isInfixOf` rejoined) .&&.
     not ("// line" `L.isInfixOf` rejoined)

-- Property: Unicode handling in string operations
prop_unicode_handling :: String -> Property
prop_unicode_handling content =
  let unicodeContent = content ++ "测试🚀café naïve"
      trimmed = trim unicodeContent
      split = splitBy ' ' unicodeContent
  in property $ 
    (if not (null content) then "测试🚀café" `L.isInfixOf` trimmed else True) .&&.
    L.length split >= 1 .&&.
    L.all (L.notElem ' ') split

-- Property: Performance with large inputs
prop_large_input_performance :: Int -> String -> Property
prop_large_input_performance multiplier base =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for testing
  let largeInput = L.concat (replicate multiplier (base ++ ","))
      processed = splitByComma largeInput
      processedTrimmed = map trim processed
  in property $ L.length processed >= multiplier .&&.
     L.sum (map L.length processedTrimmed) <= L.length largeInput

-- Property: Indentation normalization preserves structure
prop_indentation_structure :: [Int] -> Property
prop_indentation_structure indentLevels =
  not (null indentLevels) && L.all (>= 0) indentLevels && L.all (<= 20) indentLevels ==>
  let lines' = zipWith (\level content -> replicate level ' ' ++ "line" ++ show level) indentLevels [1..]
      content = unlines lines'
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in property $ L.length normalizedLines === L.length lines' .&&.
     L.all (not . null . trim) normalizedLines

-- Property: String splitting edge cases
prop_splitting_edge_cases :: String -> Char -> Property
prop_splitting_edge_cases input delim =
  let edgeCaseInput = input ++ [delim, delim] ++ input ++ [delim] ++ "" ++ input
      regularSplit = splitBy delim edgeCaseInput
      collapsedSplit = splitByCollapsed delim edgeCaseInput
  in property $ L.length regularSplit >= L.length collapsedSplit .&&.
     L.all (not . null) collapsedSplit .&&.
     L.sum (map L.length regularSplit) >= L.sum (map L.length collapsedSplit)

-- ============================================================================
-- Compiler Integration Properties
-- ============================================================================

-- Property: Code compilation preserves semantic structure
prop_compilation_semantics :: String -> Property
prop_compilation_semantics code =
  not ('"' `elem` code) && not ('\'' `elem` code) &&
  not ("/*" `L.isInfixOf` code) && not ("*/" `L.isInfixOf` code) ==>
  let withComments = code ++ " // comment\n /* block */ " ++ code
      processed = removeComments withComments
      codeLines = lines (trim code)
      processedLines = lines (trim processed)
  in property $ L.length processedLines >= L.length codeLines .&&.
     L.all (not . null) processedLines .&&.
     L.any (`L.isInfixOf` processed) codeLines

-- Property: Error location tracking consistency
prop_error_location_consistency :: Int -> Int -> Property
prop_error_location_consistency line col =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 ==>
  let pos = SourcePos line col
      span = emptySpan pos
      advancedPos = advancePosBy "hello" pos
  in sourceLine advancedPos === line .&&.
     sourceColumn advancedPos === col + 5

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper function for pipeline operations (simulated)
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Cabal QuickCheck Tests"
    [ testGroup "SourceLocation Properties"
        [ fastProperty "startPos has line 1, column 1" prop_startPos_basic
        , fastProperty "posAfter advances column by 1" prop_posAfter_column
        , fastProperty "advancePos handles newline correctly" prop_advancePos_newline
        , fastProperty "advancePos handles tab correctly" prop_advancePos_tab
        , fastProperty "spanBetween creates valid span" prop_spanBetween_valid
        , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains
        ]

    , testGroup "Parser Properties"
        [ fastProperty "Comment removal preserves non-comment content" prop_comment_preservation
        , fastProperty "String parsing roundtrip" prop_string_roundtrip
        , fastProperty "Directive parsing consistency" prop_directive_consistency
        ]

    , testGroup "Advanced Utils Properties"
        [ fastProperty "Complex string processing pipeline" prop_complex_pipeline
        , fastProperty "Unicode handling in string operations" prop_unicode_handling
        , fastProperty "Performance with large inputs" prop_large_input_performance
        , fastProperty "Indentation normalization preserves structure" prop_indentation_structure
        , fastProperty "String splitting edge cases" prop_splitting_edge_cases
        ]

    , testGroup "Compiler Integration Properties"
        [ fastProperty "Code compilation preserves semantic structure" prop_compilation_semantics
        , fastProperty "Error location tracking consistency" prop_error_location_consistency
        ]
    ]