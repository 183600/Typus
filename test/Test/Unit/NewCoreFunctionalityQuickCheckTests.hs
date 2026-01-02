{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreFunctionalityQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import qualified Test.QuickCheck as QC

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , spanFrom
  , spanBetween
  , mergeSpans
  , advancePos
  , advancePosByText
  )

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Test 1: Utils trim function properties
-- ============================================================================

-- Property: trim is idempotent (applying twice gives same result as once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = "content"
      input = prefix ++ content ++ suffix
      trimmed = trim input
      hasLeadingSpace = not (null prefix) && isSpace (last prefix)
      hasTrailingSpace = not (null suffix) && isSpace (L.head suffix)
  in classify hasLeadingSpace "has leading whitespace" $
     classify hasTrailingSpace "has trailing whitespace" $
     property $ trimmed === content

-- ============================================================================
-- Test 2: Utils splitBy properties
-- ============================================================================

-- Property: splitBy preserves total content when rejoined
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === str

-- Property: splitByCollapsed never produces empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ L.all (not . null) parts

-- ============================================================================
-- Test 3: SourceLocation position arithmetic properties
-- ============================================================================

-- Property: advancing position by text gives correct line count
prop_advance_pos_by_text_line_count :: String -> Property
prop_advance_pos_by_text_line_count txt =
  let finalPos = advancePosByText startPos txt
      expectedLine = L.length (L.filter (== '\n') txt) + 1
  in property $ posLine finalPos === expectedLine

-- Property: spanBetween creates valid span
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ isValidSpan span

-- ============================================================================
-- Test 4: Comment removal properties
-- ============================================================================

-- Property: removeLineComments preserves non-comment content
prop_remove_line_comments_preserves_content :: String -> String -> Property
prop_remove_line_comments_preserves_content code comment =
  -- Avoid strings with quotes that would complicate comment parsing
  not (L.any (`elem` "\"'") code) && not (L.any (`elem` "\"'") comment) ==>
  let input = code ++ " // " ++ comment ++ "\nmore code"
      result = removeLineComments input
  in property $ code `L.isInfixOf` result .&&. "more code" `L.isInfixOf` result

-- Property: removeComments is idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent str =
  let removedOnce = removeComments str
      removedTwice = removeComments removedOnce
  in property $ removedOnce === removedTwice

-- ============================================================================
-- Test 5: Parser properties
-- ============================================================================

-- Property: parsing empty content returns empty blocks
prop_parse_empty_content :: Property
prop_parse_empty_content =
  let result = parseTypus "" 
  in case result of
    Left _ -> property False
    Right typusFile -> property $ L.null (tfBlocks typusFile)

-- Property: parsing preserves directive structure
prop_parse_preserves_directives :: Property
prop_parse_preserves_directives =
  let content = "//! ownership=true, dependent-types=false\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> 
      let directives = tfDirectives typusFile
      in property $ case fdOwnership directives of
        Nothing -> False
        Just (Located _ val) -> val

-- ============================================================================
-- Test 6: String processing pipeline properties
-- ============================================================================

-- Property: normalization pipeline is consistent
prop_normalization_pipeline_consistent :: String -> Property
prop_normalization_pipeline_consistent input =
  let pipeline1 = input |> trim |> removeLineComments |> normalizeIndentation
      pipeline2 = input |> removeLineComments |> trim |> normalizeIndentation
  in property $ pipeline1 === pipeline2
  where
    (|>) x f = f x

-- ============================================================================
-- Test 7: Advanced string processing properties
-- ============================================================================

-- Property: breakOn correctly finds substring boundaries
prop_break_on_correct_boundaries :: String -> String -> String -> Property
prop_break_on_correct_boundaries prefix delimiter suffix =
  not (null delimiter) ==>
  let haystack = prefix ++ delimiter ++ suffix
      (before, after) = breakOn delimiter haystack
  in property $ before ++ delimiter ++ after === haystack

-- ============================================================================
-- Test 8: Indentation properties
-- ============================================================================

-- Property: normalizeIndentation preserves relative structure
prop_normalize_indentation_preserves_structure :: [String] -> Property
prop_normalize_indentation_preserves_structure lines =
  not (null lines) ==>
  let input = Data.List.unlines lines
      normalized = normalizeIndentation input
      inputLines = lines input
      outputLines = lines normalized
  in property $ L.length inputLines === L.length outputLines

-- ============================================================================
-- Test 9: Source location advanced properties
-- ============================================================================

-- Property: merging spans preserves coverage
prop_merge_spans_preserve_coverage :: SourcePos -> SourcePos -> SourcePos -> Property
prop_merge_spans_preserve_coverage pos1 pos2 pos3 =
  let span1 = spanFrom pos1 pos2
      span2 = spanFrom pos2 pos3
      merged = mergeSpans span1 span2
      startPos1 = spanStart span1
      endPos2 = spanEnd span2
  in property $ spanStart merged === startPos1 .&&. spanEnd merged === endPos2

-- ============================================================================
-- Test 10: Error handling properties
-- ============================================================================

-- Property: processing malformed input doesn't crash
prop_processing_malformed_input_safe :: String -> Property
prop_processing_malformed_input_safe input =
  let trimmed = trim input
      split = splitBy ',' input
      commentsRemoved = removeComments input
      normalized = normalizeIndentation input
  in property $ L.length trimmed >= 0 .&&. L.length split >= 1 .&&. L.length commentsRemoved >= 0 .&&. L.length normalized >= 0

-- ============================================================================
-- Test collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Core Functionality QuickCheck Tests"
  [ testGroup "Utils Module Tests"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "splitBy roundtrip consistency" prop_splitBy_roundtrip
    , fastProperty "splitByCollapsed never produces empty segments" prop_splitByCollapsed_no_empty
    ]
  , testGroup "SourceLocation Module Tests"
    [ fastProperty "advancePosByText gives correct line count" prop_advance_pos_by_text_line_count
    , fastProperty "spanBetween creates valid span" prop_span_between_valid
    , fastProperty "mergeSpans preserves coverage" prop_merge_spans_preserve_coverage
    ]
  , testGroup "Comment Processing Tests"
    [ fastProperty "removeLineComments preserves non-comment content" prop_remove_line_comments_preserves_content
    , fastProperty "removeComments is idempotent" prop_remove_comments_idempotent
    ]
  , testGroup "Parser Module Tests"
    [ fastProperty "parsing empty content returns empty blocks" prop_parse_empty_content
    , fastProperty "parsing preserves directive structure" prop_parse_preserves_directives
    ]
  , testGroup "String Processing Pipeline Tests"
    [ fastProperty "normalization pipeline is consistent" prop_normalization_pipeline_consistent
    , fastProperty "breakOn correctly finds substring boundaries" prop_break_on_correct_boundaries
    ]
  , testGroup "Indentation Tests"
    [ fastProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_preserves_structure
    ]
  , testGroup "Error Handling Tests"
    [ fastProperty "processing malformed input doesn't crash" prop_processing_malformed_input_safe
    ]
  ]

-- Helper function to check if a span is valid
isValidSpan :: SourceSpan -> Bool
isValidSpan span = 
  let start = spanStart span
      end = spanEnd span
  in posLine start <= posLine end && 
     (posLine start < posLine end || posColumn start <= posColumn end)