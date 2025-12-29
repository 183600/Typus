{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Source location math precision tests for SourceLocation module
module Test.Unit.NewSourceLocationMathPrecisionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , posAfter
  , spanBetween
  , mergeSpans
  , isValidSpan
  , spanLength
  , posDistance
  , lineDistance
  , comparePos
  , minPos
  , maxPos
  , spanContains
  , spansOverlap
  , expandSpan
  , _spanLength
  , _posDistance
  , _lineDistance
  , _spanContains
  , _spansOverlap
  , _expandSpan
  )

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate source positions with reasonable bounds
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 10000)
  column <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line column offset

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  endOffset <- choose (0, 1000)
  let end = advancePosBy (replicate endOffset 'a') start
  return $ spanBetween start end

-- Generate text with various characters for position advancement
genTextForAdvancement :: Gen String
genTextForAdvancement = listOf $ elements
  [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  , ' ', '\t', '\n', '\r'
  , '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  , '(', ')', '[', ']', '{', '}', ';', ',', '.', ':', '!', '?', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '|', '\\', '/', '<', '>'
  ]

-- Generate large text for performance testing
genLargeText :: Int -> Gen String
genLargeText size = do
  let chunk = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=[]{}|;':\",./<>? "
  return $ concat $ take (size `div` length chunk + 1) $ repeat chunk

-- ============================================================================
-- Position Arithmetic Properties
-- ============================================================================

-- Property: Position advancement by character should be consistent
prop_pos_advancement_consistent :: SourcePos -> Char -> Property
prop_pos_advancement_consistent pos char =
  let advanced1 = posAfter char pos
      advanced2 = advancePos char pos
  in property $ advanced1 === advanced2

-- Property: Position advancement by string should be cumulative
prop_pos_advancement_cumulative :: SourcePos -> String -> Property
prop_pos_advancement_cumulative pos text =
  let advancedByString = advancePosBy text pos
      advancedByChars = foldl (flip posAfter) pos text
  in property $ advancedByString === advancedByChars

-- Property: Position advancement by text should handle newlines correctly
prop_pos_advancement_newlines :: SourcePos -> String -> Property
prop_pos_advancement_newlines pos text =
  let advanced1 = advancePosBy text pos
      advanced2 = advancePosByText (T.pack text) pos
  in property $ advanced1 === advanced2

-- Property: Position advancement by line should increment line and reset column
prop_pos_advancement_by_line :: SourcePos -> Int -> Property
prop_pos_advancement_by_line pos numLines =
  numLines > 0 ==> 
  let advanced = advancePosByLine numLines pos
      expectedLine = posLine pos + numLines
      expectedColumn = 1
  in property $ posLine advanced === expectedLine .&&. posColumn advanced === expectedColumn

-- Property: Tab advancement should jump to next tab stop
prop_tab_advancement_tab_stop :: SourcePos -> Property
prop_tab_advancement_tab_stop pos =
  let advanced = posAfter '\t' pos
      originalColumn = posColumn pos
      expectedColumn = ((originalColumn - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn advanced === expectedColumn

-- Property: Position distance should be symmetric
prop_pos_distance_symmetric :: SourcePos -> SourcePos -> Property
prop_pos_distance_symmetric pos1 pos2 =
  let distance1 = posDistance pos1 pos2
      distance2 = posDistance pos2 pos1
  in property $ distance1 === distance2

-- Property: Position distance should be zero for identical positions
prop_pos_distance_zero_identical :: SourcePos -> Property
prop_pos_distance_zero_identical pos =
  let distance = posDistance pos pos
  in property $ distance === 0

-- Property: Line distance should match line difference
prop_line_distance_matches_diff :: SourcePos -> SourcePos -> Property
prop_line_distance_matches_diff pos1 pos2 =
  let lineDiff = abs (posLine pos2 - posLine pos1)
      calculatedDistance = lineDistance pos1 pos2
  in property $ lineDiff === calculatedDistance

-- Property: Position comparison should be consistent with offset
prop_pos_comparison_offset_consistent :: SourcePos -> SourcePos -> Property
prop_pos_comparison_offset_consistent pos1 pos2 =
  let comparison = comparePos pos1 pos2
      offsetComparison = compare (posOffset pos1) (posOffset pos2)
  in property $ comparison === offsetComparison

-- Property: Min/Max position should be correct
prop_min_max_position_correct :: SourcePos -> SourcePos -> Property
prop_min_max_position_correct pos1 pos2 =
  let minPos' = minPos pos1 pos2
      maxPos' = maxPos pos1 pos2
      comparison = comparePos pos1 pos2
  in property $ case comparison of
         LT -> minPos' === pos1 .&&. maxPos' === pos2
         EQ -> minPos' === pos1 .&&. maxPos' === pos1
         GT -> minPos' === pos2 .&&. maxPos' === pos1

-- ============================================================================
-- Span Arithmetic Properties
-- ============================================================================

-- Property: Span length should match position distance
prop_span_length_position_distance :: SourceSpan -> Property
prop_span_length_position_distance span =
  let calculatedLength = spanLength span
      expectedLength = posDistance (spanStart span) (spanEnd span)
  in property $ calculatedLength === expectedLength

-- Property: Span between positions should be valid
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ isValidSpan span

-- Property: Merged span should contain both original spans
prop_merged_span_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merged_span_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      contains1 = spanContains merged (spanStart span1) && spanContains merged (spanEnd span1)
      contains2 = spanContains merged (spanStart span2) && spanContains merged (spanEnd span2)
  in property $ contains1 .&&. contains2

-- Property: Span overlap should be symmetric
prop_span_overlap_symmetric :: SourceSpan -> SourceSpan -> Property
prop_span_overlap_symmetric span1 span2 =
  let overlap1 = spansOverlap span1 span2
      overlap2 = spansOverlap span2 span1
  in property $ overlap1 === overlap2

-- Property: Span expansion should preserve original content
prop_span_expansion_preserves_original :: SourceSpan -> Int -> Int -> Property
prop_span_expansion_preserves_original span before after =
  before >= 0 && after >= 0 ==> 
  let expanded = expandSpan before after span
      originalStart = spanStart span
      originalEnd = spanEnd span
  in property $ spanContains expanded originalStart .&&. spanContains expanded originalEnd

-- Property: Empty span should have zero length
prop_empty_span_zero_length :: SourcePos -> Property
prop_empty_span_zero_length pos =
  let emptySpan = SourceSpan pos pos
      length = spanLength emptySpan
  in property $ length === 0

-- ============================================================================
-- Precision and Boundary Properties
-- ============================================================================

-- Property: Large position calculations should be precise
prop_large_position_precise :: Int -> String -> Property
prop_large_position_precise multiplier text =
  multiplier > 0 && multiplier <= 1000 ==> 
  let largeText = concat $ replicate multiplier text
      startPos' = startPos
      endPos = advancePosBy largeText startPos'
      calculatedDistance = posDistance startPos' endPos
      expectedDistance = length largeText
  in property $ calculatedDistance === expectedDistance

-- Property: Position arithmetic should handle very large line numbers
prop_large_line_numbers :: Int -> Property
prop_large_line_numbers lineNum =
  lineNum > 0 && lineNum <= 100000 ==> 
  let pos = SourcePos lineNum 1 0
      advanced = advancePosByLine 1 pos
  in property $ posLine advanced === lineNum + 1

-- Property: Position arithmetic should handle very large column numbers
prop_large_column_numbers :: Int -> Property
prop_large_column_numbers columnNum =
  columnNum > 0 && columnNum <= 100000 ==> 
  let pos = SourcePos 1 columnNum 0
      advanced = posAfter 'a' pos
  in property $ posColumn advanced === columnNum + 1

-- Property: Span calculations should be precise across line boundaries
prop_span_precision_line_boundaries :: Int -> Property
prop_span_precision_line_boundaries numLines =
  numLines > 0 && numLines <= 1000 ==> 
  let text = unlines $ replicate numLines "test content"
      startPos' = startPos
      endPos = advancePosBy text startPos'
      span = spanBetween startPos' endPos
      calculatedLength = spanLength span
      expectedLength = length text
  in property $ calculatedLength === expectedLength

-- Property: Tab calculations should be precise at tab boundaries
prop_tab_precision_boundaries :: Int -> Property
prop_tab_precision_boundaries columnNum =
  columnNum > 0 && columnNum <= 100 ==> 
  let pos = SourcePos 1 columnNum 0
      afterTab = posAfter '\t' pos
      expectedColumn = ((columnNum - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn afterTab === expectedColumn

-- ============================================================================
-- Performance and Scalability Properties
-- ============================================================================

-- Property: Position calculations should handle large texts efficiently
prop_position_calculation_large_text :: Int -> Property
prop_position_calculation_large_text size =
  size > 0 && size <= 100000 ==> 
  let largeText = concat $ replicate size "a"
      startPos' = startPos
      endPos = advancePosBy largeText startPos'
      distance = posDistance startPos' endPos
  in property $ distance === size

-- Property: Span merging should handle many spans efficiently
prop_span_merging_many_spans :: Int -> Property
prop_span_merging_many_spans numSpans =
  numSpans > 0 && numSpans <= 1000 ==> 
  let positions = take numSpans $ iterate (\p -> advancePosBy "test" p) startPos
      spans = zipWith spanBetween positions (tail positions)
      merged = foldl mergeSpans (head spans) (tail spans)
  in property $ isValidSpan merged

-- ============================================================================
-- Edge Cases and Boundary Conditions
-- ============================================================================

-- Property: Position advancement with empty string should not change position
prop_empty_string_no_change :: SourcePos -> Property
prop_empty_string_no_change pos =
  let advanced = advancePosBy "" pos
  in property $ advanced === pos

-- Property: Position advancement with only newlines should increment lines
prop_only_newlines_increment_lines :: SourcePos -> Int -> Property
prop_only_newlines_increment_lines pos numNewlines =
  numNewlines > 0 ==> 
  let newlineText = concat $ replicate numNewlines "\n"
      advanced = advancePosBy newlineText pos
  in property $ posLine advanced === posLine pos + numNewlines .&&. posColumn advanced === 1

-- Property: Span with start after end should be invalid
prop_span_start_after_end_invalid :: SourcePos -> SourcePos -> Property
prop_span_start_after_end_invalid pos1 pos2 =
  pos1 > pos2 ==> 
  let span = spanBetween pos1 pos2
  in property $ not (isValidSpan span)

-- Property: Position arithmetic should handle Unicode characters
prop_unicode_position_arithmetic :: String -> Property
prop_unicode_position_arithmetic unicodeText =
  let startPos' = startPos
      advanced = advancePosBy unicodeText startPos'
      distance = posDistance startPos' advanced
  in property $ distance >= 0

-- Property: Very large offsets should be handled correctly
prop_very_large_offsets :: Int -> Property
prop_very_large_offsets offset =
  offset > 0 && offset <= 1000000 ==> 
  let pos = SourcePos 1 1 offset
      advanced = posAfter 'a' pos
  in property $ posOffset advanced === offset + 1

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Source Location Math Precision QuickCheck Tests"
  [ testGroup "Position Arithmetic"
    [ fastProperty "position advancement consistent" prop_pos_advancement_consistent
    , fastProperty "position advancement cumulative" prop_pos_advancement_cumulative
    , fastProperty "position advancement newlines" prop_pos_advancement_newlines
    , fastProperty "position advancement by line" prop_pos_advancement_by_line
    , fastProperty "tab advancement tab stop" prop_tab_advancement_tab_stop
    ]

  , testGroup "Position Distance and Comparison"
    [ fastProperty "position distance symmetric" prop_pos_distance_symmetric
    , fastProperty "position distance zero identical" prop_pos_distance_zero_identical
    , fastProperty "line distance matches diff" prop_line_distance_matches_diff
    , fastProperty "position comparison offset consistent" prop_pos_comparison_offset_consistent
    , fastProperty "min max position correct" prop_min_max_position_correct
    ]

  , testGroup "Span Arithmetic"
    [ fastProperty "span length position distance" prop_span_length_position_distance
    , fastProperty "span between valid" prop_span_between_valid
    , fastProperty "merged span contains both" prop_merged_span_contains_both
    , fastProperty "span overlap symmetric" prop_span_overlap_symmetric
    , fastProperty "span expansion preserves original" prop_span_expansion_preserves_original
    , fastProperty "empty span zero length" prop_empty_span_zero_length
    ]

  , testGroup "Precision and Boundary Properties"
    [ fastProperty "large position precise" prop_large_position_precise
    , fastProperty "large line numbers" prop_large_line_numbers
    , fastProperty "large column numbers" prop_large_column_numbers
    , fastProperty "span precision line boundaries" prop_span_precision_line_boundaries
    , fastProperty "tab precision boundaries" prop_tab_precision_boundaries
    ]

  , testGroup "Performance and Scalability"
    [ fastProperty "position calculation large text" prop_position_calculation_large_text
    , fastProperty "span merging many spans" prop_span_merging_many_spans
    ]

  , testGroup "Edge Cases and Boundary Conditions"
    [ fastProperty "empty string no change" prop_empty_string_no_change
    , fastProperty "only newlines increment lines" prop_only_newlines_increment_lines
    , fastProperty "span start after end invalid" prop_span_start_after_end_invalid
    , fastProperty "unicode position arithmetic" prop_unicode_position_arithmetic
    , fastProperty "very large offsets" prop_very_large_offsets
    ]
  ]