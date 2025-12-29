{-# LANGUAGE TemplateHaskell #-}

-- | Boundary condition tests for SourceLocation module
module Test.Unit.SourceLocationBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation 
  ( SourcePos(..)
  , SourceSpan(..)
  , spanStart
  , spanEnd
  , Located(..)
  , locatedWithSpan
  , posLine
  , posColumn
  )
import Data.Ord (comparing)

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | SourcePos should handle extreme line numbers
prop_sourcepos_extreme_lines :: Positive Int -> Property
prop_sourcepos_extreme_lines (Positive line) =
  let pos = SourcePos line 1
  in posLine pos === line

-- | SourcePos should handle extreme column numbers
prop_sourcepos_extreme_columns :: Positive Int -> Property
prop_sourcepos_extreme_columns (Positive col) =
  let pos = SourcePos 1 col
  in posColumn pos === col

-- | SourcePos should handle zero values gracefully
prop_sourcepos_zero_values :: Property
prop_sourcepos_zero_values =
  let pos = SourcePos 0 0
  in posLine pos === 0 .&&. posColumn pos === 0

-- | SourceSpan should handle same position spans
prop_sourcespan_same_position :: Positive Int -> Positive Int -> Property
prop_sourcespan_same_position (Positive line) (Positive col) =
  let pos = SourcePos line col
      span = SourceSpan pos pos
  in spanStart span === pos .&&. spanEnd span === pos

-- | SourceSpan should handle reversed positions
prop_sourcespan_reversed_positions :: Positive Int -> Positive Int -> Property
prop_sourcespan_reversed_positions (Positive line1) (Positive col1) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos (line1 + 1) (col1 + 1)
      span1 = SourceSpan pos1 pos2
      span2 = SourceSpan pos2 pos1
  in spanStart span1 === pos1 .&&. spanEnd span1 === pos2 .&&.
     spanStart span2 === pos2 .&&. spanEnd span2 === pos1

-- | Located values should handle extreme spans
prop_located_extreme_spans :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_located_extreme_spans (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let start = SourcePos line1 col1
      end = SourcePos line2 col2
      span = SourceSpan start end
      located = locatedWithSpan span value
  in locatedSpan located === span

-- | SourcePos comparison should handle equal values
prop_sourcepos_equal_comparison :: Positive Int -> Positive Int -> Property
prop_sourcepos_equal_comparison (Positive line) (Positive col) =
  let pos1 = SourcePos line col
      pos2 = SourcePos line col
  in pos1 === pos2 .&&. compare pos1 pos2 === EQ

-- | SourcePos comparison should handle different lines
prop_sourcepos_line_comparison :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_line_comparison (Positive line1) (Positive line2) (Positive col) =
  let pos1 = SourcePos line1 col
      pos2 = SourcePos line2 col
      expected = if line1 < line2 then LT else if line1 > line2 then GT else EQ
  in compare pos1 pos2 === expected

-- | SourcePos comparison should handle different columns
prop_sourcepos_column_comparison :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_column_comparison (Positive line) (Positive col1) (Positive col2) =
  let pos1 = SourcePos line col1
      pos2 = SourcePos line col2
      expected = if col1 < col2 then LT else if col1 > col2 then GT else EQ
  in compare pos1 pos2 === expected

-- | SourceSpan should handle very large positions
prop_sourcespan_large_positions :: Property
prop_sourcespan_large_positions =
  let largeNum = 1000000
      start = SourcePos largeNum largeNum
      end = SourcePos (largeNum + 1) (largeNum + 1)
      span = SourceSpan start end
  in spanStart span === start .&&. spanEnd span === end

-- | Located values should extract correctly regardless of span
prop_located_extraction_boundary :: SourceSpan -> String -> Property
prop_located_extraction_boundary span value =
  let located = locatedWithSpan span value
  case located of
    Located _ v -> v === value

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "SourceLocation Boundary QuickCheck Tests"
  [ testProperty "SourcePos: extreme line numbers" prop_sourcepos_extreme_lines
  , testProperty "SourcePos: extreme column numbers" prop_sourcepos_extreme_columns
  , testProperty "SourcePos: zero values" prop_sourcepos_zero_values
  , testProperty "SourceSpan: same position spans" prop_sourcespan_same_position
  , testProperty "SourceSpan: reversed positions" prop_sourcespan_reversed_positions
  , testProperty "Located: extreme spans" prop_located_extreme_spans
  , testProperty "SourcePos: equal comparison" prop_sourcepos_equal_comparison
  , testProperty "SourcePos: line comparison" prop_sourcepos_line_comparison
  , testProperty "SourcePos: column comparison" prop_sourcepos_column_comparison
  , testProperty "SourceSpan: large positions" prop_sourcespan_large_positions
  ]