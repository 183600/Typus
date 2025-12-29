{-# LANGUAGE TemplateHaskell #-}

-- | Mathematical property tests for SourceLocation module
module Test.Unit.NewSourceLocationMathCoreQuickCheckSpec where

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

-- | SourcePos should be ordered by line first, then column
prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering pos1 pos2 =
  let lineCompare = compare (posLine pos1) (posLine pos2)
      colCompare = compare (posColumn pos1) (posColumn pos2)
      expectedCompare = if lineCompare /= EQ then lineCompare else colCompare
      actualCompare = compare pos1 pos2
  in actualCompare === expectedCompare

-- | SourceSpan should have start <= end in ordering
prop_sourcespan_valid_order :: SourcePos -> SourcePos -> Property
prop_sourcespan_valid_order start end =
  let span = SourceSpan start end
      startPos = spanStart span
      endPos = spanEnd span
  in property $ startPos <= endPos || startPos == endPos

-- | Located values should preserve their span
prop_located_preserves_span :: SourceSpan -> Int -> Property
prop_located_preserves_span span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span

-- | SourcePos equality should be based on both line and column
prop_sourcepos_equality :: SourcePos -> SourcePos -> Property
prop_sourcepos_equality pos1 pos2 =
  let sameLine = posLine pos1 == posLine pos2
      sameCol = posColumn pos1 == posColumn pos2
      shouldBeEqual = sameLine && sameCol
      actuallyEqual = pos1 == pos2
  in shouldBeEqual === actuallyEqual

-- | SourceSpan should handle same start and end positions
prop_sourcespan_same_position :: SourcePos -> Property
prop_sourcespan_same_position pos =
  let span = SourceSpan pos pos
  in spanStart span === spanEnd span

-- | Located values should extract correctly
prop_located_extraction :: SourceSpan -> String -> Property
prop_located_extraction span value =
  let located = locatedWithSpan span value
  case located of
    Located _ v -> v === value

-- | SourcePos should be comparable
prop_sourcepos_comparable :: SourcePos -> SourcePos -> Property
prop_sourcepos_comparable pos1 pos2 =
  let comparison = compare pos1 pos2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

-- | SourceSpan should be constructible from valid positions
prop_sourcespan_constructible :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_constructible line1 col1 line2 col2 =
  let start = SourcePos line1 col1
      end = SourcePos line2 col2
      span = SourceSpan start end
  in spanStart span === start .&&. spanEnd span === end

-- | SourcePos should handle positive line numbers
prop_sourcepos_positive_line :: Positive Int -> Positive Int -> Property
prop_sourcepos_positive_line (Positive line) (Positive col) =
  let pos = SourcePos line col
  in posLine pos > 0 .&&. posColumn pos > 0

-- | SourceSpan ordering should be consistent
prop_sourcespan_ordering_consistent :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcespan_ordering_consistent start1 end1 start2 end2 =
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      startCompare = compare (spanStart span1) (spanStart span2)
      endCompare = compare (spanEnd span1) (spanEnd span2)
  in property $ (startCompare == EQ) ==> (endCompare == compare span1 span2)

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "SourceLocation Math QuickCheck Tests"
  [ testProperty "SourcePos: ordering by line then column" prop_sourcepos_ordering
  , testProperty "SourceSpan: valid start/end order" prop_sourcespan_valid_order
  , testProperty "Located: preserves span" prop_located_preserves_span
  , testProperty "SourcePos: equality based on line and column" prop_sourcepos_equality
  , testProperty "SourceSpan: same position handling" prop_sourcespan_same_position
  , testProperty "Located: value extraction" prop_located_extraction
  , testProperty "SourcePos: comparability" prop_sourcepos_comparable
  , testProperty "SourceSpan: constructibility" prop_sourcespan_constructible
  , testProperty "SourcePos: positive line numbers" prop_sourcepos_positive_line
  , testProperty "SourceSpan: ordering consistency" prop_sourcespan_ordering_consistent
  ]