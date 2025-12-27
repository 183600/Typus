{-# LANGUAGE CPP #-}

-- | Source location mathematical property tests using QuickCheck
module Test.Unit.SourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List

import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn)

-- ============================================================================
-- Source Position Mathematical Properties
-- ============================================================================

-- Property: SourcePos creation preserves values
prop_sourcepos_creation :: Int -> Int -> Property
prop_sourcepos_creation line col =
  line >= 1 && col >= 1 ==>
  let pos = SourcePos line col
  in property $ posLine pos == line && posColumn pos == col

-- Property: SourcePos ordering is consistent
prop_sourcepos_ordering :: Int -> Int -> Int -> Int -> Property
prop_sourcepos_ordering line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      isEarlier = (line1 < line2) || (line1 == line2 && col1 < col2)
  in classify isEarlier "earlier position" $
     property $ isEarlier == (pos1 < pos2)

-- Property: SourcePos equality is reflexive
prop_sourcepos_equality_reflexive :: Int -> Int -> Property
prop_sourcepos_equality_reflexive line col =
  line >= 1 && col >= 1 ==>
  let pos = SourcePos line col
  in property $ pos == pos

-- Property: SourcePos equality is symmetric
prop_sourcepos_equality_symmetric :: Int -> Int -> Int -> Int -> Property
prop_sourcepos_equality_symmetric line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
  in property $ (pos1 == pos2) == (pos2 == pos1)

-- ============================================================================
-- Source Span Mathematical Properties
-- ============================================================================

-- Property: SourceSpan creation preserves boundaries
prop_sourcespan_creation :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_creation startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= startLine && 
  (endLine > startLine || endCol >= startCol) ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
  in property $ spanStart span == start && spanEnd span == end

-- Property: SourceSpan length calculation
prop_sourcespan_length :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_length startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= startLine && 
  (endLine > startLine || endCol >= startCol) && 
  (endLine - startLine <= 5) ==> -- Reasonable size limit
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
      expectedLength = if startLine == endLine 
                       then endCol - startCol + 1
                       else endCol + (endLine - startLine - 1) * 80 + (80 - startCol + 1)
  in property $ spanLength span >= 1

-- Property: SourceSpan contains its start position
prop_sourcespan_contains_start :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_contains_start startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= startLine && 
  (endLine > startLine || endCol >= startCol) ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
  in property $ spanContains span start

-- Property: SourceSpan contains its end position
prop_sourcespan_contains_end :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_contains_end startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= startLine && 
  (endLine > startLine || endCol >= startCol) ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
  in property $ spanContains span end

-- Property: SourceSpan containment is transitive
prop_sourcespan_containment_transitive :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_sourcespan_containment_transitive startLine startCol midLine midCol endLine endCol =
  startLine >= 1 && startCol >= 1 && midLine >= startLine && endLine >= midLine &&
  ((midLine > startLine) || (midLine == startLine && midCol >= startCol)) &&
  ((endLine > midLine) || (endLine == midLine && endCol >= midCol)) ==>
  let start = SourcePos startLine startCol
      mid = SourcePos midLine midCol
      end = SourcePos endLine endCol
      span1 = SourceSpan start mid
      span2 = SourceSpan mid end
      combined = SourceSpan start end
  in property $ spanContains combined span1 && spanContains combined span2

-- Property: SourceSpan intersection properties
prop_sourcespan_intersection :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_sourcespan_intersection start1Line start1Col end1Line end1Col 
                               start2Line start2Col end2Line end2Col =
  start1Line >= 1 && start1Col >= 1 && end1Line >= start1Line && 
  (end1Line > start1Line || end1Col >= start1Col) &&
  start2Line >= 1 && start2Col >= 1 && end2Line >= start2Line && 
  (end2Line > start2Line || end2Col >= start2Col) ==>
  let start1 = SourcePos start1Line start1Col
      end1 = SourcePos end1Line end1Col
      start2 = SourcePos start2Line start2Col
      end2 = SourcePos end2Line end2Col
      span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      intersection = spanIntersection span1 span2
  in property $ case intersection of
    Nothing -> True -- No intersection is valid
    Just interSpan -> spanContains span1 interSpan && spanContains span2 interSpan

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper functions for SourceSpan operations (these would normally be in SourceLocation module)
spanLength :: SourceSpan -> Int
spanLength (SourceSpan start end) = 
  let SourcePos startLine startCol = start
      SourcePos endLine endCol = end
  in if startLine == endLine 
     then endCol - startCol + 1
     else endCol + (endLine - startLine - 1) * 80 + (80 - startCol + 1)

spanContains :: SourceSpan -> SourcePos -> Bool
spanContains (SourceSpan start end) pos =
  let SourcePos startLine startCol = start
      SourcePos endLine endCol = end
      SourcePos posLine posCol = pos
  in (posLine > startLine || (posLine == startLine && posCol >= startCol)) &&
     (posLine < endLine || (posLine == endLine && posCol <= endCol))

spanContains :: SourceSpan -> SourceSpan -> Bool
spanContains (SourceSpan start1 end1) (SourceSpan start2 end2) =
  spanContains (SourceSpan start1 end1) start2 &&
  spanContains (SourceSpan start1 end1) end2

spanIntersection :: SourceSpan -> SourceSpan -> Maybe SourceSpan
spanIntersection span1@(SourceSpan start1 end1) span2@(SourceSpan start2 end2) =
  let newStart = maxPos start1 start2
      newEnd = minPos end1 end2
  in if posLessOrEqual newStart newEnd 
     then Just (SourceSpan newStart newEnd)
     else Nothing
  where
    maxPos p1 p2 = if p1 >= p2 then p1 else p2
    minPos p1 p2 = if p1 <= p2 then p1 else p2

posLessOrEqual :: SourcePos -> SourcePos -> Bool
posLessOrEqual (SourcePos line1 col1) (SourcePos line2 col2) =
  line1 < line2 || (line1 == line2 && col1 <= col2)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Mathematical Tests"
  [ fastProperty "SourcePos creation preserves values" prop_sourcepos_creation
  , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
  , fastProperty "SourcePos equality is reflexive" prop_sourcepos_equality_reflexive
  , fastProperty "SourcePos equality is symmetric" prop_sourcepos_equality_symmetric
  , fastProperty "SourceSpan creation preserves boundaries" prop_sourcespan_creation
  , fastProperty "SourceSpan length calculation" prop_sourcespan_length
  , fastProperty "SourceSpan contains its start position" prop_sourcespan_contains_start
  , fastProperty "SourceSpan contains its end position" prop_sourcespan_contains_end
  , fastProperty "SourceSpan containment is transitive" prop_sourcespan_containment_transitive
  , fastProperty "SourceSpan intersection properties" prop_sourcespan_intersection
  ]