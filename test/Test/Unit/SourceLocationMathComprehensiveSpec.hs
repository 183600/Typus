{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (sort)
import Data.Ord (comparing)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , locatedWithSpan
  , locatedValue
  , spanContains
  , spanOverlaps
  , spanMerge
  , spanLength
  , posCompare
  , posDelta
  , posAdd
  )

-- | Comprehensive QuickCheck tests for SourceLocation mathematical operations
-- This module tests the mathematical properties L.and boundary conditions of source location handling

-- Property: startPos has correct coordinates
prop_startPos_values :: Property
prop_startPos_values =
  spLine startPos === 1 &&
  spColumn startPos === 1

-- Property: SourcePos construction maintains invariants
prop_sourcePos_construction :: Int -> Int -> Property
prop_sourcePos_construction line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
  in spLine pos === line && spColumn pos === col

-- Property: posCompare is antisymmetric
prop_posCompare_antisymmetric :: SourcePos -> SourcePos -> Property
prop_posCompare_antisymmetric pos1 pos2 =
  let cmp1 = posCompare pos1 pos2
      cmp2 = posCompare pos2 pos1
  in (cmp1 == EQ) === (cmp2 == EQ) .&&.
     (cmp1 == LT && cmp2 == GT) .||. (cmp1 == GT && cmp2 == LT) .||. (cmp1 == EQ && cmp2 == EQ)

-- Property: posCompare is transitive
prop_posCompare_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_posCompare_transitive pos1 pos2 pos3 =
  let cmp12 = posCompare pos1 pos2
      cmp23 = posCompare pos2 pos3
      cmp13 = posCompare pos1 pos3
  in (cmp12 == EQ && cmp23 == EQ) ==> cmp13 === EQ .&&.
     (cmp12 == LT && cmp23 == LT) ==> cmp13 === LT .&&.
     (cmp12 == GT && cmp23 == GT) ==> cmp13 === GT

-- Property: posDelta calculation is correct for same line
prop_posDelta_same_line :: Int -> Int -> Int -> Property
prop_posDelta_same_line line col1 col2 =
  line > 0 && col1 > 0 && col2 > 0 ==>
  let pos1 = SourcePos line col1
      pos2 = SourcePos line col2
      (deltaLine, deltaCol) = posDelta pos1 pos2
  in deltaLine === 0 && deltaCol === (col2 - col1)

-- Property: posDelta calculation is correct for different lines
prop_posDelta_different_lines :: Int -> Int -> Int -> Int -> Property
prop_posDelta_different_lines line1 col1 line2 col2 =
  line1 > 0 && line2 > 0 && col1 > 0 && col2 > 0 ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      (deltaLine, deltaCol) = posDelta pos1 pos2
  in deltaLine === (line2 - line1)

-- Property: posAdd is inverse of posDelta
prop_posAdd_inverse :: SourcePos -> SourcePos -> Property
prop_posAdd_inverse pos1 pos2 =
  let (deltaLine, deltaCol) = posDelta pos1 pos2
      reconstructed = posAdd pos1 (deltaLine, deltaCol)
  in reconstructed === pos2

-- Property: posAdd with zero delta returns original position
prop_posAdd_zero :: SourcePos -> Property
prop_posAdd_zero pos =
  posAdd pos (0, 0) === pos

-- Property: spanLength calculation is correct
prop_spanLength_single_line :: Int -> Int -> Int -> Property
prop_spanLength_single_line line startCol endCol =
  line > 0 && startCol > 0 && endCol >= startCol ==>
  let start = SourcePos line startCol
      end = SourcePos line endCol
      span = SourceSpan start end
      expectedLength = endCol - startCol + 1
  in spanLength span === expectedLength

-- Property: spanLength for multi-line spans is at least line count
prop_spanLength_multi_line :: Int -> Int -> Int -> Int -> Property
prop_spanLength_multi_line startLine startCol endLine endCol =
  startLine > 0 && endLine >= startLine && startCol > 0 && endCol > 0 ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
      lineCount = endLine - startLine + 1
  in spanLength span >= lineCount

-- Property: spanContains is reflexive
prop_spanContains_reflexive :: SourceSpan -> Property
prop_spanContains_reflexive span =
  spanContains span span === True

-- Property: spanContains is transitive
prop_spanContains_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_spanContains_transitive outer middle inner =
  spanContains outer middle && spanContains middle inner ==>
  spanContains outer inner === True

-- Property: spanOverlaps is symmetric
prop_spanOverlaps_symmetric :: SourceSpan -> SourceSpan -> Property
prop_spanOverlaps_symmetric span1 span2 =
  spanOverlaps span1 span2 === spanOverlaps span2 span1

-- Property: spanOverlaps implies spanContains L.or L.reverse
prop_spanOverlaps_implies_contains :: SourceSpan -> SourceSpan -> Property
prop_spanOverlaps_implies_contains span1 span2 =
  spanOverlaps span1 span2 ==>
  spanContains span1 span2 .||. spanContains span2 span1 .||. 
  (spanContains (ssStart span1) span2 && spanContains span2 (ssEnd span1)) .||.
  (spanContains (ssStart span2) span1 && spanContains span1 (ssEnd span2))

-- Property: spanMerge contains both original spans
prop_spanMerge_contains_both :: SourceSpan -> SourceSpan -> Property
prop_spanMerge_contains_both span1 span2 =
  let merged = spanMerge span1 span2
  in spanContains merged span1 .&&. spanContains merged span2

-- Property: spanMerge is commutative
prop_spanMerge_commutative :: SourceSpan -> SourceSpan -> Property
prop_spanMerge_commutative span1 span2 =
  spanMerge span1 span2 === spanMerge span2 span1

-- Property: spanMerge is associative
prop_spanMerge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_spanMerge_associative span1 span2 span3 =
  let merge12 = spanMerge span1 span2
      merge23 = spanMerge span2 span3
      result1 = spanMerge merge12 span3
      result2 = spanMerge span1 merge23
  in result1 === result2

-- Property: locatedWithSpan creates correct location
prop_locatedWithSpan :: String -> SourceSpan -> Property
prop_locatedWithSpan value span =
  let located = locatedWithSpan span value
  in locatedValue located === value && locatedSpan located === span

-- Property: SourcePos ordering matches tuple ordering
prop_sourcePos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcePos_ordering pos1 pos2 =
  let tuple1 = (spLine pos1, spColumn pos1)
      tuple2 = (spLine pos2, spColumn pos2)
      posCmp = posCompare pos1 pos2
      tupleCmp = compare tuple1 tuple2
  in posCmp === tupleCmp

-- Property: spanLength is non-negative
prop_spanLength_non_negative :: SourceSpan -> Property
prop_spanLength_non_negative span =
  let L.length = spanLength span
  in L.length >= 0

-- Property: spanContains for identical positions
prop_spanContains_identical_positions :: SourcePos -> Property
prop_spanContains_identical_positions pos =
  let span = SourceSpan pos pos
  in spanContains span span === True

-- Property: spanMerge with identical spans
prop_spanMerge_identical :: SourceSpan -> Property
prop_spanMerge_identical span =
  spanMerge span span === span

-- Property: posAdd with negative line delta
prop_posAdd_negative_line :: SourcePos -> Int -> Property
prop_posAdd_negative_line pos deltaLine =
  deltaLine < 0 ==>
  let newPos = posAdd pos (deltaLine, 0)
      newLine = spLine newPos
  in newLine >= 1

-- Property: posAdd with negative column delta
prop_posAdd_negative_column :: SourcePos -> Int -> Property
prop_posAdd_negative_column pos deltaCol =
  deltaCol < 0 ==>
  let newPos = posAdd pos (0, deltaCol)
      newCol = spColumn newPos
  in newCol >= 1

-- Property: spanOverlaps for adjacent spans
prop_spanOverlaps_adjacent :: Int -> Int -> Int -> Property
prop_spanOverlaps_adjacent line startCol L.length =
  line > 0 && startCol > 0 && L.length > 0 ==>
  let pos1 = SourcePos line startCol
      pos2 = SourcePos line (startCol + L.length - 1)
      pos3 = SourcePos line (startCol + L.length)
      span1 = SourceSpan pos1 pos2
      span2 = SourceSpan pos3 pos3
  in spanOverlaps span1 span2 === False

-- Property: spanContains for nested spans
prop_spanContains_nested :: Int -> Int -> Int -> Int -> Property
prop_spanContains_nested line startCol innerLength outerLength =
  line > 0 && startCol > 0 && innerLength > 0 && outerLength >= innerLength ==>
  let innerStart = SourcePos line startCol
      innerEnd = SourcePos line (startCol + innerLength - 1)
      outerStart = SourcePos line (startCol - 1)
      outerEnd = SourcePos line (startCol + outerLength)
      innerSpan = SourceSpan innerStart innerEnd
      outerSpan = SourceSpan outerStart outerEnd
  in spanContains outerSpan innerSpan === True

tests :: TestTree
tests = testGroup "SourceLocation Math Comprehensive QuickCheck tests"
  [ fastProperty "startPos has correct values" prop_startPos_values
  , fastProperty "SourcePos construction maintains invariants" prop_sourcePos_construction
  , fastProperty "posCompare is antisymmetric" prop_posCompare_antisymmetric
  , fastProperty "posCompare is transitive" prop_posCompare_transitive
  , fastProperty "posDelta calculation is correct for same line" prop_posDelta_same_line
  , fastProperty "posDelta calculation is correct for different lines" prop_posDelta_different_lines
  , fastProperty "posAdd is inverse of posDelta" prop_posAdd_inverse
  , fastProperty "posAdd with zero delta returns original position" prop_posAdd_zero
  , fastProperty "spanLength calculation is correct" prop_spanLength_single_line
  , fastProperty "spanLength for multi-line spans is at least line count" prop_spanLength_multi_line
  , fastProperty "spanContains is reflexive" prop_spanContains_reflexive
  , fastProperty "spanContains is transitive" prop_spanContains_transitive
  , fastProperty "spanOverlaps is symmetric" prop_spanOverlaps_symmetric
  , fastProperty "spanOverlaps implies spanContains L.or L.reverse" prop_spanOverlaps_implies_contains
  , fastProperty "spanMerge contains both original spans" prop_spanMerge_contains_both
  , fastProperty "spanMerge is commutative" prop_spanMerge_commutative
  , fastProperty "spanMerge is associative" prop_spanMerge_associative
  , fastProperty "locatedWithSpan creates correct location" prop_locatedWithSpan
  , fastProperty "SourcePos ordering matches tuple ordering" prop_sourcePos_ordering
  , fastProperty "spanLength is non-negative" prop_spanLength_non_negative
  , fastProperty "spanContains for identical positions" prop_spanContains_identical_positions
  , fastProperty "spanMerge with identical spans" prop_spanMerge_identical
  , fastProperty "posAdd with negative line delta" prop_posAdd_negative_line
  , fastProperty "posAdd with negative column delta" prop_posAdd_negative_column
  , fastProperty "spanOverlaps for adjacent spans" prop_spanOverlaps_adjacent
  , fastProperty "spanContains for nested spans" prop_spanContains_nested
  ]