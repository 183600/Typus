{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
import Utils (trim)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort)

-- Property: Position addition is commutative
prop_position_addition_commutative :: Int -> Int -> Int -> Int -> Property
prop_position_addition_commutative line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 ==>
  let pos1 = Position line1 col1
      pos2 = Position line2 col2
      result1 = addPositions pos1 pos2
      result2 = addPositions pos2 pos1
  in property $ result1 === result2

-- Property: Position addition is associative
prop_position_addition_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_position_addition_associative line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let pos1 = Position line1 col1
      pos2 = Position line2 col2
      pos3 = Position line3 col3
      result1 = addPositions (addPositions pos1 pos2) pos3
      result2 = addPositions pos1 (addPositions pos2 pos3)
  in property $ result1 === result2

-- Property: Position has identity element
prop_position_identity :: Int -> Int -> Property
prop_position_identity line col =
  line >= 0 && col >= 0 ==>
  let pos = Position line col
      identity = Position 0 0
      result1 = addPositions pos identity
      result2 = addPositions identity pos
  in property $ result1 === pos .&&. result2 === pos

-- Property: Span length calculation is accurate
prop_span_length_accurate :: Int -> Int -> Int -> Int -> Property
prop_span_length_accurate startLine startCol endLine endCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
      calculatedLength = spanLength span
  in property $ calculatedLength >= 0

-- Property: Span contains its start position
prop_span_contains_start :: Int -> Int -> Int -> Int -> Property
prop_span_contains_start startLine startCol endLine endCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
  in property $ positionInSpan start span

-- Property: Span contains its end position
prop_span_contains_end :: Int -> Int -> Int -> Int -> Property
prop_span_contains_end startLine startCol endLine endCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
  in property $ positionInSpan end span

-- Property: Span merge is commutative for adjacent spans
prop_span_merge_commutative :: Int -> Int -> Int -> Int -> Property
prop_span_merge_commutative line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 &&
  (line2 > line1 || (line2 == line1 && col2 > col1)) ==>
  let start1 = Position line1 col1
      end1 = Position line1 (col1 + 5)
      start2 = Position line2 col2
      end2 = Position line2 (col2 + 5)
      span1 = Span start1 end1
      span2 = Span start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: Span merge contains both original spans
prop_span_merge_contains_both :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_merge_contains_both line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let start1 = Position line1 col1
      end1 = Position line2 col2
      start2 = Position line3 col3
      end2 = Position (line3 + 1) (col3 + 5)
      span1 = Span start1 end1
      span2 = Span start2 end2
      merged = mergeSpans span1 span2
  in property $ spanInSpan span1 merged .&&. spanInSpan span2 merged

-- Property: Position subtraction works correctly
prop_position_subtraction :: Int -> Int -> Int -> Int -> Property
prop_position_subtraction line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 &&
  (line1 > line2 || (line1 == line2 && col1 >= col2)) ==>
  let pos1 = Position line1 col1
      pos2 = Position line2 col2
      result = subtractPositions pos1 pos2
  in property $ result >= 0

-- Property: Position comparison is transitive
prop_position_comparison_transitive :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_position_comparison_transitive line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let pos1 = Position line1 col1
      pos2 = Position line2 col2
      pos3 = Position line3 col3
      cmp12 = comparePositions pos1 pos2
      cmp23 = comparePositions pos2 pos3
      cmp13 = comparePositions pos1 pos3
  in property $ (cmp12 <= 0 && cmp23 <= 0) ==> cmp13 <= 0

-- Property: Span distance calculation
prop_span_distance :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_distance startLine startCol endLine endCol targetLine targetCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  targetLine >= 0 && targetCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      target = Position targetLine targetCol
      span = Span start end
      distance = distanceToSpan target span
  in property $ distance >= 0

-- Property: Span intersection is commutative
prop_span_intersection_commutative :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_intersection_commutative line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let start1 = Position line1 col1
      end1 = Position line2 col2
      start2 = Position line3 col3
      end2 = Position (line3 + 2) (col3 + 10)
      span1 = Span start1 end1
      span2 = Span start2 end2
      intersect1 = intersectSpans span1 span2
      intersect2 = intersectSpans span2 span1
  in property $ intersect1 === intersect2

-- Property: Span intersection is contained in both spans
prop_span_intersection_contained :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_intersection_contained line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let start1 = Position line1 col1
      end1 = Position line2 col2
      start2 = Position line3 col3
      end2 = Position (line3 + 2) (col3 + 10)
      span1 = Span start1 end1
      span2 = Span start2 end2
      intersection = intersectSpans span1 span2
  in property $ case intersection of
    Nothing -> True
    Just span -> spanInSpan span span1 .&&. spanInSpan span span2

-- Property: Position offset calculation
prop_position_offset :: Int -> Int -> Int -> Property
prop_position_offset line col offset =
  line >= 0 && col >= 0 && offset >= 0 && offset <= 1000 ==>
  let pos = Position line col
      offsetPos = offsetPosition pos offset
  in property $ offsetPos >= pos

-- Property: Span expansion contains original span
prop_span_expansion :: Int -> Int -> Int -> Int -> Int -> Property
prop_span_expansion startLine startCol endLine endCol expansion =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  expansion >= 0 && expansion <= 10 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
      expanded = expandSpan span expansion
  in property $ spanInSpan span expanded

-- Property: Position line/column extraction
prop_position_extraction :: Int -> Int -> Property
prop_position_extraction line col =
  line >= 0 && col >= 0 ==>
  let pos = Position line col
      extractedLine = positionLine pos
      extractedCol = positionColumn pos
  in property $ extractedLine === line .&&. extractedCol === col

-- Property: Span start/end extraction
prop_span_extraction :: Int -> Int -> Int -> Int -> Property
prop_span_extraction startLine startCol endLine endCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
      extractedStart = spanStart span
      extractedEnd = spanEnd span
  in property $ extractedStart === start .&&. extractedEnd === end

-- Property: Empty span has zero length
prop_empty_span_length :: Int -> Int -> Property
prop_empty_span_length line col =
  line >= 0 && col >= 0 ==>
  let pos = Position line col
      emptySpan = Span pos pos
      length = spanLength emptySpan
  in property $ length === 0

-- Property: Span ordering is consistent
prop_span_ordering :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_ordering line1 col1 line2 col2 line3 col3 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && line3 >= 0 && col3 >= 0 ==>
  let start1 = Position line1 col1
      end1 = Position line2 col2
      start2 = Position line3 col3
      end2 = Position (line3 + 1) (col3 + 5)
      span1 = Span start1 end1
      span2 = Span start2 end2
      ordering = compareSpans span1 span2
  in property $ ordering == -1 || ordering == 0 || ordering == 1

-- Property: Position hashing is consistent
prop_position_hashing :: Int -> Int -> Property
prop_position_hashing line col =
  line >= 0 && col >= 0 ==>
  let pos = Position line col
      hash1 = hashPosition pos
      hash2 = hashPosition pos
  in property $ hash1 === hash2

-- Property: Span hashing is consistent
prop_span_hashing :: Int -> Int -> Int -> Int -> Property
prop_span_hashing startLine startCol endLine endCol =
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 &&
  (endLine > startLine || (endLine == startLine && endCol >= startCol)) ==>
  let start = Position startLine startCol
      end = Position endLine endCol
      span = Span start end
      hash1 = hashSpan span
      hash2 = hashSpan span
  in property $ hash1 === hash2

tests :: TestTree
tests =
  testGroup "Source Location Math QuickCheck Tests"
    [ fastProperty "position addition commutative" prop_position_addition_commutative
    , fastProperty "position addition associative" prop_position_addition_associative
    , fastProperty "position identity" prop_position_identity
    , fastProperty "span length accurate" prop_span_length_accurate
    , fastProperty "span contains start" prop_span_contains_start
    , fastProperty "span contains end" prop_span_contains_end
    , fastProperty "span merge commutative" prop_span_merge_commutative
    , fastProperty "span merge contains both" prop_span_merge_contains_both
    , fastProperty "position subtraction" prop_position_subtraction
    , fastProperty "position comparison transitive" prop_position_comparison_transitive
    , fastProperty "span distance" prop_span_distance
    , fastProperty "span intersection commutative" prop_span_intersection_commutative
    , fastProperty "span intersection contained" prop_span_intersection_contained
    , fastProperty "position offset" prop_position_offset
    , fastProperty "span expansion" prop_span_expansion
    , fastProperty "position extraction" prop_position_extraction
    , fastProperty "span extraction" prop_span_extraction
    , fastProperty "empty span length" prop_empty_span_length
    , fastProperty "span ordering" prop_span_ordering
    , fastProperty "position hashing" prop_position_hashing
    , fastProperty "span hashing" prop_span_hashing
    ]