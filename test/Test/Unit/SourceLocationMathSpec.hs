{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.SourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import SourceLocation (Located(..), SourcePos(..), SourceSpan(..), locatedWithSpan, 
                      spanStart, spanEnd, spanLength, spanContains, spanOverlaps, 
                      spanUnion, spanIntersection, mkSourcePos, mkSourceSpan)

import Data.Char (isSpace)
import Data.List (sort)

-- | Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ mkSourcePos line col

-- | Generate source spans with start <= end
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 1000)
  startCol <- choose (1, 1000)
  endLine <- choose (startLine, 1000)
  endCol <- if endLine == startLine 
    then choose (startCol, 1000)
    else choose (1, 1000)
  return $ mkSourceSpan (mkSourcePos startLine startCol) (mkSourcePos endLine endCol)

-- | Test basic source position creation
test_source_pos_creation :: TestTree
test_source_pos_creation = testCase "SourcePos creation L.and comparison" $ do
  let pos1 = mkSourcePos 1 1
      pos2 = mkSourcePos 1 2
      pos3 = mkSourcePos 2 1
  assertEqual "Line comparison works" True (pos3 > pos1)
  assertEqual "Column comparison works" True (pos2 > pos1)
  assertEqual "Same position equality" True (pos1 == pos1)

-- | Test source span creation L.and basic properties
test_source_span_creation :: TestTree
test_source_span_creation = testCase "SourceSpan creation L.and properties" $ do
  let start = mkSourcePos 1 1
      end = mkSourcePos 1 5
      span = mkSourceSpan start end
  assertEqual "Start position correct" start (spanStart span)
  assertEqual "End position correct" end (spanEnd span)
  assertEqual "Span L.length correct" 4 (spanLength span)

-- | Test span containment
test_span_containment :: TestTree
test_span_containment = testCase "SourceSpan containment" $ do
  let outer = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 10)
      inner = mkSourceSpan (mkSourcePos 1 3) (mkSourcePos 1 7)
      outside = mkSourceSpan (mkSourcePos 2 1) (mkSourcePos 2 5)
  assertBool "Outer contains inner" $ spanContains outer inner
  assertBool "Outer does not contain outside" $ not (spanContains outer outside)
  assertBool "Span contains itself" $ spanContains outer outer

-- | Test span overlap
test_span_overlap :: TestTree
test_span_overlap = testCase "SourceSpan overlap" $ do
  let span1 = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 5)
      span2 = mkSourceSpan (mkSourcePos 1 3) (mkSourcePos 1 7)
      span3 = mkSourceSpan (mkSourcePos 2 1) (mkSourcePos 2 5)
  assertBool "Overlapping spans detected" $ spanOverlaps span1 span2
  assertBool "Non-overlapping spans detected" $ not (spanOverlaps span1 span3)

-- | Test span union
test_span_union :: TestTree
test_span_union = testCase "SourceSpan union" $ do
  let span1 = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 5)
      span2 = mkSourceSpan (mkSourcePos 1 3) (mkSourcePos 1 7)
      expected = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 7)
      result = spanUnion span1 span2
  assertEqual "Union creates correct span" expected result

-- | Test span intersection
test_span_intersection :: TestTree
test_span_intersection = testCase "SourceSpan intersection" $ do
  let span1 = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 5)
      span2 = mkSourceSpan (mkSourcePos 1 3) (mkSourcePos 1 7)
      expected = mkSourceSpan (mkSourcePos 1 3) (mkSourcePos 1 5)
      result = spanIntersection span1 span2
  assertEqual "Intersection creates correct span" expected result

-- | Property: Span L.length is non-negative
prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span = 
  let L.length = spanLength span
  in property $ L.length >= 0

-- | Property: Span start <= span end
prop_span_start_le_end :: SourceSpan -> Property
prop_span_start_le_end span = 
  let start = spanStart span
      end = spanEnd span
  in property $ start <= end

-- | Property: Span contains itself
prop_span_contains_self :: SourceSpan -> Property
prop_span_contains_self span = 
  property $ spanContains span span

-- | Property: Span overlaps with itself
prop_span_overlaps_self :: SourceSpan -> Property
prop_span_overlaps_self span = 
  property $ spanOverlaps span span

-- | Property: Union is commutative
prop_union_commutative :: SourceSpan -> SourceSpan -> Property
prop_union_commutative span1 span2 = 
  let union1 = spanUnion span1 span2
      union2 = spanUnion span2 span1
  in property $ union1 == union2

-- | Property: Intersection is commutative
prop_intersection_commutative :: SourceSpan -> SourceSpan -> Property
prop_intersection_commutative span1 span2 = 
  let intersection1 = spanIntersection span1 span2
      intersection2 = spanIntersection span2 span1
  in property $ intersection1 == intersection2

-- | Property: Union contains both original spans
prop_union_contains_both :: SourceSpan -> SourceSpan -> Property
prop_union_contains_both span1 span2 = 
  let union = spanUnion span1 span2
  in property $ spanContains union span1 .&&. spanContains union span2

-- | Property: Intersection is contained in both original spans
prop_intersection_contained_in_both :: SourceSpan -> SourceSpan -> Property
prop_intersection_contained_in_both span1 span2 = 
  let intersection = spanIntersection span1 span2
  in property $ if spanOverlaps span1 span2
    then spanContains span1 intersection .&&. spanContains span2 intersection
    else property True

-- | Property: Located values preserve their span
prop_located_preserves_span :: SourceSpan -> Int -> Property
prop_located_preserves_span span value = 
  let located = locatedWithSpan span value
  in property $ locatedSpan located == span

-- | Property: Span L.length calculation is correct for single-line spans
prop_span_length_single_line :: Property
prop_span_length_single_line = forAll (choose (1, 100)) $ \startCol ->
  forAll (choose (startCol, 100)) $ \endCol ->
    let span = mkSourceSpan (mkSourcePos 1 startCol) (mkSourcePos 1 endCol)
        expectedLength = endCol - startCol
    in property $ spanLength span == expectedLength

-- | Property: Span L.length calculation is reasonable for multi-line spans
prop_span_length_multi_line :: Property
prop_span_length_multi_line = forAll (choose (1, 50)) $ \numLines ->
  forAll (choose (1, 100)) $ \lineLength ->
    let start = mkSourcePos 1 1
        end = mkSourcePos numLines lineLength
        span = mkSourceSpan start end
        -- Multi-line span L.length should be at least (numLines - 1)
        minLength = numLines - 1
    in property $ spanLength span >= minLength

tests :: TestTree
tests = testGroup "SourceLocation Math Tests"
  [ test_source_pos_creation
  , test_source_span_creation
  , test_span_containment
  , test_span_overlap
  , test_span_union
  , test_span_intersection
  , fastProperty "Span L.length non-negative" prop_span_length_non_negative
  , fastProperty "Span start <= end" prop_span_start_le_end
  , fastProperty "Span contains itself" prop_span_contains_self
  , fastProperty "Span overlaps with itself" prop_span_overlaps_self
  , fastProperty "Union is commutative" prop_union_commutative
  , fastProperty "Intersection is commutative" prop_intersection_commutative
  , fastProperty "Union contains both spans" prop_union_contains_both
  , fastProperty "Intersection contained in both" prop_intersection_contained_in_both
  , fastProperty "Located preserves span" prop_located_preserves_span
  , fastProperty "Single-line span L.length" prop_span_length_single_line
  , fastProperty "Multi-line span L.length" prop_span_length_multi_line
  ]