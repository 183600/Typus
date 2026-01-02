{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Text (Text)
import qualified Data.Text as T

-- | Test suite for SourceLocation mathematical properties
tests :: TestTree
tests =
  testGroup "SourceLocation Mathematical Properties"
    [ testGroup "Position arithmetic properties"
        [ fastProperty "posAfter advances line correctly for newline" prop_posAfter_newline
        , fastProperty "posAfter advances column correctly for regular chars" prop_posAfter_regular
        , fastProperty "posAfter handles tab expansion correctly" prop_posAfter_tab
        , fastProperty "advancePosBy is consistent with repeated posAfter" prop_advancePosBy_consistency
        , fastProperty "position advancement is monotonic" prop_position_monotonic
        ]

    , testGroup "Span properties"
        [ fastProperty "emptySpan has same start L.and end" prop_empty_span_equality
        , fastProperty "spanBetween creates valid span" prop_span_between_valid
        , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
        , fastProperty "mergeSpans is associative" prop_merge_spans_associative
        , fastProperty "mergeSpans contains both inputs" prop_merge_spans_contains
        ]

    , testGroup "Located value properties"
        [ fastProperty "locatedAt creates valid location" prop_located_at_valid
        , fastProperty "mapLocated preserves position" prop_map_located_preserves
        , fastProperty "locatedValue extracts original value" prop_located_value_identity
        ]

    , testGroup "Error location conversion properties"
        [ fastProperty "toErrorLocation preserves line L.and column" prop_error_location_preserves
        , fastProperty "toErrorLocationWithSpan preserves range" prop_error_location_span_preserves
        ]

    , testGroup "Mathematical invariants"
        [ fastProperty "span L.length is non-negative" prop_span_length_non_negative
        , fastProperty "position ordering is total" prop_position_total_ordering
        , fastProperty "span containment is transitive" prop_span_containment_transitive
        ]
    ]

-- Position arithmetic properties

prop_posAfter_newline :: Int -> Property
prop_posAfter_newline line =
  line >= 0 && line <= 1000 ==> -- Reasonable bounds
  let pos = posAt line 10
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&. posColumn newPos === 1

prop_posAfter_regular :: Char -> Int -> Int -> Property
prop_posAfter_regular char line col =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 && char `notElem` "\n\t" ==>
  let pos = posAt line col
      newPos = posAfter char pos
  in property $ posLine newPos === line .&&. posColumn newPos === col + 1

prop_posAfter_tab :: Int -> Int -> Property
prop_posAfter_tab line col =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 ==>
  let pos = posAt line col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === line .&&. posColumn newPos === expectedCol

prop_advancePosBy_consistency :: String -> Int -> Int -> Property
prop_advancePosBy_consistency chars line col =
  line >= 0 && line <= 50 && col >= 0 && col <= 50 && L.length chars <= 20 ==>
  let pos = posAt line col
      advancedBy = advancePosBy chars pos
      advancedRepeated = L.foldl (flip posAfter) pos chars
  in property $ advancedBy === advancedRepeated

prop_position_monotonic :: String -> Int -> Int -> Property
prop_position_monotonic chars line col =
  line >= 0 && line <= 50 && col >= 0 && col <= 50 && L.length chars <= 20 ==>
  let pos = posAt line col
      advancedPos = advancePosBy chars pos
  in property $ posOffset advancedPos >= posOffset pos

-- Span properties

prop_empty_span_equality :: Int -> Int -> Property
prop_empty_span_equality line col =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 ==>
  let pos = posAt line col
      span = emptySpan pos
  in property $ spanStart span === spanEnd span

prop_span_between_valid :: Int -> Int -> Int -> Int -> Property
prop_span_between_valid line1 col1 line2 col2 =
  line1 >= 0 && line1 <= 100 && col1 >= 0 && col1 <= 100 &&
  line2 >= 0 && line2 <= 100 && col2 >= 0 && col2 <= 100 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ spanStart span === pos1 .&&. spanEnd span === pos2

prop_merge_spans_commutative :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_commutative line1 col1 line2 col2 line3 col3 =
  L.all (>=0) [line1, col1, line2, col2, line3, col3] &&
  L.all (<=100) [line1, col1, line2, col2, line3, col3] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

prop_merge_spans_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_associative line1 col1 line2 col2 line3 col3 line4 col4 =
  L.all (>=0) [line1, col1, line2, col2, line3, col3, line4, col4] &&
  L.all (<=100) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      pos4 = posAt line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos3 pos4
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

prop_merge_spans_contains :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains line1 col1 line2 col2 line3 col3 =
  L.all (>=0) [line1, col1, line2, col2, line3, col3] &&
  L.all (<=100) [line1, col1, line2, col2, line3, col3] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&. spanEnd merged >= spanEnd span2

-- Located value properties

prop_located_at_valid :: Int -> Int -> String -> Property
prop_located_at_valid line col value =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 && L.length value <= 20 ==>
  let pos = posAt line col
      located = locatedAt pos value
  in property $ locatedPos located === pos .&&. locatedValue located === value

prop_map_located_preserves :: Int -> Int -> String -> Property
prop_map_located_preserves line col value =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 && L.length value <= 20 ==>
  let pos = posAt line col
      located = locatedAt pos value
      mapped = mapLocated L.length located
  in property $ locatedPos located === locatedPos mapped .&&.
     locatedSpan located === locatedSpan mapped

prop_located_value_identity :: Int -> Int -> String -> Property
prop_located_value_identity line col value =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 && L.length value <= 20 ==>
  let pos = posAt line col
      located = locatedAt pos value
  in property $ locatedValue located === value

-- Error location conversion properties

prop_error_location_preserves :: Int -> Int -> Property
prop_error_location_preserves line col =
  line >= 0 && line <= 100 && col >= 0 && col <= 100 ==>
  let pos = posAt line col
      errorLoc = toErrorLocation pos
  in property $ line errorLoc === line .&&. column errorLoc === col

prop_error_location_span_preserves :: Int -> Int -> Int -> Int -> Property
prop_error_location_span_preserves line1 col1 line2 col2 =
  L.all (>=0) [line1, col1, line2, col2] &&
  L.all (<=100) [line1, col1, line2, col2] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      errorLoc = toErrorLocationWithSpan span
  in property $ line errorLoc === line1 .&&. column errorLoc === col1 .&&.
     endLine errorLoc === Just line2 .&&. endColumn errorLoc === Just col2

-- Mathematical invariants

prop_span_length_non_negative :: Int -> Int -> Int -> Int -> Property
prop_span_length_non_negative line1 col1 line2 col2 =
  L.all (>=0) [line1, col1, line2, col2] &&
  L.all (<=100) [line1, col1, line2, col2] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      L.length = posOffset pos2 - posOffset pos1
  in property $ L.length >= 0

prop_position_total_ordering :: Int -> Int -> Int -> Int -> Property
prop_position_total_ordering line1 col1 line2 col2 =
  L.all (>=0) [line1, col1, line2, col2] &&
  L.all (<=100) [line1, col1, line2, col2] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
  in property $ (pos1 <= pos2) .||. (pos2 <= pos1)

prop_span_containment_transitive :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_containment_transitive line1 col1 line2 col2 line3 col3 line4 col4 =
  L.all (>=0) [line1, col1, line2, col2, line3, col3, line4, col4] &&
  L.all (<=100) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      pos4 = posAt line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos3 pos4
      merged12 = mergeSpans span1 span2
      merged123 = mergeSpans merged12 span3
  in property $ spanStart merged123 <= spanStart span1 .&&. spanEnd merged123 >= spanEnd span3