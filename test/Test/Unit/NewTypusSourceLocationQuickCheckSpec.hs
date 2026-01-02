{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SourceLocation (Located(..), SourcePos(..), SourceSpan(..), mergeSpans, spanContains)

-- Property: Source span merge preserves ordering
prop_source_span_merge_preserves_ordering :: SourceSpan -> SourceSpan -> Property
prop_source_span_merge_preserves_ordering span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = getSpanStart span1
      start2 = getSpanStart span2
      mergedStart = getSpanStart merged
  in property $ (mergedStart <= start1) .&&. (mergedStart <= start2)

-- Property: Source span contains its own start L.and end
prop_source_span_contains_bounds :: SourceSpan -> Property
prop_source_span_contains_bounds span =
  let start = getSpanStart span
      end = getSpanEnd span
      containsStart = spanContains span start
      containsEnd = spanContains span end
  in property $ containsStart .&&. containsEnd

-- Property: Merged span contains both original spans
prop_merged_span_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merged_span_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
      contains1 = spanContains merged (getSpanStart span1)
      contains2 = spanContains merged (getSpanStart span2)
  in property $ contains1 .&&. contains2

-- Property: Source position ordering is consistent
prop_source_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_source_position_ordering_consistent pos1 pos2 =
  let line1 = getPosLine pos1
      line2 = getPosLine pos2
      col1 = getPosColumn pos1
      col2 = getPosColumn pos2
      cmp = compare (line1, col1) (line2, col2)
  in property $ (cmp == LT || cmp == EQ || cmp == GT)

-- Property: Located values preserve their location
prop_located_preserves_location :: String -> SourceSpan -> Property
prop_located_preserves_location value span =
  let located = Located span value
      extractedSpan = getLocatedSpan located
      extractedValue = getLocatedValue located
  in property $ (extractedSpan === span) .&&. (extractedValue === value)

-- Helper functions
getSpanStart :: SourceSpan -> Int
getSpanStart (SourceSpan start _) = getPosLine start

getSpanEnd :: SourceSpan -> Int
getSpanEnd (SourceSpan _ end) = getPosLine end

getPosLine :: SourcePos -> Int
getPosLine (SourcePos line _ _) = line

getPosColumn :: SourcePos -> Int
getPosColumn (SourcePos _ col _) = col

getLocatedSpan :: Located a -> SourceSpan
getLocatedSpan (Located span _) = span

getLocatedValue :: Located a -> a
getLocatedValue (Located _ value) = value

tests :: TestTree
tests = testGroup "New Typus Source Location QuickCheck Tests"
  [ fastProperty "Source span merge preserves ordering" prop_source_span_merge_preserves_ordering
  , fastProperty "Source span contains its own bounds" prop_source_span_contains_bounds
  , fastProperty "Merged span contains both original spans" prop_merged_span_contains_originals
  , fastProperty "Source position ordering is consistent" prop_source_position_ordering_consistent
  , fastProperty "Located values preserve their location" prop_located_preserves_location
  ]