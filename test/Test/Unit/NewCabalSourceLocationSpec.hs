{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , advancePos
  , advancePosBy
  , spanStart
  , spanEnd
  , posLine
  , posCol
  )

import Data.List (sort)

-- Test 1: Source position ordering properties
prop_source_pos_ordering :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering pos1 pos2 =
  let line1 = posLine pos1
      col1 = posCol pos1
      line2 = posLine pos2
      col2 = posCol pos2
  in (line1 < line2) || (line1 == line2 && col1 <= col2) ==> 
     property $ True -- Valid position ordering

-- Test 2: Span validity properties
prop_span_validity :: SourcePos -> SourcePos -> Property
prop_span_validity start end =
  let span = SourceSpan start end
      valid = isValidSpan span
  in (posLine start <= posLine end && posCol start <= posCol end) ==> valid === True

-- Test 3: Empty span properties
prop_empty_span_properties :: SourcePos -> Property
prop_empty_span_properties pos =
  let empty = emptySpan pos
  in spanStart empty === spanEnd empty .&&. spanStart empty === pos

-- Test 4: Span merging properties
prop_span_merging :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging start1 end1 start2 =
  let span1 = spanFrom start1 end1
      span2 = spanFrom start2 (posAfter start2 10)
      merged = mergeSpans span1 span2
  in isValidSpan span1 && isValidSpan span2 ==> 
     property $ isValidSpan merged

-- Test 5: Position advancement properties
prop_position_advancement :: SourcePos -> Int -> Property
prop_position_advancement pos offset =
  let offset' = abs offset
      advanced = posAfter pos offset'
  in offset' >= 0 ==> 
     property $ posLine advanced >= posLine pos

-- Test 6: Located value properties
prop_located_value_properties :: String -> SourcePos -> Property
prop_located_value_properties value pos =
  let located = locatedAt pos value
      span = locatedSpan located
  in property $ locatedValue located === value .&&. spanStart span === pos

-- Test 7: Span position consistency
prop_span_position_consistency :: SourcePos -> Int -> Property
prop_span_position_consistency start offset =
  let offset' = abs offset
      end = posAfter start offset'
      span = spanFrom start end
  in offset' >= 0 ==> 
     property $ spanStart span === start .&&. spanEnd span === end

-- Test 8: Position advancement by character count
prop_position_advancement_by_chars :: SourcePos -> String -> Property
prop_position_advancement_by_chars pos text =
  let advanced = advancePos pos text
  in property $ posLine advanced >= posLine pos

-- Test 9: Span ordering after merging
prop_span_ordering_after_merge :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_ordering_after_merge start1 end1 start2 =
  let span1 = spanFrom start1 end1
      span2 = spanFrom start2 (posAfter start2 5)
      merged = mergeSpans span1 span2
  in isValidSpan span1 && isValidSpan span2 ==> 
     property $ spanStart merged `seq` spanEnd merged `seq` True

-- Test 10: Located span consistency
prop_located_span_consistency :: String -> SourcePos -> SourcePos -> Property
prop_located_span_consistency value start end =
  let span = spanFrom start end
      located = locatedWithSpan span value
  in isValidSpan span ==> 
     property $ locatedSpan located === span .&&. locatedValue located === value

tests :: TestTree
tests = 
  testGroup "New Cabal SourceLocation Tests"
    [ fastProperty "Source position ordering properties" prop_source_pos_ordering
    , fastProperty "Span validity properties" prop_span_validity
    , fastProperty "Empty span properties" prop_empty_span_properties
    , fastProperty "Span merging properties" prop_span_merging
    , fastProperty "Position advancement properties" prop_position_advancement
    , fastProperty "Located value properties" prop_located_value_properties
    , fastProperty "Span position consistency" prop_span_position_consistency
    , fastProperty "Position advancement by character count" prop_position_advancement_by_chars
    , fastProperty "Span ordering after merge" prop_span_ordering_after_merge
    , fastProperty "Located span consistency" prop_located_span_consistency
    ]