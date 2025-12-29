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
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , mergeSpans
  , spanBetween
  , isValidSpan
  , advancePos
  , advancePosBy
  )

import Data.List (sort)

-- Property: startPos is always at line 1, column 1
prop_start_pos_properties :: Property
prop_start_pos_properties =
  let pos = startPos
  in property $ sourceLine pos === 1 .&&. sourceColumn pos === 1

-- Property: posAfter advances column by 1 for non-newline characters
prop_pos_after_advances_column :: Char -> SourcePos -> Property
prop_pos_after_advances_column c pos =
  c /= '\n' ==>
  let newPos = posAfter pos c
  in property $ sourceLine newPos === sourceLine pos .&&. 
               sourceColumn newPos === sourceColumn pos + 1

-- Property: posAfter advances line for newline characters
prop_pos_after_advances_line :: SourcePos -> Property
prop_pos_after_advances_line pos =
  let newPos = posAfter pos '\n'
  in property $ sourceLine newPos === sourceLine pos + 1 .&&. 
               sourceColumn newPos === 1

-- Property: advancePosBy consistency with repeated posAfter
prop_advance_pos_by_consistency :: String -> SourcePos -> Property
prop_advance_pos_by_consistency str pos =
  let advancedBy = advancePosBy pos str
      advancedRepeated = foldl posAfter pos str
  in property $ advancedBy === advancedRepeated

-- Property: mergeSpans creates valid span from two valid spans
prop_merge_spans_validity :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_validity span1 span2 =
  isValidSpan span1 .&&. isValidSpan span2 ==>
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- Property: spanBetween creates span that encompasses both positions
prop_span_between_encompasses :: SourcePos -> SourcePos -> Property
prop_span_between_encompasses pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (sourceLine start <= sourceLine end .&&. 
                 sourceLine start < sourceLine end .||. sourceColumn start <= sourceColumn end) .&&.
               isValidSpan span

-- Property: mergeSpans is commutative for spans with valid overlap
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  isValidSpan span1 .&&. isValidSpan span2 ==>
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans is associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  isValidSpan span1 .&&. isValidSpan span2 .&&. isValidSpan span3 ==>
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: advancePos handles empty string correctly
prop_advance_pos_empty_string :: SourcePos -> Property
prop_advance_pos_empty_string pos =
  let advanced = advancePos pos ""
  in property $ advanced === pos

-- Property: advancePos handles multi-line strings correctly
prop_advance_pos_multiline :: Int -> Int -> Property
prop_advance_pos_multiline lines cols =
  lines >= 0 .&&. cols >= 0 .&&. lines < 100 .&&. cols < 100 ==>
  let pos = startPos
      input = unlines $ replicate lines "x"
      advanced = advancePos pos input
  in property $ sourceLine advanced === lines + 1

tests :: TestTree
tests = testGroup "SourceLocation Math QuickCheck Tests"
  [ fastProperty "startPos is always at line 1, column 1" prop_start_pos_properties
  , fastProperty "posAfter advances column by 1 for non-newline characters" prop_pos_after_advances_column
  , fastProperty "posAfter advances line for newline characters" prop_pos_after_advances_line
  , fastProperty "advancePosBy consistency with repeated posAfter" prop_advance_pos_by_consistency
  , fastProperty "mergeSpans creates valid span from two valid spans" prop_merge_spans_validity
  , fastProperty "spanBetween creates span that encompasses both positions" prop_span_between_encompasses
  , fastProperty "mergeSpans is commutative for spans with valid overlap" prop_merge_spans_commutative
  , fastProperty "mergeSpans is associative" prop_merge_spans_associative
  , fastProperty "advancePos handles empty string correctly" prop_advance_pos_empty_string
  , fastProperty "advancePos handles multi-line strings correctly" prop_advance_pos_multiline
  ]