{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, 
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      advancePos, advancePosBy, sourceLine, sourceColumn)
import Data.Char (isSpace)
import qualified Data.List as List

-- Property: Source position advances correctly with newline
prop_source_pos_newline :: Int -> Int -> Property
prop_source_pos_newline line col =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 ==>
  let pos = SourcePos line col
      advanced = advancePos pos '\n'
  in sourceLine advanced === line + 1 .&&. sourceColumn advanced === 1

-- Property: Source position advances correctly with tab
prop_source_pos_tab :: Int -> Int -> Property
prop_source_pos_tab line col =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 ==>
  let pos = SourcePos line col
      advanced = advancePos pos '\t'
  in sourceLine advanced === line .&&. sourceColumn advanced >= col + 1

-- Property: Source position advances correctly with regular character
prop_source_pos_regular_char :: Int -> Int -> Char -> Property
prop_source_pos_regular_char line col ch =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 && 
  ch /= '\n' && ch /= '\t' && ch /= '\r' ==>
  let pos = SourcePos line col
      advanced = advancePos pos ch
  in sourceLine advanced === line .&&. sourceColumn advanced === col + 1

-- Property: Source span validity is consistent
prop_source_span_validity :: Int -> Int -> Int -> Int -> Property
prop_source_span_validity startLine startCol endLine endCol =
  startLine >= 1 && startLine <= 100 && startCol >= 1 && startCol <= 100 &&
  endLine >= 1 && endLine <= 100 && endCol >= 1 && endCol <= 100 ==>
  let startPos = SourcePos startLine startCol
      endPos = SourcePos endLine endCol
      span = SourceSpan startPos endPos
      valid = isValidSpan span
      shouldBeValid = startLine < endLine || (startLine == endLine && startCol <= endCol)
  in valid === shouldBeValid

-- Property: Empty span is always valid
prop_empty_span_valid :: Property
prop_empty_span_valid =
  let empty = emptySpan
  in property $ isValidSpan empty

-- Property: Span merging is commutative for overlapping spans
prop_span_merge_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merge_commutative span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property: Span merging is associative
prop_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merge_associative span1 span2 span3 =
  isValidSpan span1 && isValidSpan span2 && isValidSpan span3 ==>
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      merge123_1 = mergeSpans merge12 span3
      merge123_2 = mergeSpans span1 merge23
  in merge123_1 === merge123_2

-- Property: Located values preserve their content
prop_located_preserves_content :: String -> SourcePos -> Property
prop_located_preserves_content value pos =
  let located = locatedAt pos value
  in locatedValue located === value

-- Property: Located values preserve their position
prop_located_preserves_position :: String -> SourcePos -> Property
prop_located_preserves_position value pos =
  let located = locatedAt pos value
  in locatedPos located === pos

-- Property: Span between positions is correct
prop_span_between_positions :: SourcePos -> SourcePos -> Property
prop_span_between_positions pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in start === pos1 .&&. end === pos2

-- Property: Position advancement by string is consistent
prop_pos_advance_by_string :: SourcePos -> String -> Property
prop_pos_advance_by_string pos str =
  let advanced1 = foldl advancePos pos str
      advanced2 = advancePosBy pos str
  in advanced1 === advanced2

tests :: TestTree
tests = testGroup "New SourceLocation QuickCheck Tests"
  [ fastProperty "Source position advances with newline" prop_source_pos_newline
  , fastProperty "Source position advances with tab" prop_source_pos_tab
  , fastProperty "Source position advances with regular char" prop_source_pos_regular_char
  , fastProperty "Source span validity is consistent" prop_source_span_validity
  , fastProperty "Empty span is always valid" prop_empty_span_valid
  , fastProperty "Span merging is commutative" prop_span_merge_commutative
  , fastProperty "Span merging is associative" prop_span_merge_associative
  , fastProperty "Located values preserve content" prop_located_preserves_content
  , fastProperty "Located values preserve position" prop_located_preserves_position
  , fastProperty "Span between positions is correct" prop_span_between_positions
  ]