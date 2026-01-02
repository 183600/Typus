{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
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
  , advancePosByText
  , advancePosByLine
  )

import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- Property: SourcePos ordering is consistent
prop_source_pos_ordering :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering pos1 pos2 =
  let ord1 = compare pos1 pos2
      ord2 = compare (posOffset pos1) (posOffset pos2)
  in property $ ord1 === ord2

-- Property: startPos has correct values
prop_start_pos_values :: Property
prop_start_pos_values =
  property $ posLine startPos === 1 .&&. posColumn startPos === 1 .&&. posOffset startPos === 0

-- Property: posAfter handles newline correctly
prop_pos_after_newline :: SourcePos -> Property
prop_pos_after_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&. posColumn newPos === 1 .&&. posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly
prop_pos_after_tab :: SourcePos -> Property
prop_pos_after_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedColumn .&&. posOffset newPos === posOffset pos + 1

-- Property: posAfter handles regular character correctly
prop_pos_after_regular :: SourcePos -> Char -> Property
prop_pos_after_regular pos char =
  char `notElem` "\n\t" ==>
  let newPos = posAfter char pos
  in property $ posColumn newPos === posColumn pos + 1 .&&. posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_pos_at_correct :: Int -> Int -> Property
prop_pos_at_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates position with correct values
prop_pos_at_line_col_correct :: Int -> Int -> Int -> Property
prop_pos_at_line_col_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- Property: emptySpan has start equal to end
prop_empty_span_start_end :: Property
prop_empty_span_start_end =
  let span = emptySpan
  in property $ spanStart span === spanEnd span

-- Property: spanFrom creates span from position
prop_span_from_creates :: SourcePos -> Property
prop_span_from_creates pos =
  let span = spanFrom pos
  in property $ spanStart span === pos

-- Property: spanTo creates span to position
prop_span_to_creates :: SourcePos -> Property
prop_span_to_creates pos =
  let span = spanTo pos
  in property $ spanEnd span === pos

-- Property: spanBetween creates span between two positions
prop_span_between_correct :: SourcePos -> SourcePos -> Property
prop_span_between_correct pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ spanStart span === pos1 .&&. spanEnd span === pos2

-- Property: mergeSpans creates span covering both spans
prop_merge_spans_correct :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_correct span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      expectedStart = if posOffset start1 <= posOffset start2 then start1 else start2
      expectedEnd = if posOffset end1 >= posOffset end2 then end1 else end2
  in property $ spanStart merged === expectedStart .&&. spanEnd merged === expectedEnd

-- Property: isValidSpan checks span validity
prop_is_valid_span :: SourceSpan -> Property
prop_is_valid_span span =
  let start = spanStart span
      end = spanEnd span
      valid = posOffset start <= posOffset end
  in property $ isValidSpan span === valid

-- Property: locatedAt creates located value
prop_located_at_creates :: SourcePos -> String -> Property
prop_located_at_creates pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos .&&. locatedValue located === value

-- Property: locatedWithSpan creates located value with span
prop_located_with_span_creates :: SourceSpan -> String -> Property
prop_located_with_span_creates span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&. locatedValue located === value

-- Property: mapLocated transforms located value
prop_map_located_transforms :: SourcePos -> String -> Property
prop_map_located_transforms pos value =
  let located = locatedAt pos value
      transformed = mapLocated (L.length) located
  in property $ locatedValue transformed === L.length value

-- Property: advancePos advances position by character
prop_advance_pos_correct :: SourcePos -> Char -> Property
prop_advance_pos_correct pos char =
  let advanced = advancePos pos char
      expected = posAfter char pos
  in property $ advanced === expected

-- Property: advancePosBy advances position by multiple characters
prop_advance_pos_by_correct :: SourcePos -> String -> Property
prop_advance_pos_by_correct pos text =
  let advanced = advancePosBy pos text
      expected = foldl posAfter pos text
  in property $ advanced === expected

-- Property: advancePosByText advances position by text
prop_advance_pos_by_text_correct :: SourcePos -> String -> Property
prop_advance_pos_by_text_correct pos text =
  let advanced = advancePosByText pos text
      expected = advancePosBy pos text
  in property $ advanced === expected

-- Property: advancePosByLine advances to next line
prop_advance_pos_by_line_correct :: SourcePos -> Property
prop_advance_pos_by_line_correct pos =
  let advanced = advancePosByLine pos
      expected = pos { posLine = posLine pos + 1, posColumn = 1, posOffset = posOffset pos + 1 }
  in property $ advanced === expected

-- Property: Source position arithmetic is consistent
prop_source_pos_arithmetic :: SourcePos -> String -> Property
prop_source_pos_arithmetic pos text =
  let advanced1 = advancePosBy pos text
      advanced2 = foldl advancePos pos text
  in property $ advanced1 === advanced2

-- Property: Source span ordering is consistent
prop_source_span_ordering :: SourceSpan -> SourceSpan -> Property
prop_source_span_ordering span1 span2 =
  let start1 = spanStart span1
      start2 = spanStart span2
      ord1 = compare span1 span2
      ord2 = compare (posOffset start1) (posOffset start2)
  in property $ ord1 === ord2

-- Property: Located values preserve location
prop_located_preserves_location :: SourcePos -> String -> Property
prop_located_preserves_location pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos

-- Property: Located values preserve value
prop_located_preserves_value :: SourcePos -> String -> Property
prop_located_preserves_value pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value

-- Property: mergeSpans is commutative for spans
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans is associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: spanBetween is symmetric for reversed positions
prop_span_between_symmetric :: SourcePos -> SourcePos -> Property
prop_span_between_symmetric pos1 pos2 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
  in property $ spanStart span1 === spanEnd span2 .&&. spanEnd span1 === spanStart span2

tests :: TestTree
tests = testGroup "New SourceLocation Advanced QuickCheck"
  [ fastProperty "source pos ordering" prop_source_pos_ordering
  , fastProperty "start pos values" prop_start_pos_values
  , fastProperty "pos after newline" prop_pos_after_newline
  , fastProperty "pos after tab" prop_pos_after_tab
  , fastProperty "pos after regular" prop_pos_after_regular
  , fastProperty "pos at correct" prop_pos_at_correct
  , fastProperty "pos at line col correct" prop_pos_at_line_col_correct
  , fastProperty "empty span start end" prop_empty_span_start_end
  , fastProperty "span from creates" prop_span_from_creates
  , fastProperty "span to creates" prop_span_to_creates
  , fastProperty "span between correct" prop_span_between_correct
  , fastProperty "merge spans correct" prop_merge_spans_correct
  , fastProperty "is valid span" prop_is_valid_span
  , fastProperty "located at creates" prop_located_at_creates
  , fastProperty "located with span creates" prop_located_with_span_creates
  , fastProperty "map located transforms" prop_map_located_transforms
  , fastProperty "advance pos correct" prop_advance_pos_correct
  , fastProperty "advance pos by correct" prop_advance_pos_by_correct
  , fastProperty "advance pos by text correct" prop_advance_pos_by_text_correct
  , fastProperty "advance pos by line correct" prop_advance_pos_by_line_correct
  , fastProperty "source pos arithmetic" prop_source_pos_arithmetic
  , fastProperty "source span ordering" prop_source_span_ordering
  , fastProperty "located preserves location" prop_located_preserves_location
  , fastProperty "located preserves value" prop_located_preserves_value
  , fastProperty "merge spans commutative" prop_merge_spans_commutative
  , fastProperty "merge spans associative" prop_merge_spans_associative
  , fastProperty "span between symmetric" prop_span_between_symmetric
  ]