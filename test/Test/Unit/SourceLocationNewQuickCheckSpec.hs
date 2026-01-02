{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
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
  , advancePosByLine
  )

import Data.Char (isSpace)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.List (sort)

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

-- Property: startPos has line 1, column 1, offset 0
prop_startPos_properties :: Property
prop_startPos_properties =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter increments correctly for regular characters
prop_posAfter_regular_char :: Char -> SourcePos -> Property
prop_posAfter_regular_char c pos =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter increments line for newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-column alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: posAtLineCol creates position with correct line, column, L.and offset
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

-- Property: emptySpan creates span with same start L.and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_creates_empty :: SourcePos -> Property
prop_spanFrom_creates_empty pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_creates_empty :: SourcePos -> Property
prop_spanTo_creates_empty pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanBetween creates span with correct start L.and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
             spanEnd span === end

-- Property: mergeSpans creates span with L.minimum start L.and L.maximum end
prop_mergeSpans_correct :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_correct start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in property $ spanStart merged === min start1 start2 .&&.
             spanEnd merged === max end1 end2

-- Property: isValidSpan returns True when start <= end
prop_isValidSpan_true :: SourcePos -> SourcePos -> Property
prop_isValidSpan_true start end =
  start <= end ==>
  let span = spanBetween start end
  in property $ isValidSpan span === True

-- Property: isValidSpan returns False when start > end
prop_isValidSpan_false :: SourcePos -> SourcePos -> Property
prop_isValidSpan_false start end =
  start > end ==>
  let span = spanBetween start end
  in property $ isValidSpan span === False

-- ============================================================================
-- Located Properties
-- ============================================================================

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: String -> SourcePos -> Property
prop_locatedAt_correct value pos =
  let located = locatedAt value pos
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: String -> SourceSpan -> Property
prop_locatedWithSpan_correct value span =
  let located = locatedWithSpan value span
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated preserves location but transforms value
prop_mapLocated_preserves_location :: String -> String -> SourcePos -> Property
prop_mapLocated_preserves_location value1 value2 pos =
  let located1 = locatedAt value1 pos
      located2 = mapLocated (const value2) located1
  in property $ locatedValue located2 === value2 .&&.
             locatedPos located2 === locatedPos located1 .&&.
             locatedSpan located2 === locatedSpan located1

-- ============================================================================
-- Advanced Properties
-- ============================================================================

-- Property: advancePos with empty string returns original position
prop_advancePos_empty_string :: SourcePos -> Property
prop_advancePos_empty_string pos =
  advancePos "" pos === pos

-- Property: advancePos with single character matches posAfter
prop_advancePos_single_char :: Char -> SourcePos -> Property
prop_advancePos_single_char c pos =
  advancePos [c] pos === posAfter c pos

-- Property: advancePos is associative
prop_advancePos_associative :: String -> String -> SourcePos -> Property
prop_advancePos_associative str1 str2 pos =
  advancePos (str1 ++ str2) pos === advancePos str2 (advancePos str1 pos)

-- Property: advancePosBy increments offset by string L.length
prop_advancePosBy_offset :: String -> SourcePos -> Property
prop_advancePosBy_offset str pos =
  let newPos = advancePosBy str pos
  in property $ posOffset newPos === posOffset pos + L.length str

-- Property: advancePosByLine increments line by newline count
prop_advancePosByLine_newlines :: String -> SourcePos -> Property
prop_advancePosByLine_newlines str pos =
  let newlineCount = L.length (L.filter (== '\n') str)
      newPos = advancePosByLine str pos
  in property $ posLine newPos === posLine pos + newlineCount

-- Property: Position ordering is consistent with offset
prop_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent pos1 pos2 =
  (pos1 <= pos2) === (posOffset pos1 <= posOffset pos2)

-- Property: Span ordering is consistent with start positions
prop_span_ordering_consistent :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_ordering_consistent start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
  in (span1 <= span2) === (spanStart span1 <= spanStart span2)

-- Property: Merging spans is commutative
prop_mergeSpans_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_commutative start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
  in mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: Merging spans is associative
prop_mergeSpans_associative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_associative start1 end1 start2 end2 start3 end3 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      span3 = spanBetween start3 end3
  in mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: Merging with empty span preserves other span
prop_mergeSpans_empty_identity :: SourcePos -> SourcePos -> Property
prop_mergeSpans_empty_identity start end =
  let span = spanBetween start end
      empty = emptySpan start
  in mergeSpans span empty === span

-- Property: Located values can be compared by position
prop_located_comparison :: String -> String -> SourcePos -> SourcePos -> Property
prop_located_comparison value1 value2 pos1 pos2 =
  let loc1 = locatedAt value1 pos1
      loc2 = locatedAt value2 pos2
  in (loc1 <= loc2) === (pos1 <= pos2)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation New QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "startPos has correct values" prop_startPos_properties
    , fastProperty "posAfter handles regular characters" prop_posAfter_regular_char
    , fastProperty "posAfter handles newline" prop_posAfter_newline
    , fastProperty "posAfter handles tab alignment" prop_posAfter_tab
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    ]
  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span" prop_spanFrom_creates_empty
    , fastProperty "spanTo creates empty span" prop_spanTo_creates_empty
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans creates correct merged span" prop_mergeSpans_correct
    , fastProperty "isValidSpan returns True for valid spans" prop_isValidSpan_true
    , fastProperty "isValidSpan returns False for invalid spans" prop_isValidSpan_false
    ]
  , testGroup "Located Properties"
    [ fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates correct located value" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    ]
  , testGroup "Advanced Properties"
    [ fastProperty "advancePos with empty string" prop_advancePos_empty_string
    , fastProperty "advancePos with single character" prop_advancePos_single_char
    , fastProperty "advancePos is associative" prop_advancePos_associative
    , fastProperty "advancePosBy increments offset" prop_advancePosBy_offset
    , fastProperty "advancePosByLine counts newlines" prop_advancePosByLine_newlines
    , fastProperty "position ordering consistent with offset" prop_position_ordering_consistent
    , fastProperty "span ordering consistent with start" prop_span_ordering_consistent
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "mergeSpans empty identity" prop_mergeSpans_empty_identity
    , fastProperty "located values compared by position" prop_located_comparison
    ]
  ]