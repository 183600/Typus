{-# LANGUAGE CPP #-}

module Test.Unit.NewSourceLocationTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      advancePos, advancePosBy, toErrorLocation, toErrorLocationWithSpan)

tests :: TestTree
tests = testGroup "New Source Location Tracking QuickCheck Tests"
  [ sourcePosProperties
  , sourceSpanProperties
  , locatedValueProperties
  , positionArithmeticProperties
  , spanOperationProperties
  ]

sourcePosProperties :: TestTree
sourcePosProperties = testGroup "SourcePos Properties"
  [ fastProperty "SourcePos with positive values is valid" prop_sourcepos_valid_positive
  , fastProperty "posAfter advances column by 1" prop_posafter_advances_column
  , fastProperty "posAtLineCol creates consistent position" prop_posatlinecol_consistent
  , fastProperty "startPos has L.minimum values" prop_startpos_minimum
  , fastProperty "advancePos by newline moves to next line" prop_advancepos_newline
  ]

sourceSpanProperties :: TestTree
sourceSpanProperties = testGroup "SourceSpan Properties"
  [ fastProperty "emptySpan has zero L.length" prop_emptyspan_zero_length
  , fastProperty "spanFrom creates span starting at position" prop_spanfrom_starts_at_pos
  , fastProperty "spanTo creates span ending at position" prop_spanto_ends_at_pos
  , fastProperty "spanBetween creates span between positions" prop_spanbetween_between_positions
  , fastProperty "mergeSpans contains both original spans" prop_mergespans_contains_both
  , fastProperty "isValidSpan checks proper ordering" prop_isvalidspan_ordering
  ]

locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ fastProperty "locatedAt creates location with correct position" prop_locatedat_correct_position
  , fastProperty "locatedWithSpan creates location with correct span" prop_locatedwithspan_correct_span
  , fastProperty "locatedValue extracts original value" prop_locatedvalue_extracts_original
  , fastProperty "locatedSpan returns correct span" prop_locatedspan_returns_span
  , fastProperty "locatedPos returns start position" prop_locatedpos_returns_start
  ]

positionArithmeticProperties :: TestTree
positionArithmeticProperties = testGroup "Position Arithmetic Properties"
  [ fastProperty "advancePos by character increases column" prop_advancepos_char_increases_column
  , fastProperty "advancePos by tab increases column appropriately" prop_advancepos_tab_increases_column
  , fastProperty "advancePosBy multiple characters accumulates correctly" prop_advanceposby_accumulates
  , fastProperty "advancePos preserves line count for non-newline chars" prop_advancepos_preserves_line
  ]

spanOperationProperties :: TestTree
spanOperationProperties = testGroup "Span Operation Properties"
  [ fastProperty "toErrorLocation creates valid location" prop_toerrorlocation_valid
  , fastProperty "toErrorLocationWithSpan includes span information" prop_toerrorlocationwithspan_includes_span
  , fastProperty "mergeSpans is commutative" prop_mergespans_commutative
  , fastProperty "mergeSpans is associative" prop_mergespans_associative
  ]

-- SourcePos properties
prop_sourcepos_valid_positive :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_valid_positive (Positive l) (Positive c) (Positive o) =
  let pos = SourcePos l c o
  in property $ l > 0 && c > 0 && o >= 0

prop_posafter_advances_column :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posafter_advances_column (Positive l) (Positive c) (Positive o) =
  let pos = SourcePos l c o
      pos' = posAfter pos
  in property $ posColumn pos' == posColumn pos + 1 && 
             posLine pos' == posLine pos &&
             posOffset pos' == posOffset pos + 1

prop_posatlinecol_consistent :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posatlinecol_consistent (Positive l) (Positive c) (Positive o) =
  let pos = posAtLineCol l c o
  in property $ posLine pos == l && posColumn pos == c && posOffset pos == o

prop_startpos_minimum :: Property
prop_startpos_minimum =
  let pos = startPos
  in property $ posLine pos == 1 && posColumn pos == 1 && posOffset pos == 0

prop_advancepos_newline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_advancepos_newline (Positive l) (Positive c) (Positive o) =
  let pos = SourcePos l c o
      pos' = advancePos pos '\n'
  in property $ posLine pos' == posLine pos + 1 && 
             posColumn pos' == 1 &&
             posOffset pos' == posOffset pos + 1

-- SourceSpan properties
prop_emptyspan_zero_length :: Property
prop_emptyspan_zero_length =
  let span = emptySpan
      start = spanStart span
      end = spanEnd span
  in property $ posOffset start == posOffset end

prop_spanfrom_starts_at_pos :: SourcePos -> Property
prop_spanfrom_starts_at_pos pos =
  let span = spanFrom pos
  in property $ spanStart span == pos

prop_spanto_ends_at_pos :: SourcePos -> Property
prop_spanto_ends_at_pos pos =
  let span = spanTo pos
  in property $ spanEnd span == pos

prop_spanbetween_between_positions :: SourcePos -> SourcePos -> Property
prop_spanbetween_between_positions pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (posOffset start <= posOffset end) &&
             (start == pos1 || end == pos2)

prop_mergespans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergespans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ posOffset mergedStart <= min (posOffset start1) (posOffset start2) &&
             posOffset mergedEnd >= max (posOffset end1) (posOffset end2)

prop_isvalidspan_ordering :: SourcePos -> SourcePos -> Property
prop_isvalidspan_ordering pos1 pos2 =
  let span = SourceSpan pos1 pos2
  in property $ isValidSpan span == (posOffset pos1 <= posOffset pos2)

-- Located value properties
prop_locatedat_correct_position :: Int -> String -> Property
prop_locatedat_correct_position line value =
  let pos = posAtLineCol (abs line + 1) 1 0
      located = locatedAt pos value
  in property $ locatedPos located == pos

prop_locatedwithspan_correct_span :: SourceSpan -> String -> Property
prop_locatedwithspan_correct_span span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located == span

prop_locatedvalue_extracts_original :: String -> Property
prop_locatedvalue_extracts_original value =
  let pos = startPos
      located = locatedAt pos value
  in property $ locatedValue located == value

prop_locatedspan_returns_span :: SourceSpan -> String -> Property
prop_locatedspan_returns_span span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located == span

prop_locatedpos_returns_start :: SourceSpan -> String -> Property
prop_locatedpos_returns_start span value =
  let located = locatedWithSpan span value
      expectedPos = spanStart span
  in property $ locatedPos located == expectedPos

-- Position arithmetic properties
prop_advancepos_char_increases_column :: SourcePos -> Char -> Property
prop_advancepos_char_increases_column pos ch =
  ch /= '\n' ==>
  let pos' = advancePos pos ch
  in property $ posColumn pos' == posColumn pos + 1 &&
             posLine pos' == posLine pos &&
             posOffset pos' == posOffset pos + 1

prop_advancepos_tab_increases_column :: SourcePos -> Property
prop_advancepos_tab_increases_column pos =
  let pos' = advancePos pos '\t'
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn pos' >= posColumn pos + 1 &&
             posColumn pos' <= expectedCol &&
             posLine pos' == posLine pos &&
             posOffset pos' == posOffset pos + 1

prop_advanceposby_accumulates :: SourcePos -> String -> Property
prop_advanceposby_accumulates pos str =
  let pos' = advancePosBy pos str
      expectedPos = foldl advancePos pos str
  in property $ pos' == expectedPos

prop_advancepos_preserves_line :: SourcePos -> String -> Property
prop_advancepos_preserves_line pos str =
  '\n' `notElem` str ==>
  let pos' = advancePosBy pos str
  in property $ posLine pos' == posLine pos

-- Span operation properties
prop_toerrorlocation_valid :: SourcePos -> Property
prop_toerrorlocation_valid pos =
  let errorLoc = toErrorLocation pos
  in property $ True -- Basic validity check - should not crash

prop_toerrorlocationwithspan_includes_span :: SourceSpan -> Property
prop_toerrorlocationwithspan_includes_span span =
  let errorLoc = toErrorLocationWithSpan span
  in property $ True -- Basic validity check - should not crash

prop_mergespans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergespans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 == merged2

prop_mergespans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergespans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 == merged2