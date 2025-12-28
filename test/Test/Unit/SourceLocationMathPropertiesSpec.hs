{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector)
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
  , advancePos
  , advancePosBy
  , advancePosByText
  )

import Data.Text (Text)
import qualified Data.Text as T

-- | Mathematical property tests for SourceLocation module
tests :: TestTree
tests = testGroup "SourceLocation Mathematical Properties"
  [ testGroup "Source Position Properties"
    [ fastProperty "posAfter newline increments line and resets column" prop_posAfter_newline_increments_line
    , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab_advances_to_tab_stop
    , fastProperty "posAfter regular char increments column" prop_posAfter_regular_char_increments_column
    , fastProperty "posAfter sequence is associative" prop_posAfter_sequence_associative
    , fastProperty "posAt creates valid position" prop_posAt_creates_valid_position
    , fastProperty "posAtLineCol preserves all coordinates" prop_posAtLineCol_preserves_coordinates
    ]

  , testGroup "Source Span Properties"
    [ fastProperty "emptySpan has zero length" prop_emptySpan_zero_length
    , fastProperty "spanFrom creates valid span" prop_spanFrom_creates_valid_span
    , fastProperty "spanTo creates valid span" prop_spanTo_creates_valid_span
    , fastProperty "spanBetween is commutative for merge" prop_spanBetween_commutative_for_merge
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "mergeSpans is idempotent" prop_mergeSpans_idempotent
    , fastProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
    ]

  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos preserves monotonicity" prop_advancePos_monotonic
    , fastProperty "advancePosBy is additive" prop_advancePosBy_additive
    , fastProperty "advancePosByText handles newlines correctly" prop_advancePosByText_newlines
    , fastProperty "advancePosByText handles tabs correctly" prop_advancePosByText_tabs
    , fastProperty "advancePosByText is length-preserving" prop_advancePosByText_length_preserving
    ]

  , testGroup "Span Validation Properties"
    [ fastProperty "isValidSpan detects invalid spans" prop_isValidSpan_detection
    , fastProperty "merged spans are valid if components are valid" prop_merged_spans_valid
    , fastProperty "span operations preserve validity" prop_span_operations_preserve_validity
    ]

  , testGroup "Edge Case Properties"
    [ fastProperty "position arithmetic handles large numbers" prop_position_arithmetic_large_numbers
    , fastProperty "span operations handle edge positions" prop_span_operations_edge_positions
    , fastProperty "text advancement handles empty text" prop_text_advancement_empty_text
    , fastProperty "position advancement handles unicode" prop_position_advancement_unicode
    ]
  ]

-- ============================================================================
-- Source Position Properties
-- ============================================================================

prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

prop_posAfter_tab_advances_to_tab_stop :: SourcePos -> Property
prop_posAfter_tab_advances_to_tab_stop pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

prop_posAfter_regular_char_increments_column :: SourcePos -> Char -> Property
prop_posAfter_regular_char_increments_column pos char =
  char `notElem` "\n\t" ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

prop_posAfter_sequence_associative :: SourcePos -> String -> Property
prop_posAfter_sequence_associative pos chars =
  not (null chars) ==>
  let sequential = foldl posAfter pos chars
      individual = foldl (\p c -> posAfter c p) pos chars
  in property $ sequential === individual

prop_posAt_creates_valid_position :: Int -> Int -> Property
prop_posAt_creates_valid_position line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

prop_posAtLineCol_preserves_coordinates :: Int -> Int -> Int -> Property
prop_posAtLineCol_preserves_coordinates line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

prop_emptySpan_zero_length :: Property
prop_emptySpan_zero_length =
  let span = emptySpan
  in property $ isValidSpan span .&&.
             spanStart span === spanEnd span

prop_spanFrom_creates_valid_span :: SourcePos -> Property
prop_spanFrom_creates_valid_span pos =
  let span = spanFrom pos
  in property $ isValidSpan span .&&.
             spanStart span === pos .&&.
             spanEnd span === pos

prop_spanTo_creates_valid_span :: SourcePos -> Property
prop_spanTo_creates_valid_span pos =
  let span = spanTo pos
  in property $ isValidSpan span .&&.
             spanStart span === pos .&&.
             spanEnd span === pos

prop_spanBetween_commutative_for_merge :: SourcePos -> SourcePos -> Property
prop_spanBetween_commutative_for_merge pos1 pos2 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merge12_3 = mergeSpans (mergeSpans span1 span2) span3
      merge1_23 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merge12_3 === merge1_23

prop_mergeSpans_idempotent :: SourceSpan -> Property
prop_mergeSpans_idempotent span =
  let merged = mergeSpans span span
  in property $ merged === span

prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  isValidSpan span1 .&&. isValidSpan span2 ==>
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ posLine mergedStart <= posLine start1 .&&.
             posLine mergedStart <= posLine start2 .&&.
             posLine mergedEnd >= posLine end1 .&&.
             posLine mergedEnd >= posLine end2

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

prop_advancePos_monotonic :: SourcePos -> String -> Property
prop_advancePos_monotonic pos text =
  not (null text) ==>
  let newPos = advancePosByText pos text
  in property $ posOffset newPos >= posOffset pos .&&.
             (posLine newPos > posLine pos .||. 
              (posLine newPos === posLine pos .&&. posColumn newPos >= posColumn pos))

prop_advancePosBy_additive :: SourcePos -> Int -> Int -> Property
prop_advancePosBy_additive pos n1 n2 =
  n1 >= 0 && n2 >= 0 ==>
  let pos1 = advancePosBy pos n1
      pos2 = advancePosBy pos1 n2
      pos3 = advancePosBy pos (n1 + n2)
  in property $ pos2 === pos3

prop_advancePosByText_newlines :: SourcePos -> Int -> Property
prop_advancePosByText_newlines pos n =
  n > 0 && n <= 100 ==>
  let text = T.unlines (replicate n "line")
      newPos = advancePosByText pos text
  in property $ posLine newPos === posLine pos + n .&&.
             posColumn newPos === 1

prop_advancePosByText_tabs :: SourcePos -> Int -> Property
prop_advancePosByText_tabs pos n =
  n > 0 && n <= 20 ==>
  let text = T.replicate n "\t"
      newPos = advancePosByText pos text
      expectedCol = ((posColumn pos - 1) `div` 8 + n) * 8 + 1
  in property $ posColumn newPos === expectedCol .&&.
             posLine newPos === posLine pos

prop_advancePosByText_length_preserving :: SourcePos -> Text -> Property
prop_advancePosByText_length_preserving pos text =
  let newPos = advancePosByText pos text
      textLength = T.length text
  in property $ posOffset newPos === posOffset pos + textLength

-- ============================================================================
-- Span Validation Properties
-- ============================================================================

prop_isValidSpan_detection :: SourcePos -> SourcePos -> Property
prop_isValidSpan_detection pos1 pos2 =
  let span = SourceSpan pos1 pos2
      shouldBeValid = posOffset pos1 <= posOffset pos2
  in property $ isValidSpan span === shouldBeValid

prop_merged_spans_valid :: SourceSpan -> SourceSpan -> Property
prop_merged_spans_valid span1 span2 =
  isValidSpan span1 .&&. isValidSpan span2 ==>
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

prop_span_operations_preserve_validity :: SourcePos -> SourcePos -> Property
prop_span_operations_preserve_validity pos1 pos2 =
  posOffset pos1 <= posOffset pos2 ==>
  let span1 = spanFrom pos1
      span2 = spanTo pos2
      between = spanBetween pos1 pos2
      merged = mergeSpans span1 span2
  in property $ all isValidSpan [span1, span2, between, merged]

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

prop_position_arithmetic_large_numbers :: SourcePos -> Property
prop_position_arithmetic_large_numbers pos =
  let largeLine = posLine pos + 1000000
      largeCol = posColumn pos + 1000000
      largeOffset = posOffset pos + 1000000
      largePos = posAtLineCol largeLine largeCol largeOffset
  in property $ posLine largePos === largeLine .&&.
             posColumn largePos === largeCol .&&.
             posOffset largePos === largeOffset

prop_span_operations_edge_positions :: Property
prop_span_operations_edge_positions =
  let firstPos = posAt 1 1
      largePos = posAtLineCol 999999 999999 999999
      span1 = spanFrom firstPos
      span2 = spanTo largePos
      merged = mergeSpans span1 span2
  in property $ all isValidSpan [span1, span2, merged] .&&.
             spanStart merged === firstPos .&&.
             spanEnd merged === largePos

prop_text_advancement_empty_text :: SourcePos -> Property
prop_text_advancement_empty_text pos =
  let emptyText = ""
      newPos = advancePosByText pos emptyText
  in property $ newPos === pos

prop_position_advancement_unicode :: SourcePos -> Property
prop_position_advancement_unicode pos =
  let unicodeText = "café naïve résumé 🚀 测试"
      newPos = advancePosByText pos unicodeText
      textLength = length unicodeText
  in property $ posOffset newPos === posOffset pos + textLength