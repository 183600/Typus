{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements)

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
  , advancePosByText
  , advancePosByLine
  )

import Data.Char (isSpace)
import qualified Data.Text as T

-- ============================================================================
-- Generators
-- ============================================================================

genValidPos :: Gen SourcePos
genValidPos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

genValidSpan :: Gen SourceSpan
genValidSpan = do
  start <- genValidPos
  end <- genValidPos
  -- Ensure end is not before start
  let validEnd = if posLine end < posLine start || 
                    (posLine end == posLine start && posColumn end < posColumn start)
                 then start { posColumn = posColumn start + 1, posOffset = posOffset start + 1 }
                 else end
  return $ SourceSpan start validEnd

genLocated :: Gen String -> Gen (Located String)
genLocated genValue = do
  value <- genValue
  span <- genValidSpan
  return $ Located value span

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}<>+-*/%=|&^~'\"@#$_`"

genString :: Gen String
genString = listOf genChar

-- ============================================================================
-- Properties for SourcePos
-- ============================================================================

prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
               posColumn newPos === 1 .&&.
               posOffset newPos === posOffset pos + 1

prop_posAfter_tab_advances_to_next_tab_stop :: SourcePos -> Property
prop_posAfter_tab_advances_to_next_tab_stop pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
               posColumn newPos === expectedCol .&&.
               posOffset newPos === posOffset pos + 1

prop_posAfter_regular_char_increments_column :: SourcePos -> Char -> Property
prop_posAfter_regular_char_increments_column pos char =
  char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
               posColumn newPos === posColumn pos + 1 .&&.
               posOffset newPos === posOffset pos + 1

prop_posAt_creates_correct_position :: Int -> Int -> Property
prop_posAt_creates_correct_position line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
               posColumn pos === col .&&.
               posOffset pos === 0

prop_posAtLineCol_creates_correct_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_correct_position line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
               posColumn pos === col .&&.
               posOffset pos === offset

-- ============================================================================
-- Properties for SourceSpan
-- ============================================================================

prop_emptySpan_is_valid :: Property
prop_emptySpan_is_valid =
  let span = emptySpan
  in property $ isValidSpan span === True

prop_spanFrom_creates_valid_span :: SourcePos -> Property
prop_spanFrom_creates_valid_span pos =
  let span = spanFrom pos
  in property $ isValidSpan span === True

prop_spanTo_creates_valid_span :: SourcePos -> Property
prop_spanTo_creates_valid_span pos =
  let span = spanTo pos
  in property $ isValidSpan span === True

prop_spanBetween_orders_positions :: SourcePos -> SourcePos -> Property
prop_spanBetween_orders_positions pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ posLine start <= posLine end .&&.
               (posLine start < posLine end || posColumn start <= posColumn end)

prop_mergeSpans_contains_original_spans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_original_spans span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ posLine mergedStart <= posLine start1 .&&.
               posLine mergedStart <= posLine start2 .&&.
               posLine end1 <= posLine mergedEnd .&&.
               posLine end2 <= posLine mergedEnd

-- ============================================================================
-- Properties for Located
-- ============================================================================

prop_locatedAt_sets_correct_position :: String -> SourcePos -> Property
prop_locatedAt_sets_correct_position value pos =
  let located = locatedAt value pos
      span = locatedSpan located
  in property $ spanStart span === pos .&&. spanEnd span === pos

prop_locatedWithSpan_sets_correct_span :: String -> SourceSpan -> Property
prop_locatedWithSpan_sets_correct_span value span =
  let located = locatedWithSpan value span
  in property $ locatedSpan located === span .&&. locatedValue located === value

prop_locatedValue_returns_original_value :: String -> SourceSpan -> Property
prop_locatedValue_returns_original_value value span =
  let located = locatedWithSpan value span
  in property $ locatedValue located === value

prop_locatedPos_returns_start_position :: String -> SourceSpan -> Property
prop_locatedPos_returns_start_position value span =
  let located = locatedWithSpan value span
  in property $ locatedPos located === spanStart span

prop_mapLocated_preserves_location :: String -> String -> SourceSpan -> Property
prop_mapLocated_preserves_location value1 value2 span =
  let located1 = locatedWithSpan value1 span
      located2 = mapLocated (const value2) located1
  in property $ locatedSpan located1 === locatedSpan located2 .&&.
               locatedValue located2 === value2

-- ============================================================================
-- Properties for Position Advancement
-- ============================================================================

prop_advancePos_advances_by_single_char :: SourcePos -> Char -> Property
prop_advancePos_advances_by_single_char pos char =
  let newPos = advancePos pos char
      expectedPos = posAfter char pos
  in property $ newPos === expectedPos

prop_advancePosBy_advances_by_string :: SourcePos -> String -> Property
prop_advancePosBy_advances_by_string pos str =
  let finalPos = advancePosBy pos str
      expectedPos = foldl posAfter pos str
  in property $ finalPos === expectedPos

prop_advancePosByText_advances_by_text :: SourcePos -> String -> Property
prop_advancePosByText_advances_by_text pos str =
  let text = T.pack str
      finalPos = advancePosByText pos text
      expectedPos = advancePosBy pos str
  in property $ finalPos === expectedPos

prop_advancePosByLine_advances_by_lines :: SourcePos -> Int -> Property
prop_advancePosByLine_advances_by_lines pos lines =
  lines >= 0 ==> 
  let finalPos = advancePosByLine pos lines
      expectedLine = posLine pos + lines
  in property $ posLine finalPos === expectedLine .&&.
               posColumn finalPos === 1

-- ============================================================================
-- Properties for Span Validity
-- ============================================================================

prop_isValidSpan_checks_proper_ordering :: Int -> Int -> Int -> Int -> Property
prop_isValidSpan_checks_proper_ordering line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = SourceSpan pos1 pos2
  in property $ isValidSpan span === (line1 < line2 || (line1 == line2 && col1 <= col2))

-- ============================================================================
-- Properties for Position Arithmetic
-- ============================================================================

prop_position_arithmetic_is_consistent :: SourcePos -> String -> String -> Property
prop_position_arithmetic_is_consistent pos str1 str2 =
  let posAfterStr1 = advancePosBy pos str1
      posAfterStr2 = advancePosBy posAfterStr1 str2
      posAfterCombined = advancePosBy pos (str1 ++ str2)
  in property $ posAfterStr2 === posAfterCombined

prop_offset_monotonically_increases :: SourcePos -> String -> Property
prop_offset_monotonically_increases pos str =
  let finalPos = advancePosBy pos str
  in property $ posOffset finalPos >= posOffset pos

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "posAfter with newline increments line" prop_posAfter_newline_increments_line
    , fastProperty "posAfter with tab advances to next tab stop" prop_posAfter_tab_advances_to_next_tab_stop
    , fastProperty "posAfter with regular char increments column" prop_posAfter_regular_char_increments_column
    , fastProperty "posAt creates correct position" prop_posAt_creates_correct_position
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
    ]

  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan is valid" prop_emptySpan_is_valid
    , fastProperty "spanFrom creates valid span" prop_spanFrom_creates_valid_span
    , fastProperty "spanTo creates valid span" prop_spanTo_creates_valid_span
    , fastProperty "spanBetween orders positions" prop_spanBetween_orders_positions
    , fastProperty "mergeSpans contains original spans" prop_mergeSpans_contains_original_spans
    ]

  , testGroup "Located Properties"
    [ fastProperty "locatedAt sets correct position" prop_locatedAt_sets_correct_position
    , fastProperty "locatedWithSpan sets correct span" prop_locatedWithSpan_sets_correct_span
    , fastProperty "locatedValue returns original value" prop_locatedValue_returns_original_value
    , fastProperty "locatedPos returns start position" prop_locatedPos_returns_start_position
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    ]

  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos advances by single char" prop_advancePos_advances_by_single_char
    , fastProperty "advancePosBy advances by string" prop_advancePosBy_advances_by_string
    , fastProperty "advancePosByText advances by text" prop_advancePosByText_advances_by_text
    , fastProperty "advancePosByLine advances by lines" prop_advancePosByLine_advances_by_lines
    ]

  , testGroup "Span Validity Properties"
    [ fastProperty "isValidSpan checks proper ordering" prop_isValidSpan_checks_proper_ordering
    ]

  , testGroup "Position Arithmetic Properties"
    [ fastProperty "position arithmetic is consistent" prop_position_arithmetic_is_consistent
    , fastProperty "offset monotonically increases" prop_offset_monotonically_increases
    ]
  ]