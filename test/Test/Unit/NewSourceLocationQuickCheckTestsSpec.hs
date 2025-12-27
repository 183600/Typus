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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isControl)

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
  , spanStart
  , spanEnd
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000) 
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value span

-- Generate valid characters for position advancement
genRegularChar :: Gen Char
genRegularChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()_+-=[]{}|;':\",./<>?"

-- Generate newline characters
genNewlineChar :: Gen Char
genNewlineChar = elements "\n\r"

-- Generate tab characters
genTabChar :: Gen Char
genTabChar = return '\t'

-- Generate mixed characters
genMixedChar :: Gen Char
genMixedChar = oneof [genRegularChar, genNewlineChar, genTabChar]

-- Generate text strings
genText :: Gen Text
genText = T.pack <$> listOf genMixedChar

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: startPos has correct initial values
prop_start_pos_correct :: Property
prop_start_pos_correct =
  startPos === SourcePos 1 1 0

-- Property: posAfter with newline increments line and resets column
prop_pos_after_newline :: SourcePos -> Property
prop_pos_after_newline pos =
  let newPos = posAfter '\n' pos
      expectedLine = posLine pos + 1
      expectedCol = 1
      expectedOffset = posOffset pos + 1
  in newPos === SourcePos expectedLine expectedCol expectedOffset

-- Property: posAfter with tab advances to next tab stop
prop_pos_after_tab :: SourcePos -> Property
prop_pos_after_tab pos =
  let col = posColumn pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
      newPos = posAfter '\t' pos
  in posColumn newPos === expectedCol .&&. posLine newPos === posLine pos .&&. posOffset newPos === posOffset pos + 1

-- Property: posAfter with regular char increments column
prop_pos_after_regular :: Char -> SourcePos -> Property
prop_pos_after_regular char pos =
  not (isControl char) && char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
      expectedCol = posColumn pos + 1
  in posColumn newPos === expectedCol .&&. posLine newPos === posLine pos .&&. posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_pos_at_correct :: Int -> Int -> Property
prop_pos_at_correct line col =
  line > 0 && col > 0 ==>
  posAt line col === SourcePos line col 0

-- Property: posAtLineCol creates position with all fields
prop_pos_at_line_col_correct :: Int -> Int -> Int -> Property
prop_pos_at_line_col_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  posAtLineCol line col offset === SourcePos line col offset

-- Property: emptySpan has start and end at startPos
prop_empty_span_correct :: Property
prop_empty_span_correct =
  emptySpan === SourceSpan startPos startPos

-- Property: spanFrom creates span from position to same position
prop_span_from_correct :: SourcePos -> Property
prop_span_from_correct pos =
  spanFrom pos === SourceSpan pos pos

-- Property: spanTo creates span from startPos to position
prop_span_to_correct :: SourcePos -> Property
prop_span_to_correct pos =
  spanTo pos === SourceSpan startPos pos

-- Property: spanBetween creates correct span
prop_span_between_correct :: SourcePos -> SourcePos -> Property
prop_span_between_correct start end =
  spanBetween start end === SourceSpan start end

-- Property: mergeSpans contains both original spans
prop_merge_spans_contains_both :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_merge_spans_contains_both start1 end1 start2 end2 =
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      merged = mergeSpans span1 span2
      minStart = min start1 start2
      maxEnd = max end1 end2
  in merged === SourceSpan minStart maxEnd

-- Property: isValidSpan checks span validity
prop_is_valid_span :: SourcePos -> SourcePos -> Property
prop_is_valid_span start end =
  let span = SourceSpan start end
      valid = start <= end
  in isValidSpan span === valid

-- Property: locatedAt creates located value at startPos
prop_located_at_correct :: Int -> Property
prop_located_at_correct value =
  locatedAt value === Located value emptySpan

-- Property: locatedWithSpan creates located value with span
prop_located_with_span_correct :: Int -> SourceSpan -> Property
prop_located_with_span_correct value span =
  locatedWithSpan value span === Located value span

-- Property: locatedValue extracts the value
prop_located_value_correct :: Int -> SourceSpan -> Property
prop_located_value_correct value span =
  let located = Located value span
  in locatedValue located === value

-- Property: locatedSpan extracts the span
prop_located_span_correct :: Int -> SourceSpan -> Property
prop_located_span_correct value span =
  let located = Located value span
  in locatedSpan located === span

-- Property: locatedPos extracts the start position
prop_located_pos_correct :: Int -> SourceSpan -> Property
prop_located_pos_correct value span =
  let located = Located value span
  in locatedPos located === spanStart span

-- Property: mapLocated applies function to value
prop_map_located_correct :: Int -> Int -> SourceSpan -> Property
prop_map_located_correct value multiplier span =
  let located = Located value span
      mapped = mapLocated (* multiplier) located
  in locatedValue mapped === value * multiplier .&&. locatedSpan mapped === span

-- Property: advancePos advances position by one character
prop_advance_pos_matches_posAfter :: Char -> SourcePos -> Property
prop_advance_pos_matches_posAfter char pos =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy advances position by multiple characters
prop_advance_pos_by_consistent :: String -> SourcePos -> Property
prop_advance_pos_by_consistent chars pos =
  not (any (== '\0') chars) ==> -- Avoid null bytes
  let manualAdvance = foldl (flip posAfter) pos chars
      byAdvance = advancePosBy chars pos
  in manualAdvance === byAdvance

-- Property: advancePosByText advances position by text content
prop_advance_pos_by_text_consistent :: Text -> SourcePos -> Property
prop_advance_pos_by_text_consistent text pos =
  not (T.any (== '\0') text) ==> -- Avoid null bytes
  let manualAdvance = T.foldl (flip posAfter) pos text
      textAdvance = advancePosByText text pos
  in manualAdvance === textAdvance

-- Property: advancePosByLine advances by specific number of lines
prop_advance_pos_by_line_correct :: Int -> SourcePos -> Property
prop_advance_pos_by_line_correct numLines pos =
  numLines >= 0 && numLines <= 100 ==> -- Limit for performance
  let advanced = advancePosByLine numLines pos
      expectedLine = posLine pos + numLines
  in posLine advanced === expectedLine .&&. posColumn advanced === 1

-- Property: Position advancement is monotonic
prop_position_advancement_monotonic :: String -> SourcePos -> Property
prop_position_advancement_monotonic chars pos =
  not (any (== '\0') chars) ==>
  let finalPos = advancePosBy chars pos
  in posOffset finalPos >= posOffset pos

-- Property: Span merging is commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: Span merging is associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: Merged span contains original spans
prop_merge_spans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in mergedStart <= start1 .&&. mergedEnd >= end1 .&&.
     mergedStart <= start2 .&&. mergedEnd >= end2

-- Property: Located values preserve span information through mapping
prop_located_preserves_span :: Int -> SourceSpan -> Property
prop_located_preserves_span value span =
  let located = Located value span
      doubled = mapLocated (*2) located
      squared = mapLocated (^2) doubled
  in locatedSpan squared === span

-- Property: Position tracking with mixed characters
prop_position_mixed_characters :: Text -> Property
prop_position_mixed_characters text =
  not (T.null text) && not (T.any (== '\0') text) ==>
  let finalPos = advancePosByText text startPos
  in posLine finalPos >= 1 && posColumn finalPos >= 1 && posOffset finalPos >= T.length text - 1

-- Property: Tab advancement respects tab stops
prop_tab_advancement_tab_stops :: Int -> Property
prop_tab_advancement_tab_stops initialCol =
  initialCol > 0 && initialCol <= 100 ==>
  let pos = SourcePos 1 initialCol 0
      afterTab = posAfter '\t' pos
      expectedCol = ((initialCol - 1) `div` 8 + 1) * 8 + 1
  in posColumn afterTab === expectedCol

-- Property: Newline advancement resets column to 1
prop_newline_advancement_resets_column :: Int -> Property
prop_newline_advancement_resets_column line =
  line > 0 && line <= 100 ==>
  let pos = SourcePos line 50 0  -- Start at column 50
      afterNewline = posAfter '\n' pos
  in posLine afterNewline === line + 1 .&&. posColumn afterNewline === 1

-- Property: Offset advancement matches character count
prop_offset_advancement_matches_count :: String -> Property
prop_offset_advancement_matches_count chars =
  not (any (== '\0') chars) ==> -- Avoid null bytes
  let startOffset = posOffset startPos
      finalPos = advancePosBy chars startPos
      expectedOffset = startOffset + length chars
  in posOffset finalPos === expectedOffset

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ fastProperty "startPos has correct initial values" prop_start_pos_correct
  , fastProperty "posAfter with newline increments line and resets column" prop_pos_after_newline
  , fastProperty "posAfter with tab advances to next tab stop" prop_pos_after_tab
  , fastProperty "posAfter with regular char increments column" prop_pos_after_regular
  , fastProperty "posAt creates position with correct line and column" prop_pos_at_correct
  , fastProperty "posAtLineCol creates position with all fields" prop_pos_at_line_col_correct
  , fastProperty "emptySpan has start and end at startPos" prop_empty_span_correct
  , fastProperty "spanFrom creates span from position to same position" prop_span_from_correct
  , fastProperty "spanTo creates span from startPos to position" prop_span_to_correct
  , fastProperty "spanBetween creates correct span" prop_span_between_correct
  , fastProperty "mergeSpans contains both original spans" prop_merge_spans_contains_both
  , fastProperty "isValidSpan checks span validity" prop_is_valid_span
  , fastProperty "locatedAt creates located value at startPos" prop_located_at_correct
  , fastProperty "locatedWithSpan creates located value with span" prop_located_with_span_correct
  , fastProperty "locatedValue extracts the value" prop_located_value_correct
  , fastProperty "locatedSpan extracts the span" prop_located_span_correct
  , fastProperty "locatedPos extracts the start position" prop_located_pos_correct
  , fastProperty "mapLocated applies function to value" prop_map_located_correct
  , fastProperty "advancePos matches posAfter" prop_advance_pos_matches_posAfter
  , fastProperty "advancePosBy advances position by multiple characters" prop_advance_pos_by_consistent
  , fastProperty "advancePosByText advances position by text content" prop_advance_pos_by_text_consistent
  , fastProperty "advancePosByLine advances by specific number of lines" prop_advance_pos_by_line_correct
  , fastProperty "Position advancement is monotonic" prop_position_advancement_monotonic
  , fastProperty "Span merging is commutative" prop_merge_spans_commutative
  , fastProperty "Span merging is associative" prop_merge_spans_associative
  , fastProperty "Merged span contains original spans" prop_merge_spans_contains_originals
  , fastProperty "Located values preserve span information through mapping" prop_located_preserves_span
  , fastProperty "Position tracking with mixed characters" prop_position_mixed_characters
  , fastProperty "Tab advancement respects tab stops" prop_tab_advancement_tab_stops
  , fastProperty "Newline advancement resets column to 1" prop_newline_advancement_resets_column
  , fastProperty "Offset advancement matches character count" prop_offset_advancement_matches_count
  ]