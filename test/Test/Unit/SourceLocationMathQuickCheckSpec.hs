{-# LANGUAGE CPP #-}
module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, Property, (===), forAll, counterexample, 
                        suchThat, (==>))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, advancePos, advancePosBy, advancePosByText,
                      toErrorLocation, toErrorLocationWithSpan)

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate valid source positions (1-based line and column)
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- Generate valid source spans
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- choose (1, 1000)
  startCol <- choose (1, 1000)
  startOffset <- choose (0, 500000)
  let start = SourcePos startLine startCol startOffset
  
  -- Ensure end position is after start
  endLine <- choose (startLine, startLine + 100)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 1000)
  endOffset <- choose (startOffset, startOffset + 100000)
  let end = SourcePos endLine endCol endOffset
  
  return $ SourceSpan start end

-- Generate characters for position advancement testing
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ 
                  [' ', '\t', '\n', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', 
                   '-', '+', '=', '[', ']', '{', '}', '|', '\\', ';', ':', '\'', '"', 
                   ',', '.', '<', '>', '/', '?']

-- Generate strings for text advancement testing
genString :: Gen String
genString = listOf genChar

-- ============================================================================
-- Properties for SourcePos
-- ============================================================================

prop_pos_after_newline_increments_line :: SourcePos -> Property
prop_pos_after_newline_increments_line pos =
  let newPos = posAfter '\n' pos
  in counterexample ("Original: " ++ show pos ++ ", New: " ++ show newPos) $
     posLine newPos === posLine pos + 1 &&
     posColumn newPos === 1 &&
     posOffset newPos === posOffset pos + 1

prop_pos_after_tab_advances_to_next_tab_stop :: SourcePos -> Property
prop_pos_after_tab_advances_to_next_tab_stop pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in counterexample ("Original: " ++ show pos ++ ", New: " ++ show newPos ++ ", Expected col: " ++ show expectedCol) $
     posColumn newPos === expectedCol &&
     posOffset newPos === posOffset pos + 1

prop_pos_after_regular_char_increments_column :: SourcePos -> Property
prop_pos_after_regular_char_increments_column pos =
  forAll genChar $ \c ->
    c `notElem` ['\n', '\t'] ==> 
    let newPos = posAfter c pos
    in counterexample ("Char: " ++ show c ++ ", Original: " ++ show pos ++ ", New: " ++ show newPos) $
       posColumn newPos === posColumn pos + 1 &&
       posOffset newPos === posOffset pos + 1

prop_advance_pos_by_consistency :: SourcePos -> String -> Property
prop_advance_pos_by_consistency pos s =
  let singleAdvance = foldl (flip advancePos) pos s
      batchAdvance = advancePosBy s pos
  in singleAdvance === batchAdvance

-- ============================================================================
-- Properties for SourceSpan
-- ============================================================================

prop_empty_span_has_same_start_end :: SourcePos -> Property
prop_empty_span_has_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === spanEnd span

prop_span_between_valid_order :: SourcePos -> SourcePos -> Property
prop_span_between_valid_order pos1 pos2 =
  let span = spanBetween pos1 pos2
  in isValidSpan span === (pos1 <= pos2)

prop_merge_spans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
  in counterexample ("Span1: " ++ show span1 ++ ", Span2: " ++ show span2 ++ ", Merged: " ++ show merged) $
     spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 &&
     spanEnd merged >= spanEnd span2

prop_merge_spans_idempotent :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_idempotent span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans merged1 span2
  in merged1 === merged2

-- ============================================================================
-- Properties for Located values
-- ============================================================================

prop_located_at_uses_empty_span :: SourcePos -> String -> Property
prop_located_at_uses_empty_span pos value =
  let located = locatedAt pos value
  in locSpan located === emptySpan pos

prop_located_with_span_preserves_span :: SourceSpan -> String -> Property
prop_located_with_span_preserves_span span value =
  let located = locatedWithSpan span value
  in locSpan located === span

-- ============================================================================
-- Properties for error location conversion
-- ============================================================================

prop_to_error_location_preserves_position :: SourcePos -> Property
prop_to_error_location_preserves_position pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos &&
     column errLoc === posColumn pos

prop_to_error_location_with_span_preserves_range :: SourceSpan -> Property
prop_to_error_location_with_span_preserves_range span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in counterexample ("Span: " ++ show span ++ ", ErrorLoc: " ++ show errLoc) $
     line errLoc === posLine start &&
     column errLoc === posColumn start &&
     endLine errLoc === Just (posLine end) &&
     endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Properties for position advancement by text
-- ============================================================================

prop_advance_pos_by_text_consistency :: SourcePos -> String -> Property
prop_advance_pos_by_text_consistency pos s =
  let textAdvance = advancePosByText s pos
      stringAdvance = advancePosBy s pos
  in textAdvance === stringAdvance

-- ============================================================================
-- Mathematical properties
-- ============================================================================

prop_position_ordering_consistency :: SourcePos -> SourcePos -> SourcePos -> Property
prop_position_ordering_consistency pos1 pos2 pos3 =
  let positions = [pos1, pos2, pos3]
      sorted = positions  -- This would need actual sorting implementation
  in -- Test that offset ordering is consistent with line/column ordering
     (posOffset pos1 <= posOffset pos2) === 
     (posLine pos1 < posLine pos2 || 
      (posLine pos1 == posLine pos2 && posColumn pos1 <= posColumn pos2))

prop_span_length_math :: SourceSpan -> Property
prop_span_length_math span =
  let start = spanStart span
      end = spanEnd span
      expectedLength = posOffset end - posOffset start
  in isValidSpan span ==> expectedLength >= 0

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Math QuickCheck Tests"
  [ testGroup "SourcePos properties"
    [ fastProperty "posAfter newline increments line" prop_pos_after_newline_increments_line
    , fastProperty "posAfter tab advances to next tab stop" prop_pos_after_tab_advances_to_next_tab_stop
    , fastProperty "posAfter regular char increments column" prop_pos_after_regular_char_increments_column
    , fastProperty "advancePosBy consistency" prop_advance_pos_by_consistency
    ]
  , testGroup "SourceSpan properties"
    [ fastProperty "emptySpan has same start and end" prop_empty_span_has_same_start_end
    , fastProperty "spanBetween respects order" prop_span_between_valid_order
    , fastProperty "mergeSpans contains originals" prop_merge_spans_contains_originals
    , fastProperty "mergeSpans is idempotent" prop_merge_spans_idempotent
    ]
  , testGroup "Located value properties"
    [ fastProperty "locatedAt uses empty span" prop_located_at_uses_empty_span
    , fastProperty "locatedWithSpan preserves span" prop_located_with_span_preserves_span
    ]
  , testGroup "Error location conversion properties"
    [ fastProperty "toErrorLocation preserves position" prop_to_error_location_preserves_position
    , fastProperty "toErrorLocationWithSpan preserves range" prop_to_error_location_with_span_preserves_range
    ]
  , testGroup "Text advancement properties"
    [ fastProperty "advancePosByText consistency" prop_advance_pos_by_text_consistency
    ]
  , testGroup "Mathematical properties"
    [ fastProperty "position ordering consistency" prop_position_ordering_consistency
    , fastProperty "span length math" prop_span_length_math
    ]
  ]