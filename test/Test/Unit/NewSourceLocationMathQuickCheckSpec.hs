{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewSourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , mergeSpans, spanBetween, emptySpan
  )

-- | Test SourcePos properties
prop_startpos_valid :: Bool
prop_startpos_valid = 
    posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

prop_posafter_newline_increments_line :: Int -> Int -> Int -> Property
prop_posafter_newline_increments_line l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        newPos = posAfter '\n' pos
    in posLine newPos == l + 1 && posColumn newPos == 1 && posOffset newPos == o + 1

prop_posafter_tab_aligns_to_8 :: Int -> Int -> Int -> Property
prop_posafter_tab_aligns_to_8 l c o =
    l > 0 && c > 0 && c <= 8 && o >= 0 ==>
    let pos = SourcePos l c o
        expectedCol = ((c - 1) `div` 8 + 1) * 8 + 1
        newPos = posAfter '\t' pos
    in posLine newPos == l && posColumn newPos == expectedCol && posOffset newPos == o + 1

prop_posafter_regular_char_increments_column :: Int -> Int -> Int -> Char -> Property
prop_posafter_regular_char_increments_column l c o ch =
    l > 0 && c > 0 && o >= 0 && ch `notElem` "\n\t" ==>
    let pos = SourcePos l c o
        newPos = posAfter ch pos
    in posLine newPos == l && posColumn newPos == c + 1 && posOffset newPos == o + 1

prop_posat_creates_correct_position :: Int -> Int -> Property
prop_posat_creates_correct_position l c =
    l > 0 && c > 0 ==>
    let pos = posAt l c
    in posLine pos == l && posColumn pos == c && posOffset pos == 0

prop_posatlinecol_creates_correct_position :: Int -> Int -> Int -> Property
prop_posatlinecol_creates_correct_position l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = posAtLineCol l c o
    in posLine pos == l && posColumn pos == c && posOffset pos == o

-- | Test SourceSpan properties
prop_emptyspan_has_same_start_and_end :: Int -> Int -> Int -> Property
prop_emptyspan_has_same_start_and_end l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        span = emptySpan pos
    in spanStart span == pos && spanEnd span == pos

prop_spanfrom_creates_empty_span :: Int -> Int -> Int -> Property
prop_spanfrom_creates_empty_span l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        span1 = emptySpan pos
        span2 = spanFrom pos
    in span1 == span2

prop_spanto_creates_span_at_position :: Int -> Int -> Int -> Property
prop_spanto_creates_span_at_position l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        span = spanTo pos
    in spanStart span == pos && spanEnd span == pos

prop_spanbetween_orders_positions :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_spanbetween_orders_positions l1 c1 o1 l2 c2 o2 =
    l1 > 0 && c1 > 0 && o1 >= 0 && l2 > 0 && c2 > 0 && o2 >= 0 ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        span = spanBetween pos1 pos2
    in spanStart span == pos1 && spanEnd span == pos2

prop_merge_spans_creates_encompassing_span :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_creates_encompassing_span l1 c1 o1 l2 c2 o2 l3 c3 o3 =
    all (>0) [l1, c1, l2, c2, l3, c3] && all (>=0) [o1, o2, o3] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        pos3 = SourcePos l3 c3 o3
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos3
        merged = mergeSpans span1 span2
    in spanStart merged <= spanStart span1 && spanStart merged <= spanStart span2 &&
       spanEnd merged >= spanEnd span1 && spanEnd merged >= spanEnd span2

prop_validspan_check_works :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_validspan_check_works l1 c1 o1 l2 c2 o2 =
    all (>0) [l1, c1, l2, c2] && all (>=0) [o1, o2] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos1
        valid1 = isValidSpan span1
        valid2 = isValidSpan span2
    in (o1 <= o2 && valid1) || (o1 > o2 && not valid1)

-- | Test Located properties
prop_located_at_creates_correct_location :: Int -> Int -> Int -> String -> Property
prop_located_at_creates_correct_location l c o value =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        located = locatedAt pos value
    in locValue located == value && locatedPos located == pos && 
       locSpan located == emptySpan pos

prop_located_with_span_creates_correct_location :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span_creates_correct_location l1 c1 o1 l2 c2 o2 value =
    all (>0) [l1, c1, l2, c2] && all (>=0) [o1, o2] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        span = spanBetween pos1 pos2
        located = locatedWithSpan span value
    in locValue located == value && locSpan located == span && 
       locatedPos located == pos1

prop_map_located_preserves_location :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_map_located_preserves_location l1 c1 o1 l2 c2 o2 value =
    all (>0) [l1, c1, l2, c2] && all (>=0) [o1, o2] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        span = spanBetween pos1 pos2
        located = locatedWithSpan span value
        mapped = mapLocated (length) located
    in locatedSpan mapped == span && locatedPos mapped == pos1 &&
       locValue mapped == length value

-- | Test position advancement properties
prop_advance_pos_matches_posafter :: Int -> Int -> Int -> Char -> Property
prop_advance_pos_matches_posafter l c o ch =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        advanced1 = advancePos ch pos
        advanced2 = posAfter ch pos
    in advanced1 == advanced2

prop_advance_pos_by_empty_string :: Int -> Int -> Int -> Property
prop_advance_pos_by_empty_string l c o =
    l > 0 && c > 0 && o >= 0 ==>
    let pos = SourcePos l c o
        advanced = advancePosBy "" pos
    in advanced == pos

prop_advance_pos_by_concatenation :: Int -> Int -> Int -> String -> String -> Property
prop_advance_pos_by_concatenation l c o s1 s2 =
    l > 0 && c > 0 && o >= 0 && not (null s1) && not (null s2) ==>
    let pos = SourcePos l c o
        advanced1 = advancePosBy (s1 ++ s2) pos
        advanced2 = advancePosBy s2 (advancePosBy s1 pos)
    in advanced1 == advanced2

prop_advance_pos_by_line :: Int -> Int -> Int -> Int -> Property
prop_advance_pos_by_line l c o numLines =
    l > 0 && c > 0 && o >= 0 && numLines > 0 ==>
    let pos = SourcePos l c o
        advanced = advancePosByLine numLines pos
    in posLine advanced == l + numLines && posColumn advanced == 1

-- | Test span merging properties
prop_merge_spans_commutative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_commutative l1 c1 o1 l2 c2 o2 l3 c3 o3 =
    all (>0) [l1, c1, l2, c2, l3, c3] && all (>=0) [o1, o2, o3] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        pos3 = SourcePos l3 c3 o3
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos3
        merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in merged1 == merged2

prop_merge_spans_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_associative l1 c1 o1 l2 c2 o2 l3 c3 o3 l4 c4 o4 =
    all (>0) [l1, c1, l2, c2, l3, c3, l4, c4] && all (>=0) [o1, o2, o3, o4] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        pos3 = SourcePos l3 c3 o3
        pos4 = SourcePos l4 c4 o4
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos3
        span3 = spanBetween pos3 pos4
        merged1 = mergeSpans (mergeSpans span1 span2) span3
        merged2 = mergeSpans span1 (mergeSpans span2 span3)
    in merged1 == merged2

-- | Test position ordering properties
prop_position_ordering_by_offset :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_position_ordering_by_offset l1 c1 o1 l2 c2 o2 =
    all (>0) [l1, c1, l2, c2] && all (>=0) [o1, o2] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
    in (o1 < o2) == (pos1 < pos2)

-- | Test span coverage properties
prop_span_coverage_contains_both_ends :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_coverage_contains_both_ends l1 c1 o1 l2 c2 o2 =
    all (>0) [l1, c1, l2, c2] && all (>=0) [o1, o2] ==>
    let pos1 = SourcePos l1 c1 o1
        pos2 = SourcePos l2 c2 o2
        span = spanBetween pos1 pos2
        merged = mergeSpans span span
    in merged == span

tests :: TestTree
tests = testGroup "SourceLocation Math Properties QuickCheck Tests"
  [ testProperty "startPos is valid" prop_startpos_valid
  , testProperty "posAfter newline increments line" prop_posafter_newline_increments_line
  , testProperty "posAfter tab aligns to 8" prop_posafter_tab_aligns_to_8
  , testProperty "posAfter regular char increments column" prop_posafter_regular_char_increments_column
  , testProperty "posAt creates correct position" prop_posat_creates_correct_position
  , testProperty "posAtLineCol creates correct position" prop_posatlinecol_creates_correct_position
  , testProperty "emptySpan has same start and end" prop_emptyspan_has_same_start_and_end
  , testProperty "spanFrom creates empty span" prop_spanfrom_creates_empty_span
  , testProperty "spanTo creates span at position" prop_spanto_creates_span_at_position
  , testProperty "spanBetween orders positions" prop_spanbetween_orders_positions
  , testProperty "mergeSpans creates encompassing span" prop_merge_spans_creates_encompassing_span
  , testProperty "isValidSpan check works" prop_validspan_check_works
  , testProperty "locatedAt creates correct location" prop_located_at_creates_correct_location
  , testProperty "locatedWithSpan creates correct location" prop_located_with_span_creates_correct_location
  , testProperty "mapLocated preserves location" prop_map_located_preserves_location
  , testProperty "advancePos matches posAfter" prop_advance_pos_matches_posafter
  , testProperty "advancePosBy empty string" prop_advance_pos_by_empty_string
  , testProperty "advancePosBy concatenation" prop_advance_pos_by_concatenation
  , testProperty "advancePosByLine" prop_advance_pos_by_line
  , testProperty "mergeSpans commutative" prop_merge_spans_commutative
  , testProperty "mergeSpans associative" prop_merge_spans_associative
  , testProperty "position ordering by offset" prop_position_ordering_by_offset
  , testProperty "span coverage contains both ends" prop_span_coverage_contains_both_ends
  ]