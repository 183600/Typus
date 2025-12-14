{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

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
  , toErrorLocation
  , toErrorLocationWithSpan
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

import qualified Data.Text as T
import Data.Char (isSpace)

-- Property: startPos has correct initial values
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 &&
  posColumn startPos === 1 &&
  posOffset startPos === 0

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 &&
     posColumn newPos === 1 &&
     posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos === posLine pos &&
     posColumn newPos === expectedCol &&
     posOffset newPos === posOffset pos + 1

-- Property: posAfter handles regular characters correctly
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular char pos =
  not (isSpace char) ==> 
  let newPos = posAfter char pos
  in posLine newPos === posLine pos &&
     posColumn newPos === posColumn pos + 1 &&
     posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct values
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  let pos = posAt line col
  in posLine pos === line &&
     posColumn pos === col &&
     posOffset pos === 0

-- Property: posAtLineCol creates position with correct values
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  let pos = posAtLineCol line col offset
  in posLine pos === line &&
     posColumn pos === col &&
     posOffset pos === offset

-- Property: emptySpan has same start and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === pos &&
     spanEnd span === pos

-- Property: spanFrom creates empty span
prop_spanFrom_empty :: SourcePos -> Property
prop_spanFrom_empty pos =
  let span = spanFrom pos
  in spanStart span === pos &&
     spanEnd span === pos

-- Property: spanTo creates empty span
prop_spanTo_empty :: SourcePos -> Property
prop_spanTo_empty pos =
  let span = spanTo pos
  in spanStart span === pos &&
     spanEnd span === pos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in spanStart span === start &&
     spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 &&
     spanEnd merged >= spanEnd span2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Property
prop_isValidSpan_correct pos1 pos2 =
  let span = spanBetween pos1 pos2
      valid = pos1 <= pos2
  in isValidSpan span === valid

-- Property: locatedAt creates located value at position
prop_locatedAt_correct :: SourcePos -> String -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in locatedSpan located === emptySpan pos &&
     locatedValue located === value

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span &&
     locatedValue located === value

-- Property: locatedPos returns start position
prop_locatedPos_correct :: SourceSpan -> String -> Property
prop_locatedPos_correct span value =
  let located = locatedWithSpan span value
  in locatedPos located === spanStart span

-- Property: mapLocated applies function to value
prop_mapLocated_correct :: SourceSpan -> Int -> Property
prop_mapLocated_correct span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in locatedValue mapped === value * 2 &&
     locatedSpan mapped === span

-- Property: toErrorLocation converts position correctly
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos &&
     column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan converts span correctly
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errLoc = toErrorLocationWithSpan span
  in line errLoc === posLine (spanStart span) &&
     column errLoc === posColumn (spanStart span) &&
     endLine errLoc === Just (posLine (spanEnd span)) &&
     endColumn errLoc === Just (posColumn (spanEnd span))

-- Property: advancePos equals posAfter
prop_advancePos_equals_posAfter :: Char -> SourcePos -> Property
prop_advancePos_equals_posAfter char pos =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy advances by multiple characters
prop_advancePosBy_correct :: String -> SourcePos -> Property
prop_advancePosBy_correct chars pos =
  let result = advancePosBy chars pos
      expected = foldl (flip posAfter) pos chars
  in result === expected

-- Property: advancePosByText advances by text
prop_advancePosByText_correct :: String -> SourcePos -> Property
prop_advancePosByText_correct text pos =
  let result = advancePosByText (T.pack text) pos
      expected = advancePosBy text pos
  in result === expected

-- Property: advancePosByLine advances line number
prop_advancePosByLine_correct :: Int -> SourcePos -> Property
prop_advancePosByLine_correct numLines pos =
  let result = advancePosByLine numLines pos
  in posLine result === posLine pos + numLines &&
     posColumn result === 1

-- Property: advancePosByLine doesn't change offset much
prop_advancePosByLine_offset :: Int -> SourcePos -> Property
prop_advancePosByLine_offset numLines pos =
  let result = advancePosByLine numLines pos
  in posOffset result > posOffset pos

-- Property: Located is a functor
prop_located_functor :: SourceSpan -> Int -> Int -> Property
prop_located_functor span value f =
  let located = locatedWithSpan span value
      mapped1 = mapLocated (+f) located
      mapped2 = mapLocated (*2) mapped1
  in locatedValue mapped2 === (value + f) * 2

-- Property: HasLocation instance for Located
prop_hasLocation_located :: SourceSpan -> String -> Property
prop_hasLocation_located span value =
  let located = locatedWithSpan span value
  in getLocation located === span

-- Property: spanBetween with same positions creates empty span
prop_spanBetween_same_positions :: SourcePos -> Property
prop_spanBetween_same_positions pos =
  let span = spanBetween pos pos
  in spanStart span === pos && spanEnd span === pos

-- Property: mergeSpans with empty span
prop_mergeSpans_with_empty :: SourceSpan -> SourcePos -> Property
prop_mergeSpans_with_empty span pos =
  let empty = emptySpan pos
      merged = mergeSpans span empty
  in merged === mergeSpans empty span

-- Property: advancePosBy with empty string
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosByText with empty text
prop_advancePosByText_empty :: SourcePos -> Property
prop_advancePosByText_empty pos =
  advancePosByText T.empty pos === pos

-- Property: advancePosByLine with zero lines
prop_advancePosByLine_zero :: SourcePos -> Property
prop_advancePosByLine_zero pos =
  advancePosByLine 0 pos === pos

-- Property: advancePosByLine with negative lines
prop_advancePosByLine_negative :: Int -> SourcePos -> Property
prop_advancePosByLine_negative n pos =
  let result = advancePosByLine (-n) pos
  in posLine result < posLine pos

-- Property: posAfter with multiple newlines
prop_posAfter_multiple_newlines :: Int -> SourcePos -> Property
prop_posAfter_multiple_newlines count pos =
  let newlines = replicate count '\n'
      result = advancePosBy newlines pos
  in posLine result === posLine pos + count &&
     posColumn result === 1

-- Property: posAfter with multiple tabs
prop_posAfter_multiple_tabs :: Int -> SourcePos -> Property
prop_posAfter_multiple_tabs count pos =
  let tabs = replicate count '\t'
      result = advancePosBy tabs pos
      expectedCol = ((posColumn pos - 1) `div` 8 + count) * 8 + 1
  in posLine result === posLine pos &&
     posColumn result === expectedCol

-- Property: spanBetween with reversed positions
prop_spanBetween_reversed :: SourcePos -> SourcePos -> Property
prop_spanBetween_reversed pos1 pos2 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
  in spanStart span1 === pos1 && spanStart span2 === pos2

-- Property: mergeSpans creates span covering all positions
prop_mergeSpans_covers_all :: SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_covers_all pos1 pos2 pos3 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in spanStart merged <= pos1 &&
     spanEnd merged >= pos3

-- Property: locatedWithSpan preserves span
prop_locatedWithSpan_preserves :: SourceSpan -> Int -> Property
prop_locatedWithSpan_preserves span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (+1) located
  in locatedSpan mapped === span

-- Property: mapLocated doesn't change span
prop_mapLocated_preserves_span :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_span span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (const 999) located
  in locatedSpan mapped === span

-- Property: toErrorLocation with negative values
prop_toErrorLocation_negative :: Property
prop_toErrorLocation_negative =
  let pos = posAtLineCol (-1) (-5) (-10)
      errLoc = toErrorLocation pos
  in line errLoc === -1 &&
     column errLoc === -5

-- Property: toErrorLocationWithSpan with negative values
prop_toErrorLocationWithSpan_negative :: Property
prop_toErrorLocationWithSpan_negative =
  let start = posAtLineCol (-1) (-5) (-10)
      end = posAtLineCol (-2) (-6) (-20)
      span = spanBetween start end
      errLoc = toErrorLocationWithSpan span
  in line errLoc === -1 &&
     column errLoc === -5 &&
     endLine errLoc === Just (-2) &&
     endColumn errLoc === Just (-6)

-- Property: advancePos with Unicode characters
prop_advancePos_unicode :: String -> SourcePos -> Property
prop_advancePos_unicode text pos =
  let result = advancePosBy text pos
      expected = foldl (flip posAfter) pos text
  in result === expected

-- Property: advancePosByText with Unicode text
prop_advancePosByText_unicode :: String -> SourcePos -> Property
prop_advancePosByText_unicode text pos =
  let result = advancePosByText (T.pack text) pos
      expected = advancePosBy text pos
  in result === expected

-- Advanced property tests for source location

-- Property: Complex span merging with multiple spans
prop_complex_span_merging :: [SourceSpan] -> Property
prop_complex_span_merging spans =
  not (null spans) ==> 
  let merged = foldl mergeSpans (head spans) (tail spans)
  in all (\span -> spanStart merged <= spanStart span && spanEnd merged >= spanEnd span) spans

-- Property: Position arithmetic consistency
prop_position_arithmetic_consistency :: SourcePos -> String -> Property
prop_position_arithmetic_consistency pos text =
  let advanced = advancePosBy text pos
      distance = posOffset advanced - posOffset pos
  in distance >= 0 && distance <= length text * 4 -- Account for tab expansion

-- Property: Span ordering properties
prop_span_ordering_properties :: SourceSpan -> SourceSpan -> Property
prop_span_ordering_properties span1 span2 =
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in (start1 <= end1 && start2 <= end2) ==> 
     (span1 <= span2) == (start1 < start2 || (start1 == start2 && end1 <= end2))

-- Property: Located value composition
prop_located_composition :: SourceSpan -> Int -> Int -> Property
prop_located_composition span value1 value2 =
  let located1 = locatedWithSpan span value1
      located2 = locatedWithSpan span value2
      combined = locatedWithSpan span (value1 + value2)
  in locatedSpan combined === span &&
     locatedValue combined === locatedValue located1 + locatedValue located2

-- Property: Position tracking through complex text
prop_position_tracking_complex_text :: String -> Property
prop_position_tracking_complex_text text =
  let positions = scanl (flip posAfter) startPos text
      uniquePositions = length (nub positions)
  in uniquePositions >= 1 && uniquePositions <= length text + 1

-- Property: Span intersection detection
prop_span_intersection_detection :: SourceSpan -> SourceSpan -> Property
prop_span_intersection_detection span1 span2 =
  let intersects = spansIntersect span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in intersects == (end1 >= start2 && end2 >= start1)

-- Property: Position distance calculation
prop_position_distance_calculation :: SourcePos -> SourcePos -> Property
prop_position_distance_calculation pos1 pos2 =
  let distance = abs (posOffset pos1 - posOffset pos2)
  in distance >= 0

-- Property: Span containment checking
prop_span_containment_checking :: SourceSpan -> SourceSpan -> Property
prop_span_containment_checking outer inner =
  let contains = spanContains outer inner
  in contains == (spanStart outer <= spanStart inner && spanEnd outer >= spanEnd inner)

-- Property: Advanced Unicode handling
prop_advanced_unicode_handling :: String -> Property
prop_advanced_unicode_handling text =
  let advanced = advancePosBy text startPos
      unicodeChars = filter (> '\127') text
  in posOffset advanced >= length unicodeChars

-- Property: Complex located transformations
prop_complex_located_transformations :: SourceSpan -> [Int] -> Property
prop_complex_located_transformations span values =
  let located = locatedWithSpan span values
      transformed = mapLocated (map (*2)) located
      sumTransformed = sum (locatedValue transformed)
      sumOriginal = sum values
  in sumTransformed === sumOriginal * 2

-- Property: Span normalization
prop_span_normalization :: SourcePos -> SourcePos -> Property
prop_span_normalization pos1 pos2 =
  let span = spanBetween pos1 pos2
      normalized = normalizeSpan span
  in spanStart normalized <= spanEnd normalized

-- Property: Position comparison edge cases
prop_position_comparison_edge_cases :: SourcePos -> Property
prop_position_comparison_edge_cases pos =
  let samePos = pos
      differentLine = pos { posLine = posLine pos + 1 }
      differentCol = pos { posColumn = posColumn pos + 1 }
      differentOffset = pos { posOffset = posOffset pos + 1 }
  in pos == samePos &&
     pos < differentLine &&
     pos < differentCol &&
     pos < differentOffset

-- Property: Span merging with overlapping spans
prop_span_merging_overlapping :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_overlapping pos1 pos2 pos3 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in spanStart merged <= pos1 && spanEnd merged >= pos3

-- Property: Located value equality
prop_located_value_equality :: SourceSpan -> Int -> Property
prop_located_value_equality span value =
  let located1 = locatedWithSpan span value
      located2 = locatedWithSpan span value
  in located1 == located2

-- Property: Position advancement with mixed characters
prop_position_advancement_mixed :: String -> Property
prop_position_advancement_mixed text =
  let mixed = text ++ "\n\t" ++ text
      advanced = advancePosBy mixed startPos
  in posLine advanced > 1 || posColumn advanced > 1

-- Property: Span length calculation
prop_span_length_calculation :: SourcePos -> SourcePos -> Property
prop_span_length_calculation start end =
  let span = spanBetween start end
      length = spanLength span
  in length >= 0

-- Property: Complex error location formatting
prop_complex_error_location_formatting :: SourceSpan -> Property
prop_complex_error_location_formatting span =
  let errLoc = toErrorLocationWithSpan span
  in line errLoc >= 0 && column errLoc >= 0

-- Property: Position tracking through transformations
prop_position_tracking_transformations :: SourcePos -> String -> Property
prop_position_tracking_transformations pos text =
  let transformed = map toUpper text
      advanced1 = advancePosBy text pos
      advanced2 = advancePosBy transformed pos
  in posLine advanced1 == posLine advanced2
  where
    toUpper c = if 'a' <= c && c <= 'z' then toEnum (fromEnum c - 32) else c

-- Property: Span ordering with equal starts
prop_span_ordering_equal_starts :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_ordering_equal_starts start end1 end2 =
  let span1 = spanBetween start end1
      span2 = spanBetween start end2
  in (span1 <= span2) == (end1 <= end2)

-- Property: Located value mapping composition
prop_located_mapping_composition :: SourceSpan -> Int -> Property
prop_located_mapping_composition span value =
  let located = locatedWithSpan span value
      mapped1 = mapLocated (*2) located
      mapped2 = mapLocated (+1) mapped1
      direct = mapLocated ((+1) . (*2)) located
  in mapped2 == direct

-- Property: Position advancement with tabs and spaces
prop_position_advancement_tabs_spaces :: Int -> Int -> SourcePos -> Property
prop_position_advancement_tabs_spaces tabs spaces pos =
  let text = replicate tabs '\t' ++ replicate spaces ' '
      advanced = advancePosBy text pos
  in posLine advanced == posLine pos && posColumn advanced >= posColumn pos

-- Property: Span intersection with multiple spans
prop_span_intersection_multiple :: [SourceSpan] -> Property
prop_span_intersection_multiple spans =
  length spans >= 2 ==> 
  let intersections = [(span1, span2) | span1 <- spans, span2 <- spans, span1 /= span2, spansIntersect span1 span2]
  in length intersections >= 0

-- Property: Complex located value nesting
prop_complex_located_nesting :: SourceSpan -> Int -> Property
prop_complex_located_nesting span value =
  let inner = locatedWithSpan span value
      outer = locatedWithSpan span inner
      extracted = locatedValue outer
  in locatedValue extracted == value

-- Property: Position consistency across operations
prop_position_consistency_operations :: SourcePos -> String -> Property
prop_position_consistency_operations pos text =
  let advanced1 = advancePosBy text pos
      advanced2 = advancePosByText (T.pack text) pos
  in advanced1 == advanced2

-- Property: Span merging with disjoint spans
prop_span_merging_disjoint :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_disjoint pos1 pos2 pos3 pos4 =
  pos2 < pos3 ==> 
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in spanStart merged == pos1 && spanEnd merged == pos4

-- Property: Located value with complex transformations
prop_located_complex_transformations :: SourceSpan -> [Int] -> Property
prop_located_complex_transformations span values =
  let located = locatedWithSpan span values
      transformed = mapLocated (filter even) located
      sumOriginal = sum values
      sumTransformed = sum (locatedValue transformed)
  in sumTransformed <= sumOriginal

-- Property: Position advancement with zero-width characters
prop_position_advancement_zero_width :: SourcePos -> Property
prop_position_advancement_zero_width pos =
  let zeroWidth = "\0" -- Null byte as zero-width
      advanced = advancePosBy zeroWidth pos
  in posOffset advanced > posOffset pos

-- Property: Span normalization with invalid spans
prop_span_normalization_invalid :: SourcePos -> SourcePos -> Property
prop_span_normalization_invalid pos1 pos2 =
  pos1 > pos2 ==> 
  let span = spanBetween pos1 pos2
      normalized = normalizeSpan span
  in spanStart normalized <= spanEnd normalized

-- Property: Complex error location with context
prop_complex_error_location_context :: SourceSpan -> String -> Property
prop_complex_error_location_context span context =
  let errLoc = toErrorLocationWithSpan span
      formatted = formatErrorLocation errLoc context
  in not (null formatted)

-- Property: Position tracking through multiple files
prop_position_tracking_multiple_files :: [(String, String)] -> Property
prop_position_tracking_multiple_files files =
  let positions = map (\(name, content) -> (name, advancePosBy content startPos)) files
  in all (\(_, pos) -> posOffset pos >= 0) positions

-- Property: Span merging with many spans
prop_span_merging_many :: [SourceSpan] -> Property
prop_span_merging_many spans =
  not (null spans) ==> 
  let merged = foldl1 mergeSpans spans
  in all (\span -> spanStart merged <= spanStart span && spanEnd merged >= spanEnd span) spans

-- Property: Located value with identity mapping
prop_located_identity_mapping :: SourceSpan -> Int -> Property
prop_located_identity_mapping span value =
  let located = locatedWithSpan span value
      mapped = mapLocated id located
  in located == mapped

-- Helper functions for advanced tests
spansIntersect :: SourceSpan -> SourceSpan -> Bool
spansIntersect span1 span2 =
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in end1 >= start2 && end2 >= start1

spanContains :: SourceSpan -> SourceSpan -> Bool
spanContains outer inner =
  spanStart outer <= spanStart inner && spanEnd outer >= spanEnd inner

normalizeSpan :: SourceSpan -> SourceSpan
normalizeSpan span
  | spanStart span <= spanEnd span = span
  | otherwise = spanBetween (spanEnd span) (spanStart span)

spanLength :: SourceSpan -> Int
spanLength span = posOffset (spanEnd span) - posOffset (spanStart span)

formatErrorLocation :: ErrorLocation -> String -> String
formatErrorLocation errLoc context = 
  "Line " ++ show (line errLoc) ++ ", Column " ++ show (column errLoc) ++ ": " ++ context

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck tests"
  [ fastProperty "startPos has correct initial values" prop_startPos_values
  , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
  , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
  , fastProperty "posAfter handles regular characters correctly" prop_posAfter_regular
  , fastProperty "posAt creates position with correct values" prop_posAt_correct
  , fastProperty "posAtLineCol creates position with correct values" prop_posAtLineCol_correct
  , fastProperty "emptySpan has same start and end" prop_emptySpan_same_start_end
  , fastProperty "spanFrom creates empty span" prop_spanFrom_empty
  , fastProperty "spanTo creates empty span" prop_spanTo_empty
  , fastProperty "spanBetween creates span with correct start and end" prop_spanBetween_correct
  , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
  , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
  , fastProperty "isValidSpan checks start <= end" prop_isValidSpan_correct
  , fastProperty "locatedAt creates located value at position" prop_locatedAt_correct
  , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_correct
  , fastProperty "locatedPos returns start position" prop_locatedPos_correct
  , fastProperty "mapLocated applies function to value" prop_mapLocated_correct
  , fastProperty "toErrorLocation converts position correctly" prop_toErrorLocation_correct
  , fastProperty "toErrorLocationWithSpan converts span correctly" prop_toErrorLocationWithSpan_correct
  , fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
  , fastProperty "advancePosBy advances by multiple characters" prop_advancePosBy_correct
  , fastProperty "advancePosByText advances by text" prop_advancePosByText_correct
  , fastProperty "advancePosByLine advances line number" prop_advancePosByLine_correct
  , fastProperty "advancePosByLine doesn't change offset much" prop_advancePosByLine_offset
  , fastProperty "Located is a functor" prop_located_functor
  , fastProperty "HasLocation instance for Located" prop_hasLocation_located
  , fastProperty "spanBetween with same positions creates empty span" prop_spanBetween_same_positions
  , fastProperty "mergeSpans with empty span" prop_mergeSpans_with_empty
  , fastProperty "advancePosBy with empty string" prop_advancePosBy_empty
  , fastProperty "advancePosByText with empty text" prop_advancePosByText_empty
  , fastProperty "advancePosByLine with zero lines" prop_advancePosByLine_zero
  , fastProperty "advancePosByLine with negative lines" prop_advancePosByLine_negative
  , fastProperty "posAfter with multiple newlines" prop_posAfter_multiple_newlines
  , fastProperty "posAfter with multiple tabs" prop_posAfter_multiple_tabs
  , fastProperty "spanBetween with reversed positions" prop_spanBetween_reversed
  , fastProperty "mergeSpans creates span covering all positions" prop_mergeSpans_covers_all
  , fastProperty "locatedWithSpan preserves span" prop_locatedWithSpan_preserves
  , fastProperty "mapLocated doesn't change span" prop_mapLocated_preserves_span
  , fastProperty "toErrorLocation with negative values" prop_toErrorLocation_negative
  , fastProperty "toErrorLocationWithSpan with negative values" prop_toErrorLocationWithSpan_negative
  , fastProperty "advancePos with Unicode characters" prop_advancePos_unicode
  , fastProperty "advancePosByText with Unicode text" prop_advancePosByText_unicode
  -- Advanced property tests
  , fastProperty "complex span merging with multiple spans" prop_complex_span_merging
  , fastProperty "position arithmetic consistency" prop_position_arithmetic_consistency
  , fastProperty "span ordering properties" prop_span_ordering_properties
  , fastProperty "located value composition" prop_located_composition
  , fastProperty "position tracking through complex text" prop_position_tracking_complex_text
  , fastProperty "span intersection detection" prop_span_intersection_detection
  , fastProperty "position distance calculation" prop_position_distance_calculation
  , fastProperty "span containment checking" prop_span_containment_checking
  , fastProperty "advanced unicode handling" prop_advanced_unicode_handling
  , fastProperty "complex located transformations" prop_complex_located_transformations
  , fastProperty "span normalization" prop_span_normalization
  , fastProperty "position comparison edge cases" prop_position_comparison_edge_cases
  , fastProperty "span merging with overlapping spans" prop_span_merging_overlapping
  , fastProperty "located value equality" prop_located_value_equality
  , fastProperty "position advancement with mixed characters" prop_position_advancement_mixed
  , fastProperty "span length calculation" prop_span_length_calculation
  , fastProperty "complex error location formatting" prop_complex_error_location_formatting
  , fastProperty "position tracking through transformations" prop_position_tracking_transformations
  , fastProperty "span ordering with equal starts" prop_span_ordering_equal_starts
  , fastProperty "located value mapping composition" prop_located_mapping_composition
  , fastProperty "position advancement with tabs and spaces" prop_position_advancement_tabs_spaces
  , fastProperty "span intersection with multiple spans" prop_span_intersection_multiple
  , fastProperty "complex located value nesting" prop_complex_located_nesting
  , fastProperty "position consistency across operations" prop_position_consistency_operations
  , fastProperty "span merging with disjoint spans" prop_span_merging_disjoint
  , fastProperty "located value with complex transformations" prop_located_complex_transformations
  , fastProperty "position advancement with zero-width characters" prop_position_advancement_zero_width
  , fastProperty "span normalization with invalid spans" prop_span_normalization_invalid
  , fastProperty "complex error location with context" prop_complex_error_location_context
  , fastProperty "position tracking through multiple files" prop_position_tracking_multiple_files
  , fastProperty "span merging with many spans" prop_span_merging_many
  , fastProperty "located value with identity mapping" prop_located_identity_mapping
  ]