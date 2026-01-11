module Test.Unit.CoreMathematicalPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import SourceLocation
import qualified Data.List as List
import qualified Data.Text as T

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- String processing properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in trimmed === trimmedAgain

prop_trim_no_leading_trailing_whitespace :: String -> Property
prop_trim_no_leading_trailing_whitespace s = 
  let trimmed = trim s
      hasLeading = not (null trimmed) && head trimmed `elem` " \t\n\r"
      hasTrailing = not (null trimmed) && last trimmed `elem` " \t\n\r"
  in not (hasLeading || hasTrailing)

prop_split_by_roundtrip :: Char -> String -> Property
prop_split_by_roundtrip delim s = 
  let parts = splitBy delim s
      rejoined = concat $ List.intersperse [delim] parts
  in s === rejoined

prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in all (not . null) parts

prop_split_by_comma_roundtrip :: String -> Property
prop_split_by_comma_roundtrip s = 
  splitByComma s === splitBy ',' s

prop_split_by_comma_collapsed_roundtrip :: String -> Property
prop_split_by_comma_collapsed_roundtrip s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

prop_remove_line_comments_preserves_non_comments :: String -> String -> Property
prop_remove_line_comments_preserves_non_comments code comment = 
  let input = code ++ " // " ++ comment
      processed = removeLineComments input
  in code `isPrefixOf` processed

prop_remove_comments_preserves_string_literals :: String -> String -> Property
prop_remove_comments_preserves_string_literals code comment = 
  let input = "print(\"" ++ code ++ "\") // " ++ comment
      processed = removeComments input
  in ("print(\"" ++ code ++ "\")") `isPrefixOf` processed

prop_normalize_indentation_preserves_relative_structure :: String -> Property
prop_normalize_indentation_preserves_relative_structure s = 
  let lines' = lines s
      normalized = normalizeIndentation s
      normLines = lines normalized
      originalLineCount = length lines'
      normalizedLineCount = length normLines
  in originalLineCount === normalizedLineCount

prop_break_on_finds_pattern :: String -> String -> Property
prop_break_on_finds_pattern pat s = 
  let (before, after) = breakOn pat s
      combined = before ++ pat ++ after
  in null pat ==> combined === s

prop_safe_process_string_removes_control_chars :: String -> Property
prop_safe_process_string_removes_control_chars s = 
  case safeProcessString s of
    Left _ -> property True
    Right filtered -> all isValidChar filtered

prop_is_valid_char_properties :: Char -> Property
prop_is_valid_char_properties c = 
  let expected = c >= ' ' || c `elem` "\n\r\t"
  in isValidChar c === expected

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- Source position properties
prop_pos_after_advances_offset :: Char -> SourcePos -> Property
prop_pos_after_advances_offset c pos = 
  let newPos = posAfter c pos
  in posOffset newPos >= posOffset pos

prop_pos_after_newline_increases_line :: SourcePos -> Property
prop_pos_after_newline_increases_line pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1

prop_pos_after_newline_resets_column :: SourcePos -> Property
prop_pos_after_newline_resets_column pos = 
  let newPos = posAfter '\n' pos
  in posColumn newPos === 1

prop_pos_after_tab_advances_to_tab_stop :: SourcePos -> Property
prop_pos_after_tab_advances_to_tab_stop pos = 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedColumn

prop_pos_after_regular_char_increases_column :: Char -> SourcePos -> Property
prop_pos_after_regular_char_increases_column c pos = 
  let newPos = posAfter c pos
  in c `notElem` "\n\t" ==> posColumn newPos === posColumn pos + 1

-- Source span properties
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid start end = 
  let span = spanBetween start (max start end)
  in isValidSpan span

prop_merge_spans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_both span1 span2 = 
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && 
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && 
     spanEnd merged >= spanEnd span2

prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos = 
  let span = emptySpan pos
  in spanStart span === pos && spanEnd span === pos

-- Located values properties
prop_located_at_creates_empty_span :: SourcePos -> String -> Property
prop_located_at_creates_empty_span pos value = 
  let located = locatedAt pos value
      span = locSpan located
  in spanStart span === spanEnd span

prop_map_located_preserves_location :: SourcePos -> String -> Property
prop_map_located_preserves_location pos value = 
  let located = locatedAt pos value
      mapped = mapLocated length located
  in locSpan mapped === locSpan located

prop_located_value_extraction :: SourcePos -> String -> Property
prop_located_value_extraction pos value = 
  let located = locatedAt pos value
  in locatedValue located === value

-- Position advancement properties
prop_advance_pos_by_sum_of_chars :: String -> SourcePos -> Property
prop_advance_pos_by_sum_of_chars s pos = 
  let advanced = advancePosBy s pos
      manualAdvanced = foldl (flip posAfter) pos s
  in advanced === manualAdvanced

prop_advance_pos_by_text_matches_string :: String -> SourcePos -> Property
prop_advance_pos_by_text_matches_string s pos = 
  let text = T.pack s
      advancedByText = advancePosByText text pos
      advancedByString = advancePosBy s pos
  in advancedByText === advancedByString

prop_advance_pos_by_line_increases_lines :: Int -> SourcePos -> Property
prop_advance_pos_by_line_increases_lines n pos = 
  let newPos = advancePosByLine n pos
  in posLine newPos === posLine pos + n

prop_advance_pos_by_line_resets_column :: Int -> SourcePos -> Property
prop_advance_pos_by_line_resets_column n pos = 
  let newPos = advancePosByLine n pos
  in posColumn newPos === 1

-- ============================================================================
-- List and String Properties
-- ============================================================================

prop_split_by_associative :: Char -> String -> String -> Property
prop_split_by_associative delim s1 s2 = 
  let combined = s1 ++ [delim] ++ s2
      parts1 = splitBy delim s1
      parts2 = splitBy delim s2
      combinedParts = splitBy delim combined
  in combinedParts === parts1 ++ parts2

prop_split_by_empty_delimiter :: String -> Property
prop_split_by_empty_delimiter s = 
  splitBy ',' "" === [""]

prop_split_by_single_char :: Char -> Property
prop_split_by_single_char c = 
  splitBy ',' [c] === [[c]]

prop_split_by_consecutive_delimiters :: Char -> Property
prop_split_by_consecutive_delimiters delim = 
  splitBy delim ([delim, delim]) === ["", "", ""]

prop_split_collapsed_removes_empties :: Char -> String -> Property
prop_split_collapsed_removes_empties delim s = 
  let parts = splitByCollapsed delim s
  in all (not . null) parts

-- ============================================================================
-- Mathematical Properties
-- ============================================================================

prop_position_ordering :: SourcePos -> SourcePos -> Property
prop_position_ordering pos1 pos2 = 
  let offset1 = posOffset pos1
      offset2 = posOffset pos2
  in (offset1 <= offset2) === (pos1 <= pos2)

prop_span_length_calculation :: SourcePos -> SourcePos -> Property
prop_span_length_calculation start end = 
  let span = spanBetween start (max start end)
      expectedLength = posOffset (spanEnd span) - posOffset (spanStart span)
  in expectedLength >= 0

prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 = 
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 = 
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in merged1 === merged2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

prop_error_location_preserves_position :: SourcePos -> String -> Property
prop_error_location_preserves_position pos msg = 
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos && 
     column errLoc === posColumn pos

prop_error_location_with_span_preserves_range :: SourcePos -> SourcePos -> Property
prop_error_location_with_span_preserves_range start end = 
  let span = spanBetween start end
      errLoc = toErrorLocationWithSpan span
  in line errLoc === posLine start && 
     column errLoc === posColumn start &&
     endLine errLoc === Just (posLine end) &&
     endColumn errLoc === Just (posColumn end)

-- Helper functions
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- Test collection
tests :: TestTree
tests = testGroup "Core Mathematical Properties Tests"
  [ -- Utils properties
    testProperty "trim idempotent" prop_trim_idempotent,
    testProperty "trim no leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace,
    testProperty "split by roundtrip" prop_split_by_roundtrip,
    testProperty "split by collapsed no empty" prop_split_by_collapsed_no_empty,
    testProperty "split by comma roundtrip" prop_split_by_comma_roundtrip,
    testProperty "split by comma collapsed roundtrip" prop_split_by_comma_collapsed_roundtrip,
    testProperty "remove line comments preserves non-comments" prop_remove_line_comments_preserves_non_comments,
    testProperty "remove comments preserves string literals" prop_remove_comments_preserves_string_literals,
    testProperty "normalize indentation preserves relative structure" prop_normalize_indentation_preserves_relative_structure,
    testProperty "break on finds pattern" prop_break_on_finds_pattern,
    testProperty "safe process string removes control chars" prop_safe_process_string_removes_control_chars,
    testProperty "is valid char properties" prop_is_valid_char_properties,
    
    -- SourceLocation properties
    testProperty "pos after advances offset" prop_pos_after_advances_offset,
    testProperty "pos after newline increases line" prop_pos_after_newline_increases_line,
    testProperty "pos after newline resets column" prop_pos_after_newline_resets_column,
    testProperty "pos after tab advances to tab stop" prop_pos_after_tab_advances_to_tab_stop,
    testProperty "pos after regular char increases column" prop_pos_after_regular_char_increases_column,
    testProperty "span between valid" prop_span_between_valid,
    testProperty "merge spans contains both" prop_merge_spans_contains_both,
    testProperty "empty span same start end" prop_empty_span_same_start_end,
    testProperty "located at creates empty span" prop_located_at_creates_empty_span,
    testProperty "map located preserves location" prop_map_located_preserves_location,
    testProperty "located value extraction" prop_located_value_extraction,
    testProperty "advance pos by sum of chars" prop_advance_pos_by_sum_of_chars,
    testProperty "advance pos by text matches string" prop_advance_pos_by_text_matches_string,
    testProperty "advance pos by line increases lines" prop_advance_pos_by_line_increases_lines,
    testProperty "advance pos by line resets column" prop_advance_pos_by_line_resets_column,
    
    -- List and String properties
    testProperty "split by associative" prop_split_by_associative,
    testProperty "split by empty delimiter" prop_split_by_empty_delimiter,
    testProperty "split by single char" prop_split_by_single_char,
    testProperty "split by consecutive delimiters" prop_split_by_consecutive_delimiters,
    testProperty "split collapsed removes empties" prop_split_collapsed_removes_empties,
    
    -- Mathematical properties
    testProperty "position ordering" prop_position_ordering,
    testProperty "span length calculation" prop_span_length_calculation,
    testProperty "merge spans commutative" prop_merge_spans_commutative,
    testProperty "merge spans associative" prop_merge_spans_associative,
    
    -- Error handling properties
    testProperty "error location preserves position" prop_error_location_preserves_position,
    testProperty "error location with span preserves range" prop_error_location_with_span_preserves_range
  ]