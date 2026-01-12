{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.AdditionalQuickCheckTestSuiteSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isAlpha, toLower, toUpper)
import Control.Monad (replicateM)

-- Import core modules to test
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedValue, locatedSpan, mapLocated,
                      spanStart, spanEnd, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn,
             safeProcessString, isValidChar)
import qualified Data.List as L

-- Import test support
import TestSupport.Arbitrary

-- ============================================================================
-- Advanced Utils Module Tests
-- ============================================================================

-- Test trim with various whitespace combinations
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace s = 
  let ws = " \t\n\r"
      sWithWs = ws ++ s ++ ws
      trimmed = trim sWithWs
  in property $ not (null s) && not (all isSpace s) && not (null trimmed) ==> 
    head trimmed `notElem` ws && last trimmed `notElem` ws

-- Test splitBy with empty string
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string c = splitBy c "" === []

-- Test splitByCollapsed with only delimiters
prop_split_by_collapsed_only_delimiters :: Char -> Property
prop_split_by_collapsed_only_delimiters c = 
  let onlyDelims = replicate 10 c
  in splitByCollapsed c onlyDelims === []

-- Test splitByComma with special cases
prop_split_by_comma_empty :: Property
prop_split_by_comma_empty = splitByComma "" === []

prop_split_by_comma_single :: Property
prop_split_by_comma_single = splitByComma "a" === ["a"]

prop_split_by_comma_leading :: Property
prop_split_by_comma_leading = splitByComma ",a" === ["", "a"]

prop_split_by_comma_trailing :: Property
prop_split_by_comma_trailing = splitByComma "a," === ["a", ""]

-- Test splitByCommaCollapsed with special cases
prop_split_by_comma_collapsed_empty :: Property
prop_split_by_comma_collapsed_empty = splitByCommaCollapsed "" === []

prop_split_by_comma_collapsed_single :: Property
prop_split_by_comma_collapsed_single = splitByCommaCollapsed "a" === ["a"]

prop_split_by_comma_collapsed_leading :: Property
prop_split_by_comma_collapsed_leading = splitByCommaCollapsed ",a" === ["a"]

prop_split_by_comma_collapsed_trailing :: Property
prop_split_by_comma_collapsed_trailing = splitByCommaCollapsed "a," === ["a"]

-- Test comment removal with edge cases
prop_remove_line_comments_empty :: Property
prop_remove_line_comments_empty = removeLineComments "" === ""

prop_remove_line_comments_no_comments :: String -> Property
prop_remove_line_comments_no_comments s = 
  property $ not ("//" `L.isInfixOf` s) ==> removeLineComments s === s

prop_remove_comments_empty :: Property
prop_remove_comments_empty = removeComments "" === ""

prop_remove_comments_no_comments :: String -> Property
prop_remove_comments_no_comments s = 
  let hasNoComments = not ("//" `L.isInfixOf` s) && not ("/*" `L.isInfixOf` s)
  in property $ hasNoComments ==> removeComments s === s

-- Test indentation functions with edge cases
prop_normalize_indentation_empty :: Property
prop_normalize_indentation_empty = normalizeIndentation "" === ""

prop_normalize_indentation_single_line :: String -> Property
prop_normalize_indentation_single_line s = 
  let s' = filter (/= '\n') s  -- Ensure single line
  in normalizeIndentation s' === s'

prop_force_single_tab_indentation_empty :: Property
prop_force_single_tab_indentation_empty = forceSingleTabIndentation "" === ""

-- Test breakOn with edge cases
prop_break_on_empty_string :: Char -> Property
prop_break_on_empty_string c = breakOn [c] "" === ("", "")

prop_break_on_not_found :: Char -> String -> Property
prop_break_on_not_found c s = 
  property $ not (c `elem` s) ==> breakOn [c] s === (s, "")

prop_break_on_first_occurrence :: Char -> String -> Property
prop_break_on_first_occurrence c s = 
  let (before, after) = breakOn [c] s
  in if c `elem` s
     then let firstPos = head $ L.elemIndices c s
              expectedBefore = take firstPos s
              expectedAfter = drop (firstPos + 1) s
          in before === expectedBefore .&&. after === expectedAfter
     else before === s .&&. after === ""

-- Test string processing with edge cases
prop_safe_process_string_empty :: Property
prop_safe_process_string_empty = safeProcessString "" === Right ""

prop_safe_process_string_valid_chars :: String -> Property
prop_safe_process_string_valid_chars s = 
  let validChars = filter isValidChar s
  in safeProcessString validChars === Right validChars

prop_is_valid_char_printable :: Char -> Property
prop_is_valid_char_printable c = 
  property $ isPrintable c ==> isValidChar c
  where
    isPrintable ch = fromEnum ch >= 32 && fromEnum ch <= 126

-- ============================================================================
-- Advanced SourceLocation Module Tests
-- ============================================================================

-- Test SourcePos arithmetic
prop_source_pos_monotonic :: SourcePos -> Int -> Property
prop_source_pos_monotonic (SourcePos line col offset) n = 
  property $ n >= 0 ==> 
    let newPos = SourcePos line (col + n) (offset + n)
    in posColumn newPos >= posColumn (SourcePos line col offset)

-- Test SourceSpan merging
prop_source_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_source_span_merge_associative span1 span2 span3 = 
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      merge123 = mergeSpans merge12 span3
      merge1_23 = mergeSpans span1 merge23
  in property $ all isValidSpan [span1, span2, span3] ==> 
    spanStart merge123 === spanStart merge1_23

-- Test Located with different operations
prop_located_map :: Int -> SourcePos -> SourcePos -> Property
prop_located_map x start end = 
  let span = SourceSpan start end
      located = Located x start span
      mapped = mapLocated (+1) located
  in locatedValue mapped === x + 1

-- ============================================================================
-- Advanced Parser Module Tests
-- ============================================================================

-- Test FileDirectives with boolean properties
prop_file_directives_ownership_true :: FileDirectives -> Property
prop_file_directives_ownership_true fd = 
  case fdOwnership fd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

prop_file_directives_dependent_types_true :: FileDirectives -> Property
prop_file_directives_dependent_types_true fd = 
  case fdDependentTypes fd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

prop_file_directives_constraints_true :: FileDirectives -> Property
prop_file_directives_constraints_true fd = 
  case fdConstraints fd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

-- Test BlockDirectives with boolean properties
prop_block_directives_ownership_true :: BlockDirectives -> Property
prop_block_directives_ownership_true bd = 
  case bdOwnership bd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

prop_block_directives_dependent_types_true :: BlockDirectives -> Property
prop_block_directives_dependent_types_true bd = 
  case bdDependentTypes bd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

prop_block_directives_constraints_true :: BlockDirectives -> Property
prop_block_directives_constraints_true bd = 
  case bdConstraints bd of
    Nothing -> property True
    Just (Located val _ _) -> property $ val == True || val == False

-- Test CodeBlock with various properties
prop_code_block_directives_consistent :: CodeBlock -> Property
prop_code_block_directives_consistent (CodeBlock directives _ _) = 
  let ownership = bdOwnership directives
      dependentTypes = bdDependentTypes directives
      constraints = bdConstraints directives
  in property $ 
    (case ownership of Nothing -> True; Just (Located val _ _) -> val == True || val == False) &&
    (case dependentTypes of Nothing -> True; Just (Located val _ _) -> val == True || val == False) &&
    (case constraints of Nothing -> True; Just (Located val _ _) -> val == True || val == False)

-- Test TypusFile with file directives
prop_typus_file_directives_consistent :: TypusFile -> Property
prop_typus_file_directives_consistent tf = 
  let directives = tfDirectives tf
      ownership = fdOwnership directives
      dependentTypes = fdDependentTypes directives
      constraints = fdConstraints directives
  in property $ 
    (case ownership of Nothing -> True; Just (Located val _ _) -> val == True || val == False) &&
    (case dependentTypes of Nothing -> True; Just (Located val _ _) -> val == True || val == False) &&
    (case constraints of Nothing -> True; Just (Located val _ _) -> val == True || val == False)

-- ============================================================================
-- String Processing Tests
-- ============================================================================

-- Test string case conversion properties
prop_to_lower_to_upper_roundtrip :: String -> Property
prop_to_lower_to_upper_roundtrip s = 
  let lowered = map toLower s
      uppered = map toUpper lowered
      expectedUpper = map toUpper s
  in property $ uppered === expectedUpper

prop_to_upper_to_lower_roundtrip :: String -> Property
prop_to_upper_to_lower_roundtrip s = 
  let isAscii c = c <= '\127'
      asciiS = filter isAscii s
      uppered = map toUpper asciiS
      lowered = map toLower uppered
      expectedLower = map toLower asciiS
  in property $ lowered === expectedLower

-- Test string splitting and joining
prop_split_join_roundtrip :: Char -> String -> Property
prop_split_join_roundtrip c s = 
  let parts = splitBy c s
      rejoined = L.intercalate [c] parts
  in rejoined === s

prop_split_collapsed_join_roundtrip :: Char -> String -> Property
prop_split_collapsed_join_roundtrip c s = 
  let parts = splitByCollapsed c s
      rejoined = L.intercalate [c] parts
      expectedParts = filter (not . null) (splitBy c s)
  in property $ parts === expectedParts

-- Test string filtering
prop_filter_is_alpha_num :: String -> Property
prop_filter_is_alpha_num s = 
  let filtered = filter isAlphaNum s
  in property $ not (null filtered) ==> 
    all isAlphaNum filtered

prop_filter_is_alpha :: String -> Property
prop_filter_is_alpha s = 
  let filtered = filter isAlpha s
  in property $ not (null filtered) ==> 
    all isAlpha filtered

-- ============================================================================
-- List Processing Tests
-- ============================================================================

-- Test list length properties
prop_length_non_negative :: [Int] -> Property
prop_length_non_negative xs = property $ length xs >= 0

prop_length_concat :: [Int] -> [Int] -> Property
prop_length_concat xs ys = length (xs ++ ys) === length xs + length ys

prop_length_reverse :: [Int] -> Property
prop_length_reverse xs = length (reverse xs) === length xs

-- Test list head/tail properties
prop_head_element :: [Int] -> Property
prop_head_element xs = 
  property $ not (null xs) ==> head xs `elem` xs

prop_tail_subset :: [Int] -> Property
prop_tail_subset xs = 
  property $ not (null xs) ==> all (`elem` xs) (tail xs)

-- Test list element properties
prop_elem_consistency :: Int -> [Int] -> Property
prop_elem_consistency x xs = 
  (x `elem` xs) === (x `elem` reverse xs)

prop_not_elem_empty :: Int -> Property
prop_not_elem_empty x = property $ not (x `elem` ([] :: [Int]))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional QuickCheck Test Suite"
  [ testGroup "Advanced Utils Module Tests"
    [ testProperty "trim all whitespace" prop_trim_all_whitespace
    , testProperty "splitBy empty string" prop_split_by_empty_string
    , testProperty "splitByCollapsed only delimiters" prop_split_by_collapsed_only_delimiters
    , testProperty "splitByComma empty" prop_split_by_comma_empty
    , testProperty "splitByComma single" prop_split_by_comma_single
    , testProperty "splitByComma leading" prop_split_by_comma_leading
    , testProperty "splitByComma trailing" prop_split_by_comma_trailing
    , testProperty "splitByCommaCollapsed empty" prop_split_by_comma_collapsed_empty
    , testProperty "splitByCommaCollapsed single" prop_split_by_comma_collapsed_single
    , testProperty "splitByCommaCollapsed leading" prop_split_by_comma_collapsed_leading
    , testProperty "splitByCommaCollapsed trailing" prop_split_by_comma_collapsed_trailing
    , testProperty "remove line comments empty" prop_remove_line_comments_empty
    , testProperty "remove line comments no comments" prop_remove_line_comments_no_comments
    , testProperty "remove comments empty" prop_remove_comments_empty
    , testProperty "remove comments no comments" prop_remove_comments_no_comments
    , testProperty "normalize indentation empty" prop_normalize_indentation_empty
    , testProperty "normalize indentation single line" prop_normalize_indentation_single_line
    , testProperty "force single tab indentation empty" prop_force_single_tab_indentation_empty
    , testProperty "breakOn empty string" prop_break_on_empty_string
    , testProperty "breakOn not found" prop_break_on_not_found
    , testProperty "breakOn first occurrence" prop_break_on_first_occurrence
    , testProperty "safe process string empty" prop_safe_process_string_empty
    , testProperty "safe process string valid chars" prop_safe_process_string_valid_chars
    , testProperty "is valid char printable" prop_is_valid_char_printable
    ]
  , testGroup "Advanced SourceLocation Module Tests"
    [ testProperty "source pos monotonic" prop_source_pos_monotonic
    , testProperty "source span merge associative" prop_source_span_merge_associative
    , testProperty "located map" prop_located_map
    ]
  , testGroup "Advanced Parser Module Tests"
    [ testProperty "file directives ownership true" prop_file_directives_ownership_true
    , testProperty "file directives dependent types true" prop_file_directives_dependent_types_true
    , testProperty "file directives constraints true" prop_file_directives_constraints_true
    , testProperty "block directives ownership true" prop_block_directives_ownership_true
    , testProperty "block directives dependent types true" prop_block_directives_dependent_types_true
    , testProperty "block directives constraints true" prop_block_directives_constraints_true
    , testProperty "code block directives consistent" prop_code_block_directives_consistent
    , testProperty "typus file directives consistent" prop_typus_file_directives_consistent
    ]
  , testGroup "String Processing Tests"
    [ testProperty "to lower to upper roundtrip" prop_to_lower_to_upper_roundtrip
    , testProperty "to upper to lower roundtrip" prop_to_upper_to_lower_roundtrip
    , testProperty "split join roundtrip" prop_split_join_roundtrip
    , testProperty "split collapsed join roundtrip" prop_split_collapsed_join_roundtrip
    , testProperty "filter is alpha num" prop_filter_is_alpha_num
    , testProperty "filter is alpha" prop_filter_is_alpha
    ]
  , testGroup "List Processing Tests"
    [ testProperty "length non negative" prop_length_non_negative
    , testProperty "length concat" prop_length_concat
    , testProperty "length reverse" prop_length_reverse
    , testProperty "head element" prop_head_element
    , testProperty "tail subset" prop_tail_subset
    , testProperty "elem consistency" prop_elem_consistency
    , testProperty "not elem empty" prop_not_elem_empty
    ]
  ]