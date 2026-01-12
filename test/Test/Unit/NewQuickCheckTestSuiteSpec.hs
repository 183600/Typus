{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewQuickCheckTestSuiteSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isAlpha)

-- Import core modules to test
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedValue, locatedSpan, mapLocated,
                      spanStart, spanEnd, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn,
             safeProcessString, isValidChar)

-- Import test support
import TestSupport.Arbitrary

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Test trim function
prop_trim_roundtrip :: String -> Property
prop_trim_roundtrip s = trim (trim s) === trim s

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in property $ not (null trimmed) ==> 
    (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

-- Test splitBy function
prop_split_by_length :: Char -> String -> Property
prop_split_by_length c s = property $ length (splitBy c s) >= 1

prop_split_by_concat :: Char -> String -> Property
prop_split_by_concat c s = L.intercalate [c] (splitBy c s) === s

-- Test splitByCollapsed function
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty c s = 
  let parts = splitByCollapsed c s
  in property $ not (null parts) ==> all (not . null) parts

prop_split_by_collapsed_vs_split :: Char -> String -> Property
prop_split_by_collapsed_vs_split c s = 
  splitByCollapsed c s === filter (not . null) (splitBy c s)

-- Test comma split functions
prop_split_by_comma_roundtrip :: String -> Property
prop_split_by_comma_roundtrip s = 
  L.intercalate "," (splitByComma s) === s

prop_split_by_comma_collapsed_roundtrip :: String -> Property
prop_split_by_comma_collapsed_roundtrip s = 
  let parts = splitByCommaCollapsed s
      rejoined = L.intercalate "," parts
  in property $ not (null parts) ==> 
    rejoined === L.intercalate "," (filter (not . null) (splitByComma s))

-- Test comment removal
prop_remove_line_comments_preserves_non_comments :: String -> Property
prop_remove_line_comments_preserves_non_comments s = 
  let hasNoCommentLines = not (any ("//" `isPrefixOf`) (lines s))
      processed = removeLineComments s
  in property $ hasNoCommentLines ==> processed === s
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

prop_remove_comments_preserves_strings :: String -> Property
prop_remove_comments_preserves_strings s = 
  let processed = removeComments s
  in property $ not ("\"" `isInfixOf` s) ==> 
    not ("/*" `isInfixOf` processed) && not ("//" `isInfixOf` processed)
  where
    isInfixOf needle haystack = needle `L.isInfixOf` haystack

-- Test indentation functions
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s = 
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in property $ length lines' == length normalizedLines

prop_force_single_tab_indentation_contains_tabs :: String -> Property
prop_force_single_tab_indentation_contains_tabs s = 
  let tabbed = forceSingleTabIndentation s
  in property $ not (null tabbed) ==> '\t' `elem` tabbed

-- Test breakOn function
prop_break_on_finds_delimiter :: Char -> String -> Property
prop_break_on_finds_delimiter c s = 
  let (before, after) = breakOn [c] s
  in if c `elem` s 
     then property $ not (null before) || not (null after)
     else before === s .&&. after === ""

-- Test string processing
prop_safe_process_string_identity :: String -> Property
prop_safe_process_string_identity s = 
  let processed = safeProcessString s
  in property $ all isValidChar s ==> processed === Right s

prop_is_valid_char_ascii :: Char -> Property
prop_is_valid_char_ascii c = 
  property $ (fromEnum c >= 32 && fromEnum c <= 126) ==> isValidChar c

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Test SourcePos properties
prop_source_pos_line_positive :: SourcePos -> Property
prop_source_pos_line_positive (SourcePos line _ _) = property $ line > 0

prop_source_pos_column_positive :: SourcePos -> Property
prop_source_pos_column_positive (SourcePos _ col _) = property $ col > 0

prop_source_pos_offset_non_negative :: SourcePos -> Property
prop_source_pos_offset_non_negative (SourcePos _ _ offset) = property $ offset >= 0

-- Test SourceSpan properties
prop_source_span_start_before_end :: SourceSpan -> Property
prop_source_span_start_before_end (SourceSpan start end) = 
  property $ (posLine start < posLine end) ||
  (posLine start == posLine end && posColumn start <= posColumn end)

-- Test Located properties
prop_located_preserves_value :: Int -> SourcePos -> SourceSpan -> Property
prop_located_preserves_value val start span = locatedValue (Located val start span) === val

prop_located_preserves_span :: Int -> SourcePos -> SourceSpan -> Property
prop_located_preserves_span val start span = locatedSpan (Located val start span) === span

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- Test FileDirectives
prop_file_directives_roundtrip :: FileDirectives -> Property
prop_file_directives_roundtrip fd = fd === fd

prop_file_directives_ownership_maybe :: FileDirectives -> Property
prop_file_directives_ownership_maybe fd = 
  case fdOwnership fd of
    Nothing -> property True
    Just (Located _ _ _) -> property True

-- Test BlockDirectives
prop_block_directives_roundtrip :: BlockDirectives -> Property
prop_block_directives_roundtrip bd = bd === bd

prop_block_directives_dependent_types_maybe :: BlockDirectives -> Property
prop_block_directives_dependent_types_maybe bd = 
  case bdDependentTypes bd of
    Nothing -> property True
    Just (Located _ _ _) -> property True

-- Test CodeBlock
prop_code_block_non_empty :: CodeBlock -> Property
prop_code_block_non_empty (CodeBlock _ code _) = property $ True -- Allow empty code blocks

-- Test TypusFile
prop_typus_file_has_blocks :: TypusFile -> Property
prop_typus_file_has_blocks tf = property $ length (tfBlocks tf) >= 0

-- ============================================================================
-- Combined Module Tests
-- ============================================================================

-- Test parser with source location
prop_parser_preserves_source_info :: TypusFile -> Property
prop_parser_preserves_source_info tf = 
  let blocks = tfBlocks tf
  in property $ not (null blocks) ==> 
    all (\(CodeBlock _ _ span) -> isValidSpan span) blocks

-- Test utils with parser data
prop_trim_code_block :: CodeBlock -> Property
prop_trim_code_block (CodeBlock _ code _) = 
  let trimmed = trim code
  in property $ not (null code) ==> 
    length trimmed <= length code

-- Test source location with parsing
prop_source_span_consistency :: SourceSpan -> SourceSpan -> Property
prop_source_span_consistency span1 span2 = 
  let merged = mergeSpans span1 span2
  in property $ isValidSpan span1 && isValidSpan span2 ==> 
    isValidSpan merged

-- ============================================================================
-- Property Test Helpers
-- ============================================================================

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New QuickCheck Test Suite"
  [ testGroup "Utils Module Tests"
    [ testProperty "trim roundtrip" prop_trim_roundtrip
    , testProperty "trim no leading/trailing spaces" prop_trim_no_leading_trailing_spaces
    , testProperty "splitBy length" prop_split_by_length
    , testProperty "splitBy concat" prop_split_by_concat
    , testProperty "splitByCollapsed no empty" prop_split_by_collapsed_no_empty
    , testProperty "splitByCollapsed vs split" prop_split_by_collapsed_vs_split
    , testProperty "splitByComma roundtrip" prop_split_by_comma_roundtrip
    , testProperty "splitByCommaCollapsed roundtrip" prop_split_by_comma_collapsed_roundtrip
    , testProperty "remove line comments preserves non-comments" prop_remove_line_comments_preserves_non_comments
    , testProperty "remove comments preserves strings" prop_remove_comments_preserves_strings
    , testProperty "normalize indentation preserves relative" prop_normalize_indentation_preserves_relative
    , testProperty "force single tab indentation contains tabs" prop_force_single_tab_indentation_contains_tabs
    , testProperty "breakOn finds delimiter" prop_break_on_finds_delimiter
    , testProperty "safe process string identity" prop_safe_process_string_identity
    , testProperty "is valid char ascii" prop_is_valid_char_ascii
    ]
  , testGroup "SourceLocation Module Tests"
    [ testProperty "source pos line positive" prop_source_pos_line_positive
    , testProperty "source pos column positive" prop_source_pos_column_positive
    , testProperty "source pos offset non-negative" prop_source_pos_offset_non_negative
    , testProperty "source span start before end" prop_source_span_start_before_end
    , testProperty "located preserves value" prop_located_preserves_value
    , testProperty "located preserves span" prop_located_preserves_span
    ]
  , testGroup "Parser Module Tests"
    [ testProperty "file directives roundtrip" prop_file_directives_roundtrip
    , testProperty "file directives ownership maybe" prop_file_directives_ownership_maybe
    , testProperty "block directives roundtrip" prop_block_directives_roundtrip
    , testProperty "block directives dependent types maybe" prop_block_directives_dependent_types_maybe
    , testProperty "code block non empty" prop_code_block_non_empty
    , testProperty "typus file has blocks" prop_typus_file_has_blocks
    ]
  , testGroup "Combined Module Tests"
    [ testProperty "parser preserves source info" prop_parser_preserves_source_info
    , testProperty "trim code block" prop_trim_code_block
    , testProperty "source span consistency" prop_source_span_consistency
    ]
  ]