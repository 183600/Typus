{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewUtilsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import SourceLocation (SourcePos(..))
import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.String (IsString)
import qualified Data.Text as T

-- ============================================================================
-- Utils Module QuickCheck Tests
-- ============================================================================

-- Test trim function
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_removes_leading_trailing_spaces :: String -> Property
prop_trim_removes_leading_trailing_spaces s = 
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then property $ not (isPrefixOf " " trimmed || isSuffixOf " " trimmed)
     else property $ trimmed === s

prop_trim_preserves_non_space_content :: String -> Property
prop_trim_preserves_non_space_content s = 
  let nonSpaceContent = filter (not . isSpace) s
      trimmedNonSpaceContent = filter (not . isSpace) (trim s)
  in property $ nonSpaceContent === trimmedNonSpaceContent

-- Test splitBy function
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim = splitBy delim "" === []

prop_split_by_single_char :: Char -> Char -> Property
prop_split_by_single_char delim c = 
  if c == delim 
  then splitBy delim [c] === ["", ""]
  else splitBy delim [c] === [[c]]

prop_split_by_all_delimiters :: Char -> Positive Int -> Property
prop_split_by_all_delimiters delim (Positive n) = 
  let delimiters = replicate n delim
      result = splitBy delim delimiters
      expected = replicate (n + 1) ""
  in property $ result === expected

prop_split_by_roundtrip :: Char -> String -> Property
prop_split_by_roundtrip delim s = 
  let parts = splitBy delim s
  in if null parts
     then property $ "" === s
     else let reconstructed = concat $ map (\p -> p ++ [delim]) (init parts) ++ [last parts]
          in property $ take (length s + length parts - 1) reconstructed === s

prop_split_by_collapsed_consistency :: Char -> String -> Property
prop_split_by_collapsed_consistency delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in property $ collapsed === filter (not . null) normal

-- Test splitByComma functions
prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s = splitByComma s === splitBy ',' s

prop_split_by_comma_collapsed_consistency :: String -> Property
prop_split_by_comma_collapsed_consistency s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Test removeLineComments function
prop_remove_line_comments_preserves_non_commented :: String -> Property
prop_remove_line_comments_preserves_non_commented s = 
  if "//" `Data.List.isInfixOf` s
  then property $ True  -- If contains comment pattern, skip this test
  else property $ removeLineComments s === s

prop_remove_line_comments_handles_empty :: Property
prop_remove_line_comments_handles_empty = removeLineComments "" === ""

prop_remove_line_comments_handles_only_comment :: String -> Property
prop_remove_line_comments_handles_only_comment s = 
  let comment = "//" ++ s
      result = removeLineComments comment
  in property $ null result || all isSpace result

prop_remove_line_comments_preserves_strings :: String -> Property
prop_remove_line_comments_preserves_strings s = 
  let stringWithComment = "let x = \"// not a comment\" // actual comment"
      result = removeLineComments stringWithComment
  in property $ "// not a comment" `Data.List.isInfixOf` result

-- Test removeComments function
prop_remove_comments_preserves_non_commented :: String -> Property
prop_remove_comments_preserves_non_commented s = 
  if ("//" `Data.List.isInfixOf` s) || ("/*" `Data.List.isInfixOf` s)
  then property $ True  -- If contains comment pattern, skip this test
  else property $ removeComments s === s

prop_remove_comments_handles_empty :: Property
prop_remove_comments_handles_empty = removeComments "" === ""

prop_remove_comments_block_comments :: String -> String -> Property
prop_remove_comments_block_comments before after = 
  let input = before ++ "/* comment */" ++ after
      result = removeComments input
  in property $ before `isPrefixOf` result && after `isSuffixOf` result

prop_remove_comments_preserves_strings :: String -> Property
prop_remove_comments_preserves_strings s = 
  let stringWithComment = "let x = \"/* not a comment */\" // actual comment"
      result = removeComments stringWithComment
  in property $ "/* not a comment */" `Data.List.isInfixOf` result

-- Test normalizeIndentation function
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s = 
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in if length lines' <= 1
     then property $ normalized === s
     else property $ length normalizedLines === length lines'

prop_normalize_indentation_handles_empty :: Property
prop_normalize_indentation_handles_empty = normalizeIndentation "" === ""

prop_normalize_indentation_single_line :: String -> Property
prop_normalize_indentation_single_line s = 
  let singleLine = if '\n' `elem` s then takeWhile (/= '\n') s else s
  in property $ normalizeIndentation singleLine === singleLine

prop_normalize_indentation_removes_common_prefix :: Property
prop_normalize_indentation_removes_common_prefix = 
  let input = "  line1\n    line2\n  line3"
      result = normalizeIndentation input
      expected = "line1\n  line2\nline3"
  in property $ result === expected

-- Test forceSingleTabIndentation function
prop_force_single_tab_indentation_adds_tab :: String -> Property
prop_force_single_tab_indentation_adds_tab s = 
  let trimmed = trim s
      result = forceSingleTabIndentation s
      resultLines = lines result
  in if null trimmed
     then property $ all (\line -> null line || line == "\t") resultLines
     else property $ all (\line -> null line || '\t' `elem` line) resultLines

-- Test breakOn function
prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern s = breakOn "" s === ("", s)

prop_break_on_pattern_not_found :: String -> String -> Property
prop_break_on_pattern_not_found pat s = 
  if pat `isPrefixOf` s || pat `Data.List.isInfixOf` s
  then property $ True  -- Skip if pattern is found
  else property $ breakOn pat s === (s, "")

prop_break_on_pattern_at_start :: String -> String -> Property
prop_break_on_pattern_at_start pat s = 
  let input = pat ++ s
  in property $ breakOn pat input === ("", s)

prop_break_on_pattern_in_middle :: String -> String -> String -> Property
prop_break_on_pattern_in_middle pat before after = 
  let input = before ++ pat ++ after
      (prefix, suffix) = breakOn pat input
  in if null pat
     then (prefix === "") .&&. (suffix === input)
     else if null before && null after && not (null pat)
          then (prefix === "") .&&. (suffix === "")  -- Special case: only pat
          else (prefix === before) .&&. (suffix === after)

-- Test safeProcessString function
prop_safe_process_string_preserves_valid :: String -> Property
prop_safe_process_string_preserves_valid s = 
  let hasControl = any isControl s && not (any (`elem` "\n\r\t\DEL") s)
  in if hasControl
     then property $ case safeProcessString s of
                        Left _ -> True
                        Right processed -> not (any isControl processed) || 
                                          all (`elem` "\n\r\t\DEL") (filter isControl processed)
     else property $ safeProcessString s === Right s

prop_safe_process_string_handles_empty :: Property
prop_safe_process_string_handles_empty = safeProcessString "" === Right ""

-- Test isValidChar function
prop_is_valid_char_printable :: Char -> Property
prop_is_valid_char_printable c = 
  if c >= ' ' || c `elem` "\n\r\t"
  then property $ isValidChar c
  else property $ not (isValidChar c)

prop_is_valid_char_control :: Char -> Property
prop_is_valid_char_control c = 
  if isControl c && not (c `elem` "\n\r\t")
  then property $ not (isValidChar c)
  else property $ isValidChar c

-- Unit tests for edge cases
test_utils_edge_cases :: TestTree
test_utils_edge_cases = testGroup "Utils Edge Cases"
  [ testCase "trim with mixed spaces" $
      assertEqual "trim mixed spaces" "hello" (trim "  hello  ")
    
  , testCase "splitBy with empty parts" $
      assertEqual "splitBy empty parts" ["", "", ""] (splitBy ',' ",,")
    
  , testCase "removeLineComments with multiple lines" $
      assertEqual "remove line comments" 
                  "let x = 42\nlet y = 24" 
                  (removeLineComments "let x = 42 // comment\nlet y = 24 // another")
    
  , testCase "removeComments with nested" $
      assertEqual "remove nested comments"
                  "code  more code"
                  (removeComments "code /* outer /* inner */ */ more code")
    
  , testCase "normalizeIndentation with tabs" $
      assertEqual "normalize tabs"
                  "line1\n  line2"
                  (normalizeIndentation "\tline1\n\t\tline2")
    
  , testCase "breakOn with exact match" $
      assertEqual "breakOn exact"
                  ("hello", "world")
                  (breakOn "," "hello,world")
  ]

-- QuickCheck properties
test_utils_properties :: TestTree
test_utils_properties = testGroup "Utils QuickCheck Properties"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim removes leading/trailing spaces" prop_trim_removes_leading_trailing_spaces
  , testProperty "trim preserves non-space content" prop_trim_preserves_non_space_content
  , testProperty "splitBy empty string" prop_split_by_empty_string
  , testProperty "splitBy single char" prop_split_by_single_char
  , testProperty "splitBy all delimiters" prop_split_by_all_delimiters
  , testProperty "splitBy roundtrip" prop_split_by_roundtrip
  , testProperty "splitBy collapsed consistency" prop_split_by_collapsed_consistency
  , testProperty "splitBy comma consistency" prop_split_by_comma_consistency
  , testProperty "splitBy comma collapsed consistency" prop_split_by_comma_collapsed_consistency
  , testProperty "remove line comments preserves non-commented" prop_remove_line_comments_preserves_non_commented
  , testProperty "remove line comments handles empty" prop_remove_line_comments_handles_empty
  , testProperty "remove line comments handles only comment" prop_remove_line_comments_handles_only_comment
  , testProperty "remove line comments preserves strings" prop_remove_line_comments_preserves_strings
  , testProperty "remove comments preserves non-commented" prop_remove_comments_preserves_non_commented
  , testProperty "remove comments handles empty" prop_remove_comments_handles_empty
  , testProperty "remove comments block comments" prop_remove_comments_block_comments
  , testProperty "remove comments preserves strings" prop_remove_comments_preserves_strings
  , testProperty "normalize indentation preserves relative" prop_normalize_indentation_preserves_relative
  , testProperty "normalize indentation handles empty" prop_normalize_indentation_handles_empty
  , testProperty "normalize indentation single line" prop_normalize_indentation_single_line
  , testProperty "normalize indentation removes common prefix" prop_normalize_indentation_removes_common_prefix
  , testProperty "force single tab indentation adds tab" prop_force_single_tab_indentation_adds_tab
  , testProperty "breakOn empty pattern" prop_break_on_empty_pattern
  , testProperty "breakOn pattern not found" prop_break_on_pattern_not_found
  , testProperty "breakOn pattern at start" prop_break_on_pattern_at_start
  , testProperty "breakOn pattern in middle" prop_break_on_pattern_in_middle
  , testProperty "safe process string preserves valid" prop_safe_process_string_preserves_valid
  , testProperty "safe process string handles empty" prop_safe_process_string_handles_empty
  , testProperty "isValidChar printable" prop_is_valid_char_printable
  , testProperty "isValidChar control" prop_is_valid_char_control
  ]

-- Main test suite
utilsTests :: TestTree
utilsTests = testGroup "Utils Module Tests"
  [ test_utils_edge_cases
  , test_utils_properties
  ]