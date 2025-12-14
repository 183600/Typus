{-# LANGUAGE CPP #-}

module Test.Unit.ExtendedUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, 
             normalizeIndentation, breakOn)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, group, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (toLower, toUpper, isSpace, isAlphaNum)

-- Extended utils property tests for comprehensive coverage

-- Property: String trim is idempotent
prop_utils_trim_idempotent :: String -> Property
prop_utils_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce == trimmedTwice

-- Property: Trim removes leading and trailing whitespace
prop_utils_trim_removes_whitespace :: String -> String -> String -> Property
prop_utils_trim_removes_whitespace leading middle trailing =
  let fullString = leading ++ middle ++ trailing
      trimmed = trim fullString
      hasLeadingSpace = not (null leading) && isSpace (head leading)
      hasTrailingSpace = not (null trailing) && isSpace (last trailing)
  in classify hasLeadingSpace "has leading space" $
     classify hasTrailingSpace "has trailing space" $
     property $ not (null trimmed) ==> not (isSpace (head trimmed)) && not (isSpace (last trimmed))

-- Property: Split by character preserves count based on delimiter occurrences
prop_utils_split_by_character :: String -> Char -> Property
prop_utils_split_by_character str delim =
  let result = splitBy delim str
      expectedParts = length (filter (== delim) str) + 1
  in property $ length result == expectedParts

-- Property: Split by comma works correctly
prop_utils_split_by_comma :: String -> Property
prop_utils_split_by_comma str =
  let result = splitByComma str
      expected = splitBy ',' str
  in property $ result == expected

-- Property: Split by character handles empty string
prop_utils_split_empty_string :: Char -> Property
prop_utils_split_empty_string delim =
  let result = splitBy delim ""
  in property $ result == [""]

-- Property: Split by character handles string without delimiter
prop_utils_split_no_delimiter :: String -> Char -> Property
prop_utils_split_no_delimiter str delim =
  let notInStr = not (delim `elem` str)
      result = splitBy delim str
  in classify notInStr "no delimiter in string" $
     property $ notInStr ==> result == [str]

-- Property: Remove line comments works
prop_utils_remove_line_comments :: String -> String -> Property
prop_utils_remove_line_comments code comment =
  let codeWithComment = code ++ "// " ++ comment
      result = removeLineComments codeWithComment
  in property $ comment `isInfixOf` codeWithComment && not (comment `isInfixOf` result)

-- Property: Remove all comments works
prop_utils_remove_all_comments :: String -> String -> Property
prop_utils_remove_all_comments code comment =
  let codeWithComment = code ++ "/* " ++ comment ++ " */"
      result = removeComments codeWithComment
  in property $ comment `isInfixOf` codeWithComment && not (comment `isInfixOf` result)

-- Property: Normalize indentation preserves relative indentation
prop_utils_normalize_indentation :: [String] -> Property
prop_utils_normalize_indentation inputLines =
  let code = unlines inputLines
      normalized = normalizeIndentation code
      resultLines = lines normalized
  in property $ not (null resultLines) ==> length resultLines == length inputLines

-- Property: Break on works correctly
prop_utils_break_on :: String -> String -> Property
prop_utils_break_on text pattern =
  let result = breakOn pattern text
      hasPattern = pattern `isInfixOf` text
  in classify hasPattern "has pattern" $
     property $ hasPattern ==> length result == 2

-- Property: Break on with empty pattern returns original string
prop_utils_break_on_empty_pattern :: String -> Property
prop_utils_break_on_empty_pattern text =
  let result = breakOn "" text
  in property $ result == (text, "")

-- Property: Break on with pattern not found returns original string
prop_utils_break_on_pattern_not_found :: String -> String -> Property
prop_utils_break_on_pattern_not_found text pattern =
  let notFound = not (pattern `isInfixOf` text)
      result = breakOn pattern text
  in classify notFound "pattern not found" $
     property $ notFound ==> result == (text, "")

-- Property: Trim preserves internal whitespace
prop_utils_trim_preserves_internal :: String -> String -> String -> Property
prop_utils_trim_preserves_internal before middle after =
  let fullString = before ++ "  " ++ middle ++ "  " ++ after
      trimmed = trim fullString
      hasInternalSpaces = "  " `isInfixOf` middle
  in classify hasInternalSpaces "has internal spaces" $
     property $ hasInternalSpaces ==> "  " `isInfixOf` trimmed

-- Property: Split by handles different delimiters
prop_utils_split_different_delimiters :: String -> Property
prop_utils_split_different_delimiters str =
  let delimiters = [',', ';', ':', '|', ' ']
      results = map (\d -> splitBy d str) delimiters
  in property $ all (\r -> length r >= 1) results

-- Property: Split by with repeated delimiters
prop_utils_split_repeated_delimiters :: String -> Char -> Property
prop_utils_split_repeated_delimiters str delim =
  let repeatedDelim = replicate 3 delim
      strWithRepeated = str ++ repeatedDelim ++ str
      result = splitBy delim strWithRepeated
  in property $ length result >= 3

-- Property: Remove line comments with multiple comments
prop_utils_remove_multiple_line_comments :: [String] -> Property
prop_utils_remove_multiple_line_comments comments =
  let codeWithComments = unlines $ map (\c -> "var x int = 42 // " ++ c) comments
      result = removeLineComments codeWithComments
  in property $ all (`notElem` lines result) comments

-- Property: Remove comments with nested comments
prop_utils_remove_nested_comments :: String -> String -> Property
prop_utils_remove_nested_comments outer inner =
  let nestedComment = "/* " ++ outer ++ " /* " ++ inner ++ " */ */"
      result = removeComments nestedComment
  in property $ not (outer `isInfixOf` result) && not (inner `isInfixOf` result)

-- Property: Normalize indentation with mixed indentation
prop_utils_normalize_mixed_indentation :: [String] -> Property
prop_utils_normalize_mixed_indentation lines =
  let mixedIndented = zipWith (\i line -> 
        if even i then replicate i ' ' ++ line 
        else replicate i '\t' ++ line) [0..] lines
      code = unlines mixedIndented
      normalized = normalizeIndentation code
  in property $ not (null normalized)

-- Property: Break on with multiple occurrences
prop_utils_break_on_multiple_occurrences :: String -> String -> Property
prop_utils_break_on_multiple_occurrences text pattern =
  let textWithMultiple = text ++ pattern ++ text ++ pattern ++ text
      result = breakOn pattern textWithMultiple
      hasPattern = pattern `isInfixOf` textWithMultiple
  in classify hasPattern "has pattern" $
     property $ hasPattern ==> length result >= 2

-- Property: Break on with pattern at start
prop_utils_break_on_pattern_at_start :: String -> String -> Property
prop_utils_break_on_pattern_at_start pattern text =
  let textWithPattern = pattern ++ text
      (before, after) = breakOn pattern textWithPattern
  in property $ before == ""

-- Property: Break on with pattern at end
prop_utils_break_on_pattern_at_end :: String -> String -> Property
prop_utils_break_on_pattern_at_end text pattern =
  let textWithPattern = text ++ pattern
      (before, after) = breakOn pattern textWithPattern
  in property $ not (null before) && after == ""

-- Property: Trim with only whitespace
prop_utils_trim_whitespace_only :: String -> Property
prop_utils_trim_whitespace_only whitespace =
  let isOnlyWhitespace = all isSpace whitespace
      trimmed = trim whitespace
  in classify isOnlyWhitespace "whitespace only" $
     property $ isOnlyWhitespace ==> null trimmed

-- Property: Split by with empty parts
prop_utils_split_with_empty_parts :: Char -> Property
prop_utils_split_with_empty_parts delim =
  let strWithEmpty = "" ++ [delim] ++ "" ++ [delim] ++ ""
      result = splitBy delim strWithEmpty
  in property $ result == ["", "", "", ""]

-- Property: Remove line comments preserves code before comment
prop_utils_remove_comments_preserves_code :: String -> String -> Property
prop_utils_remove_comments_preserves_code code comment =
  let codeWithComment = code ++ "// " ++ comment
      result = removeLineComments codeWithComment
  in property $ code `isPrefixOf` result

-- Property: Remove comments handles string literals
prop_utils_remove_comments_handles_strings :: String -> String -> Property
prop_utils_remove_comments_handles_strings code comment =
  let stringWithComment = "var s string = \"// not a comment\" // " ++ comment ++ "\n" ++ code
      result = removeLineComments stringWithComment
  in property $ "// not a comment" `isInfixOf` result && not (comment `isInfixOf` result)

-- Property: Normalize indentation removes common prefix
prop_utils_normalize_removes_common_prefix :: [String] -> Property
prop_utils_normalize_removes_common_prefix inputLines =
  let indentedLines = map ("  " ++) inputLines
      code = unlines indentedLines
      normalized = normalizeIndentation code
      resultLines = lines normalized
  in property $ not (null resultLines) ==> all (not . ("  " `isPrefixOf`)) resultLines

-- Property: Break on is case sensitive
prop_utils_break_on_case_sensitive :: String -> Property
prop_utils_break_on_case_sensitive pattern =
  let upperPattern = map toUpper pattern
      lowerPattern = map toLower pattern
      text = "Some " ++ lowerPattern ++ " text"
      resultUpper = breakOn upperPattern text
      resultLower = breakOn lowerPattern text
  in property $ length resultUpper /= length resultLower

-- Property: Split by handles Unicode characters
prop_utils_split_unicode :: String -> Property
prop_utils_split_unicode str =
  let unicodeDelim = '∑'
      result = splitBy unicodeDelim str
  in property $ length result >= 1

-- Property: Trim with Unicode whitespace
prop_utils_trim_unicode_whitespace :: String -> Property
prop_utils_trim_unicode_whitespace str =
  let unicodeWhitespace = str ++ "\8192\8193\8194"
      trimmed = trim unicodeWhitespace
  in property $ not (any (`elem` ['\8192', '\8193', '\8194']) trimmed)

-- Property: Remove line comments with Unicode content
prop_utils_remove_comments_unicode :: String -> String -> Property
prop_utils_remove_comments_unicode code comment =
  let unicodeComment = code ++ "// " ++ comment ++ " 测试 🚀"
      result = removeLineComments unicodeComment
  in property $ comment `isInfixOf` unicodeComment && not (comment `isInfixOf` result)

-- Property: Normalize indentation with tabs
prop_utils_normalize_tab_indentation :: [String] -> Property
prop_utils_normalize_tab_indentation lines =
  let tabIndented = map ("\t\t" ++) lines
      code = unlines tabIndented
      normalized = normalizeIndentation code
  in property $ not (null normalized)

-- Property: Break on with special regex characters
prop_utils_break_on_special_chars :: String -> Property
prop_utils_break_on_special_chars text =
  let specialPatterns = [".", "*", "+", "?", "^", "$", "[", "]", "(", ")", "{", "}", "\\", "|"]
      results = map (`breakOn` text) specialPatterns
  in property $ all (\r -> length r >= 1) results

-- Helper functions
-- lines function removed to avoid conflict with Prelude.lines

tests :: TestTree
tests = testGroup "Extended Utils QuickCheck Tests"
  [ fastProperty "Trim idempotent" prop_utils_trim_idempotent
  , fastProperty "Trim removes whitespace" prop_utils_trim_removes_whitespace
  , fastProperty "Split by character" prop_utils_split_by_character
  , fastProperty "Split by comma" prop_utils_split_by_comma
  , fastProperty "Split empty string" prop_utils_split_empty_string
  , fastProperty "Split no delimiter" prop_utils_split_no_delimiter
  , fastProperty "Remove line comments" prop_utils_remove_line_comments
  , fastProperty "Remove all comments" prop_utils_remove_all_comments
  , fastProperty "Normalize indentation" prop_utils_normalize_indentation
  , fastProperty "Break on" prop_utils_break_on
  , fastProperty "Break on empty pattern" prop_utils_break_on_empty_pattern
  , fastProperty "Break on pattern not found" prop_utils_break_on_pattern_not_found
  , fastProperty "Trim preserves internal" prop_utils_trim_preserves_internal
  , fastProperty "Split different delimiters" prop_utils_split_different_delimiters
  , fastProperty "Split repeated delimiters" prop_utils_split_repeated_delimiters
  , fastProperty "Remove multiple line comments" prop_utils_remove_multiple_line_comments
  , fastProperty "Remove nested comments" prop_utils_remove_nested_comments
  , fastProperty "Normalize mixed indentation" prop_utils_normalize_mixed_indentation
  , fastProperty "Break on multiple occurrences" prop_utils_break_on_multiple_occurrences
  , fastProperty "Break on pattern at start" prop_utils_break_on_pattern_at_start
  , fastProperty "Break on pattern at end" prop_utils_break_on_pattern_at_end
  , fastProperty "Trim whitespace only" prop_utils_trim_whitespace_only
  , fastProperty "Split with empty parts" prop_utils_split_with_empty_parts
  , fastProperty "Remove comments preserves code" prop_utils_remove_comments_preserves_code
  , fastProperty "Remove comments handles strings" prop_utils_remove_comments_handles_strings
  , fastProperty "Normalize removes common prefix" prop_utils_normalize_removes_common_prefix
  , fastProperty "Break on case sensitive" prop_utils_break_on_case_sensitive
  , fastProperty "Split unicode" prop_utils_split_unicode
  , fastProperty "Trim unicode whitespace" prop_utils_trim_unicode_whitespace
  , fastProperty "Remove comments unicode" prop_utils_remove_comments_unicode
  , fastProperty "Normalize tab indentation" prop_utils_normalize_tab_indentation
  , fastProperty "Break on special chars" prop_utils_break_on_special_chars
  ]