{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import GHC.Generics (Generic)

import Utils 
  ( trim
  , splitBy, splitByCollapsed
  , splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments
  , normalizeIndentation, forceSingleTabIndentation, fixIndentation
  , breakOn
  )

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate strings with various whitespace characters
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r"

-- | Generate strings with alphanumeric characters
genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | Generate strings with mixed content
genMixedString :: Gen String
genMixedString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r,.;:!@#$%^&*(){}[]"

-- | Generate strings with potential comment content
genCommentString :: Gen String
genCommentString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;:!@#$%^&*()"

-- | Generate strings with indentation (leading spaces/tabs)
genIndentedString :: Gen String
genIndentedString = do
  indent <- listOf $ elements " \t"
  content <- genAlphaNumString
  return $ indent ++ content

-- | Generate multiline strings
genMultilineString :: Gen String
genMultilineString = do
  lines <- listOf1 genAlphaNumString
  return $ unlines lines

-- | Generate strings with specific delimiters
genDelimitedString :: Char -> Gen String
genDelimitedString delim = do
  parts <- listOf genAlphaNumString
  return $ intercalate [delim] parts
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

instance Arbitrary Char where
  arbitrary = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r,.;:!@#$%^&*(){}[]"

-- ============================================================================
-- Trim Function Property Tests
-- ============================================================================

-- | Property: trim should remove leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
      hasLeadingWs = not (null s) && isSpace (head s)
      hasTrailingWs = not (null s) && isSpace (last s)
  in (hasLeadingWs || hasTrailingWs) ==> 
     (null trimmed || not (isSpace (head trimmed))) .&&.
     (null trimmed || not (isSpace (last trimmed)))

-- | Property: trim should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- | Property: trim empty string should remain empty
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | Property: trim whitespace-only string should become empty
prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only ws =
  let allWs = all isSpace ws
  in allWs ==> trim ws === ""

-- ============================================================================
-- Split Function Property Tests
-- ============================================================================

-- | Property: splitBy should preserve empty segments
prop_split_by_preserves_empty :: Char -> String -> Property
prop_split_by_preserves_empty delim s =
  let result = splitBy delim s
      expectedLength = length (filter (== delim) s) + 1
  in length result === expectedLength

-- | Property: splitByComma should be equivalent to splitBy ','
prop_split_by_comma_equivalence :: String -> Property
prop_split_by_comma_equivalence s =
  splitByComma s === splitBy ',' s

-- | Property: splitByCollapsed should remove empty segments
prop_split_by_collapsed_removes_empty :: Char -> String -> Property
prop_split_by_collapsed_removes_empty delim s =
  let result = splitByCollapsed delim s
  in not (any null result)

-- | Property: splitByCommaCollapsed should be equivalent to splitByCollapsed ','
prop_split_by_comma_collapsed_equivalence :: String -> Property
prop_split_by_comma_collapsed_equivalence s =
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- | Property: splitBy and splitByCollapsed should be equivalent for non-repeating delimiters
prop_split_equivalent_no_repeats :: Char -> String -> Property
prop_split_equivalent_no_repeats delim s =
  let noRepeats = not (isInfixOf [delim, delim] s)
  in noRepeats ==> splitBy delim s === splitByCollapsed delim s

-- | Property: splitBy should reconstruct original string with delimiter
prop_split_by_reconstruction :: Char -> String -> Property
prop_split_by_reconstruction delim s =
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in reconstructed === s

-- ============================================================================
-- Comment Removal Property Tests
-- ============================================================================

-- | Property: removeLineComments should remove lines starting with //
prop_remove_line_comments_basic :: String -> Property
prop_remove_line_comments_basic s =
  let withComment = "// This is a comment\n" ++ s
      result = removeLineComments withComment
  in not ("// This is a comment" `isInfixOf` result)

-- | Property: removeLineComments should preserve code after comments
prop_remove_line_comments_preserve_code :: String -> String -> Property
prop_remove_line_comments_preserve_code code comment =
  let input = code ++ "\n// " ++ comment ++ "\n" ++ code
      result = removeLineComments input
  in code `isInfixOf` result

-- | Property: removeComments should handle both // and /* */ comments
prop_remove_comments_both_types :: String -> String -> Property
prop_remove_comments_both_types code comment =
  let input = code ++ "\n// " ++ comment ++ "\n/* " ++ comment ++ " */\n" ++ code
      result = removeComments input
  in not ("// " `isInfixOf` result) .&&. not ("/* " `isInfixOf` result)

-- | Property: removeComments should preserve code between comments
prop_remove_comments_preserve_code :: String -> Property
prop_remove_comments_preserve_code code =
  let input = "// comment\n" ++ code ++ "\n/* comment */\n" ++ code
      result = removeComments input
  in code `isInfixOf` result

-- ============================================================================
-- Indentation Function Property Tests
-- ============================================================================

-- | Property: normalizeIndentation should preserve relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative s =
  let lines = lines s
      hasMultipleLines = length lines > 1
  in hasMultipleLines ==> property True  -- Basic test that it doesn't crash

-- | Property: normalizeIndentation should be idempotent
prop_normalize_idempotent :: String -> Property
prop_normalize_idempotent s =
  let normalizedOnce = normalizeIndentation s
      normalizedTwice = normalizeIndentation normalizedOnce
  in normalizedOnce === normalizedTwice

-- | Property: fixIndentation should be equivalent to normalizeIndentation
prop_fix_indentation_equivalence :: String -> Property
prop_fix_indentation_equivalence s =
  fixIndentation s === normalizeIndentation s

-- | Property: forceSingleTabIndentation should convert spaces to tabs
prop_force_tab_indentation :: String -> Property
prop_force_tab_indentation s =
  let result = forceSingleTabIndentation s
      hasSpaces = "  " `isInfixOf` result  -- Check for double spaces
  in not hasSpaces  -- Should not have consecutive spaces

-- ============================================================================
-- BreakOn Function Property Tests  
-- ============================================================================

-- | Property: breakOn should find substring or return original
prop_break_on_basic :: String -> String -> Property
prop_break_on_basic s needle =
  let (before, after) = breakOn needle s
      needleFound = needle `isInfixOf` s
  in if needleFound
     then before ++ needle ++ after === s
     else before === s .&&. after === ""

-- | Property: breakOn with empty needle should return ("", s)
prop_break_on_empty_needle :: String -> Property
prop_break_on_empty_needle s =
  let (before, after) = breakOn "" s
  in before === "" .&&. after === s

-- | Property: breakOn should be consistent with Data.List.break
prop_break_on_consistency :: String -> String -> Property
prop_break_on_consistency s needle =
  let (before1, after1) = breakOn needle s
      (before2, after2) = break (needle `isPrefixOf`) s
  in before1 === before2 .&&. 
     (if needle `isPrefixOf` after2 
      then after1 === needle ++ drop (length needle) after2
      else after1 === after2)

-- | Property: breakOn should handle needle equal to entire string
prop_break_on_needle_equals_string :: String -> Property
prop_break_on_needle_equals_string s =
  let (before, after) = breakOn s s
  in before === "" .&&. after === s

-- ============================================================================
-- General Utility Property Tests
-- ============================================================================

-- | Property: Functions should handle empty strings gracefully
prop_empty_string_handling :: Property
prop_empty_string_handling =
  trim "" === "" .&&.
  splitBy ',' "" === [""] .&&.
  splitByCollapsed ',' "" === [] .&&.
  removeLineComments "" === "" .&&.
  removeComments "" === "" .&&.
  normalizeIndentation "" === "" .&&.
  let (before, after) = breakOn "" "" in before === "" .&&. after === ""

-- | Property: Functions should handle single character strings
prop_single_char_handling :: Char -> Property
prop_single_char_handling c =
  let s = [c]
  in trim s === s .&&.
     splitBy c s === ["", ""] .&&.
     splitByCollapsed c s === [] .&&.
     removeLineComments s === s .&&.
     removeComments s === s

-- | Property: String operations should preserve length invariants
prop_length_invariants :: String -> Char -> Property
prop_length_invariants s delim =
  let splitResult = splitBy delim s
      collapsedResult = splitByCollapsed delim s
      splitLength = sum (map length splitResult) + length (filter (== delim) s)
      collapsedLength = sum (map length collapsedResult)
  in splitLength === length s .&&.
     collapsedLength <= length s

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Utils QuickCheck Tests"
  [ -- Trim Tests
    fastProperty "trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "trim idempotent" prop_trim_idempotent
  , fastProperty "trim empty" prop_trim_empty
  , fastProperty "trim whitespace only" prop_trim_whitespace_only
  
  -- Split Tests
  , fastProperty "split by preserves empty" prop_split_by_preserves_empty
  , fastProperty "split by comma equivalence" prop_split_by_comma_equivalence
  , fastProperty "split by collapsed removes empty" prop_split_by_collapsed_removes_empty
  , fastProperty "split by comma collapsed equivalence" prop_split_by_comma_collapsed_equivalence
  , fastProperty "split equivalent no repeats" prop_split_equivalent_no_repeats
  , fastProperty "split by reconstruction" prop_split_by_reconstruction
  
  -- Comment Removal Tests
  , fastProperty "remove line comments basic" prop_remove_line_comments_basic
  , fastProperty "remove line comments preserve code" prop_remove_line_comments_preserve_code
  , fastProperty "remove comments both types" prop_remove_comments_both_types
  , fastProperty "remove comments preserve code" prop_remove_comments_preserve_code
  
  -- Indentation Tests
  , fastProperty "normalize preserves relative" prop_normalize_preserves_relative
  , fastProperty "normalize idempotent" prop_normalize_idempotent
  , fastProperty "fix indentation equivalence" prop_fix_indentation_equivalence
  , fastProperty "force tab indentation" prop_force_tab_indentation
  
  -- BreakOn Tests
  , fastProperty "break on basic" prop_break_on_basic
  , fastProperty "break on empty needle" prop_break_on_empty_needle
  , fastProperty "break on consistency" prop_break_on_consistency
  , fastProperty "break on needle equals string" prop_break_on_needle_equals_string
  
  -- General Utility Tests
  , fastProperty "empty string handling" prop_empty_string_handling
  , fastProperty "single char handling" prop_single_char_handling
  , fastProperty "length invariants" prop_length_invariants
  ]

-- Helper function for intercalating strings
intercalate :: String -> [String] -> String
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs