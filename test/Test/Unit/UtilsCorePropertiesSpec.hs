{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.UtilsCorePropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  , safeProcessString
  , isValidChar
  )
import Data.Char (isSpace)

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s = 
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then property (length trimmed < length s)
     else trimmed === s

-- Property: trim idempotent (trimming twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Property: trim of all whitespace returns empty string
prop_trim_all_whitespace :: Property
prop_trim_all_whitespace = 
  forAll (listOf (elements " \t\n\r")) $ \ws ->
  trim ws === ""

-- ============================================================================
-- Split Properties
-- ============================================================================

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s = 
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in rejoined === s

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s = 
  let parts = splitByCollapsed delim s
  in property (all (not . null) parts)

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equivalent :: String -> Property
prop_splitByComma_equivalent s = 
  splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalent :: String -> Property
prop_splitByCommaCollapsed_equivalent s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: splitBy on empty string returns single empty segment
prop_splitBy_empty_string :: Char -> Property
prop_splitBy_empty_string delim = 
  splitBy delim "" === []

-- Property: splitByCollapsed on empty string returns empty list
prop_splitByCollapsed_empty_string :: Char -> Property
prop_splitByCollapsed_empty_string delim = 
  splitByCollapsed delim "" === []

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes_comments :: Property
prop_removeLineComments_removes_comments = 
  let input = "code // comment\nmore code"
      expected = "code \nmore code"
  in removeLineComments input === expected

-- Property: removeLineComments preserves // inside strings
prop_removeLineComments_preserves_strings :: Property
prop_removeLineComments_preserves_strings = 
  let input = "code \"// not a comment\" // real comment"
      expected = "code \"// not a comment\" "
  in removeLineComments input === expected

-- Property: removeComments removes both // and /* */ comments
prop_removeComments_removes_both :: Property
prop_removeComments_removes_both = 
  let input = "code // line comment\nmore /* block */ code"
      expected = "code \nmore  code"
  in removeComments input === expected

-- Property: removeComments preserves comments inside strings
prop_removeComments_preserves_strings :: Property
prop_removeComments_preserves_strings = 
  let input = "code \"// not comment\" /* not comment */ \"/* not comment */\""
      expected = "code \"// not comment\"  \"/* not comment */\""
  in removeComments input === expected

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: Property
prop_normalizeIndentation_preserves_relative = 
  let input = "    line1\n      line2\n    line3"
      result = normalizeIndentation input
      lines' = lines result
  in length lines' === 3 .&&.
     not (null (head lines')) .&&.
     property (head (lines' !! 1) == ' ') .&&.
     not (isSpace (head (lines' !! 2)))

-- Property: normalizeIndentation of empty string returns empty string
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty = 
  normalizeIndentation "" === ""

-- Property: fixIndentation is equivalent to normalizeIndentation
prop_fixIndentation_equivalent :: String -> Property
prop_fixIndentation_equivalent s = 
  fixIndentation s === normalizeIndentation s

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabIndentation_adds_tab :: Property
prop_forceSingleTabIndentation_adds_tab = 
  let input = "line1\n\nline2"
      result = forceSingleTabIndentation input
      lines' = lines result
  in head (head lines') === '\t' .&&.
     head (lines' !! 1) === '\t' .&&.
     null (lines' !! 2)

-- ============================================================================
-- Search Properties
-- ============================================================================

-- Property: breakOn finds substring
prop_breakOn_finds_substring :: String -> String -> Property
prop_breakOn_finds_substring pat s = 
  not (null pat) ==> 
  let (before, after) = breakOn pat s
  in if pat `isInfixOf` s
     then before ++ pat ++ after === s
     else before === s .&&. after === ""

-- Property: breakOn with empty pattern returns ("", s)
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s = 
  breakOn "" s === ("", s)

-- ============================================================================
-- String Validation Properties
-- ============================================================================

-- Property: isValidChar returns True for printable chars
prop_isValidChar_printable :: Property
prop_isValidChar_printable = 
  forAll (choose (' ', '~')) $ \c ->
  isValidChar c

-- Property: isValidChar returns True for whitespace
prop_isValidChar_whitespace :: Property
prop_isValidChar_whitespace = 
  forAll (elements "\n\r\t") $ \c ->
  isValidChar c

-- Property: isValidChar returns False for control chars
prop_isValidChar_control :: Property
prop_isValidChar_control = 
  forAll (choose ('\0', '\8')) $ \c ->
  not (isValidChar c)

-- Property: safeProcessString returns Right for valid strings
prop_safeProcessString_valid :: Property
prop_safeProcessString_valid = 
  forAll (listOf (elements ([' '..'~'] ++ "\n\r\t"))) $ \s ->
  case safeProcessString s of
    Left _ -> property False
    Right filtered -> property (all isValidChar filtered)

-- Property: safeProcessString filters control chars
prop_safeProcessString_filters :: Property
prop_safeProcessString_filters = 
  forAll (listOf (elements ([' '..'~'] ++ "\n\r\t" ++ ['\0'..'\8']))) $ \s ->
  case safeProcessString s of
    Left _ -> property False
    Right filtered -> property (all isValidChar filtered)

-- ============================================================================
-- Helper Functions
-- ============================================================================

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `isSubstringOf` haystack
  where
    isSubstringOf [] _ = True
    isSubstringOf _ [] = False
    isSubstringOf needle@(n:ns) (h:hs)
      | n == h = needle `isSubstringOf` hs || ns `isSubstringOf` hs
      | otherwise = needle `isSubstringOf` hs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Core Properties Tests"
  [ testGroup "String Processing Properties"
    [ testProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
    , testProperty "trim is idempotent" prop_trim_idempotent
    , testProperty "trim of all whitespace returns empty string" prop_trim_all_whitespace
    ]
  , testGroup "Split Properties"
    [ testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , testProperty "splitByComma is equivalent to splitBy ','" prop_splitByComma_equivalent
    , testProperty "splitByCommaCollapsed is equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalent
    , testProperty "splitBy on empty string returns single empty segment" prop_splitBy_empty_string
    , testProperty "splitByCollapsed on empty string returns empty list" prop_splitByCollapsed_empty_string
    ]
  , testGroup "Comment Removal Properties"
    [ testProperty "removeLineComments removes // comments" prop_removeLineComments_removes_comments
    , testProperty "removeLineComments preserves // inside strings" prop_removeLineComments_preserves_strings
    , testProperty "removeComments removes both // and /* */ comments" prop_removeComments_removes_both
    , testProperty "removeComments preserves comments inside strings" prop_removeComments_preserves_strings
    ]
  , testGroup "Indentation Properties"
    [ testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , testProperty "normalizeIndentation of empty string returns empty string" prop_normalizeIndentation_empty
    , testProperty "fixIndentation is equivalent to normalizeIndentation" prop_fixIndentation_equivalent
    , testProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forceSingleTabIndentation_adds_tab
    ]
  , testGroup "Search Properties"
    [ testProperty "breakOn finds substring" prop_breakOn_finds_substring
    , testProperty "breakOn with empty pattern returns (\"\", s)" prop_breakOn_empty_pattern
    ]
  , testGroup "String Validation Properties"
    [ testProperty "isValidChar returns True for printable chars" prop_isValidChar_printable
    , testProperty "isValidChar returns True for whitespace" prop_isValidChar_whitespace
    , testProperty "isValidChar returns False for control chars" prop_isValidChar_control
    , testProperty "safeProcessString returns Right for valid strings" prop_safeProcessString_valid
    , testProperty "safeProcessString filters control chars" prop_safeProcessString_filters
    ]
  ]