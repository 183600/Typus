module Test.Unit.UtilsQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.Char (isSpace)
import qualified Data.Text as T

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim should not add characters
prop_trim_no_addition :: String -> Property
prop_trim_no_addition s = length (trim s) <= length s

-- Property: trim should remove leading and trailing spaces
prop_trim_removes_spaces :: String -> Property
prop_trim_removes_spaces s =
  let trimmed = trim s
  in property $ 
    if null trimmed 
    then True  -- All spaces were removed
    else not (isSpace (head trimmed)) && not (isSpace (last trimmed))

-- Property: splitBy should preserve total length when concatenated with delimiters
prop_split_by_preserves_content :: Char -> String -> Property
prop_split_by_preserves_content delim s = 
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in property $ reconstructed == s

-- Property: splitByComma should be equivalent to splitBy ','
prop_split_by_comma_equivalence :: String -> Property
prop_split_by_comma_equivalence s = 
  property $ splitByComma s == splitBy ',' s

-- Property: splitByCollapsed should remove empty segments
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- Property: splitByCommaCollapsed should be equivalent to splitByCollapsed ','
prop_split_by_comma_collapsed_equivalence :: String -> Property
prop_split_by_comma_collapsed_equivalence s = 
  property $ splitByCommaCollapsed s == splitByCollapsed ',' s

-- Property: removeLineComments should remove content after //
prop_remove_line_comments_property :: String -> Property
prop_remove_line_comments_property s = 
  let withComment = s ++ "// this is a comment\nmore code"
      processed = removeLineComments withComment
      lines' = lines processed
  in property $ 
    case lines' of
      [] -> True
      [firstLine] -> "// this is a comment" `notElem` words firstLine
      (firstLine:rest) -> "// this is a comment" `notElem` words firstLine && 
                          "more" `elem` words (head rest)

-- Property: removeComments should handle both line and block comments
prop_remove_comments_property :: String -> Property
prop_remove_comments_property s = 
  let withComments = s ++ "// line comment\nint x = 0; /* block comment */ int y = 1;"
      processed = removeComments withComments
  in property $ 
    "line comment" `notElem` words processed &&
    "block comment" `notElem` words processed &&
    "int x = 0;" `elem` words processed &&
    "int y = 1;" `elem` words processed

-- Property: normalizeIndentation should preserve relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s = 
  let indented = "  " ++ s ++ "\n    " ++ s ++ "  \n  " ++ s
      normalized = normalizeIndentation indented
      lines' = lines normalized
  in property $ 
    case lines' of
      [] -> True
      [line] -> True
      (firstLine:secondLine:rest) -> 
        length (takeWhile isSpace firstLine) < length (takeWhile isSpace secondLine)

-- Property: breakOn should find substring or return original
prop_break_on_property :: String -> String -> Property
prop_break_on_property pat s = 
  let (before, after) = breakOn pat s
      reconstructed = before ++ pat ++ after
  in property $ 
    if pat `elem` (substrings s)
    then reconstructed == s
    else before == s && after == ""

-- Property: safeProcessString should filter control characters
prop_safe_process_string_property :: String -> Property
prop_safe_process_string_property s = 
  let result = safeProcessString s
  in property $ 
    case result of
      Left _ -> True  -- Empty string case
      Right filtered -> all isValidChar filtered

-- Property: safeProcessString should preserve valid characters
prop_safe_process_string_preserves_valid :: String -> Property
prop_safe_process_string_preserves_valid s = 
  let validOnly = filter isValidChar s
      result = safeProcessString s
  in property $ 
    case result of
      Left _ -> null validOnly
      Right filtered -> filtered == validOnly

-- Helper functions
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

substrings :: String -> [String]
substrings [] = [""]
substrings s = [take i s | i <- [1..length s]] ++ substrings (tail s)

tests :: TestTree
tests = testGroup "Utils QuickCheck Properties Tests"
  [ testProperty "trim no addition" prop_trim_no_addition
  , testProperty "trim removes spaces" prop_trim_removes_spaces
  , testProperty "splitBy preserves content" prop_split_by_preserves_content
  , testProperty "splitByComma equivalence" prop_split_by_comma_equivalence
  , testProperty "splitByCollapsed no empty" prop_split_by_collapsed_no_empty
  , testProperty "splitByCommaCollapsed equivalence" prop_split_by_comma_collapsed_equivalence
  , testProperty "removeLineComments property" prop_remove_line_comments_property
  , testProperty "removeComments property" prop_remove_comments_property
  , testProperty "normalizeIndentation preserves relative" prop_normalize_indentation_preserves_relative
  , testProperty "breakOn property" prop_break_on_property
  , testProperty "safeProcessString property" prop_safe_process_string_property
  , testProperty "safeProcessString preserves valid" prop_safe_process_string_preserves_valid
  ]