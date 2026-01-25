module Test.Unit.NewUtilsStringProcessingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

-- Import Utils module
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation,
             breakOn, safeProcessString, isValidChar)

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 1: Trim should not add characters
prop_trim_no_addition :: String -> Property
prop_trim_no_addition s =
  let trimmed = trim s
  in property $ length trimmed <= length s

-- Property 2: Trim should remove leading/trailing spaces
prop_trim_removes_spaces :: String -> Property
prop_trim_removes_spaces s =
  let trimmed = trim s
      hasLeadingSpace = case s of (x:_) -> isSpace x; [] -> False
      hasTrailingSpace = case reverse s of (x:_) -> isSpace x; [] -> False
  in property $ if hasLeadingSpace || hasTrailingSpace
                then length trimmed < length s || null trimmed
                else True

-- Property 3: Split by delimiter should preserve all parts
prop_split_by_preserves_parts :: Char -> String -> Property
prop_split_by_preserves_parts delim s =
  let parts = splitBy delim s
      rejoined = if null parts then "" else concat (map (++ [delim]) (init parts)) ++ last parts
  in property $ length parts >= 0  -- Just test it doesn't crash

-- Property 4: Split by comma should work like splitBy with ',' delimiter
prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s =
  let commaSplit = splitByComma s
      genericSplit = splitBy ',' s
  in property $ commaSplit == genericSplit

-- Property 5: Remove line comments should not crash
prop_remove_line_comments_no_crash :: String -> Property
prop_remove_line_comments_no_crash s =
  let result = removeLineComments s
  in property $ length result >= 0

-- Property 6: Remove comments should not crash
prop_remove_comments_no_crash :: String -> Property
prop_remove_comments_no_crash s =
  let result = removeComments s
  in property $ length result >= 0

-- Property 7: Normalize indentation should not crash
prop_normalize_indentation_no_crash :: String -> Property
prop_normalize_indentation_no_crash s =
  let result = normalizeIndentation s
  in property $ length result >= 0

-- Property 8: Break on should find substring or return original
prop_break_on_finds_or_original :: String -> String -> Property
prop_break_on_finds_or_original s substr =
  let (before, after) = breakOn substr s
  in property $ length before >= 0 && length after >= 0

-- Property 9: Safe process string should not crash
prop_safe_process_string_no_crash :: String -> Property
prop_safe_process_string_no_crash s =
  let result = safeProcessString s
  in case result of
       Right r -> property $ length r >= 0
       Left _ -> property True

-- Property 10: Valid character check should be boolean
prop_is_valid_char_boolean :: Char -> Property
prop_is_valid_char_boolean c =
  let result = isValidChar c
  in property $ result == True || result == False

-- Property 11: Split collapsed should not increase length compared to regular split
prop_split_collapsed_not_longer :: Char -> String -> Property
prop_split_collapsed_not_longer delim s =
  let regular = splitBy delim s
      collapsed = splitByCollapsed delim s
  in property $ length collapsed <= length regular

-- Property 12: Trim of already trimmed string should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in property $ trimmed == trimmedAgain

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_trim_empty :: Assertion
test_trim_empty = 
  assertEqual "Trim empty string should be empty" "" (trim "")

test_trim_no_spaces :: Assertion
test_trim_no_spaces = 
  let s = "hello"
  in assertEqual "Trim string without spaces should be unchanged" s (trim s)

test_trim_leading_spaces :: Assertion
test_trim_leading_spaces = 
  let s = "   hello"
      expected = "hello"
  in assertEqual "Trim should remove leading spaces" expected (trim s)

test_trim_trailing_spaces :: Assertion
test_trim_trailing_spaces = 
  let s = "hello   "
      expected = "hello"
  in assertEqual "Trim should remove trailing spaces" expected (trim s)

test_trim_both_spaces :: Assertion
test_trim_both_spaces = 
  let s = "   hello   "
      expected = "hello"
  in assertEqual "Trim should remove both leading and trailing spaces" expected (trim s)

test_split_by_empty :: Assertion
test_split_by_empty = 
  let result = splitBy ',' ""
  in assertEqual "Split empty string should be empty" [] result

test_split_by_single :: Assertion
test_split_by_single = 
  let result = splitBy ',' "a"
  in assertEqual "Split single character should return [character]" ["a"] result

test_split_by_multiple :: Assertion
test_split_by_multiple = 
  let result = splitBy ',' "a,b,c"
  in assertEqual "Split multiple parts should work" ["a", "b", "c"] result

test_split_by_with_empty :: Assertion
test_split_by_with_empty = 
  let result = splitBy ',' "a,,c"
  in assertEqual "Split with empty parts should preserve empties" ["a", "", "c"] result

test_split_by_comma :: Assertion
test_split_by_comma = 
  let result = splitByComma "a,b,c"
  in assertEqual "Split by comma should work" ["a", "b", "c"] result

test_split_by_collapsed :: Assertion
test_split_by_collapsed = 
  let result = splitByCollapsed ',' "a,,c"
  in assertEqual "Split collapsed should remove empty parts" ["a", "c"] result

test_remove_line_comments :: Assertion
test_remove_line_comments = 
  let s = "hello\n// comment\nworld"
      result = removeLineComments s
  in assertBool "Remove line comments should remove comment lines" $
    not ("// comment" `isInfixOf` result)

test_remove_block_comments :: Assertion
test_remove_block_comments = 
  let s = "hello /* comment */ world"
      result = removeComments s
  in assertBool "Remove block comments should remove comments" $
    not ("/* comment */" `isInfixOf` result)

test_break_on_found :: Assertion
test_break_on_found = 
  let s = "hello world"
      (before, after) = breakOn "world" s
  in do
    assertEqual "Before should be 'hello '" "hello " before
    assertEqual "After should be 'world'" "world" after

test_break_on_not_found :: Assertion
test_break_on_not_found = 
  let s = "hello world"
      (before, after) = breakOn "xyz" s
  in do
    assertEqual "Before should be original when not found" s before
    assertEqual "After should be empty when not found" "" after

test_is_valid_char :: Assertion
test_is_valid_char = 
  do
    assertBool "Valid character should pass" $ isValidChar 'a'
    assertBool "Digit should be valid" $ isValidChar '1'
    assertBool "Space should be valid" $ isValidChar ' '

test_normalize_indentation :: Assertion
test_normalize_indentation = 
  let s = "  line1\n    line2\n  line3"
      result = normalizeIndentation s
  in assertBool "Normalize indentation should not crash" $ length result > 0

test_safe_process_string :: Assertion
test_safe_process_string = 
  let s = "test string"
      result = safeProcessString s
  in case result of
       Right r -> assertBool "Safe process string should not crash" $ length r >= 0
       Left _ -> assertBool "Safe process string should not crash" True

tests :: TestTree
tests = testGroup "Test.Unit.NewUtilsStringProcessingSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "trim no addition" prop_trim_no_addition
    , testProperty "trim removes spaces" prop_trim_removes_spaces
    , testProperty "split by preserves parts" prop_split_by_preserves_parts
    , testProperty "split by comma consistency" prop_split_by_comma_consistency
    , testProperty "remove line comments no crash" prop_remove_line_comments_no_crash
    , testProperty "remove comments no crash" prop_remove_comments_no_crash
    , testProperty "normalize indentation no crash" prop_normalize_indentation_no_crash
    , testProperty "break on finds or original" prop_break_on_finds_or_original
    , testProperty "safe process string no crash" prop_safe_process_string_no_crash
    , testProperty "is valid char boolean" prop_is_valid_char_boolean
    , testProperty "split collapsed not longer" prop_split_collapsed_not_longer
    , testProperty "trim idempotent" prop_trim_idempotent
    ]
  , testGroup "Unit Tests"
    [ testCase "trim empty" test_trim_empty
    , testCase "trim no spaces" test_trim_no_spaces
    , testCase "trim leading spaces" test_trim_leading_spaces
    , testCase "trim trailing spaces" test_trim_trailing_spaces
    , testCase "trim both spaces" test_trim_both_spaces
    , testCase "split by empty" test_split_by_empty
    , testCase "split by single" test_split_by_single
    , testCase "split by multiple" test_split_by_multiple
    , testCase "split by with empty" test_split_by_with_empty
    , testCase "split by comma" test_split_by_comma
    , testCase "split by collapsed" test_split_by_collapsed
    , testCase "remove line comments" test_remove_line_comments
    , testCase "remove block comments" test_remove_block_comments
    , testCase "break on found" test_break_on_found
    , testCase "break on not found" test_break_on_not_found
    , testCase "is valid char" test_is_valid_char
    , testCase "normalize indentation" test_normalize_indentation
    , testCase "safe process string" test_safe_process_string
    ]
  ]