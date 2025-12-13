{-# LANGUAGE CPP #-}

module Test.Unit.UtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

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
  )

import Data.Char (isSpace)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails)

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_leading_trailing :: String -> String -> Property
prop_trim_removes_leading_trailing prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = any isSpace prefix
      hasTrailing = any isSpace suffix
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     not (any isSpace (take (length prefix) trimmed)) &&
     not (any isSpace (drop (length "content") trimmed))

-- Property: trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal before middle after =
  let content = before ++ middle ++ after
      trimmed = trim content
      expected = filter (not . isSpace) before ++ middle ++ filter (not . isSpace) after
  in not (null middle) ==> 
     filter (not . isSpace) trimmed === filter (not . isSpace) expected

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in not (any null result)

-- Property: splitByComma is splitBy with comma
prop_splitByComma_is_splitBy_comma :: String -> Property
prop_splitByComma_is_splitBy_comma input =
  splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed_is_splitByCollapsed_comma :: String -> Property
prop_splitByCommaCollapsed_is_splitByCollapsed_comma input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes_comments :: String -> String -> Property
prop_removeLineComments_removes_comments prefix comment =
  let content = prefix ++ "// " ++ comment ++ "\nafter comment"
      result = removeLineComments content
  in not ("// " `Data.List.isInfixOf` result) &&
     "after comment" `Data.List.isInfixOf` result

-- Property: removeLineComments preserves comments in strings
prop_removeLineComments_preserves_string_comments :: String -> Property
prop_removeLineComments_preserves_string_comments comment =
  let content = "var s string = \"// not a comment " ++ comment ++ "\"\n// real comment"
      result = removeLineComments content
  in "// not a comment" `Data.List.isInfixOf` result &&
     not ("// real comment" `Data.List.isInfixOf` result)

-- Property: removeComments removes both // and /* */ comments
prop_removeComments_removes_both :: String -> String -> String -> Property
prop_removeComments_removes_both before comment after =
  let content = before ++ "/* block comment */" ++ comment ++ "// line comment\n" ++ after
      result = removeComments content
  in not ("/*" `Data.List.isInfixOf` result) &&
     not ("*/" `Data.List.isInfixOf` result) &&
     not ("// line comment" `Data.List.isInfixOf` result) &&
     after `Data.List.isInfixOf` result

-- Property: removeComments preserves comments in strings
prop_removeComments_preserves_string_comments :: String -> String -> Property
prop_removeComments_preserves_string_comments comment1 comment2 =
  let content = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments content
  in "// not comment1" `Data.List.isInfixOf` result &&
     "/* not comment2 */" `Data.List.isInfixOf` result &&
     not ("// real comment" `Data.List.isInfixOf` result)

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_common :: String -> String -> Property
prop_normalizeIndentation_removes_common prefix content =
  let lines = [prefix ++ "line1", prefix ++ "line2", prefix ++ "line3"]
      result = normalizeIndentation (unlines lines)
  in not (prefix `isPrefixOf` result)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> String -> String -> Property
prop_normalizeIndentation_preserves_relative prefix1 prefix2 content =
  let lines = [prefix1 ++ "line1", prefix1 ++ prefix2 ++ "line2", prefix1 ++ "line3"]
      result = normalizeIndentation (unlines lines)
      resultLines = lines result
  in length resultLines === 3 &&
     length (takeWhile isSpace (resultLines !! 1)) > length (takeWhile isSpace (resultLines !! 0))

-- Property: forceSingleTabIndentation forces tab indentation
prop_forceSingleTabIndentation_forces_tab :: String -> String -> Property
prop_forceSingleTabIndentation_forces_tab prefix content =
  let line = prefix ++ content
      result = forceSingleTabIndentation line
  in not (null content) ==> 
     head result === '\t'

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize input =
  fixIndentation input === normalizeIndentation input

-- Property: breakOn finds substring
prop_breakOn_finds_substring :: String -> String -> Property
prop_breakOn_finds_substring pat haystack =
  not (null pat) && pat `isInfixOf` haystack ==> 
  let (before, after) = breakOn pat haystack
      expectedBefore = takeWhile (not . (`isPrefixOf` pat)) (tails haystack)
  in before ++ pat ++ after === haystack

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in before === "" && after === haystack

-- Property: breakOn handles missing pattern
prop_breakOn_missing_pattern :: String -> String -> Property
prop_breakOn_missing_pattern pat haystack =
  not (null pat) && not (pat `isInfixOf` haystack) ==> 
  let (before, after) = breakOn pat haystack
  in before === haystack && after === ""

-- Property: splitBy and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Property: splitByCollapsed and join roundtrip (for non-collapsed cases)
prop_splitByCollapsed_join_roundtrip :: Char -> String -> Property
prop_splitByCollapsed_join_roundtrip delim input =
  not (any (== delim) input) ==> 
  let parts = splitByCollapsed delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent input =
  let removedOnce = removeLineComments input
      removedTwice = removeLineComments removedOnce
  in removedOnce === removedTwice

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in removedOnce === removedTwice

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalizedOnce = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalizedOnce
  in normalizedOnce === normalizedTwice

-- Property: forceSingleTabIndentation is idempotent
prop_forceSingleTabIndentation_idempotent :: String -> Property
prop_forceSingleTabIndentation_idempotent input =
  let forcedOnce = forceSingleTabIndentation input
      forcedTwice = forceSingleTabIndentation forcedOnce
  in forcedOnce === forcedTwice

-- Property: trim of empty string is empty
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: splitBy empty delimiter splits into characters
prop_splitBy_empty_delim :: String -> Property
prop_splitBy_empty_delim input =
  splitBy '\0' input === map (:[]) input

-- Property: splitByCollapsed empty string is empty
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty delim =
  splitByCollapsed delim "" === []

-- Property: removeLineComments preserves newlines
prop_removeLineComments_preserves_newlines :: String -> String -> Property
prop_removeLineComments_preserves_newlines before after =
  let content = before ++ "// comment\n" ++ after
      result = removeLineComments content
  in '\n' `elem` result

-- Property: removeComments preserves newlines in block comments
prop_removeComments_preserves_block_newlines :: String -> String -> Property
prop_removeComments_preserves_block_newlines before after =
  let content = before ++ "/* comment\nwith newlines */" ++ after
      result = removeComments content
  in '\n' `elem` result

-- Property: normalizeIndentation handles empty lines
prop_normalizeIndentation_handles_empty :: String -> String -> String -> Property
prop_normalizeIndentation_handles_empty before middle after =
  let content = before ++ "\n\n" ++ middle ++ "\n\n" ++ after
      result = normalizeIndentation content
  in "\n\n" `isInfixOf` result

-- Property: forceSingleTabIndentation handles empty lines
prop_forceSingleTabIndentation_handles_empty :: String -> String -> Property
prop_forceSingleTabIndentation_handles_empty before after =
  let content = before ++ "\n\n" ++ after
      result = forceSingleTabIndentation content
      resultLines = lines result
  in all (\line -> null line || head line == '\t') resultLines

-- Property: breakOn with pattern at start
prop_breakOn_pattern_at_start :: String -> String -> Property
prop_breakOn_pattern_at_start pat suffix =
  not (null pat) ==> 
  let haystack = pat ++ suffix
      (before, after) = breakOn pat haystack
  in before === "" && after === suffix

-- Property: breakOn with pattern at end
prop_breakOn_pattern_at_end :: String -> String -> Property
prop_breakOn_pattern_at_end pat prefix =
  not (null pat) ==> 
  let haystack = prefix ++ pat
      (before, after) = breakOn pat haystack
  in before === prefix && after === ""

-- Property: splitBy with consecutive delimiters
prop_splitBy_consecutive_delimiters :: Char -> Int -> String -> Property
prop_splitBy_consecutive_delimiters delim count suffix =
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitBy delim input
  in length parts === count + 2

-- Property: splitByCollapsed with consecutive delimiters
prop_splitByCollapsed_consecutive_delimiters :: Char -> Int -> String -> Property
prop_splitByCollapsed_consecutive_delimiters delim count suffix =
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitByCollapsed delim input
  in length parts === 2

-- Property: removeLineComments with multiple comments
prop_removeLineComments_multiple :: String -> String -> String -> Property
prop_removeLineComments_multiple before middle after =
  let content = before ++ "// comment1\n" ++ middle ++ "// comment2\n" ++ after
      result = removeLineComments content
  in not ("// comment1" `isInfixOf` result) &&
     not ("// comment2" `isInfixOf` result) &&
     middle `isInfixOf` result &&
     after `isInfixOf` result

-- Property: removeComments with nested block comments (should not remove inner)
prop_removeComments_nested_blocks :: String -> String -> String -> Property
prop_removeComments_nested_blocks before middle after =
  let content = before ++ "/* outer /* inner */ comment */" ++ middle ++ after
      result = removeComments content
  in not ("/* outer" `isInfixOf` result) &&
     not ("comment */" `isInfixOf` result) &&
     middle `isInfixOf` result &&
     after `isInfixOf` result

-- Property: normalizeIndentation with mixed tabs and spaces
prop_normalizeIndentation_mixed_whitespace :: String -> String -> String -> Property
prop_normalizeIndentation_mixed_whitespace spaces tabs content =
  let mixedPrefix = spaces ++ "\t" ++ tabs
      lines = [mixedPrefix ++ "line1", mixedPrefix ++ "line2"]
      result = normalizeIndentation (unlines lines)
  in not (any isSpace (take 1 result))

-- Property: forceSingleTabIndentation with already tab-indented content
prop_forceSingleTabIndentation_already_tabbed :: String -> Property
prop_forceSingleTabIndentation_already_tabbed content =
  let tabbed = "\t" ++ content
      result = forceSingleTabIndentation tabbed
  in result === tabbed

-- Property: breakOn with pattern longer than haystack
prop_breakOn_pattern_too_long :: String -> String -> Property
prop_breakOn_pattern_too_long pat haystack =
  length pat > length haystack ==> 
  let (before, after) = breakOn pat haystack
  in before === haystack && after === ""

-- Property: splitBy with Unicode characters
prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim input =
  let unicodeInput = input ++ "测试🚀"
      parts = splitBy delim unicodeInput
  in concat parts `Data.List.isInfixOf` unicodeInput

-- Property: trim with Unicode whitespace
prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace content =
  let unicodeContent = " \t\n\r " ++ content ++ " \t\n\r "
      trimmed = trim unicodeContent
  in not (any isSpace (take 1 trimmed)) &&
     not (any isSpace (reverse (take 1 (reverse trimmed))))

tests :: TestTree
tests = testGroup "Utils QuickCheck tests"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_leading_trailing
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma is splitBy with comma" prop_splitByComma_is_splitBy_comma
  , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitByCommaCollapsed_is_splitByCollapsed_comma
  , fastProperty "removeLineComments removes // comments" prop_removeLineComments_removes_comments
  , fastProperty "removeLineComments preserves comments in strings" prop_removeLineComments_preserves_string_comments
  , fastProperty "removeComments removes both // and /* */ comments" prop_removeComments_removes_both
  , fastProperty "removeComments preserves comments in strings" prop_removeComments_preserves_string_comments
  , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "forceSingleTabIndentation forces tab indentation" prop_forceSingleTabIndentation_forces_tab
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "breakOn finds substring" prop_breakOn_finds_substring
  , fastProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
  , fastProperty "breakOn handles missing pattern" prop_breakOn_missing_pattern
  , fastProperty "splitBy and join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "splitByCollapsed and join roundtrip" prop_splitByCollapsed_join_roundtrip
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
  , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
  , fastProperty "forceSingleTabIndentation is idempotent" prop_forceSingleTabIndentation_idempotent
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "splitBy empty delimiter splits into characters" prop_splitBy_empty_delim
  , fastProperty "splitByCollapsed empty string is empty" prop_splitByCollapsed_empty
  , fastProperty "removeLineComments preserves newlines" prop_removeLineComments_preserves_newlines
  , fastProperty "removeComments preserves newlines in block comments" prop_removeComments_preserves_block_newlines
  , fastProperty "normalizeIndentation handles empty lines" prop_normalizeIndentation_handles_empty
  , fastProperty "forceSingleTabIndentation handles empty lines" prop_forceSingleTabIndentation_handles_empty
  , fastProperty "breakOn with pattern at start" prop_breakOn_pattern_at_start
  , fastProperty "breakOn with pattern at end" prop_breakOn_pattern_at_end
  , fastProperty "splitBy with consecutive delimiters" prop_splitBy_consecutive_delimiters
  , fastProperty "splitByCollapsed with consecutive delimiters" prop_splitByCollapsed_consecutive_delimiters
  , fastProperty "removeLineComments with multiple comments" prop_removeLineComments_multiple
  , fastProperty "removeComments with nested block comments" prop_removeComments_nested_blocks
  , fastProperty "normalizeIndentation with mixed tabs and spaces" prop_normalizeIndentation_mixed_whitespace
  , fastProperty "forceSingleTabIndentation with already tab-indented content" prop_forceSingleTabIndentation_already_tabbed
  , fastProperty "breakOn with pattern longer than haystack" prop_breakOn_pattern_too_long
  , fastProperty "splitBy with Unicode characters" prop_splitBy_unicode
  , fastProperty "trim with Unicode whitespace" prop_trim_unicode_whitespace
  ]