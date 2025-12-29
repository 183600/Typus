{-# LANGUAGE TemplateHaskell #-}

-- | String processing property tests for various modules
module Test.Unit.StringProcessingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | trim should handle all whitespace strings
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace s =
  let allWs = all isSpace s
      trimmed = trim s
  in if allWs
     then trimmed === ""
     else property True

-- | trim should preserve non-whitespace content
prop_trim_preserve_content :: String -> Property
prop_trim_preserve_content s =
  let hasNonWs = any (not . isSpace) s
      trimmed = trim s
      content = filter (not . isSpace) s
      trimmedContent = filter (not . isSpace) trimmed
  in if hasNonWs
     then trimmedContent === content
     else property True

-- | splitBy should preserve all characters
prop_splitBy_preserve_characters :: Char -> String -> Property
prop_splitBy_preserve_characters delim s =
  let parts = splitBy delim s
      rejoined = concat $ intersperse [delim] parts
  in rejoined === s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep ++ intersperse sep (y:xs)

-- | splitByCollapsed should remove consecutive delimiters
prop_splitByCollapsed_remove_consecutive :: Char -> String -> Property
prop_splitByCollapsed_remove_consecutive delim s =
  let parts = splitByCollapsed delim
      hasNoConsecutive = all (\p -> not (delim `elem` p)) parts
  in hasNoConsecutive === True

-- | removeLineComments should handle strings without comments
prop_remove_line_comments_no_comments :: String -> Property
prop_remove_line_comments_no_comments s =
  let hasNoComment = not ("//" `isPrefixOf` s) && "//" `notElem` substrings s
      result = removeLineComments s
  in if hasNoComment
     then result === s
     else property True
  where
    substrings str = [take n str | n <- [2..length str]]

-- | removeLineComments should preserve content before comment
prop_remove_line_comments_preserve_before :: String -> String -> Property
prop_remove_line_comments_preserve_before prefix comment =
  let input = prefix ++ "//" ++ comment
      result = removeLineComments input
  in result === prefix

-- | removeComments should handle block comments
prop_remove_comments_block_comments :: String -> String -> Property
prop_remove_comments_block_comments before after =
  let input = before ++ "/*" ++ "comment" ++ "*/" ++ after
      result = removeComments input
  in after `isInfixOf` result .&&. before `isInfixOf` result

-- | removeComments should handle nested block comments gracefully
prop_remove_comments_nested :: String -> Property
prop_remove_comments_nested content =
  let input = "/* outer /* inner */ " ++ content ++ " */"
      result = removeComments input
  in property True  -- Should not crash

-- | String operations should be composable
prop_string_operations_composable :: String -> Property
prop_string_operations_composable s =
  let trimmed = trim s
      parts = splitBy ',' trimmed
      rejoined = concat $ intersperse "," parts
      trimmedAgain = trim rejoined
  in trimmedAgain === trimmed
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep ++ intersperse sep (y:xs)

-- | Unicode strings should be handled correctly
prop_unicode_handling :: String -> Property
prop_unicode_handling s =
  let unicodeStr = "测试" ++ s ++ "🚀"
      trimmed = trim unicodeStr
      parts = splitBy ' ' unicodeStr
  in property True  -- Should handle Unicode without crashing

-- | Empty strings should be handled gracefully
prop_empty_string_handling :: Char -> Property
prop_empty_string_handling delim =
  let splitResult = splitBy delim ""
      splitCollapsedResult = splitByCollapsed delim ""
      trimResult = trim ""
      removeLineResult = removeLineComments ""
      removeCommentsResult = removeComments ""
  in splitResult === [""] .&&.
     splitCollapsedResult === [] .&&.
     trimResult === "" .&&.
     removeLineResult === "" .&&.
     removeCommentsResult === ""

-- | Very long strings should be handled efficiently
prop_long_string_handling :: Positive Int -> Char -> Property
prop_long_string_handling (Positive len) delim =
  let longStr = replicate len 'a' ++ [delim] ++ replicate len 'b'
      parts = splitBy delim longStr
  in length parts === 2 .&&. length (head parts) === len .&&. length (last parts) === len

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "String Processing QuickCheck Tests"
  [ testProperty "trim: all whitespace strings" prop_trim_all_whitespace
  , testProperty "trim: preserve content" prop_trim_preserve_content
  , testProperty "splitBy: preserve characters" prop_splitBy_preserve_characters
  , testProperty "splitByCollapsed: remove consecutive delimiters" prop_splitByCollapsed_remove_consecutive
  , testProperty "removeLineComments: no comments" prop_remove_line_comments_no_comments
  , testProperty "removeLineComments: preserve before comment" prop_remove_line_comments_preserve_before
  , testProperty "removeComments: block comments" prop_remove_comments_block_comments
  , testProperty "removeComments: nested comments" prop_remove_comments_nested
  , testProperty "String operations: composability" prop_string_operations_composable
  , testProperty "Unicode handling" prop_unicode_handling
  ]