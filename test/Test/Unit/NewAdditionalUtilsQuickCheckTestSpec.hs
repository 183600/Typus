{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdditionalUtilsQuickCheckTestSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Test trim function properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in not (null trimmed) ==> 
     let firstChar = trimmed !! 0
         lastChar = trimmed !! (length trimmed - 1)
     in not (isSpace firstChar) && not (isSpace lastChar)

prop_trim_empty_string :: Bool
prop_trim_empty_string = trim "" == ""

-- | Test splitBy function properties
prop_split_by_empty_string :: Char -> Bool
prop_split_by_empty_string c = splitBy c "" == []

prop_split_by_single_char :: Char -> Char -> Property
prop_split_by_single_char c x = c /= x ==> splitBy c [x] == [[x]]

prop_split_by_single_delimiter :: Char -> Bool
prop_split_by_single_delimiter c = splitBy c [c] == ["", ""]

prop_split_by_all_delimiters :: Char -> Positive Int -> Property
prop_split_by_all_delimiters c (Positive n) = 
  let allDelims = replicate n c
      result = splitBy c allDelims
  in property $ length result === n + 1 .&&. all null result

prop_split_by_preserves_content :: Char -> String -> String -> Property
prop_split_by_preserves_content c s1 s2 = 
  not (any (== c) s1) && not (any (== c) s2) ==>
  splitBy c (s1 ++ [c] ++ s2) == [s1, s2]

-- | Test splitByComma function properties
prop_split_by_comma_equivalent :: String -> Property
prop_split_by_comma_equivalent s = splitByComma s === splitBy ',' s

-- | Test splitByCollapsed function properties
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty c s = 
  not (null (splitByCollapsed c s)) ==> all (not . null) (splitByCollapsed c s)

prop_split_by_collapsed_removes_consecutive :: Char -> Positive Int -> String -> Property
prop_split_by_collapsed_removes_consecutive c (Positive n) s = 
  not (any (== c) s) ==> 
  let consecutiveDelims = replicate n c
      input = s ++ consecutiveDelims ++ s
      result = splitByCollapsed c input
  in property $ result === [s, s] .&&. length result === 2

-- | Test removeLineComments function properties
prop_remove_line_comments_no_comment :: String -> Property
prop_remove_line_comments_no_comment s = 
  not ("//" `isPrefixOf` s) ==> removeLineComments s === s

prop_remove_line_comments_removes_content :: String -> String -> Property
prop_remove_line_comments_removes_content prefix suffix = 
  let input = prefix ++ "// comment"
  in not (any (== '\n') input) ==> 
     removeLineComments input === prefix

prop_remove_line_comments_preserves_multiline :: String -> String -> Property
prop_remove_line_comments_preserves_multiline line1 line2 = 
  not (any (== '\n') line1) && not (any (== '\n') line2) ==>
  let input = line1 ++ "\n" ++ line2
  in removeLineComments input === input

-- | Test utility functions
prop_string_roundtrip :: String -> Bool
prop_string_roundtrip s = s == s

prop_length_preservation :: String -> Property
prop_length_preservation s = 
  let trimmed = trim s
  in property $ length trimmed <= length s

-- | Combine all tests
newAdditionalUtilsQuickCheckTestSpec :: TestTree
newAdditionalUtilsQuickCheckTestSpec = testGroup "New Additional Utils QuickCheck Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim removes leading/trailing spaces" prop_trim_no_leading_trailing_spaces
  , testProperty "trim handles empty string" prop_trim_empty_string
  , testProperty "splitBy handles empty string" prop_split_by_empty_string
  , testProperty "splitBy handles single character" prop_split_by_single_char
  , testProperty "splitBy handles single delimiter" prop_split_by_single_delimiter
  , testProperty "splitBy handles all delimiters" prop_split_by_all_delimiters
  , testProperty "splitBy preserves content" prop_split_by_preserves_content
  , testProperty "splitByComma equivalent to splitBy ','" prop_split_by_comma_equivalent
  , testProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_no_empty
  , testProperty "splitByCollapsed removes consecutive delimiters" prop_split_by_collapsed_removes_consecutive
  , testProperty "removeLineComments preserves non-comment lines" prop_remove_line_comments_no_comment
  , testProperty "removeLineComments removes comment content" prop_remove_line_comments_removes_content
  , testProperty "removeLineComments preserves multiline" prop_remove_line_comments_preserves_multiline
  , testProperty "string roundtrip" prop_string_roundtrip
  , testProperty "length preservation after trim" prop_length_preservation
  ]