{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreUtilsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (NonEmptyList(..))
import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.List (intercalate)
import Test.QuickCheck (Positive(..))
import Data.List (isInfixOf)

-- Test properties for Utils module

-- | trim should not change the length of non-space characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s = 
  let trimmed = trim s
      nonSpaceCount = length $ filter (not . isSpace) s
      trimmedNonSpaceCount = length $ filter (not . isSpace) trimmed
  in property $ nonSpaceCount == trimmedNonSpaceCount

-- | splitBy should produce the same result when concatenated with delimiter
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim s = 
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in property $ reconstructed == s

-- | splitByComma should be equivalent to splitBy ','
prop_splitByComma_equivalence :: String -> Property
prop_splitByComma_equivalence s = 
  property $ splitByComma s == splitBy ',' s

-- | removeLineComments should remove lines starting with //
prop_removeLineComments_basic :: String -> Property
prop_removeLineComments_basic s = 
  let withComment = s ++ "\n// This is a comment\n"
      withoutComment = removeLineComments withComment
  in property $ not ("// This is a comment" `isInfixOf` withoutComment)

-- | normalizeIndentation should preserve relative indentation
prop_normalizeIndentation_preserves_structure :: [String] -> Property
prop_normalizeIndentation_preserves_structure inputLines =
  let normalized = normalizeIndentation (unlines inputLines)      -- Check that non-empty lines are still present
      originalNonEmpty = length $ filter (not . null) inputLines
      normalizedNonEmpty = length $ filter (not . null) (lines normalized)
  in property $ originalNonEmpty == normalizedNonEmpty

-- | trim should be idempotent (trim(trim(s)) == trim(s))
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce == trimmedTwice

-- | splitBy should handle empty string correctly
prop_splitBy_empty_string :: Char -> Property
prop_splitBy_empty_string delim = 
  property $ splitBy delim "" == []

-- | splitBy should handle string with only delimiters correctly
prop_splitBy_only_delimiters :: Char -> Positive Int -> Property
prop_splitBy_only_delimiters delim (Positive n) = 
  let allDelims = replicate n delim
      result = splitBy delim allDelims
      expectedLength = n + 1
  in property $ length result == expectedLength && all null result

-- Unit tests
test_trim_empty :: Assertion
test_trim_empty = assertEqual "trim empty string" "" (trim "")

test_trim_all_spaces :: Assertion
test_trim_all_spaces = assertEqual "trim all spaces" "" (trim "   ")

test_trim_no_spaces :: Assertion
test_trim_no_spaces = assertEqual "trim no spaces" "hello" (trim "hello")

test_trim_leading_spaces :: Assertion
test_trim_leading_spaces = assertEqual "trim leading spaces" "hello" (trim "   hello")

test_trim_trailing_spaces :: Assertion
test_trim_trailing_spaces = assertEqual "trim trailing spaces" "hello" (trim "hello   ")

test_trim_both_spaces :: Assertion
test_trim_both_spaces = assertEqual "trim both spaces" "hello" (trim "   hello   ")

test_splitBy_basic :: Assertion
test_splitBy_basic = assertEqual "splitBy basic" ["a", "b", "c"] (splitBy ',' "a,b,c")

test_splitBy_empty_elements :: Assertion
test_splitBy_empty_elements = assertEqual "splitBy empty elements" ["a", "", "c"] (splitBy ',' "a,,c")

test_splitBy_leading_empty :: Assertion
test_splitBy_leading_empty = assertEqual "splitBy leading empty" ["", "b", "c"] (splitBy ',' ",b,c")

test_splitBy_trailing_empty :: Assertion
test_splitBy_trailing_empty = assertEqual "splitBy trailing empty" ["a", "b", ""] (splitBy ',' "a,b,")

test_removeLineComments_simple :: Assertion
test_removeLineComments_simple = do
  let input = "hello world\n// this is a comment\ngoodbye"
  let expected = "hello world\n\ngoodbye"
  assertEqual "removeLineComments simple" expected (removeLineComments input)

test_removeLineComments_no_comments :: Assertion
test_removeLineComments_no_comments = do
  let input = "hello world\ngoodbye"
  let expected = "hello world\ngoodbye"
  assertEqual "removeLineComments no comments" expected (removeLineComments input)

test_normalizeIndentation_basic :: Assertion
test_normalizeIndentation_basic = do
  let input = "    line1\n      line2\n    line3"
  let expected = "line1\n  line2\nline3"
  assertEqual "normalizeIndentation basic" expected (normalizeIndentation input)

-- Test suite
tests :: TestTree
tests = testGroup "Core Utils Tests"
  [ testProperties "QuickCheck Properties"
    [ ("trim_preserves_content", property $ prop_trim_preserves_content "test")
    , ("splitBy_roundtrip", property $ prop_splitBy_roundtrip ',' "a,b,c")
    , ("removeLineComments_basic", property $ prop_removeLineComments_basic "test")
    ]
  , testCase "trim empty" test_trim_empty
  , testCase "trim all spaces" test_trim_all_spaces
  , testCase "trim no spaces" test_trim_no_spaces
  , testCase "splitBy basic" test_splitBy_basic
  , testCase "removeLineComments simple" test_removeLineComments_simple
  ]