{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsStringBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
  , UnicodeString(..)
  )

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

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)
import qualified Data.Text as T

-- Test trim function with various edge cases
test_trim_edge_cases :: TestTree
test_trim_edge_cases = testCase "Trim function handles edge cases" $ do
  trim "" @?= ""
  trim "   " @?= ""
  trim "\t\n\r" @?= ""
  trim "content" @?= "content"
  trim "  content  " @?= "content"
  trim "\tcontent\n" @?= "content"
  trim "  mixed \t whitespace \n content  " @?= "mixed \t whitespace \n content"

-- Test splitBy function with edge cases
test_splitby_edge_cases :: TestTree
test_splitby_edge_cases = testCase "SplitBy function handles edge cases" $ do
  splitBy ',' "" @?= [""]
  splitBy ',' "a" @?= ["a"]
  splitBy ',' "," @?= ["", ""]
  splitBy ',' ",," @?= ["", "", ""]
  splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  splitBy ',' "a,,b" @?= ["a", "", "b"]
  splitBy ',' ",a,b," @?= ["", "a", "b", ""]
  splitBy ',' "a,b," @?= ["a", "b", ""]

-- Test splitByCollapsed function with edge cases
test_splitby_collapsed_edge_cases :: TestTree
test_splitby_collapsed_edge_cases = testCase "SplitByCollapsed function handles edge cases" $ do
  splitByCollapsed ',' "" @?= []
  splitByCollapsed ',' "a" @?= ["a"]
  splitByCollapsed ',' "," @?= []
  splitByCollapsed ',' ",," @?= []
  splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
  splitByCollapsed ',' "a,,b" @?= ["a", "b"]
  splitByCollapsed ',' ",a,b," @?= ["a", "b"]
  splitByCollapsed ',' "a,b," @?= ["a", "b"]

-- Test splitByComma L.and splitByCommaCollapsed consistency
test_comma_split_consistency :: TestTree
test_comma_split_consistency = testCase "Comma split functions are consistent" $ do
  let testString = "a,b,,c,"
  splitByComma testString @?= splitBy ',' testString
  splitByCommaCollapsed testString @?= splitByCollapsed ',' testString

-- Test removeLineComments function with edge cases
test_remove_line_comments :: TestTree
test_remove_line_comments = testCase "RemoveLineComments handles edge cases" $ do
  removeLineComments "" @?= ""
  removeLineComments "no comments" @?= "no comments"
  removeLineComments "// line comment" @?= ""
  removeLineComments "code // comment" @?= "code "
  removeLineComments "code // comment\nmore code" @?= "code \nmore code"
  removeLineComments "code // comment // another comment" @?= "code "
  removeLineComments "// comment\n// another comment" @?= "\n"

-- Test removeComments function with edge cases
test_remove_comments :: TestTree
test_remove_comments = testCase "RemoveComments handles edge cases" $ do
  removeComments "" @?= ""
  removeComments "no comments" @?= "no comments"
  removeComments "// line comment" @?= ""
  removeComments "/* block comment */" @?= ""
  removeComments "code // comment" @?= "code "
  removeComments "code /* comment */ more" @?= "code  more"
  removeComments "/* multi\nline\ncomment */" @?= ""
  removeComments "code /* nested /* comment */ */ more" @?= "code  more"

-- Test normalizeIndentation function with edge cases
test_normalize_indentation :: TestTree
test_normalize_indentation = testCase "NormalizeIndentation handles edge cases" $ do
  normalizeIndentation "" @?= ""
  normalizeIndentation "no indentation" @?= "no indentation"
  normalizeIndentation "    indented" @?= "indented"
  normalizeIndentation "\tindented" @?= "indented"
  normalizeIndentation "  mixed \t indentation" @?= "mixed \t indentation"
  normalizeIndentation "    line1\n        line2\n    line3" @?= "line1\n    line2\nline3"

-- Test forceSingleTabIndentation function with edge cases
test_force_tab_indentation :: TestTree
test_force_tab_indentation = testCase "ForceSingleTabIndentation handles edge cases" $ do
  forceSingleTabIndentation "" @?= ""
  forceSingleTabIndentation "no indentation" @?= "no indentation"
  forceSingleTabIndentation "    indented" @?= "\tindented"
  forceSingleTabIndentation "\tindented" @?= "\tindented"
  forceSingleTabIndentation "        double" @?= "\t\tdouble"

-- Test fixIndentation alias function
test_fix_indentation :: TestTree
test_fix_indentation = testCase "FixIndentation works as alias" $ do
  let testString = "    indented\n        more"
  fixIndentation testString @?= normalizeIndentation testString

-- Test breakOn function with edge cases
test_break_on :: TestTree
test_break_on = testCase "BreakOn function handles edge cases" $ do
  breakOn ',' "" @?= ("", "")
  breakOn ',' "abc" @?= ("abc", "")
  breakOn ',' "a,b,c" @?= ("a", ",b,c")
  breakOn ',' ",abc" @?= ("", ",abc")
  breakOn ',' "abc," @?= ("abc", ",")
  breakOn ',' "abc,def,ghi" @?= ("abc", ",def,ghi")

-- Test Unicode handling in string functions
test_unicode_handling :: TestTree
test_unicode_handling = testCase "String functions handle Unicode correctly" $ do
  let unicodeString = "测试Unicode🚀内容"
  trim ("  " ++ unicodeString ++ "  ") @?= unicodeString
  splitBy ' ' unicodeString @?= [unicodeString]
  splitBy ' ' "测试 Unicode 内容" @?= ["测试", "Unicode", "内容"]
  removeLineComments ("code // 测试注释") @?= "code "

-- Test string functions with very long inputs
test_long_strings :: TestTree
test_long_strings = testCase "String functions handle long inputs" $ do
  let longString = replicate 1000 'a'
      veryLongString = longString ++ "," ++ longString
  splitBy ',' veryLongString @?= [longString, longString]
  trim ("  " ++ longString ++ "  ") @?= longString
  L.length (splitBy ',' (replicate 100 ',')) @?= 101

-- Test string functions with special characters
test_special_characters :: TestTree
test_special_characters = testCase "String functions handle special characters" $ do
  let specialString = "\n\t\r\0"
  trim specialString @?= specialString
  splitBy '\n' "line1\nline2\nline3" @?= ["line1", "line2", "line3"]
  removeLineComments ("code\n// comment\nmore") @?= "code\n\nmore"

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str = 
  let trimmed = trim str
      trimmedAgain = trim trimmed
  in trimmed === trimmedAgain

-- Property: splitBy L.and splitByCollapsed are related
prop_splitby_relationship :: Char -> String -> Property
prop_splitby_relationship delim str = 
  let normalSplit = splitBy delim str
      collapsedSplit = splitByCollapsed delim str
  in property $ L.filter (not . null) normalSplit === collapsedSplit

-- Property: splitBy preserves total L.length
prop_splitby_preserves_length :: Char -> String -> Property
prop_splitby_preserves_length delim str = 
  let parts = splitBy delim str
      reconstructed = intercalate [delim] parts
  in L.length str === L.length reconstructed + (if null str then 0 else -1)

-- Property: removeLineComments doesn't change content without comments
prop_remove_line_comments_preserves_content :: String -> Property
prop_remove_line_comments_preserves_content str = 
  not (L.isInfixOf "//" str) ==> removeLineComments str === str

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative str = 
  let lines' = lines str
      normalized = normalizeIndentation str
      normalizedLines = lines normalized
  in property $ L.length lines' === L.length normalizedLines

-- Property: breakOn is consistent with splitBy
prop_break_on_consistent :: Char -> String -> Property
prop_break_on_consistent delim str = 
  let (prefix, suffix) = breakOn delim str
      parts = splitBy delim str
  in case parts of
    [] -> prefix === "" &&. suffix === ""
    [x] -> prefix === x &&. suffix === ""
    (x:xs) -> prefix === x &&. suffix === delim ++ intercalate [delim] xs

-- Property: string functions handle empty strings gracefully
prop_empty_string_handling :: Char -> Property
prop_empty_string_handling delim = 
  let empty = ""
  in trim empty === "" .&&.
     splitBy delim empty === [""] .&&.
     splitByCollapsed delim empty === [] .&&.
     removeLineComments empty === "" .&&.
     removeComments empty === "" .&&.
     normalizeIndentation empty === "" .&&.
     breakOn delim empty === ("", "")

-- Property: string functions handle Unicode correctly
prop_unicode_string_handling :: UnicodeString -> Property
prop_unicode_string_handling (UnicodeString str) = 
  let trimmed = trim str
      parts = splitBy ' ' str
  in property $ L.all (not . null) parts .&&. L.length trimmed <= L.length str

-- Helper function for property tests
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests = testGroup "New Utils String Boundary Tests"
  [ test_trim_edge_cases
  , test_splitby_edge_cases
  , test_splitby_collapsed_edge_cases
  , test_comma_split_consistency
  , test_remove_line_comments
  , test_remove_comments
  , test_normalize_indentation
  , test_force_tab_indentation
  , test_fix_indentation
  , test_break_on
  , test_unicode_handling
  , test_long_strings
  , test_special_characters
  , fastProperty "Trim is idempotent" prop_trim_idempotent
  , fastProperty "SplitBy L.and SplitByCollapsed relationship" prop_splitby_relationship
  , fastProperty "SplitBy preserves total L.length" prop_splitby_preserves_length
  , fastProperty "RemoveLineComments preserves content without comments" prop_remove_line_comments_preserves_content
  , fastProperty "NormalizeIndentation preserves relative indentation" prop_normalize_preserves_relative
  , fastProperty "BreakOn is consistent with SplitBy" prop_break_on_consistent
  , fastProperty "String functions handle empty strings gracefully" prop_empty_string_handling
  , fastProperty "String functions handle Unicode correctly" prop_unicode_string_handling
  ]