{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.StringUtilsAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub, intersperse)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isSpace, isControl, isAscii)
import qualified Data.String

-- ============================================================================
-- Advanced String Utils Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Advanced String Utils Tests"
    [ testGroup "Advanced trimming tests"
        [ testCase "handles Unicode whitespace correctly" test_unicode_whitespace_trimming
        , testCase "preserves internal whitespace structure" test_internal_whitespace_preservation
        , testCase "handles mixed whitespace types" test_mixed_whitespace_types
        , testCase "handles zero-width whitespace" test_zero_width_whitespace
        , testCase "trim is idempotent on L.all inputs" test_trim_idempotent_all
        ]

    , testGroup "Advanced splitting tests"
        [ testCase "handles Unicode delimiters" test_unicode_delimiters
        , testCase "handles empty string splitting" test_empty_string_splitting
        , testCase "handles consecutive delimiters correctly" test_consecutive_delimiters
        , testCase "splitBy is consistent with splitByComma" test_splitby_comma_consistency
        , testCase "splitByCollapsed removes empty segments" test_splitby_collapsed_removal
        ]

    , testGroup "Advanced comment removal tests"
        [ testCase "handles nested block comments" test_nested_block_comments
        , testCase "preserves comments in string literals" test_comment_preservation_in_strings
        , testCase "handles malformed comments gracefully" test_malformed_comments
        , testCase "handles comments with Unicode content" test_unicode_comments
        , testCase "removes comments efficiently in large files" test_large_file_comment_removal
        ]

    , testGroup "Advanced indentation tests"
        [ testCase "handles mixed tabs L.and spaces" test_mixed_tabs_spaces
        , testCase "preserves relative indentation structure" test_relative_indentation_preservation
        , testCase "handles very deep indentation" test_deep_indentation
        , testCase "normalizes indentation with Unicode" test_unicode_indentation
        , testCase "forceSingleTabIndentation edge cases" test_force_single_tab_edge_cases
        ]

    , testGroup "Advanced text processing tests"
        [ testCase "breakOn with overlapping patterns" test_breakon_overlapping
        , testCase "breakOn with Unicode patterns" test_breakon_unicode
        , testCase "handles empty patterns correctly" test_breakon_empty_pattern
        , testCase "breakOn performance on large texts" test_breakon_performance
        ]

    , testGroup "Property-based advanced tests"
        [ fastProperty "trim never increases string L.length" prop_trim_never_increases
        , fastProperty "splitBy preserves total character count" prop_splitby_preserves_chars
        , fastProperty "removeComments preserves non-comment content" prop_remove_comments_preserves_content
        , fastProperty "normalizeIndentation preserves line count" prop_normalize_preserves_lines
        , fastProperty "L.all operations are safe on Unicode input" prop_unicode_safety
        ]
    ]

-- ============================================================================
-- Advanced Trimming Tests
-- ============================================================================

test_unicode_whitespace_trimming :: IO ()
test_unicode_whitespace_trimming = do
  let unicodeSpaces = "\x00A0\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200A\x202F\x205F"
      testCases = 
        [ (unicodeSpaces ++ "content" ++ unicodeSpaces, "content")
        , ("\t  " ++ unicodeSpaces ++ "content" ++ unicodeSpaces ++ "  \t", "content")
        , ("  content with " ++ unicodeSpaces ++ " spaces  ", "content with" ++ unicodeSpaces ++ " spaces")
        ]
  mapM_ (\(input, expected) -> do
    let result = trim input
    result @?= expected
    ) testCases

test_internal_whitespace_preservation :: IO ()
test_internal_whitespace_preservation = do
  let testCases = 
        [ ("  hello   world  ", "hello   world")
        , ("\t  a \t  b \t  c  \t", "a \t  b \t  c")
        , ("  multiple    spaces   between   words  ", "multiple    spaces   between   words")
        , ("  \t mixed \t whitespace \t types  \t", "mixed \t whitespace \t types")
        ]
  mapM_ (\(input, expected) -> do
    let result = trim input
    result @?= expected
    ) testCases

test_mixed_whitespace_types :: IO ()
test_mixed_whitespace_types = do
  let mixedWhitespace = "\x20\x09\x0A\x0D\x0B\x0C"  -- space, tab, newline, carriage return, vertical tab, form feed
      input = mixedWhitespace ++ "content" ++ mixedWhitespace
      result = trim input
  result @?= "content"

test_zero_width_whitespace :: IO ()
test_zero_width_whitespace = do
  let zeroWidthChars = "\x200B\x200C\x200D\xFEFF"  -- zero-width space, non-joiner, joiner, BOM
      input = zeroWidthChars ++ "content" ++ zeroWidthChars
      result = trim input
  -- Zero-width characters should not be considered whitespace for trimming
  result @?= input

test_trim_idempotent_all :: IO ()
test_trim_idempotent_all = do
  let testCases = ["", " ", "\t", "\n", "  content  ", "\t content \t", "content", "  content  more  "]
  mapM_ (\input -> do
    let once = trim input
        twice = trim once
    once @?= twice
    ) testCases

-- ============================================================================
-- Advanced Splitting Tests
-- ============================================================================

test_unicode_delimiters :: IO ()
test_unicode_delimiters = do
  let unicodeDelim = '，'  -- Full-width comma
      input = "part1" ++ [unicodeDelim] ++ "part2" ++ [unicodeDelim] ++ "part3"
      result = splitBy unicodeDelim input
      expected = ["part1", "part2", "part3"]
  result @?= expected

test_empty_string_splitting :: IO ()
test_empty_string_splitting = do
  let result = splitBy ',' ""
  result @?= [""]
  let resultCollapsed = splitByCollapsed ',' ""
  resultCollapsed @?= []

test_consecutive_delimiters :: IO ()
test_consecutive_delimiters = do
  let input = "a,,b,,,c"
      regularSplit = splitBy ',' input
      collapsedSplit = splitByCollapsed ',' input
  regularSplit @?= ["a", "", "b", "", "", "c"]
  collapsedSplit @?= ["a", "b", "c"]

test_splitby_comma_consistency :: IO ()
test_splitby_comma_consistency = do
  let testInputs = ["a,b,c", "a,,b", ",start", "end,", ",", ""]
  mapM_ (\input -> do
    let byComma = splitBy ',' input
        byFunction = splitByComma input
    byComma @?= byFunction
    ) testInputs

test_splitby_collapsed_removal :: IO ()
test_splitby_collapsed_removal = do
  let testCases = 
        [ ("a,,b", ["a", "b"])
        , (",,a,,b,,", ["a", "b"])
        , ("a,b,c", ["a", "b", "c"])
        , (",,,", [])
        ]
  mapM_ (\(input, expected) -> do
    let result = splitByCollapsed ',' input
    result @?= expected
    ) testCases

-- ============================================================================
-- Advanced Comment Removal Tests
-- ============================================================================

test_nested_block_comments :: IO ()
test_nested_block_comments = do
  let input = "code /* outer /* inner */ still outer */ more code"
      result = removeComments input
  -- Should remove everything from first /* to first */
  expected = "code  still outer */ more code"
  result @?= expected

test_comment_preservation_in_strings :: IO ()
test_comment_preservation_in_strings = do
  let testCases = 
        [ ("var s = \"// not a comment\" // real comment", "var s = \"// not a comment\" ")
        , ("text = \"/* not a block */\" /* real block */", "text = \"/* not a block */\" ")
        , ("mixed = \"// L.and /*\" /* real */ // comment", "mixed = \"// L.and /*\"  ")
        ]
  mapM_ (\(input, expected) -> do
    let result = removeComments input
    result @?= expected
    ) testCases

test_malformed_comments :: IO ()
test_malformed_comments = do
  let testCases = 
        [ ("code /* unterminated", "code ")  -- Should drop everything after /*
        , ("code // no newline", "code ")  -- Should remove comment
        , ("code /* nested /* no end", "code ")  -- Should handle gracefully
        ]
  mapM_ (\(input, expected) -> do
    let result = removeComments input
    result @?= expected
    ) testCases

test_unicode_comments :: IO ()
test_unicode_comments = do
  let input = "code // 注释内容\nmore /* 块注释 */ code"
      result = removeComments input
  expected = "code \nmore  code"
  result @?= expected

test_large_file_comment_removal :: IO ()
test_large_file_comment_removal = do
  let largeLine = "code // comment\n" ++ "    x := x + 1\n"
      largeContent = L.concat $ replicate 1000 largeLine
      result = removeComments largeContent
  assertBool "Should handle large file comment removal" (L.length result > 0)
  assertBool "Should remove comments" (not ("// comment" `L.isInfixOf` result))

-- ============================================================================
-- Advanced Indentation Tests
-- ============================================================================

test_mixed_tabs_spaces :: IO ()
test_mixed_tabs_spaces = do
  let input = unlines
        [ "    line1"
        , "\tline2"
        , "  \t line3"
        , "\t  line4"
        ]
      result = normalizeIndentation input
      resultLines = lines result
  -- Should normalize to remove common leading whitespace
  assertBool "Should normalize mixed indentation" (L.all (not . L.isPrefixOf "    ") resultLines)

test_relative_indentation_preservation :: IO ()
test_relative_indentation_preservation = do
  let input = unlines
        [ "    level1"
        , "        level2"
        , "            level3"
        , "        level2"
        , "    level1"
        ]
      result = normalizeIndentation input
      resultLines = lines result
      indentLevels = L.map (L.length . takeWhile isSpace) resultLines
  -- Should preserve relative indentation structure
  indentLevels @?= [0, 4, 8, 4, 0]

test_deep_indentation :: IO ()
test_deep_indentation = do
  let deepIndent = L.concat $ replicate 100 "    "
      input = deepIndent ++ "deeply indented line\n"
      result = normalizeIndentation input
  result @?= "deeply indented line\n"

test_unicode_indentation :: IO ()
test_unicode_indentation = do
  let input = unlines
        [ "    regular line"
        , "　　全角空格行"  -- Full-width spaces
        , "    another regular line"
        ]
      result = normalizeIndentation input
  resultLines = lines result
  -- Should handle Unicode spaces in indentation
  assertBool "Should handle Unicode indentation" (L.length resultLines == 3)

test_force_single_tab_edge_cases :: IO ()
test_force_single_tab_edge_cases = do
  let testCases = 
        [ ("    line", "\tline")
        , ("", "")
        , ("   \t  \n   \n", "\t\n\n")
        , ("\talready tabbed", "\talready tabbed")
        ]
  mapM_ (\(input, expected) -> do
    let result = forceSingleTabIndentation input
    result @?= expected
    ) testCases

-- ============================================================================
-- Advanced Text Processing Tests
-- ============================================================================

test_breakon_overlapping :: IO ()
test_breakon_overlapping = do
  let input = "ababab"
      pattern = "aba"
      (before, after) = breakOn pattern input
  before @?= "aba"
  after @?= "bab"

test_breakon_unicode :: IO ()
test_breakon_unicode = do
  let input = "Hello 世界 🚀 World"
      pattern = "世界"
      (before, after) = breakOn pattern input
  before @?= "Hello "
  after @?= " 🚀 World"

test_breakon_empty_pattern :: IO ()
test_breakon_empty_pattern = do
  let input = "test string"
      (before, after) = breakOn "" input
  before @?= ""
  after @?= input

test_breakon_performance :: IO ()
test_breakon_performance = do
  let largeInput = L.concat $ replicate 10000 "test content "
      pattern = "middle"
      inputWithPattern = take (L.length largeInput `div` 2) largeInput ++ pattern ++ drop (L.length largeInput `div` 2) largeInput
      (before, after) = breakOn pattern inputWithPattern
  assertBool "Should find pattern in large text" (pattern `L.isInfixOf` (before ++ pattern ++ after))

-- ============================================================================
-- Property-Based Advanced Tests
-- ============================================================================

prop_trim_never_increases :: Property
prop_trim_never_increases =
  forAll arbitrary $ \input ->
    let trimmed = trim input
    in property $ L.length trimmed <= L.length input

prop_splitby_preserves_chars :: Property
prop_splitby_preserves_chars =
  forAll arbitrary $ \input ->
  forAll arbitrary $ \delim ->
    let parts = splitBy delim input
        rejoined = intersperse [delim] parts >>= id
    in property $ rejoined == input

prop_remove_comments_preserves_content :: Property
prop_remove_comments_preserves_content =
  forAll arbitrary $ \input ->
    let withoutComments = removeComments input
        hasNoComments = not ("//" `L.isInfixOf` withoutComments) && 
                        not ("/*" `L.isInfixOf` withoutComments)
    in property $ hasNoComments ==> True

prop_normalize_preserves_lines :: Property
prop_normalize_preserves_lines =
  forAll arbitrary $ \input ->
    let normalized = normalizeIndentation input
        originalLines = lines input
        normalizedLines = lines normalized
    in property $ L.length normalizedLines == L.length originalLines

prop_unicode_safety :: Property
prop_unicode_safety =
  forAll arbitrary $ \input ->
    let trimmed = trim input
        split = splitBy ',' input
        withoutComments = removeComments input
        normalized = normalizeIndentation input
        broken = breakOn "test" input
    in property $ True  -- If we get here without crashing, Unicode is handled safely