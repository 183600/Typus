{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsPerformanceBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf)
import Test.QuickCheck.Gen (oneof, suchThat)

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

import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.List as L
import qualified Data.Text as T

-- Helper generators for edge cases

-- Generate strings with various whitespace combinations
genWhitespaceString :: Gen String
genWhitespaceString = listOf1 (elements " \t\n\r")

-- Generate strings with special characters
genSpecialCharString :: Gen String
genSpecialCharString = listOf1 (elements "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127")

-- Generate Unicode strings
genUnicodeString :: Gen String
genUnicodeString = listOf1 (elements "测试🚀café naïve résumé αβγδε 日本語 한국어")

-- Generate very large strings (limited for performance)
genLargeString :: Int -> Gen String
genLargeString maxSize = do
  size <- choose (0, min maxSize 10000)  -- Cap at 10k for performance
  vectorOf size (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']))

-- Generate strings with repeated patterns
genRepeatedPattern :: Gen String
genRepeatedPattern = do
  pattern <- listOf1 (elements ['a'..'z'])
  count <- choose (1, 100)
  return $ concat (replicate count pattern)

-- Generate strings with mixed line endings
genMixedLineEndings :: Gen String
genMixedLineEndings = do
  lines <- listOf1 (listOf1 (elements ['a'..'z']))
  endings <- listOf1 (elements ["\n", "\r\n", "\r"])
  return $ concat (L.zipWith (++) lines (take (length lines) (cycle endings)))

-- Generate strings with nested comment structures
genNestedCommentString :: Gen String
genNestedCommentString = do
  depth <- choose (0, 10)
  let buildNested 0 = "code"
      buildNested n = "/* " ++ buildNested (n-1) ++ " */"
  return $ buildNested depth

-- Performance and boundary property tests

-- Property: trim should handle very large strings efficiently
prop_trim_large_strings :: Property
prop_trim_large_strings =
  forAll (genLargeString 5000) $ \largeStr ->
  let trimmed = trim largeStr
      trimmedLength = length trimmed
      originalLength = length largeStr
  in property $ trimmedLength <= originalLength .&&.
     trimmedLength >= originalLength - 1000  -- Reasonable bound for whitespace

-- Property: trim should handle Unicode whitespace correctly
prop_trim_unicode_whitespace :: Property
prop_trim_unicode_whitespace =
  forAll genUnicodeString $ \unicodeStr ->
  let withWhitespace = " \t\n\r" ++ unicodeStr ++ " \t\n\r"
      trimmed = trim withWhitespace
  in property $ not (null trimmed) ==> not (isSpace (head trimmed)) .&&.
     not (isSpace (last trimmed))

-- Property: trim should handle null bytes correctly
prop_trim_null_bytes :: Property
prop_trim_null_bytes =
  let stringWithNulls = "\0\0\0content\0\0\0"
      trimmed = trim stringWithNulls
  in property $ "\0" `L.isInfixOf` trimmed

-- Property: splitBy should handle large strings efficiently
prop_splitBy_large_strings :: Property
prop_splitBy_large_strings =
  forAll (genLargeString 1000) $ \largeStr ->
  let withDelim = largeStr ++ "," ++ largeStr
      parts = splitBy ',' withDelim
  in property $ length parts === 3 .&&.
     all (not . null) parts

-- Property: splitBy should handle Unicode delimiters correctly
prop_splitBy_unicode_delimiters :: Property
prop_splitBy_unicode_delimiters =
  forAll genUnicodeString $ \unicodeStr ->
  let delim = '中'
      withDelim = unicodeStr ++ [delim] ++ unicodeStr
      parts = splitBy delim withDelim
  in property $ length parts === 2 .&&.
     all (not . null) parts

-- Property: splitByCollapsed should handle consecutive delimiters efficiently
prop_splitBy_collapsed_consecutive :: Property
prop_splitBy_collapsed_consecutive =
  let consecutiveDelims = replicate 1000 ','
      content = "prefix" ++ consecutiveDelims ++ "suffix"
      parts = splitByCollapsed ',' content
  in property $ length parts === 2 .&&.
     parts === ["prefix", "suffix"]

-- Property: removeLineComments should handle large files efficiently
prop_remove_line_comments_large :: Property
prop_remove_line_comments_large =
  forAll (genLargeString 2000) $ \largeStr ->
  let withComments = unlines (map (\line -> line ++ " // comment") (lines largeStr))
      cleaned = removeLineComments withComments
  in property $ length cleaned <= length withComments .&&.
     not ("// comment" `L.isInfixOf` cleaned)

-- Property: removeLineComments should handle Unicode in comments
prop_remove_line_comments_unicode :: Property
prop_remove_line_comments_unicode =
  forAll genUnicodeString $ \unicodeStr ->
  let withComment = "code // " ++ unicodeStr ++ " comment"
      cleaned = removeLineComments withComment
  in property $ not (unicodeStr `L.isInfixOf` cleaned)

-- Property: removeComments should handle deeply nested block comments
prop_remove_comments_nested :: Property
prop_remove_comments_nested =
  forAll genNestedCommentString $ \nestedStr ->
  let withComments = "prefix " ++ nestedStr ++ " suffix"
      cleaned = removeComments withComments
  in property $ not ("/*" `L.isInfixOf` cleaned) .&&.
     not ("*/" `L.isInfixOf` cleaned) .&&.
     "prefix" `L.isInfixOf` cleaned .&&.
     "suffix" `L.isInfixOf` cleaned

-- Property: removeComments should handle malformed comments gracefully
prop_remove_comments_malformed :: Property
prop_remove_comments_malformed =
  let malformed = "code /* unclosed comment // line comment\nmore code"
      cleaned = removeComments malformed
  in property $ length cleaned >= length "code more code"

-- Property: normalizeIndentation should handle mixed line endings
prop_normalize_indentation_mixed_endings :: Property
prop_normalize_indentation_mixed_endings =
  forAll genMixedLineEndings $ \mixedStr ->
  let normalized = normalizeIndentation mixedStr
      normalizedLines = lines normalized
  in property $ all (\line -> not (any isSpace (take 1 line))) normalizedLines

-- Property: normalizeIndentation should handle very deep indentation
prop_normalize_indentation_deep :: Property
prop_normalize_indentation_deep =
  let deepIndent = replicate 1000 ' ' ++ "content"
      content = unlines [deepIndent, deepIndent ++ " more"]
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in property $ all (\line -> not (any isSpace (take 1 line))) normalizedLines

-- Property: forceSingleTabIndentation should handle mixed whitespace
prop_force_tab_indentation_mixed :: Property
prop_force_tab_indentation_mixed =
  let mixedWhitespace = unlines
        [ "    content1"
        , "\tcontent2"
        , "  \t  content3"
        , "\t  content4"
        ]
      tabbed = forceSingleTabIndentation mixedWhitespace
      tabbedLines = lines tabbed
  in property $ all (\line -> null line || head line == '\t') tabbedLines

-- Property: breakOn should handle very large patterns efficiently
prop_break_on_large_pattern :: Property
prop_break_on_large_pattern =
  let largePattern = replicate 1000 'x'
      content = "prefix" ++ largePattern ++ "suffix"
      (before, after) = breakOn largePattern content
  in property $ before === "prefix" .&&. after === "suffix"

-- Property: breakOn should handle overlapping patterns
prop_break_on_overlapping_patterns :: Property
prop_break_on_overlapping_patterns =
  let pattern = "aba"
      content = "ababa"
      (before, after) = breakOn pattern content
  in property $ before ++ pattern ++ after === content

-- Property: breakOn should handle empty pattern
prop_break_on_empty_pattern :: Property
prop_break_on_empty_pattern =
  forAll (genLargeString 100) $ \content ->
  let (before, after) = breakOn "" content
  in property $ before === "" .&&. after === content

-- Property: splitByComma should handle CSV edge cases
prop_split_by_comma_csv_edge_cases :: Property
prop_split_by_comma_csv_edge_cases =
  let csvEdgeCases = 
        [ ""                    -- empty
        , ","                   -- single comma
        , ",,"                  -- consecutive commas
        , "a,b,c"               -- normal
        , "a,,b,c"              -- empty field
        , ",a,b,c"              -- leading empty
        , "a,b,c,"              -- trailing empty
        , "a,b,,c,d"            -- multiple empty
        ]
      results = map splitByComma csvEdgeCases
      expected = 
        [ [""]
        , ["", ""]
        , ["", "", ""]
        , ["a", "b", "c"]
        , ["a", "", "b", "c"]
        , ["", "a", "b", "c"]
        , ["a", "b", "c", ""]
        , ["a", "b", "", "c", "d"]
        ]
  in property $ results === expected

-- Property: splitByCommaCollapsed should handle repeated patterns efficiently
prop_split_by_comma_collapsed_repeated :: Property
prop_split_by_comma_collapsed_repeated =
  let repeated = concat (replicate 1000 "a,")
      collapsed = splitByCommaCollapsed repeated
  in property $ collapsed === ["a"]

-- Property: String processing pipeline should be efficient for large inputs
prop_string_pipeline_large_efficient :: Property
prop_string_pipeline_large_efficient =
  forAll (genLargeString 1000) $ \largeStr ->
  let withCommentsAndIndent = unlines 
        [ "    /* block comment */ " ++ largeStr ++ " // line comment"
        , "\t" ++ largeStr ++ "  /* another comment */"
        , "  " ++ largeStr
        ]
      processed = withCommentsAndIndent 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
  in property $ length processed <= length withCommentsAndIndent .&&.
     length processed >= length largeStr - 100

-- Property: trim should be idempotent even with special characters
prop_trim_idempotent_special :: Property
prop_trim_idempotent_special =
  forAll genSpecialCharString $ \specialStr ->
  let withWhitespace = " \t\n\r" ++ specialStr ++ " \t\n\r"
      trimmedOnce = trim withWhitespace
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy should preserve null bytes
prop_split_by_preserves_null_bytes :: Property
prop_split_by_preserves_null_bytes =
  let withNulls = "prefix\0middle\0suffix"
      parts = splitBy '\0' withNulls
  in property $ parts === ["prefix", "middle", "suffix"]

-- Property: removeComments should handle Unicode in block comments
prop_remove_comments_unicode_block :: Property
prop_remove_comments_unicode_block =
  forAll genUnicodeString $ \unicodeStr ->
  let withBlockComment = "code /* " ++ unicodeStr ++ " */ more code"
      cleaned = removeComments withBlockComment
  in property $ not (unicodeStr `L.isInfixOf` cleaned) .&&.
     "code" `L.isInfixOf` cleaned .&&.
     "more code" `L.isInfixOf` cleaned

-- Property: normalizeIndentation should handle tab-only indentation
prop_normalize_indentation_tabs_only :: Property
prop_normalize_indentation_tabs_only =
  let tabOnly = unlines 
        [ "\tcontent1"
        , "\t\tcontent2"
        , "\t\t\tcontent3"
        ]
      normalized = normalizeIndentation tabOnly
      normalizedLines = lines normalized
  in property $ all (\line -> not (any isSpace (take 1 line))) normalizedLines

-- Property: String functions should handle repeated operations efficiently
prop_repeated_operations_efficient :: Property
prop_repeated_operations_efficient =
  forAll (genLargeString 100) $ \baseStr ->
  let iterations = 10
      repeatedTrim = iterate trim baseStr !! iterations
      repeatedSplit = iterate (splitBy ',') baseStr !! iterations
  in property $ length repeatedTrim <= length baseStr .&&.
     length (head repeatedSplit) <= length baseStr

-- Helper operator for pipeline testing
(|>) :: a -> (a -> b) -> b
(|>) x f = f
infixl 0 |>

tests :: TestTree
tests = testGroup "Utils Performance and Boundary Tests"
  [ fastProperty "trim handles large strings efficiently" prop_trim_large_strings
  , fastProperty "trim handles Unicode whitespace correctly" prop_trim_unicode_whitespace
  , fastProperty "trim handles null bytes correctly" prop_trim_null_bytes
  , fastProperty "splitBy handles large strings efficiently" prop_splitBy_large_strings
  , fastProperty "splitBy handles Unicode delimiters correctly" prop_splitBy_unicode_delimiters
  , fastProperty "splitByCollapsed handles consecutive delimiters efficiently" prop_splitBy_collapsed_consecutive
  , fastProperty "removeLineComments handles large files efficiently" prop_remove_line_comments_large
  , fastProperty "removeLineComments handles Unicode in comments" prop_remove_line_comments_unicode
  , fastProperty "removeComments handles deeply nested block comments" prop_remove_comments_nested
  , fastProperty "removeComments handles malformed comments gracefully" prop_remove_comments_malformed
  , fastProperty "normalizeIndentation handles mixed line endings" prop_normalize_indentation_mixed_endings
  , fastProperty "normalizeIndentation handles very deep indentation" prop_normalize_indentation_deep
  , fastProperty "forceSingleTabIndentation handles mixed whitespace" prop_force_tab_indentation_mixed
  , fastProperty "breakOn handles very large patterns efficiently" prop_break_on_large_pattern
  , fastProperty "breakOn handles overlapping patterns" prop_break_on_overlapping_patterns
  , fastProperty "breakOn handles empty pattern" prop_break_on_empty_pattern
  , fastProperty "splitByComma handles CSV edge cases" prop_split_by_comma_csv_edge_cases
  , fastProperty "splitByCommaCollapsed handles repeated patterns efficiently" prop_split_by_comma_collapsed_repeated
  , fastProperty "String processing pipeline is efficient for large inputs" prop_string_pipeline_large_efficient
  , fastProperty "trim is idempotent even with special characters" prop_trim_idempotent_special
  , fastProperty "splitBy preserves null bytes" prop_split_by_preserves_null_bytes
  , fastProperty "removeComments handles Unicode in block comments" prop_remove_comments_unicode_block
  , fastProperty "normalizeIndentation handles tab-only indentation" prop_normalize_indentation_tabs_only
  , fastProperty "String functions handle repeated operations efficiently" prop_repeated_operations_efficient
  ]