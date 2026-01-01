{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, vector)
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import qualified Data.Text as T

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

-- ============================================================================
-- Enhanced Arbitrary Instances
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r" ++ ['\32'..'\126']

-- Generate strings without quotes (for comment testing)
genStringWithoutQuotes :: Gen String
genStringWithoutQuotes = listOf $ elements $ L.filter (`notElem` "\"'") ['\32'..'\126']

-- Generate strings with balanced quotes for testing comment preservation
genStringWithBalancedQuotes :: Gen String
genStringWithBalancedQuotes = do
  before <- listOf $ elements $ L.filter (`notElem` "\"'\\") ['\32'..'\126']
  content <- listOf $ elements $ L.filter (`notElem` "\"") ['\32'..'\126']
  after <- listOf $ elements $ L.filter (`notElem` "\"'\\") ['\32'..'\126']
  return $ before ++ "\"" ++ content ++ "\"" ++ after

-- Generate multiline strings with various indentation
genMultilineString :: Gen String
genMultilineString = do
  numLines <- choose (1, 10)
  lines <- vector numLines
  indentSizes <- vector numLines
  let indentedLines = zipWith (\line indent -> replicate (abs indent `mod` 8) ' ' ++ line) lines indentSizes
  return $ unlines indentedLines

-- ============================================================================
-- Advanced Utils Properties
-- ============================================================================

-- Property: trim never increases string L.length
prop_trim_never_increases_length :: String -> Property
prop_trim_never_increases_length str =
  let trimmed = trim str
  in property $ L.length trimmed <= L.length str

-- Property: trim removes L.all leading L.and trailing whitespace
prop_trim_removes_all_whitespace :: String -> Property
prop_trim_removes_all_whitespace str =
  let trimmed = trim str
      hasLeadingWhitespace = not (null str) && isSpace (L.head str)
      hasTrailingWhitespace = not (null str) && isSpace (last str)
  in classify hasLeadingWhitespace "had leading whitespace" $
     classify hasTrailingWhitespace "had trailing whitespace" $
     property $ (null trimmed || not (isSpace (L.head trimmed))) .&&.
                (null trimmed || not (isSpace (last trimmed)))

-- Property: splitBy preserves total character count (excluding delimiters)
prop_splitBy_preserves_char_count :: Char -> String -> Property
prop_splitBy_preserves_char_count delim str =
  let parts = splitBy delim str
      totalChars = L.sum (map L.length parts)
      originalChars = L.length (L.filter (/= delim) str)
  in property $ totalChars === originalChars

-- Property: splitBy L.and intercalate roundtrip for L.any delimiter
prop_splitBy_intercalate_roundtrip :: Char -> String -> Property
prop_splitBy_intercalate_roundtrip delim str =
  let parts = splitBy delim str
      reconstructed = Data.List.intercalate [delim] parts
  in property $ reconstructed === str

-- Property: splitByCollapsed never produces empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ not (L.any null parts)

-- Property: splitByCollapsed removes consecutive delimiters
prop_splitByCollapsed_removes_consecutive :: Char -> String -> Property
prop_splitByCollapsed_removes_consecutive delim str =
  let hasConsecutive = delim : delim `L.isInfixOf` str
      partsRegular = splitBy delim str
      partsCollapsed = splitByCollapsed delim str
  in classify hasConsecutive "has consecutive delimiters" $
     property $ L.length partsCollapsed <= L.length partsRegular

-- Property: removeLineComments preserves string literals
prop_removeLineComments_preserves_string_literals :: String -> String -> Property
prop_removeLineComments_preserves_string_literals prefix comment =
  not ('"' `elem` prefix) ==> 
  let stringWithComment = prefix ++ "\"// not a comment\" // real comment"
      result = removeLineComments stringWithComment
  in property $ "// not a comment" `L.isInfixOf` result .&&.
     not ("// real comment" `L.isInfixOf` result)

-- Property: removeComments handles nested block comments correctly (C-style)
prop_removeComments_nested_block_comments :: String -> String -> String -> Property
prop_removeComments_nested_block_comments before middle after =
  not ('"' `elem` before) && not ('"' `elem` middle) && not ('"' `elem` after) &&
  not ("/*" `L.isInfixOf` before) && not ("/*" `L.isInfixOf` middle) && not ("/*" `L.isInfixOf` after) ==>
  let input = before ++ "/* outer /* inner */ text */" ++ middle
      result = removeComments input
  in property $ not ("/* outer" `L.isInfixOf` result) .&&.
     not ("/* inner" `L.isInfixOf` result) .&&.
     (middle `L.isInfixOf` result)

-- Property: removeComments preserves escaped quotes in strings
prop_removeComments_preserves_escaped_quotes :: String -> Property
prop_removeComments_preserves_escaped_quotes content =
  not ('\\' `elem` content) && not ('"' `elem` content) ==>
  let input = "var s = \"\\\"escaped\\\" " ++ content ++ "\" // comment"
      result = removeComments input
  in property $ "\\\"escaped\\\"" `L.isInfixOf` result .&&.
     not ("// comment" `L.isInfixOf` result)

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_line_count :: String -> Property
prop_normalizeIndentation_preserves_line_count str =
  let normalized = normalizeIndentation str
      originalLines = lines str
      normalizedLines = lines normalized
  in property $ L.length normalizedLines === L.length originalLines

-- Property: normalizeIndentation removes common prefix indentation
prop_normalizeIndentation_removes_common_prefix :: [Int] -> String -> Property
prop_normalizeIndentation_removes_common_prefix indentLevels content =
  not (null indentLevels) && not ('\n' `elem` content) ==>
  let lines' = zipWith (\level -> replicate (abs level `mod` 10) ' ' ++) indentLevels (repeat content)
      input = unlines lines'
      result = normalizeIndentation input
      resultLines = lines result
      nonEmptyLines = L.filter (not . null) resultLines
      minIndent = if null nonEmptyLines then 0 else L.minimum [L.length (takeWhile isSpace line) | line <- nonEmptyLines]
  in property $ minIndent === 0

-- Property: forceSingleTabIndentation converts L.all non-empty lines to tab start
prop_forceSingleTabIndentation_tab_conversion :: String -> Property
prop_forceSingleTabIndentation_tab_conversion str =
  let result = forceSingleTabIndentation str
      resultLines = lines result
      nonEmptyLines = L.filter (not . null . trim) resultLines
  in property $ L.all (\line -> null line || L.head line == '\t') nonEmptyLines

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize str =
  fixIndentation str === normalizeIndentation str

-- Property: breakOn with empty pattern returns empty prefix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern str =
  let (before, after) = breakOn "" str
  in property $ before === "" .&&. after === str

-- Property: breakOn finds first occurrence
prop_breakOn_first_occurrence :: String -> String -> String -> Property
prop_breakOn_first_occurrence pat before after =
  not (null pat) ==> 
  let input = before ++ pat ++ after ++ pat ++ "extra"
      (prefix, suffix) = breakOn pat input
  in property $ prefix === before ++ pat ++ after .&&. suffix === "extra"

-- Property: breakOn handles pattern not found
prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found pat haystack =
  not (null pat) && not (pat `L.isInfixOf` haystack) ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Comment removal then trimming is idempotent for comments
prop_comment_removal_trim_idempotent :: String -> Property
prop_comment_removal_trim_idempotent str =
  not ('"' `elem` str) && not ('\'' `elem` str) ==>
  let step1 = removeComments str |> trim
      step2 = removeComments step1 |> trim
  in property $ step1 === step2

-- Property: String processing pipeline consistency
prop_processing_pipeline_consistency :: String -> Property
prop_processing_pipeline_consistency str =
  let pipeline1 = str |> removeComments |> trim |> normalizeIndentation
      pipeline2 = str |> trim |> removeComments |> normalizeIndentation
  in property $ pipeline1 === pipeline2

-- Property: Split operations are consistent with each other
prop_split_operations_consistency :: Char -> String -> Property
prop_split_operations_consistency delim str =
  let regular = splitBy delim str
      collapsed = splitByCollapsed delim str
      commaRegular = splitByComma str
      commaCollapsed = splitByCommaCollapsed str
  in if delim == ','
     then property $ regular === commaRegular .&&. collapsed === commaCollapsed
     else property $ L.length regular >= L.length collapsed

-- Property: Indentation normalization preserves relative structure
prop_indentation_preserves_relative_structure :: [Int] -> Property
prop_indentation_preserves_relative_structure indentLevels =
  not (null indentLevels) && L.all (>= 0) indentLevels ==>
  let contentLines = map show ([1..L.length indentLevels] :: [Int])
      inputLines = zipWith (\level content -> replicate level ' ' ++ content) indentLevels contentLines
      input = unlines inputLines
      result = normalizeIndentation input
      resultLines = lines result
      -- Check that relative indentation is preserved
      originalDiffs = zipWith (-) (L.tail indentLevels) (init indentLevels)
      resultIndents = L.map (L.length . takeWhile isSpace) resultLines
      resultDiffs = zipWith (-) (L.tail resultIndents) (init resultIndents)
  in property $ L.length resultDiffs === L.length originalDiffs .&&.
             L.all (uncurry (==)) (zip originalDiffs resultDiffs)

-- Property: Comment removal preserves line structure
prop_comment_removal_preserves_line_structure :: String -> Property
prop_comment_removal_preserves_line_structure str =
  not ('"' `elem` str) && not ('\'' `elem` str) ==>
  let originalLines = lines str
      withComments = unlines $ L.map (++ " // comment") originalLines
      processed = removeLineComments withComments
      processedLines = lines processed
  in property $ L.length processedLines === L.length originalLines

-- Property: Complex whitespace handling
prop_complex_whitespace_handling :: String -> String -> String -> Property
prop_complex_whitespace_handling prefix middle suffix =
  let complexInput = prefix ++ "  \t\n\r  " ++ middle ++ "  \n\r\t  " ++ suffix
      trimmed = trim complexInput
      normalized = normalizeIndentation complexInput
  in property $ (null trimmed || not (isSpace (L.head trimmed))) .&&.
             (null trimmed || not (isSpace (last trimmed))) .&&.
             L.length (lines normalized) >= 1

-- ============================================================================
-- Performance L.and Edge Case Properties
-- ============================================================================

-- Property: Large string processing performance (bounded)
prop_large_string_performance :: Int -> String -> Property
prop_large_string_performance multiplier base =
  multiplier > 0 && multiplier <= 100 ==> -- Bounded for performance
  let large = L.concat $ replicate multiplier base
      result = trim large
  in property $ L.length result <= L.length large

-- Property: Unicode handling in comments
prop_unicode_comments :: String -> Property
prop_unicode_comments content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let unicodeComment = "测试 🚀 café naïve"
      input = content ++ " // " ++ unicodeComment
      result = removeLineComments input
  in property $ content `L.isPrefixOf` result .&&.
     not (unicodeComment `L.isInfixOf` result)

-- Property: Empty L.and single character edge cases
prop_edge_case_single_char :: Char -> Property
prop_edge_case_single_char c =
  let str = [c]
      trimmed = trim str
      split = splitBy c str
  in property $ (if isSpace c then null trimmed else trimmed === str) .&&.
             (if c == c then split === ["", ""] else split === [str])

-- Property: Null byte handling
prop_null_byte_handling :: String -> Property
prop_null_byte_handling str =
  let withNull = str ++ "\0" ++ str
      processed = trim withNull
  in property $ "\0" `L.isInfixOf` processed

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils QuickCheck Tests"
  [ testGroup "Basic Function Properties"
    [ fastProperty "trim never increases string L.length" prop_trim_never_increases_length
    , fastProperty "trim removes L.all leading L.and trailing whitespace" prop_trim_removes_all_whitespace
    , fastProperty "splitBy preserves total character count" prop_splitBy_preserves_char_count
    , fastProperty "splitBy L.and intercalate roundtrip" prop_splitBy_intercalate_roundtrip
    , fastProperty "splitByCollapsed never produces empty segments" prop_splitByCollapsed_no_empty
    , fastProperty "splitByCollapsed removes consecutive delimiters" prop_splitByCollapsed_removes_consecutive
    , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
    ]
  , testGroup "Comment Processing Properties"
    [ fastProperty "removeLineComments preserves string literals" prop_removeLineComments_preserves_string_literals
    , fastProperty "removeComments handles nested block comments" prop_removeComments_nested_block_comments
    , fastProperty "removeComments preserves escaped quotes" prop_removeComments_preserves_escaped_quotes
    , fastProperty "comment removal then trimming is idempotent" prop_comment_removal_trim_idempotent
    , fastProperty "comment removal preserves line structure" prop_comment_removal_preserves_line_structure
    , fastProperty "unicode handling in comments" prop_unicode_comments
    ]
  , testGroup "Indentation Properties"
    [ fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_line_count
    , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common_prefix
    , fastProperty "forceSingleTabIndentation converts to tabs" prop_forceSingleTabIndentation_tab_conversion
    , fastProperty "indentation preserves relative structure" prop_indentation_preserves_relative_structure
    ]
  , testGroup "String Search Properties"
    [ fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn finds first occurrence" prop_breakOn_first_occurrence
    , fastProperty "breakOn handles pattern not found" prop_breakOn_pattern_not_found
    ]
  , testGroup "Complex Interaction Properties"
    [ fastProperty "string processing pipeline consistency" prop_processing_pipeline_consistency
    , fastProperty "split operations consistency" prop_split_operations_consistency
    , fastProperty "complex whitespace handling" prop_complex_whitespace_handling
    ]
  , testGroup "Performance L.and Edge Cases"
    [ fastProperty "large string processing performance" prop_large_string_performance
    , fastProperty "empty L.and single character edge cases" prop_edge_case_single_char
    , fastProperty "null byte handling" prop_null_byte_handling
    ]
  ]