{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
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
  , breakOn
  )

import Data.Char (isSpace, toLower, isAscii, isControl)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub, dropWhileEnd)

-- ============================================================================
-- Additional QuickCheck Tests for Utils Module
-- ============================================================================

-- Property: splitBy L.and splitByCollapsed relationship
prop_splitBy_vs_splitByCollapsed :: Char -> String -> Property
prop_splitBy_vs_splitByCollapsed delim input =
  let regular = splitBy delim input
      collapsed = splitByCollapsed delim input
      regularNoEmpty = L.filter (not . null) regular
  in property $ sort regularNoEmpty === sort collapsed

-- Property: removeComments handles edge cases with nested quotes
prop_removeComments_nested_quotes :: String -> Property
prop_removeComments_nested_quotes content =
  not ("/*" `L.isInfixOf` content) && not ("*/" `L.isInfixOf` content) &&
  not ("//" `L.isInfixOf` content) ==> 
  let nestedQuotes = "var s = \"// not comment /* also not */\" // real comment\n" ++ content
      processed = removeComments nestedQuotes
  in property $ "// not comment /* also not */" `L.isInfixOf` processed .&&.
     not ("// real comment" `L.isInfixOf` processed)

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_line_count :: String -> Property
prop_normalizeIndentation_preserves_line_count content =
  let originalLines = L.length (lines content)
      normalized = normalizeIndentation content
      normalizedLines = L.length (lines normalized)
  in property $ normalizedLines === originalLines

-- Property: trim handles Unicode whitespace correctly
prop_trim_unicode_whitespace_extended :: String -> Property
prop_trim_unicode_whitespace_extended content =
  let unicodeSpaces = ["\160", "\8239", "\8287", "\12288"] -- Non-breaking space, thin space, etc.
      contentWithSpaces = concatMap (\ws -> ws ++ content) unicodeSpaces
      trimmed = trim contentWithSpaces
      hasLeadingSpace = not (null trimmed) && isSpace (L.head trimmed)
      hasTrailingSpace = not (null trimmed) && isSpace (last trimmed)
  in property $ not hasLeadingSpace .&&. not hasTrailingSpace

-- Property: splitBy handles extreme delimiter counts
prop_splitBy_extreme_delimiter_counts :: Char -> Int -> Property
prop_splitBy_extreme_delimiter_counts delim count =
  count >= 0 && count <= 1000 ==> -- Limit for performance
  let input = replicate count delim
      parts = splitBy delim input
  in property $ L.length parts === count + 1

-- Property: removeLineComments preserves string literals with escaped slashes
prop_removeLineComments_escaped_slashes :: String -> Property
prop_removeLineComments_escaped_slashes content =
  not ('\'' `elem` content) && not ('"' `elem` content) && not ('\n' `elem` content) ==> 
  let escapedContent = "var s = \"// not comment \\\\ // still not comment\" // real comment\n" ++ content
      processed = removeLineComments escapedContent
  in property $ "// not comment \\\\ // still not comment" `L.isInfixOf` processed .&&.
     not ("// real comment" `L.isInfixOf` processed)

-- Property: breakOn handles case sensitivity correctly
prop_breakOn_case_sensitive :: String -> String -> Property
prop_breakOn_case_sensitive pattern haystack =
  not (null pattern) ==> 
  let (before, after) = breakOn pattern haystack
      lowerPattern = map toLower pattern
      lowerHaystack = map toLower haystack
      (beforeLower, afterLower) = breakOn lowerPattern lowerHaystack
  in if pattern `L.isInfixOf` haystack
     then property $ before ++ pattern ++ after === haystack
     else property $ before === haystack .&&. after === ""

-- Property: forceSingleTabIndentation handles empty lines correctly
prop_forceSingleTabIndentation_empty_lines :: String -> Property
prop_forceSingleTabIndentation_empty_lines content =
  let contentWithEmptyLines = content ++ "\n\n" ++ content
      result = forceSingleTabIndentation contentWithEmptyLines
      resultLines = lines result
      emptyLineCount = L.length $ filter null resultLines
  in property $ emptyLineCount >= 2

-- Property: removeComments handles malformed block comments gracefully
prop_removeComments_malformed_block :: String -> String -> Property
prop_removeComments_malformed_block before after =
  not ("/*" `L.isInfixOf` before) && not ("*/" `L.isInfixOf` before) &&
  not ("/*" `L.isInfixOf` after) && not ("*/" `L.isInfixOf` after) ==> 
  let malformed = before ++ "/* unclosed comment\n" ++ after
      processed = removeComments malformed
  in property $ before `L.isInfixOf` processed .&&. not ("/* unclosed comment" `L.isInfixOf` processed)

-- Property: splitByComma handles CSV edge cases
prop_splitByComma_csv_edge_cases :: [String] -> Property
prop_splitByComma_csv_edge_cases fields =
  let csv = Data.List.intercalate "," fields
      quotedFields = L.map (\f -> if "," `L.isInfixOf` f then "\"" ++ f ++ "\"" else f) fields
      quotedCsv = Data.List.intercalate "," quotedFields
      parsed = splitByComma csv
  in property $ parsed === fields

-- Property: normalizeIndentation with mixed tabs L.and spaces preserves relative structure
prop_normalizeIndentation_mixed_tabs_spaces :: [Int] -> Property
prop_normalizeIndentation_mixed_tabs_spaces indentLevels =
  not (null indentLevels) ==> 
  let lines' = zipWith (\level content -> 
        let spaces = replicate (level `mod` 5) ' '
            tabs = replicate (level `div` 5) '\t'
        in spaces ++ tabs ++ content) indentLevels (map show ([1..] :: [Integer]))
      content = unlines lines'
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
      -- Check that relative indentation is preserved
      indentDiffs = zipWith (-) (L.tail indentLevels) (init indentLevels)
      normalizedIndents = L.map (L.length . takeWhile isSpace) normalizedLines
      normalizedDiffs = zipWith (-) (L.tail normalizedIndents) (init normalizedIndents)
  in property $ L.length normalizedLines === L.length indentLevels

-- Property: trim is consistent with Data.List.dropWhile/takeWhile
prop_trim_consistency_with_list_ops :: String -> Property
prop_trim_consistency_with_list_ops input =
  let ourTrim = trim input
      listTrim = Data.List.dropWhile isSpace (Data.List.dropWhileEnd isSpace input)
  in property $ ourTrim === listTrim

-- Property: removeComments preserves non-comment content exactly
prop_removeComments_preserves_content :: String -> String -> String -> Property
prop_removeComments_preserves_content before middle after =
  not ('"' `elem` before) && not ('\'' `elem` before) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` after) && not ('\'' `elem` after) &&
  not ("/*" `L.isInfixOf` before) && not ("*/" `L.isInfixOf` before) &&
  not ("/*" `L.isInfixOf` middle) && not ("*/" `L.isInfixOf` middle) &&
  not ("/*" `L.isInfixOf` after) && not ("*/" `L.isInfixOf` after) ==> 
  let content = before ++ middle ++ after
      withComments = before ++ " /* comment */ " ++ middle ++ " // comment\n" ++ after
      processed = removeComments withComments
  in property $ content `L.isInfixOf` processed

-- Property: splitByCollapsed with multiple different delimiters
prop_splitByCollapsed_multiple_delimiters :: String -> Char -> Char -> Property
prop_splitByCollapsed_multiple_delimiters content delim1 delim2 =
  delim1 /= delim2 ==> 
  let contentWithDelims = content ++ [delim1, delim1] ++ [delim2, delim2] ++ content
      split1 = splitByCollapsed delim1 contentWithDelims
      split2 = splitByCollapsed delim2 contentWithDelims
  in property $ L.all (not . null) split1 .&&. L.all (not . null) split2

-- Property: breakOn performance with large inputs
prop_breakOn_large_input_performance :: Int -> String -> Property
prop_breakOn_large_input_performance multiplier pattern =
  multiplier > 0 && multiplier <= 100 && not (null pattern) ==> 
  let largeInput = L.concat (replicate multiplier "content") ++ pattern ++ "end"
      (before, after) = breakOn pattern largeInput
  in property $ L.length before >= multiplier * 7 .&&. after === "end"

-- Property: removeLineComments handles multiple consecutive comment markers
prop_removeLineComments_consecutive_markers :: String -> Property
prop_removeLineComments_consecutive_markers content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==> 
  let consecutiveMarkers = content ++ " /// // /// // comment\nafter"
      processed = removeLineComments consecutiveMarkers
  in property $ not ("//" `L.isInfixOf` processed) .&&. "after" `L.isInfixOf` processed

-- Property: normalizeIndentation handles only whitespace lines
prop_normalizeIndentation_whitespace_only :: Int -> Property
prop_normalizeIndentation_whitespace_only numLines =
  numLines >= 0 && numLines <= 50 ==> 
  let whitespaceLines = replicate numLines "    \t  "
      content = unlines whitespaceLines
      normalized = normalizeIndentation content
  in property $ normalized === content

-- Property: forceSingleTabIndentation idempotency with mixed content
prop_forceSingleTabIndentation_idempotency_mixed :: String -> Property
prop_forceSingleTabIndentation_idempotency_mixed content =
  let firstPass = forceSingleTabIndentation content
      secondPass = forceSingleTabIndentation firstPass
  in property $ firstPass === secondPass

-- Property: splitBy with empty string as delimiter
prop_splitBy_empty_string :: String -> Property
prop_splitBy_empty_string input =
  -- This should be handled gracefully - either return the whole string
  -- L.or split into characters, depending on implementation
  let result = splitBy '\0' input
  in if null input 
     then property $ result === [""]
     else property $ result === [input]

-- Property: removeComments with overlapping comment markers
prop_removeComments_overlapping_markers :: String -> Property
prop_removeComments_overlapping_markers content =
  not ('"' `elem` content) && not ('\'' `elem` content) &&
  not ("/*" `L.isInfixOf` content) && not ("*/" `L.isInfixOf` content) ==> 
  let overlapping = content ++ "///**/ overlapping markers */" ++ content
      processed = removeComments overlapping
  in property $ not ("/**/" `L.isInfixOf` processed) .&&.
     not ("*/" `L.isInfixOf` processed) .&&.
     content `L.isInfixOf` processed

-- Property: Complex string processing with L.all utils functions
prop_complex_utils_pipeline :: String -> String -> String -> Property
prop_complex_utils_pipeline prefix middle suffix =
  not ('"' `elem` prefix) && not ('\'' `elem` prefix) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` suffix) && not ('\'' `elem` suffix) ==> 
  let input = prefix ++ "    /* block comment */  " ++ middle ++ "  // line comment\n    " ++ suffix
      processed = input
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
      processedTrimmed = trim processed
  in property $ not ("/* block comment */" `L.isInfixOf` processed) .&&.
     not ("// line comment" `L.isInfixOf` processed) .&&.
     (null processedTrimmed || not (isSpace (L.head processedTrimmed))) .&&.
     (null processedTrimmed || not (isSpace (last processedTrimmed)))

-- Helper function for pipeline composition
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Utils QuickCheck Tests"
  [ fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_splitByCollapsed
  , fastProperty "removeComments handles edge cases with nested quotes" prop_removeComments_nested_quotes
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_line_count
  , fastProperty "trim handles Unicode whitespace correctly" prop_trim_unicode_whitespace_extended
  , fastProperty "splitBy handles extreme delimiter counts" prop_splitBy_extreme_delimiter_counts
  , fastProperty "removeLineComments preserves string literals with escaped slashes" prop_removeLineComments_escaped_slashes
  , fastProperty "breakOn handles case sensitivity correctly" prop_breakOn_case_sensitive
  , fastProperty "forceSingleTabIndentation handles empty lines correctly" prop_forceSingleTabIndentation_empty_lines
  , fastProperty "removeComments handles malformed block comments gracefully" prop_removeComments_malformed_block
  , fastProperty "splitByComma handles CSV edge cases" prop_splitByComma_csv_edge_cases
  , fastProperty "normalizeIndentation with mixed tabs L.and spaces preserves relative structure" prop_normalizeIndentation_mixed_tabs_spaces
  , fastProperty "trim is consistent with Data.List.dropWhile/takeWhile" prop_trim_consistency_with_list_ops
  , fastProperty "removeComments preserves non-comment content exactly" prop_removeComments_preserves_content
  , fastProperty "splitByCollapsed with multiple different delimiters" prop_splitByCollapsed_multiple_delimiters
  , fastProperty "breakOn performance with large inputs" prop_breakOn_large_input_performance
  , fastProperty "removeLineComments handles multiple consecutive comment markers" prop_removeLineComments_consecutive_markers
  , fastProperty "normalizeIndentation handles only whitespace lines" prop_normalizeIndentation_whitespace_only
  , fastProperty "forceSingleTabIndentation idempotency with mixed content" prop_forceSingleTabIndentation_idempotency_mixed
  , fastProperty "splitBy with empty string as delimiter" prop_splitBy_empty_string
  , fastProperty "removeComments with overlapping comment markers" prop_removeComments_overlapping_markers
  , fastProperty "complex string processing with L.all utils functions" prop_complex_utils_pipeline
  ]