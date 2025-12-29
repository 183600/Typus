{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewStringProcessingBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

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

import Data.Char (isSpace, isControl, isAscii, ord, chr)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)

-- ============================================================================
-- New String Processing Boundary Tests
-- ============================================================================

-- Property: trim handles all whitespace characters correctly
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace input =
  let allWhitespace = all isSpace input
      trimmed = trim input
  in classify allWhitespace "all whitespace" $
     property $ (if allWhitespace then trimmed else input) === trimmed

-- Property: trim preserves non-whitespace characters exactly
prop_trim_preserves_non_whitespace :: String -> Property
prop_trim_preserves_non_whitespace input =
  let nonWhitespace = filter (not . isSpace) input
      trimmed = trim input
      trimmedNonWhitespace = filter (not . isSpace) trimmed
  in property $ nonWhitespace === trimmedNonWhitespace

-- Property: splitBy handles Unicode characters correctly
prop_splitBy_unicode_integrity :: String -> Char -> Property
prop_splitBy_unicode_integrity input delim =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === input

-- Property: splitByCollapsed never produces empty consecutive segments
prop_splitByCollapsed_no_consecutive_empty :: Char -> String -> Property
prop_splitByCollapsed_no_consecutive_empty delim input =
  let parts = splitByCollapsed delim input
      consecutiveEmpty = any (\(a, b) -> null a && null b) (zip parts (tail parts))
  in property $ not consecutiveEmpty

-- Property: removeLineComments handles nested quotes correctly
prop_removeLineComments_nested_quotes :: String -> String -> Property
prop_removeLineComments_nested_quotes before after =
  let complex = before ++ "var s = \"He said \\\"// not a comment\\\"\" // real comment\n" ++ after
      processed = removeLineComments complex
  in not ("\"" `isInfixOf` before) && not ("\"" `isInfixOf` after) ==>
     property $ "// not a comment" `isInfixOf` processed .&&.
     not ("// real comment" `isInfixOf` processed)

-- Property: removeComments handles edge case of comment-like sequences in strings
prop_removeComments_comment_like_strings :: String -> Property
prop_removeComments_comment_like_strings content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let withCommentLikeStrings = "var s = \"/* not comment */\" // line comment\nvar t = \"// not comment\""
      processed = removeComments withCommentLikeStrings
  in property $ "/* not comment */" `isInfixOf` processed .&&.
     "// not comment" `isInfixOf` processed .&&.
     not ("// line comment" `isInfixOf` processed)

-- Property: normalizeIndentation preserves the number of non-empty lines
prop_normalizeIndentation_preserves_line_count :: String -> Property
prop_normalizeIndentation_preserves_line_count input =
  let originalLines = filter (not . all isSpace) (lines input)
      normalized = normalizeIndentation input
      normalizedLines = filter (not . all isSpace) (lines normalized)
  in property $ length originalLines === length normalizedLines

-- Property: forceSingleTabIndentation makes all non-empty lines start with tab
prop_forceSingleTabIndentation_all_tabs :: String -> Property
prop_forceSingleTabIndentation_all_tabs input =
  let processed = forceSingleTabIndentation input
      nonEmptyLines = filter (not . null . trim) (lines processed)
  in property $ all ("\t" `isPrefixOf`) nonEmptyLines

-- Property: breakOn handles pattern at various positions correctly
prop_breakOn_position_accuracy :: String -> String -> String -> Property
prop_breakOn_position_accuracy prefix pattern suffix =
  not (null pattern) ==>
  let full = prefix ++ pattern ++ suffix
      (before, after) = breakOn pattern full
  in property $ before ++ pattern ++ after === full

-- Property: String processing functions are idempotent where expected
prop_string_functions_idempotent :: String -> Property
prop_string_functions_idempotent input =
  let trimmed1 = trim input
      trimmed2 = trim trimmed1
      normalized1 = normalizeIndentation input
      normalized2 = normalizeIndentation normalized1
  in property $ trimmed1 === trimmed2 .&&. normalized1 === normalized2

-- Property: splitBy and splitByCollapsed relationship for delimiters
prop_splitBy_vs_splitByCollapsed :: Char -> String -> Property
prop_splitBy_vs_splitByCollapsed delim input =
  let regular = splitBy delim input
      collapsed = splitByCollapsed delim input
      regularHasEmpty = "" `elem` regular
  in classify regularHasEmpty "has empty segments" $
     property $ if regularHasEmpty 
                then length collapsed < length regular
                else regular === collapsed

-- Property: removeComments preserves string literals with escaped comment markers
prop_removeComments_preserves_escaped_comments :: String -> Property
prop_removeComments_preserves_escaped_comments content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let escaped = "var s = \"/* not comment */ \\\\// escaped\" // real comment"
      processed = removeComments escaped
  in property $ "/* not comment */" `isInfixOf` processed .&&.
     "\\\\// escaped" `isInfixOf` processed .&&.
     not ("// real comment" `isInfixOf` processed)

-- Property: Complex string processing pipeline maintains invariants
prop_complex_pipeline_invariants :: String -> Property
prop_complex_pipeline_invariants input =
  let step1 = removeComments input
      step2 = trim step1
      step3 = normalizeIndentation step2
  in property $ length step3 <= length step2 .&&. length step2 <= length step1

-- Property: breakOn with empty pattern behaves correctly
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern input =
  let (before, after) = breakOn "" input
  in property $ before === "" .&&. after === input

-- Property: splitByComma and splitBy consistency
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency input =
  splitByComma input === splitBy ',' input

-- Property: removeLineComments preserves newlines structure
prop_removeLineComments_preserves_newlines :: String -> Property
prop_removeLineComments_preserves_newlines input =
  let originalNewlines = length (filter (== '\n') input)
      processed = removeLineComments input
      processedNewlines = length (filter (== '\n') processed)
  in property $ processedNewlines === originalNewlines

-- Property: String functions handle control characters
prop_string_functions_control_chars :: String -> Property
prop_string_functions_control_chars input =
  let withControl = input ++ "\1\2\3\4\5"
      trimmed = trim withControl
      split = splitBy ',' withControl
  in property $ not (null trimmed) ==> not (any isControl (take 1 trimmed))

-- Property: Unicode normalization in string processing
prop_unicode_normalization :: String -> Property
prop_unicode_normalization input =
  let unicodeInput = input ++ "café naïve résumé 🚀 测试"
      processed = removeLineComments unicodeInput
  in property $ "café" `isInfixOf` processed .&&.
     "naïve" `isInfixOf` processed .&&.
     "résumé" `isInfixOf` processed .&&.
     "🚀" `isInfixOf` processed .&&.
     "测试" `isInfixOf` processed

-- Property: Edge case with very long strings
prop_very_long_strings :: Int -> String -> Property
prop_very_long_strings multiplier base =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let longString = concat (replicate multiplier base)
      processed = trim longString
  in property $ length processed <= length longString

-- Property: String processing with null bytes
prop_null_bytes_handling :: String -> Property
prop_null_bytes_handling input =
  not ('\0' `elem` input) ==>
  let withNull = input ++ "\0" ++ input
      processed = trim withNull
  in property $ "\0" `isInfixOf` processed

-- Property: Complex comment scenarios
prop_complex_comment_scenarios :: String -> String -> String -> Property
prop_complex_comment_scenarios before middle after =
  not ('"' `elem` before) && not ('"' `elem` middle) && not ('"' `elem` after) &&
  not ('\'' `elem` before) && not ('\'' `elem` middle) && not ('\'' `elem` after) ==>
  let complex = before ++ "/* block */" ++ middle ++ "// line\n" ++ after ++ "/* another */"
      processed = removeComments complex
  in property $ not ("/* block */" `isInfixOf` processed) .&&.
     not ("// line" `isInfixOf` processed) .&&.
     not ("/* another */" `isInfixOf` processed) .&&.
     (before `isInfixOf` processed) .&&.
     (middle `isInfixOf` processed) .&&.
     (after `isInfixOf` processed)

-- Property: Indentation normalization with mixed tabs and spaces
prop_mixed_indentation_normalization :: [Int] -> Property
prop_mixed_indentation_normalization indentLevels =
  not (null indentLevels) ==>
  let lines' = zipWith (\level content -> 
                       let spaces = replicate (abs level `mod` 10) ' '
                           tabs = replicate (abs level `mod` 5) '\t'
                       in spaces ++ tabs ++ "content") 
                       indentLevels (map show [1..])
      input = unlines lines'
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      nonEmptyLines = filter (not . null) normalizedLines
  in property $ all (not . isPrefixOf "    ") nonEmptyLines

-- Tests collection
tests :: TestTree
tests = testGroup "New String Processing Boundary Tests"
  [ fastProperty "trim handles all whitespace characters" prop_trim_all_whitespace
  , fastProperty "trim preserves non-whitespace characters exactly" prop_trim_preserves_non_whitespace
  , fastProperty "splitBy handles Unicode characters correctly" prop_splitBy_unicode_integrity
  , fastProperty "splitByCollapsed never produces empty consecutive segments" prop_splitByCollapsed_no_consecutive_empty
  , fastProperty "removeLineComments handles nested quotes correctly" prop_removeLineComments_nested_quotes
  , fastProperty "removeComments handles edge case of comment-like sequences in strings" prop_removeComments_comment_like_strings
  , fastProperty "normalizeIndentation preserves the number of non-empty lines" prop_normalizeIndentation_preserves_line_count
  , fastProperty "forceSingleTabIndentation makes all non-empty lines start with tab" prop_forceSingleTabIndentation_all_tabs
  , fastProperty "breakOn handles pattern at various positions correctly" prop_breakOn_position_accuracy
  , fastProperty "String processing functions are idempotent where expected" prop_string_functions_idempotent
  , fastProperty "splitBy vs splitByCollapsed relationship for delimiters" prop_splitBy_vs_splitByCollapsed
  , fastProperty "removeComments preserves string literals with escaped comment markers" prop_removeComments_preserves_escaped_comments
  , fastProperty "Complex string processing pipeline maintains invariants" prop_complex_pipeline_invariants
  , fastProperty "breakOn with empty pattern behaves correctly" prop_breakOn_empty_pattern
  , fastProperty "splitByComma and splitBy consistency" prop_splitByComma_consistency
  , fastProperty "removeLineComments preserves newlines structure" prop_removeLineComments_preserves_newlines
  , fastProperty "String functions handle control characters" prop_string_functions_control_chars
  , fastProperty "Unicode normalization in string processing" prop_unicode_normalization
  , fastProperty "Edge case with very long strings" prop_very_long_strings
  , fastProperty "String processing with null bytes" prop_null_bytes_handling
  , fastProperty "Complex comment scenarios" prop_complex_comment_scenarios
  , fastProperty "Indentation normalization with mixed tabs and spaces" prop_mixed_indentation_normalization
  ]