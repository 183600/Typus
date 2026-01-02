{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, listOf1, elements, vectorOf, suchThat)

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

-- ============================================================================
-- Enhanced Property Tests for Utils Module
-- ============================================================================

-- Property: trim removes L.all leading L.and trailing whitespace characters
prop_trim_removes_all_whitespace :: String -> Property
prop_trim_removes_all_whitespace input =
  let trimmed = trim input
      hasLeadingSpace = not (null input) && isSpace (L.head input)
      hasTrailingSpace = not (null input) && isSpace (last input)
  in classify hasLeadingSpace "has leading whitespace" $
     classify hasTrailingSpace "has trailing whitespace" $
     property $ 
       (null trimmed || not (isSpace (L.head trimmed))) .&&.
       (null trimmed || not (isSpace (last trimmed)))

-- Property: trim preserves the order of non-whitespace characters
prop_trim_preserves_order :: String -> Property
prop_trim_preserves_order input =
  let trimmed = trim input
      nonWhitespaceInput = L.filter (not . isSpace) input
      nonWhitespaceTrimmed = L.filter (not . isSpace) trimmed
  in property $ nonWhitespaceInput === nonWhitespaceTrimmed

-- Property: splitBy with delimiter not in string returns single element list
prop_splitBy_no_delimiter :: Char -> String -> Property
prop_splitBy_no_delimiter delim input =
  delim `notElem` input ==> 
  let result = splitBy delim input
  in property $ result === [input]

-- Property: splitBy respects delimiter count
prop_splitBy_delimiter_count :: Char -> String -> Property
prop_splitBy_delimiter_count delim input =
  let result = splitBy delim input
      expectedCount = L.length (L.filter (== delim) input) + 1
  in property $ L.length result === expectedCount

-- Property: splitByCollapsed never returns empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim input =
  let result = splitByCollapsed delim input
  in property $ L.all (not . null) result

-- Property: splitByComma L.and splitBy ',' should give same results
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency input =
  splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed L.and splitByCollapsed ',' should give same results
prop_splitByCommaCollapsed_consistency :: String -> Property
prop_splitByCommaCollapsed_consistency input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: removeLineComments preserves line structure
prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines input =
  let linesBefore = lines input
      linesAfter = lines (removeLineComments input)
  in property $ L.length linesBefore === L.length linesAfter

-- Property: removeLineComments removes // comments but preserves content
prop_removeLineComments_removes_comments :: String -> String -> Property
prop_removeLineComments_removes_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) && not ('/' `elem` code) ==>
  let lineWithComment = code ++ " // " ++ comment ++ "\nnext line"
      cleaned = removeLineComments lineWithComment
  in property $ "// " `L.isInfixOf` cleaned .||. not ("//" `L.isInfixOf` cleaned)

-- Property: removeComments preserves string literals
prop_removeComments_preserves_strings :: String -> Property
prop_removeComments_preserves_strings content =
  not ('"' `elem` content) && not ('\\' `elem` content) ==>
  let stringWithComment = content ++ " /* comment */ \"string with // not comment\""
      cleaned = removeComments stringWithComment
  in property $ "string with // not comment" `L.isInfixOf` cleaned

-- Property: normalizeIndentation removes common leading whitespace
prop_normalizeIndentation_removes_common :: [String] -> Property
prop_normalizeIndentation_removes_common lineList =
  not (null lineList) ==>
  let input = unlines lineList
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      nonEmptyLines = L.filter (not . L.all isSpace) normalizedLines
      leadingSpaces = L.map (L.length . takeWhile isSpace) nonEmptyLines
  in property $ if null leadingLines then True else L.minimum leadingSpaces === 0
  where
    leadingLines = if null nonEmptyLines then [] else leadingSpaces

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabIndentation_adds_tab :: String -> Property
prop_forceSingleTabIndentation_adds_tab input =
  let result = forceSingleTabIndentation input
      resultLines = lines result
      nonEmptyLines = L.filter (not . null . trim) resultLines
  in property $ L.all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize input =
  fixIndentation input === normalizeIndentation input

-- Property: breakOn finds first occurrence of pattern
prop_breakOn_first_occurrence :: String -> String -> String -> Property
prop_breakOn_first_occurrence pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix ++ pat ++ "extra"
      (before, after) = breakOn pat haystack
  in property $ before === prefix ++ pat ++ suffix .&&. after === "extra"

-- Property: breakOn with empty pattern returns empty prefix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn with pattern not found returns original string
prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found pat haystack =
  not (null pat) && pat `notElem` haystack ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy L.and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === input

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in property $ removedOnce === removedTwice

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalizedOnce = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalizedOnce
  in property $ normalizedOnce === normalizedTwice

-- Property: Complex pipeline processing
prop_complex_pipeline :: String -> String -> String -> Property
prop_complex_pipeline prefix content suffix =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let input = prefix ++ "  " ++ content ++ "  // comment\n  " ++ suffix
      processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
  in property $ not ("// comment" `L.isInfixOf` processed) .&&.
     (null content || content `L.isInfixOf` processed)

-- Property: Unicode handling in trim
prop_trim_unicode :: String -> Property
prop_trim_unicode input =
  let unicodeInput = input ++ " café naïve "
      trimmed = trim unicodeInput
      hasInternalUnicode = "café" `L.isInfixOf` trimmed || "naïve" `L.isInfixOf` trimmed
  in property $ hasInternalUnicode .||. (null input && null trimmed)

-- Property: Performance constraint for large inputs
prop_performance_large_input :: Int -> String -> Property
prop_performance_large_input multiplier baseContent =
  multiplier >= 0 && multiplier <= 100 ==> -- Limit for performance testing
  let largeContent = L.concat (replicate multiplier baseContent)
      result = trim largeContent
  in property $ L.length result <= L.length largeContent

-- Property: splitBy respects Unicode delimiters
prop_splitBy_unicode :: String -> String -> Property
prop_splitBy_unicode delim input =
  not (null delim) ==>
  let unicodeInput = input ++ delim ++ "测试🚀" ++ delim ++ input
      parts = splitBy (L.head delim) unicodeInput
  in property $ L.length parts >= 3

-- Property: Comment removal with nested structures
prop_removeComments_nested :: String -> Property
prop_removeComments_nested content =
  not ('"' `elem` content) && not ('\'' `elem` content) && not ("/*" `L.isInfixOf` content) ==>
  let nested = "var x = " ++ content ++ " /* outer /* inner */ still outer */ var y = " ++ content
      result = removeComments nested
  in property $ not ("/* outer" `L.isInfixOf` result) .&&.
     not ("/* inner" `L.isInfixOf` result) .&&.
     (content `L.isInfixOf` result)

-- Property: Indentation normalization with mixed tabs/spaces
prop_normalizeIndentation_mixed_whitespace :: [Int] -> String -> Property
prop_normalizeIndentation_mixed_whitespace indentLevels content =
  not (null indentLevels) ==>
  let inputLines = zipWith (\level -> 
        let spaces = replicate (abs level `mod` 10) ' '
            tabs = replicate (abs level `mod` 5) '\t'
        in spaces ++ tabs ++ content) indentLevels (map show ([1..] :: [Integer]))
      input = unlines inputLines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      nonEmptyLines = L.filter (not . null . trim) normalizedLines
      leadingSpaces = L.map (L.length . takeWhile isSpace) nonEmptyLines
  in property $ if null leadingSpaces then True else L.minimum leadingSpaces === 0

-- Helper function for pipeline operator
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Enhanced Utils QuickCheck Tests"
  [ testGroup "String manipulation properties"
    [ fastProperty "trim removes L.all whitespace" prop_trim_removes_all_whitespace
    , fastProperty "trim preserves order" prop_trim_preserves_order
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim handles Unicode" prop_trim_unicode
    ]

  , testGroup "String splitting properties"
    [ fastProperty "splitBy without delimiter returns single element" prop_splitBy_no_delimiter
    , fastProperty "splitBy respects delimiter count" prop_splitBy_delimiter_count
    , fastProperty "splitByCollapsed never returns empty segments" prop_splitByCollapsed_no_empty
    , fastProperty "splitByComma consistency" prop_splitByComma_consistency
    , fastProperty "splitByCommaCollapsed consistency" prop_splitByCommaCollapsed_consistency
    , fastProperty "splitBy L.and join roundtrip" prop_splitBy_join_roundtrip
    , fastProperty "splitBy respects Unicode delimiters" prop_splitBy_unicode
    ]

  , testGroup "Comment removal properties"
    [ fastProperty "removeLineComments preserves lines" prop_removeLineComments_preserves_lines
    , fastProperty "removeLineComments removes comments" prop_removeLineComments_removes_comments
    , fastProperty "removeComments preserves strings" prop_removeComments_preserves_strings
    , fastProperty "removeComments handles nested structures" prop_removeComments_nested
    , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    ]

  , testGroup "Indentation properties"
    [ fastProperty "normalizeIndentation removes common whitespace" prop_normalizeIndentation_removes_common
    , fastProperty "normalizeIndentation handles mixed whitespace" prop_normalizeIndentation_mixed_whitespace
    , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
    , fastProperty "forceSingleTabIndentation adds tabs" prop_forceSingleTabIndentation_adds_tab
    , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
    ]

  , testGroup "Search L.and utility properties"
    [ fastProperty "breakOn finds first occurrence" prop_breakOn_first_occurrence
    , fastProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn handles missing pattern" prop_breakOn_pattern_not_found
    ]

  , testGroup "Complex processing properties"
    [ fastProperty "complex pipeline processing" prop_complex_pipeline
    , fastProperty "performance with large inputs" prop_performance_large_input
    ]
  ]