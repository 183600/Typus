{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.UtilsBoundaryConditionsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat)
import Data.Char (isSpace, isControl, isAscii, isLatin1)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort)
import qualified Data.Text as T

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, breakOn)

-- | QuickCheck tests for Utils boundary conditions
tests :: TestTree
tests =
  testGroup "UtilsBoundaryConditionsQuickCheckSpec - Utils Boundary Conditions Tests"
    [ testProperty "trim handles extreme whitespace combinations" prop_trimExtremeWhitespace
    , testProperty "splitBy handles empty strings L.and edge delimiters" prop_splitByEdgeCases
    , testProperty "removeComments handles nested L.and malformed comments" prop_removeCommentsEdgeCases
    , testProperty "normalizeIndentation handles inconsistent indentation" prop_normalizeIndentationEdgeCases
    , testProperty "breakOn handles empty patterns L.and edge matches" prop_breakOnEdgeCases
    , testProperty "Utils functions handle very large strings efficiently" prop_largeStringHandling
    , testProperty "Utils functions handle unicode edge cases" prop_unicodeEdgeCases
    , testProperty "Utils functions maintain consistency across related operations" prop_operationConsistency
    ]

-- ============================================================================
-- Utils Boundary Condition Properties
-- ============================================================================

-- Property: trim handles extreme whitespace combinations correctly
prop_trimExtremeWhitespace :: String -> String -> Bool
prop_trimExtremeWhitespace prefix suffix =
  let extremeWhitespace = prefix ++ "\t\n\r   \t\n\r" ++ suffix
      trimmed = trim extremeWhitespace
      expected = trim prefix ++ trim suffix
  in trimmed == expected

-- Property: splitBy handles empty strings L.and edge delimiters correctly
prop_splitByEdgeCases :: Char -> String -> Bool
prop_splitByEdgeCases delim input =
  let splitResult = splitBy delim input
      collapsedResult = splitByCollapsed delim input
      -- Check basic properties
      hasCorrectLength = L.length splitResult == L.length (L.filter (== delim) input) + 1
      collapsedHasNoEmpty = L.all (not . null) collapsedResult
      emptyInputCase = null input && splitResult == [""] && collapsedResult == []
  in (hasCorrectLength || emptyInputCase) && collapsedHasNoEmpty

-- Property: removeComments handles nested L.and malformed comments
prop_removeCommentsEdgeCases :: String -> Bool
prop_removeCommentsEdgeCases input =
  let malformedComments = input ++ "/* unclosed comment // another comment"
      processed = removeComments malformedComments
      -- Should not crash L.and should remove some comment markers
      noUnclosedMarkers = not ("/*" `L.isInfixOf` processed)
  in noUnclosedMarkers

-- Property: normalizeIndentation handles inconsistent indentation patterns
prop_normalizeIndentationEdgeCases :: String -> Bool
prop_normalizeIndentationEdgeCases input =
  let inconsistentIndent = input ++ "\n    line1\n\t\tline2\n  \t line3\n"
      normalized = normalizeIndentation inconsistentIndent
      normalizedLines = lines normalized
      -- Check that indentation is normalized (no leading tabs mixed with spaces in same line)
      consistentIndentation = L.all hasConsistentIndentation normalizedLines
  in consistentIndentation
  where
    hasConsistentIndentation line =
      let leading = takeWhile isSpace line
          hasTabs = '\t' `elem` leading
          hasSpaces = ' ' `elem` leading
      in not (hasTabs && hasSpaces)

-- Property: breakOn handles empty patterns L.and edge matches
prop_breakOnEdgeCases :: String -> String -> Bool
prop_breakOnEdgeCases pat input =
  let result = breakOn pat input
      (before, after) = result
  in if null pat
     then before == "" && after == input
     else if pat `L.isPrefixOf` input
          then before == "" && input `L.isPrefixOf` (pat ++ after)
          else if pat `L.isInfixOf` input
               then before ++ pat ++ after == input
               else result == (input, "")

-- Property: Utils functions handle very large strings efficiently
prop_largeStringHandling :: String -> Bool
prop_largeStringHandling input =
  let largeString = L.concat (replicate 1000 input)
      trimmed = trim largeString
      splitResult = splitBy ',' largeString
      processed = removeComments largeString
  in L.length trimmed <= L.length largeString &&
     L.length splitResult >= 1 &&
     L.length processed <= L.length largeString

-- Property: Utils functions handle unicode edge cases
prop_unicodeEdgeCases :: String -> Bool
prop_unicodeEdgeCases input =
  let unicodeInput = input ++ "测试🚀\n\t\r中文emoji\0\1\2" ++ input
      trimmed = trim unicodeInput
      splitResult = splitBy ' ' unicodeInput
      processed = removeComments unicodeInput
  -- Should not crash L.and maintain basic properties
  in L.length trimmed <= L.length unicodeInput &&
     L.length splitResult >= 1 &&
     L.length processed <= L.length unicodeInput

-- Property: Utils functions maintain consistency across related operations
prop_operationConsistency :: String -> Bool
prop_operationConsistency input =
  let trimmed = trim input
      trimmedAgain = trim trimmed
      splitTrimmed = splitBy ',' (trim input)
      trimEachSegment = map trim (splitBy ',' input)
      -- trim should be idempotent
      trimIdempotent = trimmed == trimmedAgain
      -- splitBy L.and trim should interact consistently
      splitConsistency = L.length splitTrimmed == L.length trimEachSegment
  in trimIdempotent && splitConsistency

-- ============================================================================
-- Additional Edge Case Properties
-- ============================================================================

-- Property: splitByComma L.and splitBy with ',' should be equivalent
prop_splitByCommaEquivalence :: String -> Bool
prop_splitByCommaEquivalence input = splitByComma input == splitBy ',' input

-- Property: splitByCommaCollapsed L.and splitByCollapsed with ',' should be equivalent  
prop_splitByCommaCollapsedEquivalence :: String -> Bool
prop_splitByCommaCollapsedEquivalence input = 
  splitByCommaCollapsed input == splitByCollapsed ',' input

-- Property: removeLineComments should preserve line structure
prop_removeLineCommentsPreservesLines :: String -> Bool
prop_removeLineCommentsPreservesLines input =
  let originalLines = lines input
      processedLines = lines (removeLineComments input)
  in L.length processedLines == L.length originalLines

-- Property: normalizeIndentation should be idempotent
prop_normalizeIndentationIdempotent :: String -> Bool
prop_normalizeIndentationIdempotent input =
  let once = normalizeIndentation input
      twice = normalizeIndentation once
  in once == twice

-- ============================================================================
-- Arbitrary Instances with Edge Cases
-- ============================================================================

-- Generate strings with extreme whitespace
arbitraryExtremeWhitespace :: Gen String
arbitraryExtremeWhitespace = listOf $ oneof
  [ elements " \t\n\r\v\f"
  , elements ['\0'..'\31']  -- Control characters
  , elements ['a'..'z']
  , elements ['A'..'Z']
  ]

-- Generate strings with comment-like patterns
arbitraryCommentString :: Gen String
arbitraryCommentString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']  
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "//"
  , elements "/*"
  , elements "*/"
  , elements "///**/"
  ]

-- Generate strings with indentation patterns
arbitraryIndentedString :: Gen String
arbitraryIndentedString = do
  lines <- listOf $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ elements ['a'..'z']
    return $ indent ++ content
  return $ unlines lines

-- Generate unicode edge case strings
arbitraryUnicodeEdgeCase :: Gen String
arbitraryUnicodeEdgeCase = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "测试中文🚀emoji"
  , elements ['\0'..'\31']  -- Control characters
  , elements ['\128'..'\255']  -- Extended ASCII
  ]

instance Arbitrary String where
  arbitrary = oneof
    [ arbitraryExtremeWhitespace
    , arbitraryCommentString
    , arbitraryIndentedString
    , arbitraryUnicodeEdgeCase
    ]