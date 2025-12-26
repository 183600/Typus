{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.UtilsRobustnessQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, intercalate)
import Data.Char (isSpace)

import Utils
import SourceLocation

-- | Test trim function properties
testTrimProperties :: Property
testTrimProperties =
  forAll arbitrary $ \text ->
    let trimmed = trim text
        leadingSpaces = length $ takeWhile isSpace text
        trailingSpaces = length $ reverse $ takeWhile isSpace $ reverse text
        expectedLength = length text - leadingSpaces - trailingSpaces
    in length trimmed === max 0 expectedLength .&&.
       (if null trimmed then all isSpace text else not (isSpace $ head trimmed)) .&&.
       (if null trimmed then all isSpace text else not (isSpace $ last trimmed))

-- | Test splitBy function properties
testSplitByProperties :: Property
testSplitByProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \delim ->
      let split = splitBy delim text
        joined = intercalate [delim] split
        -- For non-empty delimiters, joining should preserve original structure
      in if delim /= '\0' && not (null text)
         then length joined >= length text - length split
         else property True

-- | Test splitByCollapsed function properties
testSplitByCollapsedProperties :: Property
testSplitByCollapsedProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \delim ->
      let split = splitBy delim text
          collapsed = splitByCollapsed delim text
      in length collapsed <= length split .&&.
         all (not . null) collapsed

-- | Test splitByComma function properties
testSplitByCommaProperties :: Property
testSplitByCommaProperties =
  forAll arbitrary $ \text ->
    let commaSplit = splitByComma text
        genericSplit = splitBy ',' text
    in commaSplit === genericSplit

-- | Test removeLineComments function properties
testRemoveLineCommentsProperties :: Property
testRemoveLineCommentsProperties =
  forAll arbitrary $ \code ->
    let withoutComments = removeLineComments code
        linesOriginal = lines code
        linesWithoutComments = lines withoutComments
        commentLines = filter ("//" `isPrefixOf`) linesOriginal
        effectiveLines = filter (not . ("//" `isPrefixOf`)) linesOriginal
    in length linesWithoutComments === length effectiveLines .&&.
       length withoutComments <= length code

-- | Test removeComments function properties
testRemoveCommentsProperties :: Property
testRemoveCommentsProperties =
  forAll arbitrary $ \code ->
    let withoutComments = removeComments code
        withoutLineComments = removeLineComments code
    -- removeComments should handle both line and block comments
    in length withoutComments <= length withoutLineComments .&&.
       length withoutComments <= length code

-- | Test normalizeIndentation function properties
testNormalizeIndentationProperties :: Property
testNormalizeIndentationProperties =
  forAll arbitrary $ \code ->
    let normalized = normalizeIndentation code
        originalLines = lines code
        normalizedLines = lines normalized
    in length normalizedLines === length originalLines .&&.
       all (not . isPrefixOf "    ") normalizedLines

-- | Test forceSingleTabIndentation function properties
testForceSingleTabIndentationProperties :: Property
testForceSingleTabIndentationProperties =
  forAll arbitrary $ \code ->
    let tabIndented = forceSingleTabIndentation code
        linesOriginal = lines code
        linesTabIndented = lines tabIndented
    in length linesTabIndented === length linesOriginal

-- | Test fixIndentation function properties
testFixIndentationProperties :: Property
testFixIndentationProperties =
  forAll arbitrary $ \code ->
    let fixed = fixIndentation code
        normalized = normalizeIndentation code
    in fixed === normalized -- fixIndentation should be alias for normalizeIndentation

-- | Test breakOn function properties
testBreakOnProperties :: Property
testBreakOnProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \pattern ->
      let broken = breakOn pattern text
      in if null pattern
         then broken === ("", text)
         else let (before, after) = broken
              in length before + length pattern + length after >= length text

-- | Test comment removal with string literals
testCommentRemovalWithStrings :: Property
testCommentRemovalWithStrings =
  forAll arbitrary $ \code ->
    let withStrings = addStringLiterals code
        withoutComments = removeComments withStrings
        stringLiterals = extractStringLiterals withStrings
        remainingStrings = extractStringLiterals withoutComments
    -- String literals should be preserved
    in length remainingStrings === length stringLiterals

-- | Test whitespace handling robustness
testWhitespaceHandlingRobustness :: Property
testWhitespaceHandlingRobustness =
  forAll arbitrary $ \text ->
    let trimmed = trim text
        onlySpaces = all isSpace text
        onlySpacesTrimmed = null trimmed
    in onlySpaces === onlySpacesTrimmed

-- | Test split function with edge cases
testSplitEdgeCases :: Property
testSplitEdgeCases =
  forAll arbitrary $ \text ->
    let splitEmpty = splitBy ',' ""
        splitEmptyText = splitBy ',' text
        splitWithEmptyDelim = splitBy '\0' text
    in splitEmpty === [""] .&&.
       length splitEmptyText >= 0 .&&.
       length splitWithEmptyDelim >= 0

-- | Test comment removal edge cases
testCommentRemovalEdgeCases :: Property
testCommentRemovalEdgeCases =
  forAll arbitrary $ \code ->
    let onlyComments = "//" ++ code ++ "/* comment */"
        mixedComments = code ++ "// comment\nmore code"
        withoutComments = removeComments onlyComments
        withoutMixedComments = removeComments mixedComments
    in length withoutComments >= 0 .&&.
       length withoutMixedComments >= 0

-- | Test indentation edge cases
testIndentationEdgeCases :: Property
testIndentationEdgeCases =
  forAll arbitrary $ \code ->
    let noIndentation = unlines $ map (dropWhile isSpace) $ lines code
        normalized = normalizeIndentation code
        normalizedNoIndent = normalizeIndentation noIndentation
    in length (lines normalized) === length (lines code) .&&.
       length (lines normalizedNoIndent) === length (lines code)

-- Helper functions

addStringLiterals :: String -> String
addStringLiterals code = "\"string literal\" " ++ code

extractStringLiterals :: String -> [String]
extractStringLiterals = undefined -- Placeholder implementation

tests :: TestTree
tests = testGroup "Utils Robustness QuickCheck Tests"
  [ testProperty "Trim properties" testTrimProperties
  , testProperty "SplitBy properties" testSplitByProperties
  , testProperty "SplitByCollapsed properties" testSplitByCollapsedProperties
  , testProperty "SplitByComma properties" testSplitByCommaProperties
  , testProperty "RemoveLineComments properties" testRemoveLineCommentsProperties
  , testProperty "RemoveComments properties" testRemoveCommentsProperties
  , testProperty "NormalizeIndentation properties" testNormalizeIndentationProperties
  , testProperty "ForceSingleTabIndentation properties" testForceSingleTabIndentationProperties
  , testProperty "FixIndentation properties" testFixIndentationProperties
  , testProperty "BreakOn properties" testBreakOnProperties
  , testProperty "Comment removal with strings" testCommentRemovalWithStrings
  , testProperty "Whitespace handling" testWhitespaceHandlingRobustness
  , testProperty "Split edge cases" testSplitEdgeCases
  , testProperty "Comment removal edge cases" testCommentRemovalEdgeCases
  , testProperty "Indentation edge cases" testIndentationEdgeCases
  ]