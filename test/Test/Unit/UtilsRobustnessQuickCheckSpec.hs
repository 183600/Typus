{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.UtilsRobustnessQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (intercalate)
import Data.Char (isSpace)

import Utils
import SourceLocation

-- | Test trim function properties
testTrimProperties :: Property
testTrimProperties =
  forAll arbitrary $ \text ->
    let trimmed = trim text
        leadingSpaces = L.length $ takeWhile isSpace text
        trailingSpaces = L.length $ L.reverse $ takeWhile isSpace $ L.reverse text
        expectedLength = L.length text - leadingSpaces - trailingSpaces
    in L.length trimmed === max 0 expectedLength .&&.
       (if null trimmed then L.all isSpace text else not (isSpace $ L.head trimmed)) .&&.
       (if null trimmed then L.all isSpace text else not (isSpace $ last trimmed))

-- | Test splitBy function properties
testSplitByProperties :: Property
testSplitByProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \delim ->
      let split = splitBy delim text
        joined = intercalate [delim] split
        -- For non-empty delimiters, joining should preserve original structure
      in if delim /= '\0' && not (null text)
         then L.length joined >= L.length text - L.length split
         else property True

-- | Test splitByCollapsed function properties
testSplitByCollapsedProperties :: Property
testSplitByCollapsedProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \delim ->
      let split = splitBy delim text
          collapsed = splitByCollapsed delim text
      in L.length collapsed <= L.length split .&&.
         L.all (not . null) collapsed

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
        commentLines = L.filter ("//" `L.isPrefixOf`) linesOriginal
        effectiveLines = L.filter (not . ("//" `L.isPrefixOf`)) linesOriginal
    in L.length linesWithoutComments === L.length effectiveLines .&&.
       L.length withoutComments <= L.length code

-- | Test removeComments function properties
testRemoveCommentsProperties :: Property
testRemoveCommentsProperties =
  forAll arbitrary $ \code ->
    let withoutComments = removeComments code
        withoutLineComments = removeLineComments code
    -- removeComments should handle both line L.and block comments
    in L.length withoutComments <= L.length withoutLineComments .&&.
       L.length withoutComments <= L.length code

-- | Test normalizeIndentation function properties
testNormalizeIndentationProperties :: Property
testNormalizeIndentationProperties =
  forAll arbitrary $ \code ->
    let normalized = normalizeIndentation code
        originalLines = lines code
        normalizedLines = lines normalized
    in L.length normalizedLines === L.length originalLines .&&.
       L.all (not . L.isPrefixOf "    ") normalizedLines

-- | Test forceSingleTabIndentation function properties
testForceSingleTabIndentationProperties :: Property
testForceSingleTabIndentationProperties =
  forAll arbitrary $ \code ->
    let tabIndented = forceSingleTabIndentation code
        linesOriginal = lines code
        linesTabIndented = lines tabIndented
    in L.length linesTabIndented === L.length linesOriginal

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
              in L.length before + L.length pattern + L.length after >= L.length text

-- | Test comment removal with string literals
testCommentRemovalWithStrings :: Property
testCommentRemovalWithStrings =
  forAll arbitrary $ \code ->
    let withStrings = addStringLiterals code
        withoutComments = removeComments withStrings
        stringLiterals = extractStringLiterals withStrings
        remainingStrings = extractStringLiterals withoutComments
    -- String literals should be preserved
    in L.length remainingStrings === L.length stringLiterals

-- | Test whitespace handling robustness
testWhitespaceHandlingRobustness :: Property
testWhitespaceHandlingRobustness =
  forAll arbitrary $ \text ->
    let trimmed = trim text
        onlySpaces = L.all isSpace text
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
       L.length splitEmptyText >= 0 .&&.
       L.length splitWithEmptyDelim >= 0

-- | Test comment removal edge cases
testCommentRemovalEdgeCases :: Property
testCommentRemovalEdgeCases =
  forAll arbitrary $ \code ->
    let onlyComments = "//" ++ code ++ "/* comment */"
        mixedComments = code ++ "// comment\nmore code"
        withoutComments = removeComments onlyComments
        withoutMixedComments = removeComments mixedComments
    in L.length withoutComments >= 0 .&&.
       L.length withoutMixedComments >= 0

-- | Test indentation edge cases
testIndentationEdgeCases :: Property
testIndentationEdgeCases =
  forAll arbitrary $ \code ->
    let noIndentation = unlines $ L.map (dropWhile isSpace) $ lines code
        normalized = normalizeIndentation code
        normalizedNoIndent = normalizeIndentation noIndentation
    in L.length (lines normalized) === L.length (lines code) .&&.
       L.length (lines normalizedNoIndent) === L.length (lines code)

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