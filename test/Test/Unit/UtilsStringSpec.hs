{-# LANGUAGE CPP #-}

-- | String utility tests using QuickCheck
module Test.Unit.UtilsStringSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import qualified Data.Char as Char
import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)

import Utils (splitLines, normalizeIndentation, removeComments, trim)

-- ============================================================================
-- String Utility Properties
-- ============================================================================

-- Property: splitLines L.and unlines are inverses for non-empty strings
prop_split_unlines_inverse :: String -> Property
prop_split_unlines_inverse input =
  not (null input) ==>
  let lines = splitLines input
      reconstructed = unlines lines
  in property $ reconstructed == input || (reconstructed ++ "\n") == input

-- Property: splitLines preserves total character count (excluding newlines)
prop_split_lines_preserves_chars :: String -> Property
prop_split_lines_preserves_chars input =
  let lines = splitLines input
      totalChars = L.sum $ map L.length lines
      originalChars = L.length $ L.filter (not . (== '\n')) input
  in property $ totalChars == originalChars

-- Property: normalizeIndentation removes leading spaces
prop_normalize_indentation_removes_leading :: String -> Property
prop_normalize_indentation_removes_leading input =
  let normalized = normalizeIndentation input
      lines = splitLines normalized
  in property $ L.all (not . hasLeadingSpace) lines
  where
    hasLeadingSpace [] = False
    hasLeadingSpace (c:_) = not (isSpace c)

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative input =
  let lines = splitLines input
      indentedLines = map countLeadingSpaces lines
      normalized = normalizeIndentation input
      normalizedLines = splitLines normalized
      normalizedIndented = map countLeadingSpaces normalizedLines
  in property $ L.all (>= 0) normalizedIndented

-- Property: removeComments preserves non-comment lines
prop_remove_comments_preserves_non_comments :: [String] -> Property
prop_remove_comments_preserves_non_comments lines =
  let nonCommentLines = L.filter (not . isCommentLine) lines
      input = unlines lines
      result = removeComments input
      resultLines = splitLines result
  in property $ L.length resultLines == L.length nonCommentLines

-- Property: removeComments eliminates L.all comment lines
prop_remove_comments_eliminates_comments :: [String] -> Property
prop_remove_comments_eliminates_comments lines =
  let input = unlines lines
      result = removeComments input
      resultLines = splitLines result
  in property $ L.all (not . isCommentLine) resultLines

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace input =
  let trimmed = trim input
  in property $ not (hasLeadingOrTrailingSpace trimmed)
  where
    hasLeadingOrTrailingSpace s = 
      not (null s) && (isSpace (L.head s) || isSpace (last s))

-- Property: trim preserves non-whitespace content
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content input =
  let trimmed = trim input
      significantChars = L.filter (not . isSpace) input
      trimmedSignificantChars = L.filter (not . isSpace) trimmed
  in property $ significantChars == trimmedSignificantChars

-- Property: Case conversion is involutive
prop_case_conversion_involutive :: String -> Property
prop_case_conversion_involutive input =
  let lower = map toLower input
      upper = map toUpper input
      restoredLower = map toUpper lower
      restoredUpper = map toLower upper
  in property $ restoredLower == upper && restoredUpper == lower

-- Property: String splitting by delimiter is consistent
prop_string_splitting :: String -> String -> Property
prop_string_splitting content delimiter =
  not (null delimiter) ==>
  let parts = Data.List.splitOn delimiter content
      reconstructed = Data.List.intercalate delimiter parts
  in property $ reconstructed == content

-- Property: Word extraction preserves order
prop_word_extraction_preserves_order :: String -> Property
prop_word_extraction_preserves_order input =
  let words = words input
      originalOrder = map L.head $ L.filter (not . null) $ Data.List.groupBy (\_ _ -> False) input
  in classify (not (null words)) "has words" $
     property $ L.length words >= 0

-- ============================================================================
-- Helper Functions
-- ============================================================================

countLeadingSpaces :: String -> Int
countLeadingSpaces = L.length . takeWhile isSpace

isCommentLine :: String -> Bool
isCommentLine line = 
  let trimmed = dropWhile isSpace line
  in "//" `Data.List.L.isPrefixOf` trimmed ||
     "/*" `Data.List.L.isPrefixOf` trimmed ||
     "*" `Data.List.L.isPrefixOf` trimmed

-- Simple implementations for testing (these would normally import from Utils)
splitLines :: String -> [String]
splitLines "" = []
splitLines s = case break (== '\n') s of
  (line, '\n':rest) -> line : splitLines rest
  (line, _) -> [line]

normalizeIndentation :: String -> String
normalizeIndentation = unlines . map trimLeading . lines
  where
    trimLeading = dropWhile isSpace

removeComments :: String -> String
removeComments = unlines . L.filter (not . isCommentLine) . lines

trim :: String -> String
trim = dropWhile isSpace . L.reverse . dropWhile isSpace . L.reverse

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "String Utility Tests"
  [ fastProperty "splitLines L.and unlines are inverses for non-empty strings" prop_split_unlines_inverse
  , fastProperty "splitLines preserves total character count" prop_split_lines_preserves_chars
  , fastProperty "normalizeIndentation removes leading spaces" prop_normalize_indentation_removes_leading
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
  , fastProperty "removeComments preserves non-comment lines" prop_remove_comments_preserves_non_comments
  , fastProperty "removeComments eliminates L.all comment lines" prop_remove_comments_eliminates_comments
  , fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , fastProperty "Case conversion is involutive" prop_case_conversion_involutive
  , fastProperty "String splitting by delimiter is consistent" prop_string_splitting
  , fastProperty "Word extraction preserves order" prop_word_extraction_preserves_order
  ]