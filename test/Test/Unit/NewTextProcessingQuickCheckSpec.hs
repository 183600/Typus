{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewTextProcessingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Utils (trim, splitBy, removeLineComments, removeComments, normalizeIndentation)
import Data.Char (isSpace, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (groupBy)

-- Test string processing properties
prop_trim_whitespace_removal :: String -> Bool
prop_trim_whitespace_removal s = 
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (L.head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then L.length trimmed < L.length s
     else trimmed == s

prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_preserves_internal_whitespace :: String -> Bool
prop_trim_preserves_internal_whitespace s = 
  let trimmed = trim s
      internalSpaces = filter isSpace (take (L.length s - 2) (drop 1 s))
      trimmedInternalSpaces = filter isSpace (take (L.length trimmed - 2) (drop 1 trimmed))
  in if L.length s >= 2 && L.length trimmed >= 2
     then internalSpaces == trimmedInternalSpaces
     else True

-- Test splitBy properties
prop_split_by_concatenation :: Char -> String -> Bool
prop_split_by_concatenation c s = L.concat (splitBy c s) == s

prop_split_by_empty_segments :: Char -> String -> Bool
prop_split_by_empty_segments c s = 
  let segments = splitBy c s
      hasConsecutiveDelim = L.any (\(x:y:_) -> x == c && y == c) (zip s (L.tail s))
  in if hasConsecutiveDelim 
     then L.any null segments
     else L.all (not . null) segments

prop_split_by_start_end_delimiter :: Char -> String -> Bool
prop_split_by_start_end_delimiter c s = 
  let segments = splitBy c s
      startsWithDelim = not (null s) && L.head s == c
      endsWithDelim = not (null s) && last s == c
  in case (startsWithDelim, endsWithDelim) of
    (True, True) -> L.head segments == "" && last segments == ""
    (True, False) -> L.head segments == ""
    (False, True) -> last segments == ""
    (False, False) -> L.all (not . null) segments

-- Test comment removal properties
prop_remove_line_comments_preserves_line_structure :: String -> Bool
prop_remove_line_comments_preserves_line_structure s = 
  let result = removeLineComments s
      originalLines = lines s
      resultLines = lines result
  in L.length resultLines <= L.length originalLines

prop_remove_line_comments_removes_comments :: String -> Property
prop_remove_line_comments_removes_comments s = 
  "//" `L.isInfixOf` s ==> 
  let result = removeLineComments s
      linesWithComments = L.filter ("//" `L.isInfixOf`) (lines s)
      resultLines = lines result
  in L.all (not . ("//" `L.isInfixOf`)) resultLines

prop_remove_comments_preserves_string_literals :: String -> Property
prop_remove_comments_preserves_string_literals s = 
  let hasStringLiteral = "\"" `L.isInfixOf` s
  in hasStringLiteral ==> 
  let result = removeComments s
      originalStrings = extractStringLiterals s
      resultStrings = extractStringLiterals result
  in originalStrings == resultStrings

prop_remove_comments_preserves_character_literals :: String -> Property
prop_remove_comments_preserves_character_literals s = 
  let hasCharLiteral = L.any (\(c1:c2:_) -> c1 == '\'' && c2 /= '\\') (zip s (L.tail s))
  in hasCharLiteral ==> 
  let result = removeComments s
      originalChars = extractCharLiterals s
      resultChars = extractCharLiterals result
  in originalChars == resultChars

-- Test indentation normalization properties
prop_normalize_indentation_preserves_line_count :: String -> Bool
prop_normalize_indentation_preserves_line_count s = 
  let originalLines = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in L.length originalLines == L.length normalizedLines

prop_normalize_indentation_removes_common_prefix :: String -> Bool
prop_normalize_indentation_removes_common_prefix s = 
  let normalized = normalizeIndentation s
      nonEmptyLines = L.filter (not . L.all isSpace) (lines normalized)
  in if null nonEmptyLines
     then True
     else L.all (\line -> null line || not (isSpace (L.head line))) nonEmptyLines

prop_normalize_indentation_preserves_relative_structure :: String -> Bool
prop_normalize_indentation_preserves_relative_structure s = 
  let originalLines = L.filter (not . L.all isSpace) (lines s)
      normalized = normalizeIndentation s
      normalizedLines = L.filter (not . L.all isSpace) (lines normalized)
  in if L.length originalLines >= 2 && L.length normalizedLines >= 2
     then let originalIndents = L.map (L.length . takeWhile isSpace) originalLines
              normalizedIndents = L.map (L.length . takeWhile isSpace) normalizedLines
              originalDiffs = zipWith subtract (init originalIndents) (L.tail originalIndents)
              normalizedDiffs = zipWith subtract (init normalizedIndents) (L.tail normalizedIndents)
          in originalDiffs == normalizedDiffs
     else True

-- Test text processing edge cases
prop_empty_string_processing :: Bool
prop_empty_string_processing = 
  let s = ""
  in trim s == "" && splitBy ',' s == [""] && removeLineComments s == "" &&
     removeComments s == "" && normalizeIndentation s == ""

prop_whitespace_only_string :: Int -> Property
prop_whitespace_only_string n = 
  n >= 0 && n <= 100 ==> 
  let s = replicate n ' '
  in trim s == "" && splitBy ' ' s == replicate (n + 1) "" &&
     removeLineComments s == s && removeComments s == s &&
     normalizeIndentation s == ""

prop_unicode_handling :: String -> Property
prop_unicode_handling s = 
  L.any (>= 128) (map fromEnum s) ==> 
  let trimmed = trim s
      resultComments = removeComments s
      normalized = normalizeIndentation s
  in L.length trimmed <= L.length s &&
     L.length resultComments <= L.length s &&
     L.length normalized <= L.length s

-- Test performance properties
prop_split_by_linear_performance :: Char -> String -> Bool
prop_split_by_linear_performance c s = 
  let result = splitBy c s
      expectedLength = L.length (L.filter (== c) s) + 1
  in L.length result == expectedLength

prop_comment_removal_no_exponential_growth :: String -> Bool
prop_comment_removal_no_exponential_growth s = 
  let result = removeComments s
  in L.length result <= L.length s + 1000  -- Allow some margin for error cases

-- Test string processing invariants
prop_trim_split_interaction :: Char -> String -> Bool
prop_trim_split_interaction c s = 
  let trimmed = trim s
      splitOriginal = splitBy c s
      splitTrimmed = splitBy c trimmed
  in if null trimmed
     then splitTrimmed == [""]
     else L.length splitTrimmed == L.length splitOriginal

prop_comment_indentation_interaction :: String -> Bool
prop_comment_indentation_interaction s = 
  let withComments = removeComments s
      normalized = normalizeIndentation s
      normalizedAfterComments = normalizeIndentation withComments
  in L.length (lines normalized) == L.length (lines normalizedAfterComments)

-- Helper functions
extractStringLiterals :: String -> [String]
extractStringLiterals = extractLiterals '\"'

extractCharLiterals :: String -> [String]
extractCharLiterals = extractLiterals '\''

extractLiterals :: Char -> String -> [String]
extractLiterals quoteChar = go ""
  where
    go _ [] = []
    go acc (c:cs)
      | c == quoteChar = case findLiteralEnd cs of
          Just (literal, rest) -> literal : go "" rest
          Nothing -> []  -- Unterminated literal
      | otherwise = go (acc ++ [c]) cs

findLiteralEnd :: String -> Maybe (String, String)
findLiteralEnd s = go "" s
  where
    go acc [] = Just (acc, [])
    go acc ('\\':c:cs) = go (acc ++ ['\\', c]) cs
    go acc (c:cs)
      | c == quoteChar = Just (acc, cs)
      | otherwise = go (acc ++ [c]) cs
      where quoteChar = '\"'

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests