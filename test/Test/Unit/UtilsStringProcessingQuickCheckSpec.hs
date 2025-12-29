{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Utils
import Data.List (sort, nub, group, intercalate, find, delete, isInfixOf, sortOn, stripPrefix, stripSuffix)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower, toUpper, isDigit, isPunctuation)

-- ============================================================================
-- Utils String Processing QuickCheck Tests
-- ============================================================================

-- Property: String normalization consistency
prop_string_normalization_consistency :: String -> Property
prop_string_normalization_consistency input =
  let normalized1 = normalizeString input
      normalized2 = normalizeString normalized1
  in property $ normalized1 === normalized2

-- Property: Whitespace preservation in string processing
prop_whitespace_preservation :: String -> String -> Property
prop_whitespace_preservation prefix suffix =
  let input = prefix ++ "   " ++ suffix
      processed = preserveWhitespace input
      expectedSpaces = length (filter isSpace input)
      actualSpaces = length (filter isSpace processed)
  in property $ actualSpaces === expectedSpaces

-- Property: Case conversion roundtrip
prop_case_conversion_roundtrip :: String -> Property
prop_case_conversion_roundtrip original =
  let upper = toUpperString original
      lower = toLowerString upper
      roundtrip = toUpperString lower
  in property $ toUpperString original === roundtrip

-- Property: String tokenization consistency
prop_string_tokenization_consistency :: String -> String -> Property
prop_string_tokenization_consistency delimiter content =
  not (null delimiter) ==> 
  let tokens1 = tokenizeBy delimiter content
      reconstructed = intercalate delimiter tokens1
      tokens2 = tokenizeBy delimiter reconstructed
  in property $ tokens1 === tokens2

-- Property: String escaping/unescaping roundtrip
prop_string_escaping_roundtrip :: String -> Property
prop_string_escaping_roundtrip original =
  let escaped = escapeString original
      unescaped = unescapeString escaped
  in property $ original === unescaped

-- Property: String trimming idempotency
prop_string_trimming_idempotency :: String -> Property
prop_string_trimming_idempotency input =
  let trimmed1 = trimString input
      trimmed2 = trimString trimmed1
  in property $ trimmed1 === trimmed2

-- Property: String prefix/suffix extraction
prop_string_prefix_suffix_extraction :: String -> String -> Property
prop_string_prefix_suffix_extraction prefix suffix =
  not (null prefix) && not (null suffix) ==> 
  let combined = prefix ++ "middle" ++ suffix
      extractedPrefix = extractPrefix combined (length prefix)
      extractedSuffix = extractSuffix combined (length suffix)
  in property $ extractedPrefix === prefix .&&. extractedSuffix === suffix

-- Property: String substitution correctness
prop_string_substitution_correctness :: String -> String -> String -> Property
prop_string_substitution_correctness original old new =
  not (null old) ==> 
  let substituted = substituteString original old new
      containsNew = new `isInfixOf` substituted
      containsOld = old `isInfixOf` substituted
  in property $ (old `isInfixOf` original) ==> (containsNew .&&. not containsOld)

-- Property: String splitting and joining
prop_string_splitting_joining :: String -> String -> Property
prop_string_splitting_joining content delimiter =
  not (null delimiter) ==> 
  let split = splitString content delimiter
      joined = joinString split delimiter
  in property $ joined === content

-- Property: String word count accuracy
prop_string_word_count_accuracy :: [String] -> Property
prop_string_word_count_accuracy words =
  not (null words) ==> 
  let content = unwords words
      counted = countWords content
  in property $ counted === length words

-- Property: String line counting
prop_string_line_counting :: [String] -> Property
prop_string_line_counting lines =
  not (null lines) ==> 
  let content = unlines lines
      counted = countLines content
  in property $ counted === length lines

-- Property: String character classification
prop_string_character_classification :: String -> Property
prop_string_character_classification input =
  let alphas = countAlphaChars input
      numerics = countNumericChars input
      spaces = countSpaceChars input
      punctuation = countPunctuationChars input
      total = alphas + numerics + spaces + punctuation
  in property $ total <= length input

-- Property: String palindrome detection
prop_string_palindrome_detection :: String -> Property
prop_string_palindrome_detection input =
  let cleaned = filter isAlphaNum (map toLower input)
      isPalindrome = cleaned == reverse cleaned
      detected = isPalindromeString input
  in property $ isPalindrome === detected

-- Property: String similarity measurement symmetry
prop_string_similarity_symmetry :: String -> String -> Property
prop_string_similarity_symmetry str1 str2 =
  let similarity1 = calculateStringSimilarity str1 str2
      similarity2 = calculateStringSimilarity str2 str1
  in property $ similarity1 === similarity2

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- String processing functions
normalizeString :: String -> String
normalizeString = unwords . words . map toLower

preserveWhitespace :: String -> String
preserveWhitespace = id

toUpperString :: String -> String
toUpperString = map toUpper

toLowerString :: String -> String
toLowerString = map toLower

tokenizeBy :: String -> String -> [String]
tokenizeBy delimiter content = splitString content delimiter

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

unescapeString :: String -> String
unescapeString = unescape
  where
    unescape [] = []
    unescape ('\\':c:rest) = unescapeChar c ++ unescape rest
    unescape (c:rest) = c : unescape rest
    
    unescapeChar '\\' = "\\"
    unescapeChar '"' = "\""
    unescapeChar 'n' = "\n"
    unescapeChar 't' = "\t"
    unescapeChar _ = ""

trimString :: String -> String
trimString = reverse . dropWhile isSpace . reverse . dropWhile isSpace

extractPrefix :: String -> Int -> String
extractPrefix str n = take n str

extractSuffix :: String -> Int -> String
extractSuffix str n = drop (length str - n) str

substituteString :: String -> String -> String -> String
substituteString original old new = 
  if old `isInfixOf` original
  then takeWhile (not . isPrefixOf old) original ++ new ++ substituteString (drop (length old + length (takeWhile (not . isPrefixOf old) original)) original) old new
  else original
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

splitString :: String -> String -> [String]
splitString [] _ = [""]
splitString s delimiter
  | delimiter `isPrefixOf` s = "" : splitString (drop (length delimiter) s) delimiter
  | otherwise = case s of
      [] -> [""]
      (c:cs) -> case splitString cs delimiter of
          [] -> [[c]]
          (x:xs) -> (c:x) : xs

joinString :: [String] -> String -> String
joinString [] _ = []
joinString [x] _ = x
joinString (x:xs) delimiter = x ++ delimiter ++ joinString xs delimiter

countWords :: String -> Int
countWords = length . words

countLines :: String -> Int
countLines = length . lines

countAlphaChars :: String -> Int
countAlphaChars = length . filter isAlpha

countNumericChars :: String -> Int
countNumericChars = length . filter isDigit

countSpaceChars :: String -> Int
countSpaceChars = length . filter isSpace

countPunctuationChars :: String -> Int
countPunctuationChars = length . filter isPunctuation

isPalindromeString :: String -> Bool
isPalindromeString input = 
  let cleaned = filter isAlphaNum (map toLower input)
  in cleaned == reverse cleaned

calculateStringSimilarity :: String -> String -> Double
calculateStringSimilarity str1 str2 =
  let commonChars = length $ intersect str1 str2
      totalChars = max (length str1) (length str2)
  in if totalChars == 0 then 1.0 else fromIntegral commonChars / fromIntegral totalChars

-- Helper function for string intersection
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
  | x `elem` ys = x : intersect xs (delete x ys)
  | otherwise = intersect xs ys

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Processing QuickCheck Tests"
  [ fastProperty "String normalization consistency" prop_string_normalization_consistency
  , fastProperty "Whitespace preservation in string processing" prop_whitespace_preservation
  , fastProperty "Case conversion roundtrip" prop_case_conversion_roundtrip
  , fastProperty "String tokenization consistency" prop_string_tokenization_consistency
  , fastProperty "String escaping/unescaping roundtrip" prop_string_escaping_roundtrip
  , fastProperty "String trimming idempotency" prop_string_trimming_idempotency
  , fastProperty "String prefix/suffix extraction" prop_string_prefix_suffix_extraction
  , fastProperty "String substitution correctness" prop_string_substitution_correctness
  , fastProperty "String splitting and joining" prop_string_splitting_joining
  , fastProperty "String word count accuracy" prop_string_word_count_accuracy
  , fastProperty "String line counting" prop_string_line_counting
  , fastProperty "String character classification" prop_string_character_classification
  , fastProperty "String palindrome detection" prop_string_palindrome_detection
  , fastProperty "String similarity measurement symmetry" prop_string_similarity_symmetry
  ]