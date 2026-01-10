module Test.Unit.UtilsStringProcessingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils
import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, nub)
import Data.Maybe (isJust, isNothing)

-- 测试字符串处理的属性
prop_string_length_preservation :: String -> Property
prop_string_length_preservation input = 
  let processed = Utils.normalizeWhitespace input
  in property $ length processed <= length input

prop_whitespace_normalization :: String -> Property
prop_whitespace_normalization input = 
  let normalized = Utils.normalizeWhitespace input
      hasMultipleSpaces = "  " `isInfixOf` normalized
  in property $ not hasMultipleSpaces

prop_case_conversion_preservation :: String -> Property
prop_case_conversion_preservation input = 
  let upper = Utils.toUpperCase input
      lower = Utils.toLowerCase input
      restored = Utils.toLowerCase upper
  in property $ lower === restored

prop_case_insensitive_comparison :: String -> String -> Property
prop_case_insensitive_comparison str1 str2 = 
  let sameIgnoringCase = Utils.equalsIgnoreCase str1 str2
      bothUpper = Utils.toUpperCase str1 == Utils.toUpperCase str2
  in property $ sameIgnoringCase === bothUpper

prop_string_trimming :: String -> Property
prop_string_trimming input = 
  let trimmed = Utils.trim input
      startsWithSpace = not (null trimmed) && isSpace (head trimmed)
      endsWithSpace = not (null trimmed) && isSpace (last trimmed)
  in property $ not startsWithSpace && not endsWithSpace

prop_string_splitting :: String -> String -> Property
prop_string_splitting input delimiter = 
  let parts = Utils.split delimiter input
      joined = Utils.join delimiter parts
  in property $ joined === input

prop_string_joining :: [String] -> String -> Property
prop_string_joining parts delimiter = 
  let joined = Utils.join delimiter parts
      splitAgain = Utils.split delimiter joined
  in property $ splitAgain === parts

prop_string_replacement :: String -> String -> String -> Property
prop_string_replacement input old new = 
  let replaced = Utils.replace old new input
  in property $ not (old `isInfixOf` replaced) || old === new

prop_string_prefix_checking :: String -> String -> Property
prop_string_prefix_checking input prefix = 
  let hasPrefix = Utils.hasPrefix input prefix
      actualPrefix = isPrefixOf prefix input
  in property $ hasPrefix === actualPrefix

prop_string_suffix_checking :: String -> String -> Property
prop_string_suffix_checking input suffix = 
  let hasSuffix = Utils.hasSuffix input suffix
      actualSuffix = isSuffixOf suffix input
  in property $ hasSuffix === actualSuffix

prop_string_contains :: String -> String -> Property
prop_string_contains input substr = 
  let contains = Utils.contains input substr
      actualContains = isInfixOf substr input
  in property $ contains === actualContains

prop_string_reverse :: String -> Property
prop_string_reverse input = 
  let reversed = Utils.reverse input
      doubleReversed = Utils.reverse reversed
  in property $ doubleReversed === input

prop_string_word_count :: String -> Property
prop_string_word_count input = 
  let words = Utils.words input
      count = Utils.wordCount input
  in property $ length words === count

prop_string_line_count :: String -> Property
prop_string_line_count input = 
  let lines = Utils.lines input
      count = Utils.lineCount input
  in property $ length lines === count

prop_string_indentation :: String -> Int -> Property
prop_string_indentation input indent = 
  let indented = Utils.indent input indent
      lines' = Utils.lines indented
      allIndented = all (\line -> 
        if null line then True 
        else take indent line === replicate indent ' '
      ) lines'
  in property $ allIndented

prop_string_unindentation :: String -> Property
prop_string_unindentation input = 
  let indented = Utils.indent input 4
      unindented = Utils.unindent indented
  in property $ unindented === input

prop_string_wrap :: String -> Int -> Property
prop_string_wrap input width = 
  let wrapped = Utils.wrap input width
      lines' = Utils.lines wrapped
      allWithinWidth = all (\line -> length line <= width) lines'
  in property $ allWithinWidth

prop_string_pad_left :: String -> Int -> Property
prop_string_pad_left input targetLen = 
  let padded = Utils.padLeft input targetLen
  in property $ length padded >= targetLen

prop_string_pad_right :: String -> Int -> Property
prop_string_pad_right input targetLen = 
  let padded = Utils.padRight input targetLen
  in property $ length padded >= targetLen

prop_string_center :: String -> Int -> Property
prop_string_center input targetLen = 
  let centered = Utils.center input targetLen
  in property $ length centered >= targetLen

prop_string_truncate :: String -> Int -> Property
prop_string_truncate input maxLen = 
  let truncated = Utils.truncate input maxLen
  in property $ length truncated <= maxLen

prop_string_escape :: String -> Property
prop_string_escape input = 
  let escaped = Utils.escape input
  in property $ not ('\n' `elem` escaped) && 
             not ('\t' `elem` escaped) && 
             not ('"' `elem` escaped)

prop_string_unescape :: String -> Property
prop_string_unescape input = 
  let escaped = Utils.escape input
      unescaped = Utils.unescape escaped
  in property $ unescaped === input

prop_string_quoting :: String -> Property
prop_string_quoting input = 
  let quoted = Utils.quote input
      unquoted = Utils.unquote quoted
  in property $ unquoted === input

prop_string_slugify :: String -> Property
prop_string_slugify input = 
  let slug = Utils.slugify input
      validChars = all (\c -> isAlpha c || isDigit c || c == '-' || c == '_') slug
  in property $ validChars

prop_string_capitalize :: String -> Property
prop_string_capitalize input = 
  let capitalized = Utils.capitalize input
      isCapitalized = null capitalized || isAlpha (head capitalized) && isUpper (head capitalized)
  in property $ isCapitalized

prop_string_camelize :: String -> Property
prop_string_camelize input = 
  let camelized = Utils.camelize input
      hasNoSpaces = not (isInfixOf " " camelized)
  in property $ hasNoSpaces

prop_string_snakify :: String -> Property
prop_string_snakify input = 
  let snakified = Utils.snakify input
      hasNoSpaces = not (isInfixOf " " snakified)
      hasUnderscores = isInfixOf "_" snakified
  in property $ hasNoSpaces

prop_string_kebabify :: String -> Property
prop_string_kebabify input = 
  let kebabified = Utils.kebabify input
      hasNoSpaces = not (isInfixOf " " kebabified)
      hasDashes = isInfixOf "-" kebabified
  in property $ hasNoSpaces

prop_string_similarity :: String -> String -> Property
prop_string_similarity str1 str2 = 
  let similarity = Utils.similarity str1 str2
  in property $ similarity >= 0 && similarity <= 1

prop_string_levenshtein_distance :: String -> String -> Property
prop_string_levenshtein_distance str1 str2 = 
  let distance = Utils.levenshteinDistance str1 str2
  in property $ distance >= 0

prop_string_soundex :: String -> Property
prop_string_soundex input = 
  let code = Utils.soundex input
  in property $ length code === 4 && all isDigit (tail code)

prop_string_metaphone :: String -> Property
prop_string_metaphone input = 
  let code = Utils.metaphone input
  in property $ not (null code) && all isAlpha code

tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testProperty "String length preservation" prop_string_length_preservation
  , testProperty "Whitespace normalization" prop_whitespace_normalization
  , testProperty "Case conversion preservation" prop_case_conversion_preservation
  , testProperty "Case insensitive comparison" prop_case_insensitive_comparison
  , testProperty "String trimming" prop_string_trimming
  , testProperty "String splitting" prop_string_splitting
  , testProperty "String joining" prop_string_joining
  , testProperty "String replacement" prop_string_replacement
  , testProperty "String prefix checking" prop_string_prefix_checking
  , testProperty "String suffix checking" prop_string_suffix_checking
  , testProperty "String contains" prop_string_contains
  , testProperty "String reverse" prop_string_reverse
  , testProperty "String word count" prop_string_word_count
  , testProperty "String line count" prop_string_line_count
  , testProperty "String indentation" prop_string_indentation
  , testProperty "String unindentation" prop_string_unindentation
  , testProperty "String wrap" prop_string_wrap
  , testProperty "String pad left" prop_string_pad_left
  , testProperty "String pad right" prop_string_pad_right
  , testProperty "String center" prop_string_center
  , testProperty "String truncate" prop_string_truncate
  , testProperty "String escape" prop_string_escape
  , testProperty "String unescape" prop_string_unescape
  , testProperty "String quoting" prop_string_quoting
  , testProperty "String slugify" prop_string_slugify
  , testProperty "String capitalize" prop_string_capitalize
  , testProperty "String camelize" prop_string_camelize
  , testProperty "String snakify" prop_string_snakify
  , testProperty "String kebabify" prop_string_kebabify
  , testProperty "String similarity" prop_string_similarity
  , testProperty "String levenshtein distance" prop_string_levenshtein_distance
  , testProperty "String soundex" prop_string_soundex
  , testProperty "String metaphone" prop_string_metaphone
  ]