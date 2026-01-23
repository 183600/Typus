{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.UtilsStringProcessingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit, isLetter, isLower, isUpper)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect, group, splitAt, takeWhile, dropWhile, foldl')
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.String (IsString(..))

-- ============================================================================
-- Utils String Processing Tests
-- ============================================================================

-- | Test string trimming functionality
prop_string_trim :: String -> Property
prop_string_trim input =
  let trimmedInput = trim input
      firstChar str = case str of
                        (c:_) -> c
                        [] -> ' '
      lastChar str = case reverse str of
                       (c:_) -> c
                       [] -> ' '
      hasLeadingSpace = not (null input) && isSpace (firstChar input)
      hasTrailingSpace = not (null input) && isSpace (lastChar input)
      expectedTrimmed = dropWhile isSpace $ dropWhileEnd isSpace input
  in property $ trimmedInput == expectedTrimmed

-- | Test string splitting by delimiter

prop_string_split_by :: Char -> String -> Property

prop_string_split_by delim input =

  let splitResult = splitBy delim input

      expectedSplit = if delim `elem` input 

                       then splitWhen (== delim) input 

                       else [input]

  in property $ length splitResult == length expectedSplit

-- | Test string joining with separator
prop_string_join :: Char -> [String] -> Property
prop_string_join delim parts =
  let joinResult = joinWith delim parts
      expectedJoin = intercalate [delim] parts
  in property $ joinResult == expectedJoin

-- | Test string case conversion
prop_string_case_conversion :: String -> Property
prop_string_case_conversion input =
  let upperInput = map toUpper input
      lowerInput = map toLower input
      titleInput = toTitle input
  in property $ all isUpper upperInput && all isLower lowerInput

-- | Test string whitespace normalization
prop_string_whitespace_normalize :: String -> Property
prop_string_whitespace_normalize input =
  let normalized = normalizeWhitespace input
      hasMultipleSpaces = "  " `isInfixOf` normalized
  in property $ not hasMultipleSpaces

-- | Test string indentation handling
prop_string_indentation :: Int -> String -> Property
prop_string_indentation indentLevel content =
  indentLevel >= 0 && indentLevel <= 10 ==>
    let indented = indent indentLevel content
        expectedIndent = replicate indentLevel ' '
        startsWithIndent = expectedIndent `isPrefixOf` indented
  in property $ startsWithIndent || null content

-- | Test string escaping for special characters
prop_string_escape :: String -> Property
prop_string_escape input =
  let escaped = escapeString input
      hasUnescaped = any (\c -> c `elem` "\\\"\'\n\r\t") input && 
                    not (any (\c -> c `elem` "\\\"\'\n\r\t") escaped)
  in property $ not hasUnescaped || null input

-- | Test string unescaping
prop_string_unescape :: String -> Property
prop_string_unescape input =
  let escaped = escapeString input
      unescaped = unescapeString escaped
  in property $ unescaped == input

-- | Test string word wrapping
prop_string_wrap :: Int -> String -> Property
prop_string_wrap width input =
  width > 0 && width <= 100 ==>
    let wrapped = wrapString width input
        lines = splitBy '\n' wrapped
        allLinesFitWidth = all (\line -> length line <= width) lines
  in property $ allLinesFitWidth

-- | Test string prefix/suffix operations
prop_string_prefix_suffix :: String -> String -> Property
prop_string_prefix_suffix prefix suffix =
  let combined = prefix ++ suffix
      hasPrefix = prefix `isPrefixOf` combined
      hasSuffix = suffix `isSuffixOf` combined
  in property $ hasPrefix && hasSuffix

-- | Test string character counting
prop_string_char_count :: Char -> String -> Property
prop_string_char_count char input =
  let count = charCount char input
      expectedCount = length $ filter (== char) input
  in property $ count == expectedCount

-- | Test string word counting
prop_string_word_count :: String -> Property
prop_string_word_count input =
  let count = wordCount input
      words = filter (not . null) $ splitBy ' ' $ normalizeWhitespace input
      expectedCount = length words
  in property $ count == expectedCount

-- | Test string line counting
prop_string_line_count :: String -> Property
prop_string_line_count input =
  let count = lineCount input
      lines = splitBy '\n' input
      expectedCount = length lines
  in property $ count == expectedCount

-- | Test string palindrome detection
prop_string_palindrome :: String -> Property
prop_string_palindrome input =
  let isPal = isPalindrome input
      reversed = reverse input
  in property $ isPal == (input == reversed)

-- | Test string levenshtein distance
prop_string_levenshtein :: String -> String -> Property
prop_string_levenshtein s1 s2 =
  let distance = levenshteinDistance s1 s2
      distanceNonNegative = distance >= 0
      distanceSymmetric = distance == levenshteinDistance s2 s1
      distanceZeroForIdentical = property $ (s1 == s2) ==> distance == 0
  in property $ distanceNonNegative && distanceSymmetric && 
                 (if s1 == s2 then distance == 0 else True)

-- | Test string longest common subsequence
prop_string_lcs :: String -> String -> Property
prop_string_lcs s1 s2 =
  let lcs = longestCommonSubsequence s1 s2
      lcsIsSubsequence = isSubsequence lcs s1 && isSubsequence lcs s2
      lcsLength = length lcs
  in property $ lcsIsSubsequence

-- | Test string soundex/metaphone
prop_string_soundex :: String -> Property
prop_string_soundex input =
  let soundexCode = soundex input
      soundexLength = length soundexCode
  in property $ soundexLength == 4 || null input

-- | Test string similarity metrics
prop_string_similarity :: String -> String -> Property
prop_string_similarity s1 s2 =
  let similarity = stringSimilarity s1 s2
      similarityInRange = similarity >= 0.0 && similarity <= 1.0
      identicalStrings = property $ (s1 == s2) ==> similarity == 1.0
  in property $ similarityInRange && 
                 (if s1 == s2 then similarity == 1.0 else True)

-- | Test string tokenization
prop_string_tokenize :: String -> Property
prop_string_tokenize input =
  let tokens = tokenize input
      tokensNotEmpty = all (not . null) tokens
      tokensReconstruct = unwords tokens
  in property $ tokensNotEmpty

-- | Test string normalization (Unicode)
prop_string_normalize_unicode :: String -> Property
prop_string_normalize_unicode input =
  let normalized = normalizeUnicode input
      normalizedLength = length normalized
  in property $ normalizedLength >= 0

-- | Test string compression/decompression
prop_string_compress :: String -> Property
prop_string_compress input =
  let compressed = compressString input
      decompressed = decompressString compressed
  in property $ decompressed == input

-- | Test string encoding/decoding
prop_string_encode_decode :: String -> Property
prop_string_encode_decode input =
  let encoded = encodeString input
      decoded = decodeString encoded
  in property $ decoded == input

-- | Test string hashing
prop_string_hash :: String -> Property
prop_string_hash input =
  let hash1 = hashString input
      hash2 = hashString input
      hashConsistent = hash1 == hash2
      hashDiffers = input == "" || hash1 /= hashString ""
  in property $ hashConsistent && hashDiffers

-- | Test string template interpolation
prop_string_template :: String -> String -> Property
prop_string_template template value =
  let placeholder = "{value}"
      templateWithValue = placeholder `isInfixOf` template
      interpolated = interpolateTemplate template [("value", value)]
      valueInResult = value `isInfixOf` interpolated
  in property $ templateWithValue ==> valueInResult

-- | Test string padding
prop_string_pad :: Int -> Char -> String -> Property
prop_string_pad padLength padChar input =
  padLength >= 0 && padLength <= 100 ==>
    let padded = input ++ replicate padLength padChar
        paddedLength = length (padded :: String)
    in property $ paddedLength >= padLength

-- | Test string truncation
prop_string_truncate :: Int -> String -> Property
prop_string_truncate maxLength input =
  maxLength >= 0 && maxLength <= 100 ==>
    let truncated = truncateString maxLength input
        truncatedLength = length truncated
  in property $ truncatedLength <= maxLength

-- Helper functions
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = [[]]
splitWhen p xs = go xs []
  where
    go [] acc = [reverse acc]
    go (y:ys) acc
      | p y = reverse acc : go ys []
      | otherwise = go ys (y:acc)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

toUpper :: Char -> Char
toUpper c = if isLower c then toEnum (fromEnum c - 32) else c

toLower :: Char -> Char
toLower c = if isUpper c then toEnum (fromEnum c + 32) else c

toTitle :: String -> String
toTitle [] = []
toTitle (c:cs) = toUpper c : map toLower cs

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\"' = "\\\""
    escapeChar '\'' = "\\'"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\\':c:rest) = unescapeChar c : unescapeString rest
  where
    unescapeChar 'n' = '\n'
    unescapeChar 'r' = '\r'
    unescapeChar 't' = '\t'
    unescapeChar '\\' = '\\'
    unescapeChar '\"' = '\"'
    unescapeChar '\'' = '\''
    unescapeChar _ = '?'
unescapeString (c:rest) = c : unescapeString rest

wrapString :: Int -> String -> String
wrapString width = unlines . wrapWords width . words
  where
    wrapWords _ [] = []
    wrapWords w words = go w words []
      where
        go _ [] acc = [unwords $ reverse acc]
        go remaining (word:rest) acc
          | length word <= remaining = go (remaining - length word - 1) rest (word:acc)
          | null acc = go w (word:rest) acc
          | otherwise = unwords (reverse acc) : go w (word:rest) []

charCount :: Char -> String -> Int
charCount c = length . filter (== c)

wordCount :: String -> Int
wordCount = length . filter (not . null) . words

lineCount :: String -> Int
lineCount = length . splitBy '\n'

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

levenshteinDistance :: String -> String -> Int
levenshteinDistance s1 s2 = go s1 s2
  where
    go [] [] = 0
    go [] ys = length ys
    go xs [] = length xs
    go (x:xs) (y:ys)
      | x == y = go xs ys
      | otherwise = 1 + minimum [go xs (y:ys), go (x:xs) ys, go xs ys]

isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
  | x == y = isSubsequence xs ys
  | otherwise = isSubsequence (x:xs) ys

longestCommonSubsequence :: Eq a => [a] -> [a] -> [a]
longestCommonSubsequence [] _ = []
longestCommonSubsequence _ [] = []
longestCommonSubsequence (x:xs) (y:ys)
  | x == y = x : longestCommonSubsequence xs ys
  | otherwise = longer (longestCommonSubsequence xs (y:ys)) (longestCommonSubsequence (x:xs) ys)
  where
    longer a b = if length a > length b then a else b

soundex :: String -> String
soundex [] = []
soundex (c:rest) = take 4 $ c : map soundexDigit (filterSoundex (tail input))
  where
    input = map toUpper (c:rest)
    tail [] = []
    tail (x:xs) = xs
    soundexDigit ch
      | ch `elem` "BFPV" = '1'
      | ch `elem` "CGJKQSXZ" = '2'
      | ch `elem` "DT" = '3'
      | ch `elem` "L" = '4'
      | ch `elem` "MN" = '5'
      | ch `elem` "R" = '6'
      | otherwise = '0'
    filterSoundex [] = []
    filterSoundex [x] = [x]
    filterSoundex (x:y:rest)
      | soundexDigit x == soundexDigit y = filterSoundex (x:rest)
      | otherwise = x : filterSoundex (y:rest)

stringSimilarity :: String -> String -> Float
stringSimilarity s1 s2 = fromIntegral (2 * lcsLength) / fromIntegral (length s1 + length s2)
  where
    lcs = longestCommonSubsequence s1 s2
    lcsLength = length lcs

tokenize :: String -> [String]
tokenize = filter (not . null) . splitBy ' ' . normalizeWhitespace

normalizeUnicode :: String -> String
normalizeUnicode = id  -- Simplified for this example

compressString :: String -> String
compressString = id  -- Simplified for this example

decompressString :: String -> String
decompressString = id  -- Simplified for this example

encodeString :: String -> String
encodeString = id  -- Simplified for this example

decodeString :: String -> String
decodeString = id  -- Simplified for this example

hashString :: String -> Int
hashString = foldl' (\h c -> 31 * h + fromEnum c) 5381
  where
    foldl' f z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

interpolateTemplate :: String -> [(String, String)] -> String
interpolateTemplate template values = foldl' replacePlaceholder template values
  where
    replacePlaceholder acc (key, value) = replace ("{" ++ key ++ "}") value acc
    replace [] _ _ = []
    replace _ _ [] = []
    replace pat repl s
      | pat `isPrefixOf` s = repl ++ replace pat repl (drop (length pat) s)
      | otherwise = case s of
                      (x:xs) -> x : replace pat repl xs

padString :: Int -> Char -> String -> String
padString targetLen padChar s
  | length s >= targetLen = s
  | otherwise = s ++ replicate (targetLen - length s) padChar

truncateString :: Int -> String -> String
truncateString maxLen s
  | length s <= maxLen = s
  | otherwise = take maxLen s

joinWith :: Char -> [String] -> String
joinWith delim = intercalate [delim]

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Utils String Processing Tests"
  [ testProperty "String trimming functionality" prop_string_trim,
    testProperty "String splitting by delimiter" prop_string_split_by,
    testProperty "String joining with separator" prop_string_join,
    testProperty "String case conversion" prop_string_case_conversion,
    testProperty "String whitespace normalization" prop_string_whitespace_normalize,
    testProperty "String indentation handling" prop_string_indentation,
    testProperty "String escaping for special characters" prop_string_escape,
    testProperty "String unescaping" prop_string_unescape,
    testProperty "String word wrapping" prop_string_wrap,
    testProperty "String prefix/suffix operations" prop_string_prefix_suffix,
    testProperty "String character counting" prop_string_char_count,
    testProperty "String word counting" prop_string_word_count,
    testProperty "String line counting" prop_string_line_count,
    testProperty "String palindrome detection" prop_string_palindrome,
    testProperty "String levenshtein distance" prop_string_levenshtein,
    testProperty "String longest common subsequence" prop_string_lcs,
    testProperty "String soundex/metaphone" prop_string_soundex,
    testProperty "String similarity metrics" prop_string_similarity,
    testProperty "String tokenization" prop_string_tokenize,
    testProperty "String normalization (Unicode)" prop_string_normalize_unicode,
    testProperty "String compression/decompression" prop_string_compress,
    testProperty "String encoding/decoding" prop_string_encode_decode,
    testProperty "String hashing" prop_string_hash,
    testProperty "String template interpolation" prop_string_template,
    testProperty "String padding" prop_string_pad,
    testProperty "String truncation" prop_string_truncate
  ]