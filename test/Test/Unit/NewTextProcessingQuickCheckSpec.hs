{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Text processing QuickCheck tests
-- This module contains property-based tests for text processing functions
module Test.Unit.NewTextProcessingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (toUpper, toLower, isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (foldM)

-- ============================================================================
-- Basic Text Operations Tests
-- ============================================================================

-- | Test Text conversion: String to Text and back
prop_text_roundtrip :: String -> Bool
prop_text_roundtrip s = T.unpack (T.pack s) == s

-- | Test Text length consistency
prop_text_length :: String -> Bool
prop_text_length s = T.length (T.pack s) == length s

-- | Test Text null consistency
prop_text_null :: String -> Bool
prop_text_null s = T.null (T.pack s) == null s

-- | Test Text append consistency
prop_text_append :: String -> String -> Bool
prop_text_append s1 s2 = 
  T.append (T.pack s1) (T.pack s2) == T.pack (s1 ++ s2)

-- | Test Text concat consistency
prop_text_concat :: [String] -> Bool
prop_text_concat strings = 
  T.concat (map T.pack strings) == T.pack (concat strings)

-- ============================================================================
-- Text Splitting Tests
-- ============================================================================

-- | Test Text split on single character
prop_text_splitOn_char :: Char -> String -> Bool
prop_text_splitOn_char c s = 
  let pattern = T.singleton c
      text = T.pack s
      result = T.splitOn pattern text
  in map T.unpack result == splitBy c s

-- | Test Text split on string
prop_text_splitOn_string :: String -> String -> Property
prop_text_splitOn_string pattern s = 
  not (null pattern) ==> property $
    let patternText = T.pack pattern
        text = T.pack s
        result = T.splitOn patternText text
    in map T.unpack result == splitByString pattern s
-- | Test Text lines consistency
prop_text_lines :: String -> Bool
prop_text_lines s = 
  let text = T.pack s
      result = T.lines text
  in map T.unpack result == lines s

-- | Test Text words consistency
prop_text_words :: String -> Bool
prop_text_words s = 
  let text = T.pack s
      result = T.words text
  in map T.unpack result == words s

-- ============================================================================
-- Text Searching Tests
-- ============================================================================

-- | Test Text isPrefixOf
prop_text_isPrefixOf :: String -> String -> Bool
prop_text_isPrefixOf prefix s = 
  let prefixText = T.pack prefix
      text = T.pack s
  in T.isPrefixOf prefixText text == isPrefixOf prefix s

-- | Test Text isSuffixOf
prop_text_isSuffixOf :: String -> String -> Bool
prop_text_isSuffixOf suffix s = 
  let suffixText = T.pack suffix
      text = T.pack s
  in T.isSuffixOf suffixText text == isSuffixOf suffix s

-- | Test Text isInfixOf
prop_text_isInfixOf :: String -> String -> Bool
prop_text_isInfixOf infixStr s = 
  let infixText = T.pack infixStr
      text = T.pack s
  in T.isInfixOf infixText text == isInfixOf infixStr s

-- | Test Text find substring
prop_text_find :: String -> String -> Property
prop_text_find pattern s = 
  not (null pattern) ==> property $
    let patternText = T.pack pattern
        text = T.pack s
        result = case pattern of (c:_) -> T.find (== c) text; [] -> Nothing
    in case result of
         Nothing -> not (pattern `isInfixOf` s)
         Just foundText -> T.singleton foundText `T.isInfixOf` T.pack s

-- ============================================================================
-- Text Transformation Tests
-- ============================================================================

-- | Test Text toUpper
prop_text_toUpper :: String -> Bool
prop_text_toUpper s = 
  let text = T.pack s
      result = T.toUpper text
  in T.unpack result == map toUpper s

-- | Test Text toLower
prop_text_toLower :: String -> Bool
prop_text_toLower s = 
  let text = T.pack s
      result = T.toLower text
  in T.unpack result == map toLower s

-- | Test Text reverse
prop_text_reverse :: String -> Bool
prop_text_reverse s = 
  let text = T.pack s
      result = T.reverse text
  in T.unpack result == reverse s

-- | Test Text filter
prop_text_filter :: String -> Bool
prop_text_filter s = 
  let text = T.pack s
      result = T.filter isAlphaNum text
  in T.unpack result == filter isAlphaNum s

-- | Test Text map
prop_text_map :: String -> Bool
prop_text_map s = 
  let text = T.pack s
      result = T.map toUpper text
  in T.unpack result == map toUpper s

-- ============================================================================
-- Text Whitespace Tests
-- ============================================================================

-- | Test Text strip
prop_text_strip :: String -> Bool
prop_text_strip s = 
  let text = T.pack s
      result = T.strip text
  in T.unpack result == trim s

-- | Test Text stripStart
prop_text_stripStart :: String -> Bool
prop_text_stripStart s = 
  let text = T.pack s
      result = T.stripStart text
  in T.unpack result == dropWhile isSpace s

-- | Test Text stripEnd
prop_text_stripEnd :: String -> Bool
prop_text_stripEnd s = 
  let text = T.pack s
      result = T.stripEnd text
  in T.unpack result == reverse (dropWhile isSpace (reverse s))

-- | Test Text stripPrefix
prop_text_stripPrefix :: String -> String -> Bool
prop_text_stripPrefix prefix s = 
  let prefixText = T.pack prefix
      text = T.pack s
      result = T.stripPrefix prefixText text
  in case result of
    Nothing -> not (prefix `isPrefixOf` s)
    Just stripped -> T.unpack stripped == drop (length prefix) s

-- ============================================================================
-- Text Replacement Tests
-- ============================================================================

-- | Test Text replace
prop_text_replace :: String -> String -> String -> Property
prop_text_replace old new s = 
  not (null old) ==> property $
    let oldText = T.pack old
        newText = T.pack new
        text = T.pack s
        result = T.replace oldText newText text
    in T.unpack result == replaceString old new s

-- | Test Text replace with empty old
prop_text_replace_empty :: String -> String -> Bool
prop_text_replace_empty new s = 
  let newText = T.pack new
      text = T.pack s
      result = T.replace T.empty newText text
  in T.unpack result == intercalate new (map (:[]) s)

-- ============================================================================
-- Text Comparison Tests
-- ============================================================================

-- | Test Text comparison operators
prop_text_comparison :: String -> String -> Bool
prop_text_comparison s1 s2 = 
  let text1 = T.pack s1
      text2 = T.pack s2
  in (text1 == text2) == (s1 == s2) &&
     (text1 /= text2) == (s1 /= s2) &&
     (text1 < text2) == (s1 < s2) &&
     (text1 <= text2) == (s1 <= s2) &&
     (text1 > text2) == (s1 > s2) &&
     (text1 >= text2) == (s1 >= s2)

-- | Test Text compare
prop_text_compare :: String -> String -> Bool
prop_text_compare s1 s2 = 
  let text1 = T.pack s1
      text2 = T.pack s2
  in compare text1 text2 == compare s1 s2

-- | Test Text case-insensitive comparison
prop_text_caseInsensitive :: String -> String -> Bool
prop_text_caseInsensitive s1 s2 = 
  let text1 = T.pack s1
      text2 = T.pack s2
      lower1 = T.toLower text1
      lower2 = T.toLower text2
  in (lower1 == lower2) == (map toLower s1 == map toLower s2)

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | Test Text with empty string
prop_text_empty :: Bool
prop_text_empty = 
  let text = T.pack ""
  in T.null text && T.length text == 0 && T.unpack text == ""

-- | Test Text with single character
prop_text_single :: Char -> Bool
prop_text_single c = 
  let text = T.pack [c]
  in T.length text == 1 && T.head text == c && T.unpack text == [c]

-- | Test Text with unicode content
prop_text_unicode :: String -> Bool
prop_text_unicode s = 
  let text = T.pack s
  in T.unpack text == s

-- | Test Text with special characters
prop_text_special :: String -> Bool
prop_text_special s = 
  let text = T.pack s
  in T.unpack text == s

-- | Test Text with whitespace only
prop_text_whitespace :: String -> Property
prop_text_whitespace s = 
  all isSpace s ==> property $
    let text = T.pack s
        stripped = T.strip text
    in T.null stripped

-- Helper functions
splitBy :: Char -> String -> [String]
splitBy c s = case break (==c) s of
  (a, []) -> [a]
  (a, _:b) -> a : splitBy c b

splitByString :: String -> String -> [String]
splitByString _ [] = [""]
splitByString pattern s = 
  if pattern `isPrefixOf` s
  then "" : splitByString pattern (drop (length pattern) s)
  else case s of
    (c:cs) -> case splitByString pattern cs of
      [] -> [[c]]
      (x:xs) -> (c:x) : xs

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Note: Using Data.Char functions instead of custom implementations

replaceString :: String -> String -> String -> String
replaceString _ _ [] = []
replaceString old new (c:cs) = 
  if old `isPrefixOf` (c:cs)
  then new ++ replaceString old new (drop (length old) (c:cs))
  else c : replaceString old new cs

-- Note: Using Data.List intercalate function

tests :: TestTree
tests = testGroup "New Text Processing QuickCheck Tests"
  [ testProperty "text roundtrip" prop_text_roundtrip
  , testProperty "text length" prop_text_length
  , testProperty "text null" prop_text_null
  , testProperty "text append" prop_text_append
  , testProperty "text concat" prop_text_concat
  , testProperty "text splitOn char" prop_text_splitOn_char
  , testProperty "text splitOn string" prop_text_splitOn_string
  , testProperty "text lines" prop_text_lines
  , testProperty "text words" prop_text_words
  , testProperty "text isPrefixOf" prop_text_isPrefixOf
  , testProperty "text isSuffixOf" prop_text_isSuffixOf
  , testProperty "text isInfixOf" prop_text_isInfixOf
  , testProperty "text find" prop_text_find
  , testProperty "text toUpper" prop_text_toUpper
  , testProperty "text toLower" prop_text_toLower
  , testProperty "text reverse" prop_text_reverse
  , testProperty "text filter" prop_text_filter
  , testProperty "text map" prop_text_map
  , testProperty "text strip" prop_text_strip
  , testProperty "text stripStart" prop_text_stripStart
  , testProperty "text stripEnd" prop_text_stripEnd
  , testProperty "text stripPrefix" prop_text_stripPrefix
  , testProperty "text replace" prop_text_replace
  , testProperty "text replace empty" prop_text_replace_empty
  , testProperty "text comparison" prop_text_comparison
  , testProperty "text compare" prop_text_compare
  , testProperty "text caseInsensitive" prop_text_caseInsensitive
  , testProperty "text empty" prop_text_empty
  , testProperty "text single" prop_text_single
  , testProperty "text unicode" prop_text_unicode
  , testProperty "text special" prop_text_special
  , testProperty "text whitespace" prop_text_whitespace
  ]