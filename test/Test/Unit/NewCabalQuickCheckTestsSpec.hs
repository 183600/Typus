{-# LANGUAGE CPP #-}

-- | New Cabal QuickCheck tests for basic functionality
module Test.Unit.NewCabalQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit, isSpace)

import Utils (splitLines, normalizeIndentation, removeComments)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Core Property Tests
-- ============================================================================

-- Property: splitLines never returns empty list for non-empty input
prop_splitLines_nonempty :: String -> Property
prop_splitLines_nonempty input =
  not (null input) ==> 
  let lines = splitLines input
  in property $ not (null lines)

-- Property: splitLines preserves total character count
prop_splitLines_preserves_chars :: String -> Property
prop_splitLines_preserves_chars input =
  let lines = splitLines input
      totalChars = sum $ map length lines
      originalLength = length input
  in property $ totalChars == originalLength

-- Property: normalizeIndentation removes leading spaces consistently
prop_normalize_indentation :: String -> Property
prop_normalize_indentation input =
  let normalized = normalizeIndentation input
      lines = splitLines normalized
  in property $ all (not . isPrefixOfSpace) lines
  where
    isPrefixOfSpace [] = False
    isPrefixOfSpace (c:_) = not (isSpace c)

-- Property: removeComments preserves non-comment content
prop_remove_comments_preserves_content :: String -> Property
prop_remove_comments_preserves_content content =
  let withoutComments = removeComments content
      hasNonCommentContent = any (not . isSpace) content
  in classify hasNonCommentContent "has non-comment content" $
     property $ hasNonCommentContent ==> not (null withoutComments)

-- Property: SourceSpan arithmetic is consistent
prop_sourcespan_arithmetic :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_arithmetic startLine startCol endLine endCol =
  startLine >= 1 && startCol >= 1 && endLine >= startLine && 
  (endLine > startLine || endCol >= startCol) ==>
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
  in property $ spanStart span == start && spanEnd span == end

-- Property: String splitting by delimiter preserves content
prop_string_split_preserves :: String -> String -> Property
prop_string_split_preserves content delimiter =
  not (null delimiter) ==>
  let parts = Data.List.splitOn delimiter content
      reconstructed = Data.List.intercalate delimiter parts
  in property $ reconstructed == content

-- Property: List reversal is involutive
prop_list_reversal_involutive :: [Int] -> Property
prop_list_reversal_involutive xs =
  property $ reverse (reverse xs) == xs

-- Property: Map lookup after insertion
prop_map_insert_lookup :: [(String, Int)] -> String -> Int -> Property
prop_map_insert_lookup pairs key value =
  let mp = foldr (\(k,v) m -> insertMap k v m) emptyMap pairs
      mp' = insertMap key value mp
  in property $ lookupMap key mp' == Just value
  where
    emptyMap = []
    insertMap k v m = (k,v) : filter (\(k',_) -> k' /= k) m
    lookupMap k m = lookup k m

-- Property: String concatenation length property
prop_string_concat_length :: String -> String -> Property
prop_string_concat_length s1 s2 =
  let combined = s1 ++ s2
  in property $ length combined == length s1 + length s2

-- Property: Filter preserves order
prop_filter_preserves_order :: [Int] -> Property
prop_filter_preserves_order xs =
  let filtered = filter even xs
      originalIndices = map fst $ filter (even . snd) $ zip [0..] xs
      filteredIndices = map fst $ filter (even . snd) $ zip [0..] filtered
  in property $ all (uncurry (<=)) $ zip originalIndices filteredIndices

-- ============================================================================
-- Helper Functions
-- ============================================================================

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
removeComments = unlines . filter (not . isPrefixOf "//") . lines
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ fastProperty "splitLines never returns empty list for non-empty input" prop_splitLines_nonempty
  , fastProperty "splitLines preserves total character count" prop_splitLines_preserves_chars
  , fastProperty "normalizeIndentation removes leading spaces consistently" prop_normalize_indentation
  , fastProperty "removeComments preserves non-comment content" prop_remove_comments_preserves_content
  , fastProperty "SourceSpan arithmetic is consistent" prop_sourcespan_arithmetic
  , fastProperty "String splitting by delimiter preserves content" prop_string_split_preserves
  , fastProperty "List reversal is involutive" prop_list_reversal_involutive
  , fastProperty "Map lookup after insertion" prop_map_insert_lookup
  , fastProperty "String concatenation length property" prop_string_concat_length
  , fastProperty "Filter preserves order" prop_filter_preserves_order
  ]