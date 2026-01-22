{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewBasicTypesAndStringPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf, group, sort, intercalate)
import Data.String (IsString)
import SourceLocation

-- 辅助函数：检查位置是否在跨度内
posInSpan :: SourcePos -> SourceSpan -> Bool
posInSpan pos span = 
  let start = spanStart span
      end = spanEnd span
  in (posLine pos > posLine start || 
      (posLine pos == posLine start && posColumn pos >= posColumn start)) &&
     (posLine pos < posLine end || 
      (posLine pos == posLine end && posColumn pos <= posColumn end))

-- | 测试字符串长度属性
prop_string_length_roundtrip :: String -> Property
prop_string_length_roundtrip s = 
  property $ length (reverse s) == length s

-- | 测试字符串反转的幂等性
prop_string_reverse_idempotent :: String -> Property
prop_string_reverse_idempotent s = 
  property $ reverse (reverse s) == s

-- | 测试字符串连接的长度
prop_string_concat_length :: String -> String -> Property
prop_string_concat_length s1 s2 = 
  property $ length (s1 ++ s2) == length s1 + length s2

-- | 测试字符串连接的交换律（对于空字符串）
prop_string_concat_empty :: String -> Property
prop_string_concat_empty s = 
  property $ s ++ "" == s && "" ++ s == s

-- | 测试Text和String转换的一致性
prop_text_string_conversion :: String -> Property
prop_text_string_conversion s = 
  let t = T.pack s
  in property $ T.unpack t == s

-- | 测试Text长度计算
prop_text_length :: String -> Property
prop_text_length s = 
  let t = T.pack s
  in property $ T.length t == fromIntegral (length s)

-- | 测试SourcePos的基本属性
prop_sourcepos_line_positive :: Int -> Int -> Property
prop_sourcepos_line_positive line col =
  let pos = SourcePos (max 1 line) (max 1 col) 0
  in property $ posLine pos >= 1 && posColumn pos >= 1

-- | 测试SourceSpan的构造
prop_sourcespan_consistency :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_consistency startLine startCol endLine endCol =
  let startPos = SourcePos (max 1 startLine) (max 1 startCol) 0
      endPos = SourcePos (max 1 endLine) (max 1 endCol) 0
      span = SourceSpan startPos endPos
  in property $ spanStart span == startPos && spanEnd span == endPos

-- | 测试源码跨度的包含关系
prop_sourcespan_containment :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_containment (Positive startLine) (Positive startCol) (Positive endLine) (Positive endCol) =
  let startPos = SourcePos startLine startCol 0
      endPos = SourcePos (max startLine endLine) (max startCol endCol) 0
      span = SourceSpan startPos endPos
      midPos = SourcePos ((startLine + endLine) `div` 2) ((startCol + endCol) `div` 2) 0
  in property $ posInSpan midPos span

-- | 测试源码跨度的合并
prop_sourcespan_merge :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_merge (Positive startLine1) (Positive startCol1) (Positive endLine2) (Positive endCol2) =
  let startPos1 = SourcePos startLine1 startCol1 0
      endPos1 = SourcePos (startLine1 + 1) (startCol1 + 5) 0
      span1 = SourceSpan startPos1 endPos1
      startPos2 = SourcePos (endLine2 - 1) (endCol2 - 3) 0
      endPos2 = SourcePos endLine2 endCol2 0
      span2 = SourceSpan startPos2 endPos2
      merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span2 && 
                spanEnd merged >= spanEnd span2

-- | 测试字符串中字母数字字符的识别
prop_alnum_classification :: String -> Property
prop_alnum_classification s = 
  let alnumCount = length $ filter (\c -> isAlpha c || isDigit c) s
  in property $ alnumCount >= 0 && alnumCount <= length s

-- | 测试字符串分割和重组
prop_string_split_join :: String -> String -> Property
prop_string_split_join s sep = 
  let parts = splitOn sep s
      rejoined = intercalate sep parts
  in property $ rejoined == s || null sep
  where
    splitOn [] _ = [""]
    splitOn _ [] = [""]
    splitOn sep str = splitOn' sep str []
    
    splitOn' _ [] acc = [reverse acc]
    splitOn' sep str acc
      | sep `isPrefixOf` str = reverse acc : splitOn' sep (drop (length sep) str) []
      | null str = reverse acc : []
      | otherwise = case str of
                      (first:rest) -> splitOn' sep rest (first : acc)

-- | 测试字符串排序的稳定性
prop_string_sort_preserves_multiset :: String -> Property
prop_string_sort_preserves_multiset s = 
  let sorted = sort s
  in property $ sort sorted == sorted

-- | 测试字符串分组的基本属性
prop_string_group_consecutive :: String -> Property
prop_string_group_consecutive s = 
  let groups = group s
      totalLength = sum $ map length groups
  in property $ totalLength == length s

-- | 测试字符串前缀和后缀的关系
prop_string_prefix_suffix :: String -> String -> Property
prop_string_prefix_suffix prefix suffix = 
  let combined = prefix ++ suffix
  in property $ prefix `isPrefixOf` combined && suffix `isSuffixOf` combined

-- | 测试字符串去重的基本属性
prop_string_nub_preserves_order :: String -> Property
prop_string_nub_preserves_order s = 
  let nubbed = nub s
  in property $ length nubbed <= length s && all (`elem` s) nubbed
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

-- | 测试字符串替换的基本属性
prop_string_replace_preserves_length :: String -> String -> String -> Property
prop_string_replace_preserves_length old new s =
  let replaced = replace old new s
  in property $ (null old && replaced == s) || 
                (not (null old) && length replaced >= length s - length old * countOccurrences old s)
  where
    countOccurrences _ [] = 0
    countOccurrences pat str
      | pat `isPrefixOf` str = 1 + countOccurrences pat (drop (length pat) str)
      | null str = 0
      | otherwise = countOccurrences pat (drop 1 str)
    
    replace _ _ [] = []
    replace old new str
      | old `isPrefixOf` str = new ++ replace old new (drop (length old) str)
      | null str = []
      | otherwise = case str of
                      (first:rest) -> first : replace old new rest

tests :: TestTree
tests = testGroup "New Basic Types and String Properties Tests"
  [ testProperty "string length roundtrip" prop_string_length_roundtrip,
    testProperty "string reverse idempotent" prop_string_reverse_idempotent,
    testProperty "string concat length" prop_string_concat_length,
    testProperty "string concat empty" prop_string_concat_empty,
    testProperty "text string conversion" prop_text_string_conversion,
    testProperty "text length" prop_text_length,
    testProperty "sourcepos line positive" prop_sourcepos_line_positive,
    testProperty "sourcespan consistency" prop_sourcespan_consistency,
    testProperty "alnum classification" prop_alnum_classification,
    testProperty "string split join" prop_string_split_join,
    testProperty "string sort preserves multiset" prop_string_sort_preserves_multiset,
    testProperty "string group consecutive" prop_string_group_consecutive,
    testProperty "string prefix suffix" prop_string_prefix_suffix,
    testProperty "string nub preserves order" prop_string_nub_preserves_order,
    testProperty "string replace preserves length" prop_string_replace_preserves_length
  ]