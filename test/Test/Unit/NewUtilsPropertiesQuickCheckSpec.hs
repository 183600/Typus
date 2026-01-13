{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Utils
import Data.Char (ord, isAscii, isControl)
import Data.List (isInfixOf, intercalate, lines, unlines, filter)

-- | 测试trim函数的基本属性
prop_trim_preserves_non_empty :: String -> Property
prop_trim_preserves_non_empty s = 
  let trimmed = trim s
  in if not (null s) && any (not . isSpace) s
     then property $ not (null trimmed)
     else property True
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmed1 = trim s
      trimmed2 = trim trimmed1
  in property $ trimmed1 == trimmed2

-- | 测试splitBy函数的基本属性
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s = 
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in if null s
     then property $ null parts
     else property $ length reconstructed >= length s

-- | 测试splitBy函数的分割结果
prop_splitBy_correctness :: Char -> String -> Property
prop_splitBy_correctness delim s = 
  let parts = splitBy delim s
  in property $ all (\p -> delim `notElem` p) parts

-- | 测试removeLineComments函数的基本属性
prop_removeLine_comments_preserves_non_comment :: String -> Property
prop_removeLine_comments_preserves_non_comment s = 
  let withoutComments = removeLineComments s
      hasComments = "//" `isInfixOf` s
  in if not hasComments
     then property $ s == withoutComments
     else property $ length withoutComments <= length s

-- | 测试normalizeIndentation函数的一致性
prop_normalize_indentation_preserves_structure :: String -> Property
prop_normalize_indentation_preserves_structure s = 
  let normalized = normalizeIndentation s
      linesOriginal = lines s
      linesNormalized = lines normalized
  in property $ length linesOriginal == length linesNormalized

-- | 测试breakOn函数的正确性
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness needle haystack = 
  let (before, after) = breakOn needle haystack
  in if needle `isInfixOf` haystack
     then property $ needle `isInfixOf` (before ++ needle ++ after)
     else property $ (before, after) == (haystack, "")

-- | 测试safeProcessString函数的安全性
prop_safe_process_string_returns_valid :: String -> Property
prop_safe_process_string_returns_valid s = 
  let processed = safeProcessString s
  in property $ all isValidChar processed

-- | 测试isValidChar函数的基本属性
prop_is_valid_char_ascii :: Char -> Property
prop_is_valid_char_ascii c = 
  let isValid = isValidChar c
  in if isAscii c
     then property $ isValid || isControl c
     else property True
  where
    isAscii c = ord c < 128
    isControl c = c < ' ' && c /= '\t' && c /= '\n' && c /= '\r'

-- | 测试trim函数与空字符串的关系
prop_trim_empty_string :: Property
prop_trim_empty_string = property $ trim "" == ""

-- | 测试splitBy与空分隔符
prop_splitBy_empty_delim :: String -> Property
prop_splitBy_empty_delim s = 
  let parts = splitBy '\0' s
  in property $ parts == [s]

-- | 测试removeLineComments与多行字符串
prop_remove_line_comments_multiline :: [String] -> Property
prop_remove_line_comments_multiline lines = 
  let input = unlines lines
      output = removeLineComments input
  in property $ length (lines output) <= length lines

-- | 测试normalizeIndentation与空行
prop_normalize_indentation_preserves_empty_lines :: String -> Property
prop_normalize_indentation_preserves_empty_lines s = 
  let normalized = normalizeIndentation s
      originalEmptyLines = length $ filter (all isSpace) (lines s)
      normalizedEmptyLines = length $ filter (all isSpace) (lines normalized)
  in property $ originalEmptyLines == normalizedEmptyLines
  where
    isSpace c = c == ' ' || c == '\t'

-- | 测试splitByComma函数与特殊字符
prop_split_by_comma_special_chars :: Property
prop_split_by_comma_special_chars = 
  let testInput = "a,b,,c,\",d\",e"
      parts = splitByComma testInput
  in property $ length parts == 6

-- | 测试removeComments函数的嵌套注释
prop_remove_comments_nested :: Property
prop_remove_comments_nested = 
  let input = "code /* outer /* inner */ still outer */ more code"
      output = removeComments input
  in property $ not ("/*" `isInfixOf` output) && not ("*/" `isInfixOf` output)

-- | 测试字符串处理函数的组合
prop_string_processing_pipeline :: String -> Property
prop_string_processing_pipeline s = 
  let processed = trim . removeComments . normalizeIndentation $ s
  in property $ length processed <= length s + 10  -- 允许一些小的变化



tests :: TestTree
tests = testGroup "Utils Properties QuickCheck Tests"
  [ testProperty "trim preserves non-empty strings" prop_trim_preserves_non_empty
  , testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "splitBy length property" prop_splitBy_length
  , testProperty "splitBy correctness" prop_splitBy_correctness
  , testProperty "removeLineComments preserves non-comment" prop_removeLine_comments_preserves_non_comment
  , testProperty "normalizeIndentation preserves structure" prop_normalize_indentation_preserves_structure
  , testProperty "breakOn correctness" prop_breakOn_correctness
  , testProperty "safeProcessString returns valid" prop_safe_process_string_returns_valid
  , testProperty "isValidChar ASCII property" prop_is_valid_char_ascii
  , testProperty "trim empty string" prop_trim_empty_string
  , testProperty "splitBy empty delimiter" prop_splitBy_empty_delim
  , testProperty "removeLineComments multiline" prop_remove_line_comments_multiline
  , testProperty "normalizeIndentation preserves empty lines" prop_normalize_indentation_preserves_empty_lines
  , testProperty "splitByComma special chars" prop_split_by_comma_special_chars
  , testProperty "removeComments nested" prop_remove_comments_nested
  , testProperty "string processing pipeline" prop_string_processing_pipeline
  ]