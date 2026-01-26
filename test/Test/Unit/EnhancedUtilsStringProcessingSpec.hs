{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.EnhancedUtilsStringProcessingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation)
import Data.Char (isSpace)

-- | 测试trim函数的属性：trim(trim(s)) == trim(s)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | 测试trim函数的属性：trim(s)的长度不大于s的长度
prop_trim_length :: String -> Property
prop_trim_length s = property (length (trim s) <= length s)

-- | 测试splitBy函数的属性：splitBy delim "" == [""]
prop_splitBy_empty :: Property
prop_splitBy_empty = splitBy ',' "" === []

-- | 测试splitByCollapsed函数的属性：fold (splitByCollapsed delim) (splitBy delim s) == splitByCollapsed delim s
prop_splitBy_collapsed_consistency :: String -> Property
prop_splitBy_collapsed_consistency s = 
  let delim = ','
  in splitByCollapsed delim (concat (splitBy delim s)) === splitByCollapsed delim s

-- | 测试splitByComma函数的属性：splitByComma == splitBy ','
prop_splitBy_comma_consistency :: String -> Property
prop_splitBy_comma_consistency s = splitByComma s === splitBy ',' s

-- | 测试splitByCommaCollapsed函数的属性：splitByCommaCollapsed == splitByCollapsed ','
prop_splitBy_comma_collapsed_consistency :: String -> Property
prop_splitBy_comma_collapsed_consistency s = splitByCommaCollapsed s === splitByCollapsed ',' s

-- | 测试removeLineComments函数的属性：removeLineComments不会改变不含注释的字符串
prop_remove_line_comments_no_comment :: String -> Property
prop_remove_line_comments_no_comment s = 
  not ('/' `elem` s) ==> removeLineComments s === s

-- | 测试removeComments函数的属性：removeComments不会改变不含注释的字符串
prop_remove_comments_no_comment :: String -> Property
prop_remove_comments_no_comment s = 
  not ('/' `elem` s) ==> removeComments s === s

-- | 测试normalizeIndentation函数的属性：normalizeIndentation不会增加字符串的行数
prop_normalize_indentation_lines :: String -> Property
prop_normalize_indentation_lines s = 
  let originalLines = length (lines s)
      normalizedLines = length (lines (normalizeIndentation s))
  in property (normalizedLines <= originalLines)

-- | 测试normalizeIndentation函数的属性：normalizeIndentation不会改变非缩进字符
prop_normalize_indentation_content :: String -> Property
prop_normalize_indentation_content s = 
  let filtered = filter (not . isSpace) s
      normalizedFiltered = filter (not . isSpace) (normalizeIndentation s)
  in normalizedFiltered === filtered

tests :: TestTree
tests = testGroup "Enhanced Utils String Processing Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim length" prop_trim_length
  , testProperty "splitBy empty" prop_splitBy_empty
  , testProperty "splitBy collapsed consistency" prop_splitBy_collapsed_consistency
  , testProperty "splitBy comma consistency" prop_splitBy_comma_consistency
  , testProperty "splitBy comma collapsed consistency" prop_splitBy_comma_collapsed_consistency
  , testProperty "remove line comments no comment" prop_remove_line_comments_no_comment
  , testProperty "remove comments no comment" prop_remove_comments_no_comment
  , testProperty "normalize indentation lines" prop_normalize_indentation_lines
  , testProperty "normalize indentation content" prop_normalize_indentation_content
  ]