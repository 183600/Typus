{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewUtilsQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- 测试trim函数的性质
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in null trimmed || 
     (not (L.null (L.head trimmed)) && L.head trimmed `notElem` " \t\n\r") &&
     (not (L.null (last trimmed)) && last trimmed `notElem` " \t\n\r")

-- 测试splitBy函数的性质
prop_split_by_empty_delim :: Char -> String -> Bool
prop_split_by_empty_delim delim s = L.length (splitBy delim s) >= 1

prop_split_by_comma_consistency :: String -> Bool
prop_split_by_comma_consistency s = splitBy ',' s == splitByComma s

prop_split_by_collapsed_fold :: Char -> String -> Bool
prop_split_by_collapsed_fold delim s = 
  let normal = splitBy delim
      collapsed = splitByCollapsed delim
  in L.all (not . null) collapsed

-- 测试注释移除函数的性质
prop_remove_line_comments_no_comment :: String -> Bool  
prop_remove_line_comments_no_comment s = not ('/' `elem` s) ==> removeLineComments s == s

prop_remove_line_comments_preserves_non_comment :: String -> Bool
prop_remove_line_comments_preserves_non_comment s = 
  let withoutComments = removeLineComments s
      linesBefore = lines s
      linesAfter = lines withoutComments
  in L.length linesBefore >= L.length linesAfter

-- 测试字符串处理的整体性质
prop_trim_split_consistency :: String -> Bool
prop_trim_split_consistency s = 
  let trimmed = trim s
      parts = splitBy ',' trimmed
  in L.all (trim . (trim)) parts

-- 生成测试套件
tests :: TestTree
tests = testGroup "Utils QuickCheck Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim removes leading/trailing spaces" prop_trim_no_leading_trailing_spaces
  , testProperty "splitBy always returns at least one part" prop_split_by_empty_delim
  , testProperty "splitByComma equals splitBy with ','" prop_split_by_comma_consistency
  , testProperty "splitByCollapsed removes empty parts" prop_split_by_collapsed_fold
  , testProperty "removeLineComments preserves non-comment lines" prop_remove_line_comments_preserves_non_comment
  , testProperty "trim L.and split consistency" prop_trim_split_consistency
  ]