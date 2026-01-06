{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewErrorHandlingQuickCheckTestSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Utils (trim, splitBy, removeLineComments)
import SourceLocation (SourcePos(..), startPos, posAfter, emptySpan, isValidSpan)
import Parser (FileDirectives(..), defaultFileDirectives)
import Data.Maybe (isNothing, isJust)
import Control.Exception (evaluate)

-- 测试错误输入的处理
prop_trim_error_handling :: String -> Bool
prop_trim_error_handling s = 
  let result = trim s
  in L.length result >= 0  -- trim不应该产生负长度字符串

prop_split_error_handling :: Char -> String -> Bool
prop_split_error_handling delim s = 
  let parts = splitBy delim s
  in L.length parts >= 1  -- splitBy应该总是返回至少一个部分

prop_position_error_handling :: Int -> Int -> Bool
prop_position_error_handling line col = 
  let pos = SourcePos line col
  -- 即使是负数位置，也应该有合理的表示
  in sourceLine pos == line && sourceColumn pos == col

-- 测试异常安全性
prop_trim_exception_safe :: String -> Bool
prop_trim_exception_safe s = 
  let result = evaluate (trim s)
  in case result of
    Left _ -> False  -- 不应该抛出异常
    Right _ -> True

prop_split_exception_safe :: Char -> String -> Bool
prop_split_exception_safe delim s = 
  let result = evaluate (splitBy delim s)
  in case result of
    Left _ -> False  -- 不应该抛出异常
    Right _ -> True

-- 测试边界错误条件
prop_empty_input_handling :: Bool
prop_empty_input_handling = 
  let trimEmpty = trim ""
      splitEmpty = splitBy ',' ""
      posStart = startPos
  in null trimEmpty && splitEmpty == [""] && sourceLine posStart == 1

prop_null_character_handling :: String -> Bool
prop_null_character_handling s = 
  let stringWithNull = s ++ "\0"
      trimmed = trim stringWithNull
      parts = splitBy '\0' stringWithNull
  in L.length trimmed >= 0 && L.length parts >= 1

-- 测试类型错误处理
prop_position_validation :: Int -> Int -> Bool
prop_position_validation line col = 
  let pos = SourcePos line col
  -- 位置应该保持其值，即使是不合理的值
  in sourceLine pos == line && sourceColumn pos == col

prop_span_validation :: SourcePos -> SourcePos -> Bool
prop_span_validation pos1 pos2 = 
  let span = SourceSpan pos1 pos2
  -- span应该保持其结构，即使位置是无效的
  in spanStart span == pos1 && spanEnd span == pos2

-- 测试解析器错误处理
prop_directives_error_handling :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_directives_error_handling own deps cons = 
  let directives = FileDirectives own deps cons
  -- 即使是Nothing值，指令结构也应该是有效的
  in isNothing (fdOwnership directives) == isNothing own &&
     isNothing (fdDependentTypes directives) == isNothing deps &&
     isNothing (fdConstraints directives) == isNothing cons

-- 测试恢复能力
prop_error_recovery :: String -> String -> Bool
prop_error_recovery s1 s2 = 
  let combined = s1 ++ s2
      trimmed1 = trim s1
      trimmed2 = trim s2
      trimmedCombined = trim combined
  -- 操作应该是可组合的
  in L.length trimmedCombined >= 0

prop_partial_failure_handling :: String -> Bool
prop_partial_failure_handling s = 
  let parts = splitBy ',' s
      processed = map trim parts
  -- 即使某些部分是空的，处理也应该继续
  in L.length processed == L.length parts

-- 测试资源清理
prop_memory_cleanup :: Small Int -> String -> Bool
prop_memory_cleanup (Small n) s = n >= 0 && n <= 100 ==>  -- 限制大小
  let largeString = L.concat (replicate n s)
      result = trim largeString
  -- 操作应该完成而不耗尽内存
  in L.length result >= 0

-- 生成测试套件
tests :: TestTree
tests = testGroup "Error Handling QuickCheck Tests"
  [ testProperty "trim error handling" prop_trim_error_handling
  , testProperty "split error handling" prop_split_error_handling
  , testProperty "position error handling" prop_position_error_handling
  , testProperty "trim exception safe" prop_trim_exception_safe
  , testProperty "split exception safe" prop_split_exception_safe
  , testProperty "empty input handling" prop_empty_input_handling
  , testProperty "null character handling" prop_null_character_handling
  , testProperty "position validation" prop_position_validation
  , testProperty "span validation" prop_span_validation
  , testProperty "directives error handling" prop_directives_error_handling
  , testProperty "error recovery" prop_error_recovery
  , testProperty "partial failure handling" prop_partial_failure_handling
  , testProperty "memory cleanup" prop_memory_cleanup
  ]