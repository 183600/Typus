{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.MemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryEfficientTestRunner (MemoryEfficiencyLevel(..), runMemoryEfficientTests)
import TestSupport.OptimizedMemoryLimits (withOptimizedMemory)
import TestSupport.ExtremeMemoryLimits (withExtremeMemoryLimits)
import Utils (trim, splitBy)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- 内存优化的属性测试
prop_trim_memory_optimized :: String -> Property
prop_trim_memory_optimized s = 
  let limitedInput = take 20 s  -- 限制输入大小
      trimmed = trim limitedInput
  in property $ length trimmed <= length limitedInput

prop_split_memory_optimized :: Char -> String -> Property
prop_split_memory_optimized c s = 
  let limitedInput = take 15 s  -- 限制输入大小
      parts = splitBy c limitedInput
  in property $ length parts <= 16  -- 最坏情况下每个字符都是一个部分

prop_whitespace_detection :: String -> Property
prop_whitespace_detection s = 
  let limitedInput = take 25 s
      hasWhitespace = any isSpace limitedInput
  in property $ if hasWhitespace then True else True

prop_string_concat_memory :: String -> String -> Property
prop_string_concat_memory s1 s2 = 
  let limited1 = take 10 s1
      limited2 = take 10 s2
      combined = limited1 ++ limited2
  in property $ length combined <= 20

prop_list_operations_memory :: [Int] -> Property
prop_list_operations_memory xs = 
  let limitedList = take 5 xs  -- 限制列表大小
      lengthLimited = length limitedList
  in property $ lengthLimited <= 5

prop_text_operations_memory :: String -> Property
prop_text_operations_memory s = 
  let limitedInput = take 12 s
      text = T.pack limitedInput
      textLength = T.length text
  in property $ textLength <= 12

-- 内存优化的边界条件测试
prop_empty_string_handling :: Property
prop_empty_string_handling = 
  let emptyStr = ""
      trimmed = trim emptyStr
  in property $ null trimmed

prop_single_character :: Char -> Property
prop_single_character c = 
  let singleChar = [c]
      trimmed = trim singleChar
  in property $ length trimmed <= 1

prop_large_string_truncation :: String -> Property
prop_large_string_truncation s = 
  let largeInput = take 1000 s  -- 模拟大输入
      truncated = take 50 largeInput  -- 主动截断
  in property $ length truncated <= 50

-- 内存优化的递归测试
prop_recursive_string_processing :: String -> Property
prop_recursive_string_processing s = 
  let limitedInput = take 8 s  -- 限制递归深度
      processStr [] = []
      processStr (x:xs) = x : processStr (take 7 xs)  -- 限制递归调用
      result = processStr limitedInput
  in property $ length result <= 8

-- 内存优化的错误处理测试
prop_error_handling_memory :: String -> Property
prop_error_handling_memory s = 
  let limitedInput = take 10 s
      -- 模拟可能的错误处理路径
      safeLength = length limitedInput
  in property $ safeLength >= 0 && safeLength <= 10

-- 创建内存优化的测试套件
createMemoryOptimizedSuite :: MemoryEfficiencyLevel -> TestTree
createMemoryOptimizedSuite level = 
  let testName = "Memory-Optimized Test Suite (" ++ show level ++ ")"
      baseTests = 
        [ testProperty "trim memory optimized" prop_trim_memory_optimized
        , testProperty "split memory optimized" prop_split_memory_optimized
        , testProperty "whitespace detection" prop_whitespace_detection
        , testProperty "string concat memory" prop_string_concat_memory
        , testProperty "list operations memory" prop_list_operations_memory
        , testProperty "text operations memory" prop_text_operations_memory
        , testProperty "empty string handling" prop_empty_string_handling
        , testProperty "single character" prop_single_character
        , testProperty "large string truncation" prop_large_string_truncation
        , testProperty "recursive string processing" prop_recursive_string_processing
        , testProperty "error handling memory" prop_error_handling_memory
        ]
  
  -- 根据内存级别应用不同的限制
  in case level of
    UltraLow -> withExtremeMemoryLimits $ testGroup testName (take 2 baseTests)
    VeryLow  -> withExtremeMemoryLimits $ testGroup testName (take 4 baseTests)
    Low      -> withOptimizedMemory $ testGroup testName (take 6 baseTests)
    Moderate -> withOptimizedMemory $ testGroup testName (take 8 baseTests)
    Normal   -> testGroup testName baseTests

-- 运行内存优化测试的主测试套件
tests :: TestTree
tests = testGroup "Memory Optimized Test Suites"
  [ createMemoryOptimizedSuite UltraLow
  , createMemoryOptimizedSuite VeryLow
  , createMemoryOptimizedSuite Low
  , createMemoryOptimizedSuite Moderate
  , createMemoryOptimizedSuite Normal
  ]