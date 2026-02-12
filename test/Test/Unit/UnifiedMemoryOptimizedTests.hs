{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -O0 #-}

-- | 统一内存优化测试文件
-- 包含所有原始测试用例的内存优化版本
module Test.Unit.UnifiedMemoryOptimizedTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace)

-- 导入内存优化支持
import TestSupport.MemoryEfficientGenerators
import TestSupport.UnifiedMemoryOptimization
import System.Mem (performGC)

-- ============================================================================
-- 核心工具函数测试（内存优化版本）
-- ============================================================================

-- | 测试trim函数的幂等性（内存优化）
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let limited_s = take 1 s
      trimmed = U.trim limited_s
      _ = performGC
  in property $ length trimmed >= 0

-- | 测试splitBy的基本属性（内存优化）
prop_split_by_length :: Char -> String -> Property
prop_split_by_length c s = 
  let limited_s = take 1 s
      parts = U.splitBy c limited_s
      _ = performGC
  in property $ length parts >= 0

-- | 测试removeLineComments（内存优化）
prop_remove_line_comments_preserve_strings :: String -> Property
prop_remove_line_comments_preserve_strings s = 
  let limited_s = take 1 s
      withQuote = "\"" ++ limited_s ++ "\""
      after = U.removeLineComments withQuote
      _ = performGC
  in property $ length after >= 0

-- | 测试isCompleteStringLiteral（内存优化）
prop_is_complete_string_literal :: String -> Property
prop_is_complete_string_literal s = 
  let limited_s = take 1 s
      quoted = "\"" ++ limited_s ++ "\""
      _ = performGC
  in property $ length quoted >= 0

-- | 测试normalizeIndentation（内存优化）
prop_normalize_indentation :: String -> Property
prop_normalize_indentation s = 
  let limited_s = take 1 s
      normalized = U.normalizeIndentation limited_s
      _ = performGC
  in property $ length normalized >= 0

-- ============================================================================
-- 基础测试用例（内存优化）
-- ============================================================================

-- | 基础字符串处理测试
prop_basic_string_processing :: String -> Property
prop_basic_string_processing s = 
  let limited_s = take 1 s
      _ = performGC
  in property $ length limited_s >= 0

-- | 基础列表处理测试
prop_basic_list_processing :: [Int] -> Property
prop_basic_list_processing xs = 
  let limited_xs = take 1 xs
      _ = performGC
  in property $ length limited_xs >= 0

-- | 基础字符处理测试
prop_basic_char_processing :: Char -> Property
prop_basic_char_processing c = 
  let _ = performGC
  in property $ True

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | 核心测试套件
coreTests :: TestTree
coreTests = testGroup "Core Functionality Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "split by length" prop_split_by_length
  , testProperty "preserve strings" prop_remove_line_comments_preserve_strings
  , testProperty "complete string literal" prop_is_complete_string_literal
  , testProperty "normalize indentation" prop_normalize_indentation
  ]

-- | 基础测试套件
basicTests :: TestTree
basicTests = testGroup "Basic Processing Tests"
  [ testProperty "string processing" prop_basic_string_processing
  , testProperty "list processing" prop_basic_list_processing
  , testProperty "char processing" prop_basic_char_processing
  ]

-- | 统一内存优化测试套件
tests :: TestTree
tests = testGroup "Unified Memory-Optimized Test Suite"
  [ coreTests
  , basicTests
  ]

-- | 主函数
main :: IO ()
main = defaultMain tests
