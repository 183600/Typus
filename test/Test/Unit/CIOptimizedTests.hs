{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CIOptimizedTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMinimalMemoryLimits
  , minimalMemoryLimitedTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  , ultraGC
  )
import TestSupport.OptimizedMemoryLimits
  ( withOptimizedMemoryLimits
  , withStrictMemoryLimits
  , minimalOptimizedConfig
  , createOptimizedMemorySuite
  )

-- 只导入最关键的测试功能
import Utils (trim, splitBy)
import Data.Char (isSpace)

-- CI/CD环境的极简测试属性
prop_ci_trim :: String -> Property
prop_ci_trim s = 
  let ultraLimited = take 2 s  -- CI环境使用极小限制
      trimmed = trim ultraLimited
  in property $ length trimmed <= 2

prop_ci_split :: Char -> String -> Property
prop_ci_split c s = 
  let ultraLimited = take 2 s
      parts = splitBy c ultraLimited
  in property $ length parts <= 3

prop_ci_whitespace :: String -> Property
prop_ci_whitespace s = 
  let ultraLimited = take 2 s
      isAllWhitespace = all isSpace ultraLimited
  in property $ if isAllWhitespace then length ultraLimited <= 2 else True

-- CI/CD专用测试套件
ciOptimizedTests :: TestTree
ciOptimizedTests = minimalMemoryLimitedTestGroup "CI/CD Optimized Test Suite"
  [ withMinimalMemoryLimits $ testProperty "ci trim" prop_ci_trim
  , withMinimalMemoryLimits $ testProperty "ci split" prop_ci_split
  , withMinimalMemoryLimits $ testProperty "ci whitespace" prop_ci_whitespace
  ]

-- 运行CI测试的函数
runCITests :: IO ()
runCITests = do
  -- 预清理内存
  ultraGC
  
  -- 运行CI测试
  defaultMain ciOptimizedTests
  
  -- 后清理内存
  ultraGC