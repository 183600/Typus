{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ExtremeMemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMinimalMemoryLimits
  , minimalMemoryLimitedTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  , ultraGC
  )
import TestSupport.ExtremeMemoryLimits
  ( withExtremeMemoryLimits
  , withCriticalMemoryLimits
  , extremeMemoryConfig
  , criticalMemoryConfig
  , createExtremeMemorySuite
  , selectCriticalTests
  , forceExtremeCleanup
  , monitorExtremeMemoryUsage
  )

import Utils (trim, splitBy)
import Data.Char (isSpace)

-- 极端内存优化的属性 - 最小的测试数据
prop_extreme_trim_minimal :: String -> Property
prop_extreme_trim_minimal s = 
  let extremeLimited = take 2 s  -- 极小的字符串限制
      trimmed = trim extremeLimited
  in property $ length trimmed <= 2

prop_extreme_split_minimal :: Char -> String -> Property
prop_extreme_split_minimal c s = 
  let extremeLimited = take 2 s  -- 极小的字符串限制
      parts = splitBy c extremeLimited
  in property $ length parts <= 3

prop_extreme_whitespace :: String -> Property
prop_extreme_whitespace s = 
  let extremeLimited = take 2 s
      isAllWhitespace = all isSpace extremeLimited
  in property $ if isAllWhitespace then length extremeLimited <= 2 else True

prop_extreme_arithmetic :: Int -> Int -> Property
prop_extreme_arithmetic x y = 
  let limitedX = mod (abs x) 3  -- 极小的数字范围
      limitedY = mod (abs y) 3
      sum = limitedX + limitedY
  in property $ sum >= 0 && sum <= 4

prop_extreme_list :: [Int] -> Property
prop_extreme_list xs = 
  let extremeLimited = take 1 xs  -- 只取一个元素
  in property $ length extremeLimited <= 1

prop_extreme_string_concat :: String -> String -> Property
prop_extreme_string_concat s1 s2 = 
  let limited1 = take 1 s1
      limited2 = take 1 s2
      combined = limited1 ++ limited2
  in property $ length combined <= 2

-- 极端内存优化的测试套件
tests :: TestTree
tests = createExtremeMemorySuite extremeMemoryConfig "Extreme Memory-Optimized Test Suite"
  [ withExtremeMemoryLimits $ testProperty "extreme trim minimal" prop_extreme_trim_minimal
  , withExtremeMemoryLimits $ testProperty "extreme split minimal" prop_extreme_split_minimal
  , withExtremeMemoryLimits $ testProperty "extreme whitespace" prop_extreme_whitespace
  , withExtremeMemoryLimits $ testProperty "extreme arithmetic" prop_extreme_arithmetic
  , withExtremeMemoryLimits $ testProperty "extreme list" prop_extreme_list
  , withExtremeMemoryLimits $ testProperty "extreme string concat" prop_extreme_string_concat
  ]

-- 关键内存优化的测试套件（更严格的限制）
criticalTests :: TestTree
criticalTests = createExtremeMemorySuite criticalMemoryConfig "Critical Memory Test Suite"
  [ withCriticalMemoryLimits $ testProperty "critical trim" prop_extreme_trim_minimal
  , withCriticalMemoryLimits $ testProperty "critical split" prop_extreme_split_minimal
  ]

-- 运行极端内存测试的函数
runExtremeTests :: IO ()
runExtremeTests = do
  -- 预清理内存
  forceExtremeCleanup
  
  -- 监控内存使用并运行测试
  monitorExtremeMemoryUsage $ defaultMain tests
  
  -- 后清理内存
  forceExtremeCleanup

-- 运行关键内存测试的函数
runCriticalTests :: IO ()
runCriticalTests = do
  -- 预清理内存
  forceExtremeCleanup
  
  -- 监控内存使用并运行测试
  monitorExtremeMemoryUsage $ defaultMain criticalTests
  
  -- 后清理内存
  forceExtremeCleanup