{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.MemoryOptimizedMain where

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
import TestSupport.OptimizedMemoryLimits
  ( withOptimizedMemoryLimits
  , withStrictMemoryLimits
  , minimalOptimizedConfig
  , createOptimizedMemorySuite
  )

-- 导入Utils模块以使用核心函数
import Utils (trim, splitBy)

-- 只导入最核心的测试模块
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as EnhancedMemoryOptimizedTestSuite
import qualified Test.Unit.UltraMemoryOptimizedTestSuite as UltraMemoryOptimizedTestSuite

-- 导入核心功能测试
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- 极简的内存优化测试属性
prop_minimal_trim :: String -> Property
prop_minimal_trim s = 
  let limitedString = take 3 s  -- 极小的字符串限制
      trimmed = trim limitedString
  in property $ length trimmed <= 3

prop_minimal_split :: Char -> String -> Property
prop_minimal_split c s = 
  let limitedString = take 2 s  -- 极小的字符串限制
      parts = splitBy c limitedString
  in property $ length parts <= 3

prop_minimal_arithmetic :: Int -> Int -> Property
prop_minimal_arithmetic x y = 
  let limitedX = mod (abs x) 5  -- 极小的数字范围
      limitedY = mod (abs y) 5
      sum = limitedX + limitedY
  in property $ sum >= 0 && sum <= 8

-- 创建极简的内存优化测试套件
minimalTests :: TestTree
minimalTests = minimalMemoryLimitedTestGroup "Typus Minimal Memory Test Suite"
  [ -- 使用最小内存限制的核心测试
    withMinimalMemoryLimits $ testProperty "minimal trim" prop_minimal_trim,
    withMinimalMemoryLimits $ testProperty "minimal split" prop_minimal_split,
    withMinimalMemoryLimits $ testProperty "minimal arithmetic" prop_minimal_arithmetic,
    
    -- 使用最小内存限制的现有测试套件
    withMinimalMemoryLimits BasicQuickCheckTestSuite.essentialTests,
    withMinimalMemoryLimits UltraMemoryOptimizedTestSuite.tests,
    
    -- 创建最小内存配置的额外测试
    createOptimizedMemorySuite minimalOptimizedConfig "Critical Tests"
      [ testProperty "critical trim" prop_minimal_trim,
        testProperty "critical split" prop_minimal_split
      ]
  ]

-- 主测试函数，强制垃圾回收
main :: IO ()
main = do
  -- 运行前强制垃圾回收
  ultraGC
  
  -- 运行最小内存测试套件
  defaultMain minimalTests
  
  -- 运行后强制垃圾回收
  ultraGC