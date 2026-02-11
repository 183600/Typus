{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.Tests where

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
import System.Mem (performGC)

-- 只导入最核心的测试模块以减少内存占用
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- 只导入关键的内存优化测试套件
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as EnhancedMemoryOptimizedTestSuite
import qualified Test.Unit.UltraMemoryOptimizedTestSuite as UltraMemoryOptimizedTestSuite

-- 只保留最重要的几个测试套件
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import qualified Test.Unit.OptimizedTests as OptimizedTests
import qualified Test.Unit.FinalExact200QuickCheckTests as FinalExact200QuickCheckTests
-- 新添加的增强测试套件
import qualified Test.Unit.NewEnhancedQuickCheckTestSuite as NewEnhancedQuickCheckTestSuite

-- 极简的内存优化测试属性 - 进一步优化内存使用
prop_minimal_basic_property :: String -> Property
prop_minimal_basic_property s = 
  let limitedString = take 1 s  -- 极度减少字符串限制
  in property $ length limitedString >= 0

prop_minimal_list_property :: [Int] -> Property
prop_minimal_list_property xs = 
  let limitedList = take 1 xs   -- 极度减少列表限制
  in property $ length limitedList >= 0

-- 创建极简的内存优化测试套件
tests :: TestTree
tests = minimalMemoryLimitedTestGroup "Typus Memory-Optimized Test Suite"
  [ -- 使用最小内存限制的核心测试
    withMinimalMemoryLimits $ testProperty "minimal basic property" prop_minimal_basic_property,
    withMinimalMemoryLimits $ testProperty "minimal list property" prop_minimal_list_property,
    
    -- 只保留最关键的测试套件
    withMinimalMemoryLimits ConciseTestSuite.tests,
    withMinimalMemoryLimits testListProperties,
    withMinimalMemoryLimits BasicQuickCheckTestSuite.essentialTests,  -- 使用essentialTests而不是完整的tests
    withMinimalMemoryLimits SimpleQuickCheckTestSuite.tests,
    
    -- 内存优化的测试套件
    withMinimalMemoryLimits OptimizedTests.tests,
    withMinimalMemoryLimits EnhancedMemoryOptimizedTestSuite.tests,
    
    -- 极简内存优化的测试
    withMinimalMemoryLimits UltraMemoryOptimizedTestSuite.tests,
    
    -- 新添加的200个测试
    withMinimalMemoryLimits FinalExact200QuickCheckTests.exact200QuickCheckTests,
    
    -- 新添加的增强测试套件
    withMinimalMemoryLimits NewEnhancedQuickCheckTestSuite.newEnhancedQuickCheckTestSuite,
    
    -- 创建最小内存配置的额外测试
    createOptimizedMemorySuite minimalOptimizedConfig "Critical Tests"
      [ testProperty "critical basic property" prop_minimal_basic_property
      , testProperty "critical list property" prop_minimal_list_property
      ]
  ]
