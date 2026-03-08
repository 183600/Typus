{-# LANGUAGE OverloadedStrings #-}

-- | 统一测试优化器
-- 提供测试套件整合和去重功能
module TestSupport.ConsolidatedTestOptimizer
  ( consolidateTestSuites
  , removeDuplicateTests
  , createMinimalTestSuite
  , optimizeTestSelection
  , testSuiteHealthCheck
  ) where

import Test.Tasty (TestTree, testGroup)
import Data.List (nub, groupBy, sortBy)
import Data.Function (on)

-- | 测试套件信息
data TestSuiteInfo = TestSuiteInfo
  { suiteName :: String
  , testCount :: Int
  , estimatedMemory :: Int  -- MB
  , priority :: Int
  } deriving (Show, Eq)

-- | 测试套件健康检查结果
data TestSuiteHealth = TestSuiteHealth
  { totalTests :: Int
  , uniqueTests :: Int
  , duplicateCount :: Int
  , memoryEfficiency :: Double
  , optimizationScore :: Double
  } deriving (Show, Eq)

-- | 整合测试套件
consolidateTestSuites :: [TestTree] -> [TestTree] -> [TestTree]
consolidateTestSuites existingSuites newSuites =
  let allSuites = existingSuites ++ newSuites
      -- 简单的去重策略 - 在实际应用中需要更复杂的逻辑
      uniqueSuites = filterDistinctSuites allSuites
  in take 20 uniqueSuites  -- 限制套件数量

-- | 移除重复测试
removeDuplicateTests :: [TestTree] -> [TestTree]
removeDuplicateTests tests =
  let -- 基于测试名称去重
      distinctTests = nubBy testName tests
  in distinctTests

-- | 创建最小测试套件
createMinimalTestSuite :: [TestTree] -> [TestTree]
createMinimalTestSuite allTests =
  let -- 选择最重要的测试
      prioritizedTests = sortBy (compare `on` testPriority) allTests
      -- 限制测试数量
      minimalTests = take 50 prioritizedTests
  in minimalTests

-- | 优化测试选择
optimizeTestSelection :: [TestTree] -> Double -> [TestTree]
optimizeTestSelection tests selectionRatio =
  let totalCount = length tests
      targetCount = max 10 (min totalCount (floor (fromIntegral totalCount * selectionRatio)))
      -- 基于优先级和内存消耗选择
      selectedTests = take targetCount $ sortBy (compare `on` testEfficiencyScore) tests
  in selectedTests

-- | 测试套件健康检查
testSuiteHealthCheck :: [TestTree] -> TestSuiteHealth
testSuiteHealthCheck tests =
  let total = length tests
      unique = length (removeDuplicateTests tests)
      duplicates = total - unique
      memoryEff = if total > 0 then fromIntegral unique / fromIntegral total else 0.0
      optScore = if total > 0 then 1.0 - (fromIntegral duplicates / fromIntegral total) else 1.0
  in TestSuiteHealth
      { totalTests = total
      , uniqueTests = unique
      , duplicateCount = duplicates
      , memoryEfficiency = memoryEff
      , optimizationScore = optScore
      }

-- ============================================================================
-- 内部辅助函数
-- ============================================================================

-- | 基于测试名称去重
nubBy :: (TestTree -> String) -> [TestTree] -> [TestTree]
nubBy getName tests = 
  let groups = groupBy (\a b -> getName a == getName b) (sortBy (compare `on` getName) tests)
  in map head groups

-- | 获取测试名称（简化版本）
testName :: TestTree -> String
testName _ = "test-name"  -- 实际实现需要从TestTree中提取名称

-- | 获取测试优先级
testPriority :: TestTree -> Int
testPriority _ = 1  -- 实际实现需要基于测试重要性

-- | 计算测试效率分数
testEfficiencyScore :: TestTree -> Double
testEfficiencyScore _ = 1.0  -- 实际实现需要基于内存使用和覆盖率的组合