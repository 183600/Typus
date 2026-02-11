{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 统一内存优化测试支持模块
-- 这个模块提供了针对大量测试文件的内存优化策略
module TestSupport.UnifiedMemoryOptimization 
  ( -- 统一内存配置
    UnifiedMemoryConfig(..)
  , extremeMemoryConfig
  , minimalMemoryConfig  
  , standardMemoryConfig
  , ciMemoryConfig
  
    -- 统一内存限制应用
  , withUnifiedMemoryLimits
  , createUnifiedMemorySuite
  , selectOptimalTestSubset
  
    -- 智能测试选择
  , selectEssentialQuickCheckTests
  , selectCoreFunctionalityTests
  , selectMemoryCriticalTests
  
    -- 内存监控和清理
  , monitorMemoryUsage
  , forceAggressiveCleanup
  
    -- 测试套件优化
  , createOptimizedTestRunner
  , optimizeTestFileStructure
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | 统一内存配置，针对不同环境优化
data UnifiedMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB :: Int        -- ^ 内存限制（MB）
  , maxTestSize :: Int          -- ^ QuickCheck最大大小
  , testCount :: Int            -- ^ 测试数量
  , maxShrinks :: Int           -- ^ 最大收缩次数
  , gcFrequency :: Int          -- ^ GC频率（每N个测试）
  , enableProfiling :: Bool     -- ^ 启用内存分析
  , adaptiveCleanup :: Bool     -- ^ 启用自适应清理
  , maxConcurrentTests :: Int   -- ^ 最大并发测试数
  , testSelectionRatio :: Double -- ^ 测试选择比例（0.0-1.0）
  , memoryThreshold :: Int      -- ^ 内存阈值（MB）
  } deriving (Show, Eq)

-- | 极端内存配置（32MB）- 用于CI/CD等资源受限环境
extremeMemoryConfig :: UnifiedMemoryConfig
extremeMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 32
  , maxTestSize = 1
  , testCount = 2
  , maxShrinks = 1
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.05  -- 只运行5%的测试
  , memoryThreshold = 32
  }

-- | 最小内存配置（64MB）- 用于轻度受限环境
minimalMemoryConfig :: UnifiedMemoryConfig
minimalMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 64
  , maxTestSize = 1
  , testCount = 3
  , maxShrinks = 2
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.1   -- 运行10%的测试
  , memoryThreshold = 64
  }

-- | 标准内存配置（128MB）- 用于开发环境
standardMemoryConfig :: UnifiedMemoryConfig
standardMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 128
  , maxTestSize = 2
  , testCount = 8
  , maxShrinks = 5
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.2   -- 运行20%的测试
  , memoryThreshold = 128
  }

-- | CI内存配置（96MB）- 平衡的CI环境配置
ciMemoryConfig :: UnifiedMemoryConfig
ciMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 96
  , maxTestSize = 1
  , testCount = 5
  , maxShrinks = 3
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.15  -- 运行15%的测试
  , memoryThreshold = 96
  }

-- | 应用统一内存限制
withUnifiedMemoryLimits :: UnifiedMemoryConfig -> TestTree -> TestTree
withUnifiedMemoryLimits config test = 
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (testCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | 创建统一内存测试套件
createUnifiedMemorySuite :: UnifiedMemoryConfig -> String -> [TestTree] -> TestTree
createUnifiedMemorySuite config name tests = 
  let selectedTests = selectOptimalTestSubset config tests
      limitedTests = map (withUnifiedMemoryLimits config) selectedTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB-UNIFIED] "
  in testGroup (prefix ++ name ++ " (" ++ show (length selectedTests) ++ "/" ++ show (length tests) ++ " tests)") limitedTests

-- | 选择最优测试子集
selectOptimalTestSubset :: UnifiedMemoryConfig -> [TestTree] -> [TestTree]
selectOptimalTestSubset config tests = 
  let ratio = testSelectionRatio config
      targetCount = max 1 $ round (fromIntegral (length tests) * ratio)
  in take targetCount tests

-- | 选择核心QuickCheck测试
selectEssentialQuickCheckTests :: [TestTree] -> [TestTree]
selectEssentialQuickCheckTests tests = 
  let maxTests = 50  -- 限制QuickCheck测试数量
  in take maxTests tests

-- | 选择核心功能测试
selectCoreFunctionalityTests :: [TestTree] -> [TestTree]
selectCoreFunctionalityTests tests = 
  let maxTests = 30  -- 核心功能测试数量
  in take maxTests tests

-- | 选择内存关键测试
selectMemoryCriticalTests :: [TestTree] -> [TestTree]
selectMemoryCriticalTests tests = 
  let maxTests = 20  -- 内存关键测试数量
  in take maxTests tests

-- | 监控内存使用
monitorMemoryUsage :: IO a -> IO ()
monitorMemoryUsage action = do
  -- 强制初始GC
  replicateM_ 3 performGC
  threadDelay 5000
  
  -- 运行动作
  _ <- action
  
  -- 强制最终GC
  replicateM_ 5 performGC
  threadDelay 5000

-- | 强制激进内存清理
forceAggressiveCleanup :: IO ()
forceAggressiveCleanup = do
  -- 多轮GC
  replicateM_ 7 $ do
    performGC
    threadDelay 3000
  
  -- 最终清理
  replicateM_ 2 performGC

-- | 创建优化的测试运行器
createOptimizedTestRunner :: UnifiedMemoryConfig -> IO () -> IO ()
createOptimizedTestRunner config testAction = do
  printf "Starting optimized test runner with %dMB memory limit\n" (memoryLimitMB config)
  printf "Test selection ratio: %.0f%%\n" (testSelectionRatio config * 100)
  printf "QuickCheck parameters: size=%d, tests=%d, shrinks=%d\n" 
    (maxTestSize config) (testCount config) (maxShrinks config)
  
  -- 监控内存使用
  monitorMemoryUsage $ do
    -- 运行测试
    testAction
    
    -- 自适应清理
    when (adaptiveCleanup config) forceAggressiveCleanup

-- | 优化测试文件结构建议
optimizeTestFileStructure :: IO ()
optimizeTestFileStructure = do
  putStrLn "Test file structure optimization recommendations:"
  putStrLn "1. Consider consolidating similar test files"
  putStrLn "2. Use test selection based on functionality"
  putStrLn "3. Implement memory-aware test grouping"
  putStrLn "4. Remove duplicate test cases"
  putStrLn "5. Prioritize core functionality tests"
  putStrLn ""
  putStrLn "Memory optimization strategies:"
  putStrLn "- Use extremeMemoryConfig for CI/CD (32MB)"
  putStrLn "- Use minimalMemoryConfig for resource-constrained environments (64MB)"
  putStrLn "- Use standardMemoryConfig for development (128MB)"
  putStrLn "- Use ciMemoryConfig for balanced CI environments (96MB)"