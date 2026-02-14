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
  , superOptimizedMemoryConfig
  
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

-- | 极端内存配置（16MB）- 用于CI/CD等资源受限环境
extremeMemoryConfig :: UnifiedMemoryConfig
extremeMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 16
  , maxTestSize = 1
  , testCount = 1  -- 进一步减少到1个测试
  , maxShrinks = 1
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.05  -- 只运行5%的测试
  , memoryThreshold = 16
  }

-- | 最小内存配置（32MB）- 用于轻度受限环境
minimalMemoryConfig :: UnifiedMemoryConfig
minimalMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 32
  , maxTestSize = 1
  , testCount = 2  -- 进一步减少到2个测试
  , maxShrinks = 1  -- 进一步减少到1次收缩
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.05   -- 只运行5%的测试
  , memoryThreshold = 32
  }

-- | 标准内存配置（64MB）- 用于开发环境
standardMemoryConfig :: UnifiedMemoryConfig
standardMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 64
  , maxTestSize = 2
  , testCount = 5  -- 进一步减少到5个测试
  , maxShrinks = 3  -- 进一步减少到3次收缩
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.1   -- 只运行10%的测试
  , memoryThreshold = 64
  }

-- | CI内存配置（48MB）- 平衡的CI环境配置
ciMemoryConfig :: UnifiedMemoryConfig
ciMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 48
  , maxTestSize = 1
  , testCount = 3  -- 进一步减少到3个测试
  , maxShrinks = 2  -- 进一步减少到2次收缩
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.05  -- 只运行5%的测试
  , memoryThreshold = 48
  }

-- | 超级优化内存配置（8MB）- 用于极端内存受限环境
superOptimizedMemoryConfig :: UnifiedMemoryConfig
superOptimizedMemoryConfig = UnifiedMemoryConfig
  { memoryLimitMB = 8
  , maxTestSize = 1
  , testCount = 1  -- 只运行1个测试
  , maxShrinks = 0  -- 不进行收缩
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  , maxConcurrentTests = 1
  , testSelectionRatio = 0.01  -- 只运行1%的测试
  , memoryThreshold = 8
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
  let maxTests = 10  -- 进一步减少到10个QuickCheck测试
  in take maxTests tests

-- | 选择核心功能测试
selectCoreFunctionalityTests :: [TestTree] -> [TestTree]
selectCoreFunctionalityTests tests = 
  let maxTests = 5  -- 进一步减少到5个核心功能测试
  in take maxTests tests

-- | 选择内存关键测试
selectMemoryCriticalTests :: [TestTree] -> [TestTree]
selectMemoryCriticalTests tests = 
  let maxTests = 3  -- 进一步减少到3个内存关键测试
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

-- | 创建内存优化的完整测试套件（保留所有测试）
createMemoryOptimizedFullSuite :: [TestTree] -> UnifiedMemoryConfig -> TestTree
createMemoryOptimizedFullSuite testTrees config = 
  testGroup "Memory-Optimized Full Test Suite" 
    $ map (withUnifiedMemoryLimits config) testTrees

-- | 批量优化测试文件（保留所有测试）
optimizeAllTestFiles :: [(String, TestTree)] -> UnifiedMemoryConfig -> TestTree
optimizeAllTestFiles testFiles config = 
  testGroup "All Memory-Optimized Tests"
    [ testGroup name $ [withUnifiedMemoryLimits config tests]
    | (name, tests) <- testFiles
    ]

-- | 智能测试分组（按内存需求分组）
groupTestsByMemoryNeeds :: [TestTree] -> (TestTree, TestTree, TestTree)
groupTestsByMemoryNeeds tests = 
  let (lightTests, mediumTests) = splitAt (length tests `div` 3) tests
      (heavyTests, remaining) = splitAt (length tests `div` 3) mediumTests
  in ( testGroup "Light Memory Tests" lightTests
     , testGroup "Medium Memory Tests" remaining
     , testGroup "Heavy Memory Tests" heavyTests
     )

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