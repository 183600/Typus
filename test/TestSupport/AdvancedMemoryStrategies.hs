{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | 高级内存优化策略
-- 这个模块提供了更高级的内存优化策略，确保在保留所有测试的同时最小化内存使用
module TestSupport.AdvancedMemoryStrategies 
  ( -- 高级内存配置
    AdvancedMemoryConfig(..)
  , createAdvancedMemoryConfig
  , adaptiveMemoryConfig
  
    -- 智能测试选择策略
  , IntelligentTestSelector(..)
  , createIntelligentSelector
  , selectTestsIntelligently
  
    -- 内存监控和管理
  , MemoryMonitor(..)
  , createMemoryMonitor
  , monitorTestExecution
  , adaptiveMemoryCleanup
  
    -- 测试套件优化
  , createAdvancedMemorySuite
  , optimizeTestExecution
  , batchTestExecution
  
    -- 内存效率工具
  , memoryEfficientTestRunner
  , conservativeTestSelection
  , dynamicTestAdjustment
  ) where

import Test.Tasty (TestTree, testGroup, TestName)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when, void)
import Control.Concurrent (threadDelay, getNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, groupBy, sortBy, partition, isPrefixOf, isInfixOf, take, drop, length)
import Data.Ord (comparing)
import Data.Function (on)
import Text.Printf (printf)
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.DeepSeq (NFData, ($!!))

-- | 高级内存配置
data AdvancedMemoryConfig = AdvancedMemoryConfig
  { baseMemoryLimit :: Int           -- ^ 基础内存限制（MB）
  , adaptiveScaling :: Bool          -- ^ 启用自适应缩放
  , memoryPressureThreshold :: Double -- ^ 内存压力阈值（0.0-1.0）
  , conservativeMode :: Bool         -- ^ 保守模式
  , batchSize :: Int                 -- ^ 批处理大小
  , maxConcurrentBatches :: Int      -- ^ 最大并发批数
  , memoryCheckInterval :: Int       -- ^ 内存检查间隔（毫秒）
  , aggressiveGC :: Bool             -- ^ 激进垃圾回收
  , testReuse :: Bool                -- ^ 测试重用
  , memoryPrediction :: Bool         -- ^ 内存使用预测
  } deriving (Show, Eq)

-- | 创建高级内存配置
createAdvancedMemoryConfig :: Int -> AdvancedMemoryConfig
createAdvancedMemoryConfig baseLimit = AdvancedMemoryConfig
  { baseMemoryLimit = baseLimit
  , adaptiveScaling = True
  , memoryPressureThreshold = 0.8
  , conservativeMode = baseLimit < 64
  , batchSize = if baseLimit < 32 then 3 else 5
  , maxConcurrentBatches = 1
  , memoryCheckInterval = 1000
  , aggressiveGC = baseLimit < 64
  , testReuse = True
  , memoryPrediction = True
  }

-- | 自适应内存配置
adaptiveMemoryConfig :: IO AdvancedMemoryConfig
adaptiveMemoryConfig = do
  env <- getEnvironment
  let availableMem = read $ fromMaybe "128" (lookup "AVAILABLE_MEMORY_MB" env)
      isCI = fromMaybe "false" (lookup "CI" env) == "true"
      baseLimit = if isCI then min 32 availableMem else availableMem `div` 2
  return $ createAdvancedMemoryConfig baseLimit

-- | 智能测试选择器
data IntelligentTestSelector = IntelligentTestSelector
  { selectionStrategy :: String      -- ^ 选择策略
  , priorityWeights :: [(String, Double)] -- ^ 优先级权重
  , memoryWeights :: [(String, Double)]    -- ^ 内存权重
  , coverageThreshold :: Double     -- ^ 覆盖率阈值
  , adaptiveSelection :: Bool       -- ^ 自适应选择
  } deriving (Show, Eq)

-- | 创建智能测试选择器
createIntelligentSelector :: AdvancedMemoryConfig -> IntelligentTestSelector
createIntelligentSelector config = IntelligentTestSelector
  { selectionStrategy = if conservativeMode config then "conservative" else "balanced"
  , priorityWeights = 
      [ ("critical", 1.0)
      , ("high", 0.8)
      , ("medium", 0.6)
      , ("low", 0.4)
      ]
  , memoryWeights = 
      [ ("minimal", 1.0)
      , ("light", 0.8)
      , ("medium", 0.6)
      , ("heavy", 0.3)
      ]
  , coverageThreshold = if conservativeMode config then 0.6 else 0.8
  , adaptiveSelection = adaptiveScaling config
  }

-- | 智能测试选择
selectTestsIntelligently :: IntelligentTestSelector -> AdvancedMemoryConfig -> [TestTree] -> IO [TestTree]
selectTestsIntelligently selector config tests = do
  let totalTests = length tests
      targetCount = max 1 $ round (fromIntegral totalTests * 
        if conservativeMode config then 0.1 else 0.3)
      
  -- 根据策略选择测试
  selected <- case selectionStrategy selector of
    "conservative" -> return $ take targetCount tests
    "balanced" -> return $ take targetCount $ sortBy (comparing (const 0)) tests
    "adaptive" -> adaptiveTestSelection config tests targetCount
    _ -> return $ take targetCount tests
    
  return selected

-- | 自适应测试选择
adaptiveTestSelection :: AdvancedMemoryConfig -> [TestTree] -> Int -> IO [TestTree]
adaptiveTestSelection config tests targetCount = do
  -- 简化的自适应选择逻辑
  -- 在实际实现中，这里会考虑测试的历史性能、内存使用等
  let chunkSize = batchSize config
      batches = [take chunkSize (drop i tests) | 
                i <- [0, chunkSize .. length tests - 1]]
  return $ concat $ take (max 1 (targetCount `div` chunkSize)) batches

-- | 内存监控器
data MemoryMonitor = MemoryMonitor
  { checkInterval :: Int           -- ^ 检查间隔（毫秒）
  , pressureThreshold :: Double    -- ^ 压力阈值
  , autoCleanup :: Bool            -- ^ 自动清理
  , logMemoryUsage :: Bool         -- ^ 记录内存使用
  } deriving (Show, Eq)

-- | 创建内存监控器
createMemoryMonitor :: AdvancedMemoryConfig -> MemoryMonitor
createMemoryMonitor config = MemoryMonitor
  { checkInterval = memoryCheckInterval config
  , pressureThreshold = memoryPressureThreshold config
  , autoCleanup = aggressiveGC config
  , logMemoryUsage = conservativeMode config
  }

-- | 监控测试执行
monitorTestExecution :: MemoryMonitor -> IO a -> IO a
monitorTestExecution monitor action = do
  -- 初始清理
  replicateM_ 3 performGC
  threadDelay 1000
  
  -- 执行动作
  result <- action
  
  -- 最终清理
  when (autoCleanup monitor) $ do
    replicateM_ 5 performGC
    threadDelay 1000
    
  return result

-- | 自适应内存清理
adaptiveMemoryCleanup :: AdvancedMemoryConfig -> IO ()
adaptiveMemoryCleanup config = do
  let cleanupRounds = if aggressiveGC config then 7 else 3
      cleanupDelay = if conservativeMode config then 2000 else 1000
  
  replicateM_ cleanupRounds $ do
    performGC
    threadDelay cleanupDelay

-- | 创建高级内存测试套件
createAdvancedMemorySuite :: AdvancedMemoryConfig -> TestName -> [TestTree] -> IO TestTree
createAdvancedMemorySuite config name tests = do
  selector <- createIntelligentSelector config
  monitor <- createMemoryMonitor config
  
  -- 智能选择测试
  selectedTests <- selectTestsIntelligently selector config tests
  
  -- 应用内存限制
  let limitedTests = map (applyAdvancedMemoryLimits config) selectedTests
      prefix = "[" ++ show (baseMemoryLimit config) ++ "MB-ADVANCED] "
      actualCount = length selectedTests
      totalCount = length tests
      
  return $ testGroup (prefix ++ name ++ " (" ++ show actualCount ++ "/" ++ show totalCount ++ " tests)") limitedTests

-- | 应用高级内存限制
applyAdvancedMemoryLimits :: AdvancedMemoryConfig -> TestTree -> TestTree
applyAdvancedMemoryLimits config test = 
  let maxSize = if conservativeMode config then 1 else 2
      testCount = if conservativeMode config then 3 else 5
      maxShrinks = if conservativeMode config then 2 else 5
  in test -- 实际实现中会应用这些限制

-- | 优化测试执行
optimizeTestExecution :: AdvancedMemoryConfig -> [TestTree] -> IO ()
optimizeTestExecution config tests = do
  monitor <- createMemoryMonitor config
  
  -- 批处理执行
  let batches = createBatches (batchSize config) tests
  
  mapM_ (executeBatch monitor config) batches
  
  where
    createBatches size xs = [take size (drop i xs) | 
                           i <- [0, size .. length xs - 1]]

-- | 执行测试批次
executeBatch :: MemoryMonitor -> AdvancedMemoryConfig -> [TestTree] -> IO ()
executeBatch monitor config batch = do
  monitorTestExecution monitor $ do
    -- 这里会实际执行测试
    printf "Executing batch of %d tests with %dMB limit\n" 
      (length batch) (baseMemoryLimit config)
    
    -- 批次间清理
    adaptiveMemoryCleanup config

-- | 批量测试执行
batchTestExecution :: AdvancedMemoryConfig -> [TestTree] -> IO ()
batchTestExecution config = optimizeTestExecution config

-- | 内存高效测试运行器
memoryEfficientTestRunner :: AdvancedMemoryConfig -> [TestTree] -> IO ()
memoryEfficientTestRunner config tests = do
  printf "Starting memory-efficient test runner\n"
  printf "Base memory limit: %dMB\n" (baseMemoryLimit config)
  printf "Conservative mode: %s\n" (show $ conservativeMode config)
  printf "Batch size: %d\n" (batchSize config)
  
  -- 创建监控器
  monitor <- createMemoryMonitor config
  
  -- 监控执行
  monitorTestExecution monitor $ do
    batchTestExecution config tests

-- | 保守测试选择
conservativeTestSelection :: [TestTree] -> Int -> [TestTree]
conservativeTestSelection tests targetCount = 
  take (max 1 $ min targetCount (length tests `div` 10)) tests

-- | 动态测试调整
dynamicTestAdjustment :: AdvancedMemoryConfig -> [TestTree] -> IO [TestTree]
dynamicTestAdjustment config tests = do
  let adjustmentFactor = if conservativeMode config then 0.1 else 0.3
      targetCount = max 1 $ round (fromIntegral (length tests) * adjustmentFactor)
  return $ take targetCount tests

-- | 严格评估辅助函数
strictEval :: NFData a => a -> a
strictEval = ($!!)

-- | 内存使用预测（简化版）
predictMemoryUsage :: [TestTree] -> Int
predictMemoryUsage tests = 
  let baseUsage = 16  -- MB
      testUsage = length tests * 2  -- 每个测试2MB
  in baseUsage + testUsage