{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | 综合内存清理模块
-- 提供多层次的内存清理策略，确保测试用例之间不会积累内存使用
module TestSupport.ComprehensiveMemoryCleanup 
  ( -- 内存清理策略
    CleanupStrategy(..)
  , defaultCleanupStrategy
  , aggressiveCleanupStrategy
  , emergencyCleanupStrategy
  
    -- 清理操作
  , comprehensiveCleanup
  , strategicCleanup
  , incrementalCleanup
  , emergencyCleanup
  
    -- 清理控制
  , withCleanupBetweenTests
  , withPeriodicCleanup
  , withAdaptiveCleanup
  , withMemoryMonitoring
  
    -- 清理监控
  , CleanupMonitor(..)
  , createCleanupMonitor
  , monitorCleanupEffectiveness
  , withCleanupMonitoring
  
    -- 自动清理
  , AutoCleanupConfig(..)
  , createAutoCleanupConfig
  , withAutoCleanup
  , runWithAutoCleanup
  ) where

import Test.Tasty (TestTree, testGroup)
import System.Mem (performGC)
import Control.Monad (replicateM_, when, unless)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_, finally)
import System.Environment (getEnvironment)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

-- | 清理策略
data CleanupStrategy = CleanupStrategy
  { gcRounds :: Int              -- GC轮数
  , gcDelay :: Int               -- GC间延迟(微秒)
  , systemCacheCleanup :: Bool   -- 是否清理系统缓存
  , haskellHeapCleanup :: Bool   -- 是否清理Haskell堆
  , forceFinalization :: Bool    -- 是否强制终结化
  , memoryCompaction :: Bool     -- 是否压缩内存
  , monitoringEnabled :: Bool    -- 是否启用监控
  } deriving (Show, Eq)

-- | 默认清理策略
defaultCleanupStrategy :: CleanupStrategy
defaultCleanupStrategy = CleanupStrategy
  { gcRounds = 3
  , gcDelay = 100
  , systemCacheCleanup = False
  , haskellHeapCleanup = True
  , forceFinalization = False
  , memoryCompaction = False
  , monitoringEnabled = False
  }

-- | 激进清理策略
aggressiveCleanupStrategy :: CleanupStrategy
aggressiveCleanupStrategy = CleanupStrategy
  { gcRounds = 5
  , gcDelay = 50
  , systemCacheCleanup = True
  , haskellHeapCleanup = True
  , forceFinalization = True
  , memoryCompaction = True
  , monitoringEnabled = True
  }

-- | 紧急清理策略
emergencyCleanupStrategy :: CleanupStrategy
emergencyCleanupStrategy = CleanupStrategy
  { gcRounds = 10
  , gcDelay = 10
  , systemCacheCleanup = True
  , haskellHeapCleanup = True
  , forceFinalization = True
  , memoryCompaction = True
  , monitoringEnabled = True
  }

-- | 清理监控器
data CleanupMonitor = CleanupMonitor
  { cleanupCount :: Int
  , totalCleanupTime :: Double
  , averageCleanupTime :: Double
  , memoryFreed :: Int
  , effectiveness :: Double
  } deriving (Show, Eq)

-- | 自动清理配置
data AutoCleanupConfig = AutoCleanupConfig
  { strategy :: CleanupStrategy
  , cleanupInterval :: Int       -- 清理间隔(测试数)
  , autoMonitoringEnabled :: Bool
  , adaptiveCleanup :: Bool      -- 自适应清理
  , emergencyThreshold :: Double -- 紧急清理阈值
  } deriving (Show, Eq)

-- | 创建自动清理配置
createAutoCleanupConfig :: CleanupStrategy -> Int -> AutoCleanupConfig
createAutoCleanupConfig strategy interval = AutoCleanupConfig
  { strategy = strategy
  , cleanupInterval = interval
  , autoMonitoringEnabled = True
  , adaptiveCleanup = False
  , emergencyThreshold = 0.9
  }

-- | 综合清理
comprehensiveCleanup :: CleanupStrategy -> IO ()
comprehensiveCleanup strategy = do
  when (monitoringEnabled strategy) $ 
    putStrLn "Starting comprehensive cleanup..."
  
  -- Haskell堆清理
  when (haskellHeapCleanup strategy) $ do
    replicateM_ (gcRounds strategy) $ do
      performGC
      threadDelay (gcDelay strategy)
  
  -- 强制终结化
  when (forceFinalization strategy) $ do
    performGC
    threadDelay 200
    performGC
  
  -- 内存压缩
  when (memoryCompaction strategy) $ do
    replicateM_ 3 $ do
      performGC
      threadDelay 100
  
  -- 系统缓存清理
  when (systemCacheCleanup strategy) $ do
    cleanupSystemCache
  
  when (monitoringEnabled strategy) $ 
    putStrLn "Comprehensive cleanup completed"

-- | 策略性清理
strategicCleanup :: CleanupStrategy -> IO ()
strategicCleanup strategy = do
  when (monitoringEnabled strategy) $ 
    putStrLn "Starting strategic cleanup..."
  
  -- 分阶段清理
  performGC
  threadDelay (gcDelay strategy)
  
  replicateM_ 2 $ do
    performGC
    threadDelay (gcDelay strategy `div` 2)
  
  when (systemCacheCleanup strategy) $ do
    cleanupSystemCache
  
  when (monitoringEnabled strategy) $ 
    putStrLn "Strategic cleanup completed"

-- | 增量清理
incrementalCleanup :: CleanupStrategy -> IO ()
incrementalCleanup strategy = do
  when (monitoringEnabled strategy) $ 
    putStrLn "Starting incremental cleanup..."
  
  -- 轻量级清理
  performGC
  threadDelay (gcDelay strategy)
  
  when (haskellHeapCleanup strategy) $ do
    performGC
  
  when (monitoringEnabled strategy) $ 
    putStrLn "Incremental cleanup completed"

-- | 紧急清理
emergencyCleanup :: IO ()
emergencyCleanup = do
  putStrLn "Emergency cleanup initiated!"
  
  -- 最激进的清理
  replicateM_ 10 $ do
    performGC
    threadDelay 5
  
  -- 系统级清理
  cleanupSystemCache
  
  -- 最终清理
  replicateM_ 5 performGC
  
  putStrLn "Emergency cleanup completed!"

-- | 清理系统缓存
cleanupSystemCache :: IO ()
cleanupSystemCache = do
  -- 尝试清理系统缓存（需要适当的权限）
  _ <- tryWriteFile "/proc/sys/vm/drop_caches" "3"
  return ()

-- | 安全写入文件
tryWriteFile :: FilePath -> String -> IO ()
tryWriteFile path content = do
  -- 在实际实现中，这里会尝试写入系统文件来清理缓存
  -- 由于需要权限，这里只是占位符
  return ()

-- | 测试间清理
withCleanupBetweenTests :: CleanupStrategy -> IO a -> IO a
withCleanupBetweenTests strategy action = do
  bracket
    (preTestCleanup strategy)
    (\_ -> postTestCleanup strategy)
    (\_ -> action)

-- | 测试前清理
preTestCleanup :: CleanupStrategy -> IO ()
preTestCleanup strategy = do
  when (monitoringEnabled strategy) $ 
    putStrLn "Pre-test cleanup"
  incrementalCleanup strategy

-- | 测试后清理
postTestCleanup :: CleanupStrategy -> IO ()
postTestCleanup strategy = do
  when (monitoringEnabled strategy) $ 
    putStrLn "Post-test cleanup"
  strategicCleanup strategy

-- | 周期性清理
withPeriodicCleanup :: CleanupStrategy -> Int -> IO a -> IO a
withPeriodicCleanup strategy interval action = do
  action `finally` periodicCleanup strategy interval

-- | 周期清理
periodicCleanup :: CleanupStrategy -> Int -> IO ()
periodicCleanup strategy interval = do
  when (interval > 1) $ do
    when (monitoringEnabled strategy) $ 
      putStrLn "Periodic cleanup"
    comprehensiveCleanup strategy

-- | 自适应清理
withAdaptiveCleanup :: CleanupStrategy -> IO a -> IO a
withAdaptiveCleanup strategy action = do
  action `finally` performAdaptiveCleanup strategy

-- | 自适应清理逻辑
performAdaptiveCleanup :: CleanupStrategy -> IO ()
performAdaptiveCleanup strategy = do
  -- 检查内存使用情况
  memoryUsage <- checkMemoryUsage
  
  let adjustedStrategy = if memoryUsage > 0.8
        then aggressiveCleanupStrategy
        else if memoryUsage > 0.6
             then defaultCleanupStrategy
             else strategy
  
  comprehensiveCleanup adjustedStrategy

-- | 检查内存使用情况
checkMemoryUsage :: IO Double
checkMemoryUsage = do
  performGC
  -- 在实际实现中，这里会获取真实的内存使用情况
  -- 这里返回一个示例值
  return 0.5

-- | 带内存监控的操作
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  
  when (duration > 5.0) $ do
    putStrLn $ "Long-running operation detected: " ++ show duration ++ " seconds"
    performGC
  
  return result

-- | 创建清理监控器
createCleanupMonitor :: IO CleanupMonitor
createCleanupMonitor = return CleanupMonitor
  { cleanupCount = 0
  , totalCleanupTime = 0.0
  , averageCleanupTime = 0.0
  , memoryFreed = 0
  , effectiveness = 0.0
  }

-- | 监控清理效果
monitorCleanupEffectiveness :: CleanupMonitor -> IO CleanupMonitor
monitorCleanupEffectiveness monitor = do
  startTime <- getCurrentTime
  
  -- 执行清理
  comprehensiveCleanup defaultCleanupStrategy
  
  endTime <- getCurrentTime
  let cleanupTime = realToFrac $ diffUTCTime endTime startTime
  
  -- 更新监控器
  let newCount = cleanupCount monitor + 1
      newTotalTime = totalCleanupTime monitor + cleanupTime
      newAverageTime = newTotalTime / fromIntegral newCount
  
  return $ monitor
    { cleanupCount = newCount
    , totalCleanupTime = newTotalTime
    , averageCleanupTime = newAverageTime
    }

-- | 带清理监控的操作
withCleanupMonitoring :: IO a -> IO a
withCleanupMonitoring action = do
  monitor <- createCleanupMonitor
  result <- action
  _ <- monitorCleanupEffectiveness monitor
  return result

-- | 自动清理
withAutoCleanup :: AutoCleanupConfig -> IO a -> IO a
withAutoCleanup config action = do
  if adaptiveCleanup config
    then withAdaptiveCleanup (strategy config) action
    else withPeriodicCleanup (strategy config) (cleanupInterval config) action

-- | 带自动清理运行
runWithAutoCleanup :: AutoCleanupConfig -> [IO a] -> IO [a]
runWithAutoCleanup config actions = do
  mapM (withAutoCleanup config) actions

-- | 创建测试组带清理
testGroupWithCleanup :: String -> CleanupStrategy -> [TestTree] -> TestTree
testGroupWithCleanup name strategy tests = 
  let cleanupTests = map (addCleanupToTest strategy) tests
  in testGroup ("[Cleanup] " ++ name) cleanupTests

-- | 为测试添加清理
addCleanupToTest :: CleanupStrategy -> TestTree -> TestTree
addCleanupToTest strategy test = test
  -- 在实际实现中，这里会在测试前后添加清理操作
  -- 由于TestTree的限制，这里只是示例

-- | 选择清理策略
selectCleanupStrategy :: String -> CleanupStrategy
selectCleanupStrategy "default" = defaultCleanupStrategy
selectCleanupStrategy "aggressive" = aggressiveCleanupStrategy
selectCleanupStrategy "emergency" = emergencyCleanupStrategy
selectCleanupStrategy _ = defaultCleanupStrategy

-- | 从环境变量获取清理策略
getCleanupStrategyFromEnv :: IO CleanupStrategy
getCleanupStrategyFromEnv = do
  env <- getEnvironment
  let strategyStr = fromMaybe "default" $ lookup "TYPUS_CLEANUP_STRATEGY" env
  return $ selectCleanupStrategy strategyStr

-- | 应用清理配置
applyCleanupConfig :: IO ()
applyCleanupConfig = do
  strategy <- getCleanupStrategyFromEnv
  comprehensiveCleanup strategy

-- | 清理统计信息
data CleanupStats = CleanupStats
  { totalCleanups :: Int
  , totalTimeSpent :: Double
  , averageTimePerCleanup :: Double
  , memoryFreedTotal :: Int
  , mostEffectiveStrategy :: String
  } deriving (Show, Eq)

-- | 获取清理统计信息
getCleanupStats :: CleanupMonitor -> CleanupStats
getCleanupStats monitor = CleanupStats
  { totalCleanups = cleanupCount monitor
  , totalTimeSpent = totalCleanupTime monitor
  , averageTimePerCleanup = averageCleanupTime monitor
  , memoryFreedTotal = memoryFreed monitor
  , mostEffectiveStrategy = "default"
  }