{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 增强内存监控和清理模块
-- 这个模块提供了高级内存监控、清理和优化功能
module TestSupport.EnhancedMemoryMonitor 
  ( -- 内存监控
    MemoryMonitor(..)
  , MemorySnapshot(..)
  , createMemoryMonitor
  , takeMemorySnapshot
  , monitorMemoryUsage
  , continuousMemoryMonitoring
    
    -- 内存清理
  , MemoryCleanupStrategy(..)
  , performMemoryCleanup
  , performAggressiveCleanup
  , performEmergencyCleanup
  , schedulePeriodicCleanup
    
    -- 内存优化
  , MemoryOptimization(..)
  , optimizeMemoryUsage
  , applyMemoryOptimization
  , createMemoryOptimizedAction
    
    -- 内存分析
  , MemoryAnalysisReport(..)
  , analyzeMemoryUsage
  , generateMemoryReport
  , printMemoryAnalysis
    
    -- 实用工具
    , withMemoryMonitoring
    , withMemoryCleanup
    , withMemoryOptimization
  ) where

import System.Mem (performGC)
import Control.Monad (replicateM_, when, void, forever)
import Control.Concurrent (threadDelay, forkIO, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.List (sort, groupBy, maximumBy)
import Data.Function (on)
import Data.Ord (comparing)

-- | 内存快照
data MemorySnapshot = MemorySnapshot
  { timestamp :: String              -- ^ 时间戳
  , estimatedHeapMB :: Int           -- ^ 估算堆内存（MB）
  , estimatedGCCount :: Int          -- ^ 估算GC次数
  , availableMemoryMB :: Int         -- ^ 可用内存（MB）
  , memoryPressure :: Double         -- ^ 内存压力（0.0-1.0）
  } deriving (Show, Eq)

-- | 内存监控器
data MemoryMonitor = MemoryMonitor
  { snapshots :: [MemorySnapshot]    -- ^ 内存快照历史
  , maxSnapshots :: Int              -- ^ 最大快照数量
  , monitoringEnabled :: Bool        -- ^ 是否启用监控
  , cleanupThreshold :: Double       -- ^ 清理阈值
  } deriving (Show, Eq)

-- | 内存清理策略
data MemoryCleanupStrategy = 
    LightCleanup                    -- ^ 轻量清理（1次GC）
  | StandardCleanup                 -- ^ 标准清理（3次GC）
  | AggressiveCleanup               -- ^ 激进清理（5次GC + 延迟）
  | EmergencyCleanup                -- ^ 紧急清理（10次GC + 长延迟）
  deriving (Show, Eq)

-- | 内存优化配置
data MemoryOptimization = MemoryOptimization
  { enableProfiling :: Bool         -- ^ 启用性能分析
  , maxMemoryMB :: Int              -- ^ 最大内存限制
  , gcFrequency :: Int              -- ^ GC频率
  , enableAdaptiveCleanup :: Bool   -- ^ 自适应清理
  , memoryThreshold :: Double       -- ^ 内存阈值
  } deriving (Show, Eq)

-- | 内存分析报告
data MemoryAnalysisReport = MemoryAnalysisReport
  { totalSnapshots :: Int           -- ^ 总快照数
  , averageMemoryMB :: Double       -- ^ 平均内存使用
  , peakMemoryMB :: Int             -- ^ 峰值内存
  , memoryGrowthRate :: Double      -- ^ 内存增长率
  , cleanupFrequency :: Double      -- ^ 清理频率
  , recommendations :: [String]     -- ^ 优化建议
  } deriving (Show, Eq)

-- | 创建内存监控器
createMemoryMonitor :: Int -> Double -> MemoryMonitor
createMemoryMonitor maxSnapshots cleanupThreshold = MemoryMonitor
  { snapshots = []
  , maxSnapshots = maxSnapshots
  , monitoringEnabled = True
  , cleanupThreshold = cleanupThreshold
  }

-- | 获取当前时间戳
getCurrentTimestamp :: IO String
getCurrentTimestamp = do
  now <- getCurrentTime
  return $ show now

-- | 估算堆内存（简化实现）
estimateHeapMemory :: IO Int
estimateHeapMemory = do
  -- 强制GC以获得更准确的内存估算
  performGC
  -- 简化实现：返回一个估算值
  return 32  -- 假设32MB基础内存

-- | 估算可用内存
estimateAvailableMemory :: IO Int
estimateAvailableMemory = do
  -- 简化实现：返回一个估算值
  return 256  -- 假设256MB可用内存

-- | 计算内存压力
calculateMemoryPressure :: Int -> Int -> Double
calculateMemoryPressure used available = 
  let total = used + available
  in if total > 0
     then fromIntegral used / fromIntegral total
     else 0.0

-- | 获取内存快照
takeMemorySnapshot :: IO MemorySnapshot
takeMemorySnapshot = do
  timestamp <- getCurrentTimestamp
  heapMB <- estimateHeapMemory
  availableMB <- estimateAvailableMemory
  let pressure = calculateMemoryPressure heapMB availableMB
      gcCount = 0  -- 简化实现
  return MemorySnapshot
    { timestamp = timestamp
    , estimatedHeapMB = heapMB
    , estimatedGCCount = gcCount
    , availableMemoryMB = availableMB
    , memoryPressure = pressure
    }

-- | 添加快照到监控器
addSnapshot :: MemoryMonitor -> MemorySnapshot -> MemoryMonitor
addSnapshot monitor snapshot = 
  let newSnapshots = take (maxSnapshots monitor) (snapshot : snapshots monitor)
  in monitor { snapshots = newSnapshots }

-- | 监控内存使用
monitorMemoryUsage :: MemoryMonitor -> IO a -> IO a
monitorMemoryUsage monitor action = 
  if not (monitoringEnabled monitor)
  then action
  else do
    -- 开始监控
    startSnapshot <- takeMemorySnapshot
    let monitorWithSnapshot = addSnapshot monitor startSnapshot
    
    -- 执行动作
    result <- action
    
    -- 结束监控
    endSnapshot <- takeMemorySnapshot
    let finalMonitor = addSnapshot monitorWithSnapshot endSnapshot
    
    -- 检查是否需要清理
    when (memoryPressure endSnapshot > cleanupThreshold monitor) $
      performMemoryCleanup StandardCleanup
    
    return result

-- | 持续内存监控
continuousMemoryMonitoring :: MemoryMonitor -> IO ()
continuousMemoryMonitoring monitor = 
  if not (monitoringEnabled monitor)
  then return ()
  else forever $ do
    snapshot <- takeMemorySnapshot
    let updatedMonitor = addSnapshot monitor snapshot
    printf "[Memory] Heap: %dMB, Available: %dMB, Pressure: %.2f\n"
      (estimatedHeapMB snapshot)
      (availableMemoryMB snapshot)
      (memoryPressure snapshot)
    hFlush stdout
    
    -- 检查内存压力
    when (memoryPressure snapshot > cleanupThreshold updatedMonitor) $
      performMemoryCleanup StandardCleanup
    
    threadDelay 5000000  -- 5秒间隔

-- | 执行内存清理
performMemoryCleanup :: MemoryCleanupStrategy -> IO ()
performMemoryCleanup strategy = case strategy of
  LightCleanup -> do
    performGC
    threadDelay 1000
    
  StandardCleanup -> do
    replicateM_ 3 $ do
      performGC
      threadDelay 2000
    
  AggressiveCleanup -> do
    replicateM_ 5 $ do
      performGC
      threadDelay 3000
    replicateM_ 2 performGC
    
  EmergencyCleanup -> do
    replicateM_ 8 $ do
      performGC
      threadDelay 1000
    replicateM_ 5 $ do
      performGC
      threadDelay 2000
    replicateM_ 3 performGC

-- | 执行激进清理
performAggressiveCleanup :: IO ()
performAggressiveCleanup = performMemoryCleanup AggressiveCleanup

-- | 执行紧急清理
performEmergencyCleanup :: IO ()
performEmergencyCleanup = performMemoryCleanup EmergencyCleanup

-- | 调度周期性清理
schedulePeriodicCleanup :: MemoryCleanupStrategy -> Int -> IO ()
schedulePeriodicCleanup strategy intervalSeconds = forever $ do
  performMemoryCleanup strategy
  threadDelay (intervalSeconds * 1000000)

-- | 创建内存优化配置
defaultMemoryOptimization :: MemoryOptimization
defaultMemoryOptimization = MemoryOptimization
  { enableProfiling = False
  , maxMemoryMB = 128
  , gcFrequency = 10
  , enableAdaptiveCleanup = True
  , memoryThreshold = 0.8
  }

-- | 优化内存使用
optimizeMemoryUsage :: MemoryOptimization -> IO a -> IO a
optimizeMemoryUsage optimization action = do
  -- 初始清理
  when (enableAdaptiveCleanup optimization) $
    performMemoryCleanup StandardCleanup
  
  -- 执行动作
  result <- action
  
  -- 后续清理
  when (enableAdaptiveCleanup optimization) $
    performMemoryCleanup StandardCleanup
  
  return result

-- | 应用内存优化
applyMemoryOptimization :: MemoryOptimization -> IO ()
applyMemoryOptimization optimization = do
  printf "[Memory] Applying optimization with max %dMB memory\n" (maxMemoryMB optimization)
  performMemoryCleanup StandardCleanup

-- | 创建内存优化的动作
createMemoryOptimizedAction :: MemoryOptimization -> IO a -> IO a
createMemoryOptimizedAction optimization action = do
  bracket
    (do
      printf "[Memory] Starting optimized action\n"
      performMemoryCleanup StandardCleanup
    )
    (\_ -> do
      printf "[Memory] Cleaning up after optimized action\n"
      performMemoryCleanup StandardCleanup
    )
    (\_ -> action)

-- | 分析内存使用
analyzeMemoryUsage :: [MemorySnapshot] -> MemoryAnalysisReport
analyzeMemoryUsage snapshots = 
  let total = length snapshots
      avgMemory = if total > 0 
                 then fromIntegral (sum (map estimatedHeapMB snapshots)) / fromIntegral total
                 else 0.0
      peakMemory = if total > 0 
                  then maximum (map estimatedHeapMB snapshots)
                  else 0
      growthRate = if total > 1
                  then calculateGrowthRate snapshots
                  else 0.0
      cleanupFreq = calculateCleanupFrequency snapshots
      recommendations = generateRecommendations avgMemory peakMemory growthRate
  in MemoryAnalysisReport
    { totalSnapshots = total
    , averageMemoryMB = avgMemory
    , peakMemoryMB = peakMemory
    , memoryGrowthRate = growthRate
    , cleanupFrequency = cleanupFreq
    , recommendations = recommendations
    }

-- | 计算内存增长率
calculateGrowthRate :: [MemorySnapshot] -> Double
calculateGrowthRate snapshots = 
  let sortedSnapshots = sort $ map estimatedHeapMB snapshots
      first = head sortedSnapshots
      lastValue = last sortedSnapshots
  in if first > 0
     then fromIntegral (lastValue - first) / fromIntegral first
     else 0.0

-- | 计算清理频率
calculateCleanupFrequency :: [MemorySnapshot] -> Double
calculateCleanupFrequency snapshots = 
  let highPressureSnapshots = filter (\s -> memoryPressure s > 0.8) snapshots
      total = length snapshots
  in if total > 0
     then fromIntegral (length highPressureSnapshots) / fromIntegral total
     else 0.0

-- | 生成优化建议
generateRecommendations :: Double -> Int -> Double -> [String]
generateRecommendations avgMemory peakMemory growthRate
  | avgMemory > 100 = ["考虑减少测试数据大小", "增加GC频率"]
  | peakMemory > 200 = ["峰值内存过高，建议优化测试", "使用更严格的内存限制"]
  | growthRate > 0.5 = ["检测到内存泄漏，检查测试实现", "考虑使用更激进的清理策略"]
  | otherwise = ["内存使用正常", "继续当前配置"]

-- | 生成内存报告
generateMemoryReport :: MemoryMonitor -> IO MemoryAnalysisReport
generateMemoryReport monitor = do
  return $ analyzeMemoryUsage (snapshots monitor)

-- | 打印内存分析
printMemoryAnalysis :: MemoryAnalysisReport -> IO ()
printMemoryAnalysis report = do
  putStrLn "=== 内存分析报告 ==="
  putStrLn $ "总快照数: " ++ show (totalSnapshots report)
  putStrLn $ "平均内存使用: " ++ printf "%.2f" (averageMemoryMB report) ++ "MB"
  putStrLn $ "峰值内存: " ++ show (peakMemoryMB report) ++ "MB"
  putStrLn $ "内存增长率: " ++ printf "%.2f" (memoryGrowthRate report * 100) ++ "%"
  putStrLn $ "清理频率: " ++ printf "%.2f" (cleanupFrequency report * 100) ++ "%"
  putStrLn ""
  putStrLn "优化建议:"
  mapM_ (\rec -> putStrLn $ "  - " ++ rec) (recommendations report)

-- | 带内存监控的动作
withMemoryMonitoring :: MemoryMonitor -> IO a -> IO a
withMemoryMonitoring = monitorMemoryUsage

-- | 带内存清理的动作
withMemoryCleanup :: MemoryCleanupStrategy -> IO a -> IO a
withMemoryCleanup strategy action = do
  performMemoryCleanup strategy
  result <- action
  performMemoryCleanup strategy
  return result

-- | 带内存优化的动作
withMemoryOptimization :: MemoryOptimization -> IO a -> IO a
withMemoryOptimization = optimizeMemoryUsage