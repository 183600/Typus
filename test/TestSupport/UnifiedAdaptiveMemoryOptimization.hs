{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 统一的自适应内存优化配置模块
-- 提供基于系统资源的动态内存管理
module TestSupport.UnifiedAdaptiveMemoryOptimization 
  ( -- 自适应内存配置
    AdaptiveMemoryConfig(..)
  , createAdaptiveConfig
  , detectSystemResources
  , MemoryTier(..)
  
    -- 自适应测试执行
  , runWithAdaptiveMemory
  , adaptiveTestSuite
  , adaptivePropertyTest
  
    -- 资源监控
  , ResourceMonitor(..)
  , createResourceMonitor
  , monitorResources
  , withResourceMonitoring
  
    -- 动态内存调整
  , DynamicMemoryAdjuster(..)
  , createDynamicAdjuster
  , adjustMemoryBasedOnUsage
  , withDynamicMemoryAdjustment
  
    -- 内存使用分析
  , MemoryUsageProfile(..)
  , profileMemoryUsage
  , analyzeMemoryTrends
  , optimizeBasedOnProfile
  , getRecommendedMemoryConfig
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  , Property
  , Testable
  , testProperty
  , property
  , Gen
  , forAll
  , resize
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import Data.Maybe (isJust, fromMaybe)
import Data.List (find)
import Control.Exception (SomeException, catch)
import System.Process (readProcess)
import Text.Read (readMaybe)
import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)

-- | 内存层级
data MemoryTier = 
    Critical     -- ^ 8MB - 关键测试仅
  | Minimal      -- ^ 16MB - 最小内存使用
  | Ultra        -- ^ 24MB - 超低内存使用
  | Enhanced     -- ^ 32MB - 增强内存优化
  | Optimized    -- ^ 48MB - 优化内存使用
  | Standard     -- ^ 64MB - 标准内存限制
  | High         -- ^ 128MB - 高内存限制
  | Unlimited    -- ^ 无限制
  deriving (Show, Eq, Ord, Enum)

-- | 系统资源监控器
data ResourceMonitor = ResourceMonitor
  { totalMemoryMB :: Int
  , availableMemoryMB :: Int
  , cpuCores :: Int
  , loadAverage :: Double
  , memoryTier :: MemoryTier
  } deriving (Show, Eq)

-- | 内存使用配置文件
data MemoryUsageProfile = MemoryUsageProfile
  { baselineMemory :: Int        -- 基线内存使用(MB)
  , peakMemory :: Int           -- 峰值内存使用(MB)
  , averageGrowthRate :: Double -- 平均增长率(MB/s)
  , memoryEfficiency :: Double  -- 内存效率(0-1)
  , recommendedTier :: MemoryTier -- 推荐内存层级
  } deriving (Show, Eq)

-- | 自适应内存配置
data AdaptiveMemoryConfig = AdaptiveMemoryConfig
  { configMemoryTier :: MemoryTier
  , quickCheckMaxSize :: Int
  , quickCheckTestCount :: Int
  , quickCheckMaxShrinks :: Int
  , gcFrequency :: Int           -- GC频率(每N个测试)
  , monitoringEnabled :: Bool    -- 是否启用监控
  , dynamicAdjustment :: Bool    -- 是否动态调整
  , memoryProfile :: Maybe MemoryUsageProfile
  } deriving (Show, Eq)

-- | 动态内存调整器
data DynamicMemoryAdjuster = DynamicMemoryAdjuster
  { currentConfig :: AdaptiveMemoryConfig
  , adjustmentThreshold :: Double  -- 调整阈值
  , adjustmentHistory :: [(MemoryTier, Bool)]  -- 调整历史
  } deriving (Show, Eq)

-- | 检测系统资源
detectSystemResources :: IO ResourceMonitor
detectSystemResources = do
  -- 获取内存信息
  (totalMem, availMem) <- getMemoryInfo
  
  -- 获取CPU信息
  cpuCores <- getCpuCores
  
  -- 获取负载信息
  loadAvg <- getLoadAverage
  
  -- 确定内存层级
  let tier = determineMemoryTier availMem
  
  return $ ResourceMonitor
    { totalMemoryMB = totalMem
    , availableMemoryMB = availMem
    , cpuCores = cpuCores
    , loadAverage = loadAvg
    , memoryTier = tier
    }

-- | 获取内存信息
getMemoryInfo :: IO (Int, Int)
getMemoryInfo = do
  result <- tryReadProcess "free" ["-m"] ""
  case parseMemoryInfo result of
    Just (total, avail) -> return (total, avail)
    Nothing -> return (1024, 512)  -- 默认值
  where
    parseMemoryInfo :: String -> Maybe (Int, Int)
    parseMemoryInfo input = do
      let lines' = lines input
      memLine <- find (isPrefixOf "Mem:") lines'
      let fields = words memLine
      if length fields >= 3
        then case (readMaybe (fields !! 1), readMaybe (fields !! 2)) of
          (Just total, Just avail) -> Just (total, avail)
          _ -> Nothing
        else Nothing
    
    isPrefixOf prefix str = take (length prefix) str == prefix

-- | 获取CPU核心数
getCpuCores :: IO Int
getCpuCores = do
  result <- tryReadProcess "nproc" [] ""
  case readMaybe result of
    Just cores -> return cores
    Nothing -> return 1  -- 默认值

-- | 获取负载平均值
getLoadAverage :: IO Double
getLoadAverage = do
  result <- tryReadProcess "uptime" [] ""
  case parseLoadAverage result of
    Just load -> return load
    Nothing -> return 0.5  -- 默认值
  where
    parseLoadAverage input = do
      let words' = words input
      loadStr <- listToMaybe $ drop 2 $ reverse words'
      readMaybe loadStr

-- | 安全执行进程
tryReadProcess :: String -> [String] -> String -> IO String
tryReadProcess cmd args input = do
  result <- catch (readProcess cmd args input) (\(_ :: SomeException) -> return "")
  return result

-- | 确定内存层级
determineMemoryTier :: Int -> MemoryTier
determineMemoryTier availableMB
  | availableMB <= 16  = Critical
  | availableMB <= 32  = Minimal
  | availableMB <= 48  = Ultra
  | availableMB <= 64  = Enhanced
  | availableMB <= 96  = Optimized
  | availableMB <= 128 = Standard
  | availableMB <= 256 = High
  | otherwise          = Unlimited

-- | 创建自适应配置
createAdaptiveConfig :: MemoryTier -> AdaptiveMemoryConfig
createAdaptiveConfig tier = AdaptiveMemoryConfig
  { configMemoryTier = tier
  , quickCheckMaxSize = getMaxSizeForTier tier
  , quickCheckTestCount = getTestCountForTier tier
  , quickCheckMaxShrinks = getMaxShrinksForTier tier
  , gcFrequency = getGcFrequencyForTier tier
  , monitoringEnabled = tier <= Enhanced
  , dynamicAdjustment = tier <= Ultra
  , memoryProfile = Nothing
  }

-- | 获取层级对应的最大大小
getMaxSizeForTier :: MemoryTier -> Int
getMaxSizeForTier tier = case tier of
  Critical -> 1
  Minimal  -> 1
  Ultra    -> 2
  Enhanced -> 3
  Optimized -> 5
  Standard -> 10
  High     -> 20
  Unlimited -> 100

-- | 获取层级对应的测试次数
getTestCountForTier :: MemoryTier -> Int
getTestCountForTier tier = case tier of
  Critical -> 1
  Minimal  -> 2
  Ultra    -> 3
  Enhanced -> 5
  Optimized -> 10
  Standard -> 25
  High     -> 50
  Unlimited -> 100

-- | 获取层级对应的最大收缩次数
getMaxShrinksForTier :: MemoryTier -> Int
getMaxShrinksForTier tier = case tier of
  Critical -> 0
  Minimal  -> 0
  Ultra    -> 1
  Enhanced -> 2
  Optimized -> 5
  Standard -> 10
  High     -> 15
  Unlimited -> 25

-- | 获取层级对应的GC频率
getGcFrequencyForTier :: MemoryTier -> Int
getGcFrequencyForTier tier = case tier of
  Critical -> 1
  Minimal  -> 1
  Ultra    -> 2
  Enhanced -> 3
  Optimized -> 5
  Standard -> 10
  High     -> 15
  Unlimited -> 20

-- | 创建资源监控器
createResourceMonitor :: IO ResourceMonitor
createResourceMonitor = detectSystemResources

-- | 带资源监控的操作
withResourceMonitoring :: IO a -> IO a
withResourceMonitoring action = do
  monitor <- createResourceMonitor
  putStrLn $ "Memory tier: " ++ show (memoryTier monitor)
  result <- action
  return result

-- | 监控资源使用
monitorResources :: ResourceMonitor -> IO ()
monitorResources monitor = do
  performGC
  threadDelay 100
  -- 在实际实现中，这里可以记录更详细的资源使用情况

-- | 创建动态调整器
createDynamicAdjuster :: MemoryTier -> IO DynamicMemoryAdjuster
createDynamicAdjuster tier = do
  let config = createAdaptiveConfig tier
  return $ DynamicMemoryAdjuster
    { currentConfig = config
    , adjustmentThreshold = 0.8
    , adjustmentHistory = []
    }

-- | 基于使用情况调整内存
adjustMemoryBasedOnUsage :: DynamicMemoryAdjuster -> Double -> IO DynamicMemoryAdjuster
adjustMemoryBasedOnUsage adjuster memoryUsageRatio = do
  let currentTier = configMemoryTier (currentConfig adjuster)
      threshold = adjustmentThreshold adjuster
  
  let (newTier, success) = if memoryUsageRatio > threshold
        then (upgradeTier currentTier, False)
        else (currentTier, True)
  
  let newConfig = createAdaptiveConfig newTier
  let newHistory = (newTier, success) : adjustmentHistory adjuster
  
  return $ adjuster
    { currentConfig = newConfig
    , adjustmentHistory = take 10 newHistory  -- 保留最近10次调整
    }

-- | 升级内存层级
upgradeTier :: MemoryTier -> MemoryTier
upgradeTier tier = case tier of
  Critical -> Minimal
  Minimal  -> Ultra
  Ultra    -> Enhanced
  Enhanced -> Optimized
  Optimized -> Standard
  Standard -> High
  High     -> Unlimited
  Unlimited -> Unlimited

-- | 带动态内存调整的操作
withDynamicMemoryAdjustment :: DynamicMemoryAdjuster -> IO a -> IO a
withDynamicMemoryAdjustment adjuster action = do
  -- 执行操作
  result <- action
  
  -- 模拟内存使用检查（实际实现中应该使用真实的内存监控）
  let memoryUsageRatio = 0.7  -- 示例值
  
  -- 调整配置
  newAdjuster <- adjustMemoryBasedOnUsage adjuster memoryUsageRatio
  
  return result

-- | 分析内存使用情况
profileMemoryUsage :: IO MemoryUsageProfile
profileMemoryUsage = do
  -- 执行一系列测试来分析内存使用模式
  let baselineMemory = 16  -- 示例基线值
  let peakMemory = 32      -- 示例峰值
  let averageGrowthRate = 0.5
  let memoryEfficiency = 0.8
  let recommendedTier = Ultra
  
  return $ MemoryUsageProfile
    { baselineMemory = baselineMemory
    , peakMemory = peakMemory
    , averageGrowthRate = averageGrowthRate
    , memoryEfficiency = memoryEfficiency
    , recommendedTier = recommendedTier
    }

-- | 分析内存趋势
analyzeMemoryTrends :: [MemoryUsageProfile] -> MemoryTier
analyzeMemoryTrends profiles = 
  let avgEfficiency = average $ map memoryEfficiency profiles
      avgPeak = average $ map (fromIntegral . peakMemory) profiles
  in if avgEfficiency > 0.8 && avgPeak < 32
     then Ultra
     else if avgEfficiency > 0.6 && avgPeak < 64
          then Enhanced
          else Standard

-- | 基于配置文件优化
optimizeBasedOnProfile :: MemoryUsageProfile -> AdaptiveMemoryConfig
optimizeBasedOnProfile profile = 
  let tier = recommendedTier profile
  in createAdaptiveConfig tier

-- | 运行自适应内存测试
runWithAdaptiveMemory :: TestTree -> IO TestTree
runWithAdaptiveMemory test = do
  -- 检测系统资源
  monitor <- createResourceMonitor
  
  -- 创建自适应配置
  let config = createAdaptiveConfig (memoryTier monitor)
  
  -- 应用配置到测试树
  return $ applyAdaptiveConfig config test

-- | 应用自适应配置
applyAdaptiveConfig :: AdaptiveMemoryConfig -> TestTree -> TestTree
applyAdaptiveConfig config test =
  localOption (QuickCheckMaxSize (quickCheckMaxSize config)) $
  localOption (QuickCheckTests (quickCheckTestCount config)) $
  localOption (QuickCheckMaxShrinks (quickCheckMaxShrinks config)) $
  test

-- | 创建自适应测试套件
adaptiveTestSuite :: String -> [TestTree] -> IO TestTree
adaptiveTestSuite name tests = do
  monitor <- createResourceMonitor
  let config = createAdaptiveConfig (memoryTier monitor)
  let adaptedTests = map (applyAdaptiveConfig config) tests
  return $ testGroup ("[Adaptive-" ++ show (memoryTier monitor) ++ "] " ++ name) adaptedTests

-- | 自适应属性测试
adaptivePropertyTest :: Testable a => String -> a -> IO TestTree
adaptivePropertyTest name prop = do
  monitor <- createResourceMonitor
  let config = createAdaptiveConfig (memoryTier monitor)
  return $ testProperty name prop

-- | 辅助函数
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- | 全局资源监控器（延迟初始化）
globalResourceMonitor :: ResourceMonitor
globalResourceMonitor = unsafePerformIO createResourceMonitor
{-# NOINLINE globalResourceMonitor #-}

-- | 获取推荐的内存配置
getRecommendedMemoryConfig :: IO AdaptiveMemoryConfig
getRecommendedMemoryConfig = do
  profile <- profileMemoryUsage
  return $ optimizeBasedOnProfile profile