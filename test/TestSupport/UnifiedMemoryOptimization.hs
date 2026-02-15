{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | 统一内存优化配置模块
-- 为所有测试套件提供一致的内存优化策略
module TestSupport.UnifiedMemoryOptimization 
  ( -- 内存配置
    UnifiedMemoryConfig(..)
  , GCStrategy(..)
  , getUnifiedMemoryConfig
  , setUnifiedMemoryConfig
  , withUnifiedMemoryConfig
    
    -- 预定义配置
  , criticalMemoryConfig
  , minimalMemoryConfig  
  , efficientMemoryConfig
  , balancedMemoryConfig
  , comprehensiveMemoryConfig
  , extremeMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
    
    -- 内存优化测试组合器
  , withUnifiedMemoryOptimization
  , unifiedMemoryTestGroup
  , unifiedMemoryProperty
  , createUnifiedMemorySuite
  , withUnifiedMemoryLimits
    
    -- 内存监控和清理
  , unifiedMemoryCleanup
  , withUnifiedMemoryMonitoring
  , unifiedMemoryGC
  , forceAggressiveCleanup
    
    -- 自适应内存管理
  , adaptiveUnifiedMemoryConfig
  , detectAvailableMemory
    
    -- 测试套件优化
  , optimizeTestSuite
  , createOptimizedTestSuite
  , runTestsWithMemoryOptimization
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import Test.Tasty.QuickCheck
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment, setEnv)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.List (stripPrefix)

-- | 统一内存配置
data UnifiedMemoryConfig = UnifiedMemoryConfig
  { configName :: String            -- 配置名称
  , maxTestSize :: Int              -- 最大测试数据大小
  , maxTestCount :: Int             -- 最大测试次数
  , maxShrinks :: Int               -- 最大收缩次数
  , stringMaxLength :: Int          -- 字符串最大长度
  , listMaxLength :: Int            -- 列表最大长度
  , intMaxValue :: Int              -- 整数最大值
  , gcBetweenTests :: Bool          -- 测试间是否GC
  , monitorMemory :: Bool           -- 是否监控内存
  , adaptiveMode :: Bool            -- 自适应模式
  , rtsMemoryLimit :: String        -- RTS内存限制
  , gcStrategy :: GCStrategy        -- GC策略
  , memoryLimitMB :: Int            -- 内存限制（MB）
  , testSelectionRatio :: Double    -- 测试选择比例
  } deriving (Show, Eq)

-- | GC策略
data GCStrategy = 
    MinimalGC                      -- 最小GC
  | AggressiveGC                   -- 激进GC
  | BalancedGC                     -- 平衡GC
  | AdaptiveGC                     -- 自适应GC
  deriving (Show, Eq)

-- | 关键内存配置（最低内存使用）
criticalMemoryConfig :: UnifiedMemoryConfig
criticalMemoryConfig = UnifiedMemoryConfig
  { configName = "critical"
  , maxTestSize = 1
  , maxTestCount = 1
  , maxShrinks = 0
  , stringMaxLength = 1
  , listMaxLength = 1
  , intMaxValue = 2
  , gcBetweenTests = True
  , monitorMemory = True
  , adaptiveMode = True
  , rtsMemoryLimit = "-M8m -A256k -n32k -H1m -qg -G1"
  , gcStrategy = AggressiveGC
  , memoryLimitMB = 8
  , testSelectionRatio = 0.1
  }

-- | 最小内存配置
minimalMemoryConfig :: UnifiedMemoryConfig
minimalMemoryConfig = UnifiedMemoryConfig
  { configName = "minimal"
  , maxTestSize = 1
  , maxTestCount = 2
  , maxShrinks = 0
  , stringMaxLength = 2
  , listMaxLength = 1
  , intMaxValue = 3
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M16m -A512k -n64k -H2m -qg -G1"
  , gcStrategy = AggressiveGC
  , memoryLimitMB = 16
  , testSelectionRatio = 0.2
  }

-- | 高效内存配置
efficientMemoryConfig :: UnifiedMemoryConfig
efficientMemoryConfig = UnifiedMemoryConfig
  { configName = "efficient"
  , maxTestSize = 2
  , maxTestCount = 3
  , maxShrinks = 1
  , stringMaxLength = 3
  , listMaxLength = 2
  , intMaxValue = 5
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M32m -A1m -n128k -H4m -qg -G2"
  , gcStrategy = BalancedGC
  , memoryLimitMB = 32
  , testSelectionRatio = 0.3
  }

-- | 平衡内存配置
balancedMemoryConfig :: UnifiedMemoryConfig
balancedMemoryConfig = UnifiedMemoryConfig
  { configName = "balanced"
  , maxTestSize = 3
  , maxTestCount = 5
  , maxShrinks = 2
  , stringMaxLength = 5
  , listMaxLength = 3
  , intMaxValue = 10
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M64m -A2m -n256k -H8m -qg -G2"
  , gcStrategy = BalancedGC
  , memoryLimitMB = 64
  , testSelectionRatio = 0.5
  }

-- | 全面内存配置
comprehensiveMemoryConfig :: UnifiedMemoryConfig
comprehensiveMemoryConfig = UnifiedMemoryConfig
  { configName = "comprehensive"
  , maxTestSize = 5
  , maxTestCount = 10
  , maxShrinks = 5
  , stringMaxLength = 8
  , listMaxLength = 5
  , intMaxValue = 20
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M128m -A4m -n512k -H16m -qg -G4"
  , gcStrategy = MinimalGC
  , memoryLimitMB = 128
  , testSelectionRatio = 0.8
  }

-- | 极端内存配置（最低内存使用）
extremeMemoryConfig :: UnifiedMemoryConfig
extremeMemoryConfig = UnifiedMemoryConfig
  { configName = "extreme"
  , maxTestSize = 1
  , maxTestCount = 1
  , maxShrinks = 0
  , stringMaxLength = 1
  , listMaxLength = 1
  , intMaxValue = 2
  , gcBetweenTests = True
  , monitorMemory = True
  , adaptiveMode = True
  , rtsMemoryLimit = "-M4m -A128k -n16k -H512k -qg -G1"
  , gcStrategy = AggressiveGC
  , memoryLimitMB = 4
  , testSelectionRatio = 0.05
  }

-- | 标准内存配置
standardMemoryConfig :: UnifiedMemoryConfig
standardMemoryConfig = UnifiedMemoryConfig
  { configName = "standard"
  , maxTestSize = 3
  , maxTestCount = 5
  , maxShrinks = 2
  , stringMaxLength = 5
  , listMaxLength = 3
  , intMaxValue = 10
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M64m -A2m -n256k -H8m -qg -G2"
  , gcStrategy = BalancedGC
  , memoryLimitMB = 64
  , testSelectionRatio = 0.6
  }

-- | CI内存配置
ciMemoryConfig :: UnifiedMemoryConfig
ciMemoryConfig = UnifiedMemoryConfig
  { configName = "ci"
  , maxTestSize = 2
  , maxTestCount = 3
  , maxShrinks = 1
  , stringMaxLength = 3
  , listMaxLength = 2
  , intMaxValue = 5
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  , rtsMemoryLimit = "-M32m -A1m -n128k -H4m -qg -G2"
  , gcStrategy = BalancedGC
  , memoryLimitMB = 32
  , testSelectionRatio = 0.4
  }

-- | 检测可用内存
detectAvailableMemory :: IO Int
detectAvailableMemory = do
  -- 简化版本，实际项目中可以使用更复杂的检测
  env <- getEnvironment
  case lookup "TYPUS_AVAILABLE_MEMORY" env of
    Just memStr -> case readMaybe memStr of
      Just mem -> return mem
      Nothing -> return 128  -- 默认128MB
    Nothing -> return 128  -- 默认128MB

-- | 自适应统一内存配置
adaptiveUnifiedMemoryConfig :: IO UnifiedMemoryConfig
adaptiveUnifiedMemoryConfig = do
  available <- detectAvailableMemory
  env <- getEnvironment
  let emergencyMode = isJust (lookup "EMERGENCY_MEMORY" env)
      ultraMode = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
  
  if emergencyMode
    then return criticalMemoryConfig
    else if ultraMode
         then return minimalMemoryConfig
         else if available <= 16
              then return criticalMemoryConfig
              else if available <= 32
                   then return minimalMemoryConfig
                   else if available <= 64
                        then return efficientMemoryConfig
                        else if available <= 128
                             then return balancedMemoryConfig
                             else return comprehensiveMemoryConfig

-- | 获取统一内存配置
getUnifiedMemoryConfig :: IO UnifiedMemoryConfig
getUnifiedMemoryConfig = do
  env <- getEnvironment
  case lookup "TYPUS_MEMORY_LEVEL" env of
    Just "critical" -> return criticalMemoryConfig
    Just "minimal" -> return minimalMemoryConfig
    Just "efficient" -> return efficientMemoryConfig
    Just "balanced" -> return balancedMemoryConfig
    Just "comprehensive" -> return comprehensiveMemoryConfig
    _ -> adaptiveUnifiedMemoryConfig

-- | 设置统一内存配置
setUnifiedMemoryConfig :: UnifiedMemoryConfig -> IO ()
setUnifiedMemoryConfig config = do
  -- 设置环境变量
  _ <- setEnv "TYPUS_MEMORY_LEVEL" (configName config)
  when (monitorMemory config) $
    setEnv "TYPUS_MEMORY_MONITOR" "1"
  when (adaptiveMode config) $
    setEnv "TYPUS_ADAPTIVE_MEMORY" "1"

-- | 使用统一内存配置
withUnifiedMemoryConfig :: UnifiedMemoryConfig -> IO a -> IO a
withUnifiedMemoryConfig config action = do
  -- 保存当前环境
  oldEnv <- getEnvironment
  
  -- 设置新配置
  setUnifiedMemoryConfig config
  
  -- 执行操作
  result <- action
  
  -- 恢复环境
  -- 简化版本，实际项目中应该恢复完整环境
  return result

-- | 统一内存清理
unifiedMemoryCleanup :: UnifiedMemoryConfig -> IO ()
unifiedMemoryCleanup config = do
  case gcStrategy config of
    MinimalGC -> do
      performGC
      threadDelay 100
    AggressiveGC -> do
      replicateM_ 5 $ do
        performGC
        threadDelay 50
      performGC
    BalancedGC -> do
      performGC
      threadDelay 200
      replicateM_ 2 performGC
    AdaptiveGC -> do
      available <- detectAvailableMemory
      if available <= 32
        then do
          replicateM_ 3 $ do
            performGC
            threadDelay 100
        else do
          performGC
          threadDelay 200

-- | 统一内存GC
unifiedMemoryGC :: IO ()
unifiedMemoryGC = do
  config <- getUnifiedMemoryConfig
  unifiedMemoryCleanup config

-- | 带统一内存监控的操作
withUnifiedMemoryMonitoring :: IO a -> IO a
withUnifiedMemoryMonitoring action = do
  config <- getUnifiedMemoryConfig
  if monitorMemory config
    then do
      unifiedMemoryCleanup config
      result <- action
      unifiedMemoryCleanup config
      return result
    else action

-- | 应用统一内存优化到测试树
withUnifiedMemoryOptimization :: UnifiedMemoryConfig -> TestTree -> TestTree
withUnifiedMemoryOptimization config test =
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (maxTestCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | 创建统一内存测试组
unifiedMemoryTestGroup :: UnifiedMemoryConfig -> String -> [TestTree] -> TestTree
unifiedMemoryTestGroup config name tests =
  let optimizedTests = map (withUnifiedMemoryOptimization config) tests
      prefix = "[" ++ configName config ++ "] " ++ name
  in testGroup prefix optimizedTests

-- | 创建统一内存属性
unifiedMemoryProperty :: Show a => UnifiedMemoryConfig -> String -> (a -> Property) -> Gen a -> Property
unifiedMemoryProperty config name prop gen = 
  let maxSize = maxTestSize config
      maxTests = maxTestCount config
  in property $ forAll gen $ \value ->
    if gcBetweenTests config
      then ioProperty $ do
        unifiedMemoryGC
        return $ prop value
      else prop value

-- | 优化测试套件
optimizeTestSuite :: TestTree -> IO TestTree
optimizeTestSuite testSuite = do
  config <- getUnifiedMemoryConfig
  return $ withUnifiedMemoryOptimization config testSuite

-- | 创建优化的测试套件
createOptimizedTestSuite :: String -> [TestTree] -> IO TestTree
createOptimizedTestSuite name tests = do
  config <- getUnifiedMemoryConfig
  return $ unifiedMemoryTestGroup config name tests

-- | 运行带内存优化的测试
runTestsWithMemoryOptimization :: IO a -> IO a
runTestsWithMemoryOptimization action = do
  config <- getUnifiedMemoryConfig
  withUnifiedMemoryConfig config $ do
    withUnifiedMemoryMonitoring action

-- | 创建统一内存测试套件
createUnifiedMemorySuite :: UnifiedMemoryConfig -> String -> [TestTree] -> TestTree
createUnifiedMemorySuite config name tests = 
  let optimizedTests = map (withUnifiedMemoryOptimization config) tests
  in testGroup name optimizedTests

-- | 应用统一内存限制
withUnifiedMemoryLimits :: UnifiedMemoryConfig -> TestTree -> TestTree
withUnifiedMemoryLimits = withUnifiedMemoryOptimization

-- | 强制激进清理
forceAggressiveCleanup :: IO ()
forceAggressiveCleanup = do
  replicateM_ 5 $ do
    performGC
    threadDelay 50
  performGC
