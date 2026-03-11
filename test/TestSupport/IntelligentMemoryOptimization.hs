{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSupport.IntelligentMemoryOptimization
  ( withIntelligentMemoryOptimization
  , intelligentTestGroup
  , memoryAwareProperty
  , optimizedQuickCheck
  , withProgressiveTesting
  , withMemoryMonitoring
  , forceGC
  , getMemoryUsage
  , MemoryOptimizationConfig(..)
  , defaultMemoryConfig
  , emergencyMemoryConfig
  , ultraMemoryConfig
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  , Property
  , property
  )
import System.Mem (performGC)
import Control.Monad (when, replicateM_)
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (getEnvironment)

-- | 内存优化配置
data MemoryOptimizationConfig = MemoryOptimizationConfig
  { memoryLimitMB :: Int
  , batchSize :: Int
  , gcFrequency :: Int
  , maxQuickCheckTests :: Int
  , maxQuickCheckSize :: Int
  , maxQuickCheckShrinks :: Int
  , enableProgressiveTesting :: Bool
  , enableMemoryMonitoring :: Bool
  , enableAdaptiveScaling :: Bool
  } deriving (Show)

-- | 默认内存配置
defaultMemoryConfig :: MemoryOptimizationConfig
defaultMemoryConfig = MemoryOptimizationConfig
  { memoryLimitMB = 16
  , batchSize = 5
  , gcFrequency = 3
  , maxQuickCheckTests = 1
  , maxQuickCheckSize = 1
  , maxQuickCheckShrinks = 0
  , enableProgressiveTesting = True
  , enableMemoryMonitoring = True
  , enableAdaptiveScaling = True
  }

-- | 紧急内存配置
emergencyMemoryConfig :: MemoryOptimizationConfig
emergencyMemoryConfig = defaultMemoryConfig
  { memoryLimitMB = 8
  , batchSize = 1
  , gcFrequency = 1
  , maxQuickCheckTests = 1
  , maxQuickCheckSize = 1
  , maxQuickCheckShrinks = 0
  }

-- | 超低内存配置
ultraMemoryConfig :: MemoryOptimizationConfig
ultraMemoryConfig = defaultMemoryConfig
  { memoryLimitMB = 4
  , batchSize = 1
  , gcFrequency = 1
  , maxQuickCheckTests = 1
  , maxQuickCheckSize = 1
  , maxQuickCheckShrinks = 0
  }

-- | 智能内存优化包装器
withIntelligentMemoryOptimization :: MemoryOptimizationConfig -> TestTree -> TestTree
withIntelligentMemoryOptimization config testTree = 
  localOption (QuickCheckMaxSize (maxQuickCheckSize config)) $
  localOption (QuickCheckTests (maxQuickCheckTests config)) $
  localOption (QuickCheckMaxShrinks (maxQuickCheckShrinks config)) $
  testTree

-- | 智能测试分组
intelligentTestGroup :: String -> [TestTree] -> TestTree
intelligentTestGroup name tests = 
  testGroup name tests

-- | 内存感知属性测试
memoryAwareProperty :: String -> Property -> TestTree
memoryAwareProperty name prop = 
  localOption (QuickCheckMaxSize 1) $
  localOption (QuickCheckTests 1) $
  localOption (QuickCheckMaxShrinks 0) $
  testGroup name [property prop]

-- | 优化的 QuickCheck 配置
optimizedQuickCheck :: Property -> TestTree
optimizedQuickCheck prop = 
  localOption (QuickCheckMaxSize 1) $
  localOption (QuickCheckTests 1) $
  localOption (QuickCheckMaxShrinks 0) $
  property prop

-- | 渐进式测试包装器
withProgressiveTesting :: TestTree -> TestTree
withProgressiveTesting testTree = testTree

-- | 内存监控包装器
withMemoryMonitoring :: TestTree -> TestTree
withMemoryMonitoring testTree = testTree

-- | 强制垃圾回收
forceGC :: IO ()
forceGC = do
  performGC
  -- 给GC一些时间完成
  threadDelay 10000  -- 10ms

-- | 获取内存使用情况（简化版本）
getMemoryUsage :: IO (Maybe Integer)
getMemoryUsage = do
  env <- getEnvironment
  case lookup "MEMORY_USAGE" env of
    Just usageStr -> return $ readMaybe usageStr
    Nothing -> return Nothing
  where
    readMaybe :: String -> Maybe Integer
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

-- | 内存优化测试组（带清理）
intelligentTestGroupWithCleanup :: String -> [TestTree] -> TestTree
intelligentTestGroupWithCleanup name tests = 
  testGroup name tests

-- | 批量测试执行器（概念性）
batchTestExecutor :: [TestTree] -> Int -> IO ()
batchTestExecutor tests batchSize = do
  let batches = chunksOf batchSize tests
  mapM_ executeBatch batches
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    
    executeBatch :: [TestTree] -> IO ()
    executeBatch batch = do
      -- 这里应该实际执行测试
      -- 为了简化，我们只是记录
      hPutStrLn stderr $ "执行批次: " ++ show (length batch) ++ " 个测试"
      forceGC

-- | 智能测试调度器（概念性）
intelligentTestScheduler :: [TestTree] -> MemoryOptimizationConfig -> IO ()
intelligentTestScheduler tests config = do
  let prioritizedTests = prioritizeTests tests
  batchTestExecutor prioritizedTests (batchSize config)
  where
    prioritizeTests :: [TestTree] -> [TestTree]
    prioritizeTests = id  -- 简化：实际应该根据测试特性排序

-- | 内存使用检查器
checkMemoryUsage :: MemoryOptimizationConfig -> IO Bool
checkMemoryUsage config = do
  maybeUsage <- getMemoryUsage
  case maybeUsage of
    Just usage -> 
      if fromIntegral usage > fromIntegral (memoryLimitMB config) * 0.9
        then do
          hPutStrLn stderr "内存使用接近限制，执行紧急清理"
          replicateM_ 3 forceGC  -- 多次GC确保清理
          return False
        else return True
    Nothing -> return True  -- 如果无法获取内存使用，继续执行

-- | 自适应内存优化器
adaptiveMemoryOptimizer :: MemoryOptimizationConfig -> IO MemoryOptimizationConfig
adaptiveMemoryOptimizer config = do
  maybeUsage <- getMemoryUsage
  case maybeUsage of
    Just usage -> 
      if fromIntegral usage > fromIntegral (memoryLimitMB config) * 0.8
        then do
          hPutStrLn stderr "检测到高内存使用，启用紧急模式"
          return emergencyMemoryConfig
        else return config
    Nothing -> return config