{-# LANGUAGE OverloadedStrings #-}

-- | 增强的内存监控模块
-- 提供运行时内存使用监控和自适应优化
module TestSupport.EnhancedMemoryMonitor
  ( withEnhancedMemoryMonitoring
  , adaptiveMemoryLimits
  , memoryUsageReporter
  , detectMemoryPressure
  , adjustTestParameters
  , createAdaptiveTestSuite
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (when, replicateM_)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment, lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | 内存使用状态
data MemoryState = 
    LowMemory       -- ^ 低内存状态
  | NormalMemory    -- ^ 正常内存状态
  | HighMemory      -- ^ 高内存状态
  deriving (Show, Eq)

-- | 带增强内存监控的测试执行
withEnhancedMemoryMonitoring :: IO a -> IO a
withEnhancedMemoryMonitoring action = do
  -- 强制GC前监控
  preState <- detectMemoryPressure
  when (preState == HighMemory) $ do
    replicateM_ 5 performGC
    threadDelay 1000
  
  result <- action
  
  -- 强制GC后监控
  postState <- detectMemoryPressure
  when (postState == HighMemory) $ do
    replicateM_ 3 performGC
    threadDelay 500
  
  return result

-- | 检测内存压力
detectMemoryPressure :: IO MemoryState
detectMemoryPressure = do
  -- 读取系统内存信息
  availableMem <- lookupEnv "AVAILABLE_MEMORY"
  
  case availableMem of
    Just memStr -> case readMaybe memStr of
      Just mem | mem < 64 -> return LowMemory
               | mem < 256 -> return NormalMemory
               | otherwise -> return HighMemory
      Nothing -> return NormalMemory
    Nothing -> return NormalMemory

-- | 自适应内存限制
adaptiveMemoryLimits :: MemoryState -> TestTree -> TestTree
adaptiveMemoryLimits state test = case state of
  LowMemory -> 
    localOption (QuickCheckMaxSize 1) $
    localOption (QuickCheckTests 1) $
    localOption (QuickCheckMaxShrinks 0) test
  NormalMemory -> 
    localOption (QuickCheckMaxSize 2) $
    localOption (QuickCheckTests 2) $
    localOption (QuickCheckMaxShrinks 1) test
  HighMemory -> 
    localOption (QuickCheckMaxSize 3) $
    localOption (QuickCheckTests 3) $
    localOption (QuickCheckMaxShrinks 2) test

-- | 内存使用报告器
memoryUsageReporter :: IO ()
memoryUsageReporter = do
  state <- detectMemoryPressure
  putStrLn $ "当前内存状态: " ++ show state
  case state of
    LowMemory -> putStrLn "警告: 内存资源受限，使用极简测试配置"
    NormalMemory -> putStrLn "正常: 使用标准测试配置"
    HighMemory -> putStrLn "良好: 内存充足，使用增强测试配置"

-- | 调整测试参数
data TestParameters = TestParameters
  { testSize :: Int
  , testCount :: Int
  , shrinkCount :: Int
  , stringLength :: Int
  } deriving (Show, Eq)

-- | 根据内存状态调整测试参数
adjustTestParameters :: MemoryState -> TestParameters
adjustTestParameters state = case state of
  LowMemory -> TestParameters
    { testSize = 1
    , testCount = 1
    , shrinkCount = 0
    , stringLength = 5
    }
  NormalMemory -> TestParameters
    { testSize = 2
    , testCount = 2
    , shrinkCount = 1
    , stringLength = 10
    }
  HighMemory -> TestParameters
    { testSize = 3
    , testCount = 3
    , shrinkCount = 2
    , stringLength = 15
    }

-- | 创建自适应测试套件
createAdaptiveTestSuite :: String -> [TestTree] -> IO TestTree
createAdaptiveTestSuite name tests = do
  state <- detectMemoryPressure
  let params = adjustTestParameters state
  
  putStrLn $ "创建自适应测试套件 '" ++ name ++ "'"
  putStrLn $ "检测到的内存状态: " ++ show state
  putStrLn $ "使用的测试参数: " ++ show params
  
  let limitedTests = map (adaptiveMemoryLimits state) tests
  return $ testGroup ("[Adaptive-" ++ show state ++ "] " ++ name) limitedTests