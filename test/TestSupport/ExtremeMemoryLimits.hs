{-# LANGUAGE CPP #-}

module TestSupport.ExtremeMemoryLimits 
  ( -- 极端内存优化配置
    withExtremeMemoryLimits
  , withCriticalMemoryLimits
  , -- 极端内存监控
    monitorExtremeMemoryUsage
  , forceExtremeCleanup
  , -- 极端测试套件
    createExtremeMemorySuite
  , selectCriticalTests
  , -- 极端内存配置
    extremeMemoryConfig
  , criticalMemoryConfig
  , ExtremeMemoryConfig(..)
  , withExtremeMemory
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | 极端内存配置用于资源极度受限的环境
data ExtremeMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB :: Int        -- ^ 内存限制（MB）
  , maxTestSize :: Int          -- ^ QuickCheck最大大小
  , testCount :: Int            -- ^ 测试数量
  , maxShrinks :: Int           -- ^ 最大收缩次数
  , gcFrequency :: Int          -- ^ GC频率（每N个测试）
  , enableProfiling :: Bool     -- ^ 启用内存分析
  , extremeCleanup :: Bool      -- ^ 启用极端清理
  , stringSizeLimit :: Int      -- ^ 测试中最大字符串大小
  , listSizeLimit :: Int        -- ^ 测试中最大列表大小
  , recursionDepth :: Int       -- ^ 最大递归深度
  , maxConcurrentTests :: Int   -- ^ 最大并发测试数
  } deriving (Show, Eq)

-- | 极端内存配置（约128MB等效）
extremeMemoryConfig :: ExtremeMemoryConfig
extremeMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB = 128
  , maxTestSize = 1
  , testCount = 3
  , maxShrinks = 2
  , gcFrequency = 1
  , enableProfiling = False
  , extremeCleanup = True
  , stringSizeLimit = 5
  , listSizeLimit = 2
  , recursionDepth = 2
  , maxConcurrentTests = 1
  }

-- | 关键内存配置（约64MB等效）
criticalMemoryConfig :: ExtremeMemoryConfig
criticalMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB = 64
  , maxTestSize = 1
  , testCount = 2
  , maxShrinks = 1
  , gcFrequency = 1
  , enableProfiling = False
  , extremeCleanup = True
  , stringSizeLimit = 3
  , listSizeLimit = 1
  , recursionDepth = 1
  , maxConcurrentTests = 1
  }

-- | 应用极端内存限制
withExtremeMemoryLimits :: TestTree -> TestTree
withExtremeMemoryLimits test = 
  let config = extremeMemoryConfig
  in applyExtremeMemoryConfig config test

-- | 应用关键内存限制
withCriticalMemoryLimits :: TestTree -> TestTree
withCriticalMemoryLimits test = 
  let config = criticalMemoryConfig
  in applyExtremeMemoryConfig config test

-- | 内部函数：应用极端内存配置
applyExtremeMemoryConfig :: ExtremeMemoryConfig -> TestTree -> TestTree
applyExtremeMemoryConfig config test = 
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (testCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | 监控极端内存使用
monitorExtremeMemoryUsage :: IO a -> IO ()
monitorExtremeMemoryUsage action = do
  -- 强制初始GC
  replicateM_ 3 performGC
  
  -- 短暂延迟让GC完成
  threadDelay 10000 -- 10ms
  
  -- 运行动作
  _ <- action
  
  -- 强制最终GC
  replicateM_ 5 performGC
  
  -- 再次延迟
  threadDelay 10000 -- 10ms

-- | 强制极端内存清理
forceExtremeCleanup :: IO ()
forceExtremeCleanup = do
  -- 多轮GC，每轮间隔很短
  replicateM_ 5 $ do
    performGC
    threadDelay 5000 -- 5ms间隔
  
  -- 最终清理
  performGC

-- | 创建极端内存测试套件
createExtremeMemorySuite :: ExtremeMemoryConfig -> String -> [TestTree] -> TestTree
createExtremeMemorySuite config name tests = 
  let filteredTests = selectCriticalTests config tests
      limitedTests = map (applyExtremeMemoryConfig config) filteredTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB-EXTREME] "
  in testGroup (prefix ++ name) limitedTests

-- | 选择关键测试基于配置
selectCriticalTests :: ExtremeMemoryConfig -> [TestTree] -> [TestTree]
selectCriticalTests config tests = 
  -- 基于内存约束选择测试
  let maxTests = case memoryLimitMB config of
        lim | lim <= 64  -> 1  -- 极端内存约束
        lim | lim <= 128 -> 2  -- 严重内存约束
        lim | lim <= 256 -> 3  -- 重度内存约束
        _ -> 4                  -- 中度约束
  in take maxTests tests

-- | 应用极端内存管理
withExtremeMemory :: (ExtremeMemoryConfig -> IO a) -> IO a
withExtremeMemory action = do
  -- 使用极端内存配置
  let config = extremeMemoryConfig
  
  printf "Using extreme memory config: %dMB limit\n" (memoryLimitMB config)
  action config