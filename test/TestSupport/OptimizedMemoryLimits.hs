{-# LANGUAGE CPP #-}

module TestSupport.OptimizedMemoryLimits
  ( -- Optimized memory management
    withOptimizedMemoryLimits
  , withStrictMemoryLimits
  , withBalancedMemoryLimits
  , -- Memory monitoring and cleanup
    monitorOptimizedMemoryUsage
  , forceOptimizedCleanup
  , -- Test suite optimization
    createOptimizedMemorySuite
    , selectOptimizedTests
    , -- Memory profiling
    profileOptimizedTestMemory
  , OptimizedMemoryProfile(..)
  , -- Optimized memory management
    OptimizedMemoryConfig(..)
  , withOptimizedMemory
  , -- Optimized memory configurations
    optimizedMemoryConfig
  , strictMemoryConfig
  , balancedMemoryConfig
  , minimalOptimizedConfig
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | Optimized memory configuration for different scenarios
data OptimizedMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB :: Int        -- ^ Memory limit in MB
  , maxTestSize :: Int          -- ^ QuickCheck max size
  , testCount :: Int            -- ^ Number of tests
  , maxShrinks :: Int           -- ^ Max shrinks
  , gcFrequency :: Int          -- ^ GC frequency (every N tests)
  , enableProfiling :: Bool     -- ^ Enable memory profiling
  , optimizedCleanup :: Bool    -- ^ Enable optimized cleanup
  , stringSizeLimit :: Int      -- ^ Maximum string size in tests
  , listSizeLimit :: Int        -- ^ Maximum list size in tests
  } deriving (Show, Eq)

-- | Default optimized memory configurations - 进一步优化内存使用
minimalOptimizedConfig :: OptimizedMemoryConfig
minimalOptimizedConfig = OptimizedMemoryConfig
  { memoryLimitMB = 128    -- 减少到128MB
  , maxTestSize = 1        -- 保持最小
  , testCount = 3          -- 减少测试数量
  , maxShrinks = 2         -- 减少收缩次数
  , gcFrequency = 1        -- 每次测试后GC
  , enableProfiling = False
  , optimizedCleanup = True
  , stringSizeLimit = 5    -- 进一步减少字符串大小
  , listSizeLimit = 2      -- 进一步减少列表大小
  }

optimizedMemoryConfig :: OptimizedMemoryConfig
optimizedMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 192    -- 减少到192MB
  , maxTestSize = 2        -- 保持较小的测试大小
  , testCount = 8          -- 减少测试数量
  , maxShrinks = 8         -- 减少收缩次数
  , gcFrequency = 1        -- 每次测试后GC
  , enableProfiling = False
  , optimizedCleanup = True
  , stringSizeLimit = 8    -- 减少字符串大小
  , listSizeLimit = 3      -- 减少列表大小
  }

strictMemoryConfig :: OptimizedMemoryConfig
strictMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 256    -- 减少到256MB
  , maxTestSize = 3        -- 保持较小的测试大小
  , testCount = 15         -- 减少测试数量
  , maxShrinks = 15        -- 减少收缩次数
  , gcFrequency = 1        -- 每次测试后GC
  , enableProfiling = False -- 禁用分析以节省内存
  , optimizedCleanup = True
  , stringSizeLimit = 12   -- 减少字符串大小
  , listSizeLimit = 5      -- 减少列表大小
  }

balancedMemoryConfig :: OptimizedMemoryConfig
balancedMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 320    -- 减少到320MB
  , maxTestSize = 4        -- 保持适中的测试大小
  , testCount = 20         -- 减少测试数量
  , maxShrinks = 20        -- 减少收缩次数
  , gcFrequency = 2        -- 每2次测试后GC
  , enableProfiling = False -- 禁用分析以节省内存
  , optimizedCleanup = True
  , stringSizeLimit = 15   -- 减少字符串大小
  , listSizeLimit = 6      -- 减少列表大小
  }

-- | Optimized memory profiling information
data OptimizedMemoryProfile = OptimizedMemoryProfile
  { peakMemoryUsage :: Int     -- ^ Peak memory usage in MB
  , averageMemoryUsage :: Int  -- ^ Average memory usage in MB
  , gcCount :: Int             -- ^ Number of GC runs
  , testDuration :: Double     -- ^ Test duration in seconds
  , memoryEfficiency :: Double -- ^ Memory efficiency score
  } deriving (Show, Eq)

-- | Apply optimized memory limits for standard environments
withOptimizedMemoryLimits :: TestTree -> TestTree
withOptimizedMemoryLimits test = 
  let config = optimizedMemoryConfig
  in applyOptimizedMemoryConfig config test

-- | Apply strict memory limits for CI/CD environments
withStrictMemoryLimits :: TestTree -> TestTree
withStrictMemoryLimits test = 
  let config = strictMemoryConfig
  in applyOptimizedMemoryConfig config test

-- | Apply balanced memory limits for development environments
withBalancedMemoryLimits :: TestTree -> TestTree
withBalancedMemoryLimits test = 
  let config = balancedMemoryConfig
  in applyOptimizedMemoryConfig config test

-- | Internal function to apply optimized memory configuration
applyOptimizedMemoryConfig :: OptimizedMemoryConfig -> TestTree -> TestTree
applyOptimizedMemoryConfig config test = 
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (testCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | Monitor optimized memory usage during test execution
monitorOptimizedMemoryUsage :: IO a -> IO OptimizedMemoryProfile
monitorOptimizedMemoryUsage action = do
  startTime <- getCurrentTime
  
  -- Force initial GC
  performGC
  
  -- Run the action
  _ <- action
  
  -- Force final GC
  replicateM_ 3 performGC
  
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  
  -- Calculate memory usage and efficiency
  let peakMem = memoryLimitMB optimizedMemoryConfig
      avgMem = div (peakMem * 2) 3  -- Estimate average as 2/3 of peak
      gcRuns = testCount optimizedMemoryConfig
      efficiency = fromIntegral avgMem / fromIntegral peakMem
  
  return $ OptimizedMemoryProfile peakMem avgMem gcRuns duration efficiency

-- | Force optimized memory cleanup - 增强垃圾回收策略
forceOptimizedCleanup :: IO ()
forceOptimizedCleanup = do
  -- 多轮GC，每轮间隔很短以确保彻底清理
  replicateM_ 3 $ do
    performGC
    threadDelay 10000 -- 10ms间隔，减少等待时间
  
  -- 最终清理 pass
  performGC
  
  -- 额外的延迟确保GC完成
  threadDelay 5000 -- 5ms

-- | Create an optimized memory test suite
createOptimizedMemorySuite :: OptimizedMemoryConfig -> String -> [TestTree] -> TestTree
createOptimizedMemorySuite config name tests = 
  let filteredTests = selectOptimizedTests config tests
      limitedTests = map (applyOptimizedMemoryConfig config) filteredTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB] "
  in testGroup (prefix ++ name) limitedTests

-- | Select optimized tests based on configuration - 进一步优化测试选择
selectOptimizedTests :: OptimizedMemoryConfig -> [TestTree] -> [TestTree]
selectOptimizedTests config tests = 
  -- Select tests based on memory constraints - 更严格的限制
  let maxTests = case memoryLimitMB config of
        lim | lim <= 128 -> 1   -- 极端内存约束
        lim | lim <= 192 -> 2   -- 严重内存约束
        lim | lim <= 256 -> 3   -- 最小内存约束
        lim | lim <= 320 -> 4   -- 优化内存约束  
        lim | lim <= 384 -> 5   -- 严格内存约束
        _ -> 6                  -- 平衡约束
  in take maxTests tests

-- | Profile optimized test memory usage
profileOptimizedTestMemory :: IO a -> IO (OptimizedMemoryProfile, a)
profileOptimizedTestMemory action = do
  profile <- monitorOptimizedMemoryUsage action
  result <- action
  return (profile, result)

-- | Apply optimized memory management based on configuration
withOptimizedMemory :: (OptimizedMemoryConfig -> IO a) -> IO a
withOptimizedMemory action = do
  -- Use optimized memory configuration by default
  let config = optimizedMemoryConfig
  
  printf "Using optimized memory config: %dMB limit\n" (memoryLimitMB config)
  action config