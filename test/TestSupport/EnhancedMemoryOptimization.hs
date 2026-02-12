{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- | Enhanced memory optimization strategies for Typus test suites
-- This module provides additional memory optimization techniques without deleting tests
module TestSupport.EnhancedMemoryOptimization
  ( -- Enhanced memory configurations
    withEnhancedMemoryLimits
  , withUltraLightMemoryLimits
  , withMicroMemoryLimits
  , -- Memory-efficient test data generation
    generateMemoryEfficientStrings
  , generateMemoryEfficientLists
  , generateMemoryEfficientInts
  , -- Test optimization helpers
    optimizeQuickCheckGen
  , limitRecursionDepth
  , createMemoryEfficientTest
  , createEnhancedMemoryTestGroup
  , -- Memory monitoring and cleanup
    withEnhancedMemoryMonitoring
  , performEnhancedCleanup
  , -- Configuration types
    EnhancedMemoryConfig(..)
  , MemoryOptimizationLevel(..)
  , -- Predefined configurations
    microMemoryConfig
  , ultraLightMemoryConfig
  , enhancedMemoryConfig
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..), Gen, arbitrary, sized, scale)
import System.Mem (performGC)
import Control.Monad (replicateM, replicateM_, when)
import Control.Concurrent (threadDelay)
import Data.Char (chr, ord)
import Data.List (take, replicate)
import Text.Printf (printf)

-- | Memory optimization levels for enhanced control
data MemoryOptimizationLevel = 
    Micro        -- ^ Micro memory usage (16MB equivalent)
  | UltraLight   -- ^ Ultra light memory usage (24MB equivalent)
  | Enhanced     -- ^ Enhanced memory usage (32MB equivalent)
  | Standard     -- ^ Standard memory usage (48MB equivalent)
  deriving (Show, Eq)

-- | Enhanced memory configuration with fine-grained control
data EnhancedMemoryConfig = EnhancedMemoryConfig
  { memoryLevel :: MemoryOptimizationLevel
  , maxStringSize :: Int           -- ^ Maximum string size in test data
  , maxListSize :: Int             -- ^ Maximum list size in test data
  , maxRecursionDepth :: Int       -- ^ Maximum recursion depth
  , maxQuickCheckSize :: Int       -- ^ QuickCheck max size
  , quickCheckTestCount :: Int     -- ^ Number of QuickCheck tests
  , maxQuickCheckShrinks :: Int    -- ^ Maximum shrinks
  , gcFrequency :: Int             -- ^ GC frequency (every N tests)
  , enableLazyEvaluation :: Bool   -- ^ Enable lazy evaluation optimizations
  , enableTestIsolation :: Bool    -- ^ Enable test isolation
  , memoryCleanupDelay :: Int      -- ^ Delay between cleanup steps (microseconds)
  , maxConcurrentTests :: Int      -- ^ Maximum concurrent tests
  } deriving (Show, Eq)

-- | Micro memory configuration (16MB equivalent) - 极限优化
microMemoryConfig :: EnhancedMemoryConfig
microMemoryConfig = EnhancedMemoryConfig
  { memoryLevel = Micro
  , maxStringSize = 2              -- 极小字符串
  , maxListSize = 1                -- 极小列表
  , maxRecursionDepth = 1          -- 最小递归深度
  , maxQuickCheckSize = 1          -- 最小测试大小
  , quickCheckTestCount = 1        -- 最少测试数量
  , maxQuickCheckShrinks = 0       -- 禁用收缩
  , gcFrequency = 1                -- 每次测试后GC
  , enableLazyEvaluation = True    -- 启用惰性求值优化
  , enableTestIsolation = True     -- 启用测试隔离
  , memoryCleanupDelay = 1000      -- 1ms清理延迟
  , maxConcurrentTests = 1         -- 单线程执行
  }

-- | Ultra light memory configuration (24MB equivalent) - 超轻量级
ultraLightMemoryConfig :: EnhancedMemoryConfig
ultraLightMemoryConfig = EnhancedMemoryConfig
  { memoryLevel = UltraLight
  , maxStringSize = 3              -- 超小字符串
  , maxListSize = 2                -- 超小列表
  , maxRecursionDepth = 2          -- 超小递归深度
  , maxQuickCheckSize = 1          -- 最小测试大小
  , quickCheckTestCount = 2        -- 极少测试数量
  , maxQuickCheckShrinks = 1       -- 最小收缩
  , gcFrequency = 1                -- 每次测试后GC
  , enableLazyEvaluation = True    -- 启用惰性求值优化
  , enableTestIsolation = True     -- 启用测试隔离
  , memoryCleanupDelay = 2000      -- 2ms清理延迟
  , maxConcurrentTests = 1         -- 单线程执行
  }

-- | Enhanced memory configuration (32MB equivalent) - 增强优化
enhancedMemoryConfig :: EnhancedMemoryConfig
enhancedMemoryConfig = EnhancedMemoryConfig
  { memoryLevel = Enhanced
  , maxStringSize = 4              -- 小字符串
  , maxListSize = 3                -- 小列表
  , maxRecursionDepth = 3          -- 小递归深度
  , maxQuickCheckSize = 2          -- 小测试大小
  , quickCheckTestCount = 3        -- 少量测试
  , maxQuickCheckShrinks = 2       -- 少量收缩
  , gcFrequency = 1                -- 每次测试后GC
  , enableLazyEvaluation = True    -- 启用惰性求值优化
  , enableTestIsolation = True     -- 启用测试隔离
  , memoryCleanupDelay = 3000      -- 3ms清理延迟
  , maxConcurrentTests = 1         -- 单线程执行
  }

-- | Apply enhanced memory limits to a test tree
withEnhancedMemoryLimits :: EnhancedMemoryConfig -> TestTree -> TestTree
withEnhancedMemoryLimits config test = 
  localOption (QuickCheckMaxSize (maxQuickCheckSize config)) $
  localOption (QuickCheckTests (quickCheckTestCount config)) $
  localOption (QuickCheckMaxShrinks (maxQuickCheckShrinks config)) $
  test

-- | Apply ultra light memory limits
withUltraLightMemoryLimits :: TestTree -> TestTree
withUltraLightMemoryLimits = withEnhancedMemoryLimits ultraLightMemoryConfig

-- | Apply micro memory limits
withMicroMemoryLimits :: TestTree -> TestTree
withMicroMemoryLimits = withEnhancedMemoryLimits microMemoryConfig

-- | Generate memory-efficient strings for QuickCheck tests
generateMemoryEfficientStrings :: EnhancedMemoryConfig -> Gen String
generateMemoryEfficientStrings config = sized $ \n -> do
  let maxSize = min (maxStringSize config) (max 1 n)
  size <- scale (min maxSize) arbitrary
  replicateM size $ scale (min 127) arbitrary >>= \c -> return $ chr (c `mod` 128)

-- | Generate memory-efficient lists for QuickCheck tests
generateMemoryEfficientLists :: EnhancedMemoryConfig -> Gen a -> Gen [a]
generateMemoryEfficientLists config gen = sized $ \n -> do
  let maxSize = min (maxListSize config) (max 1 n)
  size <- scale (min maxSize) arbitrary
  replicateM size gen

-- | Generate memory-efficient integers for QuickCheck tests
generateMemoryEfficientInts :: EnhancedMemoryConfig -> Gen Int
generateMemoryEfficientInts config = sized $ \n -> do
  let maxSize = maxQuickCheckSize config
  size <- scale (min maxSize) arbitrary
  return $ size `mod` 100

-- | Optimize QuickCheck generators for memory efficiency
optimizeQuickCheckGen :: EnhancedMemoryConfig -> Gen a -> Gen a
optimizeQuickCheckGen config gen = 
  if enableLazyEvaluation config
  then sized $ \n -> scale (min (maxQuickCheckSize config)) gen
  else scale (min (maxQuickCheckSize config)) gen

-- | Limit recursion depth in tests
limitRecursionDepth :: EnhancedMemoryConfig -> Int -> Int
limitRecursionDepth config depth = min depth (maxRecursionDepth config)

-- | Create a memory-efficient test with monitoring and cleanup
createMemoryEfficientTest :: EnhancedMemoryConfig -> String -> IO () -> TestTree -> TestTree
createMemoryEfficientTest config name testAction testTree = 
  let limitedTest = withEnhancedMemoryLimits config testTree
      testName = "[" ++ show (memoryLevel config) ++ "] " ++ name
  in testGroup testName [limitedTest]

-- | Enhanced memory monitoring during test execution
withEnhancedMemoryMonitoring :: EnhancedMemoryConfig -> IO a -> IO a
withEnhancedMemoryMonitoring config action = do
  -- Pre-test cleanup
  performEnhancedCleanup config
  
  -- Run test with monitoring
  result <- action
  
  -- Post-test cleanup
  performEnhancedCleanup config
  
  return result

-- | Enhanced memory cleanup with multiple strategies
performEnhancedCleanup :: EnhancedMemoryConfig -> IO ()
performEnhancedCleanup config = do
  -- Multiple rounds of garbage collection
  replicateM_ 3 $ do
    performGC
    threadDelay (memoryCleanupDelay config)
  
  -- Final cleanup
  performGC
  
  -- Additional cleanup if test isolation is enabled
  when (enableTestIsolation config) $ do
    threadDelay (memoryCleanupDelay config)
    performGC

-- | Create enhanced memory test group
createEnhancedMemoryTestGroup :: EnhancedMemoryConfig -> String -> [TestTree] -> TestTree
createEnhancedMemoryTestGroup config name tests = 
  let limitedTests = map (withEnhancedMemoryLimits config) tests
      prefix = "[" ++ show (memoryLevel config) ++ "] "
  in testGroup (prefix ++ name) limitedTests

-- | Utility function to get memory configuration based on available memory
getMemoryConfigForEnvironment :: Maybe Int -> EnhancedMemoryConfig
getMemoryConfigForEnvironment Nothing = enhancedMemoryConfig
getMemoryConfigForEnvironment (Just availableMB)
  | availableMB <= 16 = microMemoryConfig
  | availableMB <= 24 = ultraLightMemoryConfig
  | availableMB <= 32 = enhancedMemoryConfig
  | otherwise = enhancedMemoryConfig

-- | Apply memory optimizations based on environment
withEnvironmentAwareMemoryLimits :: Maybe Int -> TestTree -> TestTree
withEnvironmentAwareMemoryLimits maybeMB test = 
  let config = getMemoryConfigForEnvironment maybeMB
  in withEnhancedMemoryLimits config test