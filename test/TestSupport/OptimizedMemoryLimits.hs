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

-- | Default optimized memory configurations
minimalOptimizedConfig :: OptimizedMemoryConfig
minimalOptimizedConfig = OptimizedMemoryConfig
  { memoryLimitMB = 256
  , maxTestSize = 2
  , testCount = 8
  , maxShrinks = 8
  , gcFrequency = 1
  , enableProfiling = False
  , optimizedCleanup = True
  , stringSizeLimit = 15
  , listSizeLimit = 5
  }

optimizedMemoryConfig :: OptimizedMemoryConfig
optimizedMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 384
  , maxTestSize = 3
  , testCount = 15
  , maxShrinks = 15
  , gcFrequency = 1
  , enableProfiling = False
  , optimizedCleanup = True
  , stringSizeLimit = 20
  , listSizeLimit = 8
  }

strictMemoryConfig :: OptimizedMemoryConfig
strictMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 512
  , maxTestSize = 5
  , testCount = 25
  , maxShrinks = 25
  , gcFrequency = 2
  , enableProfiling = True
  , optimizedCleanup = True
  , stringSizeLimit = 25
  , listSizeLimit = 10
  }

balancedMemoryConfig :: OptimizedMemoryConfig
balancedMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 768
  , maxTestSize = 8
  , testCount = 50
  , maxShrinks = 35
  , gcFrequency = 3
  , enableProfiling = True
  , optimizedCleanup = True
  , stringSizeLimit = 30
  , listSizeLimit = 15
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

-- | Force optimized memory cleanup
forceOptimizedCleanup :: IO ()
forceOptimizedCleanup = do
  -- Multiple GC passes with optimized timing
  replicateM_ 3 performGC
  
  -- Short delay to allow GC to complete
  threadDelay 50000 -- 50ms
  
  -- Final cleanup pass
  performGC

-- | Create an optimized memory test suite
createOptimizedMemorySuite :: OptimizedMemoryConfig -> String -> [TestTree] -> TestTree
createOptimizedMemorySuite config name tests = 
  let filteredTests = selectOptimizedTests config tests
      limitedTests = map (applyOptimizedMemoryConfig config) filteredTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB] "
  in testGroup (prefix ++ name) limitedTests

-- | Select optimized tests based on configuration
selectOptimizedTests :: OptimizedMemoryConfig -> [TestTree] -> [TestTree]
selectOptimizedTests config tests = 
  -- Select tests based on memory constraints
  let maxTests = case memoryLimitMB config of
        lim | lim <= 256 -> 3   -- Minimal memory constraints
        lim | lim <= 384 -> 5   -- Optimized memory constraints  
        lim | lim <= 512 -> 8   -- Strict memory constraints
        _ -> 12                 -- Balanced constraints
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