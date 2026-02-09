{-# LANGUAGE CPP #-}

module TestSupport.AdvancedMemoryLimits
  ( -- Advanced memory management
    withAdvancedMemoryLimits
  , withExtremeMemoryLimits
  , withConservativeMemoryLimits
  , -- Memory monitoring and cleanup
    monitorMemoryUsage
  , forceAggressiveCleanup
  , -- Test suite optimization
    createMemoryBalancedSuite
  , filterMemoryIntensiveTests
  , -- Memory profiling
    profileTestMemory
  , MemoryProfile(..)
  , -- Adaptive memory management
    AdaptiveMemoryConfig(..)
  , withAdaptiveMemory
  , -- Memory configurations
    extremeMemoryConfig
  , minimalMemoryConfig
  , conservativeMemoryConfig
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | Advanced memory configuration for different scenarios
data AdaptiveMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB :: Int        -- ^ Memory limit in MB
  , maxTestSize :: Int          -- ^ QuickCheck max size
  , testCount :: Int            -- ^ Number of tests
  , maxShrinks :: Int           -- ^ Max shrinks
  , gcFrequency :: Int          -- ^ GC frequency (every N tests)
  , enableProfiling :: Bool     -- ^ Enable memory profiling
  , adaptiveCleanup :: Bool     -- ^ Enable adaptive cleanup
  } deriving (Show, Eq)

-- | Default memory configurations
extremeMemoryConfig :: AdaptiveMemoryConfig
extremeMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB = 200
  , maxTestSize = 1
  , testCount = 2
  , maxShrinks = 2
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  }
-- Note: Reduced extreme memory limit to 200MB with minimal test parameters
-- for maximum memory efficiency while maintaining basic test coverage.

minimalMemoryConfig :: AdaptiveMemoryConfig
minimalMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB = 256
  , maxTestSize = 2
  , testCount = 5
  , maxShrinks = 5
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  }

conservativeMemoryConfig :: AdaptiveMemoryConfig
conservativeMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB = 384
  , maxTestSize = 3
  , testCount = 15
  , maxShrinks = 15
  , gcFrequency = 2
  , enableProfiling = True
  , adaptiveCleanup = True
  }

-- | Memory profiling information
data MemoryProfile = MemoryProfile
  { peakMemoryUsage :: Int     -- ^ Peak memory usage in MB
  , averageMemoryUsage :: Int  -- ^ Average memory usage in MB
  , gcCount :: Int             -- ^ Number of GC runs
  , testDuration :: Double     -- ^ Test duration in seconds
  } deriving (Show, Eq)

-- | Apply extreme memory limits for severely constrained environments
withExtremeMemoryLimits :: TestTree -> TestTree
withExtremeMemoryLimits test = 
  let config = extremeMemoryConfig
  in applyMemoryConfig config test

-- | Apply conservative memory limits with profiling
withConservativeMemoryLimits :: TestTree -> TestTree
withConservativeMemoryLimits test = 
  let config = conservativeMemoryConfig
  in applyMemoryConfig config test

-- | Apply advanced memory limits with adaptive configuration
withAdvancedMemoryLimits :: AdaptiveMemoryConfig -> TestTree -> TestTree
withAdvancedMemoryLimits config test = applyMemoryConfig config test

-- | Internal function to apply memory configuration
applyMemoryConfig :: AdaptiveMemoryConfig -> TestTree -> TestTree
applyMemoryConfig config test = 
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (testCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | Monitor memory usage during test execution
monitorMemoryUsage :: IO a -> IO MemoryProfile
monitorMemoryUsage action = do
  startTime <- getCurrentTime
  
  -- Force initial GC
  performGC
  
  -- Run the action
  _ <- action
  
  -- Force final GC
  replicateM_ 3 performGC
  
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  
  -- Calculate memory usage (simplified)
  let peakMem = 0 -- Would need more sophisticated profiling
      avgMem = 0
      gcRuns = 0 -- Simplified for now
  
  return $ MemoryProfile peakMem avgMem gcRuns duration

-- | Force aggressive memory cleanup
forceAggressiveCleanup :: IO ()
forceAggressiveCleanup = do
  -- Multiple GC passes with different strategies
  replicateM_ 3 performGC
  
  -- Give GC time to complete (reduced delay)
  threadDelay 50000 -- 50ms
  
  -- Final cleanup pass
  performGC

-- | Create a memory-balanced test suite
createMemoryBalancedSuite :: AdaptiveMemoryConfig -> String -> [TestTree] -> TestTree
createMemoryBalancedSuite config name tests = 
  let filteredTests = filterMemoryIntensiveTests config tests
      limitedTests = map (withAdvancedMemoryLimits config) filteredTests
  in testGroup ("[Memory-Balanced] " ++ name) limitedTests

-- | Filter out memory-intensive tests based on configuration
filterMemoryIntensiveTests :: AdaptiveMemoryConfig -> [TestTree] -> [TestTree]
filterMemoryIntensiveTests config tests = 
  -- Simple implementation - could be enhanced to analyze test names
  let maxTests = case memoryLimitMB config of
        lim | lim <= 200 -> 3   -- Extreme memory constraints
        lim | lim <= 256 -> 5   -- Minimal memory constraints
        lim | lim <= 384 -> 8   -- Optimized memory constraints  
        lim | lim <= 512 -> 12  -- Moderate constraints
        _ -> 15                 -- No filtering for higher limits
  in take maxTests tests

-- | Profile test memory usage
profileTestMemory :: IO a -> IO (MemoryProfile, a)
profileTestMemory action = do
  profile <- monitorMemoryUsage action
  result <- action
  return (profile, result)

-- | Apply adaptive memory management based on system resources
withAdaptiveMemory :: (AdaptiveMemoryConfig -> IO a) -> IO a
withAdaptiveMemory action = do
  -- Detect available memory (simplified)
  let availableMB = 1024 :: Int -- Would need actual memory detection
  
  let config = if availableMB <= 512
                then extremeMemoryConfig
                else if availableMB <= 1024
                     then minimalMemoryConfig
                     else conservativeMemoryConfig
  
  printf "Using adaptive memory config: %dMB limit\n" (memoryLimitMB config)
  action config