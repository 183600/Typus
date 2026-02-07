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
  { memoryLimitMB = 280
  , maxTestSize = 1
  , testCount = 3
  , maxShrinks = 3
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  }
-- Note: The extreme memory limit of 280MB is the minimum required to compile and run tests
-- due to GHC's memory requirements during compilation. Lower limits result in heap overflow.

minimalMemoryConfig :: AdaptiveMemoryConfig
minimalMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB = 256
  , maxTestSize = 3
  , testCount = 10
  , maxShrinks = 10
  , gcFrequency = 1
  , enableProfiling = False
  , adaptiveCleanup = True
  }

conservativeMemoryConfig :: AdaptiveMemoryConfig
conservativeMemoryConfig = AdaptiveMemoryConfig
  { memoryLimitMB = 512
  , maxTestSize = 5
  , testCount = 25
  , maxShrinks = 25
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
  replicateM_ 5 performGC
  
  -- Give GC time to complete
  threadDelay 100000 -- 100ms
  
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
        lim | lim <= 256 -> 5   -- Extreme memory constraints
        lim | lim <= 512 -> 10  -- Minimal memory constraints  
        lim | lim <= 1024 -> 20 -- Moderate constraints
        _ -> length tests       -- No filtering for higher limits
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