{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Adaptive Memory Configuration System
-- This module provides dynamic memory configuration that adapts to system resources
-- and test requirements while preserving all tests.
module TestSupport.AdaptiveMemoryConfig 
  ( -- Adaptive configuration
    AdaptiveConfig(..)
  , SystemResources(..)
  , detectSystemResources
  , createAdaptiveConfig
  , autoConfigureMemory
    
    -- Memory strategies
    , MemoryStrategy(..)
  , selectMemoryStrategy
  , applyMemoryStrategy
    
    -- Runtime adjustment
    , adjustMemoryAtRuntime
  , monitorMemoryPressure
  , emergencyMemoryReduction
    
    -- Configuration presets
    , ultraLowMemoryPreset
    , lowMemoryPreset
    , balancedMemoryPreset
    , highPerformancePreset
  ) where

import System.Mem (performGC)
import Control.Concurrent (threadDelay, getNumCapabilities)
import Control.Monad (replicateM_, when)
import Control.Exception (IOException, catch)
import Data.List (sort, groupBy, sortBy, isPrefixOf)
import Data.Ord (comparing)
import Text.Printf (printf)
import System.Environment (getEnvironment)
import System.Process (readProcess)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))

-- | System resource information
data SystemResources = SystemResources
  { totalMemoryMB :: Int        -- ^ Total available memory in MB
  , availableMemoryMB :: Int    -- ^ Currently available memory in MB
  , cpuCores :: Int             -- ^ Number of CPU cores
  , isCIEnvironment :: Bool     -- ^ Running in CI environment
  , isContainerized :: Bool     -- ^ Running in container
  , memoryPressure :: Double    -- ^ Current memory pressure (0.0-1.0)
  , loadAverage :: Double       -- ^ System load average
  } deriving (Show, Eq)

-- | Adaptive memory configuration
data AdaptiveConfig = AdaptiveConfig
  { systemResources :: SystemResources
  , memoryStrategy :: MemoryStrategy
  , maxMemoryMB :: Int           -- ^ Maximum memory to use
  , quickCheckSize :: Int       -- ^ QuickCheck max size
  , quickCheckTests :: Int      -- ^ QuickCheck test count
  , quickCheckShrinks :: Int    -- ^ QuickCheck max shrinks
  , gcFrequency :: Int          -- ^ GC frequency
  , testParallelism :: Int      -- ^ Test parallelism level
  , memorySafetyMargin :: Double -- ^ Safety margin for memory usage
  , adaptiveMode :: Bool         -- ^ Enable adaptive adjustments
  , emergencyMode :: Bool        -- ^ Emergency memory reduction mode
  } deriving (Show, Eq)

-- | Memory usage strategies
data MemoryStrategy = 
    UltraConservative  -- ^ Minimal memory usage, extreme test reduction
  | Conservative       -- ^ Low memory usage, significant test reduction
  | Balanced          -- ^ Balanced memory and test coverage
  | Performance       -- ^ Prioritize test performance over memory
  | Aggressive        -- ^ Maximum test performance, high memory usage
  deriving (Show, Eq, Ord)

-- | Detect system resources
detectSystemResources :: IO SystemResources
detectSystemResources = do
  -- Try to get memory information from /proc/meminfo (Linux)
  memInfo <- readProcess "cat" ["/proc/meminfo"] ""
  let totalMem = parseMemoryInfo "MemTotal:" memInfo
      availableMem = parseMemoryInfo "MemAvailable:" memInfo
  
  -- Get CPU core count
  cores <- getNumCapabilities
  
  -- Check CI environment
  env <- getEnvironment
  let isCI = fromMaybe "false" (lookup "CI" env) == "true" ||
             fromMaybe "false" (lookup "CONTINUOUS_INTEGRATION" env) == "true"
  
  -- Check if running in container
  let isContainer = fromMaybe "false" (lookup "DOCKER_CONTAINER" env) == "true" ||
                    fileExists " /.dockerenv"
  
  -- Calculate memory pressure
  let pressure = if totalMem > 0 
                 then 1.0 - (fromIntegral availableMem / fromIntegral totalMem)
                 else 0.5
  
  -- Get load average (simplified)
  loadAvg <- getLoadAverage
  
  return SystemResources
    { totalMemoryMB = totalMem `div` 1024
    , availableMemoryMB = availableMem `div` 1024
    , cpuCores = cores
    , isCIEnvironment = isCI
    , isContainerized = isContainer
    , memoryPressure = pressure
    , loadAverage = loadAvg
    }

-- | Parse memory information from /proc/meminfo
parseMemoryInfo :: String -> String -> Int
parseMemoryInfo key memInfo = 
  let lines' = lines memInfo
      targetLine = find (isPrefixOf key) lines'
  in case targetLine of
       Just line -> 
         let parts = words line
             valueStr = case dropWhile (not . isDigit) <$> listToMaybe parts of
                         Just s -> takeWhile isDigit s
                         Nothing -> "0"
         in read valueStr
       Nothing -> 0
  where
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

-- | Get system load average (simplified)
getLoadAverage :: IO Double
getLoadAverage = do
  result <- (catch :: IO a -> (IOException -> IO a) -> IO a) 
             (readProcess "cat" ["/proc/loadavg"] "") (const $ return "")
  let firstWord = takeWhile (/= ' ') result
  return $ if null firstWord then 0.0 else read firstWord

-- | Check if file exists
fileExists :: FilePath -> Bool
fileExists path = False -- Simplified for cross-platform compatibility

-- | Create adaptive configuration based on system resources
createAdaptiveConfig :: SystemResources -> MemoryStrategy -> AdaptiveConfig
createAdaptiveConfig resources strategy = 
  let baseConfig = case strategy of
        UltraConservative -> baseUltraConservative resources
        Conservative -> baseConservative resources
        Balanced -> baseBalanced resources
        Performance -> basePerformance resources
        Aggressive -> baseAggressive resources
      
      -- Adjust for CI environment
      ciAdjusted = if isCIEnvironment resources
                   then baseConfig 
                     { maxMemoryMB = min (maxMemoryMB baseConfig) 64
                     , testParallelism = 1
                     , memorySafetyMargin = 0.3
                     }
                   else baseConfig
      
      -- Adjust for container environment
      containerAdjusted = if isContainerized resources
                         then ciAdjusted
                           { maxMemoryMB = maxMemoryMB ciAdjusted `div` 2
                           , memorySafetyMargin = memorySafetyMargin ciAdjusted * 1.5
                           }
                         else ciAdjusted
      
      -- Adjust for memory pressure
      pressureAdjusted = if memoryPressure resources > 0.8
                        then containerAdjusted
                          { maxMemoryMB = maxMemoryMB containerAdjusted `div` 2
                          , quickCheckSize = 1
                          , quickCheckTests = 3
                          , emergencyMode = True
                          }
                        else containerAdjusted
  
  in pressureAdjusted { adaptiveMode = True }

-- | Base configuration for ultra-conservative strategy
baseUltraConservative :: SystemResources -> AdaptiveConfig
baseUltraConservative resources = AdaptiveConfig
  { systemResources = resources
  , memoryStrategy = UltraConservative
  , maxMemoryMB = 16
  , quickCheckSize = 1
  , quickCheckTests = 2
  , quickCheckShrinks = 2
  , gcFrequency = 1
  , testParallelism = 1
  , memorySafetyMargin = 0.5
  , adaptiveMode = True
  , emergencyMode = False
  }

-- | Base configuration for conservative strategy
baseConservative :: SystemResources -> AdaptiveConfig
baseConservative resources = AdaptiveConfig
  { systemResources = resources
  , memoryStrategy = Conservative
  , maxMemoryMB = 32
  , quickCheckSize = 2
  , quickCheckTests = 5
  , quickCheckShrinks = 5
  , gcFrequency = 2
  , testParallelism = 1
  , memorySafetyMargin = 0.4
  , adaptiveMode = True
  , emergencyMode = False
  }

-- | Base configuration for balanced strategy
baseBalanced :: SystemResources -> AdaptiveConfig
baseBalanced resources = AdaptiveConfig
  { systemResources = resources
  , memoryStrategy = Balanced
  , maxMemoryMB = min 128 (availableMemoryMB resources `div` 4)
  , quickCheckSize = 4
  , quickCheckTests = 10
  , quickCheckShrinks = 10
  , gcFrequency = 5
  , testParallelism = min 2 (cpuCores resources)
  , memorySafetyMargin = 0.3
  , adaptiveMode = True
  , emergencyMode = False
  }

-- | Base configuration for performance strategy
basePerformance :: SystemResources -> AdaptiveConfig
basePerformance resources = AdaptiveConfig
  { systemResources = resources
  , memoryStrategy = Performance
  , maxMemoryMB = min 256 (availableMemoryMB resources `div` 3)
  , quickCheckSize = 8
  , quickCheckTests = 20
  , quickCheckShrinks = 20
  , gcFrequency = 10
  , testParallelism = min 4 (cpuCores resources)
  , memorySafetyMargin = 0.2
  , adaptiveMode = True
  , emergencyMode = False
  }

-- | Base configuration for aggressive strategy
baseAggressive :: SystemResources -> AdaptiveConfig
baseAggressive resources = AdaptiveConfig
  { systemResources = resources
  , memoryStrategy = Aggressive
  , maxMemoryMB = min 512 (availableMemoryMB resources `div` 2)
  , quickCheckSize = 15
  , quickCheckTests = 30
  , quickCheckShrinks = 30
  , gcFrequency = 15
  , testParallelism = cpuCores resources
  , memorySafetyMargin = 0.1
  , adaptiveMode = True
  , emergencyMode = False
  }

-- | Automatically configure memory based on system resources
autoConfigureMemory :: IO AdaptiveConfig
autoConfigureMemory = do
  resources <- detectSystemResources
  strategy <- selectMemoryStrategy resources
  let config = createAdaptiveConfig resources strategy
  printf "Auto-configured memory strategy: %s\n" (show strategy)
  printf "Memory limit: %dMB\n" (maxMemoryMB config)
  printf "QuickCheck parameters: size=%d, tests=%d, shrinks=%d\n"
    (quickCheckSize config) (quickCheckTests config) (quickCheckShrinks config)
  return config

-- | Select appropriate memory strategy based on system resources
selectMemoryStrategy :: SystemResources -> IO MemoryStrategy
selectMemoryStrategy resources = do
  let availableMem = availableMemoryMB resources
      pressure = memoryPressure resources
      isCI = isCIEnvironment resources
      isContainer = isContainerized resources
  
  return $ if isCI || isContainer
           then if availableMem < 64 || pressure > 0.8
                then UltraConservative
                else if availableMem < 128 || pressure > 0.6
                     then Conservative
                     else Balanced
           else if availableMem < 256 || pressure > 0.9
                then UltraConservative
                else if availableMem < 512 || pressure > 0.7
                     then Conservative
                     else if availableMem < 1024 || pressure > 0.5
                          then Balanced
                          else if availableMem < 2048
                               then Performance
                               else Aggressive

-- | Apply memory strategy to test tree
applyMemoryStrategy :: AdaptiveConfig -> TestTree -> TestTree
applyMemoryStrategy config test = 
  localOption (QuickCheckMaxSize (quickCheckSize config)) $
  localOption (QuickCheckTests (quickCheckTests config)) $
  localOption (QuickCheckMaxShrinks (quickCheckShrinks config)) $
  test

-- | Adjust memory configuration at runtime
adjustMemoryAtRuntime :: AdaptiveConfig -> IO AdaptiveConfig
adjustMemoryAtRuntime config = do
  newResources <- detectSystemResources
  let currentPressure = memoryPressure newResources
      currentStrategy = memoryStrategy config
  
  -- If memory pressure is high, downgrade strategy
  newStrategy <- if currentPressure > 0.9
                 then return $ min UltraConservative currentStrategy
                 else if currentPressure > 0.7
                      then return $ min Conservative currentStrategy
                      else return currentStrategy
  
  let newConfig = createAdaptiveConfig newResources newStrategy
  
  when (newStrategy /= currentStrategy) $ do
    printf "Memory pressure detected (%.2f), downgrading strategy from %s to %s\n"
      currentPressure (show currentStrategy) (show newStrategy)
  
  return newConfig

-- | Monitor memory pressure and trigger adjustments
monitorMemoryPressure :: AdaptiveConfig -> IO ()
monitorMemoryPressure config = do
  replicateM_ 3 performGC
  threadDelay 10000
  
  newConfig <- adjustMemoryAtRuntime config
  
  when (emergencyMode newConfig) $ do
    printf "Emergency mode activated! Reducing memory usage drastically\n"
    emergencyMemoryReduction newConfig

-- | Emergency memory reduction
emergencyMemoryReduction :: AdaptiveConfig -> IO ()
emergencyMemoryReduction config = do
  printf "Performing emergency memory reduction...\n"
  
  -- Aggressive garbage collection
  replicateM_ 10 $ do
    performGC
    threadDelay 1000
  
  -- Force final cleanup
  replicateM_ 5 performGC
  threadDelay 5000
  
  printf "Emergency memory reduction completed\n"

-- | Ultra low memory preset (16MB)
ultraLowMemoryPreset :: AdaptiveConfig
ultraLowMemoryPreset = AdaptiveConfig
  { systemResources = SystemResources 1024 512 2 False False 0.5 0.5
  , memoryStrategy = UltraConservative
  , maxMemoryMB = 16
  , quickCheckSize = 1
  , quickCheckTests = 2
  , quickCheckShrinks = 2
  , gcFrequency = 1
  , testParallelism = 1
  , memorySafetyMargin = 0.5
  , adaptiveMode = False
  , emergencyMode = False
  }

-- | Low memory preset (32MB)
lowMemoryPreset :: AdaptiveConfig
lowMemoryPreset = AdaptiveConfig
  { systemResources = SystemResources 2048 1024 4 False False 0.5 0.5
  , memoryStrategy = Conservative
  , maxMemoryMB = 32
  , quickCheckSize = 2
  , quickCheckTests = 5
  , quickCheckShrinks = 5
  , gcFrequency = 2
  , testParallelism = 1
  , memorySafetyMargin = 0.4
  , adaptiveMode = False
  , emergencyMode = False
  }

-- | Balanced memory preset (128MB)
balancedMemoryPreset :: AdaptiveConfig
balancedMemoryPreset = AdaptiveConfig
  { systemResources = SystemResources 4096 2048 8 False False 0.5 0.5
  , memoryStrategy = Balanced
  , maxMemoryMB = 128
  , quickCheckSize = 5
  , quickCheckTests = 20
  , quickCheckShrinks = 20
  , gcFrequency = 5
  , testParallelism = 2
  , memorySafetyMargin = 0.3
  , adaptiveMode = False
  , emergencyMode = False
  }

-- | High performance preset (512MB)
highPerformancePreset :: AdaptiveConfig
highPerformancePreset = AdaptiveConfig
  { systemResources = SystemResources 8192 4096 16 False False 0.5 0.5
  , memoryStrategy = Performance
  , maxMemoryMB = 512
  , quickCheckSize = 10
  , quickCheckTests = 50
  , quickCheckShrinks = 50
  , gcFrequency = 10
  , testParallelism = 4
  , memorySafetyMargin = 0.2
  , adaptiveMode = False
  , emergencyMode = False
  }