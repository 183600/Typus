{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Unified memory-optimized test runner
-- This module provides a comprehensive test runner that automatically
-- optimizes memory usage and selects tests based on available resources
module TestSupport.UnifiedMemoryOptimizedTestRunner 
  ( -- Main test runner
    runUnifiedMemoryOptimizedTests
  , runWithMemoryProfile
  
    -- Test configuration
  , UnifiedTestConfig(..)
  , defaultUnifiedConfig
  , createMemoryOptimizedConfig
  
    -- Test registration
  , TestRegistry
  , emptyRegistry
  , registerTest
  , registerTestGroup
  , registerCriticalTest
  , registerHighPriorityTest
  
    -- Memory monitoring
  , MemoryMonitor(..)
  , createMemoryMonitor
  , monitorTestExecution
  , generateMemoryReport
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import TestSupport.SmartTestSelection 
  ( TestInfo(..)
  , TestPriority(..)
  , MemoryTier(..)
  , createTestInfo
  , detectAvailableMemory
  , getMemoryTier
  , createAdaptiveConfig
  , createAdaptiveTestSuite
  , runMemoryAwareTests
  , applyAdaptiveLimits
  )
import TestSupport.ExtremeMemoryOptimization 
  ( smartMemoryCleanup
  , emergencyMemoryCleanup
  )
import System.Mem (performGC)
import System.Environment (getEnvironment)
import Data.Maybe (isJust)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Data.List (sortOn)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')

-- | Unified test configuration
data UnifiedTestConfig = UnifiedTestConfig
  { enableSmartSelection :: Bool      -- ^ Enable smart test selection
  , enableMemoryProfiling :: Bool     -- ^ Enable memory profiling
  , enableAggressiveOptimization :: Bool -- ^ Enable aggressive optimization
  , customMemoryLimit :: Maybe Int    -- ^ Custom memory limit in MB
  , maxTestExecutionTime :: Int       -- ^ Max test execution time in seconds
  , enableParallelExecution :: Bool   -- ^ Enable parallel test execution
  , gcStrategy :: GCStrategy          -- ^ Garbage collection strategy
  } deriving (Show, Eq)

-- | Garbage collection strategy
data GCStrategy 
  = Conservative    -- ^ Conservative GC
  | Aggressive      -- ^ Aggressive GC
  | Emergency       -- ^ Emergency GC
  | Adaptive        -- ^ Adaptive GC based on memory pressure
  deriving (Show, Eq)

-- | Test registry for organizing tests
data TestRegistry = TestRegistry
  { registeredTests :: [TestInfo]
  , testCategories :: Map.Map String [TestInfo]
  , testPriorities :: Map.Map TestPriority [TestInfo]
  }

-- | Memory monitor for tracking test execution
data MemoryMonitor = MemoryMonitor
  { memoryReadings :: MVar [Int]
  , testStartTimes :: MVar (Map.Map String UTCTime)
  , testMemoryUsage :: MVar (Map.Map String [Int])
  , currentMemoryUsage :: MVar Int
  }

-- | Default unified configuration
defaultUnifiedConfig :: UnifiedTestConfig
defaultUnifiedConfig = UnifiedTestConfig
  { enableSmartSelection = True
  , enableMemoryProfiling = True
  , enableAggressiveOptimization = False
  , customMemoryLimit = Nothing
  , maxTestExecutionTime = 300  -- 5 minutes
  , enableParallelExecution = False
  , gcStrategy = Adaptive
  }

-- | Create memory-optimized configuration
createMemoryOptimizedConfig :: Int -> UnifiedTestConfig
createMemoryOptimizedConfig memoryMB = defaultUnifiedConfig
  { customMemoryLimit = Just memoryMB
  , enableAggressiveOptimization = memoryMB <= 32
  , gcStrategy = if memoryMB <= 16 then Emergency else 
                if memoryMB <= 32 then Aggressive else Adaptive
  , enableParallelExecution = memoryMB >= 64
  }

-- | Create empty test registry
emptyRegistry :: TestRegistry
emptyRegistry = TestRegistry
  { registeredTests = []
  , testCategories = Map.empty
  , testPriorities = Map.empty
  }

-- | Register a test in the registry
registerTest :: String -> TestTree -> TestPriority -> Int -> String -> Bool -> TestRegistry -> TestRegistry
registerTest name tree priority memory category isQC registry = do
  let testInfo = createTestInfo name tree priority memory category isQC
      updatedTests = testInfo : registeredTests registry
      updatedCategories = Map.insertWith (++) category [testInfo] (testCategories registry)
      updatedPriorities = Map.insertWith (++) priority [testInfo] (testPriorities registry)
  registry
    { registeredTests = updatedTests
    , testCategories = updatedCategories
    , testPriorities = updatedPriorities
    }

-- | Register a test group
registerTestGroup :: String -> [(String, TestTree, TestPriority, Int)] -> TestRegistry -> TestRegistry
registerTestGroup groupName tests registry = do
  let updatedRegistry = foldl' (\reg (name, tree, priority, memory) -> 
        registerTest name tree priority memory groupName False reg) registry tests
  updatedRegistry

-- | Register a critical test
registerCriticalTest :: String -> TestTree -> Int -> String -> TestRegistry -> TestRegistry
registerCriticalTest name tree memory category =
  registerTest name tree PriorityCritical memory category False
-- | Register a high priority test
registerHighPriorityTest :: String -> TestTree -> Int -> String -> TestRegistry -> TestRegistry
registerHighPriorityTest name tree memory category = 
  registerTest name tree PriorityHigh memory category False

-- | Create memory monitor
createMemoryMonitor :: IO MemoryMonitor
createMemoryMonitor = do
  readings <- newMVar []
  startTimes <- newMVar Map.empty
  memoryUsage <- newMVar Map.empty
  currentUsage <- newMVar 0
  return MemoryMonitor
    { memoryReadings = readings
    , testStartTimes = startTimes
    , testMemoryUsage = memoryUsage
    , currentMemoryUsage = currentUsage
    }

-- | Monitor test execution memory usage
monitorTestExecution :: MemoryMonitor -> String -> IO a -> IO a
monitorTestExecution monitor testName action = do
  -- Record start time
  startTime <- getCurrentTime
  modifyMVar_ (testStartTimes monitor) (\times -> return (Map.insert testName startTime times))
  
  -- Force initial GC
  smartMemoryCleanup
  
  -- Run action with monitoring
  result <- action
  
  -- Force final GC
  smartMemoryCleanup
  
  -- Record end time
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  
  printf ("Test %s completed in %.2f seconds\n" :: String) testName (duration :: Double)
  
  return result

-- | Generate memory report
generateMemoryReport :: MemoryMonitor -> IO ()
generateMemoryReport monitor = do
  readings <- readMVar (memoryReadings monitor)
  usage <- readMVar (currentMemoryUsage monitor)
  
  printf "\n=== Memory Usage Report ===\n"
  printf "Current memory usage: %d KB\n" usage
  if not (null readings)
    then do
      let peakUsage = maximum readings
          avgUsage = sum readings `div` length readings
      printf "Peak memory usage: %d KB\n" peakUsage
      printf "Average memory usage: %d KB\n" avgUsage
      printf "Memory samples: %d\n" (length readings)
    else printf "No memory readings available\n"

-- | Run unified memory-optimized tests
runUnifiedMemoryOptimizedTests :: UnifiedTestConfig -> TestRegistry -> IO ()
runUnifiedMemoryOptimizedTests config registry = do
  printf "=== Unified Memory-Optimized Test Runner ===\n"
  
  -- Detect available memory
  availableMemory <- case customMemoryLimit config of
    Just limit -> return limit
    Nothing -> detectAvailableMemory
  
  printf "Available memory: %d MB\n" availableMemory
  printf "Memory tier: %s\n" (show (getMemoryTier availableMemory))
  
  -- Apply garbage collection strategy
  applyGCStrategy (gcStrategy config)
  
  -- Create test suite based on configuration
  testSuite <- createOptimizedTestSuite config availableMemory registry
  
  -- Run tests with memory monitoring if enabled
  if enableMemoryProfiling config
    then do
      monitor <- createMemoryMonitor
      monitorTestExecution monitor "Unified Test Suite" (defaultMain testSuite)
      generateMemoryReport monitor
    else defaultMain testSuite

-- | Apply garbage collection strategy
applyGCStrategy :: GCStrategy -> IO ()
applyGCStrategy strategy = case strategy of
  Conservative -> do
    printf "Using conservative garbage collection\n"
    replicateM_ 2 performGC
  Aggressive -> do
    printf "Using aggressive garbage collection\n"
    smartMemoryCleanup
  Emergency -> do
    printf "Using emergency garbage collection\n"
    emergencyMemoryCleanup
  Adaptive -> do
    printf "Using adaptive garbage collection\n"
    replicateM_ 3 performGC

-- | Create optimized test suite
createOptimizedTestSuite :: UnifiedTestConfig -> Int -> TestRegistry -> IO TestTree
createOptimizedTestSuite config availableMemory registry = do
  let tests = registeredTests registry
  
  if enableSmartSelection config
    then do
      printf "Using smart test selection\n"
      let adaptiveConfig = createAdaptiveConfig (getMemoryTier availableMemory)
      return $ createAdaptiveTestSuite adaptiveConfig "Unified Optimized Tests" tests
    else do
      printf "Using basic test selection\n"
      let tier = getMemoryTier availableMemory
          maxTests = case tier of
            UltraCritical -> 2
            Critical -> 3
            Low -> 5
            Moderate -> 8
            Normal -> 15
          selectedTests = take maxTests tests
          adaptiveConfig = createAdaptiveConfig tier
          limitedTests = map (\ti -> applyAdaptiveLimits adaptiveConfig (testTree ti)) selectedTests
      return $ testGroup ("Unified Tests (" ++ show (length selectedTests) ++ "/" ++ show (length tests) ++ ")") limitedTests

-- | Run tests with memory profiling
runWithMemoryProfile :: TestRegistry -> IO ()
runWithMemoryProfile registry = do
  let config = defaultUnifiedConfig
      profileConfig = config { enableMemoryProfiling = True, enableAggressiveOptimization = True }
  runUnifiedMemoryOptimizedTests profileConfig registry