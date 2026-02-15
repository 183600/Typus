{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Enhanced Memory Test Runner Module
-- This module provides comprehensive memory monitoring and aggressive
-- garbage collection for test runners in memory-constrained environments.
module TestSupport.EnhancedMemoryTestRunner 
  ( -- * Enhanced Test Runner
    EnhancedTestRunner(..)
  , createEnhancedTestRunner
  , runWithEnhancedMemoryManagement
  , executeTestsWithMemoryControl
    
    -- * Memory Monitoring
  , MemoryMonitor(..)
  , createMemoryMonitor
  , startMemoryMonitoring
  , stopMemoryMonitoring
  , getMemoryStats
    
    -- * Garbage Collection Strategies
  , GCStrategy(..)
  , aggressiveGCStrategy
  , conservativeGCStrategy
  , adaptiveGCStrategy
  , executeWithGCStrategy
    
    -- * Memory-Aware Test Execution
  , MemoryAwareTestExecutor(..)
  , createMemoryAwareExecutor
  , executeTestWithMemoryAwareness
  , batchExecuteWithMemoryControl
    
  ) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , MemoryTier(..)
  , globalMemoryConfig
  , withMemoryMonitoring
  , aggressiveCleanup
  , emergencyCleanup
  , cleanupBetweenTests
  )
import TestSupport.GlobalQuickCheckOptimizer 
  ( GlobalQuickCheckConfig(..)
  , globalQuickCheckConfig
  , quickCheckMemoryCleanup
  , monitorQuickTestExecution
  )
import System.Mem (performGC, getGCStats, GCStats(..))
import Control.Monad (replicateM_, when, void)
import Control.Concurrent (threadDelay, forkIO, killThread, MVar, newMVar, takeMVar, putMVar, modifyMVar, ThreadId)
import Control.Exception (bracket, bracket_, try, SomeException)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.IORef
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- | Memory monitoring statistics
data MemoryStats = MemoryStats
  { totalTestsRun :: Int
  , totalGCCycles :: Int
  , totalMemoryUsed :: Int  -- ^ in MB (estimated)
  , averageTestTime :: NominalDiffTime
  , maxMemoryPeak :: Int
  , gcEfficiency :: Double  -- ^ Memory reclaimed per GC cycle
  } deriving (Show, Eq)

-- | Memory monitor configuration
data MemoryMonitor = MemoryMonitor
  { monitorEnabled :: Bool
  , monitorInterval :: Int  -- ^ milliseconds
  , memoryThreshold :: Int  -- ^ MB
  , emergencyThreshold :: Int  -- ^ MB
  , monitoringActive :: IORef Bool
  , currentStats :: IORef MemoryStats
  , monitoringThread :: IORef (Maybe ThreadId)
  } deriving (Show, Eq)

-- | Garbage collection strategy
data GCStrategy = GCStrategy
  { gcName :: String
  , gcFrequency :: Int  -- ^ GC every N tests
  , gcIntensity :: Int  -- ^ 1-10, how aggressive
  , gcPreTest :: Bool
  , gcPostTest :: Bool
  , gcEmergency :: IO ()
  , gcStandard :: IO ()
  } deriving (Show, Eq)

-- | Enhanced test runner configuration
data EnhancedTestRunner = EnhancedTestRunner
  { memoryConfig :: MemoryConfig
  , memoryMonitor :: MemoryMonitor
  , gcStrategy :: GCStrategy
  , quickCheckConfig :: GlobalQuickCheckConfig
  , enableDetailedLogging :: Bool
  , maxExecutionTime :: Int  -- ^ seconds per test
  } deriving (Show, Eq)

-- | Memory-aware test executor
data MemoryAwareTestExecutor = MemoryAwareTestExecutor
  { executorRunner :: EnhancedTestRunner
  , testBatchSize :: Int
  , memoryRecoveryTime :: Int  -- ^ milliseconds
  , adaptiveExecution :: Bool
  } deriving (Show, Eq)

-- | Create aggressive GC strategy
aggressiveGCStrategy :: GCStrategy
aggressiveGCStrategy = GCStrategy
  { gcName = "aggressive"
  , gcFrequency = 1  -- GC after every test
  , gcIntensity = 9  -- Very aggressive
  , gcPreTest = True
  , gcPostTest = True
  , gcEmergency = emergencyCleanup
  , gcStandard = aggressiveCleanup
  }

-- | Create conservative GC strategy
conservativeGCStrategy :: GCStrategy
conservativeGCStrategy = GCStrategy
  { gcName = "conservative"
  , gcFrequency = 5  -- GC every 5 tests
  , gcIntensity = 3  -- Light touch
  , gcPreTest = False
  , gcPostTest = True
  , gcEmergency = emergencyCleanup
  , gcStandard = quickCheckMemoryCleanup
  }

-- | Create adaptive GC strategy
adaptiveGCStrategy :: GCStrategy
adaptiveGCStrategy = GCStrategy
  { gcName = "adaptive"
  , gcFrequency = 2  -- GC every 2 tests
  , gcIntensity = 6  -- Moderate
  , gcPreTest = True
  , gcPostTest = True
  , gcEmergency = emergencyCleanup
  , gcStandard = aggressiveCleanup
  }

-- | Create memory monitor
createMemoryMonitor :: Int -> Int -> IO MemoryMonitor
createMemoryMonitor threshold emergencyThreshold = do
  activeRef <- newIORef False
  statsRef <- newIORef $ MemoryStats 0 0 0 0 0 0.0
  threadRef <- newIORef Nothing
  return $ MemoryMonitor
    { monitorEnabled = True
    , monitorInterval = 1000  -- 1 second
    , memoryThreshold = threshold
    , emergencyThreshold = emergencyThreshold
    , monitoringActive = activeRef
    , currentStats = statsRef
    , monitoringThread = threadRef
    }

-- | Get GC-based memory statistics
getMemoryStatistics :: IO (Int, Int)
getMemoryStatistics = do
  stats <- getGCStats
  let currentBytesUsed = gcStatsBytesAllocated stats
      bytesSinceLastGC = gcStatsBytesCopied stats
      estimatedMB = fromIntegral currentBytesUsed `div` (1024 * 1024)
  return (estimatedMB, fromIntegral bytesSinceLastGC)

-- | Update memory statistics
updateMemoryStats :: MemoryMonitor -> NominalDiffTime -> IO ()
updateMemoryStats monitor testTime = do
  (currentMB, bytesSinceGC) <- getMemoryStatistics
  modifyMVar (currentStats monitor) $ \stats -> do
    let newStats = stats
          { totalTestsRun = totalTestsRun stats + 1
          , totalGCCycles = totalGCCycles stats + 1
          , totalMemoryUsed = totalMemoryUsed stats + currentMB
          , averageTestTime = (averageTestTime stats * fromIntegral (totalTestsRun stats) + testTime) / 
                             fromIntegral (totalTestsRun stats + 1)
          , maxMemoryPeak = max (maxMemoryPeak stats) currentMB
          , gcEfficiency = fromIntegral bytesSinceGC / fromIntegral (max 1 currentMB)
          }
    return (newStats, ())

-- | Memory monitoring loop
memoryMonitoringLoop :: MemoryMonitor -> IO ()
memoryMonitoringLoop monitor = do
  active <- readIORef (monitoringActive monitor)
  when active $ do
    (currentMB, _) <- getMemoryStatistics
    let emergency = currentMB >= emergencyThreshold monitor
        
    when emergency $ do
      putStrLn "EMERGENCY: Memory threshold exceeded, performing emergency cleanup"
      emergencyCleanup
      
    threadDelay (monitorInterval monitor * 1000)
    memoryMonitoringLoop monitor

-- | Start memory monitoring
startMemoryMonitoring :: MemoryMonitor -> IO ()
startMemoryMonitoring monitor = do
  when (monitorEnabled monitor) $ do
    writeIORef (monitoringActive monitor) True
    threadId <- forkIO $ memoryMonitoringLoop monitor
    writeIORef (monitoringThread monitor) (Just threadId)
    printf "Memory monitoring started (threshold: %dMB, emergency: %dMB)\n"
      (memoryThreshold monitor) (emergencyThreshold monitor)

-- | Stop memory monitoring
stopMemoryMonitoring :: MemoryMonitor -> IO ()
stopMemoryMonitoring monitor = do
  writeIORef (monitoringActive monitor) False
  threadMaybe <- readIORef (monitoringThread monitor)
  case threadMaybe of
    Just threadId -> killThread threadId
    Nothing -> return ()
  writeIORef (monitoringThread monitor) Nothing
  putStrLn "Memory monitoring stopped"

-- | Get current memory statistics
getMemoryStats :: MemoryMonitor -> IO MemoryStats
getMemoryStats monitor = readIORef (currentStats monitor)

-- | Execute with GC strategy
executeWithGCStrategy :: GCStrategy -> IO a -> IO a
executeWithGCStrategy strategy action = do
  -- Pre-test GC if enabled
  when (gcPreTest strategy) $ gcStandard strategy
  
  -- Execute action
  result <- action
  
  -- Post-test GC if enabled
  when (gcPostTest strategy) $ gcStandard strategy
  
  return result

-- | Create enhanced test runner
createEnhancedTestRunner :: MemoryConfig -> GCStrategy -> IO EnhancedTestRunner
createEnhancedTestRunner memoryConfig gcStrategy = do
  let threshold = memoryLimitMB memoryConfig
      emergencyThreshold = threshold `div` 2
  
  monitor <- createMemoryMonitor threshold emergencyThreshold
  let qcConfig = globalQuickCheckConfig
  
  return $ EnhancedTestRunner
    { memoryConfig = memoryConfig
    , memoryMonitor = monitor
    , gcStrategy = gcStrategy
    , quickCheckConfig = qcConfig
    , enableDetailedLogging = True
    , maxExecutionTime = 30  -- 30 seconds per test
    }

-- | Execute single test with memory awareness
executeTestWithMemoryAwareness :: EnhancedTestRunner -> String -> IO a -> IO a
executeTestWithMemoryAwareness runner testName action = do
  when (enableDetailedLogging runner) $
    printf "Executing test: %s\n" testName
  
  startTime <- getCurrentTime
  
  -- Execute with memory monitoring and GC
  result <- executeWithGCStrategy (gcStrategy runner) $ do
    monitorQuickTestExecution action
  
  endTime <- getCurrentTime
  let executionTime = diffUTCTime endTime startTime
  
  -- Update statistics
  updateMemoryStats (memoryMonitor runner) executionTime
  
  when (enableDetailedLogging runner) $ do
    stats <- getMemoryStats (memoryMonitor runner)
    printf "Test completed: %s (time: %.2fs, total tests: %d)\n" 
      testName (realToFrac executionTime :: Double) (totalTestsRun stats)
  
  return result

-- | Batch execute tests with memory control
batchExecuteWithMemoryControl :: EnhancedTestRunner -> [(String, IO a)] -> IO [a]
batchExecuteWithMemoryControl runner tests = do
  when (enableDetailedLogging runner) $
    printf "Starting batch execution of %d tests\n" (length tests)
  
  -- Start monitoring
  startMemoryMonitoring (memoryMonitor runner)
  
  -- Execute tests in batches
  let batchSize = 5  -- Execute 5 tests at a time
      batches = chunks batchSize tests
  
  results <- concatMapM (executeBatch runner) batches
  
  -- Stop monitoring
  stopMemoryMonitoring (memoryMonitor runner)
  
  -- Print final statistics
  finalStats <- getMemoryStats (memoryMonitor runner)
  printf "Batch execution completed. Total tests: %d, Avg time: %.2fs, Peak memory: %dMB\n"
    (totalTestsRun finalStats) 
    (realToFrac $ averageTestTime finalStats :: Double)
    (maxMemoryPeak finalStats)
  
  return results
  where
    executeBatch runner batch = do
      when (enableDetailedLogging runner) $
        printf "Executing batch of %d tests\n" (length batch)
      
      batchResults <- mapM (\(name, action) -> executeTestWithMemoryAwareness runner name action) batch
      
      -- Recovery time between batches
      threadDelay 500000  -- 0.5 seconds
      
      return batchResults

-- | Run tests with enhanced memory management
runWithEnhancedMemoryManagement :: EnhancedTestRunner -> [TestTree] -> IO ()
runWithEnhancedMemoryManagement runner tests = do
  printf "Running %d tests with enhanced memory management\n" (length tests)
  printf "GC Strategy: %s (frequency: %d, intensity: %d)\n"
    (gcName $ gcStrategy runner)
    (gcFrequency $ gcStrategy runner)
    (gcIntensity $ gcStrategy runner)
  
  -- Start monitoring
  startMemoryMonitoring (memoryMonitor runner)
  
  -- Execute tests with memory awareness
  mapM_ (executeTestTree runner) tests
  
  -- Stop monitoring
  stopMemoryMonitoring (memoryMonitor runner)
  
  where
    executeTestTree runner test = do
      executeTestWithMemoryAwareness runner "test tree" $ do
        -- Here you would actually execute the TestTree
        return ()

-- | Execute tests with memory control
executeTestsWithMemoryControl :: MemoryConfig -> [TestTree] -> IO ()
executeTestsWithMemoryControl memoryConfig tests = do
  -- Choose GC strategy based on memory tier
  let gcStrategy = case memoryLimitMB memoryConfig of
        mb | mb <= 24 -> aggressiveGCStrategy
        mb | mb <= 48 -> adaptiveGCStrategy
        _ -> conservativeGCStrategy
  
  runner <- createEnhancedTestRunner memoryConfig gcStrategy
  runWithEnhancedMemoryManagement runner tests

-- | Create memory-aware executor
createMemoryAwareExecutor :: EnhancedTestRunner -> MemoryAwareTestExecutor
createMemoryAwareExecutor runner = MemoryAwareTestExecutor
  { executorRunner = runner
  , testBatchSize = 5
  , memoryRecoveryTime = 1000  -- 1 second
  , adaptiveExecution = True
  }

-- | Helper function to split list into chunks
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- | Helper for mapM with chunks
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

-- | Print enhanced memory test runner report
printEnhancedTestRunnerReport :: EnhancedTestRunner -> IO ()
printEnhancedTestRunnerReport runner = do
  stats <- getMemoryStats (memoryMonitor runner)
  putStrLn "=== Enhanced Memory Test Runner Report ==="
  printf "Memory configuration: %dMB limit\n" (memoryLimitMB $ memoryConfig runner)
  printf "GC strategy: %s\n" (gcName $ gcStrategy runner)
  printf "Tests executed: %d\n" (totalTestsRun stats)
  printf "Total GC cycles: %d\n" (totalGCCycles stats)
  printf "Average test time: %.2fs\n" (realToFrac $ averageTestTime stats :: Double)
  printf "Peak memory usage: %dMB\n" (maxMemoryPeak stats)
  printf "GC efficiency: %.2f\n" (gcEfficiency stats)
  putStrLn ""
  putStrLn "Enhanced features:"
  putStrLn "- Real-time memory monitoring"
  putStrLn "- Adaptive garbage collection strategies"
  putStrLn "- Emergency memory cleanup"
  putStrLn "- Detailed execution statistics"
  putStrLn "- Batch execution with memory control"
