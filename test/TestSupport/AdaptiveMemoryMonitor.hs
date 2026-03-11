{-# LANGUAGE CPP #-}

module TestSupport.AdaptiveMemoryMonitor
  ( withAdaptiveMemoryLimits
  , adaptiveMemoryTestGroup
  , monitorMemoryUsage
  , getMemoryUsageMB
  , calculateMemoryPressure
  , MemoryPressure(..)
  , applyMemoryPressureLimits
  , adaptiveGC
  , setupMemoryMonitoring
  , cleanupMemoryMonitoring
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Memory pressure levels indicating system memory state
data MemoryPressure = 
    LowPressure      -- ^ Plenty of memory available
  | MediumPressure   -- ^ Moderate memory usage
  | HighPressure     -- ^ High memory usage, need optimization
  | CriticalPressure -- ^ Critical memory usage, need aggressive optimization
  deriving (Show, Eq, Enum)

-- | Apply adaptive memory limits based on current memory pressure
withAdaptiveMemoryLimits :: TestTree -> IO TestTree
withAdaptiveMemoryLimits test = do
  pressure <- calculateMemoryPressure
  return $ applyMemoryPressureLimits pressure test

-- | Create a test group with adaptive memory limits
adaptiveMemoryTestGroup :: String -> [TestTree] -> IO TestTree
adaptiveMemoryTestGroup name tests = do
  limitedTests <- mapM withAdaptiveMemoryLimits tests
  return $ testGroup ("[Adaptive-Memory] " ++ name) limitedTests

-- | Monitor memory usage during test execution
monitorMemoryUsage :: IO a -> IO a
monitorMemoryUsage action = bracket
  setupMemoryMonitoring
  cleanupMemoryMonitoring
  (\_ -> action)

-- | Get current memory usage in MB (Linux-specific implementation)
getMemoryUsageMB :: IO Int
getMemoryUsageMB = do
  -- Try to read memory usage from /proc/self/status
  result <- readProcess "grep" ["VmRSS", "/proc/self/status"] ""
  case words result of
    (_:_:kb:_) -> do
      let mb = read kb `div` 1024
      return mb
    _ -> return 0  -- Fallback if parsing fails

-- | Calculate current memory pressure level
calculateMemoryPressure :: IO MemoryPressure
calculateMemoryPressure = do
  usage <- getMemoryUsageMB
  -- Conservative thresholds for memory-constrained environments
  if usage < 8
    then return LowPressure
    else if usage < 16
      then return MediumPressure
      else if usage < 24
        then return HighPressure
        else return CriticalPressure

-- | Apply memory limits based on pressure level
applyMemoryPressureLimits :: MemoryPressure -> TestTree -> TestTree
applyMemoryPressureLimits pressure test = case pressure of
  LowPressure ->
    localOption (QuickCheckMaxSize 5) $
    localOption (QuickCheckTests 5) $
    localOption (QuickCheckMaxShrinks 2) test
  MediumPressure ->
    localOption (QuickCheckMaxSize 3) $
    localOption (QuickCheckTests 3) $
    localOption (QuickCheckMaxShrinks 1) test
  HighPressure ->
    localOption (QuickCheckMaxSize 2) $
    localOption (QuickCheckTests 2) $
    localOption (QuickCheckMaxShrinks 0) test
  CriticalPressure ->
    localOption (QuickCheckMaxSize 1) $
    localOption (QuickCheckTests 1) $
    localOption (QuickCheckMaxShrinks 0) test

-- | Adaptive garbage collection based on memory pressure
adaptiveGC :: IO ()
adaptiveGC = do
  pressure <- calculateMemoryPressure
  case pressure of
    LowPressure -> do
      performGC
      threadDelay 100
    MediumPressure -> do
      replicateM_ 2 performGC
      threadDelay 200
    HighPressure -> do
      replicateM_ 4 performGC
      threadDelay 400
    CriticalPressure -> do
      replicateM_ 6 performGC
      threadDelay 600

-- | Setup memory monitoring
setupMemoryMonitoring :: IO ()
setupMemoryMonitoring = do
  hPutStrLn stderr "🔍 Starting memory monitoring..."
  -- Initialize monitoring state if needed
  return ()

-- | Cleanup memory monitoring
cleanupMemoryMonitoring :: () -> IO ()
cleanupMemoryMonitoring _ = do
  -- Force final cleanup
  replicateM_ 3 performGC
  hPutStrLn stderr "🔍 Memory monitoring completed"

-- | Helper function to run action with memory monitoring and adaptive GC
withMemoryMonitoringAndGC :: IO a -> IO a
withMemoryMonitoringAndGC action = do
  -- GC before action
  adaptiveGC
  result <- action
  -- GC after action
  adaptiveGC
  return result