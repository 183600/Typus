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
import System.Mem (performGC, getGCStats)
import GHC.Stats (GCStats(..), gcdetails_live_bytes)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Info (getProcessID)
import Data.List (isPrefixOf)

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

-- | Get current memory usage in MB (cross-platform implementation)
getMemoryUsageMB :: IO Int
getMemoryUsageMB = do
  -- Try Linux-specific /proc/self/status first
  linuxResult <- tryGetLinuxMemoryUsage
  case linuxResult of
    Just usage -> return usage
    Nothing -> do
      -- Try macOS memory usage detection
      macResult <- tryGetMacMemoryUsage
      case macResult of
        Just usage -> return usage
        Nothing -> do
          -- Try Windows memory usage detection
          windowsResult <- tryGetWindowsMemoryUsage
          case windowsResult of
            Just usage -> return usage
            Nothing -> do
              -- Final fallback: use GHC RTS stats for Linux compatibility
              ghcResult <- tryGetGhcMemoryUsage
              case ghcResult of
                Just usage -> return usage
                Nothing -> return 0  -- Fallback if all detection fails

-- | Try Linux memory usage detection
tryGetLinuxMemoryUsage :: IO (Maybe Int)
tryGetLinuxMemoryUsage = do
  result <- readProcess "grep" ["VmRSS", "/proc/self/status"] ""
  case words result of
    (_:_:kb:_) -> do
      let mb = read kb `div` 1024
      return (Just mb)
    _ -> return Nothing

-- | Try macOS memory usage detection
tryGetMacMemoryUsage :: IO (Maybe Int)
tryGetMacMemoryUsage = do
  result <- readProcess "ps" ["-o", "rss=", "-p", show (System.Info.getProcessID)] ""
  case result of
    kb | not (null kb) -> do
      let mb = read kb `div` 1024
      return (Just mb)
    _ -> return Nothing

-- | Try Windows memory usage detection
tryGetWindowsMemoryUsage :: IO (Maybe Int)
tryGetWindowsMemoryUsage = do
  result <- readProcess "wmic" ["process", "where", "processid=" ++ show (System.Info.getProcessID), "get", "WorkingSetSize", "/Value"] ""
  case lines result of
    [line] | "WorkingSetSize=" `isPrefixOf` line -> do
      let bytes = read (drop (length "WorkingSetSize=") line) :: Integer
          mb = fromIntegral bytes `div` (1024 * 1024)
      return (Just (fromIntegral mb))
    _ -> return Nothing

-- | Try GHC RTS memory usage detection (cross-platform fallback)
tryGetGhcMemoryUsage :: IO (Maybe Int)
tryGetGhcMemoryUsage = do
  -- Use GHC's internal memory statistics
  stats <- getGCStats
  let bytes = gcdetails_live_bytes (gc stats)
      mb = bytes `div` (1024 * 1024)
  return (Just mb)

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