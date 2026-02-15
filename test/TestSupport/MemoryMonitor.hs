{-# LANGUAGE OverloadedStrings #-}
module TestSupport.MemoryMonitor where

import System.Mem (performGC)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Monad (when)
import Control.Exception (evaluate)

-- | Memory monitoring data type
data MemoryStats = MemoryStats
  { memoryBefore :: Integer  -- Memory usage before test (in arbitrary units)
  , memoryAfter :: Integer   -- Memory usage after test (in arbitrary units)
  , memoryDelta :: Integer   -- Memory difference
  , testDuration :: Integer  -- Test duration in microseconds
  } deriving (Show, Eq)

-- | Simple memory monitoring function
monitorMemoryUsage :: IO a -> IO (a, MemoryStats)
monitorMemoryUsage action = do
  -- Force garbage collection before measuring
  performGC
  
  -- Get "before" memory measurement
  memBefore <- getCPUTime
  
  -- Execute the action
  start <- getCPUTime
  result <- action
  end <- evaluate =<< getCPUTime
  
  -- Force garbage collection after measuring
  performGC
  
  -- Get "after" memory measurement
  memAfter <- getCPUTime
  
  let duration = end - start
      delta = memAfter - memBefore
      stats = MemoryStats memBefore memAfter delta duration
  
  return (result, stats)

-- | Check if memory usage exceeds threshold
isMemoryUsageAcceptable :: MemoryStats -> Integer -> Bool
isMemoryUsageAcceptable stats threshold = 
  abs (memoryDelta stats) <= threshold

-- | Print memory statistics
printMemoryStats :: MemoryStats -> String -> IO ()
printMemoryStats stats testName = do
  let deltaMb = fromIntegral (memoryDelta stats) / (1024 * 1024)
      durationMs = fromIntegral (testDuration stats) / 1000000
  
  putStrLn $ "=== Memory Stats for " ++ testName ++ " ==="
  putStrLn $ "Memory Delta: " ++ printf "%.2f" deltaMb ++ " MB"
  putStrLn $ "Test Duration: " ++ printf "%.2f" durationMs ++ " ms"
  
  when (abs (memoryDelta stats) > 1000000) $ 
    putStrLn $ "WARNING: High memory usage detected!"

-- | Enhanced memory monitoring with threshold checking
monitorWithThreshold :: IO a -> Integer -> String -> IO (a, Bool)
monitorWithThreshold action threshold testName = do
  (result, stats) <- monitorMemoryUsage action
  printMemoryStats stats testName
  let acceptable = isMemoryUsageAcceptable stats threshold
  
  when (not acceptable) $
    putStrLn $ "ERROR: Memory usage for " ++ testName ++ " exceeds threshold!"
  
  return (result, acceptable)

-- | Memory monitoring wrapper for tests
withMemoryMonitoring :: String -> IO a -> Integer -> IO a
withMemoryMonitoring testName action threshold = do
  (result, acceptable) <- monitorWithThreshold action threshold testName
  if acceptable
    then return result
    else error $ "Memory usage for " ++ testName ++ " exceeded threshold: " ++ show threshold

-- | Lightweight memory monitoring (no detailed stats)
lightweightMemoryCheck :: IO a -> IO a
lightweightMemoryCheck action = do
  performGC
  result <- action
  performGC
  return result

-- | Memory monitoring for property tests
propertyMemoryMonitor :: IO Bool -> String -> IO Bool
propertyMemoryMonitor prop testName = do
  (result, stats) <- monitorMemoryUsage prop
  
  -- Only print stats for significant memory usage
  when (abs (memoryDelta stats) > 500000) $ do
    let deltaMb = fromIntegral (memoryDelta stats) / (1024 * 1024)
    putStrLn $ "Property " ++ testName ++ " used " ++ printf "%.2f" deltaMb ++ " MB"
  
  return result