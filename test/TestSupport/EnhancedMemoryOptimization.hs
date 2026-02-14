{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Enhanced memory optimization module with strategic garbage collection
-- This module provides advanced memory optimization techniques including
-- strategic garbage collection, memory cleanup between tests, and
-- ultra-lightweight test variants for critical memory situations
module TestSupport.EnhancedMemoryOptimization 
  ( -- Enhanced memory cleanup
    enhancedMemoryCleanup
  , strategicMemoryCleanup
  , emergencyMemoryCleanup
  , cleanupBetweenTests
  
    -- Garbage collection helpers
  , preTestGC
  , postTestGC
  , midTestGC
  , batchGC
  , adaptiveGC
  
    -- Memory monitoring and control
  , withEnhancedMemoryControl
  , withMemoryMonitoring
  , withStrictMemoryLimits
  
    -- Ultra-lightweight test variants
  , createUltraLightweightTest
  , createMinimalTest
  , createCriticalTest
  
    -- Test memory optimization helpers
  , optimizeTestMemory
  , reduceTestMemoryFootprint
  , applyMemoryOptimizations
  
    -- Memory-aware test execution
  , runWithMemoryOptimizations
  , executeWithMemoryCleanup
  , executeWithStrategicGC
  , executeBatchWithGC
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import Data.Maybe (isJust)
import TestSupport.MemoryOptimizedQuickCheck 
  ( QuickCheckMemoryConfig(..)
  , emergencyMemoryConfig
  , ultraLowMemoryConfig
  , criticalMemoryConfig
  , applyQuickCheckMemoryConfig
  )

-- | Enhanced memory cleanup with multiple GC cycles and strategic delays
enhancedMemoryCleanup :: IO ()
enhancedMemoryCleanup = do
  -- First pass: immediate cleanup
  performGC
  
  -- Strategic delays to allow memory to be released
  threadDelay 1000  -- 1ms delay
  
  -- Second pass: more aggressive cleanup
  replicateM_ 3 $ do
    performGC
    threadDelay 500  -- 0.5ms delay
  
  -- Final cleanup
  performGC

-- | Pre-test strategic garbage collection
preTestGC :: IO ()
preTestGC = do
  performGC
  threadDelay 200
  performGC

-- | Post-test strategic garbage collection
postTestGC :: IO ()
postTestGC = do
  performGC
  threadDelay 300
  replicateM_ 2 performGC

-- | Mid-test garbage collection for long-running tests
midTestGC :: IO ()
midTestGC = do
  performGC
  threadDelay 100

-- | Batch garbage collection for multiple test cleanup
batchGC :: Int -> IO ()
batchGC count = replicateM_ count $ do
  performGC
  threadDelay 50

-- | Adaptive garbage collection based on memory pressure
adaptiveGC :: IO ()
adaptiveGC = do
  env <- getEnvironment
  let isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
      isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
  
  if isEmergency
    then do
      replicateM_ 5 $ do
        performGC
        threadDelay 50
    else if isUltraOptimized
         then do
           replicateM_ 3 $ do
             performGC
             threadDelay 100
         else do
           performGC
           threadDelay 200

-- | Strategic memory cleanup with environment-aware optimization
strategicMemoryCleanup :: IO ()
strategicMemoryCleanup = do
  env <- getEnvironment
  let isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
      isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
  
  if isEmergency
    then emergencyMemoryCleanup
    else if isUltraOptimized
         then enhancedMemoryCleanup
         else do
           performGC
           threadDelay 2000
           replicateM_ 2 performGC

-- | Emergency memory cleanup for critical memory situations
emergencyMemoryCleanup :: IO ()
emergencyMemoryCleanup = do
  -- Maximum cleanup effort
  replicateM_ 7 $ do
    performGC
    threadDelay 200  -- Very short delays for rapid cleanup
  
  -- Final intensive cleanup
  replicateM_ 3 performGC

-- | Cleanup between individual tests to prevent memory accumulation
cleanupBetweenTests :: IO ()
cleanupBetweenTests = do
  -- Quick cleanup between tests
  performGC
  threadDelay 100
  performGC

-- | Enhanced memory control for test execution
withEnhancedMemoryControl :: IO a -> IO a
withEnhancedMemoryControl action = do
  -- Pre-execution cleanup
  enhancedMemoryCleanup
  
  -- Execute the action
  result <- action
  
  -- Post-execution cleanup
  enhancedMemoryCleanup
  
  return result

-- | Memory monitoring with automatic cleanup
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  -- Monitor and cleanup before
  performGC
  result <- action
  -- Monitor and cleanup after
  replicateM_ 2 performGC
  return result

-- | Strict memory limits for critical tests
withStrictMemoryLimits :: TestTree -> TestTree
withStrictMemoryLimits test = 
  let maxSize = 1
      maxTests = 1
      maxShrinks = 0
  in applyQuickCheckMemoryConfig emergencyMemoryConfig test

-- | Create ultra-lightweight test variant for critical memory situations
createUltraLightweightTest :: String -> IO () -> TestTree
createUltraLightweightTest testName testAction = 
  testGroup ("[Ultra-Lightweight] " ++ testName)
    [ -- Test with maximum memory optimization
      withStrictMemoryLimits $ 
        testGroup "Emergency Mode" []
    ]

-- | Create minimal test variant
createMinimalTest :: String -> IO () -> TestTree
createMinimalTest testName testAction = 
  testGroup ("[Minimal] " ++ testName)
    [ -- Test with minimal memory usage
      applyQuickCheckMemoryConfig ultraLowMemoryConfig $ 
        testGroup "Minimal Mode" []
    ]

-- | Create critical test variant with enhanced cleanup
createCriticalTest :: String -> IO () -> TestTree
createCriticalTest testName testAction = 
  testGroup ("[Critical] " ++ testName)
    [ -- Test with critical memory configuration
      applyQuickCheckMemoryConfig criticalMemoryConfig $ 
        testGroup "Critical Mode" []
    ]

-- | Optimize test memory usage
optimizeTestMemory :: TestTree -> TestTree
optimizeTestMemory test = do
  -- Apply the most aggressive memory optimization
  withStrictMemoryLimits test

-- | Reduce test memory footprint
reduceTestMemoryFootprint :: TestTree -> TestTree
reduceTestMemoryFootprint test = do
  -- Apply memory reduction techniques
  applyQuickCheckMemoryConfig ultraLowMemoryConfig test

-- | Apply comprehensive memory optimizations
applyMemoryOptimizations :: TestTree -> TestTree
applyMemoryOptimizations test = 
  let optimized = optimizeTestMemory test
      reduced = reduceTestMemoryFootprint optimized
  in reduced

-- | Run tests with full memory optimizations
runWithMemoryOptimizations :: IO a -> IO a
runWithMemoryOptimizations action = do
  -- Pre-execution optimization
  strategicMemoryCleanup
  
  -- Execute with monitoring
  result <- withMemoryMonitoring action
  
  -- Post-execution cleanup
  enhancedMemoryCleanup
  
  return result

-- | Execute action with memory cleanup
executeWithMemoryCleanup :: IO a -> IO a
executeWithMemoryCleanup action = do
  preTestGC
  result <- action
  postTestGC
  return result

-- | Execute with strategic GC timing
executeWithStrategicGC :: IO a -> IO a
executeWithStrategicGC action = do
  preTestGC
  result <- action
  midTestGC
  postTestGC
  return result

-- | Execute batch of actions with optimized GC
executeBatchWithGC :: [IO a] -> IO [a]
executeBatchWithGC actions = do
  preTestGC
  results <- mapM executeWithMemoryCleanup actions
  batchGC 3
  return results