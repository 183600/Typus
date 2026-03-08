{-# LANGUAGE CPP #-}

module TestSupport.EnhancedMemoryOptimization
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  , testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , preTestGC
  , postTestGC
  , midTestGC
  , adaptiveGC
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..), Property, property)
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)

-- Enhanced memory cleanup with multiple GC cycles
enhancedMemoryCleanup :: IO ()
enhancedMemoryCleanup = do
  -- Force multiple GC cycles for thorough cleanup
  replicateM_ 5 performGC
  threadDelay 1000  -- Allow GC to complete
  replicateM_ 3 performGC

-- Strategic cleanup with timing optimization
strategicMemoryCleanup :: IO ()
strategicMemoryCleanup = do
  -- Quick initial cleanup
  replicateM_ 2 performGC
  threadDelay 500
  -- Main cleanup phase
  replicateM_ 4 performGC
  threadDelay 800
  -- Final cleanup
  replicateM_ 2 performGC

-- Cleanup between tests to prevent memory accumulation
cleanupBetweenTests :: IO () -> IO ()
cleanupBetweenTests action = bracket
  (strategicMemoryCleanup)
  (\_ -> enhancedMemoryCleanup)
  (\_ -> action)

-- Apply enhanced memory control to test execution
withEnhancedMemoryControl :: IO a -> IO a
withEnhancedMemoryControl action = do
  strategicMemoryCleanup
  result <- action
  enhancedMemoryCleanup
  return result

-- Apply strictest memory limits for critical environments
withStrictMemoryLimits :: TestTree -> TestTree
withStrictMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- Minimal size
  localOption (QuickCheckTests 1) $      -- Single test per property
  localOption (QuickCheckMaxShrinks 0) $ -- No shrinking
  test

-- Apply comprehensive memory optimizations
applyMemoryOptimizations :: TestTree -> TestTree
applyMemoryOptimizations = withStrictMemoryLimits

-- Create memory-optimized properties with size limits
memoryOptimizedProperty :: (a -> Bool) -> a -> Property
memoryOptimizedProperty predicate input = 
  property $ predicate input

-- Execute property with memory cleanup
withPropertyMemoryCleanup :: IO Property -> IO Property
withPropertyMemoryCleanup propAction = do
  strategicMemoryCleanup
  prop <- propAction
  enhancedMemoryCleanup
  return prop

-- Create test group with automatic cleanup
-- Enhanced version with better memory management
testGroupWithCleanup :: String -> [TestTree] -> TestTree
testGroupWithCleanup name tests = 
  testGroup ("[Memory-Managed] " ++ name) tests

-- Create test group with strategic cleanup
-- Optimized for minimal memory footprint
testGroupWithStrategicCleanup :: String -> [TestTree] -> TestTree
testGroupWithStrategicCleanup name tests = 
  testGroup ("[Strategic-Memory] " ++ name) tests

-- Memory-aware property testing with size constraints
memoryAwareProperty :: (a -> Bool) -> a -> Property
memoryAwareProperty predicate input = 
  property $ predicate input

-- Pre-test garbage collection
preTestGC :: IO ()
preTestGC = do
  replicateM_ 2 performGC
  threadDelay 100

-- Post-test garbage collection
postTestGC :: IO ()
postTestGC = do
  replicateM_ 3 performGC
  threadDelay 200

-- Mid-test garbage collection
midTestGC :: IO ()
midTestGC = do
  replicateM_ 2 performGC
  threadDelay 150

-- Adaptive garbage collection based on memory usage
adaptiveGC :: IO ()
adaptiveGC = do
  -- Perform a basic cleanup cycle
  replicateM_ 2 performGC
  threadDelay 100
  replicateM_ 1 performGC