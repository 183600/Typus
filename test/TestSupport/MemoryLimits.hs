{-# LANGUAGE CPP #-}

module TestSupport.MemoryLimits
  ( withMemoryLimits
  , withAggressiveMemoryLimits
  , withUltraMemoryLimits
  , withMinimalMemoryLimits
  , memoryLimitedTestGroup
  , aggressiveMemoryLimitedTestGroup
  , ultraMemoryLimitedTestGroup
  , minimalMemoryLimitedTestGroup
  , gcBetweenTests
  , aggressiveGC
  , ultraGC
  , withMemoryMonitoring
  , withMemoryLevel
  , memoryLevelTestGroup
  , MemoryLevel(..)
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)

-- | Memory optimization levels
data MemoryLevel = 
    Minimal      -- ^ Minimal memory usage (256MB equivalent)
  | Ultra        -- ^ Ultra low memory usage (512MB equivalent)  
  | Aggressive   -- ^ Aggressive memory limits (1GB equivalent)
  | Moderate     -- ^ Moderate memory limits (2GB equivalent)
  deriving (Show, Eq)

-- | Apply minimal memory limits to a test tree for extreme memory constraints
withMinimalMemoryLimits :: TestTree -> TestTree
withMinimalMemoryLimits test = 
  localOption (QuickCheckMaxSize 3) $
  localOption (QuickCheckTests 10) $
  localOption (QuickCheckMaxShrinks 10) $
  test

-- | Apply ultra memory limits to a test tree for very memory-constrained environments
withUltraMemoryLimits :: TestTree -> TestTree
withUltraMemoryLimits test = 
  localOption (QuickCheckMaxSize 5) $
  localOption (QuickCheckTests 25) $
  localOption (QuickCheckMaxShrinks 25) $
  test

-- | Apply moderate memory limits to a test tree
withMemoryLimits :: TestTree -> TestTree
withMemoryLimits test = 
  localOption (QuickCheckMaxSize 15) $
  localOption (QuickCheckTests 75) $
  localOption (QuickCheckMaxShrinks 50) $
  test

-- | Apply aggressive memory limits to a test tree for memory-constrained environments
withAggressiveMemoryLimits :: TestTree -> TestTree
withAggressiveMemoryLimits test = 
  localOption (QuickCheckMaxSize 10) $
  localOption (QuickCheckTests 50) $
  localOption (QuickCheckMaxShrinks 35) $
  test

-- | Create a test group with minimal memory limits
minimalMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
minimalMemoryLimitedTestGroup name tests = 
  let limitedTests = map withMinimalMemoryLimits tests
  in testGroup ("[Ultra-Memory-Optimized] " ++ name) limitedTests

-- | Create a test group with ultra memory limits
ultraMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
ultraMemoryLimitedTestGroup name tests = 
  let limitedTests = map withUltraMemoryLimits tests
  in testGroup ("[Ultra-Memory-Optimized] " ++ name) limitedTests

-- | Create a test group with memory limits
memoryLimitedTestGroup :: String -> [TestTree] -> TestTree
memoryLimitedTestGroup name tests = 
  let limitedTests = map withMemoryLimits tests
  in testGroup ("[Memory-Limited] " ++ name) limitedTests

-- | Create a test group with aggressive memory limits and garbage collection
aggressiveMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
aggressiveMemoryLimitedTestGroup name tests = 
  let limitedTests = map withAggressiveMemoryLimits tests
  in testGroup ("[Memory-Optimized] " ++ name) limitedTests

-- Force garbage collection to free memory between tests
gcBetweenTests :: IO ()
gcBetweenTests = performGC

-- | Force aggressive garbage collection to free maximum memory
aggressiveGC :: IO ()
aggressiveGC = do
  performGC
  -- Additional GC passes to ensure maximum memory cleanup
  replicateM_ 3 performGC

-- | Force ultra aggressive garbage collection for memory-critical situations
ultraGC :: IO ()
ultraGC = do
  performGC
  -- Multiple GC passes with different strategies
  replicateM_ 5 performGC

-- | Add memory monitoring and cleanup to a test
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  -- Force GC before test
  performGC
  result <- action
  -- Force GC after test to clean up
  replicateM_ 2 performGC
  return result

-- | Helper to apply memory limits based on level
withMemoryLevel :: MemoryLevel -> TestTree -> TestTree
withMemoryLevel level test = case level of
  Minimal    -> withMinimalMemoryLimits test
  Ultra      -> withUltraMemoryLimits test
  Aggressive -> withAggressiveMemoryLimits test
  Moderate   -> withMemoryLimits test

-- | Helper to create test groups with memory level
memoryLevelTestGroup :: MemoryLevel -> String -> [TestTree] -> TestTree
memoryLevelTestGroup level name tests = 
  let limitedTests = map (withMemoryLevel level) tests
      prefix = case level of
        Minimal    -> "[Ultra-Memory-Optimized] "
        Ultra      -> "[Ultra-Memory-Optimized] "
        Aggressive -> "[Memory-Optimized] "
        Moderate   -> "[Memory-Limited] "
  in testGroup (prefix ++ name) limitedTests