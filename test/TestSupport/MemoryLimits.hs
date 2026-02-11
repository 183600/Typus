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
import Control.Concurrent (threadDelay)

-- | Memory optimization levels
data MemoryLevel = 
    Minimal      -- ^ Minimal memory usage (256MB equivalent)
  | Ultra        -- ^ Ultra low memory usage (512MB equivalent)  
  | Aggressive   -- ^ Aggressive memory limits (1GB equivalent)
  | Moderate     -- ^ Moderate memory limits (2GB equivalent)
  deriving (Show, Eq)

-- | Apply minimal memory limits to a test tree for extreme memory constraints - 进一步优化
withMinimalMemoryLimits :: TestTree -> TestTree
withMinimalMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 极度减少
  localOption (QuickCheckTests 3) $      -- 极度减少
  localOption (QuickCheckMaxShrinks 2) $ -- 极度减少
  test

-- | Apply ultra memory limits to a test tree for very memory-constrained environments - 进一步优化
withUltraMemoryLimits :: TestTree -> TestTree
withUltraMemoryLimits test = 
  localOption (QuickCheckMaxSize 2) $    -- 进一步减少
  localOption (QuickCheckTests 8) $      -- 进一步减少
  localOption (QuickCheckMaxShrinks 5) $ -- 进一步减少
  test

-- | Apply moderate memory limits to a test tree - 进一步优化
withMemoryLimits :: TestTree -> TestTree
withMemoryLimits test = 
  localOption (QuickCheckMaxSize 5) $    -- 大幅减少
  localOption (QuickCheckTests 20) $     -- 大幅减少
  localOption (QuickCheckMaxShrinks 15) $ -- 大幅减少
  test

-- | Apply aggressive memory limits to a test tree for memory-constrained environments - 进一步优化
withAggressiveMemoryLimits :: TestTree -> TestTree
withAggressiveMemoryLimits test = 
  localOption (QuickCheckMaxSize 3) $    -- 进一步减少
  localOption (QuickCheckTests 12) $     -- 进一步减少
  localOption (QuickCheckMaxShrinks 8) $ -- 进一步减少
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

-- | Force aggressive garbage collection to free maximum memory - 增强垃圾回收
aggressiveGC :: IO ()
aggressiveGC = do
  performGC
  -- 多轮GC，每轮间隔很短
  replicateM_ 3 $ do
    performGC
    threadDelay 5000 -- 5ms间隔

-- | Force ultra aggressive garbage collection for memory-critical situations - 极限垃圾回收
ultraGC :: IO ()
ultraGC = do
  performGC
  -- 多轮GC，每轮间隔很短，确保彻底清理
  replicateM_ 5 $ do
    performGC
    threadDelay 3000 -- 3ms间隔，更频繁的GC
  
  -- 最终清理
  performGC

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