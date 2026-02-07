{-# LANGUAGE CPP #-}

module TestSupport.MemoryLimits
  ( withMemoryLimits
  , withAggressiveMemoryLimits
  , memoryLimitedTestGroup
  , aggressiveMemoryLimitedTestGroup
  , gcBetweenTests
  , aggressiveGC
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)

-- | Apply moderate memory limits to a test tree
withMemoryLimits :: TestTree -> TestTree
withMemoryLimits test = 
  localOption (QuickCheckMaxSize 20) $
  localOption (QuickCheckTests 100) $
  test

-- | Apply aggressive memory limits to a test tree for memory-constrained environments
withAggressiveMemoryLimits :: TestTree -> TestTree
withAggressiveMemoryLimits test = 
  localOption (QuickCheckMaxSize 10) $
  localOption (QuickCheckTests 50) $
  test

-- | Create a test group with memory limits
memoryLimitedTestGroup :: String -> [TestTree] -> TestTree
memoryLimitedTestGroup name tests = 
  let limitedTests = map withMemoryLimits tests
  in testGroup name limitedTests

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