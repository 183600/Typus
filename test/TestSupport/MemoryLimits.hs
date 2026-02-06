{-# LANGUAGE CPP #-}

module TestSupport.MemoryLimits
  ( withMemoryLimits
  , memoryLimitedTestGroup
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..))
import System.Mem (performGC)

-- | Apply memory limits to a test tree
withMemoryLimits :: TestTree -> TestTree
withMemoryLimits test = 
  localOption (QuickCheckMaxSize 20) $
  localOption (QuickCheckTests 100) $
  test

-- | Create a test group with memory limits and garbage collection
memoryLimitedTestGroup :: String -> [TestTree] -> TestTree
memoryLimitedTestGroup name tests = 
  let limitedTests = map withMemoryLimits tests
  in testGroup name limitedTests

-- Force garbage collection to free memory between tests
gcBetweenTests :: IO ()
gcBetweenTests = performGC