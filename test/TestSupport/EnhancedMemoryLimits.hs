{-# LANGUAGE CPP #-}

module TestSupport.EnhancedMemoryLimits
  ( withEnhancedMemoryLimits
  , withStrictMemoryLimits
  , withMinimalMemoryLimits
  , enhancedMemoryLimitedTestGroup
  , strictMemoryLimitedTestGroup
  , minimalMemoryLimitedTestGroup
  , gcBetweenTests
  , aggressiveGC
  , ultraGC
  , withMemoryMonitoring
  , withEnhancedMemoryLevel
  , enhancedMemoryLevelTestGroup
  , EnhancedMemoryLevel(..)
  , MemoryOptimizedGenerators(..)
  , withMemoryOptimizedGenerators
  , memoryOptimizedString
  , memoryOptimizedList
  , memoryOptimizedVector
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Test.QuickCheck (Gen, resize, arbitrary, listOf)

-- | Enhanced memory optimization levels with more granular control
data EnhancedMemoryLevel = 
    Minimal      -- ^ Minimal memory usage (128MB equivalent)
  | Strict       -- ^ Strict memory limits (256MB equivalent)  
  | Conservative -- ^ Conservative memory limits (512MB equivalent)
  | Moderate     -- ^ Moderate memory limits (1GB equivalent)
  deriving (Show, Eq)

-- | Memory-optimized generator configurations
data MemoryOptimizedGenerators = MemoryOptimizedGenerators
  { stringSizeLimit :: Int
  , listSizeLimit :: Int
  , vectorSizeLimit :: Int
  , treeDepthLimit :: Int
  , identifierLengthLimit :: Int
  }

-- Default generator configurations for different memory levels
defaultGenerators :: EnhancedMemoryLevel -> MemoryOptimizedGenerators
defaultGenerators Minimal = MemoryOptimizedGenerators
  { stringSizeLimit = 4
  , listSizeLimit = 2
  , vectorSizeLimit = 2
  , treeDepthLimit = 2
  , identifierLengthLimit = 3
  }
defaultGenerators Strict = MemoryOptimizedGenerators
  { stringSizeLimit = 6
  , listSizeLimit = 3
  , vectorSizeLimit = 3
  , treeDepthLimit = 3
  , identifierLengthLimit = 4
  }
defaultGenerators Conservative = MemoryOptimizedGenerators
  { stringSizeLimit = 8
  , listSizeLimit = 5
  , vectorSizeLimit = 5
  , treeDepthLimit = 4
  , identifierLengthLimit = 6
  }
defaultGenerators Moderate = MemoryOptimizedGenerators
  { stringSizeLimit = 12
  , listSizeLimit = 8
  , vectorSizeLimit = 8
  , treeDepthLimit = 5
  , identifierLengthLimit = 8
  }

-- | Apply minimal memory limits to a test tree for extreme memory constraints
withMinimalMemoryLimits :: TestTree -> TestTree
withMinimalMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $
  localOption (QuickCheckTests 3) $
  localOption (QuickCheckMaxShrinks 0) $
  test

-- | Apply strict memory limits to a test tree for very memory-constrained environments
withStrictMemoryLimits :: TestTree -> TestTree
withStrictMemoryLimits test = 
  localOption (QuickCheckMaxSize 2) $
  localOption (QuickCheckTests 5) $
  localOption (QuickCheckMaxShrinks 2) $
  test

-- | Apply conservative memory limits to a test tree
withConservativeMemoryLimits :: TestTree -> TestTree
withConservativeMemoryLimits test = 
  localOption (QuickCheckMaxSize 3) $
  localOption (QuickCheckTests 10) $
  localOption (QuickCheckMaxShrinks 5) $
  test

-- | Apply enhanced memory limits to a test tree for moderate memory constraints
withEnhancedMemoryLimits :: TestTree -> TestTree
withEnhancedMemoryLimits test = 
  localOption (QuickCheckMaxSize 5) $
  localOption (QuickCheckTests 15) $
  localOption (QuickCheckMaxShrinks 10) $
  test

-- | Create a test group with minimal memory limits
minimalMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
minimalMemoryLimitedTestGroup name tests = 
  let limitedTests = map withMinimalMemoryLimits tests
  in testGroup ("[Ultra-Memory-Optimized] " ++ name) limitedTests

-- | Create a test group with strict memory limits
strictMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
strictMemoryLimitedTestGroup name tests = 
  let limitedTests = map withStrictMemoryLimits tests
  in testGroup ("[Strict-Memory-Optimized] " ++ name) limitedTests



-- | Create a test group with enhanced memory limits
enhancedMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
enhancedMemoryLimitedTestGroup name tests = 
  let limitedTests = map withEnhancedMemoryLimits tests
  in testGroup ("[Enhanced-Memory-Optimized] " ++ name) limitedTests

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

-- | Helper to apply memory limits based on enhanced level
withEnhancedMemoryLevel :: EnhancedMemoryLevel -> TestTree -> TestTree
withEnhancedMemoryLevel level test = case level of
  Minimal    -> withMinimalMemoryLimits test
  Strict     -> withStrictMemoryLimits test
  Conservative -> withConservativeMemoryLimits test
  Moderate   -> withEnhancedMemoryLimits test

-- | Helper to create test groups with enhanced memory level
enhancedMemoryLevelTestGroup :: EnhancedMemoryLevel -> String -> [TestTree] -> TestTree
enhancedMemoryLevelTestGroup level name tests = 
  let limitedTests = map (withEnhancedMemoryLevel level) tests
      prefix = case level of
        Minimal    -> "[Ultra-Memory-Optimized] "
        Strict     -> "[Strict-Memory-Optimized] "
        Conservative -> "[Conservative-Memory-Optimized] "
        Moderate   -> "[Enhanced-Memory-Optimized] "
  in testGroup (prefix ++ name) limitedTests

-- | Apply memory-optimized generator configuration to a generator
withMemoryOptimizedGenerators :: EnhancedMemoryLevel -> Gen a -> Gen a
withMemoryOptimizedGenerators level gen = 
  let config = defaultGenerators level
      maxSize = stringSizeLimit config
  in resize maxSize gen

-- | Memory-optimized string generator
memoryOptimizedString :: EnhancedMemoryLevel -> Gen String
memoryOptimizedString level = 
  let config = defaultGenerators level
      size = stringSizeLimit config
  in resize size arbitrary

-- | Memory-optimized list generator
memoryOptimizedList :: EnhancedMemoryLevel -> Gen a -> Gen [a]
memoryOptimizedList level gen = 
  let config = defaultGenerators level
      size = listSizeLimit config
  in resize size (listOf gen)

-- | Memory-optimized vector generator
memoryOptimizedVector :: EnhancedMemoryLevel -> Gen a -> Gen [a]
memoryOptimizedVector level gen = 
  let config = defaultGenerators level
      size = vectorSizeLimit config
  in resize size (listOf gen)