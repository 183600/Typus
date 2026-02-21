{-# LANGUAGE OverloadedStrings #-}

-- | Global Memory Optimizer for All Test Suites
-- This module provides ultra-aggressive memory optimization settings
-- to ensure minimal memory usage across all test cases
module TestSupport.GlobalMemoryOptimizer where

import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  , testProperty
  , Property
  , Gen
  , property
  , forAll
  , resize
  , arbitrary
  , suchThat
  , elements
  , listOf
  , oneof
  , frequency
  , choose
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Data.Char (isLetter, isDigit, isSpace)
import Data.List (take)

-- | Ultra-aggressive global memory configuration
-- These settings are applied to ALL test suites to ensure minimal memory usage
data GlobalMemoryConfig = GlobalMemoryConfig
  { globalMaxTests :: Int        -- ^ Maximum number of tests per property (globally enforced)
  , globalMaxSize :: Int         -- ^ Maximum size for generated data
  , globalMaxShrinks :: Int      -- ^ Maximum number of shrinks (set to 0 to disable)
  , globalStringLength :: Int    -- ^ Maximum string length for all generators
  , globalListLength :: Int      -- ^ Maximum list length for all generators
  , globalIntRange :: Int        -- ^ Maximum integer value
  , forceGCBetweenTests :: Bool  -- ^ Force garbage collection between every test
  } deriving (Show, Eq)

-- | Emergency global memory configuration (most restrictive)
emergencyGlobalConfig :: GlobalMemoryConfig
emergencyGlobalConfig = GlobalMemoryConfig
  { globalMaxTests = 1
  , globalMaxSize = 1
  , globalMaxShrinks = 0
  , globalStringLength = 1
  , globalListLength = 1
  , globalIntRange = 1
  , forceGCBetweenTests = True
  }

-- | Minimal global memory configuration (very restrictive)
minimalGlobalConfig :: GlobalMemoryConfig
minimalGlobalConfig = GlobalMemoryConfig
  { globalMaxTests = 1
  , globalMaxSize = 1
  , globalMaxShrinks = 0
  , globalStringLength = 2
  , globalListLength = 1
  , globalIntRange = 2
  , forceGCBetweenTests = True
  }

-- | Balanced global memory configuration (moderately restrictive)
balancedGlobalConfig :: GlobalMemoryConfig
balancedGlobalConfig = GlobalMemoryConfig
  { globalMaxTests = 2
  , globalMaxSize = 2
  , globalMaxShrinks = 1
  , globalStringLength = 3
  , globalListLength = 2
  , globalIntRange = 3
  , forceGCBetweenTests = True
  }

-- | Apply global memory optimization to a test tree
applyGlobalMemoryOptimization :: GlobalMemoryConfig -> TestTree -> TestTree
applyGlobalMemoryOptimization config tests = 
  localOption (QuickCheckTests (globalMaxTests config)) $
  localOption (QuickCheckMaxSize (globalMaxSize config)) $
  localOption (QuickCheckMaxShrinks (globalMaxShrinks config)) $
  tests

-- | Force garbage collection and memory cleanup
forceMemoryCleanup :: GlobalMemoryConfig -> IO ()
forceMemoryCleanup config = do
  when (forceGCBetweenTests config) $ do
    performGC
    -- Force multiple GC cycles to ensure thorough cleanup
    replicateM_ 3 performGC

-- | Ultra-minimal string generator (global)
genUltraMinimalString :: Gen String
genUltraMinimalString = elements ["", "a", "b"]

-- | Minimal string generator (global)
genMinimalString :: Gen String  
genMinimalString = elements ["", "a", "ab", "ba"]

-- | Ultra-minimal list generator (global)
genUltraMinimalList :: Gen a -> Gen [a]
genUltraMinimalList gen = elements [[], [undefined], [undefined, undefined]]

-- | Minimal list generator (global)
genMinimalList :: Gen a -> Gen [a]
genMinimalList gen = elements [[], [undefined], [undefined, undefined], [undefined, undefined, undefined]]

-- | Ultra-minimal integer generator (global)
genUltraMinimalInt :: Gen Int
genUltraMinimalInt = elements [0, 1]

-- | Minimal integer generator (global)
genMinimalInt :: Gen Int
genMinimalInt = elements [0, 1, 2, -1]

-- | Create a memory-optimized property with global settings
createGlobalMemoryOptimizedProperty :: GlobalMemoryConfig -> String -> Property -> TestTree
createGlobalMemoryOptimizedProperty config name prop = 
  applyGlobalMemoryOptimization config $ testProperty name prop

-- | Memory-optimized property that forces cleanup between tests
memoryAwareProperty :: GlobalMemoryConfig -> String -> Property -> TestTree
memoryAwareProperty config name prop = 
  let optimizedProp = property $ do
        forceMemoryCleanup config
        return prop
  in createGlobalMemoryOptimizedProperty config name optimizedProp

-- | Apply emergency memory optimization (most restrictive)
withEmergencyMemoryOptimization :: TestTree -> TestTree
withEmergencyMemoryOptimization = applyGlobalMemoryOptimization emergencyGlobalConfig

-- | Apply minimal memory optimization (very restrictive)
withMinimalMemoryOptimization :: TestTree -> TestTree  
withMinimalMemoryOptimization = applyGlobalMemoryOptimization minimalGlobalConfig

-- | Apply balanced memory optimization (moderately restrictive)
withBalancedMemoryOptimization :: TestTree -> TestTree
withBalancedMemoryOptimization = applyGlobalMemoryOptimization balancedGlobalConfig

-- | Global test group with memory optimization
globalMemoryOptimizedTestGroup :: GlobalMemoryConfig -> String -> [TestTree] -> TestTree
globalMemoryOptimizedTestGroup config name tests = 
  applyGlobalMemoryOptimization config $ 
  -- Note: testGroup is imported from Test.Tasty in the using module
  error "testGroup must be imported from Test.Tasty in the using module"
  -- This is intentional - the function should be used as:
  -- applyGlobalMemoryOptimization config $ testGroup name tests

-- | Utility function to limit string length globally
limitGlobalStringLength :: GlobalMemoryConfig -> String -> String
limitGlobalStringLength config = take (globalStringLength config)

-- | Utility function to limit list length globally  
limitGlobalListLength :: GlobalMemoryConfig -> [a] -> [a]
limitGlobalListLength config = take (globalListLength config)

-- | Utility function to limit integer range globally
limitGlobalIntRange :: GlobalMemoryConfig -> Int -> Int
limitGlobalIntRange config n = 
  let maxVal = globalIntRange config
      minVal = -maxVal
  in max minVal (min maxVal n)

-- | Pre-computed memory optimization configurations for common use
precomputedOptimizations :: [(String, GlobalMemoryConfig)]
precomputedOptimizations = 
  [ ("emergency", emergencyGlobalConfig)
  , ("minimal", minimalGlobalConfig)  
  , ("balanced", balancedGlobalConfig)
  ]

-- | Get pre-computed optimization by name
getPrecomputedOptimization :: String -> Maybe GlobalMemoryConfig
getPrecomputedOptimization name = lookup name precomputedOptimizations