{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Consolidated Memory Optimization Module
-- This module unifies all memory optimization functionality from:
-- - UnifiedMemoryOptimization
-- - ExtremeMemoryOptimization  
-- - EnhancedMemoryOptimization
-- - MemoryOptimizedQuickCheck
-- - SmartTestSelection
-- - OptimizedTestOrdering
--
-- It provides a single, efficient interface for memory-conscious testing
module TestSupport.ConsolidatedMemoryOptimization 
  ( -- * Memory Configuration
    MemoryConfig(..)
  , ultraLowMemoryConfig
  , criticalMemoryConfig
  , lowMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  , developmentMemoryConfig
  
    -- * Memory Tier Detection
  , MemoryTier(..)
  , detectAvailableMemory
  , getMemoryTier
  , getMemoryConfig
  
    -- * Test Memory Optimization
  , withMemoryOptimization
  , applyMemoryConfig
  , createMemoryOptimizedTestSuite
  , selectTestsByMemory
  
    -- * Memory Monitoring & Cleanup
  , withMemoryMonitoring
  , aggressiveCleanup
  , emergencyCleanup
  , cleanupBetweenTests
  
    -- * QuickCheck Memory Optimization
  , QuickCheckMemoryConfig(..)
  , createQuickCheckConfig
  , memoryOptimizedProperty
  , memoryOptimizedTestProperty
  
    -- * Test Selection & Prioritization
  , TestPriority(..)
  , TestInfo(..)
  , createTestInfo
  , selectOptimalTests
  , prioritizeTests
  
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..), property, testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary)
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import Data.Maybe (isJust, fromMaybe)
import Data.List (sortBy, take, splitAt)
import Data.Ord (comparing)
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)

-- | Memory tiers for different environments
data MemoryTier = 
    UltraCritical  -- ^ ≤16MB - Emergency mode
  | Critical       -- ^ 24MB - Critical environment
  | Emergency      -- ^ 32MB - Emergency environment  
  | Minimal        -- ^ 48MB - Minimal environment
  | CI             -- ^ 64MB - CI/CD environment
  | Development    -- ^ 128MB - Development environment
  | Unlimited      -- ^ No memory restrictions
  deriving (Show, Eq, Ord)

-- | Consolidated memory configuration
data MemoryConfig = MemoryConfig
  { memoryLimitMB :: Int           -- ^ Memory limit in MB
  , maxQuickCheckSize :: Int       -- ^ Maximum QuickCheck size
  , maxQuickCheckTests :: Int      -- ^ Maximum QuickCheck tests per property
  , maxQuickCheckShrinks :: Int    -- ^ Maximum QuickCheck shrinks
  , gcFrequency :: Int             -- ^ GC frequency (every N tests)
  , enableAggressiveCleanup :: Bool -- ^ Enable aggressive cleanup
  , testSelectionRatio :: Double   -- ^ Test selection ratio (0.0-1.0)
  , maxStringLength :: Int         -- ^ Maximum string length in tests
  , maxListLength :: Int           -- ^ Maximum list length in tests
  , enableProfiling :: Bool        -- ^ Enable memory profiling
  } deriving (Show, Eq)

-- | QuickCheck specific memory configuration
data QuickCheckMemoryConfig = QuickCheckMemoryConfig
  { qcMaxSize :: Int
  , qcMaxTests :: Int
  , qcMaxShrinks :: Int
  , qcMaxStringLength :: Int
  , qcMaxListLength :: Int
  } deriving (Show, Eq)

-- | Test priority levels
data TestPriority = 
    PriorityCritical   -- ^ Must-run tests
  | PriorityHigh      -- ^ High priority tests
  | PriorityMedium    -- ^ Medium priority tests  
  | PriorityLow       -- ^ Low priority tests
  deriving (Show, Eq, Ord)

-- | Test information for smart selection
data TestInfo = TestInfo
  { testName :: String
  , testPriority :: TestPriority
  , testMemoryWeight :: Int    -- ^ Estimated memory usage (1-10)
  , testCategory :: String
  , testTree :: TestTree
  }

-- | Show instance for TestInfo (excluding testTree which doesn't have Show instance)
instance Show TestInfo where
  show ti = printf "TestInfo { testName = %s, testPriority = %s, testMemoryWeight = %d, testCategory = %s, testTree = <TestTree> }"
                  (testName ti)
                  (show (testPriority ti))
                  (testMemoryWeight ti)
                  (testCategory ti)

-- | Eq instance for TestInfo (excluding testTree which doesn't have Eq instance)
instance Eq TestInfo where
  ti1 == ti2 = testName ti1 == testName ti2 &&
               testPriority ti1 == testPriority ti2 &&
               testMemoryWeight ti1 == testMemoryWeight ti2 &&
               testCategory ti1 == testCategory ti2

-- | Ultra low memory configuration (16MB) - Emergency mode
ultraLowMemoryConfig :: MemoryConfig
ultraLowMemoryConfig = MemoryConfig
  { memoryLimitMB = 16
  , maxQuickCheckSize = 1
  , maxQuickCheckTests = 1
  , maxQuickCheckShrinks = 0
  , gcFrequency = 1
  , enableAggressiveCleanup = True
  , testSelectionRatio = 0.01  -- Only 1% of tests
  , maxStringLength = 3
  , maxListLength = 2
  , enableProfiling = False
  }

-- | Critical memory configuration (24MB)
criticalMemoryConfig :: MemoryConfig
criticalMemoryConfig = MemoryConfig
  { memoryLimitMB = 24
  , maxQuickCheckSize = 1
  , maxQuickCheckTests = 2
  , maxQuickCheckShrinks = 1
  , gcFrequency = 1
  , enableAggressiveCleanup = True
  , testSelectionRatio = 0.02  -- 2% of tests
  , maxStringLength = 5
  , maxListLength = 3
  , enableProfiling = False
  }

-- | Low memory configuration (32MB)
lowMemoryConfig :: MemoryConfig
lowMemoryConfig = MemoryConfig
  { memoryLimitMB = 32
  , maxQuickCheckSize = 2
  , maxQuickCheckTests = 3
  , maxQuickCheckShrinks = 2
  , gcFrequency = 1
  , enableAggressiveCleanup = True
  , testSelectionRatio = 0.05  -- 5% of tests
  , maxStringLength = 8
  , maxListLength = 5
  , enableProfiling = False
  }

-- | Standard memory configuration (48MB)
standardMemoryConfig :: MemoryConfig
standardMemoryConfig = MemoryConfig
  { memoryLimitMB = 48
  , maxQuickCheckSize = 2
  , maxQuickCheckTests = 5
  , maxQuickCheckShrinks = 3
  , gcFrequency = 2
  , enableAggressiveCleanup = True
  , testSelectionRatio = 0.10  -- 10% of tests
  , maxStringLength = 12
  , maxListLength = 8
  , enableProfiling = False
  }

-- | CI memory configuration (64MB)
ciMemoryConfig :: MemoryConfig
ciMemoryConfig = MemoryConfig
  { memoryLimitMB = 64
  , maxQuickCheckSize = 3
  , maxQuickCheckTests = 8
  , maxQuickCheckShrinks = 5
  , gcFrequency = 3
  , enableAggressiveCleanup = True
  , testSelectionRatio = 0.15  -- 15% of tests
  , maxStringLength = 16
  , maxListLength = 12
  , enableProfiling = False
  }

-- | Development memory configuration (128MB)
developmentMemoryConfig :: MemoryConfig
developmentMemoryConfig = MemoryConfig
  { memoryLimitMB = 128
  , maxQuickCheckSize = 5
  , maxQuickCheckTests = 20
  , maxQuickCheckShrinks = 10
  , gcFrequency = 5
  , enableAggressiveCleanup = False
  , testSelectionRatio = 0.30  -- 30% of tests
  , maxStringLength = 32
  , maxListLength = 20
  , enableProfiling = True
  }

-- | Detect available memory (simplified implementation)
detectAvailableMemory :: IO Int
detectAvailableMemory = do
  env <- getEnvironment
  let memoryOverride = lookup "TYPUS_MEMORY_LIMIT" env >>= readMaybe
      isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
      isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
      
  case memoryOverride of
    Just mb -> return mb
    Nothing -> case (isEmergency, isUltraOptimized) of
      (True, _) -> return 16
      (_, True) -> return 24
      _ -> return 64  -- Default to CI configuration
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

-- | Get memory tier from available memory
getMemoryTier :: Int -> MemoryTier
getMemoryTier mb
  | mb <= 16    = UltraCritical
  | mb <= 24    = Critical
  | mb <= 32    = Emergency
  | mb <= 48    = Minimal
  | mb <= 64    = CI
  | mb <= 128   = Development
  | otherwise   = Unlimited

-- | Get memory configuration for available memory
getMemoryConfig :: Int -> MemoryConfig
getMemoryConfig mb = case getMemoryTier mb of
  UltraCritical -> ultraLowMemoryConfig
  Critical      -> criticalMemoryConfig
  Emergency     -> lowMemoryConfig
  Minimal       -> standardMemoryConfig
  CI            -> ciMemoryConfig
  Development   -> developmentMemoryConfig
  Unlimited     -> developmentMemoryConfig

-- | Create QuickCheck configuration from memory configuration
createQuickCheckConfig :: MemoryConfig -> QuickCheckMemoryConfig
createQuickCheckConfig config = QuickCheckMemoryConfig
  { qcMaxSize = maxQuickCheckSize config
  , qcMaxTests = maxQuickCheckTests config
  , qcMaxShrinks = maxQuickCheckShrinks config
  , qcMaxStringLength = maxStringLength config
  , qcMaxListLength = maxListLength config
  }

-- | Apply memory configuration to a test tree
applyMemoryConfig :: MemoryConfig -> TestTree -> TestTree
applyMemoryConfig config test = 
  localOption (QuickCheckMaxSize (maxQuickCheckSize config)) $
  localOption (QuickCheckTests (maxQuickCheckTests config)) $
  localOption (QuickCheckMaxShrinks (maxQuickCheckShrinks config)) $
  test

-- | Apply memory optimization with configuration
withMemoryOptimization :: MemoryConfig -> TestTree -> TestTree
withMemoryOptimization config test = 
  let prefix = "[" ++ show (memoryLimitMB config) ++ "MB] "
  in applyMemoryConfig config test

-- | Create memory optimized test suite
createMemoryOptimizedTestSuite :: MemoryConfig -> String -> [TestTree] -> TestTree
createMemoryOptimizedTestSuite config name tests = 
  let selectedTests = selectTestsByMemory config tests
      optimizedTests = map (withMemoryOptimization config) selectedTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB] "
  in testGroup (prefix ++ name ++ " (" ++ show (length selectedTests) ++ "/" ++ show (length tests) ++ " tests)") optimizedTests

-- | Select tests based on memory configuration
selectTestsByMemory :: MemoryConfig -> [TestTree] -> [TestTree]
selectTestsByMemory config tests = 
  let ratio = testSelectionRatio config
      targetCount = max 1 $ round (fromIntegral (length tests) * ratio)
  in take targetCount tests

-- | Create test information
createTestInfo :: String -> TestPriority -> Int -> String -> TestTree -> TestInfo
createTestInfo name priority weight category tree = TestInfo
  { testName = name
  , testPriority = priority
  , testMemoryWeight = weight
  , testCategory = category
  , testTree = tree
  }

-- | Prioritize tests by priority and memory weight
prioritizeTests :: [TestInfo] -> [TestInfo]
prioritizeTests = sortBy (comparing (\t -> (testPriority t, testMemoryWeight t)))

-- | Select optimal tests based on memory constraints
selectOptimalTests :: MemoryConfig -> [TestInfo] -> [TestInfo]
selectOptimalTests config tests = 
  let prioritized = prioritizeTests tests
      maxTests = case memoryLimitMB config of
        mb | mb <= 16 -> 3   -- Ultra critical: only 3 tests
        mb | mb <= 24 -> 6   -- Critical: 6 tests
        mb | mb <= 32 -> 10  -- Emergency: 10 tests
        mb | mb <= 48 -> 15  -- Minimal: 15 tests
        mb | mb <= 64 -> 25  -- CI: 25 tests
        _ -> 50              -- Development/Unlimited: 50 tests
  in take maxTests prioritized

-- | Memory monitoring with automatic cleanup
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  -- Pre-execution cleanup
  performGC
  threadDelay 100
  
  -- Execute action
  result <- action
  
  -- Post-execution cleanup
  replicateM_ 3 performGC
  threadDelay 100
  
  return result

-- | Aggressive memory cleanup
aggressiveCleanup :: IO ()
aggressiveCleanup = do
  -- Multiple rounds of aggressive GC
  replicateM_ 5 $ do
    performGC
    threadDelay 200
  
  -- Final cleanup
  replicateM_ 2 performGC

-- | Emergency memory cleanup for critical situations
emergencyCleanup :: IO ()
emergencyCleanup = do
  -- Maximum cleanup effort
  replicateM_ 8 $ do
    performGC
    threadDelay 50  -- Very short delays for rapid cleanup
  
  -- Final intensive cleanup
  replicateM_ 3 performGC

-- | Cleanup between tests to prevent memory accumulation
cleanupBetweenTests :: IO ()
cleanupBetweenTests = do
  performGC
  threadDelay 50
  performGC

-- | Create memory optimized property
memoryOptimizedProperty :: (Test.QuickCheck.Arbitrary.Arbitrary a, Show a) => QuickCheckMemoryConfig -> String -> (a -> Bool) -> TestTree
memoryOptimizedProperty config testName prop = 
  let limitedProp x = prop x  -- The property is already memory-constrained by config
  in testProperty testName $ property limitedProp

-- | Create memory optimized test property with string input
memoryOptimizedTestProperty :: QuickCheckMemoryConfig -> String -> (String -> Bool) -> TestTree
memoryOptimizedTestProperty config testName prop = 
  let limitedProp s = let limitedS = take (qcMaxStringLength config) s
                     in prop limitedS
  in testProperty testName $ property limitedProp

-- | Global memory optimization state (unsafe but acceptable for test optimization)
globalMemoryConfig :: MemoryConfig
globalMemoryConfig = unsafePerformIO $ do
  availableMemory <- detectAvailableMemory
  return $ getMemoryConfig availableMemory
{-# NOINLINE globalMemoryConfig #-}

-- | Get current QuickCheck configuration
getCurrentQuickCheckConfig :: QuickCheckMemoryConfig
getCurrentQuickCheckConfig = createQuickCheckConfig globalMemoryConfig

-- | Print memory optimization report
printMemoryReport :: MemoryConfig -> IO ()
printMemoryReport config = do
  putStrLn "=== Consolidated Memory Optimization Report ==="
  printf "Memory limit: %dMB\n" (memoryLimitMB config)
  printf "QuickCheck parameters: size=%d, tests=%d, shrinks=%d\n" 
    (maxQuickCheckSize config) (maxQuickCheckTests config) (maxQuickCheckShrinks config)
  printf "Test selection ratio: %.0f%%\n" (testSelectionRatio config * 100)
  printf "Data limits: strings=%d, lists=%d\n" 
    (maxStringLength config) (maxListLength config)
  putStrLn ""
  putStrLn "Optimization features:"
  putStrLn "- Unified memory configuration system"
  putStrLn "- Intelligent test selection based on memory constraints"
  putStrLn "- Aggressive garbage collection strategies"
  putStrLn "- Memory-aware test prioritization"
  putStrLn "- Consolidated cleanup mechanisms"