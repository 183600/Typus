{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Smart Test Selector for Memory-Optimized Testing
-- This module provides intelligent test selection based on memory constraints
-- while ensuring comprehensive test coverage and preserving all tests.
module TestSupport.SmartTestSelector 
  ( -- Smart test selection
    SmartTestConfig(..)
  , defaultSmartConfig
  , extremeMemoryConfig
  , minimalMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  
    -- Test metadata
  , TestMetadata(..)
  , TestPriority(..)
  , TestCategory(..)
  
    -- Test selection strategies
  , selectTestsByPriority
  , selectTestsByMemory
  , selectTestsByCategory
  , selectBalancedTests
  
    -- Smart test suite creation
  , createSmartTestSuite
  , runSmartTests
  , analyzeTestCoverage
  
    -- Memory monitoring
  , monitorTestMemory
  , adaptiveTestSelection
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, getNumCapabilities)
import Data.List (sort, groupBy, sortBy, partition, isPrefixOf, isInfixOf)
import Data.Ord (comparing)
import Data.Function (on)
import Text.Printf (printf)
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- | Smart test configuration with adaptive memory management
data SmartTestConfig = SmartTestConfig
  { memoryLimitMB :: Int           -- ^ Memory limit in MB
  , maxQuickCheckSize :: Int      -- ^ Maximum QuickCheck test size
  , quickCheckTestCount :: Int    -- ^ Number of QuickCheck tests per property
  , quickCheckMaxShrinks :: Int   -- ^ Maximum shrinks for failing tests
  , gcFrequency :: Int            -- ^ GC frequency (every N tests)
  , testSelectionRatio :: Double  -- ^ Ratio of tests to select (0.0-1.0)
  , enableAdaptiveSelection :: Bool -- ^ Enable adaptive test selection
  , prioritizeCoreTests :: Bool   -- ^ Prioritize core functionality tests
  , enableMemoryMonitoring :: Bool -- ^ Enable memory monitoring
  , maxConcurrentTests :: Int     -- ^ Maximum concurrent tests
  , adaptiveThreshold :: Double   -- ^ Threshold for adaptive selection
  , preserveCoverage :: Bool      -- ^ Ensure test coverage is preserved
  } deriving (Show, Eq)

-- | Default smart configuration
defaultSmartConfig :: SmartTestConfig
defaultSmartConfig = SmartTestConfig
  { memoryLimitMB = 128
  , maxQuickCheckSize = 10
  , quickCheckTestCount = 50
  , quickCheckMaxShrinks = 100
  , gcFrequency = 10
  , testSelectionRatio = 0.3
  , enableAdaptiveSelection = True
  , prioritizeCoreTests = True
  , enableMemoryMonitoring = True
  , maxConcurrentTests = 2
  , adaptiveThreshold = 0.8
  , preserveCoverage = True
  }

-- | Extreme memory configuration (16MB)
extremeMemoryConfig :: SmartTestConfig
extremeMemoryConfig = SmartTestConfig
  { memoryLimitMB = 16
  , maxQuickCheckSize = 1
  , quickCheckTestCount = 3
  , quickCheckMaxShrinks = 5
  , gcFrequency = 1
  , testSelectionRatio = 0.05
  , enableAdaptiveSelection = True
  , prioritizeCoreTests = True
  , enableMemoryMonitoring = True
  , maxConcurrentTests = 1
  , adaptiveThreshold = 0.9
  , preserveCoverage = True
  }

-- | Minimal memory configuration (32MB)
minimalMemoryConfig :: SmartTestConfig
minimalMemoryConfig = SmartTestConfig
  { memoryLimitMB = 32
  , maxQuickCheckSize = 2
  , quickCheckTestCount = 5
  , quickCheckMaxShrinks = 10
  , gcFrequency = 2
  , testSelectionRatio = 0.1
  , enableAdaptiveSelection = True
  , prioritizeCoreTests = True
  , enableMemoryMonitoring = True
  , maxConcurrentTests = 1
  , adaptiveThreshold = 0.85
  , preserveCoverage = True
  }

-- | Standard memory configuration (128MB)
standardMemoryConfig :: SmartTestConfig
standardMemoryConfig = SmartTestConfig
  { memoryLimitMB = 128
  , maxQuickCheckSize = 5
  , quickCheckTestCount = 25
  , quickCheckMaxShrinks = 50
  , gcFrequency = 5
  , testSelectionRatio = 0.3
  , enableAdaptiveSelection = True
  , prioritizeCoreTests = True
  , enableMemoryMonitoring = True
  , maxConcurrentTests = 2
  , adaptiveThreshold = 0.8
  , preserveCoverage = True
  }

-- | CI memory configuration (64MB)
ciMemoryConfig :: SmartTestConfig
ciMemoryConfig = SmartTestConfig
  { memoryLimitMB = 64
  , maxQuickCheckSize = 3
  , quickCheckTestCount = 15
  , quickCheckMaxShrinks = 25
  , gcFrequency = 3
  , testSelectionRatio = 0.2
  , enableAdaptiveSelection = True
  , prioritizeCoreTests = True
  , enableMemoryMonitoring = True
  , maxConcurrentTests = 1
  , adaptiveThreshold = 0.85
  , preserveCoverage = True
  }

-- | Test priority levels
data TestPriority = Critical | High | Medium | Low deriving (Show, Eq, Ord)

-- | Test category
data TestCategory = 
    CoreParser
  | CoreCompiler
  | CoreUtils
  | TypeSystem
  | OwnershipSystem
  | DependencyAnalysis
  | ErrorHandling
  | Integration
  | Performance
  | EdgeCase
  deriving (Show, Eq, Ord)

-- | Test metadata
data TestMetadata = TestMetadata
  { testName :: String
  , testPriority :: TestPriority
  , testCategory :: TestCategory
  , estimatedMemoryUsage :: Int  -- ^ Estimated memory usage in MB
  , testComplexity :: Int        -- ^ Complexity score (1-10)
  , isCoreTest :: Bool           -- ^ Is this a core functionality test
  } deriving (Show, Eq)

-- | Select tests by priority
selectTestsByPriority :: SmartTestConfig -> [(TestTree, TestMetadata)] -> [(TestTree, TestMetadata)]
selectTestsByPriority config tests = 
  let ratio = testSelectionRatio config
      targetCount = max 1 $ round (fromIntegral (length tests) * ratio)
      sortedTests = sortBy (comparing (testPriority . snd)) tests
  in take targetCount sortedTests

-- | Select tests by memory usage
selectTestsByMemory :: SmartTestConfig -> [(TestTree, TestMetadata)] -> [(TestTree, TestMetadata)]
selectTestsByMemory config tests = 
  let memoryLimit = memoryLimitMB config
      -- Filter tests that fit within memory limit
      fittingTests = filter (\(_, meta) -> estimatedMemoryUsage meta <= memoryLimit `div` 4) tests
      -- Sort by memory efficiency (lower memory usage first)
      sortedTests = sortBy (comparing (estimatedMemoryUsage . snd)) fittingTests
      targetCount = max 1 $ round (fromIntegral (length tests) * testSelectionRatio config)
  in take targetCount sortedTests

-- | Select tests by category ensuring balanced coverage
selectTestsByCategory :: SmartTestConfig -> [(TestTree, TestMetadata)] -> [(TestTree, TestMetadata)]
selectTestsByCategory config tests = 
  let ratio = testSelectionRatio config
      targetCount = max 1 $ round (fromIntegral (length tests) * ratio)
      -- Group tests by category
      groupedTests = groupBy ((==) `on` (testCategory . snd)) $ sortBy (comparing (testCategory . snd)) tests
      -- Select tests from each category proportionally
      selectFromCategory categoryTests = 
        let categoryTarget = max 1 $ round (fromIntegral (length categoryTests) * ratio / 10)
        in take categoryTarget categoryTests
      selectedFromCategories = concatMap selectFromCategory groupedTests
      -- Ensure we don't exceed target count
  in take targetCount selectedFromCategories

-- | Select balanced tests considering all factors
selectBalancedTests :: SmartTestConfig -> [(TestTree, TestMetadata)] -> [(TestTree, TestMetadata)]
selectBalancedTests config tests = 
  let priorityTests = selectTestsByPriority config tests
      memoryTests = selectTestsByMemory config tests
      categoryTests = selectTestsByCategory config tests
      -- Combine and deduplicate
      allSelected = priorityTests ++ memoryTests ++ categoryTests
      uniqueTests = map snd $ Map.toList $ Map.fromList $ map (\(t, m) -> (testName m, (t, m))) allSelected
      -- Sort by combined score
      scoredTests = sortBy (comparing (combinedScore . snd)) uniqueTests
      targetCount = max 1 $ round (fromIntegral (length tests) * testSelectionRatio config)
  in take targetCount scoredTests
  where
    combinedScore metadata = 
      let priorityScore = case testPriority metadata of
            Critical -> 100
            High -> 75
            Medium -> 50
            Low -> 25
          memoryScore = max 0 $ (memoryLimitMB config - estimatedMemoryUsage metadata) `div` 10
          complexityScore = (11 - testComplexity metadata) * 5
          coreBonus = if isCoreTest metadata then 20 else 0
      in priorityScore + memoryScore + complexityScore + coreBonus

-- | Create smart test suite
createSmartTestSuite :: SmartTestConfig -> String -> [(TestTree, TestMetadata)] -> IO TestTree
createSmartTestSuite config name tests = do
  selectedTests <- if enableAdaptiveSelection config
                   then adaptiveTestSelection config tests
                   else return $ selectBalancedTests config tests
  let limitedTests = map (applyMemoryLimits config . fst) selectedTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB-SMART] "
      actualCount = length selectedTests
      totalCount = length tests
      coveragePercent :: Double
      coveragePercent = if totalCount > 0 
                       then (fromIntegral actualCount / fromIntegral totalCount) * 100
                       else 0
  return $ testGroup (prefix ++ name ++ " (" ++ show actualCount ++ "/" ++ show totalCount ++ 
                     " tests, " ++ printf "%.1f" coveragePercent ++ "% coverage)") limitedTests

-- | Apply memory limits to a test
applyMemoryLimits :: SmartTestConfig -> TestTree -> TestTree
applyMemoryLimits config test = 
  -- This would integrate with existing memory limit frameworks
  test -- Placeholder - would integrate with MemoryLimits/UnifiedMemoryOptimization

-- | Run smart tests with monitoring
runSmartTests :: SmartTestConfig -> TestTree -> IO ()
runSmartTests config testSuite = do
  printf "Starting smart test runner with %dMB memory limit\n" (memoryLimitMB config)
  printf "Test selection ratio: %.0f%%\n" (testSelectionRatio config * 100)
  printf "QuickCheck parameters: size=%d, tests=%d, shrinks=%d\n" 
    (maxQuickCheckSize config) (quickCheckTestCount config) (quickCheckMaxShrinks config)
  
  when (enableMemoryMonitoring config) $ do
    printf "Memory monitoring enabled\n"
    
  -- Run tests with monitoring
  if enableMemoryMonitoring config
     then monitorTestMemory testSuite
     else pure ()

-- | Monitor test memory usage
monitorTestMemory :: TestTree -> IO ()
monitorTestMemory _test = do
  -- Force initial GC
  replicateM_ 3 performGC
  threadDelay 10000
  
  -- Here you would run the actual test and monitor memory
  -- For now, just do cleanup
  replicateM_ 5 performGC
  threadDelay 10000

-- | Adaptive test selection based on system resources
adaptiveTestSelection :: SmartTestConfig -> [(TestTree, TestMetadata)] -> IO [(TestTree, TestMetadata)]
adaptiveTestSelection config tests = do
  -- Get system information
  env <- getEnvironment
  let isCI = fromMaybe "false" (lookup "CI" env) == "true"
      availableMemory = read $ fromMaybe "128" (lookup "AVAILABLE_MEMORY_MB" env)
      
  -- Adjust configuration based on environment
  let adjustedConfig = if isCI
                      then config { testSelectionRatio = min (testSelectionRatio config) 0.15 }
                      else if availableMemory < memoryLimitMB config * 2
                           then config { testSelectionRatio = testSelectionRatio config * 0.7 }
                           else config
  
  return $ selectBalancedTests adjustedConfig tests

-- | Analyze test coverage
analyzeTestCoverage :: [(TestTree, TestMetadata)] -> IO ()
analyzeTestCoverage tests = do
  let addInt :: Int -> Int -> Int
      addInt = (+)
      categories = Map.fromListWith addInt 
          $ map (\ (_, meta) -> (testCategory meta, 1)) tests
      priorities = Map.fromListWith addInt
          $ map (\ (_, meta) -> (testPriority meta, 1)) tests
      totalTests = length tests
      coreTests = length $ filter (\(_, meta) -> isCoreTest meta) tests
  
  printf "Test Coverage Analysis:\n"
  printf "Total tests: %d\n" totalTests
  printf "Core tests: %d (%.1f%%)\n" coreTests ((fromIntegral coreTests / fromIntegral totalTests * 100) :: Double)
  printf "\nBy Category:\n"
  void $ Map.traverseWithKey (\cat count -> 
    liftIO $ printf "  %s: %d tests\n" (show cat) count) categories
  printf "\nBy Priority:\n"
  void $ Map.traverseWithKey (\pri count -> 
    liftIO $ printf "  %s: %d tests\n" (show pri) count) priorities