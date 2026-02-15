{-# LANGUAGE OverloadedStrings #-}

-- | Smart Test Selector Module
-- This module provides intelligent test selection based on available memory,
-- test priorities, and historical execution data.
module TestSupport.SmartTestSelector 
  ( -- * Smart Test Selection
    SmartTestSelector(..)
  , createSmartTestSelector
  , selectTestsSmart
  , selectTestsByMemoryTier
    
    -- * Test Category Management
  , TestCategory(..)
  , categorizeTests
  , getTestCategory
    
    -- * Memory-Aware Selection
  , MemoryAwareSelection(..)
  , createMemoryAwareSelection
  , applyMemoryConstraints
    
    -- * Test Execution Planning
  , ExecutionPlan(..)
  , createExecutionPlan
  , optimizeExecutionOrder
  ) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , MemoryTier(..)
  , TestInfo(..)
  , TestPriority(..)
  , createTestInfo
  , prioritizeTests
  , detectAvailableMemory
  , getMemoryTier
  , getMemoryConfig
  , withMemoryMonitoring
  , cleanupBetweenTests
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Data.List (sortBy, partition, take)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

-- | Test categories for intelligent selection
data TestCategory = 
    Core           -- ^ Core functionality tests
  | Parser         -- ^ Parser tests
  | Compiler       -- ^ Compiler tests
  | DependentTypes -- ^ Dependent type system tests
  | Ownership      -- ^ Ownership system tests
  | ErrorHandler   -- ^ Error handling tests
  | Integration    -- ^ Integration tests
  | Performance    -- ^ Performance tests
  | Regression     -- ^ Regression tests
  | EdgeCase       -- ^ Edge case tests
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Memory-aware selection strategy
data MemoryAwareSelection = MemoryAwareSelection
  { memoryConfig :: MemoryConfig
  , maxTestsPerCategory :: Int
  , prioritizeCritical :: Bool
  , enableAdaptiveSelection :: Bool
  , fallbackToEssential :: Bool
  } deriving (Show, Eq)

-- | Smart test selector configuration
data SmartTestSelector = SmartTestSelector
  { memoryAwareSelection :: MemoryAwareSelection
  , categoryPriorities :: [(TestCategory, Int)]
  , criticalTests :: [String]  -- ^ Test names that must run
  , excludedTests :: [String]  -- ^ Test names to exclude
  } deriving (Show, Eq)

-- | Test execution plan
data ExecutionPlan = ExecutionPlan
  { selectedTests :: [TestInfo]
  , estimatedMemoryUsage :: Int  -- ^ in MB
  , executionOrder :: [TestInfo]
  , cleanupStrategy :: String
  } deriving (Show, Eq)

-- | Create smart test selector
createSmartTestSelector :: MemoryConfig -> SmartTestSelector
createSmartTestSelector config = SmartTestSelector
  { memoryAwareSelection = createMemoryAwareSelection config
  , categoryPriorities = 
      [ (Core, 10)
      , (Parser, 8)
      , (Compiler, 7)
      , (ErrorHandler, 6)
      , (DependentTypes, 5)
      , (Ownership, 5)
      , (Integration, 4)
      , (Performance, 2)
      , (Regression, 3)
      , (EdgeCase, 1)
      ]
  , criticalTests = 
      [ "core basic functionality"
      , "parser basic parsing"
      , "compiler basic compilation"
      , "error handling basic"
      ]
  , excludedTests = 
      [ "performance regression"
      , "memory intensive"
      , "stress test"
      ]
  }

-- | Create memory-aware selection
createMemoryAwareSelection :: MemoryConfig -> MemoryAwareSelection
createMemoryAwareSelection config = MemoryAwareSelection
  { memoryConfig = config
  , maxTestsPerCategory = case memoryLimitMB config of
      mb | mb <= 16 -> 1   -- Ultra critical: 1 test per category
      mb | mb <= 24 -> 2   -- Critical: 2 tests per category
      mb | mb <= 32 -> 3   -- Emergency: 3 tests per category
      mb | mb <= 48 -> 5   -- Minimal: 5 tests per category
      mb | mb <= 64 -> 8   -- CI: 8 tests per category
      _ -> 10              -- Development: 10 tests per category
  , prioritizeCritical = True
  , enableAdaptiveSelection = True
  , fallbackToEssential = True
  }

-- | Get test category from test name or path
getTestCategory :: String -> TestCategory
getTestCategory testName
  | any (`isInfixOf` testName) ["core", "basic"] = Core
  | any (`isInfixOf` testName) ["parser", "parse", "syntax"] = Parser
  | any (`isInfixOf` testName) ["compiler", "compile", "ir"] = Compiler
  | any (`isInfixOf` testName) ["dependent", "type", "typus"] = DependentTypes
  | any (`isInfixOf` testName) ["ownership", "borrow", "move"] = Ownership
  | any (`isInfixOf` testName) ["error", "handler", "recovery"] = ErrorHandler
  | any (`isInfixOf` testName) ["integration", "endtoend", "e2e"] = Integration
  | any (`isInfixOf` testName) ["performance", "perf", "benchmark"] = Performance
  | any (`isInfixOf` testName) ["regression", "bug", "fix"] = Regression
  | otherwise = EdgeCase
  where
    isInfixOf = flip $ \x y -> x `elem` words y

-- | Categorize tests by their category
categorizeTests :: [TestInfo] -> [(TestCategory, [TestInfo])]
categorizeTests tests = 
  let categories = [Core .. EdgeCase]
      testsByCategory cat = (cat, filter (\t -> getTestCategory (testName t) == cat) tests)
  in map testsByCategory categories

-- | Check if test is critical
isCriticalTest :: SmartTestSelector -> TestInfo -> Bool
isCriticalTest selector test = 
  testName test `elem` criticalTests selector

-- | Check if test should be excluded
isExcludedTest :: SmartTestSelector -> TestInfo -> Bool
isExcludedTest selector test = 
  testName test `elem` excludedTests selector

-- | Select tests from a category based on memory constraints
selectFromCategory :: SmartTestSelector -> TestCategory -> [TestInfo] -> [TestInfo]
selectFromCategory selector category tests = 
  let maxPerCat = maxTestsPerCategory (memoryAwareSelection selector)
      prioritizeCrit = prioritizeCritical (memoryAwareSelection selector)
      (critical, nonCritical) = partition (isCriticalTest selector) tests
      selectedCritical = if prioritizeCrit then critical else []
      selectedNonCritical = take (maxPerCat - length selectedCritical) $ 
        sortBy (comparing testMemoryWeight) nonCritical
  in selectedCritical ++ selectedNonCritical

-- | Apply memory constraints to test selection
applyMemoryConstraints :: MemoryAwareSelection -> [TestInfo] -> [TestInfo]
applyMemoryConstraints selection tests = 
  let config = memoryConfig selection
      maxTotalTests = case memoryLimitMB config of
        mb | mb <= 16 -> 5   -- Ultra critical: 5 tests total
        mb | mb <= 24 -> 10  -- Critical: 10 tests total
        mb | mb <= 32 -> 15  -- Emergency: 15 tests total
        mb | mb <= 48 -> 25  -- Minimal: 25 tests total
        mb | mb <= 64 -> 40  -- CI: 40 tests total
        _ -> 60              -- Development: 60 tests total
  in take maxTotalTests tests

-- | Smart test selection based on memory and priorities
selectTestsSmart :: SmartTestSelector -> [TestInfo] -> [TestInfo]
selectTestsSmart selector tests = 
  let -- Filter out excluded tests
      filteredTests = filter (not . isExcludedTest selector) tests
      
      -- Categorize tests
      categorized = categorizeTests filteredTests
      
      -- Select from each category based on memory constraints
      selectedByCategory = concatMap (\(cat, ts) -> selectFromCategory selector cat ts) categorized
      
      -- Prioritize selected tests
      prioritized = prioritizeTests selectedByCategory
      
      -- Apply final memory constraints
      finalSelection = applyMemoryConstraints (memoryAwareSelection selector) prioritized
      
  in finalSelection

-- | Select tests based on memory tier
selectTestsByMemoryTier :: MemoryTier -> [TestInfo] -> [TestInfo]
selectTestsByMemoryTier tier tests = 
  let memoryConfig = case tier of
        UltraCritical -> getMemoryConfig 16
        Critical      -> getMemoryConfig 24
        Emergency     -> getMemoryConfig 32
        Minimal       -> getMemoryConfig 48
        CI            -> getMemoryConfig 64
        Development   -> getMemoryConfig 128
        Unlimited     -> getMemoryConfig 256
      
      selector = createSmartTestSelector memoryConfig
  in selectTestsSmart selector tests

-- | Estimate memory usage for tests
estimateMemoryUsage :: [TestInfo] -> Int
estimateMemoryUsage tests = 
  let memoryPerTest = 2  -- Base 2MB per test
      weightMultiplier = 0.5  -- 0.5MB per weight unit
      totalWeight = sum $ map testMemoryWeight tests
  in length tests * memoryPerTest + round (fromIntegral totalWeight * weightMultiplier)

-- | Optimize execution order for memory efficiency
optimizeExecutionOrder :: [TestInfo] -> [TestInfo]
optimizeExecutionOrder tests = 
  -- Sort by memory weight (lightest first) and priority
  sortBy (comparing (\t -> (testMemoryWeight t, testPriority t))) tests

-- | Create execution plan
createExecutionPlan :: SmartTestSelector -> [TestInfo] -> ExecutionPlan
createExecutionPlan selector allTests = 
  let selectedTests = selectTestsSmart selector allTests
      estimatedMemory = estimateMemoryUsage selectedTests
      executionOrder = optimizeExecutionOrder selectedTests
      cleanupStrategy = case memoryLimitMB (memoryConfig (memoryAwareSelection selector)) of
        mb | mb <= 24 -> "aggressive"  -- Aggressive cleanup for low memory
        mb | mb <= 48 -> "standard"    -- Standard cleanup for medium memory
        _ -> "minimal"                 -- Minimal cleanup for high memory
  in ExecutionPlan
      { selectedTests = selectedTests
      , estimatedMemoryUsage = estimatedMemory
      , executionOrder = executionOrder
      , cleanupStrategy = cleanupStrategy
      }

-- | Execute tests with memory-aware cleanup
executeWithMemoryAwareness :: ExecutionPlan -> IO ()
executeWithMemoryAwareness plan = do
  printf "Executing %d tests with estimated memory usage: %dMB\n" 
    (length (selectedTests plan)) (estimatedMemoryUsage plan)
  printf "Cleanup strategy: %s\n" (cleanupStrategy plan)
  
  -- Initial cleanup
  performGC
  
  -- Execute tests in optimized order
  mapM_ executeTest (executionOrder plan)
  
  where
    executeTest test = do
      printf "Executing: %s (priority: %s, weight: %d)\n" 
        (testName test) (show (testPriority test)) (testMemoryWeight test)
      
      -- Execute with memory monitoring
      withMemoryMonitoring $ do
        -- Here you would actually execute the test
        return ()
      
      -- Cleanup based on strategy
      case cleanupStrategy plan of
        "aggressive" -> do
          replicateM_ 3 performGC
        "standard" -> do
          performGC
        _ -> return ()

-- | Print selection report
printSelectionReport :: ExecutionPlan -> IO ()
printSelectionReport plan = do
  putStrLn "=== Smart Test Selection Report ==="
  printf "Selected tests: %d\n" (length (selectedTests plan))
  printf "Estimated memory usage: %dMB\n" (estimatedMemoryUsage plan)
  printf "Cleanup strategy: %s\n" (cleanupStrategy plan)
  putStrLn ""
  putStrLn "Test breakdown by category:"
  let categorized = categorizeTests (selectedTests plan)
  mapM_ (\(cat, tests) -> printf "  %s: %d tests\n" (show cat) (length tests)) categorized
  putStrLn ""
  putStrLn "Execution order (first 10 tests):"
  mapM_ (\t -> printf "  %s (weight: %d)\n" (testName t) (testMemoryWeight t)) 
    (take 10 (executionOrder plan))