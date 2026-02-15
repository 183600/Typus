{-# LANGUAGE OverloadedStrings #-}

-- | Memory Optimized Test Integration Module
-- This module provides a unified interface for all memory optimization
-- features and demonstrates how to use them together effectively.
module TestSupport.MemoryOptimizedTestIntegration 
  ( -- * Main Integration Interface
    runMemoryOptimizedTests
  , createMemoryOptimizedTestSuite
  , initializeMemoryOptimizations
    
    -- * Memory Configuration
    MemoryOptimizationLevel(..)
    , configureMemoryOptimization
    , detectEnvironment
    
    -- * Quick Integration Functions
    , quickMemoryOptimizedTest
    , quickMemoryOptimizedTests
    , runTestsInMemoryConstrainedMode
    
    -- * Reporting and Monitoring
    , MemoryOptimizationReport(..)
    , generateOptimizationReport
    , printMemoryOptimizationSummary
    
  ) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , MemoryTier(..)
  , detectAvailableMemory
  , getMemoryTier
  , getMemoryConfig
  , withMemoryOptimization
  , createMemoryOptimizedTestSuite
  )
import TestSupport.SmartTestSelector 
  ( SmartTestSelector(..)
  , TestInfo(..)
  , createTestInfo
  , createSmartTestSelector
  , selectTestsSmart
  , createExecutionPlan
  )
import TestSupport.GlobalQuickCheckOptimizer 
  ( globalQuickCheckConfig
  , initializeGlobalQuickCheckOptimization
  , globallyOptimizeTest
  , applyGlobalQuickCheckOptimization
  )
import TestSupport.EnhancedMemoryTestRunner 
  ( EnhancedTestRunner(..)
  , createEnhancedTestRunner
  , executeTestsWithMemoryControl
  , aggressiveGCStrategy
  , conservativeGCStrategy
  )
import TestSupport.PrioritizedTestHierarchy 
  ( TestHierarchy(..)
  , PriorityLevel(..)
  , createTestHierarchy
  , createExecutionPlan
  , executeHierarchyPlan
  , createDefaultTestHierarchy
  )
import System.Mem (performGC)
import Control.Monad (when, replicateM_)
import System.Environment (getEnvironment)
import Data.Maybe (isJust, fromMaybe)
import Text.Printf (printf)
import Data.List (take)
import System.IO.Unsafe (unsafePerformIO)

-- | Memory optimization levels
data MemoryOptimizationLevel = 
    UltraConservative  -- ^ 16MB - Emergency mode
  | Conservative       -- ^ 24MB - Critical environment
  | Moderate          -- ^ 32MB - Emergency environment
  | Balanced          -- ^ 48MB - Minimal environment
  | Standard          -- ^ 64MB - CI/CD environment
  | Generous          -- ^ 128MB - Development environment
  | Unrestricted      -- ^ No memory restrictions
  deriving (Show, Eq)

-- | Memory optimization report
data MemoryOptimizationReport = MemoryOptimizationReport
  { optimizationLevel :: MemoryOptimizationLevel
  , memoryLimit :: Int  -- ^ MB
  , testsSelected :: Int
  , testsTotal :: Int
  , estimatedMemoryUsage :: Int  -- ^ MB
  , optimizationFeatures :: [String]
  , performanceImpact :: String
  } deriving (Show, Eq)

-- | Detect environment type
detectEnvironment :: IO MemoryOptimizationLevel
detectEnvironment = do
  env <- getEnvironment
  let isCI = isJust (lookup "CI" env) || isJust (lookup "CONTINUOUS_INTEGRATION" env)
      isGitHubActions = isJust (lookup "GITHUB_ACTIONS" env)
      isTravis = isJust (lookup "TRAVIS" env)
      isCircleCI = isJust (lookup "CIRCLECI" env)
      isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
      isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
      memoryOverride = lookup "TYPUS_MEMORY_LEVEL" env
      
  case memoryOverride of
    Just "ultra" -> return UltraConservative
    Just "conservative" -> return Conservative
    Just "moderate" -> return Moderate
    Just "balanced" -> return Balanced
    Just "standard" -> return Standard
    Just "generous" -> return Generous
    Just "unrestricted" -> return Unrestricted
    _ -> case (isEmergency, isUltraOptimized, isCI || isGitHubActions || isTravis || isCircleCI) of
      (True, _, _) -> return UltraConservative
      (_, True, _) -> return Conservative
      (_, _, True) -> return Standard
      _ -> return Generous

-- | Convert optimization level to memory configuration
optimizationLevelToConfig :: MemoryOptimizationLevel -> MemoryConfig
optimizationLevelToConfig level = case level of
  UltraConservative -> getMemoryConfig 16
  Conservative      -> getMemoryConfig 24
  Moderate          -> getMemoryConfig 32
  Balanced          -> getMemoryConfig 48
  Standard          -> getMemoryConfig 64
  Generous          -> getMemoryConfig 128
  Unrestricted      -> getMemoryConfig 256

-- | Configure memory optimization
configureMemoryOptimization :: MemoryOptimizationLevel -> IO MemoryConfig
configureMemoryOptimization level = do
  let config = optimizationLevelToConfig level
  printf "Configuring memory optimization for level: %s (%dMB limit)\n" 
    (show level) (memoryLimitMB config)
  return config

-- | Initialize all memory optimizations
initializeMemoryOptimizations :: MemoryOptimizationLevel -> IO MemoryConfig
initializeMemoryOptimizations level = do
  printf "Initializing memory optimizations for level: %s\n" (show level)
  
  -- Configure memory optimization
  config <- configureMemoryOptimization level
  
  -- Initialize global QuickCheck optimization
  initializeGlobalQuickCheckOptimization
  
  -- Initial garbage collection
  replicateM_ 3 performGC
  
  printf "Memory optimizations initialized successfully\n"
  return config

-- | Create memory-optimized test suite
createMemoryOptimizedTestSuite :: MemoryOptimizationLevel -> String -> [TestTree] -> IO TestTree
createMemoryOptimizedTestSuite level name tests = do
  config <- configureMemoryOptimization level
  
  -- Create test hierarchy
  let hierarchy = createDefaultTestHierarchy tests
  
  -- Create execution plan
  let plan = createExecutionPlan hierarchy config
  
  -- Apply optimizations to selected tests
  let optimizedTests = map applyGlobalQuickCheckOptimization 
                         $ map (withMemoryOptimization config) 
                         $ map testTree 
                         $ planExecutionOrder plan
  
  return $ testGroup (name ++ " [" ++ show level ++ "]") optimizedTests

-- | Quick memory-optimized test for single test
quickMemoryOptimizedTest :: MemoryOptimizationLevel -> TestTree -> IO TestTree
quickMemoryOptimizedTest level test = do
  config <- configureMemoryOptimization level
  return $ applyGlobalQuickCheckOptimization $ withMemoryOptimization config test

-- | Quick memory-optimized tests for multiple tests
quickMemoryOptimizedTests :: MemoryOptimizationLevel -> [TestTree] -> IO [TestTree]
quickMemoryOptimizedTests level tests = do
  config <- configureMemoryOptimization level
  return $ map (applyGlobalQuickCheckOptimization . withMemoryOptimization config) tests

-- | Run tests in memory-constrained mode
runTestsInMemoryConstrainedMode :: MemoryOptimizationLevel -> [TestTree] -> IO ()
runTestsInMemoryConstrainedMode level tests = do
  printf "Running tests in memory-constrained mode: %s\n" (show level)
  
  -- Initialize optimizations
  config <- initializeMemoryOptimizations level
  
  -- Create enhanced test runner
  let gcStrategy = case level of
        UltraConservative -> aggressiveGCStrategy
        Conservative      -> aggressiveGCStrategy
        Moderate          -> aggressiveGCStrategy
        _                 -> conservativeGCStrategy
  
  runner <- createEnhancedTestRunner config gcStrategy
  
  -- Execute tests with memory control
  executeTestsWithMemoryControl runner tests

-- | Run memory-optimized tests (main interface)
runMemoryOptimizedTests :: MemoryOptimizationLevel -> [TestTree] -> IO ()
runMemoryOptimizedTests level tests = do
  printf "=== Memory Optimized Test Execution ===\n"
  printf "Optimization level: %s\n" (show level)
  printf "Total tests provided: %d\n" (length tests)
  
  -- Detect if we should use constrained mode
  env <- getEnvironment
  let forceConstrained = isJust (lookup "FORCE_CONSTRAINED_MODE" env)
  
  if forceConstrained || level `elem` [UltraConservative, Conservative, Moderate]
    then runTestsInMemoryConstrainedMode level tests
    else do
      -- Standard optimized execution
      optimizedSuite <- createMemoryOptimizedTestSuite level "Memory Optimized Tests" tests
      defaultMain optimizedSuite

-- | Generate optimization report
generateOptimizationReport :: MemoryOptimizationLevel -> [TestTree] -> IO MemoryOptimizationReport
generateOptimizationReport level tests = do
  let config = optimizationLevelToConfig level
      hierarchy = createDefaultTestHierarchy tests
      plan = createExecutionPlan hierarchy config
      
      selectedCount = length $ planSelectedTests plan
      totalCount = length tests
      estimatedMemory = planEstimatedMemory plan
      
      features = case level of
        UltraConservative -> 
          [ "Emergency memory mode"
          , "Ultra-aggressive GC"
          , "Minimal test selection (1%)"
          , "String length limit: 3"
          , "List length limit: 2"
          ]
        Conservative ->
          [ "Critical memory mode"
          , "Aggressive GC"
          , "Essential test selection (2%)"
          , "String length limit: 5"
          , "List length limit: 3"
          ]
        Moderate ->
          [ "Emergency memory mode"
          , "Standard GC"
          , "Important test selection (5%)"
          , "String length limit: 8"
          , "List length limit: 5"
          ]
        Balanced ->
          [ "Minimal memory mode"
          , "Standard GC"
          , "Balanced test selection (10%)"
          , "String length limit: 12"
          , "List length limit: 8"
          ]
        Standard ->
          [ "CI/CD memory mode"
          , "Optimized GC"
          , "Comprehensive test selection (15%)"
          , "String length limit: 16"
          , "List length limit: 12"
          ]
        Generous ->
          [ "Development memory mode"
          , "Standard GC"
          , "Extensive test selection (30%)"
          , "String length limit: 32"
          , "List length limit: 20"
          ]
        Unrestricted ->
          [ "Unrestricted memory mode"
          , "Standard GC"
          , "Full test selection"
          , "No string length limits"
          , "No list length limits"
          ]
      
      performanceImpact = case level of
        UltraConservative -> "Very high memory savings, significantly slower execution"
        Conservative      -> "High memory savings, slower execution"
        Moderate          -> "Good memory savings, moderate performance impact"
        Balanced          -> "Moderate memory savings, minimal performance impact"
        Standard          -> "Low memory savings, minimal performance impact"
        Generous          -> "Minimal memory savings, no performance impact"
        Unrestricted      -> "No memory savings, optimal performance"
  
  return $ MemoryOptimizationReport
    { optimizationLevel = level
    , memoryLimit = memoryLimitMB config
    , testsSelected = selectedCount
    , testsTotal = totalCount
    , estimatedMemoryUsage = estimatedMemory
    , optimizationFeatures = features
    , performanceImpact = performanceImpact
    }

-- | Print memory optimization summary
printMemoryOptimizationSummary :: MemoryOptimizationLevel -> [TestTree] -> IO ()
printMemoryOptimizationSummary level tests = do
  report <- generateOptimizationReport level tests
  
  putStrLn "=== Memory Optimization Summary ==="
  printf "Optimization level: %s\n" (show $ optimizationLevel report)
  printf "Memory limit: %dMB\n" (memoryLimit report)
  printf "Tests selected: %d/%d (%.1f%%)\n" 
    (testsSelected report) 
    (testsTotal report)
    (fromIntegral (testsSelected report) / fromIntegral (testsTotal report) * 100)
  printf "Estimated memory usage: %dMB\n" (estimatedMemoryUsage report)
  printf "Performance impact: %s\n" (performanceImpact report)
  putStrLn ""
  putStrLn "Optimization features:"
  mapM_ (\f -> printf "  ✓ %s\n" f) (optimizationFeatures report)
  putStrLn ""

-- | Auto-configure and run tests based on environment
runAutoOptimizedTests :: [TestTree] -> IO ()
runAutoOptimizedTests tests = do
  printf "Auto-configuring memory optimizations based on environment...\n"
  
  -- Detect environment and configure accordingly
  level <- detectEnvironment
  
  -- Print summary
  printMemoryOptimizationSummary level tests
  
  -- Run tests
  runMemoryOptimizedTests level tests

-- | Main entry point for memory-optimized testing
mainMemoryOptimized :: [TestTree] -> IO ()
mainMemoryOptimized tests = do
  putStrLn "Starting memory-optimized test execution..."
  runAutoOptimizedTests tests