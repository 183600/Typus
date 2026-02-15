{-# LANGUAGE OverloadedStrings #-}

-- | Memory Optimization Example Module
-- This module demonstrates how to use the consolidated memory optimization
-- system with existing tests.
module TestSupport.MemoryOptimizationExample 
  ( -- * Example Usage
    exampleMemoryOptimizedTests
  , exampleHierarchicalTests
  , exampleEnvironmentAwareTests
    
    -- * Integration Examples
  , integrateWithExistingTests
  , setupMemoryOptimizedTesting
    
  ) where

import Test.Tasty (TestTree, testGroup, testCase)
import TestSupport.MemoryOptimizedTestIntegration 
  ( MemoryOptimizationLevel(..)
  , runMemoryOptimizedTests
  , createMemoryOptimizedTestSuite
  , runAutoOptimizedTests
  , printMemoryOptimizationSummary
  )
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , TestInfo(..)
  , createTestInfo
  , detectAvailableMemory
  , memoryOptimizedProperty
  )
import TestSupport.SmartTestSelector 
  ( SmartTestSelector(..)
  , createSmartTestSelector
  , selectTestsSmart
  )
import TestSupport.PrioritizedTestHierarchy 
  ( TestHierarchy(..)
  , PriorityLevel(..)
  , createTestHierarchy
  , getCriticalTests
  )
import TestSupport.GlobalQuickCheckOptimizer 
  ( genSmallString
  , genSmallList
  , genSmallInt
  , createMemoryEfficientProperty
  )
import Test.Tasty.QuickCheck (testProperty, property)

-- | Example test cases
exampleBasicTests :: [TestTree]
exampleBasicTests = 
  [ testCase "basic test 1" $ return ()
  , testCase "basic test 2" $ return ()
  , testCase "basic test 3" $ return ()
  ]

exampleQuickCheckTests :: [TestTree]
exampleQuickCheckTests = 
  [ testProperty "string property" $ property $ \s -> length s >= 0
  , testProperty "list property" $ property $ \xs -> length xs >= 0
  , testProperty "int property" $ property $ \n -> n >= 0 || n < 0
  ]

exampleIntegrationTests :: [TestTree]
exampleIntegrationTests = 
  [ testCase "integration test 1" $ return ()
  , testCase "integration test 2" $ return ()
  ]

-- | All example tests combined
allExampleTests :: [TestTree]
allExampleTests = 
  exampleBasicTests ++ exampleQuickCheckTests ++ exampleIntegrationTests

-- | Example 1: Basic memory optimization
exampleMemoryOptimizedTests :: IO ()
exampleMemoryOptimizedTests = do
  putStrLn "=== Example 1: Basic Memory Optimization ==="
  
  -- Run with different optimization levels
  putStrLn "Running with Conservative optimization (24MB):"
  runMemoryOptimizedTests Conservative allExampleTests
  
  putStrLn "\nRunning with Standard optimization (64MB):"
  runMemoryOptimizedTests Standard allExampleTests

-- | Example 2: Hierarchical test organization
exampleHierarchicalTests :: IO ()
exampleHierarchicalTests = do
  putStrLn "=== Example 2: Hierarchical Test Organization ==="
  
  -- Create test information with different priorities
  let criticalTest = createTestInfo "critical functionality" PriorityCritical 1 "Core" $ head exampleBasicTests
      essentialTest = createTestInfo "essential parsing" PriorityHigh 3 "Parser" $ exampleQuickCheckTests !! 0
      optionalTest = createTestInfo "optional feature" PriorityLow 5 "Feature" $ head exampleIntegrationTests
      
      testInfos = [criticalTest, essentialTest, optionalTest]
      
      -- Create hierarchy
      hierarchy = createTestHierarchy testInfos
  
  -- Show critical tests
  let critical = getCriticalTests hierarchy
  printf "Critical tests identified: %d\n" (length critical)
  mapM_ (\t -> printf "  - %s (weight: %d)\n" (testName t) (testMemoryWeight t)) critical

-- | Example 3: Environment-aware testing
exampleEnvironmentAwareTests :: IO ()
exampleEnvironmentAwareTests = do
  putStrLn "=== Example 3: Environment-Aware Testing ==="
  
  -- Show optimization summary for different environments
  putStrLn "Summary for UltraConservative (16MB):"
  printMemoryOptimizationSummary UltraConservative allExampleTests
  
  putStrLn "Summary for Standard (64MB):"
  printMemoryOptimizationSummary Standard allExampleTests
  
  putStrLn "Summary for Generous (128MB):"
  printMemoryOptimizationSummary Generous allExampleTests
  
  -- Run with auto-detection
  putStrLn "\nRunning with auto-detected optimization:"
  runAutoOptimizedTests allExampleTests

-- | Example 4: Integration with existing tests
integrateWithExistingTests :: [TestTree] -> IO ()
integrateWithExistingTests existingTests = do
  putStrLn "=== Example 4: Integration with Existing Tests ==="
  printf "Integrating %d existing tests with memory optimization\n" (length existingTests)
  
  -- Print summary before optimization
  putStrLn "Before optimization:"
  printMemoryOptimizationSummary Standard existingTests
  
  -- Create memory-optimized suite
  optimizedSuite <- createMemoryOptimizedTestSuite 
    Standard "Existing Tests Optimized" existingTests
  
  putStrLn "\nOptimized test suite created successfully!"
  printf "Optimized suite contains %d top-level test groups\n" 
    (length optimizedSuite)

-- | Example 5: Setup memory-optimized testing
setupMemoryOptimizedTesting :: [TestTree] -> IO ()
setupMemoryOptimizedTesting tests = do
  putStrLn "=== Example 5: Setup Memory-Optimized Testing ==="
  
  -- Detect available memory
  availableMemory <- detectAvailableMemory
  printf "Detected available memory: %dMB\n" availableMemory
  
  -- Determine optimal configuration
  let optimalLevel = case availableMemory of
        mb | mb <= 16 -> UltraConservative
        mb | mb <= 24 -> Conservative
        mb | mb <= 32 -> Moderate
        mb | mb <= 48 -> Balanced
        mb | mb <= 64 -> Standard
        mb | mb <= 128 -> Generous
        _ -> Unrestricted
  
  printf "Recommended optimization level: %s\n" (show optimalLevel)
  
  -- Setup and run
  putStrLn "\nSetting up memory-optimized testing environment..."
  printMemoryOptimizationSummary optimalLevel tests
  
  putStrLn "\nRunning tests with optimal configuration..."
  runMemoryOptimizedTests optimalLevel tests

-- | Memory-efficient QuickCheck property examples
memoryEfficientProperties :: [TestTree]
memoryEfficientProperties = 
  [ testProperty "efficient string property" $ 
      property $ \s -> let limitedS = take 8 s in length limitedS >= 0
  , testProperty "efficient list property" $ 
      property $ \xs -> let limitedXs = take 5 xs in length limitedXs >= 0
  , testProperty "efficient int property" $ 
      property $ \n -> let limitedN = abs n `mod` 100 in limitedN >= 0
  ]

-- | Complete example demonstrating all features
completeExample :: IO ()
completeExample = do
  putStrLn "=== Complete Memory Optimization Example ==="
  putStrLn "This example demonstrates all memory optimization features\n"
  
  -- Run all examples
  exampleMemoryOptimizedTests
  putStrLn "\n" ++ replicate 50 '=' ++ "\n"
  
  exampleHierarchicalTests
  putStrLn "\n" ++ replicate 50 '=' ++ "\n"
  
  exampleEnvironmentAwareTests
  putStrLn "\n" ++ replicate 50 '=' ++ "\n"
  
  integrateWithExistingTests allExampleTests
  putStrLn "\n" ++ replicate 50 '=' ++ "\n"
  
  setupMemoryOptimizedTesting allExampleTests
  
  putStrLn "\n=== Complete Example Finished ==="

-- | Main example entry point
mainExample :: IO ()
mainExample = do
  putStrLn "Memory Optimization Example - Choose an example:"
  putStrLn "1. Basic memory optimization"
  putStrLn "2. Hierarchical test organization"
  putStrLn "3. Environment-aware testing"
  putStrLn "4. Integration with existing tests"
  putStrLn "5. Setup memory-optimized testing"
  putStrLn "6. Complete example (all features)"
  putStrLn "\nRunning complete example..."
  completeExample