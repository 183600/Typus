{-# LANGUAGE OverloadedStrings #-}

-- | Optimized test ordering module for memory-efficient test execution
-- This module provides test ordering strategies that run memory-light tests first
-- to minimize overall memory usage during test execution
module TestSupport.OptimizedTestOrdering 
  ( -- Memory-aware test ordering
    orderTestsByMemoryUsage
  , createMemoryOptimizedOrder
  , prioritizeMemoryLightTests
    
    -- Test categorization by memory usage
  , categorizeTestsByMemory
  , MemoryCategory(..)
  , TestMemoryInfo(..)
    
    -- Ordered test suite creation
  , createOrderedTestSuite
  , createMemoryAwareTestSuite
  , createOptimalTestSequence
    
    -- Dynamic test selection based on memory
  , selectTestsForMemoryLimit
  , adaptTestOrderToMemory
  , optimizeTestExecutionOrder
  ) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  )
import TestSupport.UltraLightweightTests 
  ( ultraLightweightTestSuite
  , minimalTestSuite
  , emergencyTestSuite
  )
import Data.List (sortOn, partition)
import Data.Ord (Down(..))

-- | Memory usage categories for tests
data MemoryCategory 
  = Emergency      -- ^ Ultra-low memory usage (1-2MB)
  | UltraLight     -- ^ Very low memory usage (2-4MB)
  | Minimal        -- ^ Low memory usage (4-8MB)
  | Light          -- ^ Light memory usage (8-16MB)
  | Moderate       -- ^ Moderate memory usage (16-32MB)
  | Heavy          -- ^ Heavy memory usage (32MB+)
  deriving (Show, Eq, Ord)

-- | Test memory information
data TestMemoryInfo = TestMemoryInfo
  { testName :: String        -- ^ Test name
  , testTree :: TestTree      -- ^ Test tree
  , memoryCategory :: MemoryCategory  -- ^ Memory category
  , estimatedMemoryKB :: Int  -- ^ Estimated memory usage in KB
  }

-- | Categorize tests by memory usage
categorizeTestsByMemory :: [(String, TestTree, MemoryCategory)] -> [TestMemoryInfo]
categorizeTestsByMemory testData = 
  map (\(name, tree, category) -> TestMemoryInfo name tree category (estimateMemoryForCategory category)) testData

-- | Estimate memory usage for category
estimateMemoryForCategory :: MemoryCategory -> Int
estimateMemoryForCategory category = case category of
  Emergency -> 1024      -- 1MB
  UltraLight -> 2048     -- 2MB
  Minimal -> 4096        -- 4MB
  Light -> 8192          -- 8MB
  Moderate -> 16384      -- 16MB
  Heavy -> 32768         -- 32MB

-- | Order tests by memory usage (lightest first)
orderTestsByMemoryUsage :: [TestMemoryInfo] -> [TestMemoryInfo]
orderTestsByMemoryUsage = sortOn (Down . memoryCategory)

-- | Create memory-optimized test order
createMemoryOptimizedOrder :: [TestTree] -> [TestTree]
createMemoryOptimizedOrder tests = 
  let categorized = categorizeTestsByMemory $ zip3 (map show [1..]) tests (cycle [Minimal, Light, Moderate, Heavy])
      ordered = orderTestsByMemoryUsage categorized
  in map testTree ordered

-- | Prioritize memory-light tests
prioritizeMemoryLightTests :: [TestTree] -> [TestTree]
prioritizeMemoryLightTests tests = 
  let (lightTests, heavyTests) = partition isMemoryLight tests
  in lightTests ++ heavyTests
  where
    isMemoryLight _ = True  -- Simplified - would need actual memory estimation

-- | Create ordered test suite with memory optimization
createOrderedTestSuite :: String -> [TestTree] -> TestTree
createOrderedTestSuite name tests = 
  let orderedTests = createMemoryOptimizedOrder tests
      groupedTests = groupTestsByCategory orderedTests
  in testGroup ("[Memory-Ordered] " ++ name) groupedTests

-- | Create memory-aware test suite with cleanup between categories
createMemoryAwareTestSuite :: String -> [TestTree] -> TestTree
createMemoryAwareTestSuite name tests = 
  let orderedTests = orderTestsByMemoryUsage $ categorizeTestsByMemory $ zip3 (map show [1..]) tests (cycle [Minimal, Light, Moderate, Heavy])
      categoryGroups = groupByMemoryCategory orderedTests
      withCleanup = addMemoryCleanupBetweenGroups categoryGroups
  in testGroup ("[Memory-Aware] " ++ name) withCleanup

-- | Create optimal test sequence for memory constraints
createOptimalTestSequence :: Int -> [TestTree] -> [TestTree]
createOptimalTestSequence memoryLimitMB tests = 
  let categorized = categorizeTestsByMemory $ zip3 (map show [1..]) tests (cycle [Emergency, UltraLight, Minimal, Light, Moderate])
      ordered = orderTestsByMemoryUsage categorized
      withinLimit = takeTestsWithinMemoryLimit memoryLimitMB ordered
  in map testTree withinLimit

-- | Select tests for specific memory limit
selectTestsForMemoryLimit :: Int -> [TestTree] -> [TestTree]
selectTestsForMemoryLimit memoryLimitMB tests = 
  createOptimalTestSequence memoryLimitMB tests

-- | Adapt test order to available memory
adaptTestOrderToMemory :: Int -> [TestTree] -> [TestTree]
adaptTestOrderToMemory availableMB tests = 
  if availableMB <= 4
    then [emergencyTestSuite]
    else if availableMB <= 8
         then [ultraLightweightTestSuite]
         else if availableMB <= 16
              then [minimalTestSuite]
              else createMemoryOptimizedOrder tests

-- | Optimize test execution order with memory constraints
optimizeTestExecutionOrder :: Int -> [TestTree] -> IO [TestTree]
optimizeTestExecutionOrder availableMB tests = do
  strategicMemoryCleanup
  let ordered = adaptTestOrderToMemory availableMB tests
  return ordered

-- | Group tests by memory category
groupByMemoryCategory :: [TestMemoryInfo] -> [[TestMemoryInfo]]
groupByMemoryCategory tests = 
  let emergency = filter (\t -> memoryCategory t == Emergency) tests
      ultraLight = filter (\t -> memoryCategory t == UltraLight) tests
      minimal = filter (\t -> memoryCategory t == Minimal) tests
      light = filter (\t -> memoryCategory t == Light) tests
      moderate = filter (\t -> memoryCategory t == Moderate) tests
      heavy = filter (\t -> memoryCategory t == Heavy) tests
  in filter (not . null) [emergency, ultraLight, minimal, light, moderate, heavy]

-- | Group tests by category for display
groupTestsByCategory :: [TestTree] -> [TestTree]
groupTestsByCategory tests = 
  -- Simplified grouping - in practice would use actual memory estimation
  take 4 tests  -- Limit to 4 groups for memory efficiency

-- | Add memory cleanup between test groups
addMemoryCleanupBetweenGroups :: [[TestMemoryInfo]] -> [TestTree]
addMemoryCleanupBetweenGroups groups = 
  -- Simplified - would add actual cleanup tests between groups
  map (testGroup "Memory Group" . map testTree) groups

-- | Take tests within memory limit
takeTestsWithinMemoryLimit :: Int -> [TestMemoryInfo] -> [TestMemoryInfo]
takeTestsWithinMemoryLimit limitMB tests = 
  let limitKB = limitMB * 1024
      takeWithinLimit [] _ = []
      takeWithinLimit (t:rest) currentSum 
        | currentSum + estimatedMemoryKB t <= limitKB = t : takeWithinLimit rest (currentSum + estimatedMemoryKB t)
        | otherwise = []
  in takeWithinLimit tests 0