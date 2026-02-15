{-# LANGUAGE OverloadedStrings #-}

-- | Prioritized Test Hierarchy Module
-- This module provides a hierarchical organization of tests based on
-- priority and memory requirements for memory-constrained environments.
module TestSupport.PrioritizedTestHierarchy 
  ( -- * Test Hierarchy Structure
    TestHierarchy(..)
  , TestLevel(..)
  , createTestHierarchy
  , buildTestHierarchy
    
    -- * Priority-Based Test Selection
  , PriorityLevel(..)
  , selectTestsByPriority
  , selectTestsByMemoryLevel
  , getCriticalTests
  , getEssentialTests
    
    -- * Memory-Constrained Execution
  , MemoryConstrainedPlan(..)
  , createExecutionPlan
  , executeHierarchyPlan
  , optimizeForMemoryConstraints
    
    -- * Test Hierarchy Management
  , addTestToHierarchy
  , removeTestFromHierarchy
  , updateTestPriority
  , getHierarchyStatistics
    
  ) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , MemoryTier(..)
  , TestPriority(..)
  , TestInfo(..)
  , createTestInfo
  , detectAvailableMemory
  , getMemoryTier
  , getMemoryConfig
  )
import TestSupport.SmartTestSelector 
  ( SmartTestSelector(..)
  , TestCategory(..)
  , createSmartTestSelector
  , selectTestsSmart
  , createExecutionPlan
  )
import TestSupport.EnhancedMemoryTestRunner 
  ( EnhancedTestRunner(..)
  , createEnhancedTestRunner
  , executeTestsWithMemoryControl
  , aggressiveGCStrategy
  , conservativeGCStrategy
  )
import Data.List (sortBy, partition, take, drop)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)
import Control.Monad (when, replicateM_)

-- | Test priority levels for hierarchy
data PriorityLevel = 
    P0_Critical     -- ^ Must run - core functionality
  | P1_Essential    -- ^ Should run - important features
  | P2_Important    -- ^ Nice to run - secondary features
  | P3_Optional     -- ^ Can skip - edge cases
  | P4_Luxury       -- ^ Skip first - comprehensive tests
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Test hierarchy levels
data TestLevel = 
    HierarchyRoot        -- ^ Root of hierarchy
  | CategoryLevel TestCategory  -- ^ Category grouping
  | PriorityLevel PriorityLevel  -- ^ Priority grouping
  | MemoryLevel MemoryTier      -- ^ Memory-based grouping
  | TestLeaf TestInfo           -- ^ Individual test
  deriving (Show, Eq)

-- | Test hierarchy structure
data TestHierarchy = TestHierarchy
  { hierarchyLevels :: Map TestLevel [TestLevel]  -- ^ Parent to children mapping
  , hierarchyTests :: Map TestLevel TestInfo       -- ^ Level to test info mapping
  , hierarchyStats :: HierarchyStats               -- ^ Statistics
  } deriving (Show, Eq)

-- | Hierarchy statistics
data HierarchyStats = HierarchyStats
  { totalTests :: Int
  , testsByPriority :: Map PriorityLevel Int
  , testsByCategory :: Map TestCategory Int
  , testsByMemoryTier :: Map MemoryTier Int
  , estimatedMemoryUsage :: Int  -- ^ in MB
  } deriving (Show, Eq)

-- | Memory-constrained execution plan
data MemoryConstrainedPlan = MemoryConstrainedPlan
  { planMemoryConfig :: MemoryConfig
  , planMemoryTier :: MemoryTier
  , planSelectedTests :: [TestInfo]
  , planExecutionOrder :: [TestInfo]
  , planEstimatedTime :: Int  -- ^ seconds
  , planEstimatedMemory :: Int  -- ^ MB
  , planPriorityCutoff :: PriorityLevel
  } deriving (Show, Eq)

-- | Create empty test hierarchy
createEmptyHierarchy :: TestHierarchy
createEmptyHierarchy = TestHierarchy
  { hierarchyLevels = Map.empty
  , hierarchyTests = Map.empty
  , hierarchyStats = HierarchyStats Map.empty Map.empty Map.empty 0 0
  }

-- | Convert TestPriority to PriorityLevel
toPriorityLevel :: TestPriority -> PriorityLevel
toPriorityLevel PriorityCritical = P0_Critical
toPriorityLevel PriorityHigh = P1_Essential
toPriorityLevel PriorityMedium = P2_Important
toPriorityLevel PriorityLow = P3_Optional

-- | Convert PriorityLevel to TestPriority
fromPriorityLevel :: PriorityLevel -> TestPriority
fromPriorityLevel P0_Critical = PriorityCritical
fromPriorityLevel P1_Essential = PriorityHigh
fromPriorityLevel P2_Important = PriorityMedium
fromPriorityLevel P3_Optional = PriorityLow
fromPriorityLevel P4_Luxury = PriorityLow

-- | Create test hierarchy from test list
createTestHierarchy :: [TestInfo] -> TestHierarchy
createTestHierarchy tests = buildTestHierarchy createEmptyHierarchy tests

-- | Build test hierarchy incrementally
buildTestHierarchy :: TestHierarchy -> [TestInfo] -> TestHierarchy
buildTestHierarchy hierarchy tests = 
  let withTests = foldl addTestToHierarchy hierarchy tests
  in withTests { hierarchyStats = calculateHierarchyStats withTests }

-- | Add test to hierarchy
addTestToHierarchy :: TestHierarchy -> TestInfo -> TestHierarchy
addTestToHierarchy hierarchy test = 
  let priority = toPriorityLevel (testPriority test)
      category = getTestCategory (testName test)
      memoryTier = getMemoryTier (testMemoryWeight test * 2)  -- Estimate memory tier
      priorityLevel = PriorityLevel priority
      categoryLevel = CategoryLevel category
      memoryLevel = MemoryLevel memoryTier
      testLeaf = TestLeaf test
      
      -- Update levels mapping
      levels = hierarchyLevels hierarchy
      levels' = Map.insertWith (++) HierarchyRoot [categoryLevel] $
                Map.insertWith (++) categoryLevel [priorityLevel] $
                Map.insertWith (++) priorityLevel [memoryLevel] $
                Map.insertWith (++) memoryLevel [testLeaf] levels
      
      -- Update tests mapping
      tests = hierarchyTests hierarchy
      tests' = Map.insert testLeaf test tests
      
  in hierarchy { hierarchyLevels = levels', hierarchyTests = tests' }

-- | Remove test from hierarchy
removeTestFromHierarchy :: TestHierarchy -> String -> TestHierarchy
removeTestFromHierarchy hierarchy testName = 
  let -- Find and remove the test
      testsToRemove = Map.filter (\t -> testName t == testName) (hierarchyTests hierarchy)
      testIds = Map.keys testsToRemove
      
      -- Remove from levels mapping
      levels' = Map.map (filter (`notElem` testIds)) (hierarchyLevels hierarchy)
      
      -- Remove from tests mapping
      tests' = Map.filterWithKey (\k _ -> k `notElem` testIds) (hierarchyTests hierarchy)
      
  in hierarchy { hierarchyLevels = levels', hierarchyTests = tests' }

-- | Update test priority
updateTestPriority :: TestHierarchy -> String -> PriorityLevel -> TestHierarchy
updateTestPriority hierarchy testName newPriority = 
  let -- Find the test
      testMaybe = Map.lookup (TestLeaf undefined) $ Map.filter (\t -> testName t == testName) (hierarchyTests hierarchy)
  in case testMaybe of
    Just test -> 
      let updatedTest = test { testPriority = fromPriorityLevel newPriority }
          hierarchyWithoutTest = removeTestFromHierarchy hierarchy testName
      in addTestToHierarchy hierarchyWithoutTest updatedTest
    Nothing -> hierarchy

-- | Calculate hierarchy statistics
calculateHierarchyStats :: TestHierarchy -> HierarchyStats
calculateHierarchyStats hierarchy = 
  let allTests = Map.elems (hierarchyTests hierarchy)
      total = length allTests
      
      testsByPriority' = Map.fromListWith (+)
        [(toPriorityLevel (testPriority t), 1) | t <- allTests]
      
      testsByCategory' = Map.fromListWith (+)
        [(getTestCategory (testName t), 1) | t <- allTests]
      
      testsByMemoryTier' = Map.fromListWith (+)
        [(getMemoryTier (testMemoryWeight t * 2), 1) | t <- allTests]
      
      estimatedMemory = sum [testMemoryWeight t * 2 | t <- allTests]
      
  in HierarchyStats
    { totalTests = total
    , testsByPriority = testsByPriority'
    , testsByCategory = testsByCategory'
    , testsByMemoryTier = testsByMemoryTier'
    , estimatedMemoryUsage = estimatedMemory
    }

-- | Get tests by priority level
getTestsByPriority :: TestHierarchy -> PriorityLevel -> [TestInfo]
getTestsByPriority hierarchy priority = 
  let priorityLevel = PriorityLevel priority
      memoryLevels = Map.findWithDefault [] priorityLevel (hierarchyLevels hierarchy)
      testLeaves = concatMap (\ml -> Map.findWithDefault [] ml (hierarchyLevels hierarchy)) memoryLevels
  in Map.findWithDefault [] (TestLeaf undefined) (hierarchyTests hierarchy)

-- | Get critical tests (P0)
getCriticalTests :: TestHierarchy -> [TestInfo]
getCriticalTests hierarchy = getTestsByPriority hierarchy P0_Critical

-- | Get essential tests (P0 + P1)
getEssentialTests :: TestHierarchy -> [TestInfo]
getEssentialTests hierarchy = 
  let critical = getTestsByPriority hierarchy P0_Critical
      essential = getTestsByPriority hierarchy P1_Essential
  in critical ++ essential

-- | Select tests by priority level
selectTestsByPriority :: TestHierarchy -> PriorityLevel -> [TestInfo]
selectTestsByPriority hierarchy maxPriority = 
  let allPriorities = [P0_Critical .. maxPriority]
  in concatMap (getTestsByPriority hierarchy) allPriorities

-- | Select tests by memory tier
selectTestsByMemoryLevel :: TestHierarchy -> MemoryTier -> [TestInfo]
selectTestsByMemoryLevel hierarchy maxTier = 
  let allTests = Map.elems (hierarchyTests hierarchy)
      suitableTests = filter (\t -> getMemoryTier (testMemoryWeight t * 2) <= maxTier) allTests
  in sortBy (comparing (testPriority)) suitableTests

-- | Create memory-constrained execution plan
createExecutionPlan :: TestHierarchy -> MemoryConfig -> MemoryConstrainedPlan
createExecutionPlan hierarchy memoryConfig = 
  let memoryTier = getMemoryTier (memoryLimitMB memoryConfig)
      
      -- Select tests based on memory constraints
      baseTests = case memoryTier of
        UltraCritical -> getCriticalTests hierarchy
        Critical      -> getEssentialTests hierarchy
        Emergency     -> selectTestsByPriority hierarchy P2_Important
        Minimal       -> selectTestsByPriority hierarchy P3_Optional
        CI            -> selectTestsByPriority hierarchy P4_Luxury
        Development   -> Map.elems (hierarchyTests hierarchy)
        Unlimited     -> Map.elems (hierarchyTests hierarchy)
      
      -- Further filter by memory constraints
      memoryFiltered = selectTestsByMemoryLevel hierarchy memoryTier
      
      -- Take intersection of base tests and memory-filtered tests
      selectedTests = filter (`elem` baseTests) memoryFiltered
      
      -- Order by priority and memory weight
      executionOrder = sortBy (comparing (\t -> (testPriority t, testMemoryWeight t))) selectedTests
      
      -- Estimate resources
      estimatedTime = length selectedTests * 2  -- 2 seconds per test estimate
      estimatedMemory = sum [testMemoryWeight t * 2 | t <- selectedTests]
      
      -- Determine priority cutoff
      priorityCutoff = if null selectedTests
                      then P4_Luxury
                      else maximum $ map (toPriorityLevel . testPriority) selectedTests
      
  in MemoryConstrainedPlan
    { planMemoryConfig = memoryConfig
    , planMemoryTier = memoryTier
    , planSelectedTests = selectedTests
    , planExecutionOrder = executionOrder
    , planEstimatedTime = estimatedTime
    , planEstimatedMemory = estimatedMemory
    , planPriorityCutoff = priorityCutoff
    }

-- | Optimize for memory constraints
optimizeForMemoryConstraints :: TestHierarchy -> Int -> MemoryConstrainedPlan
optimizeForMemoryConstraints hierarchy availableMemory = 
  let memoryConfig = getMemoryConfig availableMemory
  in createExecutionPlan hierarchy memoryConfig

-- | Execute hierarchy plan
executeHierarchyPlan :: MemoryConstrainedPlan -> IO ()
executeHierarchyPlan plan = do
  printf "Executing test hierarchy plan for %dMB memory tier (%s)\n"
    (memoryLimitMB $ planMemoryConfig plan) (show $ planMemoryTier plan)
  printf "Selected %d tests (priority cutoff: %s)\n"
    (length $ planSelectedTests plan) (show $ planPriorityCutoff plan)
  printf "Estimated execution time: %d seconds, memory usage: %dMB\n"
    (planEstimatedTime plan) (planEstimatedMemory plan)
  
  -- Create enhanced test runner and execute
  let gcStrategy = case planMemoryTier plan of
        UltraCritical -> aggressiveGCStrategy
        Critical      -> aggressiveGCStrategy
        Emergency     -> aggressiveGCStrategy
        _ -> conservativeGCStrategy
  
  runner <- createEnhancedTestRunner (planMemoryConfig plan) gcStrategy
  let testTrees = map testTree (planExecutionOrder plan)
  
  executeTestsWithMemoryControl runner testTrees

-- | Get hierarchy statistics
getHierarchyStatistics :: TestHierarchy -> HierarchyStats
getHierarchyStatistics hierarchy = hierarchyStats hierarchy

-- | Print hierarchy report
printHierarchyReport :: TestHierarchy -> IO ()
printHierarchyReport hierarchy = do
  let stats = hierarchyStats hierarchy
  putStrLn "=== Test Hierarchy Report ==="
  printf "Total tests: %d\n" (totalTests stats)
  printf "Estimated memory usage: %dMB\n" (estimatedMemoryUsage stats)
  putStrLn ""
  
  putStrLn "Tests by priority:"
  mapM_ (\(p, count) -> printf "  %s: %d tests\n" (show p) count) 
    (Map.toList $ testsByPriority stats)
  putStrLn ""
  
  putStrLn "Tests by category:"
  mapM_ (\(c, count) -> printf "  %s: %d tests\n" (show c) count)
    (Map.toList $ testsByCategory stats)
  putStrLn ""
  
  putStrLn "Tests by memory tier:"
  mapM_ (\(t, count) -> printf "  %s: %d tests\n" (show t) count)
    (Map.toList $ testsByMemoryTier stats)

-- | Print execution plan report
printExecutionPlanReport :: MemoryConstrainedPlan -> IO ()
printExecutionPlanReport plan = do
  putStrLn "=== Memory-Constrained Execution Plan ==="
  printf "Memory tier: %s (%dMB)\n" 
    (show $ planMemoryTier plan) (memoryLimitMB $ planMemoryConfig plan)
  printf "Priority cutoff: %s\n" (show $ planPriorityCutoff plan)
  printf "Selected tests: %d\n" (length $ planSelectedTests plan)
  printf "Estimated time: %d seconds\n" (planEstimatedTime plan)
  printf "Estimated memory: %dMB\n" (planEstimatedMemory plan)
  putStrLn ""
  
  putStrLn "Execution order (first 10 tests):"
  mapM_ (\t -> printf "  %s (priority: %s, weight: %d)\n" 
    (testName t) (show $ testPriority t) (testMemoryWeight t))
    (take 10 $ planExecutionOrder plan)

-- | Create default test hierarchy for the project
createDefaultTestHierarchy :: [TestTree] -> TestHierarchy
createDefaultTestHierarchy testTrees = 
  -- Convert TestTree to TestInfo with default priorities
  let testInfos = zipWith (\i tree -> 
        createTestInfo ("test-" ++ show i) PriorityMedium 5 "General" tree)
        [1..] testTrees
  in createTestHierarchy testInfos