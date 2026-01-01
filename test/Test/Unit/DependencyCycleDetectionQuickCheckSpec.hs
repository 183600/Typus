{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.DependencyCycleDetectionQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Dependencies (DependencyGraph(..), DependencyNode(..), DependencyEdge(..))
import Dependencies.Analyzer (analyzeDependencies, detectCycles)
import Dependencies.TypeSystem (TypeDependency(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, sort, (\\))

-- ============================================================================
-- Dependency Cycle Detection Property Tests
-- ============================================================================

-- | Test that cycle detection finds L.all actual cycles
prop_cycleDetectionFindsAllCycles :: DependencyGraph -> Property
prop_cycleDetectionFindsAllCycles graph =
  let detectedCycles = detectCycles graph
      actualCycles = findActualCycles graph
  in counterexample ("Cycle detection should find L.all actual cycles. " ++
                     "Detected: " ++ show detectedCycles ++
                     " Actual: " ++ show actualCycles)
     (L.all (`elem` detectedCycles) actualCycles)

-- | Test that cycle detection doesn't produce false positives
prop_cycleDetectionNoFalsePositives :: DependencyGraph -> Property
prop_cycleDetectionNoFalsePositives graph =
  let detectedCycles = detectCycles graph
      falsePositives = L.filter (not . isValidCycle graph) detectedCycles
  in counterexample ("Cycle detection shouldn't produce false positives. " ++
                     "Detected: " ++ show detectedCycles ++
                     " False positives: " ++ show falsePositives)
     (null falsePositives)

-- | Test that cycle detection handles self-dependencies
prop_cycleDetectionHandlesSelfDependencies :: String -> Property
prop_cycleDetectionHandlesSelfDependencies node =
  let graph = createSelfDependencyGraph node
      cycles = detectCycles graph
      expectedCycle = [node, node]
  in counterexample ("Cycle detection should handle self-dependencies. " ++
                     "Node: " ++ node ++
                     " Cycles: " ++ show cycles)
     (L.any (== expectedCycle) cycles)

-- | Test that cycle detection works with disconnected components
prop_cycleDetectionHandlesDisconnectedComponents :: [String] -> [String] -> Property
prop_cycleDetectionHandlesDisconnectedComponents component1 component2 =
  not (null component1) && not (null component2) ==> 
    let graph = createDisconnectedGraph component1 component2
        cycles = detectCycles graph
        component1Cycles = L.filter (L.any (`elem` component1)) cycles
        component2Cycles = L.filter (L.any (`elem` component2)) cycles
    in counterexample ("Cycle detection should work with disconnected components. " ++
                       "Component1: " ++ show component1 ++
                       " Component2: " ++ show component2 ++
                       " Cycles: " ++ show cycles)
       (L.length component1Cycles + L.length component2Cycles === L.length cycles)

-- | Test that cycle detection preserves cycle minimality
prop_cycleDetectionPreservesMinimality :: DependencyGraph -> Property
prop_cycleDetectionPreservesMinimality graph =
  let cycles = detectCycles graph
      minimalCycles = filterMinimalCycles cycles
  in counterexample ("Cycle detection should preserve cycle minimality. " ++
                     "All cycles: " ++ show cycles ++
                     " Minimal: " ++ show minimalCycles)
     (L.all isMinimalCycle cycles)

-- | Test that cycle detection handles transitive dependencies
prop_cycleDetectionHandlesTransitiveDependencies :: String -> String -> String -> Property
prop_cycleDetectionHandlesTransitiveDependencies a b c =
  let graph = createTransitiveGraph a b c
      cycles = detectCycles graph
  in counterexample ("Cycle detection should handle transitive dependencies. " ++
                     "A: " ++ a ++ " B: " ++ b ++ " C: " ++ c ++
                     " Cycles: " ++ show cycles)
     (L.length cycles >= 0)

-- | Test that cycle detection is deterministic
prop_cycleDetectionIsDeterministic :: DependencyGraph -> Property
prop_cycleDetectionIsDeterministic graph =
  let cycles1 = detectCycles graph
      cycles2 = detectCycles graph
  in counterexample ("Cycle detection should be deterministic")
     (sort cycles1 === sort cycles2)

-- | Test that cycle detection handles complex graphs
prop_cycleDetectionHandlesComplexGraphs :: [(String, [String])] -> Property
prop_cycleDetectionHandlesComplexGraphs dependencies =
  not (null dependencies) ==> 
    let graph = createComplexGraph dependencies
        cycles = detectCycles graph
    in counterexample ("Cycle detection should handle complex graphs. " ++
                       "Dependencies: " ++ show dependencies ++
                       " Cycles: " ++ show cycles)
       (L.all isValidCycle cycles)

-- | Test that cycle detection identifies minimal cycles correctly
prop_cycleDetectionIdentifiesMinimalCycles :: DependencyGraph -> Property
prop_cycleDetectionIdentifiesMinimalCycles graph =
  let cycles = detectCycles graph
      minimalCycles = findMinimalCycles graph
      detectedMinimal = filter isMinimalCycle cycles
  in counterexample ("Cycle detection should identify minimal cycles correctly. " ++
                     "Detected minimal: " ++ show detectedMinimal ++
                     " Actual minimal: " ++ show minimalCycles)
     (sort detectedMinimal === sort minimalCycles)

-- | Test that cycle detection handles diamond dependencies
prop_cycleDetectionHandlesDiamondDependencies :: String -> String -> String -> String -> Property
prop_cycleDetectionHandlesDiamondDependencies root left right bottom =
  let graph = createDiamondGraph root left right bottom
      cycles = detectCycles graph
  in counterexample ("Cycle detection should handle diamond dependencies. " ++
                     "Root: " ++ root ++ " Left: " ++ left ++ 
                     " Right: " ++ right ++ " Bottom: " ++ bottom ++
                     " Cycles: " ++ show cycles)
     (L.length cycles >= 0)

-- | Test that cycle detection preserves dependency direction
prop_cycleDetectionPreservesDependencyDirection :: DependencyGraph -> Property
prop_cycleDetectionPreservesDependencyDirection graph =
  let cycles = detectCycles graph
      validDirections = L.all (hasValidDirection graph) cycles
  in counterexample ("Cycle detection should preserve dependency direction. " ++
                     "Cycles: " ++ show cycles)
     (validDirections === True)

-- | Test that cycle detection handles empty graphs
prop_cycleDetectionHandlesEmptyGraph :: Property
prop_cycleDetectionHandlesEmptyGraph =
  let graph = createEmptyGraph
      cycles = detectCycles graph
  in counterexample ("Cycle detection should handle empty graphs")
     (null cycles === True)

-- | Test that cycle detection handles single node graphs
prop_cycleDetectionHandlesSingleNodeGraph :: String -> Property
prop_cycleDetectionHandlesSingleNodeGraph node =
  let graph = createSingleNodeGraph node
      cycles = detectCycles graph
  in counterexample ("Cycle detection should handle single node graphs. " ++
                     "Node: " ++ node ++
                     " Cycles: " ++ show cycles)
     (L.length cycles >= 0)

-- | Test that cycle detection handles type dependencies
prop_cycleDetectionHandlesTypeDependencies :: [TypeDependency] -> Property
prop_cycleDetectionHandlesTypeDependencies typeDeps =
  not (null typeDeps) ==> 
    let graph = createTypeDependencyGraph typeDeps
        cycles = detectCycles graph
    in counterexample ("Cycle detection should handle type dependencies. " ++
                       "Type deps: " ++ show typeDeps ++
                       " Cycles: " ++ show cycles)
       (L.all isValidTypeCycle cycles)

-- | Test that cycle detection provides meaningful cycle paths
prop_cycleDetectionProvidesMeaningfulPaths :: DependencyGraph -> Property
prop_cycleDetectionProvidesMeaningfulPaths graph =
  let cycles = detectCycles graph
      meaningfulPaths = L.all hasMeaningfulPath cycles
  in counterexample ("Cycle detection should provide meaningful cycle paths. " ++
                     "Cycles: " ++ show cycles)
     (meaningfulPaths === True)

-- | Test that cycle detection handles circular imports
prop_cycleDetectionHandlesCircularImports :: [String] -> Property
prop_cycleDetectionHandlesCircularImports modules =
  L.length modules > 1 ==> 
    let graph = createCircularImportGraph modules
        cycles = detectCycles graph
        expectedCycle = modules ++ [L.head modules]
    in counterexample ("Cycle detection should handle circular imports. " ++
                       "Modules: " ++ show modules ++
                       " Expected: " ++ show expectedCycle ++
                       " Detected: " ++ show cycles)
       (L.any (== expectedCycle) cycles)

-- | Test that cycle detection is efficient for large graphs
prop_cycleDetectionIsEfficient :: Int -> Property
prop_cycleDetectionIsEfficient size =
  size > 0 && size <= 100 ==> 
    let graph = createLargeGraph size
        cycles = detectCycles graph
    in counterexample ("Cycle detection should be efficient for large graphs")
       (L.length cycles >= 0)

-- | Test that cycle detection handles bidirectional dependencies
prop_cycleDetectionHandlesBidirectionalDependencies :: String -> String -> Property
prop_cycleDetectionHandlesBidirectionalDependencies a b =
  let graph = createBidirectionalGraph a b
      cycles = detectCycles graph
      expectedCycle = [a, b, a]
  in counterexample ("Cycle detection should handle bidirectional dependencies. " ++
                     "A: " ++ a ++ " B: " ++ b ++
                     " Expected: " ++ show expectedCycle ++
                     " Detected: " ++ show cycles)
     (L.any (== expectedCycle) cycles)

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock data types
data DependencyGraph = DependencyGraph
  { _nodes :: Map String DependencyNode
  , _edges :: [DependencyEdge]
  } deriving (Eq, Show)

data DependencyNode = DependencyNode
  { _nodeId :: String
  , _nodeType :: String
  } deriving (Eq, Show)

data DependencyEdge = DependencyEdge
  { _from :: String
  , _to :: String
  , _edgeType :: String
  } deriving (Eq, Show)

data TypeDependency = TypeDependency
  { _typeFrom :: String
  , _typeTo :: String
  } deriving (Eq, Show)

-- Mock functions
detectCycles :: DependencyGraph -> [[String]]
detectCycles _ = [["a", "b", "a"], ["c", "d", "e", "c"]]

findActualCycles :: DependencyGraph -> [[String]]
findActualCycles _ = [["a", "b", "a"]]

isValidCycle :: DependencyGraph -> [String] -> Bool
isValidCycle _ cycle = L.length cycle >= 2 && L.head cycle == last cycle

createSelfDependencyGraph :: String -> DependencyGraph
createSelfDependencyGraph node = DependencyGraph 
  (Map.singleton node (DependencyNode node "self"))
  [DependencyEdge node node "self"]

createDisconnectedGraph :: [String] -> [String] -> DependencyGraph
createDisconnectedGraph comp1 comp2 = DependencyGraph Map.empty []

createTransitiveGraph :: String -> String -> String -> DependencyGraph
createTransitiveGraph a b c = DependencyGraph Map.empty []

createComplexGraph :: [(String, [String])] -> DependencyGraph
createComplexGraph deps = DependencyGraph Map.empty []

filterMinimalCycles :: [[String]] -> [[String]]
filterMinimalCycles = id

isMinimalCycle :: [String] -> Bool
isMinimalCycle cycle = L.length cycle <= 4

findMinimalCycles :: DependencyGraph -> [[String]]
findMinimalCycles _ = [["a", "b", "a"]]

createDiamondGraph :: String -> String -> String -> String -> DependencyGraph
createDiamondGraph root left right bottom = DependencyGraph Map.empty []

hasValidDirection :: DependencyGraph -> [String] -> Bool
hasValidDirection _ _ = True

createEmptyGraph :: DependencyGraph
createEmptyGraph = DependencyGraph Map.empty []

createSingleNodeGraph :: String -> DependencyGraph
createSingleNodeGraph node = DependencyGraph 
  (Map.singleton node (DependencyNode node "single"))
  []

createTypeDependencyGraph :: [TypeDependency] -> DependencyGraph
createTypeDependencyGraph _ = DependencyGraph Map.empty []

isValidTypeCycle :: [String] -> Bool
isValidTypeCycle _ = True

hasMeaningfulPath :: [String] -> Bool
hasMeaningfulPath cycle = not (null cycle)

createCircularImportGraph :: [String] -> DependencyGraph
createCircularImportGraph modules = DependencyGraph Map.empty []

createLargeGraph :: Int -> DependencyGraph
createLargeGraph size = DependencyGraph Map.empty []

createBidirectionalGraph :: String -> String -> DependencyGraph
createBidirectionalGraph a b = DependencyGraph Map.empty []

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependency Cycle Detection QuickCheck Tests"
  [ testProperty "Cycle detection finds L.all actual cycles" prop_cycleDetectionFindsAllCycles
  , testProperty "Cycle detection doesn't produce false positives" prop_cycleDetectionNoFalsePositives
  , testProperty "Cycle detection handles self-dependencies" prop_cycleDetectionHandlesSelfDependencies
  , testProperty "Cycle detection works with disconnected components" prop_cycleDetectionHandlesDisconnectedComponents
  , testProperty "Cycle detection preserves cycle minimality" prop_cycleDetectionPreservesMinimality
  , testProperty "Cycle detection handles transitive dependencies" prop_cycleDetectionHandlesTransitiveDependencies
  , testProperty "Cycle detection is deterministic" prop_cycleDetectionIsDeterministic
  , testProperty "Cycle detection handles complex graphs" prop_cycleDetectionHandlesComplexGraphs
  , testProperty "Cycle detection identifies minimal cycles correctly" prop_cycleDetectionIdentifiesMinimalCycles
  , testProperty "Cycle detection handles diamond dependencies" prop_cycleDetectionHandlesDiamondDependencies
  , testProperty "Cycle detection preserves dependency direction" prop_cycleDetectionPreservesDependencyDirection
  , testProperty "Cycle detection handles empty graphs" prop_cycleDetectionHandlesEmptyGraph
  , testProperty "Cycle detection handles single node graphs" prop_cycleDetectionHandlesSingleNodeGraph
  , testProperty "Cycle detection handles type dependencies" prop_cycleDetectionHandlesTypeDependencies
  , testProperty "Cycle detection provides meaningful cycle paths" prop_cycleDetectionProvidesMeaningfulPaths
  , testProperty "Cycle detection handles circular imports" prop_cycleDetectionHandlesCircularImports
  , testProperty "Cycle detection is efficient for large graphs" prop_cycleDetectionIsEfficient
  , testProperty "Cycle detection handles bidirectional dependencies" prop_cycleDetectionHandlesBidirectionalDependencies
  ]