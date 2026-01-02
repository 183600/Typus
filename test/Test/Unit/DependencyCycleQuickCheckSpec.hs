{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyCycleQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Dependencies.AST
import Dependencies.Analyzer
import Dependencies.TypeSystem

import Data.List (nub, sort)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Mock dependency graph for testing
data DependencyGraph = DependencyGraph
  { nodes :: [String]
  , edges :: [(String, String)]
  } deriving (Show, Eq)

-- Property: Acyclic graph should have no cycles
prop_acyclic_graph_no_cycles :: [(String, String)] -> Property
prop_acyclic_graph_no_cycles edgeList =
  let graph = buildGraph edgeList
      hasCycle = detectCycles graph
  in property $ not hasCycle

-- Property: Self-dependency creates a cycle
prop_self_dependency_creates_cycle :: String -> Property
prop_self_dependency_creates_cycle node =
  let edgeList = [(node, node)]
      graph = buildGraph edgeList
      hasCycle = detectCycles graph
  in property $ hasCycle

-- Property: Adding a cycle edge to acyclic graph creates cycle
prop_adding_cycle_edge_creates_cycle :: [(String, String)] -> String -> String -> Property
prop_adding_cycle_edge_creates_cycle edgeList node1 node2 =
  node1 /= node2 ==>
  let graph = buildGraph edgeList
      cycleEdge = (node1, node2)
      reverseEdge = (node2, node1)
      graphWithCycle = buildGraph (cycleEdge : reverseEdge : edgeList)
      hasCycle = detectCycles graphWithCycle
  in property $ hasCycle

-- Property: Topological sort should preserve dependencies
prop_topological_sort_preserves_deps :: [(String, String)] -> Property
prop_topological_sort_preserves_deps edgeList =
  let graph = buildGraph edgeList
      sorted = topologicalSort graph
      positions = Map.fromList $ zip sorted [0..]
      respectsDeps = L.all (\(from, to) -> 
        case (Map.lookup from positions, Map.lookup to positions) of
          (Just fromPos, Just toPos) -> fromPos < toPos
          _ -> True) edgeList
  in property $ respectsDeps

-- Property: Cycle detection should be transitive
prop_cycle_detection_transitive :: [(String, String)] -> Property
prop_cycle_detection_transitive edgeList =
  let graph = buildGraph edgeList
      hasCycle = detectCycles graph
      transitiveClosure = computeTransitiveClosure edgeList
      hasCycleInClosure = L.any (\(a, b) -> a == b) transitiveClosure
  in property $ hasCycle ==> hasCycleInClosure

-- Property: Removing an edge from a cycle can break the cycle
prop_removing_edge_breaks_cycle :: [(String, String)] -> Property
prop_removing_edge_breaks_cycle edgeList =
  let graph = buildGraph edgeList
      hasCycle = detectCycles graph
  in hasCycle ==>
     let edgeToRemove = L.head edgeList
        remainingEdges = L.tail edgeList
        graphWithoutEdge = buildGraph remainingEdges
        stillHasCycle = detectCycles graphWithoutEdge
    in property $ not stillHasCycle || L.length edgeList == 1

-- Property: Strongly connected components should be singletons in acyclic graph
prop_scc_singletons_in_acyclic :: [(String, String)] -> Property
prop_scc_singletons_in_acyclic edgeList =
  let graph = buildGraph edgeList
      hasCycle = detectCycles graph
      sccs = findStronglyConnectedComponents graph
  in not hasCycle ==> property $ L.all (\comp -> L.length comp == 1) sccs

-- Property: Dependency analysis should detect circular imports
prop_detect_circular_imports :: [String] -> Property
prop_detect_circular_imports modules =
  L.length modules >= 3 .&&. L.length (nub modules) >= 3 ==>
  let imports = zip modules (L.tail modules ++ [L.head modules])
      hasCircular = hasCircularImports imports
  in property $ hasCircular

-- Property: Dependency resolution should terminate for acyclic graphs
prop_dependency_resolution_terminates :: [(String, String)] -> Property
prop_dependency_resolution_terminates edgeList =
  let graph = buildGraph edgeList
      hasCycle = detectCycles graph
      resolved = resolveDependencies graph
  in not hasCycle ==> property $ L.length resolved == L.length (nodes graph)

-- Property: Adding redundant edges should not create cycles
prop_redundant_edges_no_cycles :: [(String, String)] -> String -> String -> Property
prop_redundant_edges_no_cycles edgeList from to =
  from /= to ==>
  let graph = buildGraph edgeList
      hasOriginalCycle = detectCycles graph
      redundantEdge = (from, to)
      graphWithRedundant = buildGraph (redundantEdge : edgeList)
      hasNewCycle = detectCycles graphWithRedundant
  in not hasOriginalCycle ==> property $ not hasNewCycle

-- Helper functions for dependency graph operations
buildGraph :: [(String, String)] -> DependencyGraph
buildGraph edgeList =
  let allNodes = nub $ concatMap (\(from, to) -> [from, to]) edgeList
  in DependencyGraph allNodes edgeList

detectCycles :: DependencyGraph -> Bool
detectCycles graph = hasCycleHelper (nodes graph) (edges graph) Set.empty Set.empty

hasCycleHelper :: [String] -> [(String, String)] -> Set.Set String -> Set.Set String -> Bool
hasCycleHelper [] _ _ _ = False
hasCycleHelper (node:rest) edges visited recStack
  | Set.member node recStack = True
  | Set.member node visited = hasCycleHelper rest edges visited recStack
  | otherwise =
      let neighbors = [to | (from, to) <- edges, from == node]
          newVisited = Set.insert node visited
          newRecStack = Set.insert node recStack
      in L.any (\neighbor -> hasCycleHelper (neighbor:rest) edges newVisited newRecStack) neighbors

topologicalSort :: DependencyGraph -> [String]
topologicalSort graph = topologicalSortHelper (nodes graph) (edges graph) Set.empty []

topologicalSortHelper :: [String] -> [(String, String)] -> Set.Set String -> [String] -> [String]
topologicalSortHelper [] _ _ result = L.reverse result
topologicalSortHelper (node:rest) edges visited result
  | Set.member node visited = topologicalSortHelper rest edges visited result
  | hasIncomingEdges node edges visited = topologicalSortHelper (rest ++ [node]) edges visited result
  | otherwise =
      let newVisited = Set.insert node visited
          newResult = node : result
      in topologicalSortHelper rest edges newVisited newResult

hasIncomingEdges :: String -> [(String, String)] -> Set.Set String -> Bool
hasIncomingEdges node edges visited =
  L.any (\(from, to) -> to == node && not (Set.member from visited)) edges

computeTransitiveClosure :: [(String, String)] -> [(String, String)]
computeTransitiveClosure edges =
  let allNodes = nub $ concatMap (\(from, to) -> [from, to]) edges
      initialMatrix = Map.fromList [(node, Set.fromList [to | (f, to) <- edges, f == node]) | node <- allNodes]
      closure = floydWarshall initialMatrix allNodes
  in [(from, to) | from <- allNodes, to <- Set.toList $ Map.findWithDefault Set.empty from closure]

floydWarshall :: Map.Map String (Set.Set String) -> [String] -> Map.Map String (Set.Set String)
floydWarshall matrix [] = matrix
floydWarshall matrix (k:rest) =
  let newMatrix = Map.mapWithKey (\i reachable ->
        if Set.member k reachable
        then Set.union reachable (Map.findWithDefault Set.empty k matrix)
        else reachable) matrix
  in floydWarshall newMatrix rest

findStronglyConnectedComponents :: DependencyGraph -> [[String]]
findStronglyConnectedComponents graph =
  let allNodes = nodes graph
      visited = Set.empty
      components = []
  in sccHelper allNodes (edges graph) visited components

sccHelper :: [String] -> [(String, String)] -> Set.Set String -> [[String]] -> [[String]]
sccHelper [] _ _ components = components
sccHelper (node:rest) edges visited components
  | Set.member node visited = sccHelper rest edges visited components
  | otherwise =
      let component = findComponent node edges Set.empty
          newVisited = Set.union visited (Set.fromList component)
          newComponents = component : components
      in sccHelper rest edges newVisited newComponents

findComponent :: String -> [(String, String)] -> Set.Set String -> [String]
findComponent node edges visited =
  if Set.member node visited
  then []
  else
    let neighbors = [to | (from, to) <- edges, from == node] ++ [from | (from, to) <- edges, to == node]
        newVisited = Set.insert node visited
        subComponents = concatMap (\neighbor -> findComponent neighbor edges newVisited) neighbors
    in node : subComponents

hasCircularImports :: [(String, String)] -> Bool
hasCircularImports imports = detectCycles (buildGraph imports)

resolveDependencies :: DependencyGraph -> [String]
resolveDependencies graph = topologicalSort graph

tests :: TestTree
tests = testGroup "Dependency Cycle QuickCheck Tests"
  [ fastProperty "Acyclic graph has no cycles" prop_acyclic_graph_no_cycles
  , fastProperty "Self-dependency creates a cycle" prop_self_dependency_creates_cycle
  , fastProperty "Adding cycle edge creates cycle" prop_adding_cycle_edge_creates_cycle
  , fastProperty "Topological sort preserves dependencies" prop_topological_sort_preserves_deps
  , fastProperty "Cycle detection is transitive" prop_cycle_detection_transitive
  , fastProperty "Removing edge breaks cycle" prop_removing_edge_breaks_cycle
  , fastProperty "SCC are singletons in acyclic graph" prop_scc_singletons_in_acyclic
  , fastProperty "Detect circular imports" prop_detect_circular_imports
  , fastProperty "Dependency resolution terminates for acyclic graphs" prop_dependency_resolution_terminates
  , fastProperty "Redundant edges don't create cycles" prop_redundant_edges_no_cycles
  ]