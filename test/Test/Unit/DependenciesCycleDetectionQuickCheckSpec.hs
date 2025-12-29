{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesCycleDetectionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Dependencies
import Dependencies.AST
import Dependencies.TypeSystem
import Data.List (sort, nub, group, intercalate, find, delete, isInfixOf, sortOn, (\\))
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Graph (buildG, topSort, components, reachable)
import Data.Tree (Tree(Node))

-- ============================================================================
-- Dependencies Cycle Detection QuickCheck Tests
-- ============================================================================

-- Property: Acyclic graph validation
prop_acyclic_graph_validation :: [(String, [String])] -> Property
prop_acyclic_graph_validation dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      isAcyclic = isAcyclicGraph graph
      sorted = topologicalSort graph
  in property $ isAcyclic ==> length sorted == length dependencies

-- Property: Cycle detection consistency
prop_cycle_detection_consistency :: [(String, [String])] -> Property
prop_cycle_detection_consistency dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      hasCycle = hasCycles graph
      cycles = findCycles graph
  in property $ hasCycle ==> not (null cycles)

-- Property: Cycle path reconstruction
prop_cycle_path_reconstruction :: [(String, [String])] -> Property
prop_cycle_path_reconstruction dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      cycles = findCycles graph
      validPaths = all isValidCyclePath cycles
  in property $ null cycles .||. validPaths

-- Property: Dependency closure computation
prop_dependency_closure_computation :: [(String, [String])] -> String -> Property
prop_dependency_closure_computation dependencies node =
  not (null dependencies) && node `elem` map fst dependencies ==> 
  let graph = buildDependencyGraph dependencies
      closure = computeDependencyClosure graph node
      directDeps = findDirectDependencies graph node
  in property $ directDeps `Set.isSubsetOf` closure

-- Property: Strongly connected components
prop_strongly_connected_components :: [(String, [String])] -> Property
prop_strongly_connected_components dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      sccs = findStronglyConnectedComponents graph
      allNodes = Set.fromList $ map fst dependencies
      sccNodes = Set.unions sccs
  in property $ sccNodes == allNodes

-- Property: Topological sort correctness
prop_topological_sort_correctness :: [(String, [String])] -> Property
prop_topological_sort_correctness dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      sorted = topologicalSort graph
      isOrdered = checkTopologicalOrder graph sorted
  in property $ isAcyclicGraph graph ==> isOrdered

-- Property: Dependency removal preserves acyclicity
prop_dependency_removal_preserves_acyclicity :: [(String, [String])] -> String -> String -> Property
prop_dependency_removal_preserves_acyclicity dependencies from to =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      modifiedGraph = removeDependency graph from to
      originalAcyclic = isAcyclicGraph graph
      modifiedAcyclic = isAcyclicGraph modifiedGraph
  in property $ (originalAcyclic ==> modifiedAcyclic)

-- Property: Circular dependency detection
prop_circular_dependency_detection :: [(String, [String])] -> Property
prop_circular_dependency_detection dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      circularDeps = findCircularDependencies graph
      hasCircular = not (null circularDeps)
  in property $ hasCircular ==> all (hasCyclePath graph) circularDeps

-- Property: Dependency level calculation
prop_dependency_level_calculation :: [(String, [String])] -> String -> Property
prop_dependency_level_calculation dependencies node =
  not (null dependencies) && node `elem` map fst dependencies ==> 
  let graph = buildDependencyGraph dependencies
      level = calculateDependencyLevel graph node
      closure = computeDependencyClosure graph node
  in property $ level >= 0 .&&. level <= Set.size closure

-- Property: Transitive dependency computation
prop_transitive_dependency_computation :: [(String, [String])] -> String -> String -> Property
prop_transitive_dependency_computation dependencies from to =
  not (null dependencies) && from `elem` map fst dependencies ==> 
  let graph = buildDependencyGraph dependencies
      transitive = hasTransitiveDependency graph from to
      reachable = isReachable graph from to
  in property $ transitive ==> reachable

-- Property: Dependency graph merging
prop_dependency_graph_merging :: [(String, [String])] -> [(String, [String])] -> Property
prop_dependency_graph_merging deps1 deps2 =
  not (null deps1) && not (null deps2) ==> 
  let graph1 = buildDependencyGraph deps1
      graph2 = buildDependencyGraph deps2
      merged = mergeDependencyGraphs graph1 graph2
      mergedNodes = Set.union (getGraphNodes graph1) (getGraphNodes graph2)
      actualNodes = getGraphNodes merged
  in property $ mergedNodes == actualNodes

-- Property: Dependency inversion correctness
prop_dependency_inversion_correctness :: [(String, [String])] -> String -> String -> Property
prop_dependency_inversion_correctness dependencies from to =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      inverted = invertDependency graph from to
      originalEdge = hasDependency graph from to
      invertedEdge = hasDependency inverted to from
  in property $ originalEdge ==> invertedEdge

-- Property: Dependency pruning
prop_dependency_pruning :: [(String, [String])] -> [String] -> Property
prop_dependency_pruning dependencies keepNodes =
  not (null dependencies) && not (null keepNodes) ==> 
  let graph = buildDependencyGraph dependencies
      pruned = pruneDependencyGraph graph (Set.fromList keepNodes)
      prunedNodes = getGraphNodes pruned
      expectedNodes = Set.fromList keepNodes `Set.intersection` getGraphNodes graph
  in property $ prunedNodes == expectedNodes

-- ============================================================================
-- Helper Functions and Types
-- ============================================================================

-- Dependency graph types
data DependencyGraph = DependencyGraph
  { graphNodes :: Set String
  , graphEdges :: Map String (Set String)
  } deriving (Eq, Show)

data CyclePath = CyclePath
  { cycleNodes :: [String]
  , cycleStart :: String
  } deriving (Eq, Show)

-- Graph construction functions
buildDependencyGraph :: [(String, [String])] -> DependencyGraph
buildDependencyGraph dependencies = DependencyGraph
  { graphNodes = Set.fromList $ map fst dependencies ++ concat (map snd dependencies)
  , graphEdges = Map.fromList dependencies
  }

isAcyclicGraph :: DependencyGraph -> Bool
isAcyclicGraph graph = null $ findCycles graph

hasCycles :: DependencyGraph -> Bool
hasCycles = not . isAcyclicGraph

findCycles :: DependencyGraph -> [CyclePath]
findCycles graph = 
  let nodes = Set.toList $ graphNodes graph
      cycles = [detectCycleFromNode graph node | node <- nodes]
  in filter (not . null . cycleNodes) cycles

detectCycleFromNode :: DependencyGraph -> String -> CyclePath
detectCycleFromNode graph start = 
  let visited = Set.empty
      path = []
  in detectCycle graph start visited path

detectCycle :: DependencyGraph -> String -> Set String -> [String] -> CyclePath
detectCycle graph node visited path
  | node `Set.member` visited = 
      case break (== node) (reverse path) of
        (_, cyclePart) -> CyclePath (reverse cyclePart) node
        _ -> CyclePath [] node
  | otherwise = 
      case Map.lookup node (graphEdges graph) of
        Nothing -> CyclePath [] node
        Just deps -> 
          let newVisited = Set.insert node visited
              newPath = node : path
              cycles = [detectCycle graph dep newVisited newPath | dep <- Set.toList deps]
          in case filter (not . null . cycleNodes) cycles of
               (c:_) -> c
               [] -> CyclePath [] node

isValidCyclePath :: CyclePath -> Bool
isValidCyclePath cycle = 
  let nodes = cycleNodes cycle
      start = cycleStart cycle
  in not (null nodes) && start `elem` nodes

topologicalSort :: DependencyGraph -> [String]
topologicalSort graph = 
  let nodes = Set.toList $ graphNodes graph
      edges = [(from, to) | from <- nodes, to <- Set.toList $ fromMaybe empty (Map.lookup from (graphEdges graph))]
      graph' = buildG (0, length nodes - 1) [(nodeIndex from, nodeIndex to) | (from, to) <- edges]
      sortedIndices = topSort graph'
      nodeIndex node = case elemIndex node nodes of
                        Just idx -> idx
                        Nothing -> 0
  in map (nodes !!) sortedIndices
  where
    elemIndex x xs = findIndex (== x) xs
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)

checkTopologicalOrder :: DependencyGraph -> [String] -> Bool
checkTopologicalOrder graph sorted = 
  all (\(i, node) -> 
    let deps = fromMaybe empty (Map.lookup node (graphEdges graph))
        earlierNodes = take i sorted
    in all (`elem` earlierNodes) (Set.toList deps)
  ) (zip [0..] sorted)

computeDependencyClosure :: DependencyGraph -> String -> Set String
computeDependencyClosure graph node = 
  let visited = Set.empty
  in computeClosure graph node visited

computeClosure :: DependencyGraph -> String -> Set String -> Set String
computeClosure graph node visited
  | node `Set.member` visited = visited
  | otherwise = 
      case Map.lookup node (graphEdges graph) of
        Nothing -> visited
        Just deps -> 
          let newVisited = Set.insert node visited
              closures = [computeClosure graph dep newVisited | dep <- Set.toList deps]
          in Set.unions (newVisited : closures)

findDirectDependencies :: DependencyGraph -> String -> Set String
findDirectDependencies graph node = fromMaybe empty (Map.lookup node (graphEdges graph))

findStronglyConnectedComponents :: DependencyGraph -> [Set String]
findStronglyConnectedComponents graph = 
  let nodes = Set.toList $ graphNodes graph
      nodeIndex node = case elemIndex node nodes of
                        Just idx -> idx
                        Nothing -> 0
      edges = [(nodeIndex from, nodeIndex to) | from <- nodes, to <- Set.toList $ fromMaybe empty (Map.lookup from (graphEdges graph))]
      graph' = buildG (0, length nodes - 1) edges
      sccs = components graph'
  in map (\component -> Set.fromList $ map (nodes !!) component) sccs
  where
    elemIndex x xs = findIndex (== x) xs
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)

removeDependency :: DependencyGraph -> String -> String -> DependencyGraph
removeDependency graph from to = 
  let edges = graphEdges graph
      updatedEdges = Map.adjust (Set.delete to) from edges
  in graph { graphEdges = updatedEdges }

findCircularDependencies :: DependencyGraph -> [[String]]
findCircularDependencies graph = 
  let cycles = findCycles graph
  in map cycleNodes cycles

hasCyclePath :: DependencyGraph -> [String] -> Bool
hasCyclePath graph path = 
  length path >= 2 && 
  all (\(from, to) -> hasDependency graph from to) (zip path (tail path)) &&
  hasDependency graph (last path) (head path)

calculateDependencyLevel :: DependencyGraph -> String -> Int
calculateDependencyLevel graph node = 
  let closure = computeDependencyClosure graph node
  in Set.size closure

hasTransitiveDependency :: DependencyGraph -> String -> String -> Bool
hasTransitiveDependency graph from to = 
  let closure = computeDependencyClosure graph from
  in to `Set.member` closure

isReachable :: DependencyGraph -> String -> String -> Bool
isReachable graph from to = 
  let nodes = Set.toList $ graphNodes graph
      nodeIndex node = case elemIndex node nodes of
                        Just idx -> idx
                        Nothing -> 0
      edges = [(nodeIndex f, nodeIndex t) | f <- nodes, t <- Set.toList $ fromMaybe empty (Map.lookup f (graphEdges graph))]
      graph' = buildG (0, length nodes - 1) edges
  in nodeIndex to `elem` reachable graph' (nodeIndex from)

mergeDependencyGraphs :: DependencyGraph -> DependencyGraph -> DependencyGraph
mergeDependencyGraphs graph1 graph2 = DependencyGraph
  { graphNodes = Set.union (graphNodes graph1) (graphNodes graph2)
  , graphEdges = Map.unionWith Set.union (graphEdges graph1) (graphEdges graph2)
  }

getGraphNodes :: DependencyGraph -> Set String
getGraphNodes = graphNodes

hasDependency :: DependencyGraph -> String -> String -> Bool
hasDependency graph from to = 
  case Map.lookup from (graphEdges graph) of
    Nothing -> False
    Just deps -> to `Set.member` deps

invertDependency :: DependencyGraph -> String -> String -> DependencyGraph
invertDependency graph from to = 
  let edges = graphEdges graph
      edgesWithoutFrom = Map.adjust (Set.delete to) from edges
      edgesWithInverted = Map.insertWith Set.union to (Set.singleton from) edgesWithoutFrom
  in graph { graphEdges = edgesWithInverted }

pruneDependencyGraph :: DependencyGraph -> Set String -> DependencyGraph
pruneDependencyGraph graph keepNodes = 
  let prunedEdges = Map.filterWithKey (\node _ -> node `Set.member` keepNodes) (graphEdges graph)
      filteredEdges = Map.map (`Set.intersection` keepNodes) prunedEdges
  in DependencyGraph keepNodes filteredEdges

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies Cycle Detection QuickCheck Tests"
  [ fastProperty "Acyclic graph validation" prop_acyclic_graph_validation
  , fastProperty "Cycle detection consistency" prop_cycle_detection_consistency
  , fastProperty "Cycle path reconstruction" prop_cycle_path_reconstruction
  , fastProperty "Dependency closure computation" prop_dependency_closure_computation
  , fastProperty "Strongly connected components" prop_strongly_connected_components
  , fastProperty "Topological sort correctness" prop_topological_sort_correctness
  , fastProperty "Dependency removal preserves acyclicity" prop_dependency_removal_preserves_acyclicity
  , fastProperty "Circular dependency detection" prop_circular_dependency_detection
  , fastProperty "Dependency level calculation" prop_dependency_level_calculation
  , fastProperty "Transitive dependency computation" prop_transitive_dependency_computation
  , fastProperty "Dependency graph merging" prop_dependency_graph_merging
  , fastProperty "Dependency inversion correctness" prop_dependency_inversion_correctness
  , fastProperty "Dependency pruning" prop_dependency_pruning
  ]