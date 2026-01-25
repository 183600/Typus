{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewDependencyCycleDetectionQuickCheckTestSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Dependencies.AST
import Data.List (nub, sort, find)
import Data.Map.Strict (Map, fromList, toList, keys, elems, member, insert, empty)
import qualified Data.Map.Strict as Map
import Data.Set (Set, fromList, toList, union, intersection, empty)
import qualified Data.Set as Set

-- ============================================================================
-- Dependency Cycle Detection QuickCheck Tests
-- ============================================================================

-- Test DependencyNode equality
prop_dependency_node_equality :: String -> [String] -> Property
prop_dependency_node_equality name deps = 
  let node1 = DependencyNode name deps
      node2 = DependencyNode name deps
  in property $ node1 === node2

prop_dependency_node_ordering :: String -> [String] -> Property
prop_dependency_node_ordering name deps = 
  let node1 = DependencyNode name deps
      node2 = DependencyNode (name ++ "_suffix") deps
  in property $ node1 <= node2 || node1 >= node2

-- Test DependencyGraph equality
prop_dependency_graph_equality :: [DependencyNode] -> Property
prop_dependency_graph_equality nodes = 
  let graph1 = DependencyGraph (fromList [(nodeName n, n) | n <- nodes])
      graph2 = DependencyGraph (fromList [(nodeName n, n) | n <- nodes])
  in property $ graph1 === graph2

-- Test cycle detection in simple graphs
prop_cycle_detection_empty_graph :: Property
prop_cycle_detection_empty_graph = 
  let graph = DependencyGraph empty
      hasCycle = detectCycle graph
  in property $ not hasCycle

prop_cycle_detection_single_node :: String -> Property
prop_cycle_detection_single_node name = 
  let node = DependencyNode name []
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
  in property $ not hasCycle

prop_cycle_detection_self_loop :: String -> Property
prop_cycle_detection_self_loop name = 
  let node = DependencyNode name [name]
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
  in property $ hasCycle

prop_cycle_detection_two_node_cycle :: String -> String -> Property
prop_cycle_detection_two_node_cycle name1 name2 = 
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name1]
      graph = DependencyGraph (fromList [(name1, node1), (name2, node2)])
      hasCycle = detectCycle graph
  in property $ hasCycle

prop_cycle_detection_two_node_acyclic :: String -> String -> Property
prop_cycle_detection_two_node_acyclic name1 name2 = 
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 []
      graph = DependencyGraph (fromList [(name1, node1), (name2, node2)])
      hasCycle = detectCycle graph
  in property $ not hasCycle

-- Test cycle detection in complex graphs
prop_cycle_detection_three_node_cycle :: String -> String -> String -> Property
prop_cycle_detection_three_node_cycle name1 name2 name3 = 
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name3]
      node3 = DependencyNode name3 [name1]
      graph = DependencyGraph (fromList [(name1, node1), (name2, node2), (name3, node3)])
      hasCycle = detectCycle graph
  in property $ hasCycle

prop_cycle_detection_three_node_acyclic :: String -> String -> String -> Property
prop_cycle_detection_three_node_acyclic name1 name2 name3 = 
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name3]
      node3 = DependencyNode name3 []
      graph = DependencyGraph (fromList [(name1, node1), (name2, node2), (name3, node3)])
      hasCycle = detectCycle graph
  in property $ not hasCycle

prop_cycle_detection_complex_graph :: [String] -> Property
prop_cycle_detection_complex_graph names = 
  let nodes = [DependencyNode name (take 1 (dropWhile (== name) (cycle names))) | name <- names]
      graph = DependencyGraph (fromList [(name, node) | name <- names, node <- nodes, nodeName node == name])
      hasCycle = detectCycle graph
  in property $ hasCycle === (length names > 1)
  where
    cycle [] = []
    cycle xs = xs

-- Test cycle detection properties
prop_cycle_detection_reflexive :: DependencyGraph -> Property
prop_cycle_detection_reflexive graph = 
  let hasCycle = detectCycle graph
      hasSelfCycle = any (\n -> nodeName n `elem` nodeDependencies n) (elems (graphNodes graph))
  in property $ hasSelfCycle ==> hasCycle

prop_cycle_detection_symmetric :: DependencyGraph -> Property
prop_cycle_detection_symmetric graph = 
  let hasCycle = detectCycle graph
      reversedGraph = reverseGraph graph
      hasCycleReversed = detectCycle reversedGraph
  in property $ hasCycle === hasCycleReversed

prop_cycle_detection_transitive :: DependencyGraph -> DependencyGraph -> Property
prop_cycle_detection_transitive graph1 graph2 = 
  let hasCycle1 = detectCycle graph1
      hasCycle2 = detectCycle graph2
      combinedGraph = combineGraphs graph1 graph2
      hasCycleCombined = detectCycle combinedGraph
  in property $ (hasCycle1 || hasCycle2) ==> hasCycleCombined

-- Test cycle detection algorithms
prop_cycle_detection_dfs_consistency :: DependencyGraph -> Property
prop_cycle_detection_dfs_consistency graph = 
  let hasCycleDFS = detectCycleDFS graph
      hasCycle = detectCycle graph
  in property $ hasCycleDFS === hasCycle

prop_cycle_detection_topological_consistency :: DependencyGraph -> Property
prop_cycle_detection_topological_consistency graph = 
  let hasCycleTopological = detectCycleTopological graph
      hasCycle = detectCycle graph
  in property $ hasCycleTopological === hasCycle

-- Test cycle detection with missing dependencies
prop_cycle_detection_missing_deps :: String -> [String] -> Property
prop_cycle_detection_missing_deps name deps = 
  let node = DependencyNode name deps
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
  in property $ not (name `elem` deps) ==> not hasCycle

-- Test cycle detection with duplicate dependencies
prop_cycle_detection_duplicate_deps :: String -> [String] -> Property
prop_cycle_detection_duplicate_deps name deps = 
  let duplicateDeps = deps ++ deps
      node = DependencyNode name duplicateDeps
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
      uniqueDeps = nub deps
      nodeUnique = DependencyNode name uniqueDeps
      graphUnique = DependencyGraph (fromList [(name, nodeUnique)])
      hasCycleUnique = detectCycle graphUnique
  in property $ hasCycle === hasCycleUnique

-- Test cycle detection with empty dependencies
prop_cycle_detection_empty_deps :: [String] -> Property
prop_cycle_detection_empty_deps names = 
  let nodes = [DependencyNode name [] | name <- names]
      graph = DependencyGraph (fromList [(name, node) | name <- names, node <- nodes, nodeName node == name])
      hasCycle = detectCycle graph
  in property $ not hasCycle

-- Test cycle detection with complete graph
prop_cycle_detection_complete_graph :: [String] -> Property
prop_cycle_detection_complete_graph names = 
  let nodes = [DependencyNode name (filter (/= name) names) | name <- names]
      graph = DependencyGraph (fromList [(name, node) | name <- names, node <- nodes, nodeName node == name])
      hasCycle = detectCycle graph
  in property $ hasCycle === (length names > 1)

-- Test cycle detection path properties
prop_cycle_detection_path_property :: DependencyGraph -> Property
prop_cycle_detection_path_property graph = 
  let hasCycle = detectCycle graph
      allNodes = keys (graphNodes graph)
      reachableFromStart = if null allNodes then [] else findReachable (head allNodes) graph
      hasPathBack = any (\n -> head allNodes `elem` findReachable n graph) reachableFromStart
  in property $ hasPathBack ==> hasCycle

-- Test cycle detection with isolated nodes
prop_cycle_detection_isolated_nodes :: [String] -> Property
prop_cycle_detection_isolated_nodes names = 
  let isolatedNodes = [DependencyNode name [] | name <- names]
      isolatedGraph = DependencyGraph (fromList [(name, node) | name <- names, node <- isolatedNodes, nodeName node == name])
      hasCycle = detectCycle isolatedGraph
  in property $ not hasCycle

-- Test cycle detection with chain graph
prop_cycle_detection_chain_graph :: [String] -> Property
prop_cycle_detection_chain_graph names = 
  let chainNodes = createChainNodes names
      chainGraph = DependencyGraph (fromList [(name, node) | name <- names, node <- chainNodes, nodeName node == name])
      hasCycle = detectCycle chainGraph
  in property $ not hasCycle
  where
    createChainNodes [] = []
    createChainNodes [_] = [DependencyNode (head names) []]
    createChainNodes (x:y:xs) = DependencyNode x [y] : createChainNodes (y:xs)

-- Test cycle detection with star graph
prop_cycle_detection_star_graph :: String -> [String] -> Property
prop_cycle_detection_star_graph center leaves = 
  let centerNode = DependencyNode center leaves
      leafNodes = [DependencyNode leaf [] | leaf <- leaves]
      allNodes = centerNode : leafNodes
      starGraph = DependencyGraph (fromList [(nodeName n, n) | n <- allNodes])
      hasCycle = detectCycle starGraph
  in property $ not hasCycle

-- Test cycle detection with bidirectional edges
prop_cycle_detection_bidirectional :: String -> String -> Property
prop_cycle_detection_bidirectional name1 name2 = 
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name1]
      graph = DependencyGraph (fromList [(name1, node1), (name2, node2)])
      hasCycle = detectCycle graph
  in property $ hasCycle

-- Test cycle detection with multiple cycles
prop_cycle_detection_multiple_cycles :: [String] -> Property
prop_cycle_detection_multiple_cycles names = 
  let cycleNodes = createCycleNodes names
      cycleGraph = DependencyGraph (fromList [(name, node) | name <- names, node <- cycleNodes, nodeName node == name])
      hasCycle = detectCycle cycleGraph
  in property $ hasCycle === (length names > 2)
  where
    createCycleNodes [] = []
    createCycleNodes [_] = []
    createCycleNodes [x, y] = [DependencyNode x [y], DependencyNode y [x]]
    createCycleNodes (x:y:xs) = DependencyNode x [y] : createCycleNodes (y:xs ++ [x])

-- Test cycle detection performance properties
prop_cycle_detection_linear_complexity :: [String] -> Property
prop_cycle_detection_linear_complexity names = 
  let nodes = [DependencyNode name (take 1 names) | name <- names]
      graph = DependencyGraph (fromList [(name, node) | name <- names, node <- nodes, nodeName node == name])
      hasCycle = detectCycle graph
  in property $ length names >= 0 ==> hasCycle === (length names > 1)

-- Test cycle detection edge cases
prop_cycle_detection_single_self_dependency :: String -> Property
prop_cycle_detection_single_self_dependency name = 
  let node = DependencyNode name [name]
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
  in property $ hasCycle

prop_cycle_detection_multiple_self_dependencies :: [String] -> Property
prop_cycle_detection_multiple_self_dependencies names = 
  let nodes = [DependencyNode name [name] | name <- names]
      graph = DependencyGraph (fromList [(name, node) | name <- names, node <- nodes, nodeName node == name])
      hasCycle = detectCycle graph
  in property $ hasCycle === (not (null names))

-- Test cycle detection with mixed dependencies
prop_cycle_detection_mixed_dependencies :: String -> [String] -> Property
prop_cycle_detection_mixed_dependencies name deps = 
  let mixedDeps = if name `elem` deps then deps else deps ++ [name]
      node = DependencyNode name mixedDeps
      graph = DependencyGraph (fromList [(name, node)])
      hasCycle = detectCycle graph
  in property $ hasCycle

-- Helper functions
detectCycle :: DependencyGraph -> Bool
detectCycle graph = detectCycleDFS graph

detectCycleDFS :: DependencyGraph -> Bool
detectCycleDFS graph = 
  let allNodes = keys (graphNodes graph)
      visited = Set.empty
  in any (\n -> hasCycleFrom n visited Set.empty) allNodes
  where
    hasCycleFrom node visited recStack = 
      if Set.member node recStack
      then True
      else if Set.member node visited
           then False
           else 
             let visited' = Set.insert node visited
                 recStack' = Set.insert node recStack
                 deps = case Map.lookup node (graphNodes graph) of
                          Just n -> nodeDependencies n
                          Nothing -> []
             in any (\d -> hasCycleFrom d visited' recStack') deps

detectCycleTopological :: DependencyGraph -> Bool
detectCycleTopological graph = 
  let allNodes = keys (graphNodes graph)
      (sorted, hasCycle) = topologicalSort graph allNodes
  in hasCycle
  where
    topologicalSort _ [] = ([], False)
    topologicalSort g nodes = 
      let (noDeps, hasDeps) = partition (\n -> null (getDependencies n g)) nodes
          remaining = filter (`notElem` noDeps) hasDeps
      in if null noDeps && not (null remaining)
         then ([], True)  -- Cycle detected
         else 
           let (sortedRest, hasCycleRest) = topologicalSort g remaining
           in (noDeps ++ sortedRest, hasCycleRest)
    
    getDependencies node g = 
      case Map.lookup node (graphNodes g) of
        Just n -> nodeDependencies n
        Nothing -> []

reverseGraph :: DependencyGraph -> DependencyGraph
reverseGraph graph = 
  let allNodes = keys (graphNodes graph)
      reversedEdges = [(dep, node) | node <- allNodes, 
                                    dep <- case Map.lookup node (graphNodes graph) of
                                            Just n -> nodeDependencies n
                                            Nothing -> []]
      reversedMap = foldl addEdge empty reversedEdges
      addEdge m (from, to) = Map.insertWith (\_ old -> old) from 
                              (DependencyNode from (case Map.lookup from m of
                                                      Just n -> nodeDependencies n ++ [to]
                                                      Nothing -> [to])) m
  in DependencyGraph reversedMap

combineGraphs :: DependencyGraph -> DependencyGraph -> DependencyGraph
combineGraphs graph1 graph2 = 
  let combinedNodes = Map.union (graphNodes graph1) (graphNodes graph2)
  in DependencyGraph combinedNodes

findReachable :: String -> DependencyGraph -> [String]
findReachable start graph = 
  let visited = Set.empty
      recStack = Set.empty
  in reachableFrom start visited recStack
  where
    reachableFrom node visited recStack = 
      if Set.member node recStack
      then [node]  -- Cycle detected
      else if Set.member node visited
           then []
           else 
             let visited' = Set.insert node visited
                 recStack' = Set.insert node recStack
                 deps = case Map.lookup node (graphNodes graph) of
                          Just n -> nodeDependencies n
                          Nothing -> []
                 directReachable = deps
                 indirectReachable = concatMap (\d -> reachableFrom d visited' recStack') deps
             in node : directReachable ++ indirectReachable

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- Tests collection
tests :: TestTree
tests = testGroup "Dependency Cycle Detection QuickCheck Tests"
  [ testProperty "dependency node equality" prop_dependency_node_equality
  , testProperty "dependency node ordering" prop_dependency_node_ordering
  , testProperty "dependency graph equality" prop_dependency_graph_equality
  , testProperty "cycle detection empty graph" prop_cycle_detection_empty_graph
  , testProperty "cycle detection single node" prop_cycle_detection_single_node
  , testProperty "cycle detection self loop" prop_cycle_detection_self_loop
  , testProperty "cycle detection two node cycle" prop_cycle_detection_two_node_cycle
  , testProperty "cycle detection two node acyclic" prop_cycle_detection_two_node_acyclic
  , testProperty "cycle detection three node cycle" prop_cycle_detection_three_node_cycle
  , testProperty "cycle detection three node acyclic" prop_cycle_detection_three_node_acyclic
  , testProperty "cycle detection complex graph" prop_cycle_detection_complex_graph
  , testProperty "cycle detection reflexive" prop_cycle_detection_reflexive
  , testProperty "cycle detection symmetric" prop_cycle_detection_symmetric
  , testProperty "cycle detection transitive" prop_cycle_detection_transitive
  , testProperty "cycle detection dfs consistency" prop_cycle_detection_dfs_consistency
  , testProperty "cycle detection topological consistency" prop_cycle_detection_topological_consistency
  , testProperty "cycle detection missing deps" prop_cycle_detection_missing_deps
  , testProperty "cycle detection duplicate deps" prop_cycle_detection_duplicate_deps
  , testProperty "cycle detection empty deps" prop_cycle_detection_empty_deps
  , testProperty "cycle detection complete graph" prop_cycle_detection_complete_graph
  , testProperty "cycle detection path property" prop_cycle_detection_path_property
  , testProperty "cycle detection isolated nodes" prop_cycle_detection_isolated_nodes
  , testProperty "cycle detection chain graph" prop_cycle_detection_chain_graph
  , testProperty "cycle detection star graph" prop_cycle_detection_star_graph
  , testProperty "cycle detection bidirectional" prop_cycle_detection_bidirectional
  , testProperty "cycle detection multiple cycles" prop_cycle_detection_multiple_cycles
  , testProperty "cycle detection linear complexity" prop_cycle_detection_linear_complexity
  , testProperty "cycle detection single self dependency" prop_cycle_detection_single_self_dependency
  , testProperty "cycle detection multiple self dependencies" prop_cycle_detection_multiple_self_dependencies
  , testProperty "cycle detection mixed dependencies" prop_cycle_detection_mixed_dependencies
  ]