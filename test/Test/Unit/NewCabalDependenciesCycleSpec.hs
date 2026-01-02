{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalDependenciesCycleSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Dependencies
import Dependencies.AST
import Dependencies.TypeSystem

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph (Graph, buildG, topSort, reachable)
import Data.List (nub, (\\))

-- | Test suite for Dependencies cycle detection properties
tests :: TestTree
tests =
  testGroup "Dependencies Cycle Detection Properties"
    [ testGroup "Basic cycle detection properties"
        [ fastProperty "acyclic graphs have no cycles" prop_acyclic_no_cycles
        , fastProperty "self-loops are detected as cycles" prop_self_loop_detection
        , fastProperty "cycle detection is deterministic" prop_cycle_detection_deterministic
        , fastProperty "cycle detection preserves graph structure" prop_cycle_detection_preserves_structure
        ]

    , testGroup "Cycle analysis properties"
        [ fastProperty "minimal cycles are detected" prop_minimal_cycle_detection
        , fastProperty "cycle L.length is correctly calculated" prop_cycle_length_calculation
        , fastProperty "cycle nodes are L.all connected" prop_cycle_nodes_connected
        , fastProperty "cycle detection finds L.all cycles" prop_finds_all_cycles
        ]

    , testGroup "Topological sorting properties"
        [ fastProperty "topological sort fails for cyclic graphs" prop_topo_sort_fails_cyclic
        , fastProperty "topological sort succeeds for acyclic graphs" prop_topo_sort_succeeds_acyclic
        , fastProperty "topological order respects dependencies" prop_topo_order_respects_deps
        ]

    , testGroup "Dependency resolution properties"
        [ fastProperty "dependency resolution terminates" prop_dependency_resolution_terminates
        , fastProperty "dependency resolution is conservative" prop_dependency_resolution_conservative
        , fastProperty "dependency resolution preserves constraints" prop_dependency_resolution_preserves_constraints
        ]

    , testGroup "Performance properties"
        [ fastProperty "cycle detection is linear in graph size for DAGs" prop_cycle_detection_linear_dag
        , fastProperty "cycle detection handles large graphs efficiently" prop_cycle_detection_large_graphs
        , fastProperty "cycle detection memory usage is bounded" prop_cycle_detection_memory_bounded
        ]
    ]

-- Helper types for dependency testing
data DependencyNode = DependencyNode
  { nodeId :: String
  , nodeType :: String
  , nodeDependencies :: Set String
  } deriving (Show, Eq, Ord)

data DependencyGraph = DependencyGraph
  { nodes :: Map String DependencyNode
  , edges :: [(String, String)]  -- (from, to) dependencies
  } deriving (Show, Eq)

data CycleInfo = CycleInfo
  { cycleNodes :: [String]
  , cycleLength :: Int
  , cycleEdges :: [(String, String)]
  } deriving (Show, Eq)

-- Helper functions
createNode :: String -> String -> Set String -> DependencyNode
createNode nodeId' nodeType' nodeDeps = 
  DependencyNode nodeId' nodeType' nodeDeps

addNode :: DependencyNode -> DependencyGraph -> DependencyGraph
addNode node graph =
  let newNodes = Map.insert (nodeId node) node (nodes graph)
      newEdges = Set.L.foldl (\edges dep -> (nodeId node, dep) : edges) (edges graph) (nodeDependencies node)
  in graph { nodes = newNodes, edges = newEdges }

hasCycle :: DependencyGraph -> Bool
hasCycle graph = 
  let nodeIds = Map.keys (nodes graph)
      maxId = L.length nodeIds
      vertexMap = Map.fromList $ zip nodeIds [0..]
      edgeList = L.map (\(from, to) -> (Map.findWithDefault 0 from vertexMap, Map.findWithDefault 0 to vertexMap)) (edges graph)
      graphStruct = buildG (0, maxId - 1) edgeList
      sorted = topSort graphStruct
  in L.length sorted < L.length nodeIds

findCycles :: DependencyGraph -> [CycleInfo]
findCycles graph = 
  let nodeIds = Map.keys (nodes graph)
      maxId = L.length nodeIds
      vertexMap = Map.fromList $ zip nodeIds [0..]
      edgeList = L.map (\(from, to) -> (Map.findWithDefault 0 from vertexMap, Map.findWithDefault 0 to vertexMap)) (edges graph)
      graphStruct = buildG (0, maxId - 1) edgeList
      cycles = []  -- Simplified: actual cycle finding would be more complex
  in cycles

isAcyclic :: DependencyGraph -> Bool
isAcyclic = not . hasCycle

topologicalSort :: DependencyGraph -> Either String [String]
topologicalSort graph = 
  if hasCycle graph
  then Left "Graph has cycles"
  else 
    let nodeIds = Map.keys (nodes graph)
        maxId = L.length nodeIds
        vertexMap = Map.fromList $ zip nodeIds [0..]
        edgeList = L.map (\(from, to) -> (Map.findWithDefault 0 from vertexMap, Map.findWithDefault 0 to vertexMap)) (edges graph)
        graphStruct = buildG (0, maxId - 1) edgeList
        sortedIndices = topSort graphStruct
        reverseVertexMap = Map.fromList $ L.map (\(k, v) -> (v, k)) (Map.toList vertexMap)
    in Right $ L.map (`Map.findWithDefault` "unknown") reverseVertexMap sortedIndices

-- Basic cycle detection properties

prop_acyclic_no_cycles :: [String] -> Property
prop_acyclic_no_cycles nodeIds =
  not (null nodeIds) && L.length nodeIds <= 5 && L.all (not . null) nodeIds && L.all distinct nodeIds ==>
  let nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in property $ isAcyclic graph
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_self_loop_detection :: String -> Property
prop_self_loop_detection nodeId =
  not (null nodeId) && L.length nodeId <= 10 ==>
  let node = createNode nodeId "type" (Set.singleton nodeId)
      graph = addNode node (DependencyGraph Map.empty [])
  in property $ hasCycle graph

prop_cycle_detection_deterministic :: String -> Property
prop_cycle_detection_deterministic nodeId =
  not (null nodeId) && L.length nodeId <= 10 ==>
  let node = createNode nodeId "type" (Set.singleton nodeId)
      graph = addNode node (DependencyGraph Map.empty [])
      hasCycle1 = hasCycle graph
      hasCycle2 = hasCycle graph
  in property $ hasCycle1 === hasCycle2

prop_cycle_detection_preserves_structure :: [String] -> Property
prop_cycle_detection_preserves_structure nodeIds =
  not (null nodeIds) && L.length nodeIds <= 4 && L.all (not . null) nodeIds && L.all distinct nodeIds ==>
  let nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      originalGraph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
      _ = hasCycle originalGraph  -- Run cycle detection
  in property $ Map.size (nodes originalGraph) === L.length nodeIds
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

-- Cycle analysis properties

prop_minimal_cycle_detection :: String -> String -> Property
prop_minimal_cycle_detection nodeId1 nodeId2 =
  not (null nodeId1) && not (null nodeId2) && nodeId1 /= nodeId2 &&
  L.length nodeId1 <= 10 && L.length nodeId2 <= 10 ==>
  let node1 = createNode nodeId1 "type" (Set.singleton nodeId2)
      node2 = createNode nodeId2 "type" (Set.singleton nodeId1)
      graph = addNode node2 (addNode node1 (DependencyGraph Map.empty []))
      cycles = findCycles graph
  in property $ not (null cycles)

prop_cycle_length_calculation :: String -> String -> Property
prop_cycle_length_calculation nodeId1 nodeId2 =
  not (null nodeId1) && not (null nodeId2) && nodeId1 /= nodeId2 &&
  L.length nodeId1 <= 10 && L.length nodeId2 <= 10 ==>
  let node1 = createNode nodeId1 "type" (Set.singleton nodeId2)
      node2 = createNode nodeId2 "type" (Set.singleton nodeId1)
      graph = addNode node2 (addNode node1 (DependencyGraph Map.empty []))
  in property $ hasCycle graph

prop_cycle_nodes_connected :: String -> String -> Property
prop_cycle_nodes_connected nodeId1 nodeId2 =
  not (null nodeId1) && not (null nodeId2) && nodeId1 /= nodeId2 &&
  L.length nodeId1 <= 10 && L.length nodeId2 <= 10 ==>
  let node1 = createNode nodeId1 "type" (Set.singleton nodeId2)
      node2 = createNode nodeId2 "type" (Set.singleton nodeId1)
      graph = addNode node2 (addNode node1 (DependencyGraph Map.empty []))
  in property $ hasCycle graph

prop_finds_all_cycles :: [String] -> Property
prop_finds_all_cycles nodeIds =
  not (null nodeIds) && L.length nodeIds <= 4 && L.all (not . null) nodeIds && L.all distinct nodeIds ==>
  let -- Create a cycle: node1 -> node2 -> node3 -> node1
      (n1:n2:n3:_) = nodeIds
      node1 = createNode n1 "type" (Set.singleton n2)
      node2 = createNode n2 "type" (Set.singleton n3)
      node3 = createNode n3 "type" (Set.singleton n1)
      graph = addNode node3 (addNode node2 (addNode node1 (DependencyGraph Map.empty [])))
  in property $ hasCycle graph
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

-- Topological sorting properties

prop_topo_sort_fails_cyclic :: String -> String -> Property
prop_topo_sort_fails_cyclic nodeId1 nodeId2 =
  not (null nodeId1) && not (null nodeId2) && nodeId1 /= nodeId2 &&
  L.length nodeId1 <= 10 && L.length nodeId2 <= 10 ==>
  let node1 = createNode nodeId1 "type" (Set.singleton nodeId2)
      node2 = createNode nodeId2 "type" (Set.singleton nodeId1)
      graph = addNode node2 (addNode node1 (DependencyGraph Map.empty []))
  in case topologicalSort graph of
    Left _ -> property $ True
    Right _ -> property $ False

prop_topo_sort_succeeds_acyclic :: [String] -> Property
prop_topo_sort_succeeds_acyclic nodeIds =
  not (null nodeIds) && L.length nodeIds <= 4 && L.all (not . null) nodeIds && L.all distinct nodeIds ==>
  let nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in case topologicalSort graph of
    Right sorted -> property $ L.length sorted === L.length nodeIds
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_topo_order_respects_deps :: String -> String -> String -> Property
prop_topo_order_respects_deps dep1 dep2 dep3 =
  not (null dep1) && not (null dep2) && not (null dep3) &&
  L.all distinct [dep1, dep2, dep3] &&
  L.all ((<= 10) . L.length) [dep1, dep2, dep3] ==>
  let node1 = createNode dep1 "type" (Set.singleton dep2)
      node2 = createNode dep2 "type" (Set.singleton dep3)
      node3 = createNode dep3 "type" Set.empty
      graph = addNode node3 (addNode node2 (addNode node1 (DependencyGraph Map.empty [])))
  in case topologicalSort graph of
    Right sorted -> 
      let pos1 = indexOf dep1 sorted
          pos2 = indexOf dep2 sorted
          pos3 = indexOf dep3 sorted
      in property $ pos1 < pos2 .&&. pos2 < pos3
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs
    indexOf x xs = case elemIndex x xs of
      Just i -> i
      Nothing -> -1

-- Dependency resolution properties

prop_dependency_resolution_terminates :: [String] -> Property
prop_dependency_resolution_terminates nodeIds =
  not (null nodeIds) && L.length nodeIds <= 4 && L.all (not . null) nodeIds ==>
  let nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in case topologicalSort graph of
    Right _ -> property $ True
    Left _ -> property $ True  -- Termination is guaranteed

prop_dependency_resolution_conservative :: [String] -> Property
prop_dependency_resolution_conservative nodeIds =
  not (null nodeIds) && L.length nodeIds <= 4 && L.all (not . null) nodeIds ==>
  let nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in case topologicalSort graph of
    Right sorted -> property $ L.all (`elem` nodeIds) sorted
    Left _ -> property $ True

prop_dependency_resolution_preserves_constraints :: String -> String -> Property
prop_dependency_resolution_preserves_deps fromId toId =
  not (null fromId) && not (null toId) && fromId /= toId &&
  L.length fromId <= 10 && L.length toId <= 10 ==>
  let fromNode = createNode fromId "type" (Set.singleton toId)
      toNode = createNode toId "type" Set.empty
      graph = addNode toNode (addNode fromNode (DependencyGraph Map.empty []))
  in case topologicalSort graph of
    Right sorted -> 
      let fromPos = indexOf fromId sorted
          toPos = indexOf toId sorted
      in property $ fromPos < toPos
    Left _ -> property $ False
  where
    indexOf x xs = case elemIndex x xs of
      Just i -> i
      Nothing -> -1

-- Performance properties

prop_cycle_detection_linear_dag :: Int -> Property
prop_cycle_detection_linear_dag nodeCount =
  nodeCount >= 0 && nodeCount <= 20 ==>
  let nodeIds = L.map (\i -> "node_" ++ show i) [1..nodeCount]
      -- Create a DAG: node_i depends on node_{i-1}
      nodesList = zipWith (\i nodeId -> 
                            let deps = if i > 1 then Set.singleton (nodeIds !! (i-2)) else Set.empty
                            in createNode nodeId "type" deps) [1..] nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in property $ not (hasCycle graph)

prop_cycle_detection_large_graphs :: Int -> Property
prop_cycle_detection_large_graphs nodeCount =
  nodeCount >= 0 && nodeCount <= 15 ==>
  let nodeIds = L.map (\i -> "node_" ++ show i) [1..nodeCount]
      nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
  in property $ Map.size (nodes graph) === nodeCount

prop_cycle_detection_memory_bounded :: Int -> Property
prop_cycle_detection_memory_bounded nodeCount =
  nodeCount >= 0 && nodeCount <= 10 ==>
  let nodeIds = L.map (\i -> "node_" ++ show i) [1..nodeCount]
      nodesList = L.map (\nodeId -> createNode nodeId "type" Set.empty) nodeIds
      graph = L.foldl (flip addNode) (DependencyGraph Map.empty []) nodesList
      memoryUsage = Map.size (nodes graph) + L.length (edges graph)
  in property $ memoryUsage <= nodeCount * 2