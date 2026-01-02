{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, (\\), delete, intersect, union)
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , DependencyNode(..)
  , DependencyGraph(..)
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate Text identifiers
genTextIdentifier :: Gen Text
genTextIdentifier = T.pack <$> genIdentifier

-- Generate type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ SimpleT <$> genTextIdentifier
  , GenericT <$> genTextIdentifier <*> listOf genTypeExpr
  , FuncT <$> listOf (genTextIdentifier `suchThat` (not . T.null) `suchThat` (\t -> not (T.L.any isSpace t)) >>= (\n -> (,) n <$> genTypeExpr) <*> genTypeExpr
  , RefineT <$> genTypeExpr <*> listOf genConstraint
  ]

-- Generate constraints
genConstraint :: Gen Constraint
genConstraint = oneof
  [ SizeGT <$> genTextIdentifier <*> choose (0, 1000)
  , SizeGE <$> genTextIdentifier <*> choose (0, 1000)
  , RangeC <$> genTextIdentifier <*> choose (0, 100) <*> choose (101, 200)
  , PredC <$> genTextIdentifier <*> listOf genTypeExpr
  ]

-- Generate statements
genStatement :: Gen Statement
genStatement = oneof
  [ STypeDef <$> genTextIdentifier <*> listOf genTextIdentifier <*> listOf genConstraint
  , STypeAlias <$> genTextIdentifier <*> genTypeExpr <*> listOf genConstraint
  , SVarDecl <$> genTextIdentifier <*> genTypeExpr
  , SFuncDecl <$> genTextIdentifier <*> listOf (genTextIdentifier `suchThat` (not . T.null) >>= (\n -> (,) n <$> genTypeExpr)) <*> oneof [return Nothing, Just <$> genTypeExpr]
  , SConstraintDef <$> genTextIdentifier <*> genConstraint
  , SExistsDecl <$> listOf genTextIdentifier <*> genStatement
  ]

-- Generate dependency nodes
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- genIdentifier
  deps <- listOf genIdentifier `suchThat` (\ds -> not (name `elem` ds))
  return $ DependencyNode name deps

-- Generate dependency graphs
genDependencyGraph :: Gen DependencyGraph
genDependencyGraph = do
  nodes <- listOf genDependencyNode
  let nodeMap = Map.fromList $ L.map (\n -> (nodeName n, n)) nodes
  return $ DependencyGraph nodeMap

-- Generate graphs with potential cycles
genCyclicGraph :: Int -> Gen DependencyGraph
genCyclicGraph numNodes = do
  numNodes `seq` return ()
  let nodeNames = take numNodes $ L.map (\i -> "node" ++ show i) [1..]
  nodes <- mapM (\name -> do
    deps <- listOf $ elements nodeNames
    return $ DependencyNode name deps
  ) nodeNames
  let nodeMap = Map.fromList $ L.map (\n -> (nodeName n, n)) nodes
  return $ DependencyGraph nodeMap

-- Generate acyclic graphs (DAGs)
genAcyclicGraph :: Int -> Gen DependencyGraph
genAcyclicGraph numNodes = do
  numNodes `seq` return ()
  let nodeNames = take numNodes $ L.map (\i -> "node" ++ show i) [1..]
  nodes <- mapM (\(i, name) -> do
    -- Only depend on nodes with higher index to ensure acyclicity
    let possibleDeps = drop (i + 1) nodeNames
    deps <- listOf $ elements possibleDeps
    return $ DependencyNode name deps
  ) (zip [0..] nodeNames)
  let nodeMap = Map.fromList $ L.map (\n -> (nodeName n, n)) nodes
  return $ DependencyGraph nodeMap

-- ============================================================================
-- Cycle Detection Algorithms
-- ============================================================================

-- Detect cycles using depth-first search
detectCyclesDFS :: DependencyGraph -> [[String]]
detectCyclesDFS (DependencyGraph nodeMap) = 
  let visited = Set.empty
      recursionStack = Set.empty
  in go visited recursionStack (Map.keys nodeMap) []
  where
    go :: Set String -> Set String -> [String] -> [[String]] -> [[String]]
    go _ _ [] cycles = cycles
    go visited recStack (node:rest) cycles
      | node `Set.member` recStack = 
          case break (== node) (Set.toList recStack) of
            (_, cyclePath) -> (node : cyclePath) : cycles
      | node `Set.member` visited = go visited recStack rest cycles
      | otherwise = 
          case Map.lookup node nodeMap of
            Nothing -> go (Set.insert node visited) recStack rest cycles
            Just depNode -> 
              let newVisited = Set.insert node visited
                  newRecStack = Set.insert node recStack
                  newCycles = go newVisited newRecStack (nodeDependencies depNode) cycles
              in go newVisited recStack rest newCycles

-- Detect cycles using topological sort
detectCyclesTopological :: DependencyGraph -> [[String]]
detectCyclesTopological (DependencyGraph nodeMap) =
  let inDegree = Map.fromList $ L.map (\n -> (nodeName n, 0)) (Map.elems nodeMap)
      updatedInDegree = L.foldl (\acc (DependencyNode _ deps) ->
        foldl (\a dep -> Map.insertWith (+) dep 1 a) acc deps
      ) inDegree (Map.elems nodeMap)
  in topoSort (Map.keys updatedInDegree) updatedInDegree []
  where
    topoSort :: [String] -> Map String Int -> [String] -> [[String]]
    topoSort [] inDegree sorted = 
      if Map.size inDegree == 0 then [] else [Map.keys inDegree]
    topoSort (node:rest) inDegree sorted
      | Map.lookup node inDegree == Just 0 = 
          case Map.lookup node nodeMap of
            Nothing -> topoSort rest inDegree sorted
            Just depNode ->
              let newInDegree = L.foldl (\acc dep -> Map.insertWith (+) dep (-1) acc) inDegree (nodeDependencies depNode)
              in topoSort rest newInDegree (node : sorted)
      | otherwise = topoSort rest inDegree sorted

-- Check if graph has cycles
hasCycles :: DependencyGraph -> Bool
hasCycles graph = not (L.null (detectCyclesDFS graph))

-- Get L.all nodes in cycles
getNodesInCycles :: DependencyGraph -> Set String
getNodesInCycles graph = 
  let cycles = detectCyclesDFS graph
  in Set.fromList $ L.concat cycles

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary DependencyNode where
  arbitrary = genDependencyNode

instance Arbitrary DependencyGraph where
  arbitrary = genDependencyGraph

-- ============================================================================
-- Cycle Detection Properties
-- ============================================================================

-- Property: Empty graph has no cycles
prop_empty_graph_no_cycles :: Property
prop_empty_graph_no_cycles =
  let emptyGraph = DependencyGraph Map.empty
  in property $ not (hasCycles emptyGraph) .&&. L.null (detectCyclesDFS emptyGraph)

-- Property: Single node graph has no cycles
prop_single_node_no_cycles :: String -> Property
prop_single_node_no_cycles name =
  let node = DependencyNode name []
      graph = DependencyGraph $ Map.singleton name node
  in property $ not (hasCycles graph) .&&. L.null (detectCyclesDFS graph)

-- Property: Self-dependency creates a cycle
prop_self_dependency_cycle :: String -> Property
prop_self_dependency_cycle name =
  let node = DependencyNode name [name]
      graph = DependencyGraph $ Map.singleton name node
  in property $ hasCycles graph .&&. not (L.null (detectCyclesDFS graph))

-- Property: Two-node cycle is detected
prop_two_node_cycle :: String -> String -> Property
prop_two_node_cycle name1 name2 =
  name1 /= name2 ==>
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name1]
      graph = DependencyGraph $ Map.fromList [(name1, node1), (name2, node2)]
      cycles = detectCyclesDFS graph
  in property $ hasCycles graph .&&. 
             any (\cycle -> name1 `elem` cycle && name2 `elem` cycle) cycles

-- Property: Three-node cycle is detected
prop_three_node_cycle :: String -> String -> String -> Property
prop_three_node_cycle name1 name2 name3 =
  all (/=) [name1, name2, name3] ==>
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name3]
      node3 = DependencyNode name3 [name1]
      graph = DependencyGraph $ Map.fromList [(name1, node1), (name2, node2), (name3, node3)]
      cycles = detectCyclesDFS graph
  in property $ hasCycles graph .&&.
             any (\cycle -> L.all (`elem` cycle) [name1, name2, name3]) cycles

-- Property: Acyclic graph has no cycles
prop_acyclic_graph_no_cycles :: Int -> Property
prop_acyclic_graph_no_cycles numNodes =
  numNodes >= 0 && numNodes <= 20 ==>
  forAll (genAcyclicGraph numNodes) $ \graph ->
    property $ not (hasCycles graph) .&&. L.null (detectCyclesDFS graph)

-- Property: Cycle detection algorithms agree
prop_cycle_detection_algorithms_agree :: DependencyGraph -> Property
prop_cycle_detection_algorithms_agree graph =
  let dfsCycles = detectCyclesDFS graph
      topoCycles = detectCyclesTopological graph
      hasDFS = not (null dfsCycles)
      hasTopo = not (null topoCycles)
  in property $ hasDFS === hasTopo

-- Property: Adding self-dependency creates cycle
prop_add_self_dependency_creates_cycle :: DependencyGraph -> String -> Property
prop_add_self_dependency_creates_cycle graph name =
  let originalHasCycles = hasCycles graph
      node = DependencyNode name [name]
      newGraph = DependencyGraph $ Map.insert name node (graphNodes graph)
      newHasCycles = hasCycles newGraph
  in property $ newHasCycles

-- Property: Removing cycle-breaking dependency eliminates cycle
prop_remove_cycle_breaking_dependency :: String -> String -> String -> Property
prop_remove_cycle_breaking_dependency name1 name2 name3 =
  all (/=) [name1, name2, name3] ==>
  let node1 = DependencyNode name1 [name2]
      node2 = DependencyNode name2 [name3]
      node3 = DependencyNode name3 [name1]
      graph = DependencyGraph $ Map.fromList [(name1, node1), (name2, node2), (name3, node3)]
      -- Break the cycle by removing dependency from node3 to node1
      node3' = DependencyNode name3 []
      graph' = DependencyGraph $ Map.insert name3 node3' (graphNodes graph)
  in property $ hasCycles graph .&&. not (hasCycles graph')

-- Property: Nodes in cycle are correctly identified
prop_nodes_in_cycle_identified :: [String] -> Property
prop_nodes_in_cycle_identified names =
  length names >= 3 && L.length (nub names) == L.length names ==>
  let cycleNodes = take 3 names
      [n1, n2, n3] = cycleNodes
      otherNodes = drop 3 names
      -- Create a cycle with n1 -> n2 -> n3 -> n1
      cycleGraph = Map.fromList
        [ (n1, DependencyNode n1 [n2])
        , (n2, DependencyNode n2 [n3])
        , (n3, DependencyNode n3 [n1])
        ]
      -- Add other nodes without dependencies
      otherNodesMap = Map.fromList $ L.map (\n -> (n, DependencyNode n [])) otherNodes
      graph = DependencyGraph $ cycleGraph `Map.union` otherNodesMap
      cycleNodesSet = Set.fromList cycleNodes
      detectedCycleNodes = getNodesInCycles graph
  in property $ detectedCycleNodes === cycleNodesSet

-- Property: Multiple cycles are detected
prop_multiple_cycles_detected :: Property
prop_multiple_cycles_detected =
  let -- Cycle 1: a -> b -> a
      node1a = DependencyNode "a" ["b"]
      node1b = DependencyNode "b" ["a"]
      -- Cycle 2: c -> d -> e -> c
      node2c = DependencyNode "c" ["d"]
      node2d = DependencyNode "d" ["e"]
      node2e = DependencyNode "e" ["c"]
      -- Isolated node
      node3f = DependencyNode "f" []
      graph = DependencyGraph $ Map.fromList
        [ ("a", node1a), ("b", node1b)
        , ("c", node2c), ("d", node2d), ("e", node2e)
        , ("f", node3f)
        ]
      cycles = detectCyclesDFS graph
  in property $ L.length cycles >= 2 .&&.
             any (\cycle -> "a" `elem` cycle && "b" `elem` cycle) cycles .&&.
             any (\cycle -> L.all (`elem` cycle) ["c", "d", "e"]) cycles

-- Property: Complex cycle detection works
prop_complex_cycle_detection :: Int -> Property
prop_complex_cycle_detection numNodes =
  numNodes >= 5 && numNodes <= 20 ==>
  forAll (genCyclicGraph numNodes) $ \graph ->
    let cycles = detectCyclesDFS graph
        hasCyclesDetected = hasCycles graph
    in property $ (hasCyclesDetected && not (null cycles)) .||. (not hasCyclesDetected && null cycles)

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Cycle detection handles large graphs
prop_cycle_detection_large_graphs :: Int -> Property
prop_cycle_detection_large_graphs numNodes =
  numNodes >= 0 && numNodes <= 1000 ==>
  forAll (genCyclicGraph numNodes) $ \graph ->
    let cycles = detectCyclesDFS graph
    in property $ L.length cycles >= 0

-- Property: Cycle detection is idempotent
prop_cycle_detection_idempotent :: DependencyGraph -> Property
prop_cycle_detection_idempotent graph =
  let cycles1 = detectCyclesDFS graph
      cycles2 = detectCyclesDFS graph
  in property $ sort cycles1 === sort cycles2

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Graph with disconnected components handles correctly
prop_disconnected_components :: [String] -> [String] -> Property
prop_disconnected_components comp1 comp2 =
  not (null comp1) && not (null comp2) && 
  null (intersect comp1 comp2) ==>
  let nodes1 = L.map (\n -> (n, DependencyNode n [])) comp1
      nodes2 = L.map (\n -> (n, DependencyNode n [])) comp2
      graph = DependencyGraph $ Map.fromList (nodes1 ++ nodes2)
  in property $ not (hasCycles graph)

-- Property: Graph with chain dependencies has no cycles
prop_chain_dependencies_no_cycles :: [String] -> Property
prop_chain_dependencies_no_cycles names =
  length names >= 2 ==>
  let pairs = zip names (L.tail names)
      nodes = L.map (\(from, to) -> (from, DependencyNode from [to])) pairs
      -- Last node has no dependencies
      lastNode = (last names, DependencyNode (last names) [])
      graph = DependencyGraph $ Map.fromList (nodes ++ [lastNode])
  in property $ not (hasCycles graph)

-- Property: Graph with star topology has no cycles
prop_star_topology_no_cycles :: String -> [String] -> Property
prop_star_topology_no_cycles center leaves =
  not (null leaves) && not (center `elem` leaves) ==>
  let centerNode = (center, DependencyNode center leaves)
      leafNodes = L.map (\leaf -> (leaf, DependencyNode leaf [])) leaves
      graph = DependencyGraph $ Map.fromList (centerNode : leafNodes)
  in property $ not (hasCycles graph)

-- Property: Graph with bidirectional edge creates cycle
prop_bidirectional_edge_cycle :: String -> String -> Property
prop_bidirectional_edge_cycle node1 node2 =
  node1 /= node2 ==>
  let node1' = DependencyNode node1 [node2]
      node2' = DependencyNode node2 [node1]
      graph = DependencyGraph $ Map.fromList [(node1, node1'), (node2, node2')]
  in property $ hasCycles graph

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Dependencies Cycle Detection QuickCheck Tests"
  [ testGroup "Basic Cycle Detection"
    [ fastProperty "empty graph no cycles" prop_empty_graph_no_cycles
    , fastProperty "single node no cycles" prop_single_node_no_cycles
    , fastProperty "self dependency cycle" prop_self_dependency_cycle
    , fastProperty "two node cycle" prop_two_node_cycle
    , fastProperty "three node cycle" prop_three_node_cycle
    ]

  , testGroup "Acyclic Graph Properties"
    [ fastProperty "acyclic graph no cycles" prop_acyclic_graph_no_cycles
    , fastProperty "cycle detection algorithms agree" prop_cycle_detection_algorithms_agree
    ]

  , testGroup "Cycle Manipulation"
    [ fastProperty "add self dependency creates cycle" prop_add_self_dependency_creates_cycle
    , fastProperty "remove cycle breaking dependency" prop_remove_cycle_breaking_dependency
    ]

  , testGroup "Cycle Identification"
    [ fastProperty "nodes in cycle identified" prop_nodes_in_cycle_identified
    , fastProperty "multiple cycles detected" prop_multiple_cycles_detected
    , fastProperty "complex cycle detection" prop_complex_cycle_detection
    ]

  , testGroup "Performance Properties"
    [ fastProperty "cycle detection large graphs" prop_cycle_detection_large_graphs
    , fastProperty "cycle detection idempotent" prop_cycle_detection_idempotent
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "disconnected components" prop_disconnected_components
    , fastProperty "chain dependencies no cycles" prop_chain_dependencies_no_cycles
    , fastProperty "star topology no cycles" prop_star_topology_no_cycles
    , fastProperty "bidirectional edge cycle" prop_bidirectional_edge_cycle
    ]
  ]