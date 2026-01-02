{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesCycleDetectionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Dependencies.AST
import Dependencies.TypeSystem
import Dependencies.Analyzer
import Data.List (nub, sort, intersect, union, (\\))
import Data.Set (Set, toList, fromList, union, intersection, difference)
import qualified Data.Set as Set

-- Simple graph representation for cycle detection testing
data DependencyGraph = DependencyGraph
  { nodes :: [String]
  , edges :: [(String, String)]
  } deriving (Show, Eq)

instance Arbitrary DependencyGraph where
  arbitrary = sized genGraph
    where
      genGraph 0 = do
        n <- choose (0, 3)
        nodes <- vectorOf n (elements ["A", "B", "C", "D", "E"])
        return $ DependencyGraph (nub nodes) []
      genGraph n = do
        let maxNodes = min (n + 1) 5
        numNodes <- choose (1, maxNodes)
        nodeNames <- vectorOf numNodes (elements ["A", "B", "C", "D", "E", "F", "G", "H"])
        let uniqueNodes = nub nodeNames
        numEdges <- choose (0, L.length uniqueNodes * 2)
        edges <- vectorOf numEdges $ do
          from <- elements uniqueNodes
          to <- elements uniqueNodes
          return (from, to)
        return $ DependencyGraph uniqueNodes edges

-- Property: Acyclic graphs should have no cycles
prop_acyclic_graph_no_cycles :: DependencyGraph -> Property
prop_acyclic_graph_no_cycles graph =
  let acyclicEdges = L.filter (uncurry (/=)) $ edges graph
      acyclicGraph = DependencyGraph (nodes graph) acyclicEdges
      hasCycle = detectDirectCycles acyclicGraph
  in classify (L.length (nodes acyclicGraph) > 3) "medium graph" $
     classify (L.length (edges acyclicGraph) > 5) "many edges" $
     property $ not hasCycle

-- Property: Self-loops are detected as cycles
prop_self_loop_detection :: String -> Property
prop_self_loop_detection nodeName =
  not (null nodeName) ==>
  let graph = DependencyGraph [nodeName] [(nodeName, nodeName)]
      hasCycle = detectDirectCycles graph
  in property $ hasCycle

-- Property: Simple two-node cycles are detected
prop_two_node_cycle_detection :: String -> String -> Property
prop_two_node_cycle_detection node1 node2 =
  node1 /= node2 ==>
  let graph = DependencyGraph [node1, node2] [(node1, node2), (node2, node1)]
      hasCycle = detectDirectCycles graph
  in property $ hasCycle

-- Property: Complex cycles are detected
prop_complex_cycle_detection :: Property
prop_complex_cycle_detection =
  let graph = DependencyGraph ["A", "B", "C", "D"] 
                              [("A", "B"), ("B", "C"), ("C", "D"), ("D", "A")]
      hasCycle = detectDirectCycles graph
  in property $ hasCycle

-- Property: Transitive dependencies are tracked correctly
prop_transitive_dependencies :: DependencyGraph -> Property
prop_transitive_dependencies graph =
  L.length (nodes graph) >= 3 ==>
  let startNode = L.head (nodes graph)
      transitiveDeps = getTransitiveDependencies graph startNode
      directDeps = getDirectDependencies graph startNode
      allDeps = getAllDependencies graph startNode
  in property $ Set.isSubsetOf (fromList directDeps) (fromList allDeps) &&
                Set.isSubsetOf (fromList transitiveDeps) (fromList allDeps)

-- Property: Cycle detection works with disconnected components
prop_disconnected_components :: DependencyGraph -> DependencyGraph -> Property
prop_disconnected_components graph1 graph2 =
  let disjointNodes = L.null (nodes graph1 `intersect` nodes graph2)
      combinedGraph = DependencyGraph 
        (nodes graph1 `union` nodes graph2)
        (edges graph1 `union` edges graph2)
      cycles1 = detectDirectCycles graph1
      cycles2 = detectDirectCycles graph2
      cyclesCombined = detectDirectCycles combinedGraph
  in disjointNodes ==> 
     property $ cyclesCombined == (cycles1 || cycles2)

-- Helper functions for cycle detection
detectDirectCycles :: DependencyGraph -> Bool
detectDirectCycles graph = L.any (uncurry (==)) (edges graph) || hasPathCycle graph

hasPathCycle :: DependencyGraph -> Bool
hasPathCycle graph = L.any (`hasPathTo` graph) (nodes graph)
  where
    hasPathTo node g = L.any (pathExists node) (nodes g)
    
    pathExists from to = from /= to && reachable from to (edges g)
    
    reachable _ _ [] = False
    reachable target currentEdges ((from, to):rest)
      | from == target = to == target || reachable target rest currentEdges
      | otherwise = reachable target (L.filter ((/=) from) currentEdges) rest

getDirectDependencies :: DependencyGraph -> String -> [String]
getDirectDependencies graph node = map snd $ L.filter ((==) node . fst) (edges graph)

getTransitiveDependencies :: DependencyGraph -> String -> [String]
getTransitiveDependencies graph node = 
  let direct = getDirectDependencies graph node
      indirect = concatMap (getTransitiveDependencies graph) direct
  in nub (direct ++ indirect)

getAllDependencies :: DependencyGraph -> String -> [String]
getAllDependencies graph node = 
  let visited = collectDeps graph node []
  in L.filter (/= node) visited
  where
    collectDeps g current visited'
      | current `elem` visited' = visited'
      | otherwise = 
          let direct = getDirectDependencies g current
              newVisited = current : visited'
          in L.foldr (collectDeps g) newVisited direct

tests :: TestTree
tests = testGroup "Dependencies Cycle Detection QuickCheck Tests"
  [ fastProperty "Acyclic graphs have no cycles" prop_acyclic_graph_no_cycles
  , fastProperty "Self-loops are detected as cycles" prop_self_loop_detection
  , fastProperty "Two-node cycles are detected" prop_two_node_cycle_detection
  , fastProperty "Complex cycles are detected" prop_complex_cycle_detection
  , fastProperty "Transitive dependencies are tracked correctly" prop_transitive_dependencies
  , fastProperty "Disconnected components work correctly" prop_disconnected_components
  , testCase "Manual cycle detection test" $ do
      let graphWithCycle = DependencyGraph ["A", "B", "C"] [("A", "B"), ("B", "C"), ("C", "A")]
          graphWithoutCycle = DependencyGraph ["A", "B", "C"] [("A", "B"), ("B", "C")]
      assertBool "Graph with cycle should be detected" $ detectDirectCycles graphWithCycle
      assertBool "Graph without cycle should not be detected" $ not $ detectDirectCycles graphWithoutCycle
  ]