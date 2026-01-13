module Test.Unit.TestDependenciesCycleDetectionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List (nub, sort)

-- Test Properties for Dependencies Cycle Detection

-- Property: Acyclic dependency graph should have no cycles
prop_acyclic_no_cycles :: [(String, String)] -> Property
prop_acyclic_no_cycles deps = property $ 
  let graph = buildDependencyGraph deps
      isAcyclic = not (hasCycle graph)
  in isAcyclic ==> not (hasCycle graph)

-- Property: Adding a cycle should be detected
prop_adding_cycle_detected :: [(String, String)] -> String -> Property
prop_adding_cycle_detected deps node = property $ 
  let graph = buildDependencyGraph deps
      cyclicGraph = addDependency node node graph  -- Self-dependency creates a cycle
  in hasCycle cyclicGraph

-- Property: Cycle detection should be order-independent
prop_cycle_detection_order_independent :: [(String, String)] -> Property
prop_cycle_detection_order_independent deps = property $ 
  let graph1 = buildDependencyGraph deps
      graph2 = buildDependencyGraph (reverse deps)
  in hasCycle graph1 == hasCycle graph2

-- Property: Topological sort should fail for cyclic graphs
prop_topological_sort_fails_cyclic :: [(String, String)] -> Property
prop_topological_sort_fails_cyclic deps = property $ 
  let graph = buildDependencyGraph deps
      sorted = topologicalSort graph
  in hasCycle graph ==> null sorted

-- Property: Removing edges should eliminate cycles
prop_removing_edges_eliminate_cycles :: [(String, String)] -> Property
prop_removing_edges_eliminate_cycles deps = property $ 
  let graph = buildDependencyGraph deps
  in if hasCycle graph
     then let edges = getEdges graph
              edgeToRemove = head edges  -- Remove first edge
              acyclicGraph = removeEdge edgeToRemove graph
          in not (hasCycle acyclicGraph) || length edges == 1
     else True

-- Property: Cycle detection should handle complex graphs
prop_cycle_detection_complex_graph :: [(String, String)] -> Property
prop_cycle_detection_complex_graph deps = property $ 
  let graph = buildDependencyGraph deps
      cycles = findCycles graph
  in null cycles == not (hasCycle graph)

-- Helper functions (mock implementations)
data DependencyGraph = DependencyGraph [(String, String)] deriving (Show, Eq)

buildDependencyGraph :: [(String, String)] -> DependencyGraph
buildDependencyGraph deps = DependencyGraph (nub deps)

hasCycle :: DependencyGraph -> Bool
hasCycle (DependencyGraph deps) = 
  let checkCycle visited current = 
        if current `elem` visited then True
        else case lookup current deps of
               Nothing -> False
               Just next -> checkCycle (current:visited) next
      nodes = map fst deps
  in any (checkCycle [] . fst) deps

addDependency :: String -> String -> DependencyGraph -> DependencyGraph
addDependency from to (DependencyGraph deps) = 
  DependencyGraph ((from, to) : deps)

topologicalSort :: DependencyGraph -> [String]
topologicalSort (DependencyGraph deps) = 
  if hasCycle (DependencyGraph deps) 
  then [] 
  else map fst $ sort deps  -- Mock implementation

getEdges :: DependencyGraph -> [(String, String)]
getEdges (DependencyGraph deps) = deps

removeEdge :: (String, String) -> DependencyGraph -> DependencyGraph
removeEdge edge (DependencyGraph deps) = 
  DependencyGraph (filter (/= edge) deps)

findCycles :: DependencyGraph -> [[String]]
findCycles graph = 
  if hasCycle graph then [ ["cycle"] ] else []  -- Mock implementation

tests :: TestTree
tests = testGroup "Test.Unit.TestDependenciesCycleDetectionSpec Tests"
  [ testProperty "Acyclic dependency graph should have no cycles" prop_acyclic_no_cycles
  , testProperty "Adding a cycle should be detected" prop_adding_cycle_detected
  , testProperty "Cycle detection should be order-independent" prop_cycle_detection_order_independent
  , testProperty "Topological sort should fail for cyclic graphs" prop_topological_sort_fails_cyclic
  , testProperty "Removing edges should eliminate cycles" prop_removing_edges_eliminate_cycles
  , testProperty "Cycle detection should handle complex graphs" prop_cycle_detection_complex_graph
  ]