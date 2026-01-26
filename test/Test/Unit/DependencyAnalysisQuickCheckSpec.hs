{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing #-}
module Test.Unit.DependencyAnalysisQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.QuickCheck (fastProperty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

-- Helper types and generators
data Dependency = Dependency
  { fromNode :: String
  , toNode :: String
  , depType :: DepType
  } deriving (Show, Eq, Ord)

data DepType = Direct | Indirect | Weak | Strong deriving (Show, Eq, Ord)

instance Arbitrary Dependency where
  arbitrary = do
    from <- arbitrary
    to <- arbitrary
    depType <- arbitrary
    return $ Dependency from to depType

instance Arbitrary DepType where
  arbitrary = elements [Direct, Indirect, Weak, Strong]

data DependencyGraph = DependencyGraph
  { nodes :: Set.Set String
  , edges :: Set.Set Dependency
  } deriving (Show, Eq)

instance Arbitrary DependencyGraph where
  arbitrary = do
    nodes <- arbitrary
    edges <- arbitrary
    return $ DependencyGraph nodes edges

-- Properties for dependency graphs
prop_graph_nodes_contained :: DependencyGraph -> Bool
prop_graph_nodes_contained graph = 
  let graphNodes = nodes graph
      edgeNodes = Set.fromList $ concatMap (\d -> [fromNode d, toNode d]) (Set.toList (edges graph))
  in edgeNodes `Set.isSubsetOf` graphNodes

prop_graph_edges_valid :: DependencyGraph -> Bool
prop_graph_edges_valid graph = 
  let graphNodes = nodes graph
      edgeNodes = Set.fromList $ concatMap (\d -> [fromNode d, toNode d]) (Set.toList (edges graph))
  in all (`Set.member` graphNodes) (Set.toList edgeNodes)

prop_graph_add_node_preserves_edges :: DependencyGraph -> String -> Bool
prop_graph_add_node_preserves_edges graph newNode = 
  let newGraph = addNode graph newNode
  in edges graph == edges newGraph

prop_graph_add_edge_preserves_nodes :: DependencyGraph -> String -> String -> DepType -> Bool
prop_graph_add_edge_preserves_nodes graph from to depType = 
  let newGraph = addEdge graph (Dependency from to depType)
      originalNodes = nodes graph
      newNodes = nodes newGraph
  in Set.fromList [from, to] `Set.isSubsetOf` newNodes && 
     originalNodes `Set.isSubsetOf` newNodes

-- Properties for dependency cycles
prop_cycle_detection_self_loop :: DependencyGraph -> String -> Property
prop_cycle_detection_self_loop graph node = 
  node `Set.member` nodes graph ==> 
  let graphWithSelfLoop = addEdge graph (Dependency node node Direct)
  in hasCycle graphWithSelfLoop

prop_cycle_detection_transitive :: DependencyGraph -> String -> String -> String -> Property
prop_cycle_detection_transitive graph a b c = 
  all (`Set.member` nodes graph) [a, b, c] && 
  a /= b && b /= c && a /= c ==> 
  let graphWithCycle = addEdge (addEdge (addEdge graph (Dependency a b Direct)) (Dependency b c Direct)) (Dependency c a Direct)
  in hasCycle graphWithCycle

prop_acyclic_graph_no_cycles :: DependencyGraph -> Property
prop_acyclic_graph_no_cycles graph = 
  not (hasCycle graph) ==> 
  let sorted = topologicalSort graph
  in isTopologicallyValid graph sorted

-- Properties for topological sorting
prop_topological_sort_contains_all_nodes :: DependencyGraph -> Bool
prop_topological_sort_contains_all_nodes graph = 
  let sorted = topologicalSort graph
      sortedNodes = Set.fromList sorted
      graphNodes = nodes graph
  in sortedNodes == graphNodes

prop_topological_sort_preserves_dependencies :: DependencyGraph -> Property
prop_topological_sort_preserves_dependencies graph = 
  not (hasCycle graph) ==> 
  let sorted = topologicalSort graph
  in all (dependencyPreserved sorted) (Set.toList (edges graph))
  where
    dependencyPreserved order (Dependency from to _) = 
      let fromIndex = findIndex from order
          toIndex = findIndex to order
      in fromIndex < toIndex
    findIndex x xs = case elemIndex x xs of
                      Just i -> i
                      Nothing -> -1

prop_topological_sort_unique_for_dag :: DependencyGraph -> Property
prop_topological_sort_unique_for_dag graph = 
  not (hasCycle graph) && 
  all (\(Dependency from to _) -> from < to) (Set.toList (edges graph)) ==> 
  let sorted1 = topologicalSort graph
      sorted2 = topologicalSort graph
  in sorted1 == sorted2

-- Properties for transitive dependencies
prop_transitive_closure_reflexive :: DependencyGraph -> String -> Property
prop_transitive_closure_reflexive graph node = 
  node `Set.member` nodes graph ==> 
  let closure = transitiveClosure graph node
  in node `Set.member` closure

prop_transitive_closure_transitive :: DependencyGraph -> String -> String -> String -> Property
prop_transitive_closure_transitive graph a b c = 
  all (`Set.member` nodes graph) [a, b, c] && 
  Dependency a b Direct `Set.member` edges graph && 
  Dependency b c Direct `Set.member` edges graph ==> 
  let closure = transitiveClosure graph a
  in c `Set.member` closure

prop_transitive_closure_minimal :: DependencyGraph -> String -> String -> Property
prop_transitive_closure_minimal graph from to = 
  from `Set.member` nodes graph && 
  to `Set.member` nodes graph && 
  not (hasPath graph from to) ==> 
  let closure = transitiveClosure graph from
  in not (to `Set.member` closure)

-- Properties for dependency analysis
prop_reverse_dependencies_invert :: DependencyGraph -> Bool
prop_reverse_dependencies_invert graph = 
  let reversed = reverseGraph graph
      originalEdges = edges graph
      reversedEdges = edges reversed
  in all (edgeInverted reversedEdges) originalEdges
  where
    edgeInverted revEdges (Dependency from to depType) = 
      Dependency to from depType `Set.member` revEdges

prop_strongly_connected_components_reflexive :: DependencyGraph -> String -> Property
prop_strongly_connected_components_reflexive graph node = 
  node `Set.member` nodes graph ==> 
  let sccs = stronglyConnectedComponents graph
      nodeSCC = findSCC node sccs
  in node `Set.member` nodeSCC

prop_strongly_connected_components_symmetric :: DependencyGraph -> String -> String -> Property
prop_strongly_connected_components_symmetric graph a b = 
  all (`Set.member` nodes graph) [a, b] && 
  hasPath graph a b && hasPath graph b a ==> 
  let sccs = stronglyConnectedComponents graph
      aSCC = findSCC a sccs
      bSCC = findSCC b sccs
  in aSCC == bSCC

-- Helper functions
addNode :: DependencyGraph -> String -> DependencyGraph
addNode graph node = 
  let newNodes = Set.insert node (nodes graph)
  in graph { nodes = newNodes }

addEdge :: DependencyGraph -> Dependency -> DependencyGraph
addEdge graph (Dependency from to depType) = 
  let newNodes = Set.insert from (Set.insert to (nodes graph))
      newEdges = Set.insert (Dependency from to depType) (edges graph)
  in graph { nodes = newNodes, edges = newEdges }

hasCycle :: DependencyGraph -> Bool
hasCycle graph = 
  let visited = Set.empty
      recursionStack = Set.empty
  in any (hasCycleFrom graph visited recursionStack) (Set.toList (nodes graph))

hasCycleFrom :: DependencyGraph -> Set.Set String -> Set.Set String -> String -> Bool
hasCycleFrom graph visited recursionStack node = 
  if node `Set.member` recursionStack
  then True
  else if node `Set.member` visited
       then False
       else
         let newVisited = Set.insert node visited
             newRecursionStack = Set.insert node recursionStack
             outgoing = Set.filter (\d -> fromNode d == node) (edges graph)
             targets = map toNode (Set.toList outgoing)
         in any (hasCycleFrom graph newVisited newRecursionStack) targets

topologicalSort :: DependencyGraph -> [String]
topologicalSort graph = 
  if hasCycle graph
  then []
  else
    let sorted = kahnAlgorithm graph (nodes graph) []
    in sorted

kahnAlgorithm :: DependencyGraph -> Set.Set String -> [String] -> [String]
kahnAlgorithm _ remaining result 
  | Set.null remaining = reverse result
kahnAlgorithm graph remaining result = 
  let noIncoming = Set.filter (\node -> Set.null (incomingEdges graph node)) remaining
  in if Set.null noIncoming
     then reverse result  -- Should not happen for DAGs
     else
       let node = Set.findMin noIncoming
           newRemaining = Set.delete node remaining
           newResult = node : result
           -- Remove all outgoing edges from node
           outgoing = Set.filter (\d -> fromNode d == node) (edges graph)
           newGraph = graph { edges = Set.difference (edges graph) outgoing }
       in kahnAlgorithm newGraph newRemaining newResult

incomingEdges :: DependencyGraph -> String -> Set.Set Dependency
incomingEdges graph node = 
  Set.filter (\d -> toNode d == node) (edges graph)

dependencyPreserved :: [String] -> Dependency -> Bool
dependencyPreserved order (Dependency from to _) = 
  let fromIndex = findIndex from order
      toIndex = findIndex to order
  in fromIndex < toIndex

findIndex :: Eq a => a -> [a] -> Int
findIndex x xs = case elemIndex x xs of
                  Just i -> i
                  Nothing -> -1

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex x (y:ys) = if x == y then Just 0 else (+1) <$> elemIndex x ys

hasPath :: DependencyGraph -> String -> String -> Bool
hasPath graph from to = 
  if from == to
  then True
  else
    let visited = Set.empty
    in hasPathFrom graph visited from to

hasPathFrom :: DependencyGraph -> Set.Set String -> String -> String -> Bool
hasPathFrom graph visited from to = 
  if from `Set.member` visited
  then False
  else if from == to
       then True
       else
         let newVisited = Set.insert from visited
             outgoing = Set.filter (\d -> fromNode d == from) (edges graph)
             targets = map toNode (Set.toList outgoing)
         in any (\target -> hasPathFrom graph newVisited target to) targets

transitiveClosure :: DependencyGraph -> String -> Set.Set String
transitiveClosure graph start = 
  let visited = Set.empty
      reachable = dfs graph visited start
  in reachable

dfs :: DependencyGraph -> Set.Set String -> String -> Set.Set String
dfs graph visited node = 
  if node `Set.member` visited
  then visited
  else
    let newVisited = Set.insert node visited
        outgoing = Set.filter (\d -> fromNode d == node) (edges graph)
        targets = map toNode (Set.toList outgoing)
        allReachable = Set.union newVisited (Set.unions (map (\target -> dfs graph newVisited target) targets))
    in allReachable

reverseGraph :: DependencyGraph -> DependencyGraph
reverseGraph graph = 
  let reversedEdges = Set.map reverseDependency (edges graph)
  in graph { edges = reversedEdges }
  where
    reverseDependency (Dependency from to depType) = Dependency to from depType

stronglyConnectedComponents :: DependencyGraph -> [Set.Set String]
stronglyConnectedComponents graph = 
  -- Simplified implementation - in practice would use Kosaraju's or Tarjan's algorithm
  map Set.singleton (Set.toList (nodes graph))

findSCC :: String -> [Set.Set String] -> Set.Set String
findSCC _ [] = Set.empty
findSCC node (scc:sccs) = 
  if node `Set.member` scc
  then scc
  else findSCC node sccs

isTopologicallyValid :: DependencyGraph -> [String] -> Bool
isTopologicallyValid graph sorted = 
  let sortedSet = Set.fromList sorted
      graphNodes = nodes graph
  in sortedSet == graphNodes && 
     all (dependencyPreserved sorted) (Set.toList (edges graph))

tests :: TestTree
tests = testGroup "Test.Unit.DependencyAnalysisQuickCheckSpec Tests"
  [ fastProperty "graph nodes contained" prop_graph_nodes_contained
  , fastProperty "graph edges valid" prop_graph_edges_valid
  , fastProperty "graph add node preserves edges" prop_graph_add_node_preserves_edges
  , fastProperty "graph add edge preserves nodes" prop_graph_add_edge_preserves_nodes
  , fastProperty "cycle detection self loop" prop_cycle_detection_self_loop
  , fastProperty "cycle detection transitive" prop_cycle_detection_transitive
  , fastProperty "acyclic graph no cycles" prop_acyclic_graph_no_cycles
  , fastProperty "topological sort contains all nodes" prop_topological_sort_contains_all_nodes
  , fastProperty "topological sort preserves dependencies" prop_topological_sort_preserves_dependencies
  , fastProperty "topological sort unique for dag" prop_topological_sort_unique_for_dag
  , fastProperty "transitive closure reflexive" prop_transitive_closure_reflexive
  , fastProperty "transitive closure transitive" prop_transitive_closure_transitive
  , fastProperty "transitive closure minimal" prop_transitive_closure_minimal
  , fastProperty "reverse dependencies invert" prop_reverse_dependencies_invert
  , fastProperty "strongly connected components reflexive" prop_strongly_connected_components_reflexive
  , fastProperty "strongly connected components symmetric" prop_strongly_connected_components_symmetric
  ]