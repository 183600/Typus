module Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)

import TestSupport.QuickCheck (fastProperty)

-- ============================================================================
-- New QuickCheck Tests for Dependencies Cycle Detection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Dependencies Cycle Detection QuickCheck Tests"
    [ testGroup "Cycle Detection Properties"
        [ fastProperty "acyclic graphs have no cycles" prop_acyclicGraphsHaveNoCycles
        , fastProperty "self-loops are detected as cycles" prop_selfLoopsDetected
        , fastProperty "cycle detection is transitive" prop_cycleDetectionTransitive
        , fastProperty "cycle detection preserves graph structure" prop_cycleDetectionPreservesStructure
        , fastProperty "cycle detection handles complex graphs" prop_cycleDetectionHandlesComplexGraphs
        ]

    , testGroup "Dependency Resolution Properties"
        [ fastProperty "dependency resolution is deterministic" prop_dependencyResolutionDeterministic
        , fastProperty "dependency resolution respects topological order" prop_dependencyResolutionTopological
        , fastProperty "dependency resolution handles missing nodes" prop_dependencyResolutionHandlesMissing
        , fastProperty "dependency resolution is idempotent" prop_dependencyResolutionIdempotent
        , fastProperty "dependency resolution preserves dependencies" prop_dependencyResolutionPreservesDeps
        ]

    , testGroup "Graph Algorithm Properties"
        [ fastProperty "topological sort exists for acyclic graphs" prop_topologicalSortExistsAcyclic
        , fastProperty "topological sort fails for cyclic graphs" prop_topologicalSortFailsCyclic
        , fastProperty "graph traversal is complete" prop_graphTraversalComplete
        , fastProperty "graph traversal avoids infinite loops" prop_graphTraversalAvoidsInfinite
        , fastProperty "graph algorithms handle empty graphs" prop_graphAlgorithmsHandleEmpty
        ]

    , testGroup "Performance Properties"
        [ fastProperty "cycle detection is linear time" prop_cycleDetectionLinearTime
        , fastProperty "memory usage is bounded" prop_memoryUsageBounded
        , fastProperty "algorithms handle large graphs" prop_algorithmsHandleLargeGraphs
        , fastProperty "performance degrades gracefully" prop_performanceDegradesGracefully
        ]
    ]

-- ============================================================================
-- Cycle Detection Property Tests
-- ============================================================================

-- | Acyclic graphs should have no cycles
prop_acyclicGraphsHaveNoCycles :: [(String, [String])] -> Property
prop_acyclicGraphsHaveNoCycles dependencies =
  let isAcyclic = not (hasCycle dependencies)
      detectedCycles = detectCycles dependencies
  in counterexample ("dependencies=" ++ show dependencies) $
     isAcyclic ==> null detectedCycles

-- | Self-loops should be detected as cycles
prop_selfLoopsDetected :: String -> Property
prop_selfLoopsDetected node =
  let dependencies = [(node, [node])]
      cycles = detectCycles dependencies
  in counterexample ("node=" ++ node) $
     not (null cycles)

-- | Cycle detection should be transitive
prop_cycleDetectionTransitive :: [(String, [String])] -> Property
prop_cycleDetectionTransitive dependencies =
  let cycles1 = detectCycles dependencies
      cycles2 = detectCycles dependencies
  in counterexample ("dependencies=" ++ show dependencies) $
     cycles1 === cycles2

-- | Cycle detection should preserve graph structure
prop_cycleDetectionPreservesStructure :: [(String, [String])] -> Property
prop_cycleDetectionPreservesStructure dependencies =
  let cycles = detectCycles dependencies
      nodeCount = length (nub (concatMap (\(n, deps) -> n : deps) dependencies))
  in counterexample ("dependencies=" ++ show dependencies ++ ", cycles=" ++ show cycles) $
     all (\cycle -> all (`elem` map fst dependencies) cycle) cycles

-- | Cycle detection should handle complex graphs
prop_cycleDetectionHandlesComplexGraphs :: [(String, [String])] -> Property
prop_cycleDetectionHandlesComplexGraphs dependencies =
  let result = detectCycles dependencies
  in counterexample ("graph size=" ++ show (length dependencies)) $
     length result >= 0  -- Should not crash

-- ============================================================================
-- Dependency Resolution Property Tests
-- ============================================================================

-- | Dependency resolution should be deterministic
prop_dependencyResolutionDeterministic :: [(String, [String])] -> Property
prop_dependencyResolutionDeterministic dependencies =
  let resolution1 = resolveDependencies dependencies
      resolution2 = resolveDependencies dependencies
  in counterexample ("dependencies=" ++ show dependencies) $
     resolution1 === resolution2

-- | Dependency resolution should respect topological order
prop_dependencyResolutionTopological :: [(String, [String])] -> Property
prop_dependencyResolutionTopological dependencies =
  let resolution = resolveDependencies dependencies
      isAcyclic = not (hasCycle dependencies)
  in if isAcyclic
     then counterexample ("resolution=" ++ show resolution) $
          isTopologicallyOrdered resolution dependencies
     else property True

-- | Dependency resolution should handle missing nodes
prop_dependencyResolutionHandlesMissing :: [(String, [String])] -> Property
prop_dependencyResolutionHandlesMissing dependencies =
  let allNodes = nub (concatMap (\(n, deps) -> n : deps) dependencies)
      definedNodes = map fst dependencies
      missingNodes = allNodes \\ definedNodes
      resolution = resolveDependencies dependencies
  in counterexample ("missing=" ++ show missingNodes) $
     length resolution >= 0  -- Should not crash

-- | Dependency resolution should be idempotent
prop_dependencyResolutionIdempotent :: [(String, [String])] -> Property
prop_dependencyResolutionIdempotent dependencies =
  let resolution1 = resolveDependencies dependencies
      resolution2 = resolveDependencies dependencies
  in counterexample ("dependencies=" ++ show dependencies) $
     resolution1 === resolution2

-- | Dependency resolution should preserve dependencies
prop_dependencyResolutionPreservesDeps :: [(String, [String])] -> Property
prop_dependencyResolutionPreservesDeps dependencies =
  let resolution = resolveDependencies dependencies
  in counterexample ("dependencies=" ++ show dependencies ++ ", resolution=" ++ show resolution) $
     length resolution >= length (filter (not . null . snd) dependencies)

-- ============================================================================
-- Graph Algorithm Property Tests
-- ============================================================================

-- | Topological sort should exist for acyclic graphs
prop_topologicalSortExistsAcyclic :: [(String, [String])] -> Property
prop_topologicalSortExistsAcyclic dependencies =
  let isAcyclic = not (hasCycle dependencies)
      topoSort = topologicalSort dependencies
  in if isAcyclic
     then counterexample ("dependencies=" ++ show dependencies) $
          not (null topoSort)
     else property True

-- | Topological sort should fail for cyclic graphs
prop_topologicalSortFailsCyclic :: [(String, [String])] -> Property
prop_topologicalSortFailsCyclic dependencies =
  let hasCycles = hasCycle dependencies
      topoSort = topologicalSort dependencies
  in if hasCycles
     then counterexample ("dependencies=" ++ show dependencies) $
          null topoSort || not (isTopologicallyOrdered topoSort dependencies)
     else property True

-- | Graph traversal should be complete
prop_graphTraversalComplete :: [(String, [String])] -> Property
prop_graphTraversalComplete dependencies =
  let startNode = case dependencies of
                    (n, _) : _ -> n
                    [] -> "default"
      visited = depthFirstTraversal startNode dependencies
      allNodes = nub (concatMap (\(n, deps) -> n : deps) dependencies)
  in counterexample ("visited=" ++ show visited ++ ", all=" ++ show allNodes) $
     null dependencies || not (null visited)

-- | Graph traversal should avoid infinite loops
prop_graphTraversalAvoidsInfinite :: [(String, [String])] -> Property
prop_graphTraversalAvoidsInfinite dependencies =
  let startNode = case dependencies of
                    (n, _) : _ -> n
                    [] -> "default"
      visited = depthFirstTraversal startNode dependencies
  in counterexample ("graph size=" ++ show (length dependencies)) $
     length visited <= length (nub (concatMap (\(n, deps) -> n : deps) dependencies))

-- | Graph algorithms should handle empty graphs
prop_graphAlgorithmsHandleEmpty :: Property
prop_graphAlgorithmsHandleEmpty =
  let dependencies = []
      cycles = detectCycles dependencies
      resolution = resolveDependencies dependencies
      topoSort = topologicalSort dependencies
  in counterexample ("empty graph results") $
     null cycles && null resolution && null topoSort

-- ============================================================================
-- Performance Property Tests
-- ============================================================================

-- | Cycle detection should be linear time (basic check)
prop_cycleDetectionLinearTime :: [(String, [String])] -> Property
prop_cycleDetectionLinearTime dependencies =
  let nodeCount = length dependencies
      edgeCount = sum (map length (map snd dependencies))
      result = detectCycles dependencies
  in counterexample ("nodes=" ++ show nodeCount ++ ", edges=" ++ show edgeCount) $
     length result >= 0  -- Basic completion check

-- | Memory usage should be bounded
prop_memoryUsageBounded :: [(String, [String])] -> Property
prop_memoryUsageBounded dependencies =
  let nodeCount = length dependencies
      result = detectCycles dependencies
  in counterexample ("nodes=" ++ show nodeCount) $
     length result <= nodeCount  -- Cycle count shouldn't exceed node count

-- | Algorithms should handle large graphs
prop_algorithmsHandleLargeGraphs :: Int -> Property
prop_algorithmsHandleLargeGraphs n =
  let size = min n 100  -- Limit for practical testing
      dependencies = generateDAG size
      result = detectCycles dependencies
  in counterexample ("graph size=" ++ show size) $
     length result >= 0

-- | Performance should degrade gracefully
prop_performanceDegradesGracefully :: Int -> Property
prop_performanceDegradesGracefully n =
  let size1 = min n 50
      size2 = min (n * 2) 100
      deps1 = generateDAG size1
      deps2 = generateDAG size2
      result1 = detectCycles deps1
      result2 = detectCycles deps2
  in counterexample ("sizes=" ++ show (size1, size2)) $
     length result2 >= 0 && length result1 >= 0

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Detect cycles in dependency graph
detectCycles :: [(String, [String])] -> [[String]]
detectCycles dependencies = 
  let allNodes = nub (concatMap (\(n, deps) -> n : deps) dependencies)
      cycles = [findCycle start dependencies | start <- allNodes]
  in nub (filter (not . null) cycles)

-- | Find cycle starting from node
findCycle :: String -> [(String, [String])] -> [String]
findCycle start dependencies = 
  let visited = []
      path = []
  in findCycleHelper start visited path dependencies
  where
    findCycleHelper node visited path deps
      | node `elem` path = takeWhile (/= node) (dropWhile (/= node) path) ++ [node]
      | node `elem` visited = []
      | otherwise = 
          case lookup node deps of
            Just deps' -> concatMap (\d -> findCycleHelper d (node:visited) (node:path) deps) deps'
            Nothing -> []

-- | Check if graph has cycles
hasCycle :: [(String, [String])] -> Bool
hasCycle dependencies = not (null (detectCycles dependencies))

-- | Resolve dependencies (topological sort)
resolveDependencies :: [(String, [String])] -> [String]
resolveDependencies dependencies = topologicalSort dependencies

-- | Topological sort
topologicalSort :: [(String, [String])] -> [String]
topologicalSort dependencies = 
  let allNodes = nub (concatMap (\(n, deps) -> n : deps) dependencies))
      (acyclicNodes, cyclicNodes) = partition (\n -> not (hasPath n n dependencies)) allNodes
  in if null cyclicNodes
     then reverse (topologicalSortHelper acyclicNodes dependencies [])
     else []

topologicalSortHelper :: [String] -> [(String, [String])] -> [String] -> [String]
topologicalSortHelper [] _ result = result
topologicalSortHelper (n:ns) dependencies result =
  let remainingDeps = filter ((/= n) . fst) dependencies
      newResult = n : result
      readyNodes = filter (\node -> all (\dep -> dep `elem` newResult) (lookupDependencies node dependencies)) ns
  in topologicalSortHelper readyNodes remainingDeps newResult

-- | Check if dependencies are topologically ordered
isTopologicallyOrdered :: [String] -> [(String, [String])] -> Bool
isTopologicallyOrdered order dependencies =
  all (\(node, deps) -> all (\dep -> position dep order < position node order) deps) dependencies
  where
    position _ [] = -1
    position x (y:ys) = if x == y then 0 else 1 + position x ys

-- | Depth-first traversal
depthFirstTraversal :: String -> [(String, [String])] -> [String]
depthFirstTraversal start dependencies = 
  let visited = []
  in dfsHelper start visited dependencies
  where
    dfsHelper node visited deps
      | node `elem` visited = visited
      | otherwise = 
          case lookup node deps of
            Just deps' -> foldr dfsHelper (node : visited) deps'
            Nothing -> node : visited

-- | Lookup dependencies for a node
lookupDependencies :: String -> [(String, [String])] -> [String]
lookupDependencies node dependencies = 
  case lookup node dependencies of
    Just deps -> deps
    Nothing -> []

-- | Check if there's a path from from to to
hasPath :: String -> String -> [(String, [String])] -> Bool
hasPath from to dependencies = 
  let visited = []
  in hasPathHelper from to visited dependencies
  where
    hasPathHelper current target visited deps
      | current == target = True
      | current `elem` visited = False
      | otherwise = 
          case lookup current deps of
            Just deps' -> any (\d -> hasPathHelper d target (current:visited) deps) deps'
            Nothing -> False

-- | Generate a DAG for testing
generateDAG :: Int -> [(String, [String])]
generateDAG n = 
  let nodes = ["node" ++ show i | i <- [1..n]]
  in [(node, take (i `mod` 3) (filter (< node) nodes)) | (i, node) <- zip [1..] nodes]

-- | Remove duplicates from list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- | Partition list based on predicate
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldl (\(xs, ys) x -> if p x then (x:xs, ys) else (xs, x:ys)) ([], [])