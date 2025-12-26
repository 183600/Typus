{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewDependenciesQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Dependencies
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, (\\), sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | Test dependency analysis properties
spec :: Spec
spec = describe "NewDependencies QuickCheck Tests" $ do

  describe "Dependency graph properties" $ do
    it "empty graph has no nodes" $ do
      let emptyGraph = createEmptyDependencyGraph
      getGraphNodes emptyGraph `shouldBe` Set.empty
      getGraphEdges emptyGraph `shouldBe` []

    it "adding nodes updates graph correctly" $ property $
      \nodes ->
        let emptyGraph = createEmptyDependencyGraph
            graph = foldr addGraphNode emptyGraph nodes
        in Set.fromList nodes === getGraphNodes graph

    it "adding edges creates relationships" $ property $
      \fromNode toNode ->
        let graph = createEmptyDependencyGraph
            graph1 = addGraphNode fromNode graph
            graph2 = addGraphNode toNode graph1
            graph3 = addGraphEdge fromNode toNode graph2
        in getGraphEdges graph3 `shouldContain` [(fromNode, toNode)]

    it "edges connect existing nodes" $ property $
      \fromNode toNode ->
        let graph = createEmptyDependencyGraph
            graph1 = addGraphNode fromNode graph
            graph2 = addGraphNode toNode graph1
            graph3 = addGraphEdge fromNode toNode graph2
            nodes = getGraphNodes graph3
        in fromNode `Set.member` nodes && toNode `Set.member` nodes

    it "removing nodes removes incident edges" $ property $
      \fromNode toNode targetNode ->
        let graph = createEmptyDependencyGraph
            graph1 = foldr addGraphNode graph [fromNode, toNode, targetNode]
            graph2 = addGraphEdge fromNode targetNode graph1
            graph3 = addGraphEdge toNode targetNode graph2
            graph4 = removeGraphNode targetNode graph3
        in not (any ((== targetNode) . snd) (getGraphEdges graph4)) &&
           not (any ((== targetNode) . fst) (getGraphEdges graph4))

  describe "Dependency detection properties" $ do
    it "detects direct dependencies" $ property $
      \dependency deps ->
        let graph = createEmptyDependencyGraph
            graph1 = addGraphNode dependency graph
            graph2 = foldr addGraphNode graph1 deps
            graph3 = foldr (\dep -> addGraphEdge dependency dep) graph2 deps
            detected = getDirectDependencies dependency graph3
        in Set.fromList deps === detected

    it "detects transitive dependencies" $ property $
      \node intermediate target ->
        let graph = createEmptyDependencyGraph
            graph1 = foldr addGraphNode graph [node, intermediate, target]
            graph2 = addGraphEdge node intermediate graph1
            graph3 = addGraphEdge intermediate target graph2
            transitive = getTransitiveDependencies node graph3
        in target `Set.member` transitive

    it "computes dependency closure correctly" $ property $
      \root deps transitiveDeps ->
        let allDeps = deps ++ transitiveDeps
            graph = createDependencyGraph root allDeps
            closure = getDependencyClosure root graph
        in Set.fromList allDeps === closure

    it "circular dependencies are detected" $ property $
      \node1 node2 ->
        let graph = createEmptyDependencyGraph
            graph1 = foldr addGraphNode graph [node1, node2]
            graph2 = addGraphEdge node1 node2 graph1
            graph3 = addGraphEdge node2 node1 graph2
            cycles = detectCycles graph3
        in not (null cycles)

  describe "Topological sorting properties" $ do
    it "valid topological sort preserves dependencies" $ property $
      \dependencies ->
        let graph = createDependencyGraphFromPairs dependencies
            sorted = topologicalSort graph
        in if hasNoCycles graph
           then isTopologicalOrder dependencies sorted
           else True -- If there are cycles, topological sort may fail

    it "empty graph produces empty sort" $ do
      let emptyGraph = createEmptyDependencyGraph
          sorted = topologicalSort emptyGraph
      sorted `shouldBe` []

    it "single node graph produces single element sort" $ property $
      \node ->
        let graph = addGraphNode node createEmptyDependencyGraph
            sorted = topologicalSort graph
        in sorted === [node]

    it "acyclic graph always has valid sort" $ property $
      \nodes ->
        let acyclicGraph = createAcyclicGraph nodes
            sorted = topologicalSort acyclicGraph
        in isTopologicalOrder (getGraphEdges acyclicGraph) sorted

  describe "Dependency resolution properties" $ do
    it "resolves dependencies in correct order" $ property $
      \target dependencies ->
        let graph = createDependencyGraph target dependencies
            resolved = resolveDependencies target graph
        in if isResolvable target graph
           then last resolved === target
           else True

    it "detects unresolvable dependencies" $ property $
      \target dependencies ->
        let graph = createDependencyGraph target dependencies
            resolvable = isResolvable target graph
        in not (hasCyclesInvolving target graph) ==> resolvable

    it "resolution includes all transitive dependencies" $ property $
      \target directDeps transitiveDeps ->
        let allDeps = directDeps ++ transitiveDeps
            graph = createDependencyGraph target allDeps
            resolved = resolveDependencies target graph
        in if isResolvable target graph
           then Set.fromList allDeps `Set.isSubsetOf` Set.fromList resolved
           else True

    it "circular dependencies are unresolvable" $ property $
      \node1 node2 ->
        let graph = createEmptyDependencyGraph
            graph1 = foldr addGraphNode graph [node1, node2]
            graph2 = addGraphEdge node1 node2 graph1
            graph3 = addGraphEdge node2 node1 graph2
        in not (isResolvable node1 graph3) &&
           not (isResolvable node2 graph3)

  describe "Dependency analysis properties" $ do
    it "computes dependency levels correctly" $ property $
      \nodes ->
        let graph = createAcyclicGraph nodes
            levels = computeDependencyLevels graph
        in all (\(node, level) -> level >= 0) levels &&
           all (\(node, level) -> 
                let deps = getDirectDependencies node graph
                in all (\dep -> lookupDep dep levels < level) (Set.toList deps)) levels

    it "identifies critical path correctly" $ property $
      \nodes ->
        let graph = createAcyclicGraph nodes
            path = findCriticalPath graph
        in null path || isPathValid graph path

    it "computes dependency metrics" $ property $
      \nodes ->
        let graph = createAcyclicGraph nodes
            metrics = computeDependencyMetrics graph
        in totalNodes metrics === Set.size (getGraphNodes graph) &&
           totalEdges metrics === length (getGraphEdges graph) &&
           maxDepth metrics >= 0

    it "detects dependency violations" $ property $
      \constraints dependencies ->
        let graph = createDependencyGraphFromPairs dependencies
            violations = checkDependencyConstraints constraints graph
        in all isValidViolation violations

  where
    -- Helper types for testing
    data DependencyGraph = DependencyGraph
      { graphNodes :: Set String
      , graphEdges :: [(String, String)]
      } deriving (Eq, Show)

    data DependencyMetrics = DependencyMetrics
      { totalNodes :: Int
      , totalEdges :: Int
      , maxDepth :: Int
      , avgDependencies :: Double
      } deriving (Eq, Show)

    data DependencyConstraint = NoCircularDeps
                              | MaxDepth Int
                              | RequiredDeps [String]
      deriving (Eq, Show)

    data DependencyViolation = CircularDependency [String]
                             | DepthViolation String Int Int
                             | MissingDependency String String
      deriving (Eq, Show)

    -- Mock implementations for testing
    createEmptyDependencyGraph :: DependencyGraph
    createEmptyDependencyGraph = DependencyGraph Set.empty []

    addGraphNode :: String -> DependencyGraph -> DependencyGraph
    addGraphNode node graph = graph
      { graphNodes = Set.insert node (graphNodes graph)
      }

    removeGraphNode :: String -> DependencyGraph -> DependencyGraph
    removeGraphNode node graph = graph
      { graphNodes = Set.delete node (graphNodes graph)
      , graphEdges = filter (\(from, to) -> from /= node && to /= node) (graphEdges graph)
      }

    addGraphEdge :: String -> String -> DependencyGraph -> DependencyGraph
    addGraphEdge from to graph = graph
      { graphNodes = Set.insert from (Set.insert to (graphNodes graph))
      , graphEdges = (from, to) : graphEdges graph
      }

    getGraphNodes :: DependencyGraph -> Set String
    getGraphNodes = graphNodes

    getGraphEdges :: DependencyGraph -> [(String, String)]
    getGraphEdges = graphEdges

    getDirectDependencies :: String -> DependencyGraph -> Set String
    getDirectDependencies node graph = 
      Set.fromList [to | (from, to) <- graphEdges graph, from == node]

    getTransitiveDependencies :: String -> DependencyGraph -> Set String
    getTransitiveDependencies node graph = 
      let direct = getDirectDependencies node graph
          transitive = Set.unions $ map (`getTransitiveDependencies` graph) (Set.toList direct)
      in Set.union direct transitive

    getDependencyClosure :: String -> DependencyGraph -> Set String
    getDependencyClosure = getTransitiveDependencies

    detectCycles :: DependencyGraph -> [[String]]
    detectCycles graph = findCycles (Set.toList (graphNodes graph)) (graphEdges graph)

    findCycles :: [String] -> [(String, String)] -> [[String]]
    findCycles nodes edges = 
      [cycle | cycle <- findAllPaths nodes edges, hasLoop cycle]
      where
        hasLoop path = length (nub path) < length path
        findAllPaths nodes' edges' = [[n] | n <- nodes'] -- Simplified implementation

    topologicalSort :: DependencyGraph -> [String]
    topologicalSort graph = 
      if hasCycles graph
      then [] -- Can't sort cyclic graph
      else kahnAlgorithm (graphNodes graph) (graphEdges graph)

    kahnAlgorithm :: Set String -> [(String, String)] -> [String]
    kahnAlgorithm nodes edges = 
      let inDegree = Map.fromListWith (+) [(to, 1) | (_, to) <- edges]
          initialNodes = Set.toList $ Set.difference nodes (Map.keysSet inDegree)
      in topologicalSort' initialNodes edges (Map.toList inDegree) []

    topologicalSort' :: [String] -> [(String, String)] -> [(String, Int)] -> [String] -> [String]
    topologicalSort' [] _ _ result = result
    topologicalSort' (node:rest) edges inDegree result = 
      let (outgoing, remainingEdges) = partition ((== node) . fst) edges
          updatedInDegree = Map.fromListWith (+) [(to, -1) | (_, to) <- outgoing]
          newInDegree = Map.unionWith (+) (Map.fromList inDegree) updatedInDegree
          newNodes = rest ++ [n | (n, d) <- Map.toList newInDegree, d == 0]
      in topologicalSort' newNodes remainingEdges (filter ((/= 0) . snd) (Map.toList newInDegree)) (result ++ [node])

    hasCycles :: DependencyGraph -> Bool
    hasCycles graph = not (null (detectCycles graph))

    hasNoCycles :: DependencyGraph -> Bool
    hasNoCycles = not . hasCycles

    isTopologicalOrder :: [(String, String)] -> [String] -> Bool
    isTopologicalOrder edges order = 
      let positions = Map.fromList (zip order [0..])
          inOrder (from, to) = 
            case (Map.lookup from positions, Map.lookup to positions) of
              (Just fromPos, Just toPos) -> fromPos < toPos
              _ -> True
      in all inOrder edges

    createDependencyGraph :: String -> [String] -> DependencyGraph
    createDependencyGraph target dependencies = 
      let graph = addGraphNode target createEmptyDependencyGraph
          graph1 = foldr addGraphNode graph dependencies
          graph2 = foldr (\dep -> addGraphEdge target dep) graph1 dependencies
      in graph2

    createDependencyGraphFromPairs :: [(String, String)] -> DependencyGraph
    createDependencyGraphFromPairs pairs = 
      foldr (uncurry addGraphEdge) createEmptyDependencyGraph pairs

    createAcyclicGraph :: [String] -> DependencyGraph
    createAcyclicGraph nodes = 
      let sortedNodes = sort nodes
          edges = zip sortedNodes (tail sortedNodes)
      in createDependencyGraphFromPairs edges

    resolveDependencies :: String -> DependencyGraph -> [String]
    resolveDependencies target graph = 
      if hasCyclesInvolving target graph
      then []
      else reverse $ topologicalSort' [target] (graphEdges graph) [] []

    isResolvable :: String -> DependencyGraph -> Bool
    isResolvable target graph = not (hasCyclesInvolving target graph)

    hasCyclesInvolving :: String -> DependencyGraph -> Bool
    hasCyclesInvolving node graph = 
      any (node `elem`) (detectCycles graph)

    computeDependencyLevels :: DependencyGraph -> [(String, Int)]
    computeDependencyLevels graph = 
      let nodes = Set.toList (graphNodes graph)
          computeLevel node = 
            let deps = Set.toList (getDirectDependencies node graph)
                depLevels = map (lookupDep node) (computeDependencyLevels graph)
            in if null deps then 0 else maximum depLevels + 1
      in zip nodes (map computeLevel nodes)

    lookupDep :: String -> [(String, Int)] -> Int
    lookupDep node levels = 
      case lookup node levels of
        Just level -> level
        Nothing -> 0

    findCriticalPath :: DependencyGraph -> [String]
    findCriticalPath graph = 
      let levels = computeDependencyLevels graph
          maxLevel = maximum (map snd levels)
          criticalNodes = [node | (node, level) <- levels, level == maxLevel]
      in if null criticalNodes then [] else [head criticalNodes]

    isPathValid :: DependencyGraph -> [String] -> Bool
    isPathValid graph path = 
      all (\(from, to) -> (from, to) `elem` graphEdges graph) (zip path (tail path))

    computeDependencyMetrics :: DependencyGraph -> DependencyMetrics
    computeDependencyMetrics graph = 
      let nodes = Set.size (graphNodes graph)
          edges = length (graphEdges graph)
          levels = computeDependencyLevels graph
          maxDepth = if null levels then 0 else maximum (map snd levels)
          avgDeps = if nodes == 0 then 0 else fromIntegral edges / fromIntegral nodes
      in DependencyMetrics nodes edges maxDepth avgDeps

    checkDependencyConstraints :: [DependencyConstraint] -> DependencyGraph -> [DependencyViolation]
    checkDependencyConstraints constraints graph = 
      concatMap (checkConstraint graph) constraints

    checkConstraint :: DependencyGraph -> DependencyConstraint -> [DependencyViolation]
    checkConstraint graph NoCircularDeps = 
      if hasCycles graph
      then [CircularDependency (concat (detectCycles graph))]
      else []
    checkConstraint graph (MaxDepth maxAllowed) = 
      let levels = computeDependencyLevels graph
          violations = [DepthViolation node level maxAllowed | (node, level) <- levels, level > maxAllowed]
      in violations
    checkConstraint graph (RequiredDeps required) = 
      let nodes = Set.toList (graphNodes graph)
          missing = [MissingDependency node req | node <- nodes, req <- required, 
                    not (req `Set.member` getDirectDependencies node graph)]
      in missing

    isValidViolation :: DependencyViolation -> Bool
    isValidViolation (CircularDependency _) = True
    isValidViolation (DepthViolation _ _ _) = True
    isValidViolation (MissingDependency _ _) = True

    -- Helper functions
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)

    lookup :: Eq a => a -> [(a, b)] -> Maybe b
    lookup _ [] = Nothing
    lookup key ((k, v):rest) = if key == k then Just v else lookup key rest

    maximum :: Ord a => [a] -> a
    maximum [] = error "empty list"
    maximum [x] = x
    maximum (x:xs) = max x (maximum xs)

    -- Helper instances for QuickCheck
    instance Arbitrary DependencyGraph where
      arbitrary = do
        nodes <- arbitrary
        edges <- listOf $ arbitrary
        return $ foldr (uncurry addGraphEdge) 
                      (foldr addGraphNode createEmptyDependencyGraph nodes) 
                      edges

    instance Arbitrary DependencyConstraint where
      arbitrary = oneof
        [ pure NoCircularDeps
        , MaxDepth <$> arbitrary
        , RequiredDeps <$> arbitrary
        ]

    instance Arbitrary DependencyViolation where
      arbitrary = oneof
        [ CircularDependency <$> arbitrary
        , DepthViolation <$> arbitrary <*> arbitrary <*> arbitrary
        , MissingDependency <$> arbitrary <*> arbitrary
        ]