module Test.Unit.DependenciesCycleDetectionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, startPos)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

-- Test cases for basic cycle detection
testBasicCycleDetection :: TestTree
testBasicCycleDetection = testGroup "Basic cycle detection tests"
  [ testCase "detect simple cycle" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeA])]
          cycles = detectCycles dependencies
      in length cycles @?= 1
  , testCase "detect no cycle in linear dependency" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeC]), (nodeC, [])]
          cycles = detectCycles dependencies
      in length cycles @?= 0
  , testCase "detect self-cycle" $
      let nodeA = createNode "A" startPos
          dependencies = Map.fromList [(nodeA, [nodeA])]
          cycles = detectCycles dependencies
      in length cycles @?= 1
  ]

-- Test cases for complex cycle detection
testComplexCycleDetection :: TestTree
testComplexCycleDetection = testGroup "Complex cycle detection tests"
  [ testCase "detect cycle in diamond dependency" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          nodeD = createNode "D" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB, nodeC])
            , (nodeB, [nodeD])
            , (nodeC, [nodeD])
            , (nodeD, [nodeA])  -- Creates cycle A->B->D->A and A->C->D->A
            ]
          cycles = detectCycles dependencies
      in length cycles @?= 2
  , testCase "detect multiple independent cycles" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          nodeD = createNode "D" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeA])
            , (nodeC, [nodeD])
            , (nodeD, [nodeC])
            ]
          cycles = detectCycles dependencies
      in length cycles @?= 2
  , testCase "detect cycle in complex graph" $
      let nodes = map (`createNode` startPos) ["A", "B", "C", "D", "E", "F"]
          (nodeA:nodeB:nodeC:nodeD:nodeE:nodeF:_) = nodes
          dependencies = Map.fromList 
            [ (nodeA, [nodeB, nodeC])
            , (nodeB, [nodeD])
            , (nodeC, [nodeD, nodeE])
            , (nodeD, [nodeF])
            , (nodeE, [nodeF])
            , (nodeF, [nodeB])  -- Creates cycle B->D->F->B
            ]
          cycles = detectCycles dependencies
      in length cycles @?= 1
  ]

-- Test cases for cycle path extraction
testCyclePathExtraction :: TestTree
testCyclePathExtraction = testGroup "Cycle path extraction tests"
  [ testCase "extract simple cycle path" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeA])]
          cycles = detectCycles dependencies
          paths = map extractCyclePath cycles
      in paths @?= [[nodeA, nodeB]]
  , testCase "extract complex cycle path" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            , (nodeC, [nodeA])
            ]
          cycles = detectCycles dependencies
          paths = map extractCyclePath cycles
      in paths @?= [[nodeA, nodeB, nodeC]]
  , testCase "extract shortest cycle path" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          nodeD = createNode "D" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB, nodeC])
            , (nodeB, [nodeD])
            , (nodeC, [nodeD])
            , (nodeD, [nodeA])
            ]
          cycles = detectCycles dependencies
          paths = map extractCyclePath cycles
      in any (\path -> length path == 3) paths @?= True
  ]

-- Test cases for cycle resolution
testCycleResolution :: TestTree
testCycleResolution = testGroup "Cycle resolution tests"
  [ testCase "break simple cycle" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeA])]
          resolved = breakCycles dependencies
          cycles = detectCycles resolved
      in length cycles @?= 0
  , testCase "break cycle by removing edge" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            , (nodeC, [nodeA])
            ]
          resolved = breakCycles dependencies
          cycles = detectCycles resolved
      in length cycles @?= 0
  , testCase "preserve acyclic dependencies" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            ]
          resolved = breakCycles dependencies
          aDeps = Map.findWithDefault [] nodeA resolved
          bDeps = Map.findWithDefault [] nodeB resolved
      in do aDeps @?= [nodeB]
            bDeps @?= [nodeC]
  ]

-- Test cases for dependency analysis
testDependencyAnalysis :: TestTree
testDependencyAnalysis = testGroup "Dependency analysis tests"
  [ testCase "topological sort" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            , (nodeC, [])
            ]
          sorted = topologicalSort dependencies
      in sorted @?= [nodeC, nodeB, nodeA]
  , testCase "topological sort fails with cycles" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeA])]
          sorted = topologicalSort dependencies
      in sorted @?= []
  , testCase "find transitive dependencies" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          nodeD = createNode "D" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            , (nodeC, [nodeD])
            ]
          transitive = findTransitiveDependencies nodeA dependencies
      in Set.fromList transitive @?= Set.fromList [nodeB, nodeC, nodeD]
  ]

-- Test cases for dependency validation
testDependencyValidation :: TestTree
testDependencyValidation = testGroup "Dependency validation tests"
  [ testCase "validate missing dependencies" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList [(nodeA, [nodeB, nodeC])]
          available = Set.fromList [nodeA, nodeB]
          missing = findMissingDependencies dependencies available
      in missing @?= Set.fromList [nodeC]
  , testCase "validate circular dependencies" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          dependencies = Map.fromList [(nodeA, [nodeB]), (nodeB, [nodeA])]
          validation = validateDependencies dependencies
      in hasCircularDependencies validation @?= True
  , testCase "validate dependency levels" $
      let nodeA = createNode "A" startPos
          nodeB = createNode "B" startPos
          nodeC = createNode "C" startPos
          dependencies = Map.fromList 
            [ (nodeA, [nodeB])
            , (nodeB, [nodeC])
            ]
          levels = calculateDependencyLevels dependencies
      in do Map.lookup nodeA levels @?= Just 2
            Map.lookup nodeB levels @?= Just 1
            Map.lookup nodeC levels @?= Just 0
  ]

-- Mock data types and functions for testing
data DependencyNode = DependencyNode
  { nodeId :: String
  , nodePosition :: SourcePos
  } deriving (Show, Eq, Ord)

data Cycle = Cycle
  { cycleNodes :: [DependencyNode]
  , cycleStart :: DependencyNode
  } deriving (Show, Eq)

data ValidationResult = ValidationResult
  { hasCircularDependencies :: Bool
  , missingNodes :: Set.Set DependencyNode
  , dependencyLevels :: Map.Map DependencyNode Int
  } deriving (Show, Eq)

-- Mock implementations
createNode :: String -> SourcePos -> DependencyNode
createNode name pos = DependencyNode name pos

detectCycles :: Map.Map DependencyNode [DependencyNode] -> [Cycle]
detectCycles dependencies = 
  let nodes = Map.keys dependencies
      visited = Set.empty
      recStack = Set.empty
  in findCyclesHelper nodes visited recStack dependencies []

findCyclesHelper :: [DependencyNode] -> Set.Set DependencyNode -> Set.Set DependencyNode -> 
  Map.Map DependencyNode [DependencyNode] -> [Cycle] -> [Cycle]
findCyclesHelper [] _ _ _ cycles = cycles
findCyclesHelper (node:rest) visited recStack dependencies cycles
  | node `Set.member` recStack = 
      let cycleNodes = takeWhile (/= node) (node : rest) ++ [node]
          cycle = Cycle cycleNodes node
      in findCyclesHelper rest visited recStack dependencies (cycle : cycles)
  | node `Set.member` visited = 
      findCyclesHelper rest visited recStack dependencies cycles
  | otherwise = 
      let newVisited = Set.insert node visited
          newRecStack = Set.insert node recStack
          deps = Map.findWithDefault [] node dependencies
          cyclesWithDeps = findCyclesHelper deps newVisited newRecStack dependencies cycles
          finalRecStack = Set.delete node newRecStack
      in findCyclesHelper rest newVisited finalRecStack dependencies cyclesWithDeps

extractCyclePath :: Cycle -> [DependencyNode]
extractCyclePath cycle = cycleNodes cycle

breakCycles :: Map.Map DependencyNode [DependencyNode] -> Map.Map DependencyNode [DependencyNode]
breakCycles dependencies = 
  let cycles = detectCycles dependencies
      edgesToRemove = concatMap (\cycle -> 
                                  let nodes = cycleNodes cycle
                                  in case nodes of
                                       [] -> []
                                       (first:rest) -> zip nodes (rest ++ [first])
                              ) cycles
      removeEdge (node, dep) deps = filter (/= dep) deps
  in Map.mapWithKey (\node deps -> foldr removeEdge deps (filter (\(n, _) -> n == node) edgesToRemove)) dependencies

topologicalSort :: Map.Map DependencyNode [DependencyNode] -> [DependencyNode]
topologicalSort dependencies = 
  let cycles = detectCycles dependencies
  in if null cycles
     then topologicalSortHelper dependencies (Map.keys dependencies) []
     else []

topologicalSortHelper :: Map.Map DependencyNode [DependencyNode] -> [DependencyNode] -> [DependencyNode] -> [DependencyNode]
topologicalSortHelper _ [] sorted = sorted
topologicalSortHelper dependencies nodes sorted = 
  let ready = filter (\node -> all (`elem` sorted) (Map.findWithDefault [] node dependencies)) nodes
      rest = filter (`notElem` ready) nodes
  in if null ready
     then []  -- Cycle detected
     else topologicalSortHelper dependencies rest (sorted ++ ready)

findTransitiveDependencies :: DependencyNode -> Map.Map DependencyNode [DependencyNode] -> [DependencyNode]
findTransitiveDependencies node dependencies = 
  let direct = Map.findWithDefault [] node dependencies
      indirect = concatMap (`findTransitiveDependencies` dependencies) direct
  in nub (direct ++ indirect)

findMissingDependencies :: Map.Map DependencyNode [DependencyNode] -> Set.Set DependencyNode -> Set.Set DependencyNode
findMissingDependencies dependencies available = 
  let allDeps = concat (Map.elems dependencies)
  in Set.difference (Set.fromList allDeps) available

validateDependencies :: Map.Map DependencyNode [DependencyNode] -> ValidationResult
validateDependencies dependencies = 
  let cycles = detectCycles dependencies
      allNodes = Set.fromList (Map.keys dependencies)
      allDeps = Set.fromList (concat (Map.elems dependencies))
      missing = Set.difference allDeps allNodes
      levels = calculateDependencyLevels dependencies
  in ValidationResult (not (null cycles)) missing levels

calculateDependencyLevels :: Map.Map DependencyNode [DependencyNode] -> Map.Map DependencyNode Int
calculateDependencyLevels dependencies = 
  let nodes = Map.keys dependencies
      levels = calculateLevelsHelper nodes dependencies 0 Map.empty
  in levels

calculateLevelsHelper :: [DependencyNode] -> Map.Map DependencyNode [DependencyNode] -> Int -> 
  Map.Map DependencyNode Int -> Map.Map DependencyNode Int
calculateLevelsHelper [] _ _ levels = levels
calculateLevelsHelper (node:rest) dependencies currentDepth levels = 
  let deps = Map.findWithDefault [] node dependencies
      depLevels = map (\dep -> Map.findWithDefault 0 dep levels) deps
      nodeLevel = if null deps then 0 else maximum depLevels + 1
      updatedLevels = Map.insert node nodeLevel levels
  in calculateLevelsHelper rest dependencies currentDepth updatedLevels

-- QuickCheck properties
prop_cycle_detection_consistency :: Map.Map DependencyNode [DependencyNode] -> Property
prop_cycle_detection_consistency dependencies = 
  let cycles = detectCycles dependencies
      resolved = breakCycles dependencies
      cyclesAfterBreak = detectCycles resolved
  in null cyclesAfterBreak === True

prop_topological_sort_properties :: Map.Map DependencyNode [DependencyNode] -> Property
prop_topological_sort_properties dependencies = 
  let cycles = detectCycles dependencies
      sorted = topologicalSort dependencies
  in if null cycles
     then length sorted === length (Map.keys dependencies)
     else sorted === []

prop_transitive_dependencies_transitive :: DependencyNode -> Map.Map DependencyNode [DependencyNode] -> Property
prop_transitive_dependencies_transitive node dependencies = 
  let transitive = findTransitiveDependencies node dependencies
      direct = Map.findWithDefault [] node dependencies
      indirect = concatMap (`findTransitiveDependencies` dependencies) direct
  in all (`elem` transitive) indirect === True

tests :: TestTree
tests = testGroup "Dependencies Cycle Detection Tests"
  [ testBasicCycleDetection
  , testComplexCycleDetection
  , testCyclePathExtraction
  , testCycleResolution
  , testDependencyAnalysis
  , testDependencyValidation
  -- , testProperty "cycle detection consistency" prop_cycle_detection_consistency
--  , testProperty "topological sort properties" prop_topological_sort_properties
--  , testProperty "transitive dependencies transitive" prop_transitive_dependencies_transitive
  ]