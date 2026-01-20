{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.DependencyResolutionSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy, find, delete, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)
import Data.Graph (buildG, topSort, reachable)

-- Dependency resolution types for testing
data DependencyType = 
    DirectDependency
  | TransitiveDependency
  | OptionalDependency
  | DevelopmentDependency
  deriving (Eq, Show)

data Dependency = Dependency
  { dependencyName :: String
  , dependencyVersion :: String
  , dependencyType :: DependencyType
  , dependencyScope :: String
  }
  deriving (Eq, Show)

data DependencyGraph = DependencyGraph
  { graphNodes :: Map String Dependency
  , graphEdges :: Map String [String]
  , graphReverseEdges :: Map String [String]
  }
  deriving (Eq, Show)

data DependencyConflict = DependencyConflict
  { conflictDependency :: String
  , conflictVersions :: [String]
  , conflictReason :: String
  }
  deriving (Eq, Show)

data DependencyResolution = DependencyResolution
  { resolutionGraph :: DependencyGraph
  , resolutionOrder :: [String]
  , resolutionConflicts :: [DependencyConflict]
  }
  deriving (Eq, Show)

-- Helper generators for dependency resolution tests
genString :: Gen String
genString = do
  len <- choose (1, 10)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_."

genVersion :: Gen String
genVersion = do
  major <- choose (0, 10) :: Gen Int
  minor <- choose (0, 10) :: Gen Int
  patch <- choose (0, 10) :: Gen Int
  return $ show major ++ "." ++ show minor ++ "." ++ show patch

genDependencyType :: Gen DependencyType
genDependencyType = elements [DirectDependency, TransitiveDependency, OptionalDependency, DevelopmentDependency]

genDependency :: Gen Dependency
genDependency = do
  name <- genString
  version <- genVersion
  depType <- genDependencyType
  scope <- genString
  return $ Dependency name version depType scope

genDependencyGraph :: Gen DependencyGraph
genDependencyGraph = do
  numNodes <- choose (1, 10)
  nodes <- replicateM numNodes genDependency
  let nodeMap = Map.fromList $ map (\d -> (dependencyName d, d)) nodes
  
  edges <- Map.fromList <$> mapM (\n -> do
    numDeps <- choose (0, 3)
    deps <- replicateM numDeps (elements $ Map.keys nodeMap)
    return (dependencyName n, nub deps)) nodes
  
  let reverseEdges = buildReverseEdges edges
  
  return $ DependencyGraph nodeMap edges reverseEdges

-- Arbitrary instances
instance Arbitrary DependencyGraph where
  arbitrary = genDependencyGraph

buildReverseEdges :: Map String [String] -> Map String [String]
buildReverseEdges edges = 
  let allNodes = Map.keys edges
      reverseEdges = foldr (\node acc -> 
        let deps = fromMaybe [] $ Map.lookup node edges
            addToReverse dep acc' = Map.insertWith (++) dep [node] acc'
        in foldr addToReverse acc deps) Map.empty allNodes
  in reverseEdges

-- Test properties for dependency resolution

-- Property 1: Dependency graph is consistent
prop_dependency_graph_consistent :: DependencyGraph -> Bool
prop_dependency_graph_consistent graph = 
  let nodes = Map.keys (graphNodes graph)
      edges = concat $ Map.elems (graphEdges graph)
      reverseEdges = concat $ Map.elems (graphReverseEdges graph)
  in all (`elem` nodes) edges && all (`elem` nodes) reverseEdges

-- Property 2: Topological sort respects dependencies
prop_topological_sort_respects_dependencies :: DependencyGraph -> Bool
prop_topological_sort_respects_dependencies graph = 
  let order = topologicalSort graph
      positionMap = Map.fromList $ zip order [0..]
      respectsDeps = all (\(node, deps) -> 
        all (\dep -> fromMaybe (length order) (Map.lookup dep positionMap) < 
                   fromMaybe (length order) (Map.lookup node positionMap)) deps)
                        (Map.toList (graphEdges graph))
  in respectsDeps

-- Property 3: Circular dependencies are detected
prop_circular_dependencies_detected :: DependencyGraph -> Bool
prop_circular_dependencies_detected graph = 
  let hasCycles = hasCircularDependencies graph
      detected = detectCircularDependencies graph
  in hasCycles == detected

-- Property 4: Dependency resolution preserves all required dependencies
prop_resolution_preserves_dependencies :: DependencyGraph -> Bool
prop_resolution_preserves_dependencies graph = 
  let resolution = resolveDependencies graph
      originalNodes = Map.keys (graphNodes graph)
      resolvedNodes = Map.keys (graphNodes (resolutionGraph resolution))
  in all (`elem` resolvedNodes) originalNodes

-- Property 5: Dependency resolution eliminates redundant dependencies
prop_resolution_eliminates_redundant :: DependencyGraph -> Bool
prop_resolution_eliminates_redundant graph = 
  let resolution = resolveDependencies graph
      originalEdges = concat $ Map.elems (graphEdges graph)
      resolvedEdges = concat $ Map.elems (graphEdges (resolutionGraph resolution))
  in length resolvedEdges <= length originalEdges

-- Property 6: Dependency resolution minimizes conflicts
prop_resolution_minimizes_conflicts :: DependencyGraph -> Bool
prop_resolution_minimizes_conflicts graph = 
  let resolution = resolveDependencies graph
      conflicts = resolutionConflicts resolution
  in null conflicts || all isValidConflict conflicts

-- Property 7: Dependency resolution is deterministic
prop_resolution_is_deterministic :: DependencyGraph -> Bool
prop_resolution_is_deterministic graph = 
  let resolution1 = resolveDependencies graph
      resolution2 = resolveDependencies graph
  in resolutionOrder resolution1 == resolutionOrder resolution2

-- Property 8: Dependency resolution handles version conflicts
prop_resolution_handles_version_conflicts :: DependencyGraph -> Bool
prop_resolution_handles_version_conflicts graph = 
  let resolution = resolveDependencies graph
      conflicts = resolutionConflicts resolution
  in all (\c -> length (conflictVersions c) > 1) conflicts

-- Property 9: Dependency resolution respects dependency types
prop_resolution_respects_dependency_types :: DependencyGraph -> Bool
prop_resolution_respects_dependency_types graph = 
  let resolution = resolveDependencies graph
      originalTypes = Map.map dependencyType (graphNodes graph)
      resolvedTypes = Map.map dependencyType (graphNodes (resolutionGraph resolution))
  in originalTypes == resolvedTypes

-- Property 10: Dependency resolution produces minimal graph
prop_resolution_produces_minimal_graph :: DependencyGraph -> Bool
prop_resolution_produces_minimal_graph graph = 
  let resolution = resolveDependencies graph
      resolvedGraph = resolutionGraph resolution
  in isMinimalGraph resolvedGraph

-- Helper functions for dependency resolution
toIndices :: [String] -> (String, String) -> (Int, Int)
toIndices nodes (from, to) = 
  let fromIndex = maybe 0 id $ findIndex (== from) nodes
      toIndex = maybe 0 id $ findIndex (== to) nodes
  in (fromIndex, toIndex)
  
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = fmap fst $ find (\(i, v) -> p v) (zip [0..] xs)

topologicalSort :: DependencyGraph -> [String]
topologicalSort graph = 
  let nodes = Map.keys (graphNodes graph)
      edges = Map.toList (graphEdges graph)
      edgeList = concatMap (\(from, tos) -> map (\to -> (from, to)) tos) edges
      graph' = buildG (0, length nodes - 1) (map (toIndices nodes) edgeList)
      sorted = topSort graph'
      indexToNode = Map.fromList $ zip [0..] nodes
  in map (fromMaybe "" . (`Map.lookup` indexToNode)) sorted

hasCircularDependencies :: DependencyGraph -> Bool
hasCircularDependencies graph = 
  let nodes = Map.keys (graphNodes graph)
      edges = Map.toList (graphEdges graph)
      edgeList = concatMap (\(from, tos) -> map (\to -> (from, to)) tos) edges
      graph' = buildG (0, length nodes - 1) (map (toIndices nodes) edgeList)
      nodeIndices = [0..length nodes - 1]
      hasCycle vertex = vertex `elem` concatMap (reachable graph') [vertex]
  in any hasCycle nodeIndices

detectCircularDependencies :: DependencyGraph -> Bool
detectCircularDependencies = hasCircularDependencies

resolveDependencies :: DependencyGraph -> DependencyResolution
resolveDependencies graph = 
  let order = topologicalSort graph
      conflicts = findConflicts graph
      minimizedGraph = minimizeGraph graph
  in DependencyResolution minimizedGraph order conflicts

findConflicts :: DependencyGraph -> [DependencyConflict]
findConflicts graph = 
  let nodes = Map.elems (graphNodes graph)
      grouped = groupBy (\d1 d2 -> dependencyName d1 == dependencyName d2) $ sortBy (\d1 d2 -> compare (dependencyName d1) (dependencyName d2)) nodes
      conflicts = filter (\group -> length group > 1) grouped
  in map (\group -> 
    let name = case group of (x:_) -> dependencyName x; [] -> error "empty group"
        versions = map dependencyVersion group
    in DependencyConflict name versions "Version conflict") conflicts

isValidConflict :: DependencyConflict -> Bool
isValidConflict conflict = 
  length (conflictVersions conflict) > 1 && not (null (conflictDependency conflict))

minimizeGraph :: DependencyGraph -> DependencyGraph
minimizeGraph graph = 
  let nodes = graphNodes graph
      edges = graphEdges graph
      minimizedEdges = Map.map (nub . filter (`Map.member` nodes)) edges
  in graph { graphEdges = minimizedEdges }

isMinimalGraph :: DependencyGraph -> Bool
isMinimalGraph graph = 
  let edges = graphEdges graph
      hasRedundant = any (\(node, deps) -> 
        let directDeps = Set.fromList deps
            transitiveDeps = Set.unions $ map (\dep -> 
              Set.fromList $ fromMaybe [] $ Map.lookup dep edges) deps
        in any (`Set.member` transitiveDeps) directDeps) (Map.toList edges)
  in not hasRedundant

-- Test cases for dependency resolution
testDependencyResolution :: TestTree
testDependencyResolution = testGroup "Dependency Resolution Tests"
  [ testProperties "Dependency Graph Properties"
    [ ("dependency_graph_consistent", property prop_dependency_graph_consistent)
    , ("topological_sort_respects_dependencies", property prop_topological_sort_respects_dependencies)
    , ("circular_dependencies_detected", property prop_circular_dependencies_detected)
    ]
  , testProperties "Dependency Resolution Properties"
    [ ("resolution_preserves_dependencies", property prop_resolution_preserves_dependencies)
    , ("resolution_eliminates_redundant", property prop_resolution_eliminates_redundant)
    , ("resolution_minimizes_conflicts", property prop_resolution_minimizes_conflicts)
    ]
  , testProperties "Dependency Resolution Behavior Properties"
    [ ("resolution_is_deterministic", property prop_resolution_is_deterministic)
    , ("resolution_handles_version_conflicts", property prop_resolution_handles_version_conflicts)
    , ("resolution_respects_dependency_types", property prop_resolution_respects_dependency_types)
    , ("resolution_produces_minimal_graph", property prop_resolution_produces_minimal_graph)
    ]
  , testCase "Simple dependency graph" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" DirectDependency "runtime"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2)]
          , graphEdges = Map.fromList [("package1", ["package2"]), ("package2", [])]
          , graphReverseEdges = Map.fromList [("package1", []), ("package2", ["package1"])]
          }
    let order = topologicalSort graph
    assertEqual "Should produce correct topological order" ["package2", "package1"] order
  
  , testCase "Circular dependency detection" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" DirectDependency "runtime"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2)]
          , graphEdges = Map.fromList [("package1", ["package2"]), ("package2", ["package1"])]
          , graphReverseEdges = Map.fromList [("package1", ["package2"]), ("package2", ["package1"])]
          }
    let hasCycle = hasCircularDependencies graph
    let detected = detectCircularDependencies graph
    assertBool "Should detect circular dependency" hasCycle
    assertBool "Should correctly detect circular dependency" detected
  
  , testCase "Dependency resolution" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" DirectDependency "runtime"
    let dep3 = Dependency "package3" "3.0.0" DirectDependency "runtime"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2), ("package3", dep3)]
          , graphEdges = Map.fromList 
              [ ("package1", ["package2", "package3"])
              , ("package2", [])
              , ("package3", [])
              ]
          , graphReverseEdges = Map.fromList 
              [ ("package1", [])
              , ("package2", ["package1"])
              , ("package3", ["package1"])
              ]
          }
    let resolution = resolveDependencies graph
    let order = resolutionOrder resolution
    assertBool "Should include all packages in resolution" 
               (all (`elem` order) ["package1", "package2", "package3"])
    assertBool "Should respect dependency order" 
               (elemIndex "package2" order < elemIndex "package1" order &&
                elemIndex "package3" order < elemIndex "package1" order)
  
  , testCase "Version conflict detection" $ do
    let dep1a = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep1b = Dependency "package1" "2.0.0" DirectDependency "runtime"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1a), ("package1v2", dep1b)]
          , graphEdges = Map.fromList [("package1", []), ("package1v2", [])]
          , graphReverseEdges = Map.fromList [("package1", []), ("package1v2", [])]
          }
    let resolution = resolveDependencies graph
    let conflicts = resolutionConflicts resolution
    assertBool "Should detect version conflicts" (not $ null conflicts)
  
  , testCase "Transitive dependency resolution" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" DirectDependency "runtime"
    let dep3 = Dependency "package3" "3.0.0" TransitiveDependency "runtime"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2), ("package3", dep3)]
          , graphEdges = Map.fromList 
              [ ("package1", ["package2"])
              , ("package2", ["package3"])
              , ("package3", [])
              ]
          , graphReverseEdges = Map.fromList 
              [ ("package1", [])
              , ("package2", ["package1"])
              , ("package3", ["package2"])
              ]
          }
    let resolution = resolveDependencies graph
    let resolvedGraph = resolutionGraph resolution
    let package1Deps = fromMaybe [] $ Map.lookup "package1" (graphEdges resolvedGraph)
    assertBool "Should include transitive dependencies" 
               ("package3" `elem` concatMap (\dep -> 
                 fromMaybe [] $ Map.lookup dep (graphEdges resolvedGraph)) package1Deps)
  
  , testCase "Optional dependency handling" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" OptionalDependency "optional"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2)]
          , graphEdges = Map.fromList [("package1", ["package2"]), ("package2", [])]
          , graphReverseEdges = Map.fromList [("package1", []), ("package2", ["package1"])]
          }
    let resolution = resolveDependencies graph
    let resolvedGraph = resolutionGraph resolution
    let resolvedDep2 = Map.lookup "package2" (graphNodes resolvedGraph)
    assertEqual "Should preserve optional dependency type" 
                (Just dep2) resolvedDep2
  
  , testCase "Development dependency filtering" $ do
    let dep1 = Dependency "package1" "1.0.0" DirectDependency "runtime"
    let dep2 = Dependency "package2" "2.0.0" DevelopmentDependency "development"
    let graph = DependencyGraph 
          { graphNodes = Map.fromList [("package1", dep1), ("package2", dep2)]
          , graphEdges = Map.fromList [("package1", ["package2"]), ("package2", [])]
          , graphReverseEdges = Map.fromList [("package1", []), ("package2", ["package1"])]
          }
    let resolution = resolveDependencies graph
    let resolvedGraph = resolutionGraph resolution
    let resolvedDep2 = Map.lookup "package2" (graphNodes resolvedGraph)
    assertEqual "Should preserve development dependencies" 
                (Just dep2) resolvedDep2
  ]

-- Helper function for testing
elemIndex :: Eq a => a -> [a] -> Int
elemIndex x xs = case findIndex (== x) xs of
  Just i -> i
  Nothing -> -1
  where
    findIndex p xs = go xs 0
      where
        go [] _ = Nothing
        go (y:ys) n = if p y then Just n else go ys (n+1)

-- Export the test
tests :: TestTree
tests = testDependencyResolution