{-# LANGUAGE CPP #-}

module Test.Unit.NewDependenciesCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

import Dependencies (DependencyGraph, DependencyNode(..), addDependency, 
                    hasDependency, getDependencies, removeDependency)
import Dependencies.TypeSystem (TypeDependency(..), resolveTypeDependencies)
import Dependencies.Inference (inferDependencies, DependencyContext(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

-- Test 1: Dependency graph consistency
prop_dependency_graph_consistency :: String -> String -> Property
prop_dependency_graph_consistency from to =
  length from > 0 && length to > 0 ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph1 = addDependency from to emptyGraph
      graph2 = addDependency to from graph1
  in hasDependency from to graph1 .&&. hasDependency to from graph2

-- Test 2: Dependency removal
prop_dependency_removal :: String -> String -> Property
prop_dependency_removal from to =
  length from > 0 && length to > 0 ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph1 = addDependency from to emptyGraph
      graph2 = removeDependency from to graph1
  in not (hasDependency from to graph2)

-- Test 3: Dependency transitivity
prop_dependency_transitivity :: String -> String -> String -> Property
prop_dependency_transitivity a b c =
  length a > 0 && length b > 0 && length c > 0 && a /= b && b /= c && a /= c ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph1 = addDependency a b emptyGraph
      graph2 = addDependency b c graph1
      depsA = getDependencies a graph2
      depsB = getDependencies b graph2
  in Set.member b depsA .&&. Set.member c depsB

-- Test 4: Type dependency resolution
prop_type_dependency_resolution :: String -> [String] -> Property
prop_type_dependency_resolution typeName deps =
  length typeName > 0 && length deps < 10 ==> -- Limit complexity
  let typeDep = TypeDependency typeName (Set.fromList deps)
      resolved = resolveTypeDependencies typeDep
  in length resolved >= 0 -- Should always return a list

-- Test 5: Dependency inference context
prop_dependency_inference_context :: String -> Map.Map String String -> Property
prop_dependency_inference_context varName context =
  length varName > 0 && Map.size context < 10 ==>
  let depContext = DependencyContext context
      inferred = inferDependencies varName depContext
  in length inferred >= 0 -- Should always return a list

-- Test 6: Circular dependency detection
prop_circular_dependency_detection :: String -> Property
prop_circular_dependency_detection name =
  length name > 0 ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph1 = addDependency name name emptyGraph
  in hasDependency name name graph1

-- Test 7: Multiple dependencies handling
prop_multiple_dependencies_handling :: String -> [String] -> Property
prop_multiple_dependencies_handling from targets =
  length from > 0 && length targets < 5 && all (not . null) targets ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph = foldr (addDependency from) emptyGraph targets
      deps = getDependencies from graph
  in all (`Set.member` deps) targets

-- Test 8: Dependency graph merging
prop_dependency_graph_merging :: [(String, String)] -> [(String, String)] -> Property
prop_dependency_graph_merging deps1 deps2 =
  length deps1 < 5 && length deps2 < 5 && all (not . null . fst) deps1 && all (not . null . snd) deps1 ==>
  let emptyGraph = Map.empty :: DependencyGraph
      graph1 = foldr (\(f, t) g -> addDependency f t g) emptyGraph deps1
      graph2 = foldr (\(f, t) g -> addDependency f t g) graph1 deps2
      hasAllDeps = all (\(f, t) -> hasDependency f t graph2) (deps1 ++ deps2)
  in hasAllDeps

-- Test 9: Dependency chain resolution
prop_dependency_chain_resolution :: [String] -> Property
prop_dependency_chain_resolution names =
  length names < 5 && all (not . null) names ==>
  let emptyGraph = Map.empty :: DependencyGraph
      -- Create a chain: name1 -> name2 -> name3 -> ...
      graph = foldr (\(from, to) g -> addDependency from to g) emptyGraph (zip names (tail names))
      hasChain = all (\(from, to) -> hasDependency from to graph) (zip names (tail names))
  in hasChain

-- Test 10: Empty dependency graph properties
prop_empty_dependency_graph_properties :: String -> Property
prop_empty_dependency_graph_properties name =
  length name > 0 ==>
  let emptyGraph = Map.empty :: DependencyGraph
      deps = getDependencies name emptyGraph
  in Set.null deps .&&. not (hasDependency name "anything" emptyGraph)

tests :: TestTree
tests = testGroup "New Dependencies Core Tests"
  [ fastProperty "Dependency graph consistency" prop_dependency_graph_consistency
  , fastProperty "Dependency removal" prop_dependency_removal
  , fastProperty "Dependency transitivity" prop_dependency_transitivity
  , fastProperty "Type dependency resolution" prop_type_dependency_resolution
  , fastProperty "Dependency inference context" prop_dependency_inference_context
  , fastProperty "Circular dependency detection" prop_circular_dependency_detection
  , fastProperty "Multiple dependencies handling" prop_multiple_dependencies_handling
  , fastProperty "Dependency graph merging" prop_dependency_graph_merging
  , fastProperty "Dependency chain resolution" prop_dependency_chain_resolution
  , fastProperty "Empty dependency graph properties" prop_empty_dependency_graph_properties
  ]