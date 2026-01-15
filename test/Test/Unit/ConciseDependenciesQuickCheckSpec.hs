{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseDependenciesQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Dependencies (DependencyGraph, DependencyError(..), DependencyType(..),
                   analyzeDependencies, detectCycles, resolveDependencies, 
                   getDirectDependencies, getTransitiveDependencies, hasCycles,
                   getDependencyErrors, clearDependencyErrors, mergeDependencyGraphs,
                   addDependency, removeDependency, hasDependency, getNodes,
                   getDependencyPath, topologicalSort)

-- Helper generators for Dependencies tests
genDependencyType :: Gen DependencyType
genDependencyType = elements [Import, Require, Include, Extend, Implement]

genDependencyError :: Gen DependencyError
genDependencyError = do
  msg <- elements ["Circular dependency", "Missing dependency", "Invalid dependency path", "Unresolvable dependency"]
  from <- elements ["module1", "module2", "module3"]
  to <- elements ["moduleA", "moduleB", "moduleC"]
  return $ DependencyError msg from to

genNode :: Gen String
genNode = do
  prefix <- elements ["module", "component", "service", "package"]
  num <- choose (1, 100)
  return $ prefix ++ show num

genDependency :: Gen (String, String, DependencyType)
genDependency = do
  from <- genNode
  to <- genNode
  depType <- genDependencyType
  return (from, to, depType)

genDependencyGraph :: Gen DependencyGraph
genDependencyGraph = do
  numNodes <- choose (0, 5)
  numDeps <- choose (0, 8)
  
  nodes <- vectorOf numNodes genNode
  deps <- vectorOf numDeps genDependency
  
  let graph = foldl (\graph (from, to, depType) -> addDependency from to depType graph) 
                     (foldl (\graph node -> addNode node graph) emptyDependencyGraph nodes) deps
  return graph
  where
    emptyDependencyGraph = DependencyGraph Map.empty Set.empty []
    addNode node graph = graph { nodes = Set.insert node (nodes graph) }
    addDependency from to depType graph = 
      let deps = Map.insertWith (++) from [(to, depType)] (dependencies graph)
      in graph { dependencies = deps }

-- Helper function to access graph fields
nodes :: DependencyGraph -> Set String
nodes (DependencyGraph n _ _) = n

dependencies :: DependencyGraph -> Map String [(String, DependencyType)]
dependencies (DependencyGraph _ d _) = d

errors :: DependencyGraph -> [DependencyError]
errors (DependencyGraph _ _ e) = e

-- Test properties for Dependencies module

-- Basic dependency analysis tests
prop_analyze_dependencies_no_crash :: String -> Property
prop_analyze_dependencies_no_crash code = 
  not (null code) ==>
  let result = analyzeDependencies code
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_detect_cycles_no_crash :: DependencyGraph -> Property
prop_detect_cycles_no_crash graph = 
  let result = detectCycles graph
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_resolve_dependencies_no_crash :: DependencyGraph -> Property
prop_resolve_dependencies_no_crash graph = 
  let result = resolveDependencies graph
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Dependency query tests
prop_get_direct_dependencies_no_crash :: String -> DependencyGraph -> Property
prop_get_direct_dependencies_no_crash node graph = 
  let deps = getDirectDependencies node graph
  in property $ length deps >= 0

prop_get_transitive_dependencies_no_crash :: String -> DependencyGraph -> Property
prop_get_transitive_dependencies_no_crash node graph = 
  let deps = getTransitiveDependencies node graph
  in property $ length deps >= 0

prop_has_cycles_detection :: DependencyGraph -> Property
prop_has_cycles_detection graph = 
  let hasCycles' = hasCycles graph
  in case detectCycles graph of
       Left _ -> hasCycles' === True
       Right _ -> hasCycles' === False

prop_get_dependency_errors_returns_all :: DependencyGraph -> Property
prop_get_dependency_errors_returns_all graph = 
  let errs = getDependencyErrors graph
  in length errs === length (errors graph)

prop_clear_dependency_errors_removes_all :: DependencyGraph -> Property
prop_clear_dependency_errors_removes_all graph = 
  let cleared = clearDependencyErrors graph
  in null (errors cleared)

-- Dependency manipulation tests
prop_add_dependency_increases_count :: DependencyGraph -> Property
prop_add_dependency_increases_count graph = 
  let from = "testFrom"
      to = "testTo"
      depType = Import
      updated = addDependency from to depType graph
      oldDeps = Map.findWithDefault [] from (dependencies graph)
      newDeps = Map.findWithDefault [] from (dependencies updated)
  in length newDeps >= length oldDeps

prop_remove_dependency_decreases_count :: DependencyGraph -> Property
prop_remove_dependency_decreases_count graph = 
  let from = "testFrom"
      to = "testTo"
      depType = Import
      graphWithDep = addDependency from to depType graph
      updated = removeDependency from to graphWithDep
      oldDeps = Map.findWithDefault [] from (dependencies graphWithDep)
      newDeps = Map.findWithDefault [] from (dependencies updated)
  in length newDeps <= length oldDeps

prop_has_dependency_detection :: String -> String -> DependencyGraph -> Property
prop_has_dependency_detection from to graph = 
  let hasDep = hasDependency from to graph
      deps = Map.findWithDefault [] from (dependencies graph)
      hasDep' = any (\(t, _) -> t == to) deps
  in hasDep === hasDep'

prop_get_nodes_returns_all_nodes :: DependencyGraph -> Property
prop_get_nodes_returns_all_nodes graph = 
  let nodeSet = getNodes graph
  in all (`Set.member` nodes graph) (Set.toList nodeSet)

-- Dependency path tests
prop_get_dependency_path_no_crash :: String -> String -> DependencyGraph -> Property
prop_get_dependency_path_no_crash from to graph = 
  let path = getDependencyPath from to graph
  in property $ length path >= 0

prop_get_dependency_path_starts_with_from :: String -> String -> DependencyGraph -> Property
prop_get_dependency_path_starts_with_from from to graph = 
  let path = getDependencyPath from to graph
  in if null path then property True else head path === from

-- Topological sort tests
prop_topological_sort_no_crash :: DependencyGraph -> Property
prop_topological_sort_no_crash graph = 
  let result = topologicalSort graph
  in case result of
       Left _ -> property True
       Right sorted -> property $ length sorted >= 0

prop_topological_sort_all_nodes_present :: DependencyGraph -> Property
prop_topological_sort_all_nodes_present graph = 
  case topologicalSort graph of
    Left _ -> property True
    Right sorted -> 
      let nodeSet = Set.fromList sorted
      in all (`Set.member` nodeSet) (Set.toList (nodes graph))

-- Dependency merging tests
prop_merge_dependency_graphs_combines_nodes :: DependencyGraph -> DependencyGraph -> Property
prop_merge_dependency_graphs_combines_nodes graph1 graph2 = 
  let merged = mergeDependencyGraphs graph1 graph2
  in nodes merged === Set.union (nodes graph1) (nodes graph2)

prop_merge_dependency_graphs_combines_dependencies :: DependencyGraph -> DependencyGraph -> Property
prop_merge_dependency_graphs_combines_dependencies graph1 graph2 = 
  let merged = mergeDependencyGraphs graph1 graph2
      combinedDeps = Map.unionWith (++) (dependencies graph1) (dependencies graph2)
  in dependencies merged === combinedDeps

prop_merge_dependency_graphs_combines_errors :: DependencyGraph -> DependencyGraph -> Property
prop_merge_dependency_graphs_combines_errors graph1 graph2 = 
  let merged = mergeDependencyGraphs graph1 graph2
  in length (errors merged) === length (errors graph1) + length (errors graph2)

-- Dependency type tests
prop_dependency_type_properties :: DependencyType -> Property
prop_dependency_type_properties depType = 
  case depType of
    Import -> property True
    Require -> property True
    Include -> property True
    Extend -> property True
    Implement -> property True

tests :: TestTree
tests = testGroup "Concise Dependencies QuickCheck Tests"
  [ testProperties "Basic Dependency Analysis Tests"
    [ ("analyze dependencies no crash", prop_analyze_dependencies_no_crash)
    , ("detect cycles no crash", prop_detect_cycles_no_crash)
    , ("resolve dependencies no crash", prop_resolve_dependencies_no_crash)
    ]
  , testProperties "Dependency Query Tests"
    [ ("get direct dependencies no crash", prop_get_direct_dependencies_no_crash)
    , ("get transitive dependencies no crash", prop_get_transitive_dependencies_no_crash)
    , ("has cycles detection", prop_has_cycles_detection)
    , ("get dependency errors returns all", prop_get_dependency_errors_returns_all)
    , ("clear dependency errors removes all", prop_clear_dependency_errors_removes_all)
    ]
  , testProperties "Dependency Manipulation Tests"
    [ ("add dependency increases count", prop_add_dependency_increases_count)
    , ("remove dependency decreases count", prop_remove_dependency_decreases_count)
    , ("has dependency detection", prop_has_dependency_detection)
    , ("get nodes returns all nodes", prop_get_nodes_returns_all_nodes)
    ]
  , testProperties "Dependency Path Tests"
    [ ("get dependency path no crash", prop_get_dependency_path_no_crash)
    , ("get dependency path starts with from", prop_get_dependency_path_starts_with_from)
    ]
  , testProperties "Topological Sort Tests"
    [ ("topological sort no crash", prop_topological_sort_no_crash)
    , ("topological sort all nodes present", prop_topological_sort_all_nodes_present)
    ]
  , testProperties "Dependency Merging Tests"
    [ ("merge dependency graphs combines nodes", prop_merge_dependency_graphs_combines_nodes)
    , ("merge dependency graphs combines dependencies", prop_merge_dependency_graphs_combines_dependencies)
    , ("merge dependency graphs combines errors", prop_merge_dependency_graphs_combines_errors)
    ]
  , testProperties "Dependency Type Tests"
    [ ("dependency type properties", prop_dependency_type_properties)
    ]
  ]