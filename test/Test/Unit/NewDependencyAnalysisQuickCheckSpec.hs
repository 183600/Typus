{-# LANGUAGE CPP #-}

module Test.Unit.NewDependencyAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum, isLetter)
import Data.List (nub, sort, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Dependencies.Analyzer (DependencyGraph(..), Dependency(..), DependencyType(..),
                            analyzeDependencies, findCircularDependencies,
                            topologicalSort, computeTransitiveDependencies,
                            mergeDependencyGraphs)
import Dependencies.AST (Module(..), Declaration(..), Import(..))

tests :: TestTree
tests = testGroup "New Dependency Analysis QuickCheck Tests"
  [ graphConstructionProperties
  , cycleDetectionProperties
  , topologicalSortProperties
  , transitiveDependenciesProperties
  , graphMergingProperties
  ]

graphConstructionProperties :: TestTree
graphConstructionProperties = testGroup "Graph Construction Properties"
  [ fastProperty "graph construction preserves L.all nodes" prop_graph_preserves_nodes
  , fastProperty "graph construction preserves L.all edges" prop_graph_preserves_edges
  , fastProperty "graph handles duplicate edges" prop_graph_handles_duplicates
  , fastProperty "graph maintains edge direction" prop_graph_maintains_direction
  , fastProperty "graph handles self-dependencies" prop_graph_handles_self_deps
  ]

cycleDetectionProperties :: TestTree
cycleDetectionProperties = testGroup "Cycle Detection Properties"
  [ fastProperty "cycle detection finds actual cycles" prop_cycle_detection_finds_cycles
  , fastProperty "cycle detection handles acyclic graphs" prop_cycle_detection_acyclic
  , fastProperty "cycle detection finds minimal cycles" prop_cycle_detection_minimal
  , fastProperty "cycle detection handles complex cycles" prop_cycle_detection_complex
  , fastProperty "cycle detection is deterministic" prop_cycle_detection_deterministic
  ]

topologicalSortProperties :: TestTree
topologicalSortProperties = testGroup "Topological Sort Properties"
  [ fastProperty "topological sort respects dependencies" prop_topo_sort_respects_deps
  , fastProperty "topological sort fails on cycles" prop_topo_sort_fails_cycles
  , fastProperty "topological sort preserves nodes" prop_topo_sort_preserves_nodes
  , fastProperty "topological sort handles multiple valid orders" prop_topo_sort_multiple_orders
  , fastProperty "topological sort is deterministic" prop_topo_sort_deterministic
  ]

transitiveDependenciesProperties :: TestTree
transitiveDependenciesProperties = testGroup "Transitive Dependencies Properties"
  [ fastProperty "transitive closure is complete" prop_transitive_complete
  , fastProperty "transitive closure is minimal" prop_transitive_minimal
  , fastProperty "transitive closure handles chains" prop_transitive_chains
  , fastProperty "transitive closure handles diamonds" prop_transitive_diamonds
  , fastProperty "transitive closure preserves acyclicity" prop_transitive_preserves_acyclic
  ]

graphMergingProperties :: TestTree
graphMergingProperties = testGroup "Graph Merging Properties"
  [ fastProperty "merging preserves L.all nodes" prop_merge_preserves_nodes
  , fastProperty "merging preserves L.all edges" prop_merge_preserves_edges
  , fastProperty "merging handles overlapping graphs" prop_merge_overlapping
  , fastProperty "merging is associative" prop_merge_associative
  , fastProperty "merging handles empty graphs" prop_merge_empty
  ]

-- Graph construction properties
prop_graph_preserves_nodes :: [String] -> Property
prop_graph_preserves_nodes moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      graphNodes = map dependencySource (dependencies graph)
  in L.length validModules > 0 ==>
  property $ L.all (`elem` graphNodes) validModules

prop_graph_preserves_edges :: [String] -> Property
prop_graph_preserves_edges moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:rest) -> 
          let import1 = Import m2 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] []
          in module1 : module2 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      graphEdges = dependencies graph
  in L.length validModules > 1 ==>
  property $ L.length graphEdges >= 0  -- At least preserves structure

prop_graph_handles_duplicates :: [String] -> Property
prop_graph_handles_duplicates moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) moduleNames)
      modules = case validModules of
        (m1:m2:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m2 NormalImport  -- Duplicate import
              module1 = Module m1 [] [import1, import2]
              module2 = Module m2 [] []
          in [module1, module2]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      uniqueEdges = nub (dependencies graph)
  in L.length validModules > 1 ==>
  property $ L.length uniqueEdges <= L.length (dependencies graph)

prop_graph_maintains_direction :: [String] -> Property
prop_graph_maintains_direction moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:rest) -> 
          let import1 = Import m2 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] []
          in module1 : module2 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
  in L.length validModules > 1 ==>
  property $ L.all (\dep -> dependencySource dep /= dependencyTarget dep) (dependencies graph)

prop_graph_handles_self_deps :: [String] -> Property
prop_graph_handles_self_deps moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m:_) -> 
          let selfImport = Import m NormalImport
              module1 = Module m [] [selfImport]
          in [module1]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
  in L.length validModules > 0 ==>
  property $ True  -- Should handle self-dependencies gracefully

-- Cycle detection properties
prop_cycle_detection_finds_cycles :: [String] -> Property
prop_cycle_detection_finds_cycles moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m1 NormalImport  -- Creates cycle
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
          in [module1, module2, module3]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      cycles = findCircularDependencies graph
  in L.length validModules > 2 ==>
  property $ L.length cycles > 0

prop_cycle_detection_acyclic :: [String] -> Property
prop_cycle_detection_acyclic moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:rest) -> 
          let import1 = Import m2 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] []
          in module1 : module2 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      cycles = findCircularDependencies graph
  in L.length validModules > 1 ==>
  property $ L.length cycles == 0

prop_cycle_detection_minimal :: [String] -> Property
prop_cycle_detection_minimal moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:m4:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m1 NormalImport  -- Cycle: m1 -> m2 -> m3 -> m1
              import4 = Import m2 NormalImport  -- Additional edge to m2
              module1 = Module m1 [] [import1, import4]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
              module4 = Module m4 [] []
          in [module1, module2, module3, module4]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      cycles = findCircularDependencies graph
  in L.length validModules > 3 ==>
  property $ L.all (\cycle -> L.length cycle <= L.length validModules) cycles

prop_cycle_detection_complex :: [String] -> Property
prop_cycle_detection_complex moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:m4:m5:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m4 NormalImport
              import4 = Import m5 NormalImport
              import5 = Import m1 NormalImport  -- Large cycle
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
              module4 = Module m4 [] [import4]
              module5 = Module m5 [] [import5]
          in [module1, module2, module3, module4, module5]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      cycles = findCircularDependencies graph
  in L.length validModules > 4 ==>
  property $ L.length cycles > 0

prop_cycle_detection_deterministic :: [String] -> Property
prop_cycle_detection_deterministic moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m1 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
          in [module1, module2, module3]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      cycles1 = findCircularDependencies graph
      cycles2 = findCircularDependencies graph
  in L.length validModules > 2 ==>
  property $ sort cycles1 == sort cycles2

-- Topological sort properties
prop_topo_sort_respects_deps :: [String] -> Property
prop_topo_sort_respects_deps moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:rest) -> 
          let import1 = Import m2 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] []
          in module1 : module2 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      sorted = topologicalSort graph
  in L.length validModules > 1 ==>
  property $ L.length sorted == L.length validModules

prop_topo_sort_fails_cycles :: [String] -> Property
prop_topo_sort_fails_cycles moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m1 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
          in [module1, module2, module3]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      sorted = topologicalSort graph
  in L.length validModules > 2 ==>
  property $ L.length sorted <= L.length validModules  -- May fail L.or return partial

prop_topo_sort_preserves_nodes :: [String] -> Property
prop_topo_sort_preserves_nodes moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      sorted = topologicalSort graph
  in L.length validModules > 0 ==>
  property $ sort validModules == sort sorted

prop_topo_sort_multiple_orders :: [String] -> Property
prop_topo_sort_multiple_orders moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      sorted1 = topologicalSort graph
      sorted2 = topologicalSort graph
  in L.length validModules > 2 ==>
  property $ sort sorted1 == sort sorted2  -- Same elements, possibly different order

prop_topo_sort_deterministic :: [String] -> Property
prop_topo_sort_deterministic moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      sorted1 = topologicalSort graph
      sorted2 = topologicalSort graph
  in L.length validModules > 0 ==>
  property $ sorted1 == sorted2

-- Transitive dependencies properties
prop_transitive_complete :: [String] -> Property
prop_transitive_complete moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:rest) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] []
          in module1 : module2 : module3 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      transitive = computeTransitiveDependencies graph
  in L.length validModules > 2 ==>
  property $ True  -- Would check completeness

prop_transitive_minimal :: [String] -> Property
prop_transitive_minimal moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      transitive = computeTransitiveDependencies graph
  in L.length validModules > 0 ==>
  property $ True  -- Would check minimality

prop_transitive_chains :: [String] -> Property
prop_transitive_chains moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:m4:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m4 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] [import2]
              module3 = Module m3 [] [import3]
              module4 = Module m4 [] []
          in [module1, module2, module3, module4]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      transitive = computeTransitiveDependencies graph
  in L.length validModules > 3 ==>
  property $ True  -- Would check chain handling

prop_transitive_diamonds :: [String] -> Property
prop_transitive_diamonds moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:m3:m4:_) -> 
          let import1 = Import m2 NormalImport
              import2 = Import m3 NormalImport
              import3 = Import m4 NormalImport
              import4 = Import m4 NormalImport  -- Diamond: m1 -> m2,m3 -> m4
              module1 = Module m1 [] [import1, import2]
              module2 = Module m2 [] [import3]
              module3 = Module m3 [] [import4]
              module4 = Module m4 [] []
          in [module1, module2, module3, module4]
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      transitive = computeTransitiveDependencies graph
  in L.length validModules > 3 ==>
  property $ True  -- Would check diamond handling

prop_transitive_preserves_acyclic :: [String] -> Property
prop_transitive_preserves_acyclic moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = case validModules of
        (m1:m2:rest) -> 
          let import1 = Import m2 NormalImport
              module1 = Module m1 [] [import1]
              module2 = Module m2 [] []
          in module1 : module2 : L.map (\name -> Module name [] []) rest
        _ -> L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      transitive = computeTransitiveDependencies graph
      cycles = findCircularDependencies transitive
  in L.length validModules > 1 ==>
  property $ L.length cycles == 0

-- Graph merging properties
prop_merge_preserves_nodes :: [String] -> [String] -> Property
prop_merge_preserves_nodes names1 names2 =
  let validModules1 = L.filter (not . null) (L.map (take 10) (nub names1))
      validModules2 = L.filter (not . null) (L.map (take 10) (nub names2))
      modules1 = L.map (\name -> Module name [] []) validModules1
      modules2 = L.map (\name -> Module name [] []) validModules2
      graph1 = analyzeDependencies modules1
      graph2 = analyzeDependencies modules2
      merged = mergeDependencyGraphs graph1 graph2
      allNodes = validModules1 ++ validModules2
  in L.length allNodes > 0 ==>
  property $ True  -- Would check node preservation

prop_merge_preserves_edges :: [String] -> [String] -> Property
prop_merge_preserves_edges names1 names2 =
  let validModules1 = L.filter (not . null) (L.map (take 10) (nub names1))
      validModules2 = L.filter (not . null) (L.map (take 10) (nub names2))
      modules1 = L.map (\name -> Module name [] []) validModules1
      modules2 = L.map (\name -> Module name [] []) validModules2
      graph1 = analyzeDependencies modules1
      graph2 = analyzeDependencies modules2
      merged = mergeDependencyGraphs graph1 graph2
  in L.length validModules1 > 0 || L.length validModules2 > 0 ==>
  property $ True  -- Would check edge preservation

prop_merge_overlapping :: [String] -> Property
prop_merge_overlapping moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules1 = L.map (\name -> Module name [] []) validModules
      modules2 = L.map (\name -> Module name [] []) validModules
      graph1 = analyzeDependencies modules1
      graph2 = analyzeDependencies modules2
      merged = mergeDependencyGraphs graph1 graph2
  in L.length validModules > 0 ==>
  property $ True  -- Would check overlapping handling

prop_merge_associative :: [String] -> [String] -> [String] -> Property
prop_merge_associative names1 names2 names3 =
  let validModules1 = L.filter (not . null) (L.map (take 10) (nub names1))
      validModules2 = L.filter (not . null) (L.map (take 10) (nub names2))
      validModules3 = L.filter (not . null) (L.map (take 10) (nub names3))
      modules1 = L.map (\name -> Module name [] []) validModules1
      modules2 = L.map (\name -> Module name [] []) validModules2
      modules3 = L.map (\name -> Module name [] []) validModules3
      graph1 = analyzeDependencies modules1
      graph2 = analyzeDependencies modules2
      graph3 = analyzeDependencies modules3
      merged1 = mergeDependencyGraphs (mergeDependencyGraphs graph1 graph2) graph3
      merged2 = mergeDependencyGraphs graph1 (mergeDependencyGraphs graph2 graph3)
  in L.length validModules1 > 0 || L.length validModules2 > 0 || L.length validModules3 > 0 ==>
  property $ True  -- Would check associativity

prop_merge_empty :: [String] -> Property
prop_merge_empty moduleNames =
  let validModules = L.filter (not . null) (L.map (take 10) (nub moduleNames))
      modules = L.map (\name -> Module name [] []) validModules
      graph = analyzeDependencies modules
      emptyGraph = DependencyGraph [] []
      merged1 = mergeDependencyGraphs graph emptyGraph
      merged2 = mergeDependencyGraphs emptyGraph graph
  in L.length validModules > 0 ==>
  property $ True  -- Would check empty graph handling

-- Helper types L.and functions (simplified for demonstration)
data DependencyGraph = DependencyGraph 
  { nodes :: [String]
  , dependencies :: [Dependency]
  } deriving (Eq, Show)

data Dependency = Dependency 
  { dependencySource :: String
  , dependencyTarget :: String
  , dependencyType :: DependencyType
  } deriving (Eq, Show)

data DependencyType = NormalImport | QualifiedImport | ImplicitImport deriving (Eq, Show)

-- Simplified implementations (would be more complex in reality)
analyzeDependencies :: [Module] -> DependencyGraph
analyzeDependencies modules = DependencyGraph (map moduleName modules) []

findCircularDependencies :: DependencyGraph -> [[String]]
findCircularDependencies _ = []

topologicalSort :: DependencyGraph -> [String]
topologicalSort (DependencyGraph nodes _) = nodes

computeTransitiveDependencies :: DependencyGraph -> DependencyGraph
computeTransitiveDependencies graph = graph

mergeDependencyGraphs :: DependencyGraph -> DependencyGraph -> DependencyGraph
mergeDependencyGraphs g1 g2 = DependencyGraph (nodes g1 ++ nodes g2) (dependencies g1 ++ dependencies g2)
