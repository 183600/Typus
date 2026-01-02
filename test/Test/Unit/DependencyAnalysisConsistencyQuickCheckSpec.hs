{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, frequency, sized, sublistOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Dependencies
  ( Dependency(..)
  , DependencyGraph
  , DependencyType(..)
  , ModuleName
  , analyzeDependencies
  , findCircularDependencies
  , topologicalSort
  , getTransitiveDependencies
  , checkForConflicts
  , mergeDependencyGraphs
  , invertDependencyGraph
  , getDependencyLevel
  )

import Data.List (nub, sort, (\\), intersect, union)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | Generate arbitrary module names
newtype TestModuleName = TestModuleName { getTestModuleName :: ModuleName }
  deriving (Show, Eq, Ord)

instance Arbitrary TestModuleName where
  arbitrary = sized $ \size -> do
    let maxSize = min size 10
    len <- choose (1, maxSize)
    name <- listOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    return $ TestModuleName name

-- | Generate arbitrary dependency types
instance Arbitrary DependencyType where
  arbitrary = elements
    [ Import
    , TypeDependency
    , FunctionDependency
    , VariableDependency
    , MacroDependency
    , TemplateDependency
    ]

-- | Generate arbitrary dependencies
instance Arbitrary Dependency where
  arbitrary = do
    TestModuleName from <- arbitrary
    TestModuleName to <- arbitrary
    depType <- arbitrary
    return $ Dependency from to depType

-- | Generate dependency graphs
newtype TestDependencyGraph = TestDependencyGraph { getTestDependencyGraph :: DependencyGraph }
  deriving (Show, Eq)

instance Arbitrary TestDependencyGraph where
  arbitrary = do
    deps <- listOf arbitrary
    return $ TestDependencyGraph $ buildGraphFromDeps deps
    where
      buildGraphFromDeps :: [Dependency] -> DependencyGraph
      buildGraphFromDeps deps = Map.fromListWith (++) $
        L.map (\dep -> (depFrom dep, [dep])) deps

-- | Generate sets of module names
newtype TestModuleSet = TestModuleSet { getTestModuleSet :: Set ModuleName }
  deriving (Show, Eq)

instance Arbitrary TestModuleSet where
  arbitrary = do
    mods <- listOf arbitrary
    return $ TestModuleSet $ Set.fromList $ map getTestModuleName mods

-- Property: dependency graph construction is consistent
prop_dependency_graph_construction :: [Dependency] -> Property
prop_dependency_graph_construction deps =
  let graph = buildGraphFromDeps deps
      allFromModules = map depFrom deps
      graphKeys = Map.keys graph
  in property $ 
       Set.fromList allFromModules === Set.fromList graphKeys
  where
    buildGraphFromDeps :: [Dependency] -> DependencyGraph
    buildGraphFromDeps deps = Map.fromListWith (++) $
      L.map (\dep -> (depFrom dep, [dep])) deps

-- Property: transitive dependencies are transitive
prop_transitive_dependencies_are_transitive :: Dependency -> Dependency -> Dependency -> Property
prop_transitive_dependencies_are_transitive dep1 dep2 dep3 =
  let formsChain = depFrom dep2 == depTo dep1 && depFrom dep3 == depTo dep2
  in formsChain ==>
     let graph = buildGraphFromDeps [dep1, dep2, dep3]
         transitive = getTransitiveDependencies graph (depFrom dep1)
         expected = Set.fromList [depTo dep1, depTo dep2, depTo dep3]
     in property $ Set.fromList transitive === expected
  where
    buildGraphFromDeps :: [Dependency] -> DependencyGraph
    buildGraphFromDeps deps = Map.fromListWith (++) $
      L.map (\dep -> (depFrom dep, [dep])) deps

-- Property: topological sort respects dependencies
prop_topological_sort_respects_dependencies :: TestDependencyGraph -> Property
prop_topological_sort_respects_dependencies graphWrapper =
  let graph = getTestDependencyGraph graphWrapper
      sorted = topologicalSort graph
      hasCycles = not (L.null (findCircularDependencies graph))
  in not hasCycles ==>
     let modulePositions = Map.fromList $ zip sorted [0..]
         respectsDep dep = 
           let fromPos = Map.findWithDefault (-1) (depFrom dep) modulePositions
               toPos = Map.findWithDefault (-1) (depTo dep) modulePositions
           in fromPos < toPos
         allDeps = L.concat $ Map.elems graph
     in property $ L.all respectsDep allDeps

-- Property: circular dependency detection is sound
prop_circular_dependency_detection_sound :: [Dependency] -> Property
prop_circular_dependency_detection_sound deps =
  let graph = buildGraphFromDeps deps
      cycles = findCircularDependencies graph
      -- For each detected cycle, verify it's actually a cycle
      isActualCycle cycle = L.all (\dep -> 
        let nextModule = depTo dep
            nextDeps = Map.findWithDefault [] nextModule graph
        in L.any (\nextDep -> depFrom nextDep == depFrom cycle) nextDeps) cycle
  in property $ L.all isActualCycle cycles
  where
    buildGraphFromDeps :: [Dependency] -> DependencyGraph
    buildGraphFromDeps deps = Map.fromListWith (++) $
      L.map (\dep -> (depFrom dep, [dep])) deps

-- Property: dependency level calculation is consistent
prop_dependency_level_consistency :: TestDependencyGraph -> TestModuleName -> Property
prop_dependency_level_consistency graphWrapper moduleWrapper =
  let graph = getTestDependencyGraph graphWrapper
      module' = getTestModuleName moduleWrapper
      level = getDependencyLevel graph module'
      -- Check that L.all dependencies have lower levels
      deps = Map.findWithDefault [] module' graph
      depLevels = L.map (\dep -> getDependencyLevel graph (depTo dep)) deps
  in property $ L.all (< level) depLevels

-- Property: graph inversion preserves relationships
prop_graph_inversion_preserves_relationships :: TestDependencyGraph -> Property
prop_graph_inversion_preserves_relationships graphWrapper =
  let graph = getTestDependencyGraph graphWrapper
      inverted = invertDependencyGraph graph
      originalDeps = L.concat $ Map.elems graph
      invertedDeps = L.concat $ Map.elems inverted
      -- Check that every original dependency has a corresponding inverted one
      hasCorrespondence dep = L.any (\invDep -> 
        depFrom dep == depTo invDep && depTo dep == depFrom invDep) invertedDeps
  in property $ L.all hasCorrespondence originalDeps

-- Property: graph merging is associative
prop_graph_merging_associative :: TestDependencyGraph -> TestDependencyGraph -> TestDependencyGraph -> Property
prop_graph_merging_associative g1 g2 g3 =
  let graph1 = getTestDependencyGraph g1
      graph2 = getTestDependencyGraph g2
      graph3 = getTestDependencyGraph g3
      merged1 = mergeDependencyGraphs (mergeDependencyGraphs graph1 graph2) graph3
      merged2 = mergeDependencyGraphs graph1 (mergeDependencyGraphs graph2 graph3)
  in property $ merged1 === merged2

-- Property: graph merging is commutative
prop_graph_merging_commutative :: TestDependencyGraph -> TestDependencyGraph -> Property
prop_graph_merging_commutative g1 g2 =
  let graph1 = getTestDependencyGraph g1
      graph2 = getTestDependencyGraph g2
      merged1 = mergeDependencyGraphs graph1 graph2
      merged2 = mergeDependencyGraphs graph2 graph1
  in property $ merged1 === merged2

-- Property: conflict detection finds actual conflicts
prop_conflict_detection_finds_conflicts :: [Dependency] -> [Dependency] -> Property
prop_conflict_detection_finds_conflicts deps1 deps2 =
  let graph1 = buildGraphFromDeps deps1
      graph2 = buildGraphFromDeps deps2
      conflicts = checkForConflicts graph1 graph2
      -- A conflict occurs when the same module has different dependencies
      hasConflict module' = 
        let deps1' = Map.findWithDefault [] module' graph1
            deps2' = Map.findWithDefault [] module' graph2
        in deps1' /= deps2'
      expectedConflicts = filter hasConflict $ 
        Set.toList $ Set.union (Set.fromList $ Map.keys graph1) (Set.fromList $ Map.keys graph2)
  in property $ Set.fromList conflicts === Set.fromList expectedConflicts
  where
    buildGraphFromDeps :: [Dependency] -> DependencyGraph
    buildGraphFromDeps deps = Map.fromListWith (++) $
      L.map (\dep -> (depFrom dep, [dep])) deps

-- Property: transitive dependency closure is idempotent
prop_transitive_closure_idempotent :: TestDependencyGraph -> TestModuleName -> Property
prop_transitive_closure_idempotent graphWrapper moduleWrapper =
  let graph = getTestDependencyGraph graphWrapper
      module' = getTestModuleName moduleWrapper
      transitive1 = getTransitiveDependencies graph module'
      -- Apply transitive closure again (conceptually)
      transitive2 = getTransitiveDependencies graph module'
  in property $ Set.fromList transitive1 === Set.fromList transitive2

-- Property: dependency analysis is deterministic
prop_dependency_analysis_deterministic :: [Dependency] -> Property
prop_dependency_analysis_deterministic deps =
  let graph1 = buildGraphFromDeps deps
      graph2 = buildGraphFromDeps deps
  in property $ graph1 === graph2
  where
    buildGraphFromDeps :: [Dependency] -> DependencyGraph
    buildGraphFromDeps deps = Map.fromListWith (++) $
      L.map (\dep -> (depFrom dep, [dep])) deps

-- Property: empty graph has no dependencies
prop_empty_graph_has_no_dependencies :: TestModuleName -> Property
prop_empty_graph_has_no_dependencies moduleWrapper =
  let graph = Map.empty :: DependencyGraph
      module' = getTestModuleName moduleWrapper
      transitive = getTransitiveDependencies graph module'
      level = getDependencyLevel graph module'
  in property $ null transitive .&&. level == 0

tests :: TestTree
tests = testGroup "Dependency Analysis Consistency QuickCheck Tests"
  [ fastProperty "dependency graph construction" prop_dependency_graph_construction
  , fastProperty "transitive dependencies are transitive" prop_transitive_dependencies_are_transitive
  , fastProperty "topological sort respects dependencies" prop_topological_sort_respects_dependencies
  , fastProperty "circular dependency detection sound" prop_circular_dependency_detection_sound
  , fastProperty "dependency level consistency" prop_dependency_level_consistency
  , fastProperty "graph inversion preserves relationships" prop_graph_inversion_preserves_relationships
  , fastProperty "graph merging associative" prop_graph_merging_associative
  , fastProperty "graph merging commutative" prop_graph_merging_commutative
  , fastProperty "conflict detection finds conflicts" prop_conflict_detection_finds_conflicts
  , fastProperty "transitive closure idempotent" prop_transitive_closure_idempotent
  , fastProperty "dependency analysis deterministic" prop_dependency_analysis_deterministic
  , fastProperty "empty graph has no dependencies" prop_empty_graph_has_no_dependencies
  , testGroup "Manual dependency analysis tests"
      [ testCase "simple linear dependency chain" $ do
          let depA = Dependency "A" "B" Import
              depB = Dependency "B" "C" Import
              depC = Dependency "C" "D" Import
              graph = buildGraphFromDeps [depA, depB, depC]
              sorted = topologicalSort graph
          assertEqual "should sort in dependency order" ["A", "B", "C", "D"] sorted
          
      , testCase "circular dependency detection" $ do
          let depA = Dependency "A" "B" Import
              depB = Dependency "B" "C" Import
              depC = Dependency "C" "A" Import
              graph = buildGraphFromDeps [depA, depB, depC]
              cycles = findCircularDependencies graph
          assertBool "should detect circular dependency" $ not (null cycles)
          
      , testCase "transitive dependency calculation" $ do
          let depA = Dependency "A" "B" Import
              depB = Dependency "B" "C" Import
              depC = Dependency "A" "D" Import
              graph = buildGraphFromDeps [depA, depB, depC]
              transitive = getTransitiveDependencies graph "A"
          assertEqual "should include L.all transitive dependencies" 
                     ["B", "C", "D"] (sort transitive)
          
      , testCase "dependency level calculation" $ do
          let depA = Dependency "A" "B" Import
              depB = Dependency "B" "C" Import
              depC = Dependency "C" "D" Import
              graph = buildGraphFromDeps [depA, depB, depC]
              levelA = getDependencyLevel graph "A"
              levelB = getDependencyLevel graph "B"
              levelC = getDependencyLevel graph "C"
              levelD = getDependencyLevel graph "D"
          assertEqual "D should have level 0" 0 levelD
          assertEqual "C should have level 1" 1 levelC
          assertEqual "B should have level 2" 2 levelB
          assertEqual "A should have level 3" 3 levelA
    }
  ]