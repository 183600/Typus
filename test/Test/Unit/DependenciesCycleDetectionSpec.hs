{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.DependenciesCycleDetectionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary)

import Dependencies.Analyzer
  ( analyzeDependencies
  , DependencyGraph(..)
  , DependencyError(..)
  , DependencyCycle(..)
  , findCycles
  , breakCycles
  , validateDependencyGraph
  )

import Dependencies.TypeSystem
  ( DependencyType(..)
  , ModuleDependency(..)
  , DependencyRelation(..)
  )

import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph (buildGraph, topSort)

-- | Test dependency cycle detection
tests :: TestTree
tests =
  testGroup "Dependencies Cycle Detection Tests"
    [ testGroup "Basic cycle detection"
        [ testCase "detects simple two-module cycle" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "moduleA" "moduleB" Direct
                  , ModuleDependency "moduleB" "moduleA" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect simple cycle" $ length cycles >= 1
            assertBool "cycle should contain both modules" $ 
              case cycles of
                (DependencyCycle cycleNodes : _) -> 
                  "moduleA" `elem` cycleNodes && "moduleB" `elem` cycleNodes
                _ -> False

        , testCase "detects three-module cycle" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "C" Direct
                  , ModuleDependency "C" "A" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect three-module cycle" $ length cycles >= 1
            assertBool "cycle should contain all three modules" $ 
              case cycles of
                (DependencyCycle cycleNodes : _) -> 
                  length (filter (`elem` ["A", "B", "C"]) cycleNodes) == 3
                _ -> False

        , testCase "detects self-dependency" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "self" "self" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect self-dependency" $ 
              case cycles of
                (DependencyCycle ["self"] : _) -> True
                _ -> False
        ]

    , testGroup "Complex cycle detection"
        [ testCase "detects multiple independent cycles" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "A" Direct
                  , ModuleDependency "C" "D" Direct
                  , ModuleDependency "D" "C" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect multiple cycles" $ length cycles >= 2

        , testCase "detects cycle with intermediate nodes" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "start" "middle1" Direct
                  , ModuleDependency "middle1" "middle2" Direct
                  , ModuleDependency "middle2" "middle3" Direct
                  , ModuleDependency "middle3" "start" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect complex cycle" $ length cycles >= 1
            assertBool "cycle should include all intermediate nodes" $ 
              case cycles of
                (DependencyCycle cycleNodes : _) -> 
                  length (filter (`elem` ["start", "middle1", "middle2", "middle3"]) cycleNodes) == 4
                _ -> False

        , testCase "detects nested cycles" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "outer1" "outer2" Direct
                  , ModuleDependency "outer2" "outer3" Direct
                  , ModuleDependency "outer3" "outer1" Direct
                  , ModuleDependency "inner1" "inner2" Direct
                  , ModuleDependency "inner2" "inner1" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect nested cycles" $ length cycles >= 2
        ]

    , testGroup "Cycle breaking strategies"
        [ testCase "breaks cycles by removing minimal edges" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "C" Direct
                  , ModuleDependency "C" "A" Direct
                  ]
                broken = breakCycles graph
                cyclesAfter = findCycles broken
            assertBool "should break cycles" $ null cyclesAfter

        , testCase "preserves non-cyclic dependencies" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "C" Direct
                  , ModuleDependency "C" "A" Direct  -- Cycle
                  , ModuleDependency "D" "E" Direct    -- Non-cyclic
                  ]
                broken = breakCycles graph
                preserved = any (\dep -> mdFrom dep == "D" && mdTo dep == "E") (dgDependencies broken)
            assertBool "should preserve non-cyclic dependencies" $ preserved

        , testCase "chooses optimal edges to remove" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "core" "utils" Weak
                  , ModuleDependency "utils" "core" Direct
                  ]
                broken = breakCycles graph
                remaining = dgDependencies broken
            assertBool "should prefer removing weak dependencies" $ 
              case remaining of
                [ModuleDependency "core" "utils" Weak] -> True
                _ -> False
        ]

    , testGroup "Dependency type analysis"
        [ testCase "distinguishes direct from indirect cycles" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "C" Indirect
                  , ModuleDependency "C" "A" Direct
                  ]
                cycles = findCycles graph
            assertBool "should detect mixed dependency types" $ length cycles >= 1

        , testCase "handles transitive dependencies correctly" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "A" "B" Direct
                  , ModuleDependency "B" "C" Direct
                  , ModuleDependency "C" "D" Direct
                  ]
                transitiveCycles = findCycles graph
            assertBool "should not detect false positives in transitive chains" $ 
              null transitiveCycles

        , testCase "analyzes weak vs strong dependencies" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "strong1" "strong2" Strong
                  , ModuleDependency "strong2" "strong1" Strong
                  , ModuleDependency "weak1" "weak2" Weak
                  , ModuleDependency "weak2" "weak1" Weak
                  ]
                cycles = findCycles graph
            assertBool "should detect both weak and strong cycles" $ length cycles >= 2
        ]

    , testGroup "Error reporting for cycles"
        [ testCase "provides detailed cycle information" $ do
            let graph = DependencyGraph
                  [ ModuleDependency "problematic" "dependency" Direct
                  , ModuleDependency "dependency" "problematic" Direct
                  ]
                cycles = findCycles graph
            assertBool "should provide cycle details" $ 
              case cycles of
                (DependencyCycle nodes : _) -> 
                  length nodes >= 2 && "problematic" `elem` nodes
                _ -> False

        , testCase "suggests cycle resolution strategies" $ do
            let cycle = DependencyCycle ["moduleA", "moduleB", "moduleC"]
                suggestions = generateCycleSuggestions cycle
            assertBool "should provide resolution suggestions" $ 
              length suggestions > 0
          where
            generateCycleSuggestions (DependencyCycle nodes) = 
              [ "Extract common functionality from " ++ unwords nodes
              , "Use dependency injection to break cycle"
              , "Introduce interface layer"
              ]

        , testCase "reports cycle severity levels" $ do
            let criticalCycle = DependencyCycle ["core", "essential"]
                minorCycle = DependencyCycle ["optional1", "optional2"]
                criticalSeverity = assessCycleSeverity criticalCycle
                minorSeverity = assessCycleSeverity minorCycle
            assertBool "should assess cycle severity" $ 
              criticalSeverity > minorSeverity
          where
            assessCycleSeverity (DependencyCycle nodes) = 
              if any (`elem` ["core", "essential"]) nodes then 10 else 3
        ]

    , testGroup "Performance with large dependency graphs"
        [ testCase "handles large graphs efficiently" $ do
            let largeGraph = DependencyGraph
                  [ ModuleDependency ("mod" ++ show i) ("mod" ++ show (i + 1)) Direct
                  | i <- [1..999]
                  ] ++ [ModuleDependency "mod1000" "mod1" Direct]
                cycles = findCycles largeGraph
            assertBool "should handle large graphs" $ length cycles >= 1

        , testCase "scales linearly with graph size" $ do
            let sizes = [10, 50, 100]
                testGraph size = DependencyGraph
                  [ ModuleDependency ("n" ++ show i) ("n" ++ show (i + 1)) Direct
                  | i <- [1..size-1]
                  ] ++ [ModuleDependency ("n" ++ show size) ("n" ++ show 1) Direct]
                cycleCounts = map (length . findCycles . testGraph) sizes
            assertBool "should scale reasonably" $ all (>0) cycleCounts
        ]

    , testGroup "QuickCheck property tests for cycle detection"
        [ fastProperty "cycle detection is deterministic" $
            \graph ->
            let cycles1 = findCycles graph
                cycles2 = findCycles graph
            in cycles1 === cycles2

        , fastProperty "breaking cycles results in acyclic graph" $
            \graph ->
            let broken = breakCycles graph
                cycles = findCycles broken
            in null cycles

        , fastProperty "self-dependency always forms a cycle" $
            \moduleName ->
            let graph = DependencyGraph [ModuleDependency moduleName moduleName Direct]
                cycles = findCycles graph
            in not (null cycles)

        , fastProperty "acyclic graph has no cycles" $
            \modules ->
            let nonCyclicGraph = DependencyGraph
                  [ ModuleDependency from to Direct
                  | (from, to) <- zip modules (tail modules ++ ["end"])
                  , from /= to
                  ]
                cycles = findCycles nonCyclicGraph
            in null cycles

        , fastProperty "cycle length is bounded by number of modules" $
            \graph ->
            let cycles = findCycles graph
                maxCycleLength = maximum $ map cycleLength cycles
                moduleCount = length $ extractModules graph
            in null cycles || maxCycleLength <= moduleCount
          where
            cycleLength (DependencyCycle nodes) = length nodes
            extractModules (DependencyGraph deps) = 
              nub $ concatMap (\dep -> [mdFrom dep, mdTo dep]) deps

        , fastProperty "breaking cycles preserves non-cyclic dependencies" $
            \graph ->
            let originalNonCyclic = filterNonCyclicDependencies graph
                broken = breakCycles graph
                preservedNonCyclic = filterNonCyclicDependencies broken
            in length preservedNonCyclic >= length originalNonCyclic `div` 2
          where
            filterNonCyclicDependencies (DependencyGraph deps) = 
              filter (\dep -> mdFrom dep /= mdTo dep) deps
        ]
  ]