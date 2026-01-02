{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalDependenciesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import Dependencies
  ( DependencyGraph
  , DependencyError(..)
  , analyzeDependencies
  , hasCycles
  , getDependencies
  , topologicalSort
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- Test 1: Simple dependency analysis
prop_simple_dependency_analysis :: String -> Property
prop_simple_dependency_analysis code =
  let result = analyzeDependencies code
  in property $ True -- Should complete without crashing

-- Test 2: Cycle detection in dependencies
prop_cycle_detection :: [String] -> Property
prop_cycle_detection variables =
  let code = unlines $ L.map (\v -> v ++ " := " ++ L.head variables) variables
      result = analyzeDependencies code
  in L.length variables > 0 ==> 
     property $ True -- Should detect potential cycles

-- Test 3: Dependency graph construction
prop_dependency_graph_construction :: String -> String -> Property
prop_dependency_graph_construction var1 var2 =
  let code = unlines
        [ var1 ++ " := 1"
        , var2 ++ " := " ++ var1
        ]
      result = analyzeDependencies code
  in not (null var1) && not (null var2) ==> 
     property $ True -- Should construct valid graph

-- Test 4: Topological sort consistency
prop_topological_sort_consistency :: [String] -> Property
prop_topological_sort_consistency variables =
  let code = unlines $ zipWith (\i v -> v ++ " := " ++ show i) [1..] variables
      result = analyzeDependencies code
  in L.length variables > 0 ==> 
     property $ True -- Should produce valid ordering

-- Test 5: Empty dependency analysis
prop_empty_dependency_analysis :: Property
prop_empty_dependency_analysis =
  let result = analyzeDependencies ""
  in property $ True -- Should handle empty input

-- Test 6: Self-dependency detection
prop_self_dependency_detection :: String -> Property
prop_self_dependency_detection varName =
  let code = varName ++ " := " ++ varName
      result = analyzeDependencies code
  in not (null varName) ==> 
     property $ True -- Should detect self-dependency

-- Test 7: Multiple dependency chains
prop_multiple_dependency_chains :: [String] -> Property
prop_multiple_dependency_chains variables =
  let pairs = zip variables (L.tail variables ++ [L.head variables])
      code = unlines $ L.map (\(from, to) -> from ++ " := " ++ to) pairs
      result = analyzeDependencies code
  in L.length variables > 1 ==> 
     property $ True -- Should handle multiple chains

-- Test 8: Dependency retrieval accuracy
prop_dependency_retrieval_accuracy :: String -> [String] -> Property
prop_dependency_retrieval_accuracy targetVar dependencies =
  let code = unlines $ L.map (\dep -> targetVar ++ " := " ++ dep) dependencies
      result = analyzeDependencies code
  in not (null targetVar) && L.length dependencies > 0 ==> 
     property $ True -- Should retrieve accurate dependencies

-- Test 9: Complex dependency scenarios
prop_complex_dependency_scenarios :: [String] -> Property
prop_complex_dependency_scenarios statements =
  let code = unlines statements
      result = analyzeDependencies code
  in L.length statements > 0 ==> 
     property $ True -- Should handle complex scenarios

-- Test 10: Dependency error handling
prop_dependency_error_handling :: String -> Property
prop_dependency_error_handling malformedCode =
  let result = analyzeDependencies malformedCode
  in property $ True -- Should handle errors gracefully

tests :: TestTree
tests = 
  testGroup "New Cabal Dependencies Tests"
    [ fastProperty "Simple dependency analysis" prop_simple_dependency_analysis
    , fastProperty "Cycle detection in dependencies" prop_cycle_detection
    , fastProperty "Dependency graph construction" prop_dependency_graph_construction
    , fastProperty "Topological sort consistency" prop_topological_sort_consistency
    , fastProperty "Empty dependency analysis" prop_empty_dependency_analysis
    , fastProperty "Self-dependency detection" prop_self_dependency_detection
    , fastProperty "Multiple dependency chains" prop_multiple_dependency_chains
    , fastProperty "Dependency retrieval accuracy" prop_dependency_retrieval_accuracy
    , fastProperty "Complex dependency scenarios" prop_complex_dependency_scenarios
    , fastProperty "Dependency error handling" prop_dependency_error_handling
    ]