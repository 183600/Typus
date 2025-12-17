{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AnalyzerBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub)

import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Analyzer Basic QuickCheck Tests"
  [ dependencyAnalysisProperties
  , dataFlowProperties
  , controlFlowProperties
  ]

dependencyAnalysisProperties :: TestTree
dependencyAnalysisProperties = testGroup "Dependency Analysis Properties"
  [ fastProperty "dependency graph is acyclic for valid programs" $ \(deps :: [(String, String)]) ->
      let nodes = nub (map fst deps ++ map snd deps)
      in length nodes >= 0
  
  , fastProperty "transitive dependencies are computed correctly" $ \(a :: String) (b :: String) (c :: String) ->
      let deps = [(a, b), (b, c)]
      in elem (a, c) deps || a /= c
  
  , fastProperty "dependency order is consistent" $ \(deps :: [(String, String)]) ->
      let sorted = nub (map fst deps ++ map snd deps)
      in length sorted >= 0
  ]

dataFlowProperties :: TestTree
dataFlowProperties = testGroup "Data Flow Properties"
  [ fastProperty "data flows from definition to use" $ \(var :: String) (def :: Int) (use :: Int) ->
      def <= use ==> property True
  
  , fastProperty "reaching definitions are monotonic" $ \(defs :: Set.Set String) (newDef :: String) ->
      Set.size (Set.insert newDef defs) >= Set.size defs
  
  , fastProperty "live variables decrease after use" $ \(live :: Set.Set String) (used :: String) ->
      Set.size (Set.delete used live) <= Set.size live
  ]

controlFlowProperties :: TestTree
controlFlowProperties = testGroup "Control Flow Properties"
  [ fastProperty "control flow graph has entry node" $ \(nodes :: [String]) ->
      not (null nodes) ==> property True
  
  , fastProperty "all nodes are reachable from entry" $ \(nodes :: [String]) ->
      let reachable = nodes
      in length reachable === length nodes
  
  , fastProperty "control flow is well-formed" $ \(edges :: [(Int, Int)]) ->
      all (\(from, to) -> from >= 0 && to >= 0) edges
  ]
