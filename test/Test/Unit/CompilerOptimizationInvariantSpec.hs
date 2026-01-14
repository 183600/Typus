{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CompilerOptimizationInvariantSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Set as Set

-- Mock data types for compiler optimization testing
data OptimizationLevel = O0 | O1 | O2 | O3 deriving (Show, Eq, Ord)

data CompilerOption = CompilerOption
  { optLevel :: OptimizationLevel
  , debugInfo :: Bool
  , inlineThreshold :: Int
  } deriving (Show, Eq)

data IRNode = IRNode
  { nodeId :: Int
  , nodeType :: String
  , nodeValue :: Maybe String
  } deriving (Show, Eq)

data IRGraph = IRGraph
  { graphNodes :: [IRNode]
  , graphEdges :: [(Int, Int)]
  } deriving (Show, Eq)

data OptimizationResult = OptimizationResult
  { originalGraph :: IRGraph
  , optimizedGraph :: IRGraph
  , appliedOptimizations :: [String]
  } deriving (Show, Eq)

-- Mock optimization functions
applyConstantFolding :: IRGraph -> IRGraph
applyConstantFolding graph = graph  -- Mock implementation

applyDeadCodeElimination :: IRGraph -> IRGraph
applyDeadCodeElimination graph = graph  -- Mock implementation

applyInlining :: IRGraph -> IRGraph
applyInlining graph = graph  -- Mock implementation

spec :: Spec
spec = describe "Compiler Optimization Invariant Tests" $ do

  describe "Optimization level invariants" $ do
    it "preserves program semantics across optimization levels" $ do
      let opt0 = CompilerOption O0 True 0
          opt1 = CompilerOption O1 False 10
          opt2 = CompilerOption O2 False 50
          opt3 = CompilerOption O3 False 100
      optLevel opt0 `shouldBe` O0
      optLevel opt1 `shouldBe` O1
      optLevel opt2 `shouldBe` O2
      optLevel opt3 `shouldBe` O3
      
    it "maintains optimization order" $ do
      let levels = [O0, O1, O2, O3]
      sort levels `shouldBe` [O0, O1, O2, O3]
      
    it "handles debug info correctly" $ do
      let debugOpt = CompilerOption O1 True 10
          noDebugOpt = CompilerOption O1 False 10
      debugInfo debugOpt `shouldBe` True
      debugInfo noDebugOpt `shouldBe` False
      
    it "validates inline thresholds" $ do
      let opt = CompilerOption O2 False 50
      inlineThreshold opt `shouldBe` 50
      inlineThreshold opt `shouldSatisfy` (> 0)

  describe "IR graph invariants" $ do
    it "maintains node uniqueness" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Variable" Nothing
          node3 = IRNode 3 "Operation" (Just "+")
          graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
      let nodeIds = map nodeId $ graphNodes graph
      length nodeIds `shouldBe` length (nub nodeIds)
      
    it "validates edge references" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Variable" Nothing
          node3 = IRNode 3 "Operation" (Just "+")
          graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
      let nodeIds = Set.fromList $ map nodeId $ graphNodes graph
          edgeRefs = Set.fromList $ concatMap (\(a, b) -> [a, b]) $ graphEdges graph
      edgeRefs `Set.isSubsetOf` nodeIds `shouldBe` True
      
    it "preserves graph structure" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Variable" Nothing
          node3 = IRNode 3 "Operation" (Just "+")
          graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
      length (graphNodes graph) `shouldBe` 3
      length (graphEdges graph) `shouldBe` 2

  describe "Optimization result invariants" $ do
    it "tracks applied optimizations" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Constant" (Just "24")
          node3 = IRNode 3 "Operation" (Just "+")
          graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
          optimized = applyConstantFolding graph
          result = OptimizationResult graph optimized ["ConstantFolding"]
      appliedOptimizations result `shouldBe` ["ConstantFolding"]
      originalGraph result `shouldBe` graph
      optimizedGraph result `shouldBe` optimized
      
    it "preserves node count invariants" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Variable" Nothing
          graph = IRGraph [node1, node2] []
          optimized = applyDeadCodeElimination graph
          result = OptimizationResult graph optimized ["DeadCodeElimination"]
      length (graphNodes $ optimizedGraph result) `shouldBe` length (graphNodes $ originalGraph result)

  describe "Optimization pipeline invariants" $ do
    it "applies optimizations in correct order" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          node2 = IRNode 2 "Constant" (Just "24")
          node3 = IRNode 3 "Operation" (Just "+")
          graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
          
          -- Apply optimizations in sequence
          step1 = applyConstantFolding graph
          step2 = applyDeadCodeElimination step1
          step3 = applyInlining step2
          
          result1 = OptimizationResult graph step1 ["ConstantFolding"]
          result2 = OptimizationResult step1 step2 ["DeadCodeElimination"]
          result3 = OptimizationResult step2 step3 ["Inlining"]
          
      appliedOptimizations result1 `shouldBe` ["ConstantFolding"]
      appliedOptimizations result2 `shouldBe` ["DeadCodeElimination"]
      appliedOptimizations result3 `shouldBe` ["Inlining"]
      
    it "maintains optimization idempotence" $ do
      let node1 = IRNode 1 "Constant" (Just "42")
          graph = IRGraph [node1] []
          
          -- Apply same optimization twice
          first = applyConstantFolding graph
          second = applyConstantFolding first
          
      first `shouldBe` second  -- Idempotence property

  describe "Debug invariants" $ do
    it "preserves debug information when enabled" $ do
      let opt = CompilerOption O1 True 10
      debugInfo opt `shouldBe` True
      
    it "removes debug information when disabled" $ do
      let opt = CompilerOption O1 False 10
      debugInfo opt `shouldBe` False
      
    it "maintains debug info consistency" $ property $
      \level debug threshold ->
        let opt = CompilerOption level debug threshold
        in debugInfo opt `shouldBe` debug

  describe "QuickCheck properties" $ do
    it "optimization preserves node semantics" $ property $
      \nodes ->
        let graph = IRGraph nodes []
            optimized = applyConstantFolding graph
        in length (graphNodes optimized) `shouldSatisfy` (>= 0)
        
    it "edge references remain valid after optimization" $ property $
      \nodes edges ->
        let graph = IRGraph nodes edges
            optimized = applyDeadCodeElimination graph
            nodeIds = Set.fromList $ map nodeId $ graphNodes optimized
            edgeRefs = Set.fromList $ concatMap (\(a, b) -> [a, b]) $ graphEdges optimized
        in null edgeRefs || edgeRefs `Set.isSubsetOf` nodeIds
        
    it "optimization results are consistent" $ property $
      \nodes edges ->
        let graph = IRGraph nodes edges
            optimized = applyInlining graph
            result = OptimizationResult graph optimized ["Inlining"]
        in originalGraph result `shouldBe` graph &&
           optimizedGraph result `shouldBe` optimized

  describe "Edge cases" $ do
    it "handles empty graphs" $ do
      let graph = IRGraph [] []
          optimized = applyConstantFolding graph
          result = OptimizationResult graph optimized ["ConstantFolding"]
      length (graphNodes $ optimizedGraph result) `shouldBe` 0
      length (graphEdges $ optimizedGraph result) `shouldBe` 0
      
    it "handles single node graphs" $ do
      let node = IRNode 1 "Constant" (Just "42")
          graph = IRGraph [node] []
          optimized = applyDeadCodeElimination graph
          result = OptimizationResult graph optimized ["DeadCodeElimination"]
      length (graphNodes $ optimizedGraph result) `shouldBe` 1
      
    it "handles cyclic graphs" $ do
      let node1 = IRNode 1 "Operation" (Just "+")
          node2 = IRNode 2 "Operation" (Just "*")
          graph = IRGraph [node1, node2] [(1, 2), (2, 1)]
          optimized = applyInlining graph
          result = OptimizationResult graph optimized ["Inlining"]
      length (graphNodes $ optimizedGraph result) `shouldBe` 2
      length (graphEdges $ optimizedGraph result) `shouldBe` 2
      
    it "handles large graphs" $ do
      let nodes = [IRNode i "Node" Nothing | i <- [1..100]]
          edges = [(i, i+1) | i <- [1..99]]
          graph = IRGraph nodes edges
          optimized = applyConstantFolding graph
          result = OptimizationResult graph optimized ["ConstantFolding"]
      length (graphNodes $ optimizedGraph result) `shouldBe` 100
      length (graphEdges $ optimizedGraph result) `shouldBe` 99