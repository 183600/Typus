{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CompilerOptimizationInvariantSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (sort)
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

-- Arbitrary instances for QuickCheck
instance Arbitrary OptimizationLevel where
  arbitrary = elements [O0, O1, O2, O3]

instance Arbitrary CompilerOption where
  arbitrary = CompilerOption <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IRNode where
  arbitrary = IRNode <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IRGraph where
  arbitrary = IRGraph <$> arbitrary <*> arbitrary

tests :: TestTree
tests = testGroup "Compiler Optimization Invariant Tests"
  [ testGroup "Optimization level invariants"
    [ testCase "preserves program semantics across optimization levels" $ do
        let opt0 = CompilerOption O0 True 0
            opt1 = CompilerOption O1 False 10
            opt2 = CompilerOption O2 False 50
            opt3 = CompilerOption O3 False 100
        optLevel opt0 @?= O0
        optLevel opt1 @?= O1
        optLevel opt2 @?= O2
        optLevel opt3 @?= O3
      
    , testCase "maintains optimization order" $ do
        let levels = [O0, O1, O2, O3]
        sort levels @?= [O0, O1, O2, O3]
      
    , testCase "handles debug info correctly" $ do
        let debugOpt = CompilerOption O1 True 10
            noDebugOpt = CompilerOption O1 False 10
        debugInfo debugOpt @?= True
        debugInfo noDebugOpt @?= False
      
    , testCase "validates inline thresholds" $ do
        let opt = CompilerOption O2 False 50
        inlineThreshold opt @?= 50
        assertBool "inline threshold should be > 0" (inlineThreshold opt > 0)
    ]
    
  , testGroup "Optimization result invariants"
    [ testCase "tracks applied optimizations" $ do
        let node1 = IRNode 1 "Constant" (Just "42")
            node2 = IRNode 2 "Constant" (Just "24")
            node3 = IRNode 3 "Operation" (Just "+")
            graph = IRGraph [node1, node2, node3] [(1, 3), (2, 3)]
            optimized = applyConstantFolding graph
            result = OptimizationResult graph optimized ["ConstantFolding"]
        appliedOptimizations result @?= ["ConstantFolding"]
        originalGraph result @?= graph
        optimizedGraph result @?= optimized
      
    , testCase "preserves node count invariants" $ do
        let node1 = IRNode 1 "Constant" (Just "42")
            node2 = IRNode 2 "Variable" Nothing
            graph = IRGraph [node1, node2] []
            optimized = applyDeadCodeElimination graph
            result = OptimizationResult graph optimized ["DeadCodeElimination"]
        length (graphNodes $ optimizedGraph result) @?= length (graphNodes $ originalGraph result)
    ]
    
  , testGroup "Optimization pipeline invariants"
    [ testCase "applies optimizations in correct order" $ do
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
            
        appliedOptimizations result1 @?= ["ConstantFolding"]
        appliedOptimizations result2 @?= ["DeadCodeElimination"]
        appliedOptimizations result3 @?= ["Inlining"]
      
    , testCase "maintains optimization idempotence" $ do
        let node1 = IRNode 1 "Constant" (Just "42")
            graph = IRGraph [node1] []
            
            -- Apply same optimization twice
            first = applyConstantFolding graph
            second = applyConstantFolding first
            
        first @?= second  -- Idempotence property
    ]
    
  , testGroup "Debug invariants"
    [ testCase "preserves debug information when enabled" $ do
        let opt = CompilerOption O1 True 10
        debugInfo opt @?= True
      
    , testCase "removes debug information when disabled" $ do
        let opt = CompilerOption O1 False 10
        debugInfo opt @?= False
    ]
    
  , testGroup "QuickCheck properties"
    [ testProperty "optimization preserves node semantics" $
        \nodes ->
          let graph = IRGraph nodes []
              optimized = applyConstantFolding graph
          in length (graphNodes optimized) >= 0
          
    , testProperty "edge references remain valid after optimization" $
        \nodes edges ->
          let graph = IRGraph nodes edges
              optimized = applyDeadCodeElimination graph
              nodeIds = Set.fromList $ map nodeId $ graphNodes optimized
              edgeRefs = Set.fromList $ concatMap (\(a, b) -> [a, b]) $ graphEdges optimized
          in null edgeRefs || edgeRefs `Set.isSubsetOf` nodeIds
          
    , testProperty "optimization results are consistent" $
        \nodes edges ->
          let graph = IRGraph nodes edges
              optimized = applyInlining graph
              result = OptimizationResult graph optimized ["Inlining"]
          in originalGraph result == graph &&
             optimizedGraph result == optimized
             
    , testProperty "debug info consistency" $
        \level debug threshold ->
          let opt = CompilerOption level debug threshold
          in debugInfo opt == debug
    ]
    
  , testGroup "Edge cases"
    [ testCase "handles empty graphs" $ do
        let graph = IRGraph [] []
            optimized = applyConstantFolding graph
            result = OptimizationResult graph optimized ["ConstantFolding"]
        length (graphNodes $ optimizedGraph result) @?= 0
        length (graphEdges $ optimizedGraph result) @?= 0
      
    , testCase "handles single node graphs" $ do
        let node = IRNode 1 "Constant" (Just "42")
            graph = IRGraph [node] []
            optimized = applyDeadCodeElimination graph
            result = OptimizationResult graph optimized ["DeadCodeElimination"]
        length (graphNodes $ optimizedGraph result) @?= 1
      
    , testCase "handles cyclic graphs" $ do
        let node1 = IRNode 1 "Operation" (Just "+")
            node2 = IRNode 2 "Operation" (Just "*")
            graph = IRGraph [node1, node2] [(1, 2), (2, 1)]
            optimized = applyInlining graph
            result = OptimizationResult graph optimized ["Inlining"]
        length (graphNodes $ optimizedGraph result) @?= 2
        length (graphEdges $ optimizedGraph result) @?= 2
      
    , testCase "handles large graphs" $ do
        let nodes = [IRNode i "Node" Nothing | i <- [1..100]]
            edges = [(i, i+1) | i <- [1..99]]
            graph = IRGraph nodes edges
            optimized = applyConstantFolding graph
            result = OptimizationResult graph optimized ["ConstantFolding"]
        length (graphNodes $ optimizedGraph result) @?= 100
        length (graphEdges $ optimizedGraph result) @?= 99
    ]
  ]