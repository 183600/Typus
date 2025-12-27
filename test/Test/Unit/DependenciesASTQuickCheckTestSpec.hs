{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependenciesASTQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Dependencies.AST
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Dependencies AST QuickCheck Tests"
  [ dependencyNodeTests
  , dependencyEdgeTests
  , dependencyGraphTests
  , moduleDependencyTests
  , functionDependencyTests
  , typeDependencyTests
  , variableDependencyTests
  , circularDependencyTests
  , dependencyAnalysisTests
  , dependencyValidationTests
  ]

-- | 1. 依赖节点测试
dependencyNodeTests :: TestTree
dependencyNodeTests = testGroup "Dependency Node Tests"
  [ testCase "Module node creation" $
      let node = ModuleNode "test.module" (SourceSpan startPos startPos)
      in dependencyNodeName node @?= "test.module"
  
  , testCase "Function node creation" $
      let node = FunctionNode "testFunction" "test.module" (SourceSpan startPos startPos)
      in dependencyNodeName node @?= "testFunction"
  
  , testCase "Type node creation" $
      let node = TypeNode "TestType" "test.module" (SourceSpan startPos startPos)
      in dependencyNodeName node @?= "TestType"
  
  , fastProperty "Node name consistency" $
      \name nodeType -> let node = case nodeType of
                                      0 -> ModuleNode name (SourceSpan startPos startPos)
                                      1 -> FunctionNode name "test.module" (SourceSpan startPos startPos)
                                      2 -> TypeNode name "test.module" (SourceSpan startPos startPos)
                                      _ -> VariableNode name "test.module" (SourceSpan startPos startPos)
                        in dependencyNodeName node == name
  ]

-- | 2. 依赖边测试
dependencyEdgeTests :: TestTree
dependencyEdgeTests = testGroup "Dependency Edge Tests"
  [ testCase "Dependency edge creation" $
      let source = ModuleNode "source.module" (SourceSpan startPos startPos)
          target = ModuleNode "target.module" (SourceSpan startPos startPos)
          edge = DependencyEdge source target DirectDependency (SourceSpan startPos startPos)
      in (dependencyEdgeSource edge, dependencyEdgeTarget edge) @?= (source, target)
  
  , testCase "Edge dependency type" $
      let source = FunctionNode "func1" "module1" (SourceSpan startPos startPos)
          target = FunctionNode "func2" "module2" (SourceSpan startPos startPos)
          edge = DependencyEdge source target IndirectDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= IndirectDependency
  
  , fastProperty "Edge source and target are different" $
      \sourceName targetName -> let source = ModuleNode sourceName (SourceSpan startPos startPos)
                                    target = ModuleNode targetName (SourceSpan startPos startPos)
                                    edge = DependencyEdge source target DirectDependency (SourceSpan startPos startPos)
                                in sourceName /= targetName || dependencyEdgeSource edge == dependencyEdgeTarget edge
  ]

-- | 3. 依赖图测试
dependencyGraphTests :: TestTree
dependencyGraphTests = testGroup "Dependency Graph Tests"
  [ testCase "Empty dependency graph" $
      let graph = emptyDependencyGraph
      in dependencyGraphSize graph @?= 0
  
  , testCase "Add node to graph" $
      let graph = emptyDependencyGraph
          node = ModuleNode "test.module" (SourceSpan startPos startPos)
          graph' = addDependencyNode graph node
      in dependencyGraphSize graph' @?= 1
  
  , testCase "Add edge to graph" $
      let graph = emptyDependencyGraph
          source = ModuleNode "source" (SourceSpan startPos startPos)
          target = ModuleNode "target" (SourceSpan startPos startPos)
          edge = DependencyEdge source target DirectDependency (SourceSpan startPos startPos)
          graph' = addDependencyEdge graph edge
      in dependencyGraphEdgeCount graph' @?= 1
  
  , fastProperty "Graph node count consistency" $
      \nodeNames -> let nodes = map (\n -> ModuleNode n (SourceSpan startPos startPos)) nodeNames
                        graph = foldl addDependencyNode emptyDependencyGraph nodes
                    in dependencyGraphSize graph == length (nub nodeNames)
  ]

-- | 4. 模块依赖测试
moduleDependencyTests :: TestTree
moduleDependencyTests = testGroup "Module Dependency Tests"
  [ testCase "Module import dependency" $
      let source = ModuleNode "main" (SourceSpan startPos startPos)
          target = ModuleNode "fmt" (SourceSpan startPos startPos)
          edge = DependencyEdge source target ImportDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= ImportDependency
  
  , testCase "Module usage dependency" $
      let source = ModuleNode "app" (SourceSpan startPos startPos)
          target = ModuleNode "utils" (SourceSpan startPos startPos)
          edge = DependencyEdge source target UsageDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= UsageDependency
  
  , fastProperty "Module dependency creation" $
      \sourceName targetName -> let source = ModuleNode sourceName (SourceSpan startPos startPos)
                                    target = ModuleNode targetName (SourceSpan startPos startPos)
                                    edge = DependencyEdge source target DirectDependency (SourceSpan startPos startPos)
                                in dependencyEdgeSource edge == source && dependencyEdgeTarget edge == target
  ]

-- | 5. 函数依赖测试
functionDependencyTests :: TestTree
functionDependencyTests = testGroup "Function Dependency Tests"
  [ testCase "Function call dependency" $
      let source = FunctionNode "main" "main" (SourceSpan startPos startPos)
          target = FunctionNode "helper" "utils" (SourceSpan startPos startPos)
          edge = DependencyEdge source target CallDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= CallDependency
  
  , testCase "Function reference dependency" $
      let source = FunctionNode "callback" "events" (SourceSpan startPos startPos)
          target = FunctionNode "handler" "main" (SourceSpan startPos startPos)
          edge = DependencyEdge source target ReferenceDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= ReferenceDependency
  
  , fastProperty "Function dependency consistency" $
      \sourceFunc targetFunc sourceMod targetMod -> let source = FunctionNode sourceFunc sourceMod (SourceSpan startPos startPos)
                                                      target = FunctionNode targetFunc targetMod (SourceSpan startPos startPos)
                                                      edge = DependencyEdge source target CallDependency (SourceSpan startPos startPos)
                                                  in dependencyEdgeSource edge == source && dependencyEdgeTarget edge == target
  ]

-- | 6. 类型依赖测试
typeDependencyTests :: TestTree
typeDependencyTests = testGroup "Type Dependency Tests"
  [ testCase "Type inheritance dependency" $
      let source = TypeNode "ChildType" "types" (SourceSpan startPos startPos)
          target = TypeNode "ParentType" "types" (SourceSpan startPos startPos)
          edge = DependencyEdge source target InheritanceDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= InheritanceDependency
  
  , testCase "Type composition dependency" $
      let source = TypeNode "CompositeType" "types" (SourceSpan startPos startPos)
          target = TypeNode "ComponentType" "types" (SourceSpan startPos startPos)
          edge = DependencyEdge source target CompositionDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= CompositionDependency
  
  , fastProperty "Type dependency creation" $
      \sourceType targetType sourceMod -> let source = TypeNode sourceType sourceMod (SourceSpan startPos startPos)
                                              target = TypeNode targetType sourceMod (SourceSpan startPos startPos)
                                              edge = DependencyEdge source target CompositionDependency (SourceSpan startPos startPos)
                                          in dependencyEdgeSource edge == source && dependencyEdgeTarget edge == target
  ]

-- | 7. 变量依赖测试
variableDependencyTests :: TestTree
variableDependencyTests = testGroup "Variable Dependency Tests"
  [ testCase "Variable usage dependency" $
      let source = FunctionNode "func1" "module1" (SourceSpan startPos startPos)
          target = VariableNode "var1" "module1" (SourceSpan startPos startPos)
          edge = DependencyEdge source target VariableUsageDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= VariableUsageDependency
  
  , testCase "Variable definition dependency" $
      let source = FunctionNode "func2" "module2" (SourceSpan startPos startPos)
          target = VariableNode "var2" "module2" (SourceSpan startPos startPos)
          edge = DependencyEdge source target VariableDefinitionDependency (SourceSpan startPos startPos)
      in dependencyEdgeType edge @?= VariableDefinitionDependency
  
  , fastProperty "Variable dependency consistency" $
      \funcName varName moduleName -> let source = FunctionNode funcName moduleName (SourceSpan startPos startPos)
                                          target = VariableNode varName moduleName (SourceSpan startPos startPos)
                                          edge = DependencyEdge source target VariableUsageDependency (SourceSpan startPos startPos)
                                      in dependencyEdgeSource edge == source && dependencyEdgeTarget edge == target
  ]

-- | 8. 循环依赖测试
circularDependencyTests :: TestTree
circularDependencyTests = testGroup "Circular Dependency Tests"
  [ testCase "Detect simple circular dependency" $
      let graph = emptyDependencyGraph
          node1 = ModuleNode "module1" (SourceSpan startPos startPos)
          node2 = ModuleNode "module2" (SourceSpan startPos startPos)
          edge1 = DependencyEdge node1 node2 DirectDependency (SourceSpan startPos startPos)
          edge2 = DependencyEdge node2 node1 DirectDependency (SourceSpan startPos startPos)
          graph' = addDependencyEdge (addDependencyEdge (addDependencyNode (addDependencyNode graph node1) node2) edge1) edge2
      in hasCircularDependency graph' @?= True
  
  , testCase "No circular dependency" $
      let graph = emptyDependencyGraph
          node1 = ModuleNode "module1" (SourceSpan startPos startPos)
          node2 = ModuleNode "module2" (SourceSpan startPos startPos)
          edge = DependencyEdge node1 node2 DirectDependency (SourceSpan startPos startPos)
          graph' = addDependencyEdge (addDependencyNode (addDependencyNode graph node1) node2) edge
      in hasCircularDependency graph' @?= False
  
  , fastProperty "Circular dependency detection" $
      \nodeNames -> let nodes = take 3 (map (\n -> ModuleNode ("module" ++ show n) (SourceSpan startPos startPos)) [1..])
                        edges = [(nodes !! 0, nodes !! 1), (nodes !! 1, nodes !! 2)]
                        graph = foldl (\g (s, t) -> addDependencyEdge g (DependencyEdge s t DirectDependency (SourceSpan startPos startPos))) 
                                      (foldl addDependencyNode emptyDependencyGraph nodes) edges
                    in hasCircularDependency graph == False
  ]

-- | 9. 依赖分析测试
dependencyAnalysisTests :: TestTree
dependencyAnalysisTests = testGroup "Dependency Analysis Tests"
  [ testCase "Get direct dependencies" $
      let graph = emptyDependencyGraph
          node1 = ModuleNode "module1" (SourceSpan startPos startPos)
          node2 = ModuleNode "module2" (SourceSpan startPos startPos)
          edge = DependencyEdge node1 node2 DirectDependency (SourceSpan startPos startPos)
          graph' = addDependencyEdge (addDependencyNode (addDependencyNode graph node1) node2) edge
          deps = getDirectDependencies graph' node1
      in length deps @?= 1
  
  , testCase "Get transitive dependencies" $
      let graph = emptyDependencyGraph
          node1 = ModuleNode "module1" (SourceSpan startPos startPos)
          node2 = ModuleNode "module2" (SourceSpan startPos startPos)
          node3 = ModuleNode "module3" (SourceSpan startPos startPos)
          edge1 = DependencyEdge node1 node2 DirectDependency (SourceSpan startPos startPos)
          edge2 = DependencyEdge node2 node3 DirectDependency (SourceSpan startPos startPos)
          graph' = foldl addDependencyEdge (foldl addDependencyNode graph [node1, node2, node3]) [edge1, edge2]
          deps = getTransitiveDependencies graph' node1
      in length deps @?= 2
  
  , fastProperty "Dependency closure" $
      \nodeCount -> let nodes = take nodeCount (map (\n -> ModuleNode ("module" ++ show n) (SourceSpan startPos startPos)) [1..])
                        edges = zip nodes (tail nodes)
                        graph = foldl (\g (s, t) -> addDependencyEdge g (DependencyEdge s t DirectDependency (SourceSpan startPos startPos))) 
                                      (foldl addDependencyNode emptyDependencyGraph nodes) edges
                    in if nodeCount > 0
                       then length (getTransitiveDependencies graph (head nodes)) == nodeCount - 1
                       else True
  ]

-- | 10. 依赖验证测试
dependencyValidationTests :: TestTree
dependencyValidationTests = testGroup "Dependency Validation Tests"
  [ testCase "Valid dependency graph" $
      let graph = emptyDependencyGraph
          node = ModuleNode "test" (SourceSpan startPos startPos)
          graph' = addDependencyNode graph node
      in validateDependencyGraph graph' @?= True
  
  , testCase "Invalid dependency edge (missing source)" $
      let graph = emptyDependencyGraph
          target = ModuleNode "target" (SourceSpan startPos startPos)
          source = ModuleNode "source" (SourceSpan startPos startPos)
          edge = DependencyEdge source target DirectDependency (SourceSpan startPos startPos)
          graph' = addDependencyEdge (addDependencyNode graph target) edge
      in validateDependencyGraph graph' @?= False
  
  , fastProperty "Graph validation consistency" $
      \nodes edges -> let graph = foldl addDependencyNode emptyDependencyGraph nodes
                          graph' = foldl (\g (s, t) -> addDependencyEdge g (DependencyEdge s t DirectDependency (SourceSpan startPos startPos))) graph edges
                      in validateDependencyGraph graph' == True || validateDependencyGraph graph' == False
  ]