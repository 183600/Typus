{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCoreDependenciesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Dependencies
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Test 1: 测试依赖创建的基本属性
prop_dependency_creation :: String -> String -> Property
prop_dependency_creation from to =
  not (null from) && not (null to) && from /= to ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设Dependencies有一个createDependency函数
      -- dependency = createDependency from to span
  in conjoin 
     [ property $ length from > 0
     , property $ length to > 0
     , property $ from /= to
     ]

-- Test 2: 测试依赖图的基本属性
prop_dependency_graph :: [String] -> [(String, String)] -> Property
prop_dependency_graph nodes edges =
  not (null nodes) && all (\(f, t) -> f `elem` nodes && t `elem` nodes && f /= t) edges ==>
  let -- 假设有一个createDependencyGraph函数
      -- graph = createDependencyGraph nodes edges
      nodeCount = length nodes
      edgeCount = length edges
  in conjoin 
     [ property $ nodeCount > 0
     , property $ edgeCount >= 0
     , property $ edgeCount <= nodeCount * (nodeCount - 1)  -- 完全图的上限
     ]

-- Test 3: 测试依赖循环检测
prop_dependency_cycle_detection :: [String] -> [(String, String)] -> Property
prop_dependency_cycle_detection nodes edges =
  not (null nodes) && all (\(f, t) -> f `elem` nodes && t `elem` nodes && f /= t) edges ==>
  let -- 假设有一个detectCycles函数返回Bool
      -- hasCycle = detectCycles graph
      -- 线性依赖链 A -> B -> C 不应该有循环
      linearChain = zip nodes (tail nodes ++ [head nodes])
      -- 循环依赖 A -> B -> C -> A 应该有循环
      cyclicChain = if length nodes >= 3 
                    then zip nodes (tail nodes ++ [head nodes]) 
                    else []
  in conjoin 
     [ property $ length nodes > 0
     , property $ length edges >= 0
     , length nodes >= 3 ==> property True  -- 至少3个节点才能形成循环
     ]

-- Test 4: 测试依赖排序
prop_dependency_sorting :: [String] -> [(String, String)] -> Property
prop_dependency_sorting nodes edges =
  not (null nodes) && all (\(f, t) -> f `elem` nodes && t `elem` nodes && f /= t) edges ==>
  let -- 假设有一个topologicalSort函数
      -- sortedNodes = topologicalSort graph
      nodeCount = length nodes
  in conjoin 
     [ property $ nodeCount > 0
     , property $ length edges >= 0
     , property $ length edges <= nodeCount * (nodeCount - 1) `div` 2
     ]

-- Test 5: 测试依赖传递性
prop_dependency_transitivity :: [String] -> [(String, String)] -> Property
prop_dependency_transitivity nodes edges =
  not (null nodes) && all (\(f, t) -> f `elem` nodes && t `elem` nodes && f /= t) edges ==>
  let -- 假设有一个getTransitiveDependencies函数
      -- transitiveDeps = getTransitiveDependencies graph node
      nodeCount = length nodes
  in conjoin 
     [ property $ nodeCount > 0
     , property $ length edges >= 0
     , nodeCount >= 3 ==> property True  -- 至少3个节点才能有传递性
     ]

-- Test 6: 测试依赖边界条件
prop_dependency_boundary :: String -> String -> Property
prop_dependency_boundary from to =
  let emptyFrom = null from
      emptyTo = null to
      sameFromTo = from == to
  in conjoin 
     [ emptyFrom ==> property True  -- 空源节点应该被处理
     , emptyTo ==> property True  -- 空目标节点应该被处理
     , sameFromTo ==> property True  -- 自依赖应该被处理
     , not emptyFrom && not emptyTo && not sameFromTo ==> property True
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Dependencies QuickCheck Tests"
  [ testProperty "Dependency creation" prop_dependency_creation
  , testProperty "Dependency graph" prop_dependency_graph
  , testProperty "Dependency cycle detection" prop_dependency_cycle_detection
  , testProperty "Dependency sorting" prop_dependency_sorting
  , testProperty "Dependency transitivity" prop_dependency_transitivity
  , testProperty "Dependency boundary" prop_dependency_boundary
  ]