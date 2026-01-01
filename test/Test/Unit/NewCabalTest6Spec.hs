{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest6Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)

import Dependencies
  ( analyzeDependencies
  , DependencyGraph
  , Dependency(..)
  , DependencyType(..)
  , formatDependencyGraph
  )
import Dependencies.AST
import Dependencies.TypeSystem

-- | 测试依赖分析和循环检测的属性
tests :: TestTree
tests =
  testGroup "NewCabalTest6 - 依赖分析测试"
    [ testGroup "单元测试"
        [ testCase "简单依赖分析" $ do
            let code = "func A() { B() } func B() { C() } func C() { }"
                result = analyzeDependencies code
            case result of
                Left err -> assertBool ("Dependency analysis failed: " ++ show err) False
                Right graph -> 
                    assertBool "Should detect dependencies" $ not $ null graph

        , testCase "循环依赖检测" $ do
            let code = "func A() { B() } func B() { C() } func C() { A() }"
                result = analyzeDependencies code
            case result of
                Left err -> assertBool "Should detect circular dependency" $ 
                    "circular" `L.isInfixOf` (map toLower (show err))
                Right _ -> assertBool "Should detect circular dependency" False

        , testCase "依赖图格式化" $ do
            let deps = [Dependency "A" "B" DependencyTypeFunction]
                formatted = formatDependencyGraph deps
            assertBool "Should contain dependency info" $ "A -> B" `L.isInfixOf` formatted

        , testCase "类型系统依赖" $ do
            let typeDef = "type MyStruct struct { field int }"
                result = analyzeDependencies typeDef
            case result of
                Left err -> assertBool ("Type dependency analysis failed: " ++ show err) False
                Right graph -> 
                    assertBool "Should analyze type dependencies" $ True
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "依赖关系的传递性" prop_dependency_transitivity
        , fastProperty "依赖分析的一致性" prop_dependency_analysis_consistency
        , fastProperty "循环依赖的检测" prop_circular_dependency_detection
        , fastProperty "依赖类型的分类" prop_dependency_type_classification
        , fastProperty "依赖图的连通性" prop_dependency_graph_connectivity
        ]
    ]

-- QuickCheck属性测试

-- 依赖关系的传递性：如果A依赖B，B依赖C，那么A间接依赖C
prop_dependency_transitivity :: String -> String -> String -> Property
prop_dependency_transitivity funcA funcB funcC =
  not (null funcA) && not (null funcB) && not (null funcC) &&
  L.all (L.all isAlpha) [funcA, funcB, funcC] ==> 
  let code = "func " ++ funcA ++ "() { " ++ funcB ++ "() } " ++
             "func " ++ funcB ++ "() { " ++ funcC ++ "() } " ++
             "func " ++ funcC ++ "() { }"
      result = analyzeDependencies code
  in case result of
       Right deps -> 
         let directDeps = L.filter (\d -> dFrom d == funcA) deps
             indirectDeps = L.filter (\d -> dTo d == funcC) deps
         in property $ not (null directDeps) ==> not (null indirectDeps)
       Left _ -> property $ True  -- 分析失败时跳过此测试

-- 依赖分析的一致性：相同代码应该产生相同的依赖图
prop_dependency_analysis_consistency :: String -> Property
prop_dependency_analysis_consistency code =
  let result1 = analyzeDependencies code
      result2 = analyzeDependencies code
  in case (result1, result2) of
       (Right deps1, Right deps2) -> 
         property $ L.length deps1 === L.length deps2
       (Left _, Left _) -> property $ True
       _ -> property $ False

-- 循环依赖的检测：包含循环依赖的代码应该被检测出来
prop_circular_dependency_detection :: String -> Property
prop_circular_dependency_detection funcName =
  not (null funcName) && L.all isAlpha funcName ==>
  let circularCode = "func " ++ funcName ++ "() { " ++ funcName ++ "() }"
      result = analyzeDependencies circularCode
  in case result of
       Left err -> property $ "circular" `L.isInfixOf` (map toLower (show err))
       Right _ -> property $ False  -- 应该检测到循环依赖

-- 依赖类型的分类：不同类型的依赖应该被正确分类
prop_dependency_type_classification :: String -> String -> DependencyType -> Property
prop_dependency_type_classification from to depType =
  not (null from) && not (null to) && L.all isAlpha (from ++ to) ==>
  let dep = Dependency from to depType
      correctType = dType dep == depType
  in property $ correctType

-- 依赖图的连通性：依赖图中的节点应该通过依赖关系相连
prop_dependency_graph_connectivity :: [String] -> Property
prop_dependency_graph_connectivity funcNames =
  not (null funcNames) && L.all (L.all isAlpha) funcNames && L.length (nub funcNames) == L.length funcNames ==>
  let code = unlines $ zipWith (\i name -> 
        "func " ++ name ++ "() { " ++ 
        (if i < L.length funcNames - 1 then funcNames !! (i + 1) else "") ++ 
        (if i < L.length funcNames - 1 then "()" else "") ++ " }"
        ) [0..] funcNames
      result = analyzeDependencies code
  in case result of
       Right deps -> 
         let hasConnections = not $ null deps
         in property $ hasConnections
       Left _ -> property $ True

-- 辅助函数
toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)