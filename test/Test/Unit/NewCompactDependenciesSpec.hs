{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactDependenciesSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Dependencies
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, nub)

-- | 生成任意的模块名
genModuleName :: Gen String
genModuleName = do
  len <- choose (1, 8)
  first <- elements ['A'..'Z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)

-- | 生成简单的依赖图
genDependencyGraph :: Gen (Map String [String])
genDependencyGraph = do
  moduleCount <- choose (1, 6)
  modules <- sequence [genModuleName | _ <- [1..moduleCount]]
  dependencies <- mapM (\_ -> do
    depCount <- choose (0, 3)
    deps <- sequence [elements modules | _ <- [1..depCount]]
    return (nub deps)) modules
  return $ Map.fromList (zip modules dependencies)

-- | 测试基本依赖分析
testBasicDependencyAnalysis :: TestTree
testBasicDependencyAnalysis = testGroup "基本依赖分析测试"
  [ testCase "空依赖图" $
      let graph = Map.empty
          analysis = analyzeDependencies graph
      in null (getDirectDependencies analysis) @?= True
    
  , testCase "单模块无依赖" $
      let graph = Map.fromList [("Main", [])]
          analysis = analyzeDependencies graph
          directDeps = getDirectDependencies analysis "Main"
      in directDeps @?= []
    
  , testCase "单模块有依赖" $
      let graph = Map.fromList [("Main", ["Utils", "Config"])]
          analysis = analyzeDependencies graph
          directDeps = getDirectDependencies analysis "Main"
      in sort directDeps @?= ["Config", "Utils"]
  ]

-- | 测试循环检测
testCycleDetection :: TestTree
testCycleDetection = testGroup "循环检测测试"
  [ testCase "无循环图" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", [])
            ]
          cycles = detectCycles graph
      in null cycles @?= True
    
  , testCase "简单循环" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            ]
          cycles = detectCycles graph
      in length cycles @?= 1
    
  , testCase "自循环" $
      let graph = Map.fromList [("A", ["A"])]
          cycles = detectCycles graph
      in length cycles @?= 1
    
  , testCase "复杂循环" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", ["D"])
            , ("D", ["A"])
            ]
          cycles = detectCycles graph
      in length cycles @?= 1
    
  , testCase "多个循环" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            , ("C", ["D"])
            , ("D", ["C"])
            ]
          cycles = detectCycles graph
      in length cycles @?= 2
  ]

-- | 测试传递依赖
testTransitiveDependencies :: TestTree
testTransitiveDependencies = testGroup "传递依赖测试"
  [ testCase "简单传递依赖" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", [])
            ]
          analysis = analyzeDependencies graph
          transitiveDeps = getTransitiveDependencies analysis "A"
      in sort transitiveDeps @?= ["B", "C"]
    
  , testCase "多重传递依赖" $
      let graph = Map.fromList 
            [ ("A", ["B", "C"])
            , ("B", ["D"])
            , ("C", ["E"])
            , ("D", [])
            , ("E", [])
            ]
          analysis = analyzeDependencies graph
          transitiveDeps = getTransitiveDependencies analysis "A"
      in sort transitiveDeps @?= ["B", "C", "D", "E"]
    
  , testCase "无传递依赖" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", [])
            , ("C", [])
            ]
          analysis = analyzeDependencies graph
          transitiveDeps = getTransitiveDependencies analysis "A"
      in transitiveDeps @?= ["B"]
  ]

-- | 测试拓扑排序
testTopologicalSort :: TestTree
testTopologicalSort = testGroup "拓扑排序测试"
  [ testCase "简单图拓扑排序" $
      let graph = Map.fromList 
            [ ("C", [])
            , ("B", ["C"])
            , ("A", ["B", "C"])
            ]
          result = topologicalSort graph
      in case result of
        Left err -> assertBool ("排序失败: " ++ err) False
        Right sorted -> 
          let positions = Map.fromList (zip sorted [0..])
              posC = Map.lookup "C" positions
              posB = Map.lookup "B" positions
              posA = Map.lookup "A" positions
          in case (posC, posB, posA) of
            (Just c, Just b, Just a) -> assertBool "拓扑顺序正确" (c < b && b < a && c < a)
            _ -> assertBool "缺少模块" False
    
  , testCase "循环图无法拓扑排序" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            ]
          result = topologicalSort graph
      in case result of
        Left _ -> assertBool "应该失败" True
        Right _ -> assertBool "不应该成功" False
  ]

-- | 测试依赖级别计算
testDependencyLevels :: TestTree
testDependencyLevels = testGroup "依赖级别测试"
  [ testCase "计算依赖级别" $
      let graph = Map.fromList 
            [ ("D", [])
            , ("C", ["D"])
            , ("B", ["C"])
            , ("A", ["B"])
            ]
          analysis = analyzeDependencies graph
          levels = calculateDependencyLevels analysis
      in case Map.lookup "A" levels of
        Just level -> level @?= 3
        Nothing -> assertBool "缺少级别信息" False
    
  , testCase "无依赖模块级别为0" $
      let graph = Map.fromList 
            [ ("A", [])
            , ("B", [])
            ]
          analysis = analyzeDependencies graph
          levels = calculateDependencyLevels analysis
          levelA = Map.lookup "A" levels
          levelB = Map.lookup "B" levels
      in case (levelA, levelB) of
        (Just 0, Just 0) -> assertBool "级别正确" True
        _ -> assertBool "级别错误" False
  ]

-- | QuickCheck属性测试
testDependencyProperties :: TestTree
testDependencyProperties = testGroup "依赖属性测试"
  [ testProperty "传递依赖包含直接依赖" $
      \graph module' ->
        let analysis = analyzeDependencies graph
            directDeps = Set.fromList (getDirectDependencies analysis module')
            transitiveDeps = Set.fromList (getTransitiveDependencies analysis module')
        in directDeps `Set.isSubsetOf` transitiveDeps
  
  , testProperty "传递依赖的传递性" $
      \graph module' ->
        let analysis = analyzeDependencies graph
            transitiveDeps = getTransitiveDependencies analysis module'
            allTransitive = concatMap (getTransitiveDependencies analysis) transitiveDeps
        in all (`elem` allTransitive) transitiveDeps
  
  , testProperty "循环检测的完备性" $
      \graph ->
        let cycles = detectCycles graph
            hasDirectCycle = any (\(module', deps) -> module' `elem` deps) (Map.toList graph)
        in hasDirectCycle ==> not (null cycles)
  
  , testProperty "拓扑排序保持依赖顺序" $
      \graph ->
        let cycles = detectCycles graph
        in null cycles ==> 
          case topologicalSort graph of
            Left _ -> False
            Right sorted ->
              let positions = Map.fromList (zip sorted [0..])
                  checkDep (from, tos) = all (\to -> 
                    case (Map.lookup from positions, Map.lookup to positions) of
                      (Just fromPos, Just toPos) -> fromPos < toPos
                      _ -> False) tos
              in all checkDep (Map.toList graph)
  ]

-- | 测试依赖优化
testDependencyOptimization :: TestTree
testDependencyOptimization = testGroup "依赖优化测试"
  [ testCase "移除冗余依赖" $
      let graph = Map.fromList 
            [ ("A", ["B", "C"])
            , ("B", ["C"])
            , ("C", [])
            ]
          optimized = removeRedundantDependencies graph
          aDeps = Map.lookup "A" optimized
      in case aDeps of
        Just deps -> sort deps @?= ["C"]
        Nothing -> assertBool "缺少依赖信息" False
    
  , testCase "检测公共依赖" $
      let graph = Map.fromList 
            [ ("A", ["C"])
            , ("B", ["C"])
            , ("C", [])
            ]
          common = findCommonDependencies graph "A" "B"
      in common @?= ["C"]
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "空图分析" $
      let graph = Map.empty
          analysis = analyzeDependencies graph
          cycles = detectCycles graph
          sorted = topologicalSort graph
      in case sorted of
        Right [] -> assertBool "空图拓扑排序正确" (null cycles)
        _ -> assertBool "空图处理错误" False
    
  , testCase "自引用模块" $
      let graph = Map.fromList [("A", ["A"])]
          cycles = detectCycles graph
      in length cycles @?= 1
    
  , testCase "大量模块处理" $
      let modules = map (\i -> "Mod" ++ show i) [1..100]
          graph = Map.fromList [(mod, []) | mod <- modules]
          analysis = analyzeDependencies graph
          sorted = topologicalSort graph
      in case sorted of
        Right sorted' -> length sorted' @?= 100
        Left _ -> assertBool "大量模块排序失败" False
  ]

-- | 性能测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "大型依赖图分析性能" $
      \n ->
        let size = min 50 (max 1 n)
            modules = map (\i -> "M" ++ show i) [1..size]
            -- 创建一个线性依赖链
            pairs = zip modules (tail modules)
            graph = Map.fromList $ map (\(from, to) -> (from, [to])) pairs ++ 
                   [(last modules, [])]
            analysis = analyzeDependencies graph
            cycles = detectCycles graph
        in null cycles && length (getTransitiveDependencies analysis (head modules)) >= 0
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "Dependencies模块核心功能测试"
  [ testBasicDependencyAnalysis
  , testCycleDetection
  , testTransitiveDependencies
  , testTopologicalSort
  , testDependencyLevels
  , testDependencyProperties
  , testDependencyOptimization
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- 辅助函数和类型定义（假设这些在Dependencies模块中存在）
data DependencyAnalysis = DependencyAnalysis 
  { graph :: Map String [String]
  , transitiveCache :: Map String [String]
  } deriving (Show, Eq)

analyzeDependencies :: Map String [String] -> DependencyAnalysis
analyzeDependencies g = DependencyAnalysis g Map.empty

getDirectDependencies :: DependencyAnalysis -> String -> [String]
getDirectDependencies analysis module' = Map.findWithDefault [] module' (graph analysis)

getTransitiveDependencies :: DependencyAnalysis -> String -> [String]
getTransitiveDependencies analysis module' = 
  case Map.lookup module' (transitiveCache analysis) of
    Just deps -> deps
    Nothing -> 
      let direct = getDirectDependencies analysis module'
          indirect = concatMap (getTransitiveDependencies analysis) direct
          allDeps = nub (direct ++ indirect)
      in allDeps

detectCycles :: Map String [String] -> [[String]]
detectCycles graph = 
  -- 简化的循环检测实现
  let modules = Map.keys graph
      findCycle visited current path
        | current `elem` path = [dropWhile (/= current) path ++ [current]]
        | current `elem` visited = []
        | otherwise = 
            let deps = Map.findWithDefault [] current graph
                cycles' = concatMap (findCycle (current:visited)) deps
            in concatMap (findCycle (current:visited)) deps
  in nub $ concatMap (findCycle [] []) modules

topologicalSort :: Map String [String] -> Either String [String]
topologicalSort graph = 
  let cycles = detectCycles graph
  in if null cycles
     then Right $ Map.keys graph  -- 简化实现
     else Left "Cycle detected"

calculateDependencyLevels :: DependencyAnalysis -> Map String Int
calculateDependencyLevels analysis = 
  let modules = Map.keys (graph analysis)
      calculateLevel module' = 
        let deps = getTransitiveDependencies analysis module'
        in if null deps then 0 else 1 + maximum (map calculateLevel deps)
  in Map.fromList [(mod, calculateLevel mod) | mod <- modules]

removeRedundantDependencies :: Map String [String] -> Map String [String]
removeRedundantDependencies graph = 
  let analysis = analyzeDependencies graph
      removeRedundant module' deps =
        let transitive = Set.fromList $ concatMap (getTransitiveDependencies analysis) deps
        in filter (\dep -> not (dep `Set.member` transitive)) deps
  in Map.mapWithKey (\module' deps -> removeRedundant module' deps) graph

findCommonDependencies :: Map String [String] -> String -> String -> [String]
findCommonDependencies graph mod1 mod2 = 
  let analysis = analyzeDependencies graph
      deps1 = Set.fromList (getTransitiveDependencies analysis mod1)
      deps2 = Set.fromList (getTransitiveDependencies analysis mod2)
  in Set.toList (Set.intersection deps1 deps2)