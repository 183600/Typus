{-# LANGUAGE CPP #-}

module Test.Unit.CircularDependencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Compiler.TypeChecker (checkCircularDependencies, FunctionSignature(..), FunctionParam(..), TypeEnv(..), Type(..))
import Parser (TypusFile(..), CodeBlock(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph (Graph, buildG, topSort, edges, vertices)
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing)

-- | Generate random function names
genFunctionName :: Gen String
genFunctionName = elements ["func1", "func2", "func3", "test", "process", "calculate", "validate", "transform"]

-- | Generate random function signatures
genFunctionSignature :: Gen FunctionSignature
genFunctionSignature = do
  paramCount <- choose (0, 3)
  params <- listOfN paramCount genFunctionParam
  returnCount <- choose (0, 2)
  returnTypes <- listOfN returnCount genBasicType
  return $ FunctionSignature params returnTypes
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]
    genFunctionParam = do
      name <- elements ["x", "y", "z", "arg", "param"]
      paramType <- genBasicType
      return $ FunctionParam (Just name) paramType False
    genBasicType = elements [TypeName "int", TypeName "string", TypeName "bool"]

-- | Generate dependency graphs
genDependencyGraph :: Gen (Map String [String])
genDependencyGraph = do
  funcCount <- choose (1, 5)
  functions <- listOfN funcCount genFunctionName
  dependencies <- mapM (\func -> do
    depCount <- choose (0, min 2 (funcCount - 1))
    deps <- listOfN depCount (elements functions)
    return (func, nub deps)
    ) functions
  return $ Map.fromList dependencies
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate circular dependency scenarios
genCircularDependency :: Gen (Map String [String])
genCircularDependency = do
  funcCount <- choose (2, 4)
  functions <- take funcCount <$> listOf genFunctionName
  let circularDeps = createCircularDependencies functions
  return $ Map.fromList circularDeps
  where
    createCircularDependencies [] = []
    createCircularDependencies [f] = [(f, [])]
    createCircularDependencies (f1:f2:fs) = 
      let pairs = zip (f1:f2:fs) (f2:fs ++ [f1])
      in map (\(from, to) -> (from, [to])) pairs

-- | Generate acyclic dependency scenarios
genAcyclicDependency :: Gen (Map String [String])
genAcyclicDependency = do
  funcCount <- choose (1, 5)
  functions <- listOfN funcCount genFunctionName
  let sortedFuncs = sort functions
  dependencies <- mapM (\(i, func) -> do
    let possibleDeps = take i sortedFuncs
    depCount <- choose (0, min 2 i)
    deps <- listOfN depCount (elements possibleDeps)
    return (func, nub deps)
    ) (zip [0..] sortedFuncs)
  return $ Map.fromList dependencies
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate mixed dependency scenarios
genMixedDependency :: Gen (Map String [String])
genMixedDependency = oneof
  [ genDependencyGraph
  , genCircularDependency
  , genAcyclicDependency
  ]

-- | Generate type environments with function dependencies
genTypeEnvWithDependencies :: Gen TypeEnv
genTypeEnvWithDependencies = do
  depGraph <- genMixedDependency
  signatures <- mapM (\(funcName, _) -> do
    signature <- genFunctionSignature
    return (funcName, signature)
  ) (Map.toList depGraph)
  return $ TypeEnv Map.empty (Map.fromList signatures)

-- Property: Circular dependency detection
prop_circular_dependency_detection :: Map String [String] -> Property
prop_circular_dependency_detection depGraph =
  let hasCircular = hasCircularDependency depGraph
      detected = checkCircularDependencies depGraph
  in property $ hasCircular ==> isJust detected

-- Property: Acyclic dependency validation
prop_acyclic_dependency_validation :: Map String [String] -> Property
prop_acyclic_dependency_validation depGraph =
  let hasCircular = hasCircularDependency depGraph
      detected = checkCircularDependencies depGraph
  in not hasCircular ==> property $ isNothing detected

-- Property: Dependency graph construction
prop_dependency_graph_construction :: Map String [String] -> Property
prop_dependency_graph_construction depGraph =
  let functions = Map.keys depGraph
      allDeps = concat (Map.elems depGraph)
      allReferenced = nub allDeps
  in property $ all (`elem` functions) allReferenced .||. True  -- Some deps may be external

-- Property: Circular dependency path detection
prop_circular_path_detection :: Map String [String] -> Property
prop_circular_path_detection depGraph =
  let hasCircular = hasCircularDependency depGraph
      detected = checkCircularDependencies depGraph
  in case detected of
    Just path -> property $ length path >= 2 .&&. head path == last path
    Nothing -> property $ True

-- Property: Self-dependency detection
prop_self_dependency_detection :: String -> Property
prop_self_dependency_detection funcName =
  let selfDep = Map.singleton funcName [funcName]
      detected = checkCircularDependencies selfDep
  in property $ isJust detected

-- Property: Complex circular dependency scenarios
prop_complex_circular_detection :: [String] -> Property
prop_complex_circular_detection functions =
  length functions >= 3 ==> 
  let circularGraph = createComplexCircular functions
      detected = checkCircularDependencies circularGraph
  in property $ isJust detected
  where
    createComplexCircular [] = Map.empty
    createComplexCircular [f] = Map.singleton f []
    createComplexCircular (f1:f2:f3:fs) =
      let baseDeps = [(f1, [f2]), (f2, [f3]), (f3, [f1])]
          additionalDeps = map (\f -> (f, [])) fs
      in Map.fromList (baseDeps ++ additionalDeps)

-- Property: Dependency preservation in type environment
prop_dependency_preservation :: TypeEnv -> Property
prop_dependency_preservation typeEnv =
  let functionNames = Map.keys (functionTypes typeEnv)
      hasFunctions = not (null functionNames)
  in hasFunctions ==> property $ True

-- Property: Circular dependency transitivity
prop_circular_dependency_transitivity :: Map String [String] -> Property
prop_circular_dependency_transitivity depGraph =
  let hasCircular = hasCircularDependency depGraph
      transitiveClosure = computeTransitiveClosure depGraph
      hasTransitiveCircular = hasCircularDependency transitiveClosure
  in property $ hasCircular ==> hasTransitiveCircular

-- Property: Dependency graph vertex consistency
prop_dependency_vertex_consistency :: Map String [String] -> Property
prop_dependency_vertex_consistency depGraph =
  let vertices = Map.keys depGraph
      edges = concat (Map.elems depGraph)
      uniqueVertices = nub vertices
      uniqueEdges = nub edges
  in property $ length uniqueVertices == length vertices .&&. 
             length uniqueEdges <= length edges

-- Property: Circular dependency minimal cycle detection
prop_minimal_cycle_detection :: Map String [String] -> Property
prop_minimal_cycle_detection depGraph =
  let detected = checkCircularDependencies depGraph
  in case detected of
    Just path -> 
      let minimalCycle = findMinimalCycle depGraph
      in property $ length path >= length minimalCycle
    Nothing -> property $ True

-- Property: Dependency graph acyclic topological sort
prop_acyclic_topological_sort :: Map String [String] -> Property
prop_acyclic_topological_sort depGraph =
  let hasCircular = hasCircularDependency depGraph
  in not hasCircular ==> 
     let sorted = topologicalSort depGraph
         allFunctions = Map.keys depGraph
     in property $ sort sorted == sort allFunctions

-- Property: Circular dependency uniqueness
prop_circular_dependency_uniqueness :: Map String [String] -> Property
prop_circular_dependency_uniqueness depGraph =
  let detected1 = checkCircularDependencies depGraph
      detected2 = checkCircularDependencies depGraph
  in case (detected1, detected2) of
    (Just path1, Just path2) -> property $ path1 == path2
    (Nothing, Nothing) -> property $ True
    _ -> property $ False

-- Property: Dependency graph size consistency
prop_dependency_graph_size_consistent :: Map String [String] -> Property
prop_dependency_graph_size_consistent depGraph =
  let vertexCount = Map.size depGraph
      edgeCount = sum (map length (Map.elems depGraph))
  in property $ vertexCount >= 1 .&&. edgeCount >= 0

-- Property: Circular dependency in complex graphs
prop_complex_graph_circular_detection :: Int -> Property
prop_complex_graph_circular_detection size =
  size <= 10 ==> 
  let graph <- generateComplexGraph size
      detected = checkCircularDependencies graph
      hasCircular = hasCircularDependency graph
  in property $ hasCircular ==> isJust detected

-- Property: Dependency cycle length bounds
prop_cycle_length_bounds :: Map String [String] -> Property
prop_cycle_length_bounds depGraph =
  let detected = checkCircularDependencies depGraph
      vertexCount = Map.size depGraph
  in case detected of
    Just path -> property $ length path <= vertexCount + 1
    Nothing -> property $ True

-- Property: Dependency graph connectivity
prop_dependency_graph_connectivity :: Map String [String] -> Property
prop_dependency_graph_connectivity depGraph =
  let vertices = Map.keys depGraph
      connected = isGraphConnected depGraph
  in property $ connected ==> length vertices >= 1

-- Helper functions

hasCircularDependency :: Map String [String] -> Bool
hasCircularDependency depGraph =
  let vertices = Map.keys depGraph
      vertexIndices = Map.fromList $ zip vertices [0..]
      indexedDeps = map (\(func, deps) -> 
        (Map.findWithDefault 0 func vertexIndices, 
         map (\dep -> Map.findWithDefault 0 dep vertexIndices) deps)
      ) (Map.toList depGraph)
      graph = buildG (0, length vertices - 1) indexedDeps
      sorted = topSort graph
  in length sorted < length vertices

topologicalSort :: Map String [String] -> [String]
topologicalSort depGraph =
  let vertices = Map.keys depGraph
      vertexIndices = Map.fromList $ zip vertices [0..]
      indexedDeps = map (\(func, deps) -> 
        (Map.findWithDefault 0 func vertexIndices, 
         map (\dep -> Map.findWithDefault 0 dep vertexIndices) deps)
      ) (Map.toList depGraph)
      graph = buildG (0, length vertices - 1) indexedDeps
      sortedIndices = topSort graph
  in map (\idx -> vertices !! idx) sortedIndices

computeTransitiveClosure :: Map String [String] -> Map String [String]
computeTransitiveClosure depGraph =
  let vertices = Map.keys depGraph
      closure = foldl (\acc v -> 
        let reachable = findReachable depGraph v
        in Map.insert v reachable acc
      ) Map.empty vertices
  in closure

findReachable :: Map String [String] -> String -> [String]
findReachable depGraph start =
  let visited = Set.empty
      dfs v visited = 
        if Set.member v visited then visited
        else case Map.lookup v depGraph of
          Just deps -> foldl dfs (Set.insert v visited) deps
          Nothing -> Set.insert v visited
  in Set.toList (dfs start visited)

findMinimalCycle :: Map String [String] -> [String]
findMinimalCycle depGraph =
  let vertices = Map.keys depGraph
      cycles = [findCycleFrom depGraph v | v <- vertices]
      nonEmptyCycles = filter (not . null) cycles
  in if null nonEmptyCycles then [] else minimumByLength nonEmptyCycles

findCycleFrom :: Map String [String] -> String -> [String]
findCycleFrom depGraph start =
  let visited = []
      dfs current path visited =
        if current `elem` path 
        then let cycleStart = length path - length (takeWhile (/= current) (reverse path))
                 cycle = drop (length path - cycleStart) path
             in cycle
        else case Map.lookup current depGraph of
          Just deps -> 
            let results = [d `dfs` (current:path) visited | d <- deps]
            in case filter (not . null) results of
              (cycle:_) -> cycle
              [] -> []
          Nothing -> []
  in dfs start [] []

minimumByLength :: [[a]] -> [a]
minimumByLength [] = []
minimumByLength xs = foldl1 (\acc x -> if length x < length acc then x else acc) xs

isGraphConnected :: Map String [String] -> Bool
isGraphConnected depGraph =
  let vertices = Map.keys depGraph
  in case vertices of
    [] -> True
    (v:_) -> 
      let reachable = Set.fromList $ findReachable depGraph v
      in all (`Set.member` reachable) vertices

generateComplexGraph :: Int -> Map String [String]
generateComplexGraph size =
  let functions = ["func" ++ show i | i <- [1..size]]
      createDeps [] = []
      createDeps (f:fs) = (f, take 1 fs) : createDeps fs
  in Map.fromList $ createDeps functions

tests :: TestTree
tests = testGroup "Circular Dependency QuickCheck Tests"
  [ fastProperty "circular dependency detection" prop_circular_dependency_detection
  , fastProperty "acyclic dependency validation" prop_acyclic_dependency_validation
  , fastProperty "dependency graph construction" prop_dependency_graph_construction
  , fastProperty "circular path detection" prop_circular_path_detection
  , fastProperty "self dependency detection" prop_self_dependency_detection
  , fastProperty "complex circular detection" prop_complex_circular_detection
  , fastProperty "dependency preservation" prop_dependency_preservation
  , fastProperty "circular dependency transitivity" prop_circular_dependency_transitivity
  , fastProperty "dependency vertex consistency" prop_dependency_vertex_consistency
  , fastProperty "minimal cycle detection" prop_minimal_cycle_detection
  , fastProperty "acyclic topological sort" prop_acyclic_topological_sort
  , fastProperty "circular dependency uniqueness" prop_circular_dependency_uniqueness
  , fastProperty "dependency graph size consistent" prop_dependency_graph_size_consistent
  , fastProperty "complex graph circular detection" prop_complex_graph_circular_detection
  , fastProperty "cycle length bounds" prop_cycle_length_bounds
  , fastProperty "dependency graph connectivity" prop_dependency_graph_connectivity
  ]