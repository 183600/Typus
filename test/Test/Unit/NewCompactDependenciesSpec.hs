module Test.Unit.NewCompactDependenciesSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Dependencies
import Data.Set 
              len <- choose (1, 8)
  first <- elements ['A'..'Z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | 
genDependencyGraph :: Gen (Map String [String])
                              genDependencyGraph = do
              moduleCount <- choose (1, 6)
  modules <- sequence [genModuleName | _ <- [1..moduleCount]]
  dependencies <- mapM (\_ -> do
              depCount <- choose (0, 3)
    deps <- sequence [elements modules | _ <- [1..depCount]]
    return (nub deps) modules
  return $ Map.fromList (zip modules dependencies)

-- | 
testBasicDependencyAnalysis :: TestTree
testBasicDependencyAnalysis = testGroup ""
  [             testCase "" $
      let graph = Map.empty
                                        analysis = analyzeDependencies graph
      in L.null (getDirectDependencies analysis) @?= True
    
    ,             testCase "" $
      let graph = Map.fromList [("Main", [])]
                                        analysis = analyzeDependencies graph
                                        directDeps = getDirectDependencies analysis "Main"
      in directDeps @?= []
    
    ,             testCase "" $
      let graph = Map.fromList [("Main", ["Utils", "Config"])]
                                        analysis = analyzeDependencies graph
                                        directDeps = getDirectDependencies analysis "Main"
      in sort directDeps @?= ["Config", "Utils"]
  ]

-- | 
testCycleDetection :: TestTree
testCycleDetection = testGroup ""
  [             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", [])
            ]
                                        cycles = detectCycles graph
      in null cycles @?= True
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            ]
                                        cycles = detectCycles graph
      in L.length cycles @?= 1
    
    ,             testCase "" $
      let graph = Map.fromList [("A", ["A"])]
                                        cycles = detectCycles graph
      in L.length cycles @?= 1
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", ["D"])
            , ("D", ["A"])
            ]
                                        cycles = detectCycles graph
      in L.length cycles @?= 1
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            , ("C", ["D"])
            , ("D", ["C"])
            ]
                                        cycles = detectCycles graph
      in L.length cycles @?= 2
  ]

-- | 
testTransitiveDependencies :: TestTree
testTransitiveDependencies = testGroup ""
  [             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["C"])
            , ("C", [])
            ]
                                        analysis = analyzeDependencies graph
                                        transitiveDeps = getTransitiveDependencies analysis "A"
      in sort transitiveDeps @?= ["B", "C"]
    
    ,             testCase "" $
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
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", [])
            , ("C", [])
            ]
                                        analysis = analyzeDependencies graph
                                        transitiveDeps = getTransitiveDependencies analysis "A"
      in transitiveDeps @?= ["B"]
  ]

-- | 
testTopologicalSort :: TestTree
testTopologicalSort = testGroup ""
  [             testCase "" $
      let graph = Map.fromList 
            [ ("C", [])
            , ("B", ["C"])
            , ("A", ["B", "C"])
            ]
                                        result = topologicalSort graph
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right sorted -> 
          let positions = Map.fromList (zip sorted [0..])
                                            posC = Map.lookup "C" positions
                                            posB = Map.lookup "B" positions
                                            posA = Map.lookup "A" positions
          in case (posC, posB, posA) of
            (Just c, Just b, Just a) -> assertBool "" (c < b && b < a && c < a)
            _ -> assertBool "" False
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B"])
            , ("B", ["A"])
            ]
                                        result = topologicalSort graph
      in case result of
        Left _ -> assertBool "" True
        Right _ -> assertBool "" False
  ]

-- | 
testDependencyLevels :: TestTree
testDependencyLevels = testGroup ""
  [             testCase "" $
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
        Nothing -> assertBool "" False
    
    ,             testCase "0" $
      let graph = Map.fromList 
            [ ("A", [])
            , ("B", [])
            ]
                                        analysis = analyzeDependencies graph
                                        levels = calculateDependencyLevels analysis
                                        levelA = Map.lookup "A" levels
                                        levelB = Map.lookup "B" levels
      in case (levelA, levelB) of
        (Just 0, Just 0) -> assertBool "" True
        _ -> assertBool "" False
  ]

-- | QuickCheck
testDependencyProperties :: TestTree
testDependencyProperties = testGroup ""
  [             testProperty "" $
      \graph module' ->
        let analysis = analyzeDependencies graph
                                          directDeps = Set.fromList (getDirectDependencies analysis module')
                                          transitiveDeps = Set.fromList (getTransitiveDependencies analysis module')
        in directDeps `Set.isSubsetOf` transitiveDeps
  
  ,             testProperty "" $
      \graph module' ->
        let analysis = analyzeDependencies graph
                                          transitiveDeps = getTransitiveDependencies analysis module'
                                          allTransitive = concatMap (getTransitiveDependencies analysis) transitiveDeps
        in L.all (`elem` allTransitive) transitiveDeps
  
  ,             testProperty "" $
      \graph ->
        let cycles = detectCycles graph
                                          hasDirectCycle = L.any (\(module', deps) -> module' `elem` deps) (Map.toList graph)
        in                               hasDirectCycle ==> not (null cycles)
  
  ,             testProperty "" $
      \graph ->
        let cycles = detectCycles graph
        in null                               cycles ==> 
          case topologicalSort graph of
            Left _ -> False
            Right sorted ->
              let positions = Map.fromList (zip sorted [0..])
                  checkDep (from, tos) = L.all (\to -> 
                    case (Map.lookup from positions, Map.lookup to positions) of
                      (Just fromPos, Just toPos) -> fromPos < toPos
                      _ -> False) tos
              in L.all checkDep (Map.toList graph)
  ]

-- | 
testDependencyOptimization :: TestTree
testDependencyOptimization = testGroup ""
  [             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["B", "C"])
            , ("B", ["C"])
            , ("C", [])
            ]
                                        optimized = removeRedundantDependencies graph
                                        aDeps = Map.lookup "A" optimized
      in case aDeps of
        Just deps -> sort deps @?= ["C"]
        Nothing -> assertBool "" False
    
    ,             testCase "" $
      let graph = Map.fromList 
            [ ("A", ["C"])
            , ("B", ["C"])
            , ("C", [])
            ]
                                        common = findCommonDependencies graph "A" "B"
      in common @?= ["C"]
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let graph = Map.empty
                                        analysis = analyzeDependencies graph
                                        cycles = detectCycles graph
                                        sorted = topologicalSort graph
      in case sorted of
        Right [] -> assertBool "" (null cycles)
        _ -> assertBool "" False
    
    ,             testCase "" $
      let graph = Map.fromList [("A", ["A"])]
                                        cycles = detectCycles graph
      in L.length cycles @?= 1
    
    ,             testCase "" $
      let modules = L.map (\i -> "Mod" ++ show i) [1..100]
                                        graph = Map.fromList [(mod, []) | mod <- modules]
                                        analysis = analyzeDependencies graph
                                        sorted = topologicalSort graph
      in case sorted of
        Right sorted' -> L.length sorted' @?= 100
        Left _ -> assertBool "" False
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "" $
      \n ->
        let size = min 50 (max 1 n)
                                          modules = L.map (\i -> "M" ++ show i) [1..size]
            -- 
                                          pairs = zip modules (L.tail modules)
                                          graph = Map.fromList $ L.map (\(from, to) -> (from, [to]) pairs ++ 
                   [(last modules, [])]
                                          analysis = analyzeDependencies graph
                                          cycles = detectCycles graph
        in null cycles && L.length (getTransitiveDependencies analysis (L.head modules) >= 0
  ]

-- | 
tests :: TestTree
tests =   testGroup "Dependencies"
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

-- Dependencies
data                               DependencyAnalysis = DependencyAnalysis 
  { graph :: Map String [String]
  , transitiveCache :: Map String [String]
  } deriving (Show, Eq)

analyzeDependencies :: Map String [String] -> DependencyAnalysis
analyzeDependencies                               g = DependencyAnalysis g Map.empty
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
detectCycles                               graph = 
  -- 
  let modules = Map.keys graph
findCycle visited current path
        | current `elem`                               path = [dropWhile (/= current) path ++ [current]]
        | current `elem`                               visited = []
        |                               otherwise = 
            let deps = Map.findWithDefault [] current graph
                cycles' = concatMap (findCycle (current:visited) deps
            in concatMap (findCycle (current:visited) deps
  in nub $ concatMap (findCycle [] []) modules

topologicalSort :: Map String [String] -> Either String [String]
topologicalSort                               graph = 
  let cycles = detectCycles graph
  in if null cycles
     then Right $ Map.keys graph  -- 
     else Left "Cycle detected"

calculateDependencyLevels :: DependencyAnalysis -> Map String Int
calculateDependencyLevels                               analysis = 
let modules = Map.keys (graph analysis)
      calculateLevel module' = 
        let deps = getTransitiveDependencies analysis module'
        in if null deps then 0 else 1 + L.maximum (map calculateLevel deps)
  in Map.fromList [(mod, calculateLevel mod) | mod <- modules]

removeRedundantDependencies :: Map String [String] -> Map String [String]
removeRedundantDependencies                               graph = 
  let analysis = analyzeDependencies graph
      removeRedundant module'                               deps =
let transitive = Set.fromList $ concatMap (getTransitiveDependencies analysis) deps
        in L.filter (\dep -> not (dep `Set.member` transitive) deps
  in Map.mapWithKey (\module' deps -> removeRedundant module' deps) graph

findCommonDependencies :: Map String [String] -> String -> String -> [String]
findCommonDependencies graph mod1                               mod2 = 
  let analysis = analyzeDependencies graph
                              deps1 = Set.fromList (getTransitiveDependencies analysis mod1)
                                    deps2 = Set.fromList (getTransitiveDependencies analysis mod2)
  in Set.toList (Set.intersection deps1 deps2)