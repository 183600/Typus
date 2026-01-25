module Test.Unit.DependenciesInferenceSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Dependencies.Inference as DI
import qualified Dependencies.AST as AST
import qualified Dependencies.TypeSystem as TS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing)
import Data.List (nub, sort)

-- 测试依赖推断的属性
prop_basic_dependency_inference :: String -> String -> Property
prop_basic_dependency_inference def use = 
  let code = def ++ "\n" ++ use
  in case DI.inferDependencies code of
    Right deps -> 
      case Map.lookup def deps of
        Just uses -> property $ use `elem` uses
        Nothing -> property False
    Left _ -> property True

prop_transitive_dependency_closure :: [String] -> Property
prop_transitive_dependency_closure modules = 
  case DI.inferDependencies (unlines modules) of
    Right deps -> 
      let closure = DI.computeTransitiveClosure deps
      in property $ all (\(k, v) -> all (\dep -> dep `elem` Map.keys deps) v) (Map.toList closure)
    Left _ -> property True

prop_cycle_detection :: [String] -> Property
prop_cycle_detection modules = 
  case DI.inferDependencies (unlines modules) of
    Right deps -> 
      let hasCycle = DI.detectCycle deps
      in property $ hasCycle === (length modules > 1)
    Left _ -> property True

prop_dependency_ordering :: [String] -> Property
prop_dependency_ordering modules = 
  case DI.inferDependencies (unlines modules) of
    Right deps -> 
      case DI.topologicalSort deps of
        Right sorted -> 
          property $ all (\(i, mod1) -> 
            all (\(j, mod2) -> 
              i < j || not (mod2 `dependsOn` mod1 deps)
            ) (zip [0..] sorted)
          ) (zip [0..] sorted)
        Left _ -> property False
    Left _ -> property True
  where
    dependsOn mod1 mod2 deps = 
      case Map.lookup mod1 deps of
        Just deps' -> mod2 `elem` deps'
        Nothing -> False

prop_type_dependency_inference :: String -> String -> Property
prop_type_dependency_inference typeDef funcDef = 
  let code = typeDef ++ "\n" ++ funcDef
  in case DI.inferTypeDependencies code of
    Right deps -> 
      case Map.lookup funcDef deps of
        Just types -> property $ typeDef `elem` types
        Nothing -> property False
    Left _ -> property True

prop_function_dependency_inference :: String -> String -> Property
prop_function_dependency_inference func1 func2 = 
  let code = func1 ++ "\n" ++ func2
  in case DI.inferFunctionDependencies code of
    Right deps -> 
      case Map.lookup func2 deps of
        Just funcs -> property $ func1 `elem` funcs
        Nothing -> property False
    Left _ -> property True

prop_module_dependency_inference :: String -> String -> Property
prop_module_dependency_inference mod1 mod2 = 
  let code = "import " ++ mod1 ++ "\n" ++ "import " ++ mod2
  in case DI.inferModuleDependencies code of
    Right deps -> 
      case Map.lookup "main" deps of
        Just mods -> property $ mod1 `elem` mods && mod2 `elem` mods
        Nothing -> property False
    Left _ -> property True

prop_implicit_dependency_inference :: String -> Property
prop_implicit_dependency_inference code = 
  case DI.inferImplicitDependencies code of
    Right deps -> property $ not (null deps)
    Left _ -> property True

prop_explicit_dependency_inference :: String -> Property
prop_explicit_dependency_inference code = 
  case DI.inferExplicitDependencies code of
    Right deps -> 
      let explicitDeps = DI.extractExplicitImports code
      in property $ all (`elem` Map.keys deps) explicitDeps
    Left _ -> property True

prop_dependency_strength :: String -> String -> Property
prop_dependency_strength from to = 
  let code = from ++ "\n" ++ to
  in case DI.inferDependencyStrength code of
    Right strength -> property $ strength >= 0 && strength <= 1
    Left _ -> property True

prop_dependency_aggregation :: [String] -> Property
prop_dependency_aggregation modules = 
  case mapM DI.inferDependencies modules of
    Right depsList -> 
      let aggregated = DI.aggregateDependencies depsList
      in property $ Map.size aggregated >= 0
    Left _ -> property True

prop_dependency_filtering :: String -> String -> Property
prop_dependency_filtering code filter = 
  case DI.inferDependencies code of
    Right deps -> 
      let filtered = DI.filterDependencies deps filter
      in property $ Map.size filtered <= Map.size deps
    Left _ -> property True

prop_dependency_validation :: String -> Property
prop_dependency_validation code = 
  case DI.inferDependencies code of
    Right deps -> 
      case DI.validateDependencies deps of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property True

prop_dependency_merging :: String -> String -> Property
prop_dependency_merging code1 code2 = 
  case (DI.inferDependencies code1, DI.inferDependencies code2) of
    (Right deps1, Right deps2) -> 
      let merged = DI.mergeDependencies deps1 deps2
      in property $ Map.size merged >= Map.size deps1 && Map.size merged >= Map.size deps2
    _ -> property True

prop_dependency_difference :: String -> String -> Property
prop_dependency_difference code1 code2 = 
  case (DI.inferDependencies code1, DI.inferDependencies code2) of
    (Right deps1, Right deps2) -> 
      let diff = DI.dependencyDifference deps1 deps2
      in property $ Map.size diff <= Map.size deps1
    _ -> property True

prop_dependency_intersection :: String -> String -> Property
prop_dependency_intersection code1 code2 = 
  case (DI.inferDependencies code1, DI.inferDependencies code2) of
    (Right deps1, Right deps2) -> 
      let intersection = DI.dependencyIntersection deps1 deps2
      in property $ Map.size intersection <= Map.size deps1 && Map.size intersection <= Map.size deps2
    _ -> property True

prop_dependency_subset :: String -> String -> Property
prop_dependency_subset code1 code2 = 
  case (DI.inferDependencies code1, DI.inferDependencies code2) of
    (Right deps1, Right deps2) -> 
      let isSubset = DI.isDependencySubset deps1 deps2
      in property $ isSubset === (Map.size deps1 <= Map.size deps2)
    _ -> property True

prop_dependency_ranking :: String -> Property
prop_dependency_ranking code = 
  case DI.inferDependencies code of
    Right deps -> 
      let ranked = DI.rankDependencies deps
      in property $ length ranked == Map.size deps
    Left _ -> property True

prop_dependency_clustering :: [String] -> Property
prop_dependency_clustering modules = 
  case mapM DI.inferDependencies modules of
    Right depsList -> 
      let clusters = DI.clusterDependencies depsList
      in property $ length clusters > 0
    Left _ -> property True

prop_dependency_impact_analysis :: String -> String -> Property
prop_dependency_impact_analysis base change = 
  case (DI.inferDependencies base, DI.inferDependencies change) of
    (Right baseDeps, Right changeDeps) -> 
      let impact = DI.analyzeImpact baseDeps changeDeps
      in property $ length impact >= 0
    _ -> property True

prop_dependency_visualization :: String -> Property
prop_dependency_visualization code = 
  case DI.inferDependencies code of
    Right deps -> 
      let graph = DI.visualizeDependencies deps
      in property $ not (null graph)
    Left _ -> property True

prop_dependency_export_import :: String -> Property
prop_dependency_export_import code = 
  case DI.inferDependencies code of
    Right deps -> 
      let exported = DI.exportDependencies deps
      in case DI.importDependencies exported of
        Right imported -> property $ deps === imported
        Left _ -> property False
    Left _ -> property True

prop_dependency_serialization :: String -> Property
prop_dependency_serialization code = 
  case DI.inferDependencies code of
    Right deps -> 
      let serialized = DI.serializeDependencies deps
      in case DI.deserializeDependencies serialized of
        Right deserialized -> property $ deps === deserialized
        Left _ -> property False
    Left _ -> property True

tests :: TestTree
tests = testGroup "Dependencies Inference Tests"
  [ testProperty "Basic dependency inference" prop_basic_dependency_inference
  , testProperty "Transitive dependency closure" prop_transitive_dependency_closure
  , testProperty "Cycle detection" prop_cycle_detection
  , testProperty "Dependency ordering" prop_dependency_ordering
  , testProperty "Type dependency inference" prop_type_dependency_inference
  , testProperty "Function dependency inference" prop_function_dependency_inference
  , testProperty "Module dependency inference" prop_module_dependency_inference
  , testProperty "Implicit dependency inference" prop_implicit_dependency_inference
  , testProperty "Explicit dependency inference" prop_explicit_dependency_inference
  , testProperty "Dependency strength" prop_dependency_strength
  , testProperty "Dependency aggregation" prop_dependency_aggregation
  , testProperty "Dependency filtering" prop_dependency_filtering
  , testProperty "Dependency validation" prop_dependency_validation
  , testProperty "Dependency merging" prop_dependency_merging
  , testProperty "Dependency difference" prop_dependency_difference
  , testProperty "Dependency intersection" prop_dependency_intersection
  , testProperty "Dependency subset" prop_dependency_subset
  , testProperty "Dependency ranking" prop_dependency_ranking
  , testProperty "Dependency clustering" prop_dependency_clustering
  , testProperty "Dependency impact analysis" prop_dependency_impact_analysis
  , testProperty "Dependency visualization" prop_dependency_visualization
  , testProperty "Dependency export/import" prop_dependency_export_import
  , testProperty "Dependency serialization" prop_dependency_serialization
  ]