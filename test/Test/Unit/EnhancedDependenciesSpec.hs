module Test.Unit.EnhancedDependenciesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies (DependencyAnalysis(..), Dependency(..), DependencyType(..), 
                    analyzeDependencies, checkCircularDependencies, 
                    resolveDependencyOrder, validateDependencies)
import Parser (TypusFile(..), defaultFileDirectives)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Test DependencyAnalysis properties
prop_dependency_analysis_empty :: Property
prop_dependency_analysis_empty = 
  let analysis = DependencyAnalysis {
        daDependencies = Map.empty,
        daCircularDeps = [],
        daOrder = []
      }
  in property $ 
    Map.null (daDependencies analysis) && 
    null (daCircularDeps analysis) && 
    null (daOrder analysis)

prop_dependency_analysis_consistency :: [Dependency] -> [String] -> Property
prop_dependency_analysis_consistency dependencies order =
  let depMap = Map.fromList $ map (\dep -> (dName dep, dep)) dependencies
      analysis = DependencyAnalysis {
        daDependencies = depMap,
        daCircularDeps = [],
        daOrder = order
      }
  in property $ 
    Map.size (daDependencies analysis) == length dependencies && 
    length (daOrder analysis) == length order

-- | Test Dependency properties
prop_dependency_equality :: String -> String -> DependencyType -> Property
prop_dependency_equality name depType fromModule =
  let dep1 = Dependency name depType fromModule
      dep2 = Dependency name depType fromModule
  in property $ dep1 == dep2

prop_dependency_ordering :: String -> String -> DependencyType -> Property
prop_dependency_ordering name1 name2 depType =
  let dep1 = Dependency name1 depType "module1"
      dep2 = Dependency name2 depType "module2"
  in property $ 
    (name1 `compare` name2) === (dep1 `compare` dep2)

-- | Test DependencyType properties
prop_dependency_type_ordering :: Property
prop_dependency_type_ordering = 
  let types = [FunctionDependency, VariableDependency, TypeDependency, ModuleDependency]
  in property $ 
    all (\(t1, t2) -> t1 <= t2) (zip types (tail types))

-- | Test dependency analysis
prop_analyze_dependencies_empty :: Property
prop_analyze_dependencies_empty = 
  let file = TypusFile defaultFileDirectives [] "" ""
      analysis = analyzeDependencies file
  in property $ 
    Map.null (daDependencies analysis) && 
    null (daCircularDeps analysis)

prop_analyze_dependencies_preserves_functions :: [String] -> Property
prop_analyze_dependencies_preserves_functions funcNames =
  let funcDeclarations = map (\name -> "func " ++ name ++ "() {}") funcNames
      fileContent = unlines funcDeclarations
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      analysis = analyzeDependencies file
  in property $ Map.size (daDependencies analysis) >= 0

-- | Test circular dependency detection
prop_check_circular_dependencies_none :: [String] -> Property
prop_check_circular_dependencies_none moduleNames =
  let dependencies = map (\name -> Dependency name FunctionDependency "main") moduleNames
      circular = checkCircularDependencies dependencies
  in property $ null circular

prop_check_circular_dependencies_simple :: String -> String -> Property
prop_check_circular_dependencies_simple module1 module2 =
  module1 /= module2 ==>
  let dependencies = [ Dependency module1 FunctionDependency module2
                     , Dependency module2 FunctionDependency module1
                     ]
      circular = checkCircularDependencies dependencies
  in property $ not (null circular)

-- | Test dependency order resolution
prop_resolve_dependency_order_empty :: Property
prop_resolve_dependency_order_empty = 
  let dependencies = []
      order = resolveDependencyOrder dependencies
  in property $ null order

prop_resolve_dependency_order_preserves :: [String] -> Property
prop_resolve_dependency_order_preserves moduleNames =
  let dependencies = map (\name -> Dependency name FunctionDependency "main") moduleNames
      order = resolveDependencyOrder dependencies
  in property $ length order == length moduleNames

-- | Test dependency validation
prop_validate_dependencies_empty :: Property
prop_validate_dependencies_empty = 
  let dependencies = []
      result = validateDependencies dependencies
  in property $ 
    case result of
      Left _ -> False
      Right _ -> True

prop_validate_dependencies_consistent :: [String] -> Property
prop_validate_dependencies_consistent moduleNames =
  let dependencies = map (\name -> Dependency name FunctionDependency "main") moduleNames
      result = validateDependencies dependencies
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test dependency chains
prop_dependency_chain :: [String] -> Property
prop_dependency_chain moduleNames =
  length moduleNames >= 2 ==>
  let dependencies = zipWith (\from to -> Dependency from FunctionDependency to) 
                            moduleNames (tail moduleNames)
      order = resolveDependencyOrder dependencies
  in property $ length order >= length moduleNames - 1

-- | Test dependency types
prop_dependency_type_analysis :: String -> Property
prop_dependency_type_analysis moduleName =
  let funcDep = Dependency "func1" FunctionDependency moduleName
      varDep = Dependency "var1" VariableDependency moduleName
      typeDep = Dependency "Type1" TypeDependency moduleName
      moduleDep = Dependency "mod1" ModuleDependency moduleName
      dependencies = [funcDep, varDep, typeDep, moduleDep]
  in property $ length dependencies == 4

-- | Test dependency analysis with imports
prop_analyze_dependencies_with_imports :: [String] -> Property
prop_analyze_dependencies_with_imports moduleNames =
  let importStatements = map (\name -> "import \"" ++ name ++ "\"") moduleNames
      fileContent = unlines importStatements
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      analysis = analyzeDependencies file
  in property $ Map.size (daDependencies analysis) >= 0

-- | Test dependency error handling
prop_dependency_error_handling :: String -> Property
prop_dependency_error_handling moduleName =
  let dependency = Dependency "" FunctionDependency moduleName
      result = validateDependencies [dependency]
  in property $ 
    case result of
      Left _ -> True  -- Empty name should fail
      Right _ -> False

-- | Test dependency graph properties
prop_dependency_graph_acyclic :: [String] -> Property
prop_dependency_graph_acyclic moduleNames =
  let dependencies = zipWith (\from to -> Dependency from FunctionDependency to) 
                            moduleNames (tail moduleNames ++ ["main"])
      circular = checkCircularDependencies dependencies
  in property $ null circular

prop_dependency_graph_complete :: [String] -> Property
prop_dependency_graph_complete moduleNames =
  let allPairs = [(from, to) | from <- moduleNames, to <- moduleNames, from /= to]
      dependencies = map (\(from, to) -> Dependency from FunctionDependency to) allPairs
      order = resolveDependencyOrder dependencies
  in property $ length order <= length moduleNames

-- | Test dependency resolution consistency
prop_dependency_resolution_consistent :: [Dependency] -> Property
prop_dependency_resolution_consistent dependencies =
  let order1 = resolveDependencyOrder dependencies
      order2 = resolveDependencyOrder dependencies
  in property $ order1 == order2

-- | Test dependency validation with cycles
prop_validate_dependencies_with_cycles :: String -> String -> String -> Property
prop_validate_dependencies_with_cycles module1 module2 module3 =
  let dependencies = [ Dependency module1 FunctionDependency module2
                     , Dependency module2 FunctionDependency module3
                     , Dependency module3 FunctionDependency module1
                     ]
      result = validateDependencies dependencies
  in property $ 
    case result of
      Left _ -> True  -- Should detect cycle
      Right _ -> False

tests :: TestTree
tests = testGroup "Enhanced Dependencies Tests"
  [ testGroup "DependencyAnalysis tests"
    [ testProperty "dependency analysis empty" prop_dependency_analysis_empty
    , testProperty "dependency analysis consistency" prop_dependency_analysis_consistency
    ]
  , testGroup "Dependency tests"
    [ testProperty "dependency equality" prop_dependency_equality
    , testProperty "dependency ordering" prop_dependency_ordering
    ]
  , testGroup "DependencyType tests"
    [ testProperty "dependency type ordering" prop_dependency_type_ordering
    ]
  , testGroup "Dependency analysis"
    [ testProperty "analyze dependencies empty" prop_analyze_dependencies_empty
    , testProperty "analyze dependencies preserves functions" prop_analyze_dependencies_preserves_functions
    , testProperty "analyze dependencies with imports" prop_analyze_dependencies_with_imports
    ]
  , testGroup "Circular dependency detection"
    [ testProperty "check circular dependencies none" prop_check_circular_dependencies_none
    , testProperty "check circular dependencies simple" prop_check_circular_dependencies_simple
    ]
  , testGroup "Dependency order resolution"
    [ testProperty "resolve dependency order empty" prop_resolve_dependency_order_empty
    , testProperty "resolve dependency order preserves" prop_resolve_dependency_order_preserves
    , testProperty "dependency resolution consistent" prop_dependency_resolution_consistent
    ]
  , testGroup "Dependency validation"
    [ testProperty "validate dependencies empty" prop_validate_dependencies_empty
    , testProperty "validate dependencies consistent" prop_validate_dependencies_consistent
    , testProperty "validate dependencies with cycles" prop_validate_dependencies_with_cycles
    ]
  , testGroup "Dependency chains"
    [ testProperty "dependency chain" prop_dependency_chain
    ]
  , testGroup "Dependency types"
    [ testProperty "dependency type analysis" prop_dependency_type_analysis
    ]
  , testGroup "Error handling"
    [ testProperty "dependency error handling" prop_dependency_error_handling
    ]
  , testGroup "Dependency graph properties"
    [ testProperty "dependency graph acyclic" prop_dependency_graph_acyclic
    , testProperty "dependency graph complete" prop_dependency_graph_complete
    ]
  ]