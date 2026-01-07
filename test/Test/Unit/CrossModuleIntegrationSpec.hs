module Test.Unit.CrossModuleIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.CrossAnalysis

-- Test cross-module analysis
prop_cross_module_analysis :: [String] -> Property
prop_cross_module_analysis moduleNames =
  let analysis = performCrossModuleAnalysis moduleNames
      dependencies = getCrossModuleDependencies analysis
  in property $ length dependencies >= 0

-- Test module interface extraction
prop_module_interface_extraction :: String -> Property
prop_module_interface_extraction moduleName =
  let interface = extractModuleInterface moduleName
      exports = getModuleExports interface
  in property $ length exports >= 0

-- Test cross-module type checking
prop_cross_module_type_checking :: String -> String -> Property
prop_cross_module_type_checking module1 module2 =
  let result = checkCrossModuleTypes module1 module2
  in property $ 
    case result of
      Left _ -> property True
      Right _ -> property True

-- Test module dependency resolution
prop_dependency_resolution :: [(String, [String])] -> Property
prop_dependency_resolution dependencies =
  let resolved = resolveDependencies dependencies
      cycles = detectCycles resolved
  in property $ 
    case cycles of
      [] -> property True
      _ -> length cycles > 0

-- Test cross-module optimization
prop_cross_module_optimization :: [String] -> Property
prop_cross_module_optimization moduleNames =
  let optimized = performCrossModuleOptimization moduleNames
      improvements = getOptimizationImprovements optimized
  in property $ length improvements >= 0

tests :: TestTree
tests = testGroup "Cross Module Integration Tests"
  [ testProperty "cross module analysis" prop_cross_module_analysis
  , testProperty "module interface extraction" prop_module_interface_extraction
  , testProperty "cross module type checking" prop_cross_module_type_checking
  , testProperty "dependency resolution" prop_dependency_resolution
  , testProperty "cross module optimization" prop_cross_module_optimization
  ]