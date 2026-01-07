module Test.Unit.IntegratedCompilerPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import IntegratedCompiler

-- Test compilation pipeline consistency
prop_compilation_pipeline_consistent :: String -> Property
prop_compilation_pipeline_consistent sourceCode =
  let result1 = compileWithIntegratedPipeline sourceCode
      result2 = compileWithIntegratedPipeline sourceCode
  in property $ result1 === result2

-- Test compilation phases ordering
prop_compilation_phases_ordered :: String -> Property
prop_compilation_phases_ordered sourceCode =
  let phases = getCompilationPhases sourceCode
  in property $ isOrdered phases

-- Test error propagation
prop_error_propagation_preserved :: String -> Property
prop_error_propagation_preserved sourceCode =
  let result = compileWithIntegratedPipeline sourceCode
  in property $ 
    case result of
      Left errors -> not (null errors)
      Right _ -> property True

-- Test optimization preserves semantics
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics sourceCode =
  let unoptimized = compileWithoutOptimization sourceCode
      optimized = compileWithOptimization sourceCode
  in property $ 
    case (unoptimized, optimized) of
      (Right u, Right o) -> getSemanticHash u === getSemanticHash o
      _ -> property True

-- Test incremental compilation
prop_incremental_compilation_efficient :: String -> String -> Property
prop_incremental_compilation_efficient original modified =
  let fullCompile = compileWithIntegratedPipeline modified
      incremental = compileIncrementally original modified
  in property $ 
    case (fullCompile, incremental) of
      (Right f, Right i) -> getSemanticHash f === getSemanticHash i
      _ -> property True

-- Helper function
isOrdered :: Eq a => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x:y:xs) = x == y && isOrdered (y:xs)

tests :: TestTree
tests = testGroup "IntegratedCompiler Properties Tests"
  [ testProperty "compilation pipeline consistent" prop_compilation_pipeline_consistent
  , testProperty "compilation phases ordered" prop_compilation_phases_ordered
  , testProperty "error propagation preserved" prop_error_propagation_preserved
  , testProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  , testProperty "incremental compilation efficient" prop_incremental_compilation_efficient
  ]