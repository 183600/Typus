module Test.Unit.ValueAnalysisFlowSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import ValueAnalysis

-- Test value flow analysis
prop_value_flow_preservation :: String -> Property
prop_value_flow_preservation valueId =
  let flow = createValueFlow valueId
      preserved = preserveValueFlow flow
  in property $ getValueFlowId preserved === valueId

-- Test value dependency tracking
prop_value_dependency_transitive :: String -> String -> String -> Property
prop_value_dependency_transitive val1 val2 val3 =
  let dep1 = addDependency val1 val2
      dep2 = addDependency val2 val3
      transitive = getTransitiveDependencies val1 [dep1, dep2]
  in property $ val3 `elem` transitive

-- Test value type inference
prop_value_type_inference_consistent :: String -> Property
prop_value_type_inference_consistent valueName =
  let type1 = inferValueType valueName
      type2 = inferValueType valueName
  in property $ type1 === type2

-- Test value scope resolution
prop_value_scope_resolution_nested :: String -> Property
prop_value_scope_resolution_nested valueName =
  let outerScope = createScope "outer"
      innerScope = createNestedScope outerScope "inner"
      resolved1 = resolveValue valueName outerScope
      resolved2 = resolveValue valueName innerScope
  in property $ 
    case (resolved1, resolved2) of
      (Nothing, Just _) -> property True
      (Just v1, Just v2) -> getValueName v1 === getValueName v2
      _ -> property True

-- Test value lifecycle tracking
prop_value_lifecycle_tracking :: String -> Property
prop_value_lifecycle_tracking valueId =
  let lifecycle = trackValueLifecycle valueId
      created = getValueCreationTime lifecycle
      accessed = getValueLastAccessTime lifecycle
  in property $ created <= accessed

tests :: TestTree
tests = testGroup "ValueAnalysis Flow Tests"
  [ testProperty "value flow preservation" prop_value_flow_preservation
  , testProperty "value dependency transitive" prop_value_dependency_transitive
  , testProperty "value type inference consistent" prop_value_type_inference_consistent
  , testProperty "value scope resolution nested" prop_value_scope_resolution_nested
  , testProperty "value lifecycle tracking" prop_value_lifecycle_tracking
  ]