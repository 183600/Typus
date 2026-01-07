module Test.Unit.DependentTypeBridgeSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.DependentTypeBridge

-- Test dependent type bridging
prop_dependent_type_bridging :: String -> Property
prop_dependent_type_bridging typeName =
  let bridge = createDependentTypeBridge typeName
      bridgedType = getBridgedType bridge
  in property $ getTypeName bridgedType === typeName

-- Test type constraint propagation
prop_constraint_propagation :: [String] -> Property
prop_constraint_propagation constraints =
  let bridge = createDependentTypeBridge "test"
      bridgeWithConstraints = addConstraints bridge constraints
      propagated = getPropagatedConstraints bridgeWithConstraints
  in property $ propagated === constraints

-- Test type dependency tracking
prop_type_dependency_tracking :: String -> [String] -> Property
prop_type_dependency_tracking baseType dependencies =
  let bridge = createDependentTypeBridge baseType
      bridgeWithDeps = addTypeDependencies bridge dependencies
      trackedDeps = getTypeDependencies bridgeWithDeps
  in property $ trackedDeps === dependencies

-- Test type refinement
prop_type_refinement :: String -> String -> Property
prop_type_refinement originalType refinedType =
  let bridge = createDependentTypeBridge originalType
      refinedBridge = refineType bridge refinedType
      currentType = getCurrentType refinedBridge
  in property $ currentType === refinedType

-- Test type equivalence checking
prop_type_equivalence :: String -> String -> Property
prop_type_equivalence type1 type2 =
  let bridge1 = createDependentTypeBridge type1
      bridge2 = createDependentTypeBridge type2
      equivalent = checkTypeEquivalence bridge1 bridge2
  in property $ 
    if type1 == type2 
    then equivalent
    else not equivalent

tests :: TestTree
tests = testGroup "Dependent Type Bridge Tests"
  [ testProperty "dependent type bridging" prop_dependent_type_bridging
  , testProperty "constraint propagation" prop_constraint_propagation
  , testProperty "type dependency tracking" prop_type_dependency_tracking
  , testProperty "type refinement" prop_type_refinement
  , testProperty "type equivalence" prop_type_equivalence
  ]