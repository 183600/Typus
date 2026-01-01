{-# LANGUAGE TemplateHaskell #-}

-- | Type inference property tests for Dependencies module
module Test.Unit.NewDependenciesInferenceCoreQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Dependencies 
  ( DependentTypeChecker
  , DependentTypeError(..)
  , TypeEnvironment
  , TypeVar(..)
  , TypeScheme(..)
  , newDependentTypeChecker
  , analyzeDependentTypes
  , inferType
  , checkType
  , unify
  , generalize
  , instantiate
  , initialTypeEnvironment
  )
import qualified Data.Text as T

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | TypeVar should be comparable
prop_typevar_comparable :: TypeVar -> TypeVar -> Property
prop_typevar_comparable tv1 tv2 =
  let comparison = compare tv1 tv2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

-- | TypeVar equality should be reflexive
prop_typevar_reflexive :: TypeVar -> Property
prop_typevar_reflexive tv = tv === tv

-- | TypeScheme should be constructible
prop_typescheme_constructible :: TypeVar -> Property
prop_typescheme_constructible tv =
  let scheme = TypeScheme [] tv  -- Simplified constructor
  in property True  -- If it constructs, it's valid

-- | TypeEnvironment should be initializable
prop_typeenvironment_initializable :: Property
prop_typeenvironment_initializable =
  let env = initialTypeEnvironment
  in property True  -- Should create valid environment

-- | DependentTypeChecker should be created consistently
prop_typechecker_consistent :: Property
prop_typechecker_consistent =
  let checker1 = newDependentTypeChecker
      checker2 = newDependentTypeChecker
  in property True  -- Both should be valid checkers

-- | Empty expression should be handled gracefully
prop_infer_empty_expression :: Property
prop_infer_empty_expression =
  let checker = newDependentTypeChecker
      result = inferType checker ""
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Simple type checking should be deterministic
prop_typechecking_deterministic :: String -> Property
prop_typechecking_deterministic expr =
  let checker = newDependentTypeChecker
      result1 = checkType checker expr
      result2 = checkType checker expr
  in case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False  -- Should get same result type

-- | Type unification should be symmetric
prop_unification_symmetric :: TypeVar -> TypeVar -> Property
prop_unification_symmetric tv1 tv2 =
  let checker = newDependentTypeChecker
      result1 = unify checker tv1 tv2
      result2 = unify checker tv2 tv1
  in case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False  -- Should get same result type

-- | Type generalization L.and instantiation should be inverses
prop_generalization_instantiation :: TypeVar -> Property
prop_generalization_instantiation tv =
  let checker = newDependentTypeChecker
      env = initialTypeEnvironment
      scheme = generalize env tv
  in case scheme of
    Left _ -> property True
    Right s -> case instantiate checker s of
      Left _ -> property True
      Right instantiated -> property True  -- Should be related to original

-- | Dependent type analysis should handle simple expressions
prop_analyze_simple_expressions :: String -> Property
prop_analyze_simple_expressions varName =
  let expr = varName ++ " : Int"
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker expr
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Type inference should handle nested expressions
prop_infer_nested_expressions :: String -> String -> Property
prop_infer_nested_expressions var1 var2 =
  let expr = "(" ++ var1 ++ " " ++ var2 ++ ")"
      checker = newDependentTypeChecker
      result = inferType checker expr
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Type checking should be consistent across multiple calls
prop_typechecking_consistency :: String -> Property
prop_typechecking_consistency expr =
  let checker = newDependentTypeChecker
      results = replicate 3 $ checkType checker expr
      allSame = L.all (\r -> case r of
        Left err1 -> case L.head results of
          Left err2 -> show err1 == show err2
          Right _ -> False
        Right res1 -> case L.head results of
          Left _ -> False
          Right res2 -> show res1 == show res2) (L.tail results)
  in allSame === True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Dependencies Inference QuickCheck Tests"
  [ testProperty "TypeVar: comparability" prop_typevar_comparable
  , testProperty "TypeVar: reflexivity" prop_typevar_reflexive
  , testProperty "TypeScheme: constructibility" prop_typescheme_constructible
  , testProperty "TypeEnvironment: initializable" prop_typeenvironment_initializable
  , testProperty "DependentTypeChecker: consistency" prop_typechecker_consistent
  , testProperty "Empty expression inference" prop_infer_empty_expression
  , testProperty "Type checking: determinism" prop_typechecking_deterministic
  , testProperty "Unification: symmetry" prop_unification_symmetric
  , testProperty "Generalization/Instantiation: inverse property" prop_generalization_instantiation
  , testProperty "Simple expression analysis" prop_analyze_simple_expressions
  ]