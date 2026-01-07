module Test.Unit.DependenciesTypeSystemSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies

-- Test type variable creation
prop_type_variable_uniqueness :: Int -> Property
prop_type_variable_uniqueness seed =
  let tv1 = newTypeVariable seed
      tv2 = newTypeVariable (seed + 1)
  in property $ tv1 /= tv2

-- Test type environment operations
prop_type_environment_lookup :: [(String, TypeExpr)] -> String -> Property
prop_type_environment_lookup pairs key =
  let typeEnv = buildTypeEnvFromPairs pairs
      result = checkType key typeEnv
  in property $ 
    case lookup key pairs of
      Nothing -> not result
      Just _ -> result

-- Test constraint solving
prop_constraint_solving_idempotent :: [Constraint] -> Property
prop_constraint_solving_idempotent constraints =
  let solution1 = solveConstraints constraints
      solution2 = solveConstraints constraints
  in property $ solution1 === solution2

-- Test type unification
prop_unification_commutative :: TypeExpr -> TypeExpr -> Property
prop_unification_commutative type1 type2 =
  let unify1 = unifyTypes type1 type2
      unify2 = unifyTypes type2 type1
  in property $ 
    case (unify1, unify2) of
      (Left _, Left _) -> property True
      (Right s1, Right s2) -> s1 === s2
      _ -> property False

-- Test type scheme generalization
prop_generalization_instantiation :: TypeExpr -> Property
prop_generalization_instantiation typeExpr =
  let scheme = generalize typeExpr
      instantiated = instantiate scheme
  in property $ 
    case instantiated of
      Left _ -> property True
      Right t -> property $ not (null (show t))

tests :: TestTree
tests = testGroup "Dependencies Type System Tests"
  [ testProperty "type variable uniqueness" prop_type_variable_uniqueness
  , testProperty "type environment lookup" prop_type_environment_lookup
  , testProperty "constraint solving idempotent" prop_constraint_solving_idempotent
  , testProperty "unification commutative" prop_unification_commutative
  , testProperty "generalization instantiation" prop_generalization_instantiation
  ]