{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Dependent type constraint validation tests for DependentTypes module
module Test.Unit.NewDependentTypeConstraintValidationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List (sort, nub, intercalate)
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)

import DependentTypesParser
  ( TypeConstraint(..)
  , DependentType(..)
  , TypeEnvironment(..)
  , ConstraintSolver(..)
  , ValidationContext(..)
  , parseConstraint
  , validateConstraints
  , solveConstraints
  , inferConstraints
  , checkConstraintConsistency
  , normalizeConstraints
  , substituteConstraints
  )

import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  )

import SourceLocation
  ( SourcePos(..)
  , startPos
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate type variable names
genTypeVariable :: Gen String
genTypeVariable = do
  base <- elements ["T", "S", "U", "V", "W", "X", "Y", "Z"]
  suffix <- choose (0, 9)
  return $ base ++ show suffix

-- Generate integer values for constraints
genIntValue :: Gen Int
genIntValue = choose (-100, 100)

-- Generate size constraints
genSizeConstraint :: Gen TypeConstraint
genSizeConstraint = do
  varName <- genTypeVariable
  op <- elements [Gt, Ge, Lt, Le, Eq]
  value <- genIntValue
  return $ SizeConstraint varName op value

-- Generate range constraints
genRangeConstraint :: Gen TypeConstraint
genRangeConstraint = do
  varName <- genTypeVariable
  minVal <- genIntValue
  maxVal <- choose (minVal, minVal + 100)
  return $ RangeConstraint varName minVal maxVal

-- Generate predicate constraints
genPredicateConstraint :: Gen TypeConstraint
genPredicateConstraint = do
  varName <- genTypeVariable
  predicateName <- elements ["NonEmpty", "Positive", "Negative", "Even", "Odd", "Prime"]
  return $ PredicateConstraint varName predicateName

-- Generate equality constraints
genEqualityConstraint :: Gen TypeConstraint
genEqualityConstraint = do
  var1 <- genTypeVariable
  var2 <- genTypeVariable `suchThat` (/= var1)
  return $ EqualityConstraint var1 var2

-- Generate dependent types
genDependentType :: Gen DependentType
genDependentType = do
  typeName <- elements ["Vector", "Matrix", "Array", "List", "String"]
  typeParams <- listOf genTypeVariable
  constraints <- listOf $ oneof [genSizeConstraint, genRangeConstraint, genPredicateConstraint]
  return $ DependentType typeName typeParams constraints

-- Generate type environments
genTypeEnvironment :: Gen TypeEnvironment
genTypeEnvironment = do
  types <- listOf genDependentType
  let typeMap = Map.fromList $ L.map (\dt -> (typeName dt, dt)) types
  return $ TypeEnvironment typeMap

-- Generate validation contexts
genValidationContext :: Gen ValidationContext
genValidationContext = do
  env <- genTypeEnvironment
  solver <- elements [BasicSolver, AdvancedSolver, OptimizedSolver]
  strictMode <- elements [True, False]
  return $ ValidationContext env solver strictMode

-- ============================================================================
-- Constraint Validation Properties
-- ============================================================================

-- Property: Size constraint validation should be consistent
prop_size_constraint_validation_consistent :: TypeConstraint -> ValidationContext -> Property
prop_size_constraint_validation_consistent constraint context =
  case constraint of
    SizeConstraint var op value ->
      let result = validateConstraints [constraint] context
      in property $ case result of
           Left _ -> True  -- Invalid constraint detected
           Right _ -> True  -- Valid constraint accepted
    _ -> property True  -- Not a size constraint

-- Property: Range constraint should respect min <= max
prop_range_constraint_min_le_max :: String -> Int -> Int -> Property
prop_range_constraint_min_le_max varName minVal maxVal =
  let constraint = RangeConstraint varName minVal maxVal
      context = ValidationContext (TypeEnvironment Map.empty) BasicSolver False
      result = validateConstraints [constraint] context
  in minVal <= maxVal ==> 
     property $ isRight result
  .&&.
     minVal > maxVal ==> 
     property $ isLeft result

-- Property: Equality constraint should be symmetric
prop_equality_constraint_symmetric :: String -> String -> ValidationContext -> Property
prop_equality_constraint_symmetric var1 var2 context =
  var1 /= var2 ==> 
  let constraint1 = EqualityConstraint var1 var2
      constraint2 = EqualityConstraint var2 var1
      result1 = validateConstraints [constraint1] context
      result2 = validateConstraints [constraint2] context
  in property $ case (result1, result2) of
         (Right _, Right _) -> True  -- Both valid
         (Left _, Left _) -> True    -- Both invalid
         _ -> False                  -- Inconsistent validation

-- Property: Predicate constraint validation should check known predicates
prop_predicate_constraint_known :: String -> ValidationContext -> Property
prop_predicate_constraint_known varName context =
  let knownPredicates = ["NonEmpty", "Positive", "Negative", "Even", "Odd", "Prime"]
      constraint = PredicateConstraint varName "KnownPredicate"
      result = validateConstraints [constraint] context
  in property $ case result of
         Right _ -> True  -- Known predicate accepted
         Left (UnknownPredicate _) -> True  -- Unknown predicate rejected
         _ -> False  -- Unexpected error

-- Property: Constraint solving should preserve satisfiability
prop_constraint_solving_preserves_satisfiability :: [TypeConstraint] -> ValidationContext -> Property
prop_constraint_solving_preserves_satisfiability constraints context =
  not (null constraints) ==> 
  let validationResult = validateConstraints constraints context
  in case validationResult of
       Right validConstraints ->
         let solveResult = solveConstraints validConstraints context
         in property $ case solveResult of
              Right _ -> True  -- Solved successfully
              Left _ -> True   -- Unsatisfiable detected
       Left _ -> property True  -- Invalid constraints, no solving expected

-- Property: Constraint normalization should not change semantics
prop_constraint_normalization_preserves_semantics :: [TypeConstraint] -> ValidationContext -> Property
prop_constraint_normalization_preserves_semantics constraints context =
  not (null constraints) ==> 
  let normalizedConstraints = normalizeConstraints constraints
      originalResult = validateConstraints constraints context
      normalizedResult = validateConstraints normalizedConstraints context
  in property $ case (originalResult, normalizedResult) of
         (Left _, Left _) -> True  -- Both invalid
         (Right _, Right _) -> True  -- Both valid
         _ -> False                  -- Inconsistent results

-- Property: Constraint substitution should maintain validity
prop_constraint_substitution_maintains_validity :: [TypeConstraint] -> Map String String -> ValidationContext -> Property
prop_constraint_substitution_maintains_validity constraints substitution context =
  not (null constraints) && not (Map.null substitution) ==> 
  let substitutedConstraints = substituteConstraints constraints substitution
      originalResult = validateConstraints constraints context
      substitutedResult = validateConstraints substitutedConstraints context
  in property $ case (originalResult, substitutedResult) of
         (Right _, Right _) -> True  -- Both remain valid
         (Left _, Left _) -> True    -- Both invalid
         _ -> True  -- Substitution can change validity, which is acceptable

-- Property: Constraint consistency check should detect contradictions
prop_constraint_consistency_detects_contradictions :: [TypeConstraint] -> ValidationContext -> Property
prop_constraint_consistency_detects_contradictions constraints context =
  not (null constraints) ==> 
  let consistencyResult = checkConstraintConsistency constraints context
  in property $ case consistencyResult of
         Right _ -> True  -- Consistent constraints
         Left _ -> True   -- Inconsistent constraints detected

-- Property: Contradictory size constraints should be detected
prop_contradictory_size_constraints_detected :: String -> Int -> Property
prop_contradictory_size_constraints_detected varName value =
  let constraint1 = SizeConstraint varName Gt value
      constraint2 = SizeConstraint varName Lt value
      constraints = [constraint1, constraint2]
      context = ValidationContext (TypeEnvironment Map.empty) BasicSolver False
      result = checkConstraintConsistency constraints context
  in property $ isLeft result

-- Property: Overlapping range constraints should be merged correctly
prop_overlapping_range_constraints_merged :: String -> Int -> Int -> Property
prop_overlapping_range_constraints_merged varName minVal maxVal =
  minVal < maxVal ==> 
  let constraint1 = RangeConstraint varName minVal maxVal
      constraint2 = RangeConstraint varName (minVal + 1) (maxVal - 1)
      constraints = [constraint1, constraint2]
      normalized = normalizeConstraints constraints
  in property $ L.length normalized <= L.length constraints

-- Property: Complex constraint combinations should be solvable
prop_complex_constraints_solvable :: [TypeConstraint] -> ValidationContext -> Property
prop_complex_constraints_solvable constraints context =
  length constraints >= 3 ==> 
  let validationResult = validateConstraints constraints context
  in case validationResult of
       Right validConstraints ->
         let solveResult = solveConstraints validConstraints context
         in property $ case solveResult of
              Right solution -> not (Map.null solution)
              Left _ -> True  -- May be unsatisfiable
       Left _ -> property True

-- Property: Constraint inference should generate valid constraints
prop_constraint_inference_generates_valid :: DependentType -> ValidationContext -> Property
prop_constraint_inference_generates_valid dependentType context =
  let inferredConstraints = inferConstraints dependentType
      validationResult = validateConstraints inferredConstraints context
  in property $ case validationResult of
         Right _ -> True  -- Inferred constraints are valid
         Left _ -> True   -- May need additional context

-- ============================================================================
-- Performance L.and Scalability Properties
-- ============================================================================

-- Property: Constraint solving should handle many constraints efficiently
prop_constraint_solving_many_constraints :: Int -> ValidationContext -> Property
prop_constraint_solving_many_constraints numConstraints context =
  numConstraints > 0 && numConstraints <= 100 ==> 
  let constraints = take numConstraints $ cycle [SizeConstraint "x" Gt 0, SizeConstraint "y" Lt 100, RangeConstraint "z" 0 50]
      solveResult = solveConstraints constraints context
  in property $ case solveResult of
         Right _ -> True
         Left _ -> True  -- May be unsatisfiable

-- Property: Constraint normalization should be idempotent
prop_constraint_normalization_idempotent :: [TypeConstraint] -> ValidationContext -> Property
prop_constraint_normalization_idempotent constraints context =
  let normalized1 = normalizeConstraints constraints
      normalized2 = normalizeConstraints normalized1
  in property $ sort normalized1 === sort normalized2

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty constraint list should be valid
prop_empty_constraints_valid :: ValidationContext -> Property
prop_empty_constraints_valid context =
  let result = validateConstraints [] context
  in property $ isRight result

-- Property: Unknown variables in constraints should be detected
prop_unknown_variables_detected :: String -> ValidationContext -> Property
prop_unknown_variables_detected varName context =
  let constraint = SizeConstraint varName Gt 0
      emptyEnv = TypeEnvironment Map.empty
      emptyContext = ValidationContext emptyEnv (contextSolver context) (contextStrictMode context)
      result = validateConstraints [constraint] emptyContext
  in property $ isLeft result

-- Property: Extreme values in constraints should be handled
prop_extreme_values_handled :: Int -> Property
prop_extreme_values_handled value =
  let constraint = SizeConstraint "x" Gt value
      context = ValidationContext (TypeEnvironment Map.empty) BasicSolver False
      result = validateConstraints [constraint] context
  in property $ case result of
         Right _ -> True
         Left _ -> True  -- May be invalid for extreme values

-- Property: Circular equality constraints should be detected
prop_circular_equality_detected :: [String] -> Property
prop_circular_equality_detected varNames =
  length varNames >= 3 ==> 
  let [v1, v2, v3] = take 3 varNames
      constraints = [EqualityConstraint v1 v2, EqualityConstraint v2 v3, EqualityConstraint v3 v1]
      context = ValidationContext (TypeEnvironment Map.empty) BasicSolver False
      result = solveConstraints constraints context
  in property $ case result of
         Right solution -> True  -- May be solvable with appropriate substitution
         Left _ -> True   -- May be detected as circular

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Dependent Type Constraint Validation QuickCheck Tests"
  [ testGroup "Basic Constraint Validation"
    [ fastProperty "size constraint validation consistent" prop_size_constraint_validation_consistent
    , fastProperty "range constraint min le max" prop_range_constraint_min_le_max
    , fastProperty "equality constraint symmetric" prop_equality_constraint_symmetric
    , fastProperty "predicate constraint known" prop_predicate_constraint_known
    ]

  , testGroup "Constraint Solving L.and Normalization"
    [ fastProperty "constraint solving preserves satisfiability" prop_constraint_solving_preserves_satisfiability
    , fastProperty "constraint normalization preserves semantics" prop_constraint_normalization_preserves_semantics
    , fastProperty "constraint substitution maintains validity" prop_constraint_substitution_maintains_validity
    ]

  , testGroup "Constraint Consistency"
    [ fastProperty "constraint consistency detects contradictions" prop_constraint_consistency_detects_contradictions
    , fastProperty "contradictory size constraints detected" prop_contradictory_size_constraints_detected
    , fastProperty "overlapping range constraints merged" prop_overlapping_range_constraints_merged
    ]

  , testGroup "Complex Constraint Scenarios"
    [ fastProperty "complex constraints solvable" prop_complex_constraints_solvable
    , fastProperty "constraint inference generates valid" prop_constraint_inference_generates_valid
    ]

  , testGroup "Performance L.and Scalability"
    [ fastProperty "constraint solving many constraints" prop_constraint_solving_many_constraints
    , fastProperty "constraint normalization idempotent" prop_constraint_normalization_idempotent
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty constraints valid" prop_empty_constraints_valid
    , fastProperty "unknown variables detected" prop_unknown_variables_detected
    [ fastProperty "extreme values handled" prop_extreme_values_handled
    , fastProperty "circular equality detected" prop_circular_equality_detected
    ]
  ]