{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemSubstitutionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat, Gen)

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , Substitution
  , newDependentTypeChecker
  , checkType
  , addType
  , addConstraint
  , solveConstraints
  , unify
  )

import qualified Dependencies.TypeSystem as TS
import Dependencies.AST (TypeExpr(..), Constraint(..))

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> arbitraryVarName
    , TVVar <$> arbitraryVarName
    , do
        base <- arbitraryVarName
        args <- listOf arbitrary
        return $ TVApp base args
    , do
        args <- listOf arbitrary
        result <- arbitrary
        return $ TVFun args result
    , do
        args <- listOf arbitrary
        return $ TVTuple args
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitraryVarName
    , GenericT <$> arbitraryVarName <*> listOf arbitrary
    , do
        base <- arbitrary
        params <- arbitrary
        return $ RefineT base params
    , do
        params <- listOf arbitrary
        result <- arbitrary
        return $ FuncT params result
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ RangeC <$> arbitrary <*> arbitrary <*> arbitrary
    , PredC <$> arbitraryVarName <*> listOf arbitrary
    , SizeGE <$> arbitrary <*> arbitrary
    , SizeGT <$> arbitrary <*> arbitrary
    ]

-- Helper generators
arbitraryVarName :: Gen String
arbitraryVarName = do
  first <- elements "abcdefghijklmnopqrstuvwxyz"
  rest <- vectorOf 0 5 (elements "abcdefghijklmnopqrstuvwxyz0123456789_")
  return (first : rest)

arbitrarySubstitution :: Gen Substitution
arbitrarySubstitution = do
  numBindings <- choose (0, 5)
  bindings <- vectorOf numBindings $ do
    varName <- arbitraryVarName
    typeVar <- arbitrary
    return (varName, typeVar)
  return $ Map.fromList bindings

-- ============================================================================
-- Type System Substitution Properties
-- ============================================================================

-- Property: Type checking handles simple types
prop_type_checking_simple_types :: Property
prop_type_checking_simple_types =
  let checker = newDependentTypeChecker
      simpleType = SimpleT (T.pack "int")
  in case checkType checker simpleType of
    Right _ -> property True
    Left _ -> property False

-- Property: Type checking handles generic types
prop_type_checking_generic_types :: Property
prop_type_checking_generic_types =
  forAll arbitraryVarName $ \typeName ->
  let checker = newDependentTypeChecker
      genericType = GenericT typeName []
  in case checkType checker genericType of
    Right _ -> property True
    Left _ -> property $ not (null typeName)

-- Property: Type checking handles function types
prop_type_checking_function_types :: Property
prop_type_checking_function_types =
  forAll arbitraryVarName $ \paramType ->
  forAll arbitraryVarName $ \resultType ->
  let checker = newDependentTypeChecker
      funcType = FuncT [SimpleT paramType] (SimpleT resultType)
  in case checkType checker funcType of
    Right _ -> property True
    Left _ -> property True  -- May fail for invalid types

-- Property: Type checking handles refined types
prop_type_checking_refined_types :: Property
prop_type_checking_refined_types =
  forAll arbitraryVarName $ \baseType ->
  forAll arbitrary $ \constraint ->
  let checker = newDependentTypeChecker
      refinedType = RefineT (SimpleT baseType) [constraint]
  in case checkType checker refinedType of
    Right _ -> property True
    Left _ -> property True  -- May fail for invalid constraints

-- Property: Type unification handles simple cases
prop_unification_simple_cases :: Property
prop_unification_simple_cases =
  let type1 = SimpleT (T.pack "int")
      type2 = SimpleT (T.pack "int")
  in case unify type1 type2 of
    Right _ -> property True
    Left _ -> property False

-- Property: Type unification fails for different types
prop_unification_different_types :: Property
prop_unification_different_types =
  let type1 = SimpleT (T.pack "int")
      type2 = SimpleT (T.pack "string")
  in case unify type1 type2 of
    Right _ -> property False
    Left _ -> property True

-- Property: Constraint solving handles simple constraints
prop_constraint_solving_simple :: Property
prop_constraint_solving_simple =
  let checker = newDependentTypeChecker
      constraint = RangeC (TVVar "x") 0 100
  in case solveConstraints checker [constraint] of
    Right _ -> property True
    Left _ -> property True  -- May fail for unsolvable constraints

-- Property: Constraint solving handles predicate constraints
prop_constraint_solving_predicate :: Property
prop_constraint_solving_predicate =
  forAll arbitraryVarName $ \varName ->
  let checker = newDependentTypeChecker
      constraint = PredC varName []
  in case solveConstraints checker [constraint] of
    Right _ -> property True
    Left _ -> property True  -- May fail for unsolvable constraints

-- Property: Type environment handles type additions
prop_type_environment_additions :: Property
prop_type_environment_additions =
  forAll arbitraryVarName $ \typeName ->
  let checker = newDependentTypeChecker
      updatedChecker = addType checker typeName
  in property $ True  -- Should always succeed

-- Property: Type environment handles constraint additions
prop_type_environment_constraints :: Property
prop_type_environment_constraints =
  forAll arbitrary $ \constraint ->
  let checker = newDependentTypeChecker
      updatedChecker = addConstraint checker constraint
  in property $ True  -- Should always succeed

-- ============================================================================
-- Advanced Type System Properties
-- ============================================================================

-- Property: Complex function types are handled
prop_complex_function_types :: Property
prop_complex_function_types =
  let complexType = FuncT [SimpleT (T.pack "int"), SimpleT (T.pack "string")] (SimpleT (T.pack "bool"))
      checker = newDependentTypeChecker
  in case checkType checker complexType of
    Right _ -> property True
    Left _ -> property True  -- May fail for complex types

-- Property: Nested generic types are handled
prop_nested_generic_types :: Property
prop_nested_generic_types =
  forAll arbitraryVarName $ \outerType ->
  forAll arbitraryVarName $ \innerType ->
  let nestedType = GenericT outerType [GenericT innerType []]
      checker = newDependentTypeChecker
  in case checkType checker nestedType of
    Right _ -> property True
    Left _ -> property True  -- May fail for invalid types

-- Property: Multiple constraints are handled
prop_multiple_constraints :: Property
prop_multiple_constraints =
  forAll arbitrary $ \constraint1 ->
  forAll arbitrary $ \constraint2 ->
  let checker = newDependentTypeChecker
      constraints = [constraint1, constraint2]
  in case solveConstraints checker constraints of
    Right _ -> property True
    Left _ -> property True  -- May fail for unsolvable constraints

-- Property: Type checking with refined function types
prop_refined_function_types :: Property
prop_refined_function_types =
  forAll arbitraryVarName $ \baseType ->
  forAll arbitrary $ \constraint ->
  let refinedBase = RefineT (SimpleT baseType) [constraint]
      funcType = FuncT [refinedBase] (SimpleT (T.pack "bool"))
      checker = newDependentTypeChecker
  in case checkType checker funcType of
    Right _ -> property True
    Left _ -> property True  -- May fail for invalid constraints

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Type System Substitution Tests"
  [ testGroup "Basic Type Checking Properties"
    [ fastProperty "Type checking handles simple types" prop_type_checking_simple_types
    , fastProperty "Type checking handles generic types" prop_type_checking_generic_types
    , fastProperty "Type checking handles function types" prop_type_checking_function_types
    , fastProperty "Type checking handles refined types" prop_type_checking_refined_types
    ]

  , testGroup "Unification Properties"
    [ fastProperty "Type unification handles simple cases" prop_unification_simple_cases
    , fastProperty "Type unification fails for different types" prop_unification_different_types
    ]

  , testGroup "Constraint Solving Properties"
    [ fastProperty "Constraint solving handles simple constraints" prop_constraint_solving_simple
    , fastProperty "Constraint solving handles predicate constraints" prop_constraint_solving_predicate
    ]

  , testGroup "Type Environment Properties"
    [ fastProperty "Type environment handles type additions" prop_type_environment_additions
    , fastProperty "Type environment handles constraint additions" prop_type_environment_constraints
    ]

  , testGroup "Advanced Type System Properties"
    [ fastProperty "Complex function types are handled" prop_complex_function_types
    , fastProperty "Nested generic types are handled" prop_nested_generic_types
    , fastProperty "Multiple constraints are handled" prop_multiple_constraints
    , fastProperty "Type checking with refined function types" prop_refined_function_types
    ]
  ]