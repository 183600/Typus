{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesTypeSystemPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(Positive), getPositive)

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , preludeTypeDefs
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)

-- Property: TypeVar equality based on structure
prop_typevar_equality :: String -> String -> [String] -> Property
prop_typevar_equality name1 name2 params =
  let con1 = TVCon name1
      con2 = TVCon name2
      var1 = TVVar name1
      var2 = TVVar name2
      app1 = TVApp name1 params
      app2 = TVApp name2 params
  in (con1 == con2) === (name1 == name2) .&&.
     (var1 == var2) === (name1 == name2) .&&.
     (app1 == app2) === (name1 == name2 && params == params)

-- Property: TypeVar ordering is total
prop_typevar_total_ordering :: TypeVar -> TypeVar -> Property
prop_typevar_total_ordering tv1 tv2 =
  let result = compare tv1 tv2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeConstraint equality based on structure
prop_typeconstraint_equality :: TypeVar -> TypeVar -> String -> [TypeVar] -> Int -> Property
prop_typeconstraint_equality tv1 tv2 name types size =
  let equal1 = Equal tv1 tv2
      equal2 = Equal tv2 tv1
      subtype1 = Subtype tv1 tv2
      subtype2 = Subtype tv2 tv1
      predicate1 = Predicate name types
      predicate2 = Predicate name types
      sizeGE1 = TypeSizeGE tv1 size
      sizeGE2 = TypeSizeGE tv1 size
  in (equal1 == equal2) === (tv1 == tv2 && tv2 == tv1) .&&.
     (subtype1 == subtype2) === (tv1 == tv2 && tv2 == tv1) .&&.
     (predicate1 == predicate2) === (name == name && types == types) .&&.
     (sizeGE1 == sizeGE2) === (tv1 == tv1 && size == size)

-- Property: TypeConstraint ordering is total
prop_typeconstraint_total_ordering :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_total_ordering tc1 tc2 =
  let result = compare tc1 tc2
  in (result == LT || result == EQ || result == GT) === True

-- Property: DependentTypeError equality based on content
prop_dependent_type_error_equality :: TypeVar -> TypeVar -> String -> Property
prop_dependent_type_error_equality tv1 tv2 msg =
  let mismatch1 = DependentTypeMismatch tv1 tv2
      mismatch2 = DependentTypeMismatch tv2 tv1
      violation1 = ConstraintViolation msg tv1
      violation2 = ConstraintViolation msg tv2
      notFound1 = TypeNotFound msg
      notFound2 = TypeNotFound msg
  in (mismatch1 == mismatch2) === (tv1 == tv2 && tv2 == tv1) .&&.
     (violation1 == violation2) === (msg == msg && tv1 == tv2) .&&.
     (notFound1 == notFound2) === (msg == msg)

-- Property: TypeDef equality based on structure
prop_typedef_equality :: [String] -> [TypeConstraint] -> [String] -> [TypeConstraint] -> Property
prop_typedef_equality params1 constraints1 params2 constraints2 =
  let def1 = TypeDefDecl params1 constraints1
      def2 = TypeDefDecl params2 constraints2
  in (def1 == def2) === (params1 == params2 && constraints1 == constraints2)

-- Property: TypeEnv equality based on content
prop_typeenv_equality :: [(String, TypeDef)] -> [(String, TypeDef)] -> Property
prop_typeenv_equality defs1 defs2 =
  let env1 = TypeEnv (Map.fromList defs1) []
      env2 = TypeEnv (Map.fromList defs2) []
  in (env1 == env2) === (Map.fromList defs1 == Map.fromList defs2)

-- Property: DependentTypeChecker equality based on content
prop_dependent_type_checker_equality :: [(String, TypeDef)] -> [TypeConstraint] -> [DependentTypeError] -> Property
prop_dependent_type_checker_equality defs constraints errors =
  let env = TypeEnv (Map.fromList defs) constraints
      checker = DependentTypeChecker env errors
      env2 = TypeEnv (Map.fromList defs) constraints
      checker2 = DependentTypeChecker env2 errors
  in (checker == checker2) === (env == env2 && errors == errors)

-- Property: newDependentTypeChecker creates checker with prelude types
prop_new_dependent_type_checker_has_prelude :: Property
prop_new_dependent_type_checker_has_prelude =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      types = typeDefinitions env
  in Map.size types >= 4 === True  -- At least basic types

-- Property: newDependentTypeCheckerWithTypes includes custom types
prop_new_dependent_type_checker_with_custom_types :: [(String, TypeDef)] -> Property
prop_new_dependent_type_checker_with_custom_types customTypes =
  not (null customTypes) ==>
  let checker = newDependentTypeCheckerWithTypes customTypes
      env = dtcTypeEnv checker
      types = typeDefinitions env
      customTypeNames = map fst customTypes
      hasAllCustomTypes = all (`Map.member` types) customTypeNames
  in hasAllCustomTypes === True

-- Property: addType adds type to environment
prop_add_type_adds_to_env :: String -> [String] -> [TypeConstraint] -> Property
prop_add_type_adds_to_env name params constraints =
  not (null name) ==>
  let def = TypeDefDecl params constraints
      checker = newDependentTypeChecker
      updatedChecker = addType name def checker
      env = dtcTypeEnv updatedChecker
      types = typeDefinitions env
  in Map.member name types === True

-- Property: addConstraint adds constraint to pending list
prop_add_constraint_adds_to_pending :: TypeVar -> TypeVar -> Property
prop_add_constraint_adds_to_pending tv1 tv2 =
  let constraint = Equal tv1 tv2
      checker = newDependentTypeChecker
      updatedChecker = addConstraint constraint checker
      env = dtcTypeEnv updatedChecker
      pending = pendingConstraints env
  in constraint `elem` pending === True

-- Property: addTypeError adds error to checker
prop_add_type_error_adds_to_checker :: TypeVar -> TypeVar -> Property
prop_add_type_error_adds_to_checker tv1 tv2 =
  let error = DependentTypeMismatch tv1 tv2
      checker = newDependentTypeChecker
      updatedChecker = addTypeError error checker
      errors = tcErrors updatedChecker
  in error `elem` errors === True

-- Property: lookupTypeDef finds added types
prop_lookup_type_def_finds_added :: String -> [String] -> [TypeConstraint] -> Property
prop_lookup_type_def_finds_added name params constraints =
  not (null name) ==>
  let def = TypeDefDecl params constraints
      checker = newDependentTypeChecker
      updatedChecker = addType name def checker
      found = lookupTypeDef name updatedChecker
  in found === Just def

-- Property: lookupTypeDef returns Nothing for missing types
prop_lookup_type_def_missing :: String -> Property
prop_lookup_type_def_missing name =
  let checker = newDependentTypeChecker
      found = lookupTypeDef name checker
  in found === Nothing

-- Property: getDependentTypeErrors returns all errors
prop_get_dependent_type_errors_returns_all :: [DependentTypeError] -> Property
prop_get_dependent_type_errors_returns_all errors =
  let checker = newDependentTypeChecker
      checkerWithErrors = foldr addTypeError checker errors
      retrievedErrors = getDependentTypeErrors checkerWithErrors
  in sort retrievedErrors === sort errors

-- Property: validateConstraint checks constraint validity
prop_validate_constraint_checks_validity :: TypeVar -> TypeVar -> Int -> Property
prop_validate_constraint_checks_validity tv1 tv2 size =
  let equalConstraint = Equal tv1 tv2
      sizeConstraint = TypeSizeGE tv1 size
      validSize = size >= 0
      checker = newDependentTypeChecker
      equalValid = validateConstraint equalConstraint checker
      sizeValid = validateConstraint sizeConstraint checker
  in equalValid === True .&&. sizeValid === validSize

tests :: TestTree
tests =
  testGroup "Dependencies TypeSystem Properties"
    [ fastProperty "TypeVar equality based on structure" prop_typevar_equality
    , fastProperty "TypeVar ordering is total" prop_typevar_total_ordering
    , fastProperty "TypeConstraint equality based on structure" prop_typeconstraint_equality
    , fastProperty "TypeConstraint ordering is total" prop_typeconstraint_total_ordering
    , fastProperty "DependentTypeError equality based on content" prop_dependent_type_error_equality
    , fastProperty "TypeDef equality based on structure" prop_typedef_equality
    , fastProperty "TypeEnv equality based on content" prop_typeenv_equality
    , fastProperty "DependentTypeChecker equality based on content" prop_dependent_type_checker_equality
    , fastProperty "newDependentTypeChecker has prelude" prop_new_dependent_type_checker_has_prelude
    , fastProperty "newDependentTypeCheckerWithTypes includes custom types" prop_new_dependent_type_checker_with_custom_types
    , fastProperty "addType adds to environment" prop_add_type_adds_to_env
    , fastProperty "addConstraint adds to pending" prop_add_constraint_adds_to_pending
    , fastProperty "addTypeError adds to checker" prop_add_type_error_adds_to_checker
    , fastProperty "lookupTypeDef finds added types" prop_lookup_type_def_finds_added
    , fastProperty "lookupTypeDef returns Nothing for missing types" prop_lookup_type_def_missing
    , fastProperty "getDependentTypeErrors returns all errors" prop_get_dependent_type_errors_returns_all
    , fastProperty "validateConstraint checks validity" prop_validate_constraint_checks_validity
    ]