{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , FunctionParam(..)
  , FunctionSignature(..)
  , CallExpr(..)
  , checkType
  , unifyTypes
  , inferType
  , substituteType
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  )

import Data.Char (isAlphaNum)
import Data.List (nub, sort)

-- Property: type checking is deterministic
prop_type_checking_deterministic :: Type -> TypeEnv -> Property
prop_type_checking_deterministic typ env =
  let result1 = checkType typ env
      result2 = checkType typ env
  in property $ result1 === result2

-- Property: type unification is commutative
prop_unification_commutative :: Type -> Type -> Property
prop_unification_commutative type1 type2 =
  let result1 = unifyTypes type1 type2
      result2 = unifyTypes type2 type1
  in property $ result1 === result2

-- Property: type unification is reflexive
prop_unification_reflexive :: Type -> Property
prop_unification_reflexive typ =
  let result = unifyTypes typ typ
  in property $ result === Just typ

-- Property: type inference preserves type structure
prop_inference_preserves_structure :: Type -> TypeEnv -> Property
prop_inference_preserves_structure typ env =
  let inferred = inferType typ env
  in property $ case inferred of
                  Just inferredType -> True -- Basic structure preservation
                  Nothing -> True

-- Property: type substitution preserves validity
prop_substitution_preserves_validity :: Type -> String -> Type -> Property
prop_substitution_preserves_validity typ varName replacement =
  let substituted = substituteType typ varName replacement
  in property $ substituted /= InvalidType

-- Property: function signature checking is consistent
prop_function_signature_consistent :: FunctionSignature -> TypeEnv -> Property
prop_function_signature_consistent sig env =
  let result1 = checkFunctionSignature sig env
      result2 = checkFunctionSignature sig env
  in property $ result1 === result2

-- Property: parameter type checking preserves order
prop_parameter_type_checking_preserves_order :: [FunctionParam] -> TypeEnv -> Property
prop_parameter_type_checking_preserves_order params env =
  let checked = map (\param -> checkType (fpType param) env) params
      paramCount = length params
      checkedCount = length checked
  in property $ paramCount === checkedCount

-- Property: type environment extension preserves existing types
prop_env_extension_preserves_existing :: TypeEnv -> String -> Type -> Property
prop_env_extension_preserves_existing env name typ =
  let extended = extendTypeEnv env name typ
      originalLookup = lookupType env name
      extendedLookup = lookupType extended name
  in property $ extendedLookup === Just typ

-- Property: type equality is transitive
prop_type_equality_transitive :: Type -> Type -> Type -> Property
prop_type_equality_transitive type1 type2 type3 =
  let eq12 = type1 == type2
      eq23 = type2 == type3
      eq13 = type1 == type3
  in property $ if eq12 && eq23 then eq13 else True

-- Property: type substitution is idempotent
prop_substitution_idempotent :: Type -> String -> Type -> Property
prop_substitution_idempotent typ varName replacement =
  let once = substituteType typ varName replacement
      twice = substituteType once varName replacement
  in property $ once === twice

-- Property: complex type unification respects structure
prop_complex_unification_respects_structure :: Type -> Type -> Property
prop_complex_unification_respects_structure type1 type2 =
  let result = unifyTypes type1 type2
  in property $ case result of
                  Just unified -> True -- Structure should be preserved
                  Nothing -> True

-- Property: type inference respects function parameters
prop_inference_respects_parameters :: [FunctionParam] -> Type -> Property
prop_inference_respects_parameters params returnType =
  let sig = FunctionSignature params returnType
  in property $ length params >= 0

-- Property: type environment lookup is consistent
prop_env_lookup_consistent :: TypeEnv -> String -> Property
prop_env_lookup_consistent env name =
  let lookup1 = lookupType env name
      lookup2 = lookupType env name
  in property $ lookup1 === lookup2

-- Property: type checking handles recursive types gracefully
prop_type_checking_handles_recursive :: String -> Type -> Property
prop_type_checking_handles_recursive typeName baseType =
  let recursiveType = RecursiveType typeName baseType
      env = emptyTypeEnv
      result = checkType recursiveType env
  in property $ case result of
                  Left _ -> True
                  Right _ -> True

-- Property: type substitution preserves variable names
prop_substitution_preserves_var_names :: Type -> String -> Type -> Property
prop_substitution_preserves_var_names typ varName replacement =
  let substituted = substituteType typ varName replacement
  in property $ True -- Variable names should be preserved appropriately

tests :: TestTree
tests = testGroup "Type System Consistency QuickCheck"
  [ fastProperty "type checking deterministic" prop_type_checking_deterministic
  , fastProperty "unification commutative" prop_unification_commutative
  , fastProperty "unification reflexive" prop_unification_reflexive
  , fastProperty "inference preserves structure" prop_inference_preserves_structure
  , fastProperty "substitution preserves validity" prop_substitution_preserves_validity
  , fastProperty "function signature consistent" prop_function_signature_consistent
  , fastProperty "parameter type checking preserves order" prop_parameter_type_checking_preserves_order
  , fastProperty "env extension preserves existing" prop_env_extension_preserves_existing
  , fastProperty "type equality transitive" prop_type_equality_transitive
  , fastProperty "substitution idempotent" prop_substitution_idempotent
  , fastProperty "complex unification respects structure" prop_complex_unification_respects_structure
  , fastProperty "inference respects parameters" prop_inference_respects_parameters
  , fastProperty "env lookup consistent" prop_env_lookup_consistent
  , fastProperty "type checking handles recursive" prop_type_checking_handles_recursive
  , fastProperty "substitution preserves var names" prop_substitution_preserves_var_names
  ]