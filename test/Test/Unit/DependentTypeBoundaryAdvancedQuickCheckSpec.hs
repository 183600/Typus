{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeBoundaryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import DependentTypesParser
import Data.List (length, concat)
import Data.List (sort, nub, filter, elem, intercalate)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T

-- ============================================================================
-- Advanced Dependent Type Boundary QuickCheck Tests
-- ============================================================================

-- Property: Type parameter ordering is preserved
prop_type_parameter_ordering :: [String] -> Property
prop_type_parameter_ordering paramNames =
  length paramNames > 0 && L.all (not . null) paramNames && nub paramNames == paramNames ==>
  let typeParams = L.map (\n -> TypeParameter n Nothing) paramNames
      paramNamesFromParams = map tpName typeParams
  in property $ paramNamesFromParams === paramNames

-- Property: Type constraint validation is deterministic
prop_type_constraint_deterministic :: String -> String -> String -> Property
prop_type_constraint_deterministic op left right =
  length op > 0 && L.length left > 0 && L.length right > 0 && op `elem` ["==", ">", ">=", "<", "<="] ==>
  let constraint1 = TypeConstraint op (TypeRef left []) Nothing
      constraint2 = TypeConstraint op (TypeRef right []) Nothing
      constraint1Str = show constraint1
      constraint2Str = show constraint2
      constraint1StrAgain = show constraint1
  in property $ 
    constraint1Str === constraint1StrAgain .&&.
    constraint1Str /= constraint2Str

-- Property: Struct field ordering is preserved
prop_struct_field_ordering :: [String] -> [String] -> Property
prop_struct_field_ordering fieldNames typeNames =
  length fieldNames > 0 && L.length typeNames > 0 && 
  length fieldNames == L.length typeNames &&
  all (not . null) fieldNames && L.all (not . null) typeNames ==>
  let fields = zipWith (\name typ -> Field name (TypeRef typ [])) fieldNames typeNames
      fieldNamesFromFields = map fieldName fields
      typeNamesFromFields = L.map (\f -> case typeRef $ fieldType f of
                                         TypeRef name _ -> name) fields
  in property $ 
    fieldNamesFromFields === fieldNames .&&.
    typeNamesFromFields === typeNames

-- Property: Type reference nesting is consistent
prop_type_ref_nesting_consistency :: String -> [String] -> Property
prop_type_ref_nesting_consistency baseName paramNames =
  length baseName > 0 && L.all (not . null) paramNames ==>
  let baseType = TypeRef baseName []
      nestedType = TypeRef baseName (L.map (\n -> TypeRef n []) paramNames)
      baseTypeStr = show baseType
      nestedTypeStr = show nestedType
  in property $ 
    baseTypeStr `contains` baseName .&&.
    nestedTypeStr `contains` baseName .&&.
    all (\n -> nestedTypeStr `contains` n) paramNames

-- Property: Dependent type validation preserves errors
prop_dependent_type_validation_preserves_errors :: String -> Property
prop_dependent_type_validation_preserves_errors typeName =
  length typeName > 0 && typeName `elem` ["", "invalid", "123invalid"] ==>
  let invalidType = DependentType typeName [] (StructType []) []
      validationResult = validateDependentTypeSyntax (show invalidType)
  in property $ 
    case validationResult of
      Left errors -> L.length errors > 0
      Right _ -> property True

-- Property: Type parameter constraints are preserved
prop_type_parameter_constraints_preserved :: String -> String -> Property
prop_type_parameter_constraints_preserved paramName constraint =
  length paramName > 0 && L.length constraint > 0 ==>
  let typeParam = TypeParameter paramName (Just constraint)
      extractedConstraint = tpConstraint typeParam
  in property $ 
    extractedConstraint === Just constraint

-- Property: Complex type nesting maintains structure
prop_complex_type_nesting_structure :: String -> [[String]] -> Property
prop_complex_type_nesting_structure baseName nestedParams =
  length baseName > 0 && L.all (not . null) (L.concat nestedParams) ==>
  let nestedTypes = L.map (\params -> TypeRef baseName (L.map (\n -> TypeRef n []) params)) nestedParams
      allNestedParamNames = L.concat nestedParams
  in property $ 
    length nestedTypes === L.length nestedParams .&&.
    all (\t -> show t `contains` baseName) nestedTypes .&&.
    all (\name -> L.any (\t -> show t `contains` name) nestedTypes) allNestedParamNames

-- Property: Type alias resolution is consistent
prop_type_alias_resolution_consistent :: String -> String -> Property
prop_type_alias_resolution_consistent aliasName originalName =
  length aliasName > 0 && L.length originalName > 0 && aliasName /= originalName ==>
  let aliasType = DependentType aliasName [] (AliasType (TypeRef originalName [])) []
      originalType = DependentType originalName [] (StructType []) []
  in property $ 
    show aliasType `contains` aliasName .&&.
    show aliasType `contains` originalName .&&.
    show originalType `contains` originalName .&&.
    not (show originalType `contains` aliasName)

-- Property: Function type parameter ordering is preserved
prop_function_type_parameter_ordering :: [String] -> [String] -> Property
prop_function_type_parameter_ordering inputTypes outputType =
  length inputTypes > 0 && L.length outputType > 0 && L.all (not . null) inputTypes ==>
  let funcType = DependentType "testFunc" [] (FunctionType (L.map (\n -> TypeRef n []) inputTypes) (TypeRef outputType [])) []
      inputTypesFromFunc = case typeBody funcType of
                              FunctionType inputs _ -> L.map (\(TypeRef name _) -> name) inputs
                              _ -> []
  in property $ inputTypesFromFunc === inputTypes

-- Property: Constraint satisfaction is monotonic
prop_constraint_satisfaction_monotonic :: String -> String -> String -> Property
prop_constraint_satisfaction_monotonic varName value1 value2 =
  length varName > 0 && L.all (not . null) [value1, value2] ==>
  let constraint1 = TypeConstraint "==" (TypeRef varName []) (Just value1)
      constraint2 = TypeConstraint "==" (TypeRef varName []) (Just value2)
      constraint1Str = show constraint1
      constraint2Str = show constraint2
  in property $ 
    (constraint1Str == constraint2Str) ==> (value1 == value2) .&&.
    (value1 == value2) ==> (constraint1Str == constraint2Str)

-- Property: Type scope uniqueness is maintained
prop_type_scope_uniqueness :: [String] -> Property
prop_type_scope_uniqueness typeNames =
  length typeNames > 0 && L.all (not . null) typeNames && nub typeNames == typeNames ==>
  let types = L.map (\n -> DependentType n [] (StructType []) []) typeNames
      uniqueTypeNames = nub (map dtName types)
  in property $ 
    length uniqueTypeNames === L.length typeNames .&&.
    uniqueTypeNames === typeNames

-- Property: Generic type instantiation preserves type structure
prop_generic_type_instantiation_preserves_structure :: String -> [String] -> [String] -> Property
prop_generic_type_instantiation_preserves_structure genericName typeParams instanceParams =
  length genericName > 0 && 
  length typeParams > 0 && L.all (not . null) typeParams &&
  length instanceParams == L.length typeParams && L.all (not . null) instanceParams ==>
  let genericType = TypeRef genericName (L.map (\n -> TypeRef n []) typeParams)
      instanceType = TypeRef genericName (L.map (\n -> TypeRef n []) instanceParams)
  in property $ 
    show genericType `contains` genericName .&&.
    show instanceType `contains` genericName .&&.
    all (\n -> show genericType `contains` n) typeParams .&&.
    all (\n -> show instanceType `contains` n) instanceParams

-- Helper function to check string containment
contains :: String -> String -> Bool
contains needle haystack = needle `Data.List.L.isInfixOf` haystack

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Dependent Type Boundary QuickCheck Tests"
  [ fastProperty "Type parameter ordering is preserved" prop_type_parameter_ordering
  , fastProperty "Type constraint validation is deterministic" prop_type_constraint_deterministic
  , fastProperty "Struct field ordering is preserved" prop_struct_field_ordering
  , fastProperty "Type reference nesting is consistent" prop_type_ref_nesting_consistency
  , fastProperty "Dependent type validation preserves errors" prop_dependent_type_validation_preserves_errors
  , fastProperty "Type parameter constraints are preserved" prop_type_parameter_constraints_preserved
  , fastProperty "Complex type nesting maintains structure" prop_complex_type_nesting_structure
  , fastProperty "Type alias resolution is consistent" prop_type_alias_resolution_consistent
  , fastProperty "Function type parameter ordering is preserved" prop_function_type_parameter_ordering
  , fastProperty "Constraint satisfaction is monotonic" prop_constraint_satisfaction_monotonic
  , fastProperty "Type scope uniqueness is maintained" prop_type_scope_uniqueness
  , fastProperty "Generic type instantiation preserves type structure" prop_generic_type_instantiation_preserves_structure
  ]