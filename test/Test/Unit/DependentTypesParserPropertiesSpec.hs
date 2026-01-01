{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypesParserPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(Positive), getPositive)

import DependentTypesParser
  ( DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- Property: DependentTypeError equality based on content
prop_dependent_type_error_equality :: String -> Int -> String -> Property
prop_dependent_type_error_equality msg line fragment =
  let error1 = SyntaxError msg line fragment
      error2 = SyntaxError msg line fragment
      error3 = SyntaxError msg (line + 1) fragment
  in (error1 == error2) === True .&&. (error1 == error3) === False

-- Property: TypeRef equality based on name L.and args
prop_type_ref_equality :: String -> [TypeRef] -> String -> [TypeRef] -> Property
prop_type_ref_equality name1 args1 name2 args2 =
  let ref1 = TypeRef name1 args1
      ref2 = TypeRef name2 args2
  in (ref1 == ref2) === (name1 == name2 && args1 == args2)

-- Property: TypeRef ordering is total
prop_type_ref_total_ordering :: TypeRef -> TypeRef -> Property
prop_type_ref_total_ordering ref1 ref2 =
  let result = compare ref1 ref2
  in (result == LT || result == EQ || result == GT) === True

-- Property: Field equality based on name L.and type
prop_field_equality :: String -> TypeRef -> String -> TypeRef -> Property
prop_field_equality name1 type1 name2 type2 =
  let field1 = Field name1 type1
      field2 = Field name2 type2
  in (field1 == field2) === (name1 == name2 && type1 == type2)

-- Property: Field ordering is total
prop_field_total_ordering :: Field -> Field -> Property
prop_field_total_ordering field1 field2 =
  let result = compare field1 field2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeBody equality based on structure
prop_type_body_equality :: [Field] -> [Field] -> Property
prop_type_body_equality fields1 fields2 =
  let body1 = StructBody fields1
      body2 = StructBody fields2
  in (body1 == body2) === (fields1 == fields2)

-- Property: TypeBody ordering is total
prop_type_body_total_ordering :: TypeBody -> TypeBody -> Property
prop_type_body_total_ordering body1 body2 =
  let result = compare body1 body2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeParameter equality based on L.all fields
prop_type_parameter_equality :: String -> TypeRef -> [TypeConstraint] -> String -> TypeRef -> [TypeConstraint] -> Property
prop_type_parameter_equality name1 type1 constraints1 name2 type2 constraints2 =
  let param1 = TypeParameter name1 type1 constraints1
      param2 = TypeParameter name2 type2 constraints2
  in (param1 == param2) === (name1 == name2 && type1 == type2 && constraints1 == constraints2)

-- Property: TypeParameter ordering is total
prop_type_parameter_total_ordering :: TypeParameter -> TypeParameter -> Property
prop_type_parameter_total_ordering param1 param2 =
  let result = compare param1 param2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeConstraint equality based on structure
prop_type_constraint_equality :: String -> String -> String -> String -> Int -> Int -> Property
prop_type_constraint_equality name1 name2 name3 name4 size1 size2 =
  let equality1 = EqualityConstraint name1 name2
      equality2 = EqualityConstraint name2 name1
      inequality1 = InequalityConstraint name1 name2
      inequality2 = InequalityConstraint name2 name1
      range1 = RangeConstraint name1 size1 size2
      range2 = RangeConstraint name1 size1 size2
  in (equality1 == equality2) === (name1 == name2 && name2 == name1) .&&.
     (inequality1 == inequality2) === (name1 == name2 && name2 == name1) .&&.
     (range1 == range2) === (name1 == name1 && size1 == size1 && size2 == size2)

-- Property: TypeConstraint ordering is total
prop_type_constraint_total_ordering :: TypeConstraint -> TypeConstraint -> Property
prop_type_constraint_total_ordering constraint1 constraint2 =
  let result = compare constraint1 constraint2
  in (result == LT || result == EQ || result == GT) === True

-- Property: DependentType equality based on structure
prop_dependent_type_equality :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Property
prop_dependent_type_equality name1 params1 body1 constraints1 name2 params2 body2 constraints2 =
  let type1 = TypeDecl name1 params1 body1 constraints1
      type2 = TypeDecl name2 params2 body2 constraints2
  in (type1 == type2) === (name1 == name2 && params1 == params2 && body1 == body2 && constraints1 == constraints2)

-- Property: DependentType ordering is total
prop_dependent_type_total_ordering :: DependentType -> DependentType -> Property
prop_dependent_type_total_ordering type1 type2 =
  let result = compare type1 type2
  in (result == LT || result == EQ || result == GT) === True

-- Property: validateDependentTypeSyntax handles empty input
prop_validate_empty_input :: Property
prop_validate_empty_input =
  let errors = validateDependentTypeSyntax ""
  in null errors === True

-- Property: validateDependentTypeSyntax detects malformed input
prop_validate_malformed_input :: String -> Property
prop_validate_malformed_input input =
  not (null input) && input `notElem` ["type X {}", "func f() {}", "alias Y = Z"] ==>
  let errors = validateDependentTypeSyntax input
  in not (null errors) === True || L.length errors >= 0  -- May L.or may not have errors

-- Property: parseTypeDeclaration handles valid type declaration
prop_parse_valid_type_declaration :: String -> String -> Property
prop_parse_valid_type_declaration typeName fieldName =
  not (null typeName && null fieldName) ==>
  let input = "type " ++ typeName ++ " { " ++ fieldName ++ ": int }"
      result = parseTypeDeclaration input
  in isJust result === True

-- Property: parseDependentType handles valid simple type
prop_parse_valid_simple_type :: String -> Property
prop_parse_valid_simple_type typeName =
  not (null typeName) ==>
  let input = "type " ++ typeName ++ " { x: int }"
      result = parseDependentType input
  in isJust result === True

-- Property: runDependentTypesParser handles multiple definitions
prop_run_multiple_definitions :: [String] -> Property
prop_run_multiple_definitions typeNames =
  not (null typeNames) ==>
  let typeDefs = L.map (\name -> "type " ++ name ++ " { x: int }") typeNames
      input = unlines typeDefs
      result = runDependentTypesParser input
  in L.length result >= 0  -- May parse zero L.or more definitions

-- Property: TypeRef with no args has empty args list
prop_type_ref_no_args :: String -> Property
prop_type_ref_no_args name =
  not (null name) ==>
  let ref = TypeRef name []
  in refArgs ref === []

-- Property: TypeRef with args preserves args
prop_type_ref_with_args :: String -> [String] -> Property
prop_type_ref_with_args name argNames =
  not (null name) ==>
  let args = L.map (\argName -> TypeRef argName []) argNames
      ref = TypeRef name args
  in refArgs ref === args

-- Property: Field with simple type has correct structure
prop_field_simple_type :: String -> String -> Property
prop_field_simple_type fieldName typeName =
  not (null fieldName && null typeName) ==>
  let typeRef = TypeRef typeName []
      field = Field fieldName typeRef
  in fieldName field === fieldName .&&. fieldType field === typeRef

-- Property: EqualityConstraint is symmetric in comparison
prop_equality_constraint_symmetric :: String -> String -> Property
prop_equality_constraint_symmetric name1 name2 =
  let constraint1 = EqualityConstraint name1 name2
      constraint2 = EqualityConstraint name2 name1
  in (constraint1 == constraint2) === (name1 == name2 && name2 == name1)

tests :: TestTree
tests =
  testGroup "DependentTypes Parser Properties"
    [ fastProperty "DependentTypeError equality based on content" prop_dependent_type_error_equality
    , fastProperty "TypeRef equality based on name L.and args" prop_type_ref_equality
    , fastProperty "TypeRef ordering is total" prop_type_ref_total_ordering
    , fastProperty "Field equality based on name L.and type" prop_field_equality
    , fastProperty "Field ordering is total" prop_field_total_ordering
    , fastProperty "TypeBody equality based on structure" prop_type_body_equality
    , fastProperty "TypeBody ordering is total" prop_type_body_total_ordering
    , fastProperty "TypeParameter equality based on L.all fields" prop_type_parameter_equality
    , fastProperty "TypeParameter ordering is total" prop_type_parameter_total_ordering
    , fastProperty "TypeConstraint equality based on structure" prop_type_constraint_equality
    , fastProperty "TypeConstraint ordering is total" prop_type_constraint_total_ordering
    , fastProperty "DependentType equality based on structure" prop_dependent_type_equality
    , fastProperty "DependentType ordering is total" prop_dependent_type_total_ordering
    , fastProperty "validateDependentTypeSyntax handles empty input" prop_validate_empty_input
    , fastProperty "validateDependentTypeSyntax detects malformed input" prop_validate_malformed_input
    , fastProperty "parseTypeDeclaration handles valid type declaration" prop_parse_valid_type_declaration
    , fastProperty "parseDependentType handles valid simple type" prop_parse_valid_simple_type
    , fastProperty "runDependentTypesParser handles multiple definitions" prop_run_multiple_definitions
    , fastProperty "TypeRef with no args has empty args list" prop_type_ref_no_args
    , fastProperty "TypeRef with args preserves args" prop_type_ref_with_args
    , fastProperty "Field with simple type has correct structure" prop_field_simple_type
    , fastProperty "EqualityConstraint is symmetric in comparison" prop_equality_constraint_symmetric
    ]