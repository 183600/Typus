{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypesValidationComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (isInfixOf, null, length, sort)
import Data.Maybe (isJust, isNothing)

import DependentTypesParser
  ( DependentTypesParser(..)
  , DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentParseResult
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

-- | Comprehensive QuickCheck tests for Dependent Types validation
-- This module tests dependent type parsing, validation, and constraint checking

-- Property: TypeRef construction maintains invariants
prop_typeRef_construction :: String -> [String] -> Property
prop_typeRef_construction name args =
  not (null name) && all (not . null) args ==>
  let typeRef = TypeRef name args
  in refName typeRef === name && refArgs typeRef === args

-- Property: TypeRef equality is structural
prop_typeRef_equality :: String -> [String] -> Property
prop_typeRef_equality name args =
  not (null name) ==>
  let typeRef1 = TypeRef name args
      typeRef2 = TypeRef name args
      typeRef3 = TypeRef (name ++ "X") args
  in typeRef1 === typeRef2 && typeRef1 /= typeRef3

-- Property: TypeRef Show is readable
prop_typeRef_show :: String -> [String] -> Property
prop_typeRef_show name args =
  not (null name) && length args <= 3 ==>
  let typeRef = TypeRef name args
      shown = show typeRef
  in name `isInfixOf` shown

-- Property: runDependentTypesParser handles empty input
prop_runDependentTypesParser_empty :: Property
prop_runDependentTypesParser_empty =
  let result = runDependentTypesParser ""
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> null types && null errors

-- Property: runDependentTypesParser handles simple type definition
prop_runDependentTypesParser_simple :: String -> Property
prop_runDependentTypesParser_simple typeName =
  not (null typeName) && not (' ' `elem` typeName) ==>
  let input = "type " ++ typeName ++ " = struct { }"
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 1 && null errors

-- Property: parseDependentType handles basic type
prop_parseDependentType_basic :: String -> Property
prop_parseDependentType_basic typeName =
  not (null typeName) && not (' ' `elem` typeName) ==>
  let input = "type " ++ typeName ++ " = struct { }"
      result = parseDependentType input
  in case result of
    Left _ -> property False
    Right (depType, parser) -> dtName depType === typeName

-- Property: parseTypeDeclaration validates syntax
prop_parseTypeDeclaration_valid :: String -> String -> Property
prop_parseTypeDeclaration_valid typeName fieldName =
  not (null typeName) && not (null fieldName) &&
  not (' ' `elem` typeName) && not (' ' `elem` fieldName) ==>
  let input = "type " ++ typeName ++ " = struct { " ++ fieldName ++ ": int }"
      result = parseTypeDeclaration input
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: validateDependentTypeSyntax handles valid input
prop_validateDependentTypeSyntax_valid :: String -> Property
prop_validateDependentTypeSyntax_valid typeName =
  not (null typeName) && not (' ' `elem` typeName) ==>
  let input = "type " ++ typeName ++ " = struct { }"
      result = validateDependentTypeSyntax input
  in null result

-- Property: validateDependentTypeSyntax detects syntax errors
prop_validateDependentTypeSyntax_invalid :: Property
prop_validateDependentTypeSyntax_invalid =
  let input = "type = struct { }"  -- Missing type name
      result = validateDependentTypeSyntax input
  in not (null result)

-- Property: TypeRef with nested generics
prop_typeRef_nested_generics :: String -> String -> String -> Property
prop_typeRef_nested_generics outer inner innermost =
  not (null outer) && not (null inner) && not (null innermost) &&
  all (not . null) [outer, inner, innermost] ==>
  let innerRef = TypeRef inner [TypeRef innermost []]
      outerRef = TypeRef outer [innerRef]
      shown = show outerRef
  in outer `isInfixOf` shown && inner `isInfixOf` shown && innermost `isInfixOf` shown

-- Property: Field construction maintains invariants
prop_field_construction :: String -> String -> Property
prop_field_construction fieldName typeName =
  not (null fieldName) && not (null typeName) ==>
  let fieldType = TypeRef typeName []
      field = Field fieldName fieldType
  in fName field === fieldName && fType field === fieldType

-- Property: TypeParameter construction
prop_typeParameter_construction :: String -> String -> Property
prop_typeParameter_construction paramName constraint =
  not (null paramName) ==>
  let param = TypeParameter paramName constraint
  in tpName param === paramName && tpConstraint param === constraint

-- Property: TypeConstraint equality
prop_typeConstraint_equality :: String -> String -> Property
prop_typeConstraint_equality op value =
  not (null op) && not (null value) ==>
  let constraint1 = TypeConstraint op value
      constraint2 = TypeConstraint op value
      constraint3 = TypeConstraint (op ++ "X") value
  in constraint1 === constraint2 && constraint1 /= constraint3

-- Property: DependentType construction
prop_dependentType_construction :: String -> TypeBody -> [TypeConstraint] -> Property
prop_dependentType_construction typeName body constraints =
  not (null typeName) ==>
  let depType = DependentType typeName body constraints
  in dtName depType === typeName && dtBody depType === body && dtConstraints depType === constraints

-- Property: parseDependentType handles generic parameters
prop_parseDependentType_generic :: String -> String -> Property
prop_parseDependentType_generic typeName param =
  not (null typeName) && not (null param) &&
  not (' ' `elem` typeName) && not (' ' `elem` param) ==>
  let input = "type " ++ typeName ++ "<" ++ param ++ "> = struct { }"
      result = parseDependentType input
  in case result of
    Left _ -> property False
    Right (depType, parser) -> dtName depType === typeName

-- Property: parseDependentType handles where constraints
prop_parseDependentType_constraints :: String -> String -> String -> Property
prop_parseDependentType_constraints typeName field constraint =
  not (null typeName) && not (null field) && not (null constraint) &&
  all (not . null) [typeName, field, constraint] ==>
  let input = "type " ++ typeName ++ " = struct { " ++ field ++ ": int } where " ++ constraint
      result = parseDependentType input
  in case result of
    Left _ -> property False
    Right (depType, parser) -> dtName depType === typeName

-- Property: DependentTypeError contains meaningful information
prop_dependentTypeError_information :: String -> Property
prop_dependentTypeError_information errorMsg =
  not (null errorMsg) ==>
  let error = SyntaxError errorMsg 1 ""
      shown = show error
  in errorMsg `isInfixOf` shown

-- Property: multiple type definitions are parsed correctly
prop_multiple_type_definitions :: String -> String -> Property
prop_multiple_type_definitions type1 type2 =
  not (null type1) && not (null type2) && type1 /= type2 &&
  not (' ' `elem` type1) && not (' ' `elem` type2) ==>
  let input = "type " ++ type1 ++ " = struct { }\ntype " ++ type2 ++ " = struct { }"
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 2 && null errors

-- Property: type alias parsing
prop_type_alias_parsing :: String -> String -> Property
prop_type_alias_parsing aliasName originalType =
  not (null aliasName) && not (null originalType) &&
  not (' ' `elem` aliasName) && not (' ' `elem` originalType) ==>
  let input = "alias " ++ aliasName ++ " = " ++ originalType
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 1

-- Property: function type parsing
prop_function_type_parsing :: String -> String -> String -> Property
prop_function_type_parsing funcName paramType returnType =
  not (null funcName) && not (null paramType) && not (null returnType) &&
  all (not . null) [funcName, paramType, returnType] ==>
  let input = "func " ++ funcName ++ "(" ++ paramType ++ ") -> " ++ returnType
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 1

-- Property: complex struct with multiple fields
prop_complex_struct_fields :: String -> [String] -> Property
prop_complex_struct_fields structName fieldNames =
  not (null structName) && not (null fieldNames) && length fieldNames <= 3 ==>
  let fieldDefs = unwords $ map (\f -> f ++ ": int") fieldNames
      input = "type " ++ structName ++ " = struct { " ++ fieldDefs ++ " }"
      result = parseDependentType input
  in case result of
    Left _ -> property False
    Right (depType, parser) -> dtName depType === structName

-- Property: nested struct types
prop_nested_struct_types :: String -> String -> String -> Property
prop_nested_struct_types outerName innerName fieldName =
  not (null outerName) && not (null innerName) && not (null fieldName) &&
  all (not . null) [outerName, innerName, fieldName] ==>
  let input = "type " ++ innerName ++ " = struct { " ++ fieldName ++ ": int }\ntype " ++ outerName ++ " = struct { value: " ++ innerName ++ " }"
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 2

-- Property: constraint validation preserves semantics
prop_constraint_validation :: String -> String -> Property
prop_constraint_validation operator value =
  not (null operator) && not (null value) ==>
  let constraint = TypeConstraint operator value
      shown = show constraint
  in operator `isInfixOf` shown && value `isInfixOf` shown

-- Property: parser handles comments gracefully
prop_parser_handles_comments :: String -> String -> Property
prop_parser_handles_comments typeName comment =
  not (null typeName) && not (null comment) ==>
  let input = "-- " ++ comment ++ "\ntype " ++ typeName ++ " = struct { }"
      result = parseDependentType input
  in case result of
    Left _ -> property False
    Right (depType, parser) -> dtName depType === typeName

-- Property: parser recovers from syntax errors
prop_parser_error_recovery :: String -> String -> Property
prop_parser_error_recovery invalidType validType =
  not (null validType) && not (' ' `elem` validType) ==>
  let input = "type = struct { }\ntype " ++ validType ++ " = struct { }"
      result = runDependentTypesParser input
  in case result of
    Left _ -> property False
    Right (types, errors, parser) -> length types >= 1

tests :: TestTree
tests = testGroup "Dependent Types Validation Comprehensive QuickCheck tests"
  [ fastProperty "TypeRef construction maintains invariants" prop_typeRef_construction
  , fastProperty "TypeRef equality is structural" prop_typeRef_equality
  , fastProperty "TypeRef Show is readable" prop_typeRef_show
  , fastProperty "runDependentTypesParser handles empty input" prop_runDependentTypesParser_empty
  , fastProperty "runDependentTypesParser handles simple type definition" prop_runDependentTypesParser_simple
  , fastProperty "parseDependentType handles basic type" prop_parseDependentType_basic
  , fastProperty "parseTypeDeclaration validates syntax" prop_parseTypeDeclaration_valid
  , fastProperty "validateDependentTypeSyntax handles valid input" prop_validateDependentTypeSyntax_valid
  , fastProperty "validateDependentTypeSyntax detects syntax errors" prop_validateDependentTypeSyntax_invalid
  , fastProperty "TypeRef with nested generics" prop_typeRef_nested_generics
  , fastProperty "Field construction maintains invariants" prop_field_construction
  , fastProperty "TypeParameter construction" prop_typeParameter_construction
  , fastProperty "TypeConstraint equality" prop_typeConstraint_equality
  , fastProperty "DependentType construction" prop_dependentType_construction
  , fastProperty "parseDependentType handles generic parameters" prop_parseDependentType_generic
  , fastProperty "parseDependentType handles where constraints" prop_parseDependentType_constraints
  , fastProperty "DependentTypeError contains meaningful information" prop_dependentTypeError_information
  , fastProperty "multiple type definitions are parsed correctly" prop_multiple_type_definitions
  , fastProperty "type alias parsing" prop_type_alias_parsing
  , fastProperty "function type parsing" prop_function_type_parsing
  , fastProperty "complex struct with multiple fields" prop_complex_struct_fields
  , fastProperty "nested struct types" prop_nested_struct_types
  , fastProperty "constraint validation preserves semantics" prop_constraint_validation
  , fastProperty "parser handles comments gracefully" prop_parser_handles_comments
  , fastProperty "parser recovers from syntax errors" prop_parser_error_recovery
  ]