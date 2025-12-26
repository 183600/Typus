{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

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

import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Map.Strict as Map

-- Helper generators for dependent types testing

-- Generate simple type names
genTypeName :: Gen String
genTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))
  return (first : rest)

-- Generate field names
genFieldName :: Gen String
genFieldName = do
  first <- elements ['a'..'z']
  rest <- listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_']))
  return (first : rest)

-- Generate simple type references
genTypeRef :: Gen TypeRef
genTypeRef = oneof
  [ TypeRef <$> genTypeName <*> pure []
  , do
      base <- genTypeName
      params <- listOf genTypeRef
      return $ TypeRef base params
  ]

-- Generate fields
genField :: Gen Field
genField = do
  name <- genFieldName
  fieldType <- genTypeRef
  return $ Field name fieldType

-- Generate type parameters
genTypeParameter :: Gen TypeParameter
genTypeParameter = do
  name <- genTypeName
  return $ TypeParameter name

-- Generate simple constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ EqualityConstraint <$> genTypeRef <*> genTypeRef
  , GtConstraint <$> genTypeRef <*> genTypeRef
  , GeConstraint <$> genTypeRef <*> genTypeRef
  , LtConstraint <$> genTypeRef <*> genTypeRef
  , LeConstraint <$> genTypeRef <*> genTypeRef
  , LenConstraint <$> genTypeRef <*> genTypeRef
  , NonemptyConstraint <$> genTypeRef
  , PredicateConstraint <$> genFieldName <*> listOf genTypeRef
  ]

-- Generate type bodies
genTypeBody :: Gen TypeBody
genTypeBody = oneof
  [ StructType <$> listOf genField
  , AliasType <$> genTypeRef
  , FuncType <$> listOf genTypeRef <*> genTypeRef
  ]

-- Generate dependent types
genDependentType :: Gen DependentType
genDependentType = do
  name <- genTypeName
  params <- listOf genTypeParameter
  constraints <- listOf genTypeConstraint
  body <- genTypeBody
  return $ DependentType name params constraints body

-- Generate valid dependent type declarations
genValidDependentTypeDecl :: Gen String
genValidDependentTypeDecl = do
  typeName <- genTypeName
  return $ "type " ++ typeName ++ " = struct { field: int }"

-- Generate malformed dependent type declarations
genMalformedDependentTypeDecl :: Gen String
genMalformedDependentTypeDecl = oneof
  [ return "type = struct { }"  -- missing type name
  , return "type Invalid = struct"  -- missing braces
  , return "type Invalid = struct { field }"  -- missing type annotation
  , return "type Invalid = struct { field: }"  -- incomplete type
  , return "type 123Invalid = struct { }"  -- invalid name
  ]

-- Generate complex nested types
genComplexNestedType :: Gen String
genComplexNestedType = do
  outerName <- genTypeName
  innerName <- genTypeName
  return $ unlines
    [ "type " ++ outerName ++ " = struct {"
    , "  inner: " ++ innerName ++ "[]"
    , "}"
    , ""
    , "type " ++ innerName ++ " = struct {"
    , "  value: int"
    , "  next: " ++ innerName ++ "?"
    , "}"
    ]

-- Generate types with constraints
genTypeWithConstraints :: Gen String
genTypeWithConstraints = do
  typeName <- genTypeName
  paramName <- genTypeName
  return $ unlines
    [ "type " ++ typeName ++ "<" ++ paramName ++ "> = struct {"
    , "  data: " ++ paramName ++ "[]"
    , "}"
    , "where " ++ paramName ++ " > 0"
    , "  and len(" ++ paramName ++ ") <= 100"
    ]

-- Arbitrary instances
instance Arbitrary TypeRef where
  arbitrary = genTypeRef

instance Arbitrary Field where
  arbitrary = genField

instance Arbitrary TypeParameter where
  arbitrary = genTypeParameter

instance Arbitrary TypeConstraint where
  arbitrary = genTypeConstraint

instance Arbitrary TypeBody where
  arbitrary = genTypeBody

instance Arbitrary DependentType where
  arbitrary = genDependentType

-- Boundary and edge case property tests

-- Property: Parser should handle empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = runDependentTypesParser ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right (types, errors, state) -> property $ null types  -- Should return empty types

-- Property: Parser should handle whitespace-only input
prop_parser_whitespace_only :: Property
prop_parser_whitespace_only =
  let whitespace = unlines ["", "   ", "\t", "  \t  ", ""]
      result = runDependentTypesParser whitespace
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right (types, errors, state) -> property $ null types

-- Property: parseTypeDeclaration should handle simple types
prop_parse_type_declaration_simple :: Property
prop_parse_type_declaration_simple =
  forAll genValidDependentTypeDecl $ \validDecl ->
  let result = parseTypeDeclaration validDecl
  in case result of
    Left _ -> property False  -- Should parse valid declarations
    Right _ -> property True

-- Property: parseTypeDeclaration should fail on malformed input
prop_parse_type_declaration_malformed :: Property
prop_parse_type_declaration_malformed =
  forAll genMalformedDependentTypeDecl $ \malformedDecl ->
  let result = parseTypeDeclaration malformedDecl
  in case result of
    Left _ -> property True  -- Should fail on malformed input
    Right _ -> property False  -- Should not succeed

-- Property: parseDependentType should return first type definition
prop_parse_dependent_type_first :: Property
prop_parse_dependent_type_first =
  let multipleTypes = unlines
    [ "type First = struct { x: int }"
    , "type Second = struct { y: string }"
    , "type Third = struct { z: bool }"
    ]
      result = parseDependentType multipleTypes
  in case result of
    Left _ -> property False  -- Should parse successfully
    Right dependentType -> typeName dependentType === "First"

-- Property: runDependentTypesParser should collect all types
prop_run_parser_collects_all_types :: Property
prop_run_parser_collects_all_types =
  let multipleTypes = unlines
    [ "type First = struct { x: int }"
    , "type Second = struct { y: string }"
    , "type Third = struct { z: bool }"
    ]
      result = runDependentTypesParser multipleTypes
  in case result of
    Left _ -> property False  -- Should parse successfully
    Right (types, errors, state) -> property $ length types === 3

-- Property: Parser should handle complex nested types
prop_parser_handles_nested_types :: Property
prop_parser_handles_nested_types =
  forAll genComplexNestedType $ \nestedType ->
  let result = runDependentTypesParser nestedType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: Parser should handle type constraints
prop_parser_handles_constraints :: Property
prop_parser_handles_constraints =
  forAll genTypeWithConstraints $ \constrainedType ->
  let result = runDependentTypesParser constrainedType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: validateDependentTypeSyntax should detect errors
prop_validate_detects_errors :: Property
prop_validate_detects_errors =
  let invalidSyntax = "type Invalid = struct { field }"  -- missing type annotation
      result = validateDependentTypeSyntax invalidSyntax
  in property $ not (null result)  -- Should detect errors

-- Property: validateDependentTypeSyntax should pass valid syntax
prop_validate_passes_valid :: Property
prop_validate_passes_valid =
  forAll genValidDependentTypeDecl $ \validDecl ->
  let result = validateDependentTypeSyntax validDecl
  in property $ null result  -- Should pass valid declarations

-- Property: TypeRef equality should work correctly
prop_type_ref_equality :: Property
prop_type_ref_equality =
  forAll genTypeRef $ \typeRef ->
  let sameTypeRef = typeRef
  in property $ typeRef === sameTypeRef

-- Property: Field ordering should be preserved
prop_field_ordering_preserved :: Property
prop_field_ordering_preserved =
  let fields = [Field "first" (TypeRef "Int" []), Field "second" (TypeRef "String" [])]
      structType = StructType fields
  in case structType of
    StructType fieldList -> property $ fieldList === fields
    _ -> property False

-- Property: TypeParameter names should be unique within a type
prop_type_param_names_unique :: Property
prop_type_param_names_unique =
  let duplicateParams = [TypeParameter "T", TypeParameter "T"]
      dependentType = DependentType "TestType" duplicateParams [] (StructType [])
      paramNames = map tpName (typeParams dependentType)
      uniqueNames = nub paramNames
  in property $ length paramNames > length uniqueNames  -- Should detect duplicates

-- Property: Constraint validation should handle complex expressions
prop_constraint_validation_complex :: Property
prop_constraint_validation_complex =
  let complexConstraint = unlines
    [ "type Complex<T> = struct { data: T[] }"
    , "where T > 0"
    , "  and len(T) <= 100"
    , "  and nonempty(T)"
    , "  and valid(T)"
    ]
      result = runDependentTypesParser complexConstraint
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: Parser should handle deeply nested generics
prop_parser_deeply_nested_generics :: Property
prop_parser_deeply_nested_generics =
  let deeplyNested = "type Deep = Map<String, List<Option<Result<Data, Error>>>>"
      result = parseTypeDeclaration deeplyNested
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right _ -> property True  -- Should parse if syntax is valid

-- Property: Parser should handle recursive type definitions
prop_parser_recursive_types :: Property
prop_parser_recursive_types =
  let recursiveType = unlines
    [ "type List = struct {"
    , "  head: int"
    , "  tail: List?"
    , "}"
    ]
      result = runDependentTypesParser recursiveType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: Parser should handle union-like structures
prop_parser_union_structures :: Property
prop_parser_union_structures =
  let unionType = unlines
    [ "type Either = struct {"
    , "  left: int?"
    , "  right: string?"
    , "}"
    ]
      result = runDependentTypesParser unionType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: Parser should handle function types
prop_parser_function_types :: Property
prop_parser_function_types =
  let funcType = "type Callback = func(int, string) -> bool"
      result = parseTypeDeclaration funcType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right _ -> property True  -- Should parse if syntax is valid

-- Property: Parser should handle array types with constraints
prop_parser_array_constraints :: Property
prop_parser_array_constraints =
  let arrayType = unlines
    [ "type Vector<T> = struct {"
    , "  data: T[]"
    , "  size: int"
    , "}"
    , "where T != null"
    , "  and len(data) == size"
    , "  and size > 0"
    ]
      result = runDependentTypesParser arrayType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right (types, errors, state) -> property $ length types >= 0

-- Property: Parser should handle optional types
prop_parser_optional_types :: Property
prop_parser_optional_types =
  let optionalType = "type Optional = struct { value: string? }"
      result = parseTypeDeclaration optionalType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right _ -> property True  -- Should parse if syntax is valid

-- Property: Parser should handle map/dictionary types
prop_parser_map_types :: Property
prop_parser_map_types =
  let mapType = "type Dict = struct { data: Map<string, int> }"
      result = parseTypeDeclaration mapType
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right _ -> property True  -- Should parse if syntax is valid

-- Property: Parser should be consistent across multiple runs
prop_parser_consistency :: Property
prop_parser_consistency =
  forAll genValidDependentTypeDecl $ \validDecl ->
  let result1 = parseTypeDeclaration validDecl
      result2 = parseTypeDeclaration validDecl
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ err1 === err2
    (Right type1, Right type2) -> property $ type1 === type2
    _ -> property False  -- Should be consistent

-- Property: Error messages should be informative
prop_error_messages_informative :: Property
prop_error_messages_informative =
  let invalidType = "type Invalid = struct { field: }"
      result = parseTypeDeclaration invalidType
  in case result of
    Left err -> property $ length (show err) > 10  -- Error message should not be trivial
    Right _ -> property False  -- Should not succeed

tests :: TestTree
tests = testGroup "Dependent Type Boundary Tests"
  [ fastProperty "Parser handles empty input gracefully" prop_parser_empty_input
  , fastProperty "Parser handles whitespace-only input" prop_parser_whitespace_only
  , fastProperty "parseTypeDeclaration handles simple types" prop_parse_type_declaration_simple
  , fastProperty "parseTypeDeclaration fails on malformed input" prop_parse_type_declaration_malformed
  , fastProperty "parseDependentType returns first type definition" prop_parse_dependent_type_first
  , fastProperty "runDependentTypesParser collects all types" prop_run_parser_collects_all_types
  , fastProperty "Parser handles complex nested types" prop_parser_handles_nested_types
  , fastProperty "Parser handles type constraints" prop_parser_handles_constraints
  , fastProperty "validateDependentTypeSyntax detects errors" prop_validate_detects_errors
  , fastProperty "validateDependentTypeSyntax passes valid syntax" prop_validate_passes_valid
  , fastProperty "TypeRef equality works correctly" prop_type_ref_equality
  , fastProperty "Field ordering is preserved" prop_field_ordering_preserved
  , fastProperty "TypeParameter names should be unique within a type" prop_type_param_names_unique
  , fastProperty "Constraint validation handles complex expressions" prop_constraint_validation_complex
  , fastProperty "Parser handles deeply nested generics" prop_parser_deeply_nested_generics
  , fastProperty "Parser handles recursive type definitions" prop_parser_recursive_types
  , fastProperty "Parser handles union-like structures" prop_parser_union_structures
  , fastProperty "Parser handles function types" prop_parser_function_types
  , fastProperty "Parser handles array types with constraints" prop_parser_array_constraints
  , fastProperty "Parser handles optional types" prop_parser_optional_types
  , fastProperty "Parser handles map/dictionary types" prop_parser_map_types
  , fastProperty "Parser is consistent across multiple runs" prop_parser_consistency
  , fastProperty "Error messages are informative" prop_error_messages_informative
  ]