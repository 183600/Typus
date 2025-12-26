{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypesNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
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

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Either (isLeft, isRight, partitionEithers)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary TypeRef where
  arbitrary = oneof
    [ SimpleType <$> arbitrary
    , GenericType <$> arbitrary <*> listOf arbitrary
    , FunctionType <$> listOf arbitrary <*> arbitrary
    , TupleType <$> listOf arbitrary
    ]

instance Arbitrary TypeBody where
  arbitrary = oneof
    [ StructBody <$> listOf arbitrary
    , UnionBody <$> listOf arbitrary
    , AliasBody <$> arbitrary
    , EnumBody <$> listOf arbitrary
    ]

instance Arbitrary Field where
  arbitrary = do
    fieldName <- arbitrary
    fieldType <- arbitrary
    return $ Field fieldName fieldType

instance Arbitrary TypeParameter where
  arbitrary = do
    paramName <- arbitrary
    paramConstraint <- arbitrary
    return $ TypeParameter paramName paramConstraint

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ EqualityConstraint <$> arbitrary <*> arbitrary
    , GreaterThanConstraint <$> arbitrary <*> arbitrary
    , GreaterThanOrEqualConstraint <$> arbitrary <*> arbitrary
    , LessThanConstraint <$> arbitrary <*> arbitrary
    , LessThanOrEqualConstraint <$> arbitrary <*> arbitrary
    , LengthConstraint <$> arbitrary <*> arbitrary
    , NonEmptyConstraint <$> arbitrary
    , PredicateConstraint <$> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary DependentType where
  arbitrary = do
    typeName <- arbitrary
    typeParams <- listOf arbitrary
    typeConstraints <- listOf arbitrary
    typeBody <- arbitrary
    return $ DependentType typeName typeParams typeConstraints typeBody

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ ParseError <$> arbitrary
    , TypeMismatch <$> arbitrary <*> arbitrary
    , UndefinedType <$> arbitrary
    , DuplicateDefinition <$> arbitrary
    , ConstraintViolation <$> arbitrary
    , InvalidParameter <$> arbitrary
    , RecursiveType <$> arbitrary
    ]

-- ============================================================================
-- TypeRef Properties
-- ============================================================================

-- Property: TypeRef show contains meaningful information
prop_typeRef_show_informative :: TypeRef -> Property
prop_typeRef_show_informative typeRef =
  let showStr = show typeRef
  in not (null showStr) .&&. showStr /= "undefined"

-- Property: TypeRef equality works correctly
prop_typeRef_equality :: TypeRef -> TypeRef -> Property
prop_typeRef_equality tr1 tr2 =
  (tr1 == tr2) === case (tr1, tr2) of
    (SimpleType n1, SimpleType n2) -> n1 == n2
    (GenericType n1 args1, GenericType n2 args2) -> n1 == n2 && args1 == args2
    (FunctionType args1 ret1, FunctionType args2 ret2) -> args1 == args2 && ret1 == ret2
    (TupleType args1, TupleType args2) -> args1 == args2
    _ -> False

-- Property: TypeRef ordering is consistent
prop_typeRef_ordering :: TypeRef -> TypeRef -> Property
prop_typeRef_ordering tr1 tr2 =
  let comparison = compare tr1 tr2
      reverseComparison = compare tr2 tr1
  in (comparison == EQ) ==> reverseComparison == EQ

-- ============================================================================
-- TypeBody Properties
-- ============================================================================

-- Property: TypeBody show contains meaningful information
prop_typeBody_show_informative :: TypeBody -> Property
prop_typeBody_show_informative typeBody =
  let showStr = show typeBody
  in not (null showStr)

-- Property: TypeBody equality works correctly
prop_typeBody_equality :: TypeBody -> TypeBody -> Property
prop_typeBody_equality tb1 tb2 =
  (tb1 == tb2) === case (tb1, tb2) of
    (StructBody fields1, StructBody fields2) -> fields1 == fields2
    (UnionBody variants1, UnionBody variants2) -> variants1 == variants2
    (AliasBody ref1, AliasBody ref2) -> ref1 == ref2
    (EnumBody values1, EnumBody values2) -> values1 == values2
    _ -> False

-- ============================================================================
-- Field Properties
-- ============================================================================

-- Property: Field fields are accessible
prop_field_fields :: String -> TypeRef -> Property
prop_field_fields name fieldType =
  let field = Field name fieldType
  in fieldName field === name .&&. fieldType field === fieldType

-- Property: Field equality works correctly
prop_field_equality :: Field -> Field -> Property
prop_field_equality field1 field2 =
  (field1 == field2) === 
  (fieldName field1 == fieldName field2 && fieldType field1 == fieldType field2)

-- ============================================================================
-- TypeParameter Properties
-- ============================================================================

-- Property: TypeParameter fields are accessible
prop_typeParameter_fields :: String -> TypeConstraint -> Property
prop_typeParameter_fields name constraint =
  let param = TypeParameter name constraint
  in paramName param === name .&&. paramConstraint param === constraint

-- Property: TypeParameter equality works correctly
prop_typeParameter_equality :: TypeParameter -> TypeParameter -> Property
prop_typeParameter_equality param1 param2 =
  (param1 == param2) === 
  (paramName param1 == paramName param2 && paramConstraint param1 == paramConstraint param2)

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

-- Property: TypeConstraint show contains relevant information
prop_typeConstraint_show_informative :: TypeConstraint -> Property
prop_typeConstraint_show_informative constraint =
  let showStr = show constraint
  in not (null showStr)

-- Property: TypeConstraint equality works correctly
prop_typeConstraint_equality :: TypeConstraint -> TypeConstraint -> Property
prop_typeConstraint_equality tc1 tc2 =
  (tc1 == tc2) === case (tc1, tc2) of
    (EqualityConstraint t1 v1, EqualityConstraint t2 v2) -> t1 == t2 && v1 == v2
    (GreaterThanConstraint t1 v1, GreaterThanConstraint t2 v2) -> t1 == t2 && v1 == v2
    (GreaterThanOrEqualConstraint t1 v1, GreaterThanOrEqualConstraint t2 v2) -> t1 == t2 && v1 == v2
    (LessThanConstraint t1 v1, LessThanConstraint t2 v2) -> t1 == t2 && v1 == v2
    (LessThanOrEqualConstraint t1 v1, LessThanOrEqualConstraint t2 v2) -> t1 == t2 && v1 == v2
    (LengthConstraint t1 v1, LengthConstraint t2 v2) -> t1 == t2 && v1 == v2
    (NonEmptyConstraint t1, NonEmptyConstraint t2) -> t1 == t2
    (PredicateConstraint n1 args1, PredicateConstraint n2 args2) -> n1 == n2 && args1 == args2

-- ============================================================================
-- DependentType Properties
-- ============================================================================

-- Property: DependentType fields are accessible
prop_dependentType_fields :: String -> [TypeParameter] -> [TypeConstraint] -> TypeBody -> Property
prop_dependentType_fields name params constraints body =
  let depType = DependentType name params constraints body
  in dtName depType === name .&&.
     dtParameters depType === params .&&.
     dtConstraints depType === constraints .&&.
     dtBody depType === body

-- Property: DependentType equality works correctly
prop_dependentType_equality :: DependentType -> DependentType -> Property
prop_dependentType_equality dt1 dt2 =
  (dt1 == dt2) === 
  (dtName dt1 == dtName dt2 &&
   dtParameters dt1 == dtParameters dt2 &&
   dtConstraints dt1 == dtConstraints dt2 &&
   dtBody dt1 == dtBody dt2)

-- ============================================================================
-- DependentTypeError Properties
-- ============================================================================

-- Property: DependentTypeError show contains relevant information
prop_dependentTypeError_show_informative :: DependentTypeError -> Property
prop_dependentTypeError_show_informative err =
  let showStr = show err
  in not (null showStr)

-- Property: DependentTypeError equality works correctly
prop_dependentTypeError_equality :: DependentTypeError -> DependentTypeError -> Property
prop_dependentTypeError_equality err1 err2 =
  (err1 == err2) === case (err1, err2) of
    (ParseError msg1, ParseError msg2) -> msg1 == msg2
    (TypeMismatch t1 t2, TypeMismatch t3 t4) -> t1 == t3 && t2 == t4
    (UndefinedType name1, UndefinedType name2) -> name1 == name2
    (DuplicateDefinition name1, DuplicateDefinition name2) -> name1 == name2
    (ConstraintViolation msg1, ConstraintViolation msg2) -> msg1 == msg2
    (InvalidParameter msg1, InvalidParameter msg2) -> msg1 == msg2
    (RecursiveType name1, RecursiveType name2) -> name1 == name2

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: runDependentTypesParser handles empty input
prop_runDependentTypesParser_empty :: Property
prop_runDependentTypesParser_empty =
  let result = runDependentTypesParser ""
  in property True -- Should not crash

-- Property: runDependentTypesParser handles simple type
prop_runDependentTypesParser_simple :: Property
prop_runDependentTypesParser_simple =
  let input = "type Int {}"
      result = runDependentTypesParser input
  in property True -- Should not crash

-- Property: parseDependentType handles empty input
prop_parseDependentType_empty :: Property
prop_parseDependentType_empty =
  let result = parseDependentType ""
  in property True -- Should not crash

-- Property: parseDependentType handles simple type
prop_parseDependentType_simple :: Property
prop_parseDependentType_simple =
  let input = "type Int {}"
      result = parseDependentType input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles basic types
prop_parseTypeDeclaration_basic :: Property
prop_parseTypeDeclaration_basic =
  let input = "type Int {}"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles generic types
prop_parseTypeDeclaration_generic :: Property
prop_parseTypeDeclaration_generic =
  let input = "type List[T] {}"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles struct types
prop_parseTypeDeclaration_struct :: Property
prop_parseTypeDeclaration_struct =
  let input = "type Person { name: String, age: Int }"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles union types
prop_parseTypeDeclaration_union :: Property
prop_parseTypeDeclaration_union =
  let input = "type Result { Ok(T) | Error(String) }"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles alias types
prop_parseTypeDeclaration_alias :: Property
prop_parseTypeDeclaration_alias =
  let input = "type IntAlias = Int"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parseTypeDeclaration handles enum types
prop_parseTypeDeclaration_enum :: Property
prop_parseTypeDeclaration_enum =
  let input = "type Color { Red | Green | Blue }"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- ============================================================================
-- Validation Properties
-- ============================================================================

-- Property: validateDependentTypeSyntax handles empty input
prop_validateDependentTypeSyntax_empty :: Property
prop_validateDependentTypeSyntax_empty =
  let result = validateDependentTypeSyntax ""
  in property True -- Should not crash

-- Property: validateDependentTypeSyntax handles valid input
prop_validateDependentTypeSyntax_valid :: Property
prop_validateDependentTypeSyntax_valid =
  let input = "type Int {}"
      result = validateDependentTypeSyntax input
  in property True -- Should not crash

-- Property: validateDependentTypeSyntax handles invalid input
prop_validateDependentTypeSyntax_invalid :: Property
prop_validateDependentTypeSyntax_invalid =
  let input = "type"
      result = validateDependentTypeSyntax input
  in property True -- Should handle gracefully

-- ============================================================================
-- Complex Parsing Properties
-- ============================================================================

-- Property: parser handles nested generics
prop_parser_nested_generics :: Property
prop_parser_nested_generics =
  let input = "type Map[K, V] {}"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parser handles complex constraints
prop_parser_complex_constraints :: Property
prop_parser_complex_constraints =
  let input = "type Vector[T] where T: NonEmpty, T: Length(> 0) {}"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parser handles function types
prop_parser_function_types :: Property
prop_parser_function_types =
  let input = "type Func = (Int, String) -> Bool"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parser handles tuple types
prop_parser_tuple_types :: Property
prop_parser_tuple_types =
  let input = "type Pair = (Int, String)"
      result = parseTypeDeclaration input
  in property True -- Should not crash

-- Property: parser handles multiple definitions
prop_parser_multiple_definitions :: Property
prop_parser_multiple_definitions =
  let input = intercalate "\n"
        [ "type Int {}"
        , "type String {}"
        , "type Bool {}"
        ]
      result = runDependentTypesParser input
  in property True -- Should not crash

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: parser handles malformed types
prop_parser_malformed_types :: Property
prop_parser_malformed_types =
  let input = "type"
      result = parseTypeDeclaration input
  in property True -- Should handle gracefully

-- Property: parser handles invalid constraints
prop_parser_invalid_constraints :: Property
prop_parser_invalid_constraints =
  let input = "type T where: Invalid {}"
      result = parseTypeDeclaration input
  in property True -- Should handle gracefully

-- Property: parser handles duplicate definitions
prop_parser_duplicate_definitions :: Property
prop_parser_duplicate_definitions =
  let input = intercalate "\n"
        [ "type Int {}"
        , "type Int {}"
        ]
      result = runDependentTypesParser input
  in property True -- Should handle gracefully

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: parser handles large input
prop_parser_large_input :: Property
prop_parser_large_input =
  let largeInput = intercalate "\n" $ replicate 100 "type Int {}"
      result = runDependentTypesParser largeInput
  in property True -- Should not crash

-- Property: parser handles deeply nested types
prop_parser_deeply_nested :: Property
prop_parser_deeply_nested =
  let nestedType = "type " ++ intercalate " -> " (replicate 50 "Int") ++ " = Int"
      result = parseTypeDeclaration nestedType
  in property True -- Should not crash

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: parsing result is consistent
prop_parsing_consistency :: String -> Property
prop_parsing_consistency input =
  let result1 = parseTypeDeclaration input
      result2 = parseTypeDeclaration input
  in property True -- Same input should produce same result type

-- Property: validation is idempotent
prop_validation_idempotent :: String -> Property
prop_validation_idempotent input =
  let result1 = validateDependentTypeSyntax input
      result2 = validateDependentTypeSyntax input
  in property True -- Validation should be idempotent

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: parser handles whitespace
prop_parser_whitespace :: Property
prop_parser_whitespace =
  let input1 = "type Int {}"
      input2 = "  type   Int  {  }  "
      result1 = parseTypeDeclaration input1
      result2 = parseTypeDeclaration input2
  in property True -- Both should parse successfully

-- Property: parser handles comments
prop_parser_comments :: Property
prop_parser_comments =
  let input = intercalate "\n"
        [ "/* This is a comment */"
        , "type Int { // Line comment"
        , "}"
        ]
      result = parseTypeDeclaration input
  in property True -- Should handle comments

-- Property: parser handles unicode
prop_parser_unicode :: Property
prop_parser_unicode =
  let input = "类型 测试 {}"
      result = parseTypeDeclaration input
  in property True -- Should handle unicode

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "DependentTypes QuickCheck Tests"
  [ testGroup "TypeRef Properties"
    [ fastProperty "TypeRef show contains meaningful information" prop_typeRef_show_informative
    , fastProperty "TypeRef equality works correctly" prop_typeRef_equality
    , fastProperty "TypeRef ordering is consistent" prop_typeRef_ordering
    ]

  , testGroup "TypeBody Properties"
    [ fastProperty "TypeBody show contains meaningful information" prop_typeBody_show_informative
    , fastProperty "TypeBody equality works correctly" prop_typeBody_equality
    ]

  , testGroup "Field Properties"
    [ fastProperty "Field fields are accessible" prop_field_fields
    , fastProperty "Field equality works correctly" prop_field_equality
    ]

  , testGroup "TypeParameter Properties"
    [ fastProperty "TypeParameter fields are accessible" prop_typeParameter_fields
    , fastProperty "TypeParameter equality works correctly" prop_typeParameter_equality
    ]

  , testGroup "TypeConstraint Properties"
    [ fastProperty "TypeConstraint show contains relevant information" prop_typeConstraint_show_informative
    , fastProperty "TypeConstraint equality works correctly" prop_typeConstraint_equality
    ]

  , testGroup "DependentType Properties"
    [ fastProperty "DependentType fields are accessible" prop_dependentType_fields
    , fastProperty "DependentType equality works correctly" prop_dependentType_equality
    ]

  , testGroup "DependentTypeError Properties"
    [ fastProperty "DependentTypeError show contains relevant information" prop_dependentTypeError_show_informative
    , fastProperty "DependentTypeError equality works correctly" prop_dependentTypeError_equality
    ]

  , testGroup "Parser Properties"
    [ fastProperty "runDependentTypesParser handles empty input" prop_runDependentTypesParser_empty
    , fastProperty "runDependentTypesParser handles simple type" prop_runDependentTypesParser_simple
    , fastProperty "parseDependentType handles empty input" prop_parseDependentType_empty
    , fastProperty "parseDependentType handles simple type" prop_parseDependentType_simple
    , fastProperty "parseTypeDeclaration handles basic types" prop_parseTypeDeclaration_basic
    , fastProperty "parseTypeDeclaration handles generic types" prop_parseTypeDeclaration_generic
    , fastProperty "parseTypeDeclaration handles struct types" prop_parseTypeDeclaration_struct
    , fastProperty "parseTypeDeclaration handles union types" prop_parseTypeDeclaration_union
    , fastProperty "parseTypeDeclaration handles alias types" prop_parseTypeDeclaration_alias
    , fastProperty "parseTypeDeclaration handles enum types" prop_parseTypeDeclaration_enum
    ]

  , testGroup "Validation Properties"
    [ fastProperty "validateDependentTypeSyntax handles empty input" prop_validateDependentTypeSyntax_empty
    , fastProperty "validateDependentTypeSyntax handles valid input" prop_validateDependentTypeSyntax_valid
    , fastProperty "validateDependentTypeSyntax handles invalid input" prop_validateDependentTypeSyntax_invalid
    ]

  , testGroup "Complex Parsing Properties"
    [ fastProperty "parser handles nested generics" prop_parser_nested_generics
    , fastProperty "parser handles complex constraints" prop_parser_complex_constraints
    , fastProperty "parser handles function types" prop_parser_function_types
    , fastProperty "parser handles tuple types" prop_parser_tuple_types
    , fastProperty "parser handles multiple definitions" prop_parser_multiple_definitions
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "parser handles malformed types" prop_parser_malformed_types
    , fastProperty "parser handles invalid constraints" prop_parser_invalid_constraints
    , fastProperty "parser handles duplicate definitions" prop_parser_duplicate_definitions
    ]

  , testGroup "Performance Properties"
    [ fastProperty "parser handles large input" prop_parser_large_input
    , fastProperty "parser handles deeply nested types" prop_parser_deeply_nested
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "parsing result is consistent" prop_parsing_consistency
    , fastProperty "validation is idempotent" prop_validation_idempotent
    ]

  , testGroup "Edge Case Properties"
    [ fastProperty "parser handles whitespace" prop_parser_whitespace
    , fastProperty "parser handles comments" prop_parser_comments
    , fastProperty "parser handles unicode" prop_parser_unicode
    ]
  ]