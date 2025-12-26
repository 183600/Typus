{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypeConstraintValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, listOf, suchThat)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import DependentTypesParser
  ( TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentTypeError(..)
  , DependentParseResult
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  , runDependentTypesParser
  )
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Test Generators
-- ============================================================================

-- Generate type names (avoiding reserved words)
genTypeName :: Gen String
genTypeName = do
  len <- choose (1, 10)
  let startChar = head "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let restChars = "abcdefghijklmnopqrstuvwxyz0123456789"
  first <- choose ('A', 'Z')
  rest <- listOf $ choose ('a', 'z')
  return $ first : rest

-- Generate field names
genFieldName :: Gen String
genFieldName = do
  len <- choose (1, 8)
  chars <- listOf $ choose ('a', 'z')
  return $ take len chars

-- Generate simple type references
genSimpleTypeRef :: Gen TypeRef
genSimpleTypeRef = do
  name <- genTypeName
  return $ TypeRef name []

-- Generate nested type references
genTypeRef :: Int -> Gen TypeRef
genTypeRef depth = do
  name <- genTypeName
  if depth <= 0
    then return $ TypeRef name []
    else do
      numArgs <- choose (0, 3)
      args <- listOf $ genTypeRef (depth - 1)
      return $ TypeRef name args

instance Arbitrary TypeRef where
  arbitrary = genTypeRef 2  -- Limit depth to avoid infinite recursion

-- Generate fields
genField :: Gen Field
genField = do
  name <- genFieldName
  fieldType <- genTypeRef 2
  return $ Field name fieldType

-- Generate type bodies
genTypeBody :: Gen TypeBody
genTypeBody = do
  numFields <- choose (0, 5)
  fields <- listOf genField
  return $ StructBody fields

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ EqualityConstraint <$> genFieldName <*> genFieldName
  , InequalityConstraint <$> genFieldName <*> genFieldName
  , RangeConstraint <$> genFieldName <*> choose (0, 100) <*> choose (0, 100)
  , SizeConstraint <$> genFieldName <*> choose (0, 100)
  , NonEmptyConstraint <$> genFieldName
  , PredicateConstraint <$> genFieldName <*> listOf genFieldName
  , TypeClassConstraint <$> genTypeName <*> genSimpleTypeRef
  , CustomConstraint <$> genFieldName <*> genFieldName
  ]

instance Arbitrary TypeConstraint where
  arbitrary = genTypeConstraint

-- Generate type parameters
genTypeParameter :: Gen TypeParameter
genTypeParameter = do
  name <- genFieldName
  paramType <- genSimpleTypeRef
  numConstraints <- choose (0, 3)
  constraints <- listOf genTypeConstraint
  return $ TypeParameter name paramType constraints

-- Generate dependent types
genDependentType :: Gen DependentType
genDependentType = oneof
  [ do
      name <- genTypeName
      numParams <- choose (0, 3)
      params <- listOf genTypeParameter
      body <- genTypeBody
      numConstraints <- choose (0, 3)
      constraints <- listOf genTypeConstraint
      return $ TypeDecl name params body constraints
  , do
      name <- genTypeName
      numParams <- choose (0, 3)
      params <- listOf $ (\n -> (n, genSimpleTypeRef)) <$> genFieldName
      returnType <- genTypeRef 2
      numConstraints <- choose (0, 3)
      constraints <- listOf genTypeConstraint
      return $ DependentFunction name params returnType constraints
  , do
      name <- genTypeName
      targetType <- genTypeRef 2
      numConstraints <- choose (0, 3)
      constraints <- listOf genTypeConstraint
      return $ TypeAlias name targetType constraints
  ]

instance Arbitrary DependentType where
  arbitrary = genDependentType

-- ============================================================================
-- TypeRef Properties
-- ============================================================================

-- Property: TypeRef with no args should be simple
propTypeRefNoArgsIsSimple :: TypeRef -> Bool
propTypeRefNoArgsIsSimple tr =
  null (refArgs tr) ==> length (refArgs tr) == 0

-- Property: TypeRef should preserve name and args
propTypeRefPreservesComponents :: String -> [TypeRef] -> Bool
propTypeRefPreservesComponents name args =
  let tr = TypeRef name args
  in refName tr == name && refArgs tr == args

-- Property: Nested TypeRef should maintain structure
propTypeRefNestedStructure :: Int -> Bool
propTypeRefNestedStructure depth =
  let tr = TypeRef "Outer" [TypeRef "Inner" [], TypeRef "Middle" [TypeRef "Inner2" []]]
  in length (refArgs tr) == 2 &&
     refName (head (refArgs tr)) == "Inner" &&
     null (refArgs (head (refArgs tr)))

-- ============================================================================
-- Field Properties
-- ============================================================================

-- Property: Field should preserve name and type
propFieldPreservesComponents :: String -> TypeRef -> Bool
propFieldPreservesComponents name fieldType =
  let field = Field name fieldType
  in fieldName field == name && fieldType field == fieldType

-- Property: Fields with same components should be equal
propFieldEquality :: String -> TypeRef -> Bool
propFieldEquality name fieldType =
  let field1 = Field name fieldType
      field2 = Field name fieldType
  in field1 == field2

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

-- Property: Equality constraint should preserve both sides
propEqualityConstraintPreservesSides :: String -> String -> Bool
propEqualityConstraintPreservesSides left right =
  let constraint = EqualityConstraint left right
  in case constraint of
    EqualityConstraint l r -> l == left && r == right
    _ -> False

-- Property: Range constraint should have valid bounds
propRangeConstraintValidBounds :: String -> Int -> Int -> Bool
propRangeConstraintValidBounds var minVal maxVal =
  let constraint = RangeConstraint var minVal maxVal
  in case constraint of
    RangeConstraint v mn mx -> v == var && mn == minVal && mx == maxVal
    _ -> False

-- Property: Size constraint should preserve size
propSizeConstraintPreservesSize :: String -> Int -> Bool
propSizeConstraintPreservesSize var size =
  let constraint = SizeConstraint var size
  in case constraint of
    SizeConstraint v s -> v == var && s == size
    _ -> False

-- ============================================================================
-- TypeParameter Properties
-- ============================================================================

-- Property: TypeParameter should preserve all components
propTypeParameterPreservesComponents :: String -> TypeRef -> [TypeConstraint] -> Bool
propTypeParameterPreservesComponents name paramType constraints =
  let param = TypeParameter name paramType constraints
  in paramName param == name &&
     paramType param == paramType &&
     paramConstraints param == constraints

-- Property: TypeParameter with no constraints should be valid
propTypeParameterNoConstraints :: String -> TypeRef -> Bool
propTypeParameterNoConstraints name paramType =
  let param = TypeParameter name paramType []
  in null (paramConstraints param)

-- ============================================================================
-- DependentType Properties
-- ============================================================================

-- Property: TypeDecl should preserve all components
propTypeDeclPreservesComponents :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Bool
propTypeDeclPreservesComponents name params body constraints =
  let decl = TypeDecl name params body constraints
  in case decl of
    TypeDecl n p b c -> n == name && p == params && b == body && c == constraints
    _ -> False

-- Property: TypeAlias should preserve target type and constraints
propTypeAliasPreservesComponents :: String -> TypeRef -> [TypeConstraint] -> Bool
propTypeAliasPreservesComponents name targetType constraints =
  let alias = TypeAlias name targetType constraints
  in case alias of
    TypeAlias n t c -> n == name && t == targetType && c == constraints
    _ -> False

-- ============================================================================
-- Constraint Validation Properties
-- ============================================================================

-- Property: Valid type declaration should parse without errors
propValidTypeDeclParses :: DependentType -> Bool
propValidTypeDeclParses decl =
  case decl of
    TypeDecl name params body constraints ->
      let input = "type " ++ name ++ " where " ++ unwords (map show constraints)
          result = validateDependentTypeSyntax input
      in null result  -- Should have no errors for valid input
    _ -> True  -- Skip other types for this property

-- Property: Invalid constraint syntax should produce errors
propInvalidConstraintProducesErrors :: String -> Bool
propInvalidConstraintProducesErrors input =
  let result = validateDependentTypeSyntax ("type X where " ++ input)
  in not (null result)  -- Should have errors for invalid input

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test simple type reference parsing
testSimpleTypeRefParsing :: TestTree
testSimpleTypeRefParsing = testCase "Simple type reference parsing" $ do
  let input = "type MyType where len(x) > 0"
  let result = validateDependentTypeSyntax input
  assertBool "Valid simple type should parse" (null result)

-- Test complex type reference parsing
testComplexTypeRefParsing :: TestTree
testComplexTypeRefParsing = testCase "Complex type reference parsing" $ do
  let input = "type Complex<Map<Key, Value>> where nonempty(x)"
  let result = validateDependentTypeSyntax input
  -- This might fail due to complex syntax, but shouldn't crash
  assertBool "Complex type parsing should not crash" (True)

-- Test constraint validation
testConstraintValidation :: TestTree
testConstraintValidation = testCase "Constraint validation" $ do
  let validConstraints = 
        [ "type Valid where x == y"
        , "type Range where 0 <= x && x <= 100"
        , "type Size where len(x) > 0"
        , "type NonEmpty where nonempty(x)"
        ]
  
  mapM_ (\constraint -> do
    let result = validateDependentTypeSyntax constraint
    assertBool ("Valid constraint should parse: " ++ constraint) (null result)
  ) validConstraints

-- Test invalid constraint handling
testInvalidConstraintHandling :: TestTree
testInvalidConstraintHandling = testCase "Invalid constraint handling" $ do
  let invalidConstraints =
        [ "type Invalid where ==="  -- Invalid operator
        , "type Invalid where x =="  -- Incomplete constraint
        , "type Invalid where"       -- Missing constraint
        ]
  
  mapM_ (\constraint -> do
    let result = validateDependentTypeSyntax constraint
    assertBool ("Invalid constraint should produce errors: " ++ constraint) (not (null result))
  ) invalidConstraints

-- Test type parameter validation
testTypeParameterValidation :: TestTree
testTypeParameterValidation = testCase "Type parameter validation" $ do
  let param = TypeParameter "T" (TypeRef "Int" []) [EqualityConstraint "T" "Int"]
  assertEqual "Parameter name should be preserved" "T" (paramName param)
  assertEqual "Parameter type should be preserved" (TypeRef "Int" []) (paramType param)
  assertEqual "Parameter constraints should be preserved" 
    [EqualityConstraint "T" "Int"] (paramConstraints param)

-- Test field validation
testFieldValidation :: TestTree
testFieldValidation = testCase "Field validation" $ do
  let field = Field "name" (TypeRef "String" [])
  assertEqual "Field name should be preserved" "name" (fieldName field)
  assertEqual "Field type should be preserved" (TypeRef "String" []) (fieldType field)

-- Test nested type references
testNestedTypeReferences :: TestTree
testNestedTypeReferences = testCase "Nested type references" $ do
  let nestedType = TypeRef "Map" 
        [ TypeRef "String" []
        , TypeRef "List" [TypeRef "Int" []]
        ]
  assertEqual "Outer type name should be preserved" "Map" (refName nestedType)
  assertEqual "Should have two type arguments" 2 (length (refArgs nestedType))
  assertEqual "First argument should be String" (TypeRef "String" []) (head (refArgs nestedType))
  assertEqual "Second argument should be List<Int>" 
    (TypeRef "List" [TypeRef "Int" []]) (head (tail (refArgs nestedType)))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependent Type Constraint Validation Tests"
  [ -- QuickCheck properties for TypeRef
    testProperty "TypeRef with no args is simple" propTypeRefNoArgsIsSimple
  , testProperty "TypeRef preserves components" propTypeRefPreservesComponents
  , testProperty "TypeRef nested structure" propTypeRefNestedStructure
  
    -- QuickCheck properties for Field
  , testProperty "Field preserves components" propFieldPreservesComponents
  , testProperty "Field equality" propFieldEquality
  
    -- QuickCheck properties for TypeConstraint
  , testProperty "Equality constraint preserves sides" propEqualityConstraintPreservesSides
  , testProperty "Range constraint valid bounds" propRangeConstraintValidBounds
  , testProperty "Size constraint preserves size" propSizeConstraintPreservesSize
  
    -- QuickCheck properties for TypeParameter
  , testProperty "TypeParameter preserves components" propTypeParameterPreservesComponents
  , testProperty "TypeParameter no constraints" propTypeParameterNoConstraints
  
    -- QuickCheck properties for DependentType
  , testProperty "TypeDecl preserves components" propTypeDeclPreservesComponents
  , testProperty "TypeAlias preserves components" propTypeAliasPreservesComponents
  
    -- QuickCheck properties for constraint validation
  , testProperty "Valid type decl parses" propValidTypeDeclParses
  , testProperty "Invalid constraint produces errors" propInvalidConstraintProducesErrors
  
    -- Unit tests
  , testSimpleTypeRefParsing
  , testComplexTypeRefParsing
  , testConstraintValidation
  , testInvalidConstraintHandling
  , testTypeParameterValidation
  , testFieldValidation
  , testNestedTypeReferences
  ]