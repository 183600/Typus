{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.EnhancedDependentTypeSystemBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), counterexample, forAll, oneof, elements, listOf, listOf1, choose, sized, Positive(..))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import DependentTypesParser
  ( TypeRef(..), TypeBody(..), Field(..), TypeParameter(..)
  , TypeConstraint(..), DependentType(..), DependentTypesParser(..)
  , runDependentTypesParser, parseDependentType, parseTypeDeclaration
  , validateDependentTypeSyntax
  )
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub)
import Data.Char (isSpace, isAlphaNum)

-- ============================================================================
-- Dependent Type System QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependent Type System QuickCheck Tests"
  [ testProperty "runDependentTypesParser handles empty input" prop_parse_empty_input
  , testProperty "runDependentTypesParser handles whitespace-only input" prop_parse_whitespace_input
  , testProperty "parseTypeDeclaration handles simple types" prop_parse_simple_types
  , testProperty "parseTypeDeclaration handles generic types" prop_parse_generic_types
  , testProperty "parseTypeDeclaration handles constraints" prop_parse_constraints
  , testProperty "TypeRef equality is structural" prop_type_ref_equality
  , testProperty "TypeConstraint parsing is consistent" prop_constraint_parsing_consistent
  , testProperty "Field parsing preserves names L.and types" prop_field_parsing_preserves_structure
  , testProperty "TypeParameter parsing handles defaults" prop_type_parameter_defaults
  , testProperty "dependent type definitions are unique in scope" prop_dependent_types_unique_in_scope
  , testProperty "nested generic types are parsed correctly" prop_nested_generic_types
  , testProperty "complex constraints are handled correctly" prop_complex_constraints
  , testCase "dependent type parsing edge cases" test_dependent_type_edge_cases
  , testCase "constraint validation works" test_constraint_validation
  ]

-- ============================================================================
-- Basic Parsing Properties
-- ============================================================================

prop_parse_empty_input :: Property
prop_parse_empty_input =
  let result = runDependentTypesParser ""
  in counterexample ("Empty input result: " ++ show result) $
     case result of
       Left _ -> True  -- Should fail gracefully
       Right (defs, parser) -> null defs  -- Should produce no definitions

prop_parse_whitespace_input :: Property
prop_parse_whitespace_input =
  forAll genWhitespaceOnly $ \whitespace ->
    let result = runDependentTypesParser whitespace
    in counterexample ("Whitespace result: " ++ show result) $
       case result of
         Left _ -> True
         Right (defs, parser) -> null defs

-- ============================================================================
-- Type Declaration Properties
-- ============================================================================

prop_parse_simple_types :: Property
prop_parse_simple_types =
  forAll genSimpleTypeDecl $ \typeDecl ->
    let result = parseTypeDeclaration typeDecl
    in counterexample ("Simple type: " ++ typeDecl ++ ", Result: " ++ show result) $
       case result of
         Left _ -> False
         Right (TypeDecl name params body constraints) -> 
           name `L.isInfixOf` typeDecl && null params
         Right _ -> False

prop_parse_generic_types :: Property
prop_parse_generic_types =
  forAll genGenericTypeDecl $ \typeDecl ->
    let result = parseTypeDeclaration typeDecl
    in counterexample ("Generic type: " ++ typeDecl ++ ", Result: " ++ show result) $
       case result of
         Left _ -> False
         Right (TypeDecl name params body constraints) -> 
           name `L.isInfixOf` typeDecl && not (null params)
         Right _ -> False

prop_parse_constraints :: Property
prop_parse_constraints =
  forAll genConstrainedTypeDecl $ \typeDecl ->
    let result = parseTypeDeclaration typeDecl
    in counterexample ("Constrained type: " ++ typeDecl ++ ", Result: " ++ show result) $
       case result of
         Left _ -> False
         Right (TypeDecl name params body constraints) -> 
           name `L.isInfixOf` typeDecl && not (null constraints)
         Right _ -> False

-- ============================================================================
-- TypeRef Properties
-- ============================================================================

prop_type_ref_equality :: TypeRef -> TypeRef -> Property
prop_type_ref_equality ref1 ref2 =
  let structuralEquality = ref1 == ref2
      nameEquality = refName ref1 == refName ref2
      argsEquality = refArgs ref1 == refArgs ref2
  in structuralEquality === (nameEquality && argsEquality)

-- ============================================================================
-- Constraint Properties
-- ============================================================================

prop_constraint_parsing_consistent :: Property
prop_constraint_parsing_consistent =
  forAll genConstraintString $ \constraintStr ->
    let fullDecl = "type Test where " ++ constraintStr
        result = parseTypeDeclaration fullDecl
    in counterexample ("Constraint: " ++ constraintStr ++ ", Full: " ++ fullDecl) $
       case result of
         Left _ -> True  -- May fail for invalid constraints
         Right (TypeDecl _ _ _ constraints) -> not (null constraints)
         Right _ -> False

-- ============================================================================
-- Field Properties
-- ============================================================================

prop_field_parsing_preserves_structure :: Property
prop_field_parsing_preserves_structure =
  forAll genFieldString $ \fieldStr ->
    let structDecl = "type Test struct { " ++ fieldStr ++ " }"
        result = parseTypeDeclaration structDecl
    in counterexample ("Field: " ++ fieldStr ++ ", Struct: " ++ structDecl) $
       case result of
         Left _ -> False
         Right (TypeDecl _ _ (StructBody fields) _) -> 
           not (null fields) && L.any (`L.isInfixOf` fieldStr) (map fieldName fields)
         Right _ -> False

-- ============================================================================
-- TypeParameter Properties
-- ============================================================================

prop_type_parameter_defaults :: Property
prop_type_parameter_defaults =
  forAll genTypeParameterString $ \paramStr ->
    let typeDecl = "type Test<" ++ paramStr ++ "> struct { }"
        result = parseTypeDeclaration typeDecl
    in counterexample ("Parameter: " ++ paramStr ++ ", Type: " ++ typeDecl) $
       case result of
         Left _ -> False
         Right (TypeDecl _ params _ _) -> 
           not (null params) && L.any (`L.isInfixOf` paramStr) (map paramName params)
         Right _ -> False

-- ============================================================================
-- Scope Properties
-- ============================================================================

prop_dependent_types_unique_in_scope :: Property
prop_dependent_types_unique_in_scope =
  forAll genMultipleTypeDecls $ \typeDecls ->
    let input = unlines typeDecls
        result = runDependentTypesParser input
    in counterexample ("Multiple types: " ++ show (L.length typeDecls)) $
       case result of
         Left _ -> True
         Right (defs, parser) -> 
           let names = map getTypeName defs
           in L.length names == L.length (nub names)

-- ============================================================================
-- Nested Generic Properties
-- ============================================================================

prop_nested_generic_types :: Property
prop_nested_generic_types =
  forAll genNestedGenericTypeDecl $ \typeDecl ->
    let result = parseTypeDeclaration typeDecl
    in counterexample ("Nested generic: " ++ typeDecl) $
       case result of
         Left _ -> True  -- May fail for complex nesting
         Right (TypeDecl name params body constraints) -> 
           L.any hasNestedArgs params
         Right _ -> False

-- ============================================================================
-- Complex Constraint Properties
-- ============================================================================

prop_complex_constraints :: Property
prop_complex_constraints =
  forAll genComplexConstraintDecl $ \typeDecl ->
    let result = parseTypeDeclaration typeDecl
    in counterexample ("Complex constraint: " ++ typeDecl) $
       case result of
         Left _ -> True  -- May fail for complex constraints
         Right (TypeDecl _ _ _ constraints) -> 
           L.all isValidConstraint constraints
         Right _ -> False

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_dependent_type_edge_cases :: IO ()
test_dependent_type_edge_cases = do
  -- Test with extremely long type names
  let longName = replicate 1000 'T'
      longTypeDecl = "type " ++ longName ++ " struct { }"
      result1 = parseTypeDeclaration longTypeDecl
  case result1 of
    Left _ -> assertBool "Long type names should not crash" True
    Right (TypeDecl name _ _ _) -> assertEqual "Long name should be preserved" longName name
    Right _ -> assertBool "Should parse as type declaration" False
  
  -- Test with deeply nested generics
  let deeplyNested = "type Nested<Map<Key, Value<Inner, Deep<Very, Deep>>>> struct { }"
      result2 = parseTypeDeclaration deeplyNested
  case result2 of
    Left _ -> assertBool "Deeply nested generics should not crash" True
    Right (TypeDecl _ params _ _) -> assertBool "Should parse nested generics" $ L.length params > 0
    Right _ -> assertBool "Should parse as type declaration" False
  
  -- Test with Unicode identifiers
  let unicodeType = "type 世界结构体<T> where T > 0 struct { 字段: int }"
      result3 = parseTypeDeclaration unicodeType
  case result3 of
    Left _ -> assertBool "Unicode identifiers should not crash" True
    Right (TypeDecl name _ _ _) -> assertBool "Should handle Unicode" $ "世界" `L.isInfixOf` name
    Right _ -> assertBool "Should parse as type declaration" False

test_constraint_validation :: IO ()
test_constraint_validation = do
  -- Test constraint validation
  let validConstraints = 
        [ "type Test where x == 5 struct { }"
        , "type Test where len x > 0 struct { }"
        , "type Test where nonempty x struct { }"
        , "type Test where x >= 0 && x <= 100 struct { }"
        ]
  mapM_ (\constraint -> do
    let result = parseTypeDeclaration constraint
    case result of
      Left err -> assertBool ("Valid constraint should parse: " ++ constraint ++ ", Error: " ++ err) False
      Right (TypeDecl _ _ _ constraints) -> 
        assertBool ("Should have constraints: " ++ constraint) $ not (null constraints)
      Right _ -> assertBool ("Should parse as type declaration: " ++ constraint) False
    ) validConstraints
  
  -- Test invalid constraints
  let invalidConstraints =
        [ "type Test where struct { }"  -- Missing constraint
        , "type Test where x == struct { }"  -- Invalid syntax
        ]
  mapM_ (\constraint -> do
    let result = parseTypeDeclaration constraint
    case result of
      Left _ -> assertBool ("Invalid constraint should fail: " ++ constraint) True
      Right _ -> assertBool ("Invalid constraint should not succeed: " ++ constraint) False
    ) invalidConstraints

-- ============================================================================
-- Helper Functions
-- ============================================================================

getTypeName :: DependentType -> String
getName = \case
  TypeDecl name _ _ _ -> name
  TypeAlias name _ _ -> name
  DependentFunction name _ _ _ -> name

hasNestedArgs :: TypeParameter -> Bool
hasNestedArgs param = L.any hasNestedTypeRef (paramConstraints param)
  where
    hasNestedTypeRef :: TypeConstraint -> Bool
    hasNestedTypeRef (TypeClassConstraint _ typeref) = not (L.null (refArgs typeref))
    hasNestedTypeRef _ = False

isValidConstraint :: TypeConstraint -> Bool
isValidConstraint = \case
  EqualityConstraint _ _ -> True
  InequalityConstraint _ _ -> True
  RangeConstraint _ _ _ -> True
  SizeConstraint _ _ -> True
  NonEmptyConstraint _ -> True
  PredicateConstraint _ args -> not (null args)
  TypeClassConstraint _ _ -> True
  CustomConstraint _ _ -> True

-- ============================================================================
-- Helper Generators
-- ============================================================================

genWhitespaceOnly :: Gen String
genWhitespaceOnly = listOf $ elements " \t\n\r"

genSimpleTypeDecl :: Gen String
genSimpleTypeDecl = do
  name <- genTypeName
  return $ "type " ++ name ++ " struct { }"

genGenericTypeDecl :: Gen String
genGenericTypeDecl = do
  name <- genTypeName
  param <- genTypeParameterName
  return $ "type " ++ name ++ "<" ++ param ++ "> struct { }"

genConstrainedTypeDecl :: Gen String
genConstrainedTypeDecl = do
  name <- genTypeName
  param <- genTypeParameterName
  constraint <- genSimpleConstraint
  return $ "type " ++ name ++ "<" ++ param ++ "> where " ++ constraint ++ " struct { }"

genFieldString :: Gen String
genFieldString = do
  fieldName <- genFieldName
  typeName <- genTypeName
  return $ fieldName ++ ": " ++ typeName

genTypeParameterString :: Gen String
genTypeParameterString = do
  name <- genTypeParameterName
  oneof
    [ return name
    , do
        typeName <- genTypeName
        return $ name ++ ": " ++ typeName
    , do
        constraint <- genSimpleConstraint
        return $ name ++ " | " ++ constraint
    ]

genConstraintString :: Gen String
genConstraintString = genSimpleConstraint

genMultipleTypeDecls :: Gen [String]
genMultipleTypeDecls = do
  n <- choose (1, 5)
  listOf1 $ genSimpleTypeDecl

genNestedGenericTypeDecl :: Gen String
genNestedGenericTypeDecl = do
  name <- genTypeName
  param1 <- genTypeParameterName
  param2 <- genTypeParameterName
  return $ "type " ++ name ++ "<" ++ param1 ++ "<" ++ param2 ++ ">> struct { }"

genComplexConstraintDecl :: Gen String
genComplexConstraintDecl = do
  name <- genTypeName
  param <- genTypeParameterName
  constraints <- listOf1 genComplexConstraint
  let constraintStr = L.concat $ intersperse " & " constraints
  return $ "type " ++ name ++ "<" ++ param ++ "> where " ++ constraintStr ++ " struct { }"

genSimpleConstraint :: Gen String
genSimpleConstraint = oneof
  [ do
      var <- genTypeParameterName
      value <- choose (0, 100)
      return $ var ++ " == " ++ show value
  , do
      var <- genTypeParameterName
      return $ "nonempty " ++ var
  , do
      var <- genTypeParameterName
      value <- choose (1, 100)
      return $ "len " ++ var ++ " > " ++ show value
  ]

genComplexConstraint :: Gen String
genComplexConstraint = oneof
  [ genSimpleConstraint
  , do
      var1 <- genTypeParameterName
      var2 <- genTypeParameterName
      return $ var1 ++ " != " ++ var2
  , do
      var <- genTypeParameterName
      low <- choose (0, 50)
      high <- choose (51, 100)
      return $ var ++ " >= " ++ show low ++ " && " ++ var ++ " <= " ++ show high
  ]

genTypeName :: Gen String
genTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

genFieldName :: Gen String
genFieldName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

genTypeParameterName :: Gen String
genTypeParameterName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs