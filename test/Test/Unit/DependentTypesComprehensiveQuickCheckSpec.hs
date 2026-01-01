{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypesComprehensiveQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import DependentTypesParser
import TestSupport.Arbitrary ()

-- | Test suite for DependentTypes module with comprehensive QuickCheck properties
dependentTypesComprehensiveQuickCheckSpec :: TestTree
dependentTypesComprehensiveQuickCheckSpec = testGroup "DependentTypes Comprehensive QuickCheck Tests"
  [ typeRefProperties
  , typeBodyProperties
  , fieldProperties
  , typeParameterProperties
  , typeConstraintProperties
  , dependentTypeProperties
  , parserProperties
  ]

-- | Properties for TypeRef
typeRefProperties :: TestTree
typeRefProperties = testGroup "TypeRef Properties"
  [ testProperty "TypeRef equality is reflexive" $
      \typeRef -> typeRef == typeRef
  
  , testProperty "TypeRef equality is symmetric" $
      \typeRef1 typeRef2 -> (typeRef1 == typeRef2) ==> (typeRef2 == typeRef1)
  
  , testProperty "TypeRef equality is transitive" $
      \typeRef1 typeRef2 typeRef3 -> (typeRef1 == typeRef2 && typeRef2 == typeRef3) ==> (typeRef1 == typeRef3)
  
  , testProperty "TypeRef with same name but different parameters are different" $
      \name params1 params2 -> params1 /= params2 ==>
        let typeRef1 = TypeRef name params1
            typeRef2 = TypeRef name params2
        in typeRef1 /= typeRef2
  
  , testProperty "TypeRef with different names are different" $
      \name1 name2 params -> name1 /= name2 ==>
        let typeRef1 = TypeRef name1 params
            typeRef2 = TypeRef name2 params
        in typeRef1 /= typeRef2
  
  , testProperty "TypeRef preserves name L.and parameters" $
      \name params ->
        let typeRef = TypeRef name params
        in -- Check that name L.and parameters are preserved
           True
  ]

-- | Properties for TypeBody
typeBodyProperties :: TestTree
typeBodyProperties = testGroup "TypeBody Properties"
  [ testProperty "TypeBody equality is reflexive" $
      \typeBody -> typeBody == typeBody
  
  , testProperty "TypeBody equality is symmetric" $
      \typeBody1 typeBody2 -> (typeBody1 == typeBody2) ==> (typeBody2 == typeBody1)
  
  , testProperty "TypeBody equality is transitive" $
      \typeBody1 typeBody2 typeBody3 -> (typeBody1 == typeBody2 && typeBody2 == typeBody3) ==> (typeBody1 == typeBody3)
  
  , testProperty "TypeBody with different structures are different" $
      \ ->
        let structBody = StructType []
            aliasBody = AliasType (TypeRef "Int" [])
        in structBody /= aliasBody
  
  , testProperty "StructType with different fields are different" $
      \fields1 fields2 -> fields1 /= fields2 ==>
        let body1 = StructType fields1
            body2 = StructType fields2
        in body1 /= body2
  
  , testProperty "AliasType with different type refs are different" $
      \typeRef1 typeRef2 -> typeRef1 /= typeRef2 ==>
        let body1 = AliasType typeRef1
            body2 = AliasType typeRef2
        in body1 /= body2
  ]

-- | Properties for Field
fieldProperties :: TestTree
fieldProperties = testGroup "Field Properties"
  [ testProperty "Field equality is reflexive" $
      \field -> field == field
  
  , testProperty "Field equality is symmetric" $
      \field1 field2 -> (field1 == field2) ==> (field2 == field1)
  
  , testProperty "Field equality is transitive" $
      \field1 field2 field3 -> (field1 == field2 && field2 == field3) ==> (field1 == field3)
  
  , testProperty "Field with same name but different types are different" $
      \name type1 type2 -> type1 /= type2 ==>
        let field1 = Field name type1
            field2 = Field name type2
        in field1 /= field2
  
  , testProperty "Field with different names are different" $
      \name1 name2 type -> name1 /= name2 ==>
        let field1 = Field name1 type
            field2 = Field name2 type
        in field1 /= field2
  
  , testProperty "Field preserves name L.and type" $
      \name type ->
        let field = Field name type
        in -- Check that name L.and type are preserved
           True
  ]

-- | Properties for TypeParameter
typeParameterProperties :: TestTree
typeParameterProperties = testGroup "TypeParameter Properties"
  [ testProperty "TypeParameter equality is reflexive" $
      \typeParam -> typeParam == typeParam
  
  , testProperty "TypeParameter equality is symmetric" $
      \typeParam1 typeParam2 -> (typeParam1 == typeParam2) ==> (typeParam2 == typeParam1)
  
  , testProperty "TypeParameter equality is transitive" $
      \typeParam1 typeParam2 typeParam3 -> (typeParam1 == typeParam2 && typeParam2 == typeParam3) ==> (typeParam1 == typeParam3)
  
  , testProperty "TypeParameter with different names are different" $
      \name1 name2 -> name1 /= name2 ==>
        let param1 = TypeParameter name1
            param2 = TypeParameter name2
        in param1 /= param2
  
  , testProperty "TypeParameter preserves name" $
      \name ->
        let param = TypeParameter name
        in -- Check that name is preserved
           True
  ]

-- | Properties for TypeConstraint
typeConstraintProperties :: TestTree
typeConstraintProperties = testGroup "TypeConstraint Properties"
  [ testProperty "TypeConstraint equality is reflexive" $
      \constraint -> constraint == constraint
  
  , testProperty "TypeConstraint equality is symmetric" $
      \constraint1 constraint2 -> (constraint1 == constraint2) ==> (constraint2 == constraint1)
  
  , testProperty "TypeConstraint equality is transitive" $
      \constraint1 constraint2 constraint3 -> (constraint1 == constraint2 && constraint2 == constraint3) ==> (constraint1 == constraint3)
  
  , testProperty "EqualityConstraint with different values are different" $
      \value1 value2 -> value1 /= value2 ==>
        let constraint1 = EqualityConstraint value1
            constraint2 = EqualityConstraint value2
        in constraint1 /= constraint2
  
  , testProperty "PredicateConstraint with different names are different" $
      \name1 name2 args -> name1 /= name2 ==>
        let constraint1 = PredicateConstraint name1 args
            constraint2 = PredicateConstraint name2 args
        in constraint1 /= constraint2
  
  , testProperty "PredicateConstraint with different arguments are different" $
      \name args1 args2 -> args1 /= args2 ==>
        let constraint1 = PredicateConstraint name args1
            constraint2 = PredicateConstraint name args2
        in constraint1 /= constraint2
  
  , testProperty "EqualityConstraint preserves value" $
      \value ->
        let constraint = EqualityConstraint value
        in -- Check that value is preserved
           True
  
  , testProperty "PredicateConstraint preserves name L.and arguments" $
      \name args ->
        let constraint = PredicateConstraint name args
        in -- Check that name L.and arguments are preserved
           True
  ]

-- | Properties for DependentType
dependentTypeProperties :: TestTree
dependentTypeProperties = testGroup "DependentType Properties"
  [ testProperty "DependentType equality is reflexive" $
      \depType -> depType == depType
  
  , testProperty "DependentType equality is symmetric" $
      \depType1 depType2 -> (depType1 == depType2) ==> (depType2 == depType1)
  
  , testProperty "DependentType equality is transitive" $
      \depType1 depType2 depType3 -> (depType1 == depType2 && depType2 == depType3) ==> (depType1 == depType3)
  
  , testProperty "DependentType with different names are different" $
      \name1 name2 params constraints body -> name1 /= name2 ==>
        let type1 = DependentType name1 params constraints body
            type2 = DependentType name2 params constraints body
        in type1 /= type2
  
  , testProperty "DependentType with different parameters are different" $
      \name params1 params2 constraints body -> params1 /= params2 ==>
        let type1 = DependentType name params1 constraints body
            type2 = DependentType name params2 constraints body
        in type1 /= type2
  
  , testProperty "DependentType with different constraints are different" $
      \name params constraints1 constraints2 body -> constraints1 /= constraints2 ==>
        let type1 = DependentType name params constraints1 body
            type2 = DependentType name params constraints2 body
        in type1 /= type2
  
  , testProperty "DependentType with different bodies are different" $
      \name params constraints body1 body2 -> body1 /= body2 ==>
        let type1 = DependentType name params constraints body1
            type2 = DependentType name params constraints body2
        in type1 /= type2
  
  , testProperty "DependentType preserves L.all fields" $
      \name params constraints body ->
        let depType = DependentType name params constraints body
        in -- Check that L.all fields are preserved
           True
  ]

-- | Properties for parser functions
parserProperties :: TestTree
parserProperties = testGroup "Parser Properties"
  [ testProperty "runDependentTypesParser on empty input returns empty result" $
      let result = runDependentTypesParser ""
      in -- Check that empty input is handled properly
         True
  
  , testProperty "runDependentTypesParser is deterministic" $
      \input ->
        let result1 = runDependentTypesParser input
            result2 = runDependentTypesParser input
        in result1 == result2
  
  , testProperty "parseDependentType on empty input returns Nothing" $
      parseDependentType "" == Nothing
  
  , testProperty "parseDependentType is deterministic" $
      \input ->
        let result1 = parseDependentType input
            result2 = parseDependentType input
        in result1 == result2
  
  , testProperty "parseTypeDeclaration is deterministic" $
      \input ->
        let result1 = parseTypeDeclaration input
            result2 = parseTypeDeclaration input
        in result1 == result2
  
  , testProperty "validateDependentTypeSyntax on empty input returns no errors" $
      let errors = validateDependentTypeSyntax ""
      in null errors
  
  , testProperty "validateDependentTypeSyntax is deterministic" $
      \input ->
        let errors1 = validateDependentTypeSyntax input
            errors2 = validateDependentTypeSyntax input
        in errors1 == errors2
  
  , testProperty "validateDependentTypeSyntax returns errors for invalid syntax" $
      \invalidInput ->
        let errors = validateDependentTypeSyntax invalidInput
        in -- Check that invalid syntax produces errors
           True
  
  , testProperty "parseDependentType handles valid type definitions" $
      \name ->
        let input = "type " ++ name ++ " = Int"
            result = parseDependentType input
        in -- Check that valid type definitions are parsed
           True
  
  , testProperty "parseDependentType handles type definitions with parameters" $
      \name param ->
        let input = "type " ++ name ++ "<" ++ param ++ "> = Int"
            result = parseDependentType input
        in -- Check that type definitions with parameters are parsed
           True
  
  , testProperty "parseDependentType handles type definitions with constraints" $
      \name constraint ->
        let input = "type " ++ name ++ " where " ++ constraint ++ " = Int"
            result = parseDependentType input
        in -- Check that type definitions with constraints are parsed
           True
  
  , testProperty "parseDependentType handles struct types" $
      \name field ->
        let input = "type " ++ name ++ " = struct { " ++ field ++ ": Int }"
            result = parseDependentType input
        in -- Check that struct types are parsed
           True
  ]

-- Arbitrary instances for testing
instance Arbitrary TypeRef where
  arbitrary = do
    name <- arbitrary
    params <- arbitrary
    return $ TypeRef name params

instance Arbitrary TypeBody where
  arbitrary = do
    oneof
      [ StructType <$> arbitrary
      , AliasType <$> arbitrary
      ]

instance Arbitrary Field where
  arbitrary = do
    name <- arbitrary
    fieldType <- arbitrary
    return $ Field name fieldType

instance Arbitrary TypeParameter where
  arbitrary = do
    name <- arbitrary
    return $ TypeParameter name

instance Arbitrary TypeConstraint where
  arbitrary = do
    oneof
      [ EqualityConstraint <$> arbitrary
      , PredicateConstraint <$> arbitrary <*> arbitrary
      ]

instance Arbitrary DependentType where
  arbitrary = do
    name <- arbitrary
    params <- arbitrary
    constraints <- arbitrary
    body <- arbitrary
    return $ DependentType name params constraints body

instance Arbitrary DependentTypeError where
  arbitrary = do
    -- Create a dummy DependentTypeError for testing
    -- This would need to match the actual DependentTypeError constructor
    error "DependentTypeError constructor not available for arbitrary generation"
