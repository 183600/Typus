{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import DependentTypesParser
  ( DependentTypesParser(..)
  , DependentTypeError(..)
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

-- | Generate valid identifiers for type names, field names, etc.
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate simple type references
genTypeRef :: Gen TypeRef
genTypeRef = do
  typeName <- genIdentifier
  typeParams <- listOf genTypeRef
  return $ TypeRef typeName typeParams

-- | Generate simple type bodies
genTypeBody :: Gen TypeBody
genTypeBody = oneof
  [ genStructBody
  , genUnionBody
  , genAliasBody
  ]

genStructBody :: Gen TypeBody
genStructBody = do
  numFields <- choose (1, 5)
  fields <- sequence [genField | _ <- [1..numFields]]
  return $ StructType fields

genUnionBody :: Gen TypeBody
genUnionBody = do
  numVariants <- choose (1, 5)
  variants <- sequence [genTypeRef | _ <- [1..numVariants]]
  return $ UnionType variants

genAliasBody :: Gen TypeBody
genAliasBody = do
  targetType <- genTypeRef
  return $ AliasType targetType

-- | Generate fields for struct types
genField :: Gen Field
genField = do
  fieldName <- genIdentifier
  fieldType <- genTypeRef
  return $ Field fieldName fieldType

-- | Generate type parameters
genTypeParameter :: Gen TypeParameter
genTypeParameter = do
  paramName <- genIdentifier
  paramType <- oneof [return Nothing, Just <$> genTypeRef]
  return $ TypeParameter paramName paramType

-- | Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ genEqualityConstraint
  , genComparisonConstraint
  , genLengthConstraint
  , genNonEmptyConstraint
  , genPredicateConstraint
  ]

genEqualityConstraint :: Gen TypeConstraint
genEqualityConstraint = do
  left <- genTypeRef
  right <- genTypeRef
  return $ EqualityConstraint left right

genComparisonConstraint :: Gen TypeConstraint
genComparisonConstraint = do
  left <- genTypeRef
  op <- elements ["==", ">", ">=", "<", "<="]
  right <- genTypeRef
  return $ ComparisonConstraint left op right

genLengthConstraint :: Gen TypeConstraint
genLengthConstraint = do
  targetType <- genTypeRef
  L.length <- choose (1, 100)
  return $ LengthConstraint targetType L.length

genNonEmptyConstraint :: Gen TypeConstraint
genNonEmptyConstraint = do
  targetType <- genTypeRef
  return $ NonEmptyConstraint targetType

genPredicateConstraint :: Gen TypeConstraint
genPredicateConstraint = do
  predicateName <- genIdentifier
  args <- listOf genTypeRef
  return $ PredicateConstraint predicateName args

-- | Generate dependent types
genDependentType :: Gen DependentType
genDependentType = do
  typeName <- genIdentifier
  typeParams <- listOf genTypeParameter
  constraints <- listOf genTypeConstraint
  typeBody <- genTypeBody
  return $ DependentType typeName typeParams constraints typeBody

-- | Generate simple type declarations
genTypeDeclaration :: Gen String
genTypeDeclaration = do
  typeName <- genIdentifier
  numFields <- choose (1, 3)
  fields <- sequence [do
    fieldName <- genIdentifier
    fieldType <- genIdentifier
    return $ fieldName ++ ": " ++ fieldType
    | _ <- [1..numFields]]
  let fieldsStr = unlines $ L.map ("  " ++) fields
  return $ "type " ++ typeName ++ " {\n" ++ fieldsStr ++ "}"

-- | Generate function declarations
genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genIdentifier
  paramName <- genIdentifier
  paramType <- genIdentifier
  returnType <- genIdentifier
  return $ "func " ++ funcName ++ "(" ++ paramName ++ ": " ++ paramType ++ ") " ++ returnType

-- | Test TypeRef equality
prop_typeRefEquality :: Property
prop_typeRefEquality = forAll genTypeRef $ \typeRef ->
  typeRef == typeRef

-- | Test TypeRef show property
prop_typeRefShow :: Property
prop_typeRefShow = forAll genTypeRef $ \typeRef ->
  let typeRefStr = show typeRef
  in not (null typeRefStr)

-- | Test Field properties
prop_fieldProperties :: Property
prop_fieldProperties = forAll genField $ \field ->
  let fieldName = fieldName field
      fieldType = fieldType field
  in not (null fieldName)

-- | Test Field equality
prop_fieldEquality :: Property
prop_fieldEquality = forAll genField $ \field1 ->
  forAll genField $ \field2 ->
    let sameName = fieldName field1 == fieldName field2
        sameType = fieldType field1 == fieldType field2
    in (field1 == field2) == (sameName && sameType)

-- | Test TypeParameter properties
prop_typeParameterProperties :: Property
prop_typeParameterProperties = forAll genTypeParameter $ \typeParam ->
  let paramName = tpName typeParam
  in not (null paramName)

-- | Test TypeConstraint equality
prop_typeConstraintEquality :: Property
prop_typeConstraintEquality = forAll genTypeConstraint $ \constraint ->
  constraint == constraint

-- | Test EqualityConstraint properties
prop_equalityConstraintProperties :: Property
prop_equalityConstraintProperties = forAll genTypeRef $ \left ->
  forAll genTypeRef $ \right ->
    let constraint = EqualityConstraint left right
    in show constraint == "EqualityConstraint " ++ show left ++ " " ++ show right

-- | Test ComparisonConstraint properties
prop_comparisonConstraintProperties :: Property
prop_comparisonConstraintProperties = forAll genTypeRef $ \left ->
  forAll (elements ["==", ">", ">=", "<", "<="]) $ \op ->
    forAll genTypeRef $ \right ->
      let constraint = ComparisonConstraint left op right
      in show constraint == "ComparisonConstraint " ++ show left ++ " " ++ op ++ " " ++ show right

-- | Test LengthConstraint properties
prop_lengthConstraintProperties :: Property
prop_lengthConstraintProperties = forAll genTypeRef $ \targetType ->
  forAll (choose (1, 100)) $ \L.length ->
    let constraint = LengthConstraint targetType L.length
    in show constraint == "LengthConstraint " ++ show targetType ++ " " ++ show L.length

-- | Test NonEmptyConstraint properties
prop_nonEmptyConstraintProperties :: Property
prop_nonEmptyConstraintProperties = forAll genTypeRef $ \targetType ->
  let constraint = NonEmptyConstraint targetType
  in show constraint == "NonEmptyConstraint " ++ show targetType

-- | Test PredicateConstraint properties
prop_predicateConstraintProperties :: Property
prop_predicateConstraintProperties = forAll genIdentifier $ \predicateName ->
  forAll (listOf genTypeRef) $ \args ->
    let constraint = PredicateConstraint predicateName args
    in show constraint == "PredicateConstraint " ++ predicateName ++ " " ++ show args

-- | Test TypeBody equality
prop_typeBodyEquality :: Property
prop_typeBodyEquality = forAll genTypeBody $ \typeBody ->
  typeBody == typeBody

-- | Test StructType properties
prop_structTypeProperties :: Property
prop_structTypeProperties = forAll genStructBody $ \structType ->
  case structType of
    StructType fields -> not (null fields)
    _ -> False

-- | Test UnionType properties
prop_unionTypeProperties :: Property
prop_unionTypeProperties = forAll genUnionBody $ \unionType ->
  case unionType of
    UnionType variants -> not (null variants)
    _ -> False

-- | Test AliasType properties
prop_aliasTypeProperties :: Property
prop_aliasTypeProperties = forAll genAliasBody $ \aliasType ->
  case aliasType of
    AliasType targetType -> True
    _ -> False

-- | Test DependentType properties
prop_dependentTypeProperties :: Property
prop_dependentTypeProperties = forAll genDependentType $ \depType ->
  let typeName = dtName depType
  in not (null typeName)

-- | Test DependentType equality
prop_dependentTypeEquality :: Property
prop_dependentTypeEquality = forAll genDependentType $ \type1 ->
  forAll genDependentType $ \type2 ->
    let sameName = dtName type1 == dtName type2
    in (type1 == type2) ==> sameName

-- | Test parsing simple type declaration
prop_parseSimpleTypeDeclaration :: Property
prop_parseSimpleTypeDeclaration = forAll genTypeDeclaration $ \typeDecl ->
  let result = parseTypeDeclaration typeDecl
  in case result of
    Left _ -> False
    Right _ -> True

-- | Test parsing empty input
prop_parseEmptyInput :: Property
prop_parseEmptyInput = 
  let result = parseDependentType ""
  in case result of
    Left _ -> True  -- Should fail on empty input
    Right _ -> False

-- | Test parsing with syntax validation
prop_validateSyntax :: Property
prop_validateSyntax = forAll genTypeDeclaration $ \typeDecl ->
  let errors = validateDependentTypeSyntax typeDecl
  in null errors  -- Should be valid for generated declarations

-- | Test runDependentTypesParser on simple input
prop_runDependentTypesParser :: Property
prop_runDependentTypesParser = forAll genTypeDeclaration $ \typeDecl ->
  let result = runDependentTypesParser typeDecl
  in case result of
    Left _ -> False
    Right (_, _) -> True

tests :: TestTree
tests = testGroup "Custom DependentTypes QuickCheck Tests"
  [ testProperty "TypeRef equality" prop_typeRefEquality
  , testProperty "TypeRef show" prop_typeRefShow
  , testProperty "Field properties" prop_fieldProperties
  , testProperty "Field equality" prop_fieldEquality
  , testProperty "TypeParameter properties" prop_typeParameterProperties
  , testProperty "TypeConstraint equality" prop_typeConstraintEquality
  , testProperty "EqualityConstraint properties" prop_equalityConstraintProperties
  , testProperty "ComparisonConstraint properties" prop_comparisonConstraintProperties
  , testProperty "LengthConstraint properties" prop_lengthConstraintProperties
  , testProperty "NonEmptyConstraint properties" prop_nonEmptyConstraintProperties
  , testProperty "PredicateConstraint properties" prop_predicateConstraintProperties
  , testProperty "TypeBody equality" prop_typeBodyEquality
  , testProperty "StructType properties" prop_structTypeProperties
  , testProperty "UnionType properties" prop_unionTypeProperties
  , testProperty "AliasType properties" prop_aliasTypeProperties
  , testProperty "DependentType properties" prop_dependentTypeProperties
  , testProperty "DependentType equality" prop_dependentTypeEquality
  , testProperty "parse simple type declaration" prop_parseSimpleTypeDeclaration
  , testProperty "parse empty input" prop_parseEmptyInput
  , testProperty "validate syntax" prop_validateSyntax
  , testProperty "run DependentTypesParser" prop_runDependentTypesParser
  ]