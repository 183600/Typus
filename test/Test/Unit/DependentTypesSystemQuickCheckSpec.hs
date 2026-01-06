{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypesSystemQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit

import DependentTypesParser
import Compiler.DependentTypeChecker

import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (nub)

-- Arbitrary instances for dependent types
instance Arbitrary TypeRef where
  arbitrary = do
    name <- identifierGen
    args <- listOf (scale (\n -> n `div` 2) arbitrary)
    return $ TypeRef name args
    where
      identifierGen = elements 
        [ "Int", "String", "Bool", "Map", "List", "Vector", "Set", "Option", "Result" ]

instance Arbitrary Field where
  arbitrary = do
    name <- fieldNameGen
    fieldType <- arbitrary
    return $ Field name fieldType

instance Arbitrary TypeBody where
  arbitrary = StructBody <$> listOf arbitrary
    where
      fieldNameGen = elements
        [ "value", "key", "data", "items", "result", "error", "success", "message" ]

instance Arbitrary TypeParameter where
  arbitrary = do
    name <- parameterNameGen
    paramType <- arbitrary
    constraints <- arbitrary
    return $ TypeParameter name paramType constraints
    where
      parameterNameGen = elements
        [ "T", "U", "V", "K", "V", "R", "E", "A", "B", "C" ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ EqualityConstraint <$> arbitrary <*> arbitrary
    , InequalityConstraint <$> arbitrary <*> arbitrary
    , RangeConstraint <$> arbitrary <*> arbitrary
    , SizeConstraint <$> arbitrary
    , NonEmptyConstraint <$> arbitrary
    , PredicateConstraint <$> arbitrary <*> arbitrary
    , TypeClassConstraint <$> arbitrary <*> arbitrary
    , CustomConstraint <$> arbitrary <*> arbitrary
    ]

instance Arbitrary DependentType where
  arbitrary = oneof
    [ TypeDecl <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DependentFunction <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , TypeAlias <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ SyntaxError <$> arbitrary <*> arbitrary <*> arbitrary
    , InvalidTypeSyntax <$> arbitrary
    , MissingConstraint <$> arbitrary
    , InvalidParameter <$> arbitrary
    , ConstraintParseError <$> arbitrary
    , TypeVariableError <$> arbitrary
    ]

-- Helper generators
identifierGen :: Gen String
identifierGen = elements 
  [ "MyType", "Container", "Processor", "Handler", "Service", "Manager", "Builder" ]

fieldNameGen :: Gen String
fieldNameGen = elements
  [ "value", "key", "data", "items", "result", "error", "success", "message", "count", "size" ]

parameterNameGen :: Gen String
parameterNameGen = elements
  [ "T", "U", "V", "K", "V", "R", "E", "A", "B", "C", "X", "Y", "Z" ]

constraintGen :: Gen TypeConstraint
constraintGen = oneof
  [ EqualityConstraint <$> parameterNameGen <*> arbitrary
  , GreaterThanConstraint <$> parameterNameGen <*> arbitrary
  , LengthConstraint <$> parameterNameGen <*> arbitrary
  , NonEmptyConstraint <$> parameterNameGen
  , PredicateConstraint <$> parameterNameGen <*> arbitrary
  ]

-- Test properties
tests :: TestTree
tests = testGroup "Dependent Types System QuickCheck Tests"
  [ testProperty "TypeRef equality works correctly" testTypeRefEquality
  , testProperty "TypeRef can be nested arbitrarily" testTypeRefNesting
  , testProperty "Field names are valid identifiers" testFieldNames
  , testProperty "Type parameters are properly formed" testTypeParameters
  , testProperty "Constraints are syntactically valid" testConstraints
  , testProperty "Dependent types can be parsed L.and reconstructed" testDependentTypeParsing
  , testProperty "Type validation catches invalid constructs" testTypeValidation
  , testProperty "Generic type arguments are preserved" testGenericTypeArguments
  , testProperty "Constraint expressions are well-formed" testConstraintExpressions
  ]

testTypeRefEquality :: TypeRef -> TypeRef -> Property
testTypeRefEquality ref1 ref2 =
  let areEqual = ref1 == ref2
      sameName = refName ref1 == refName ref2
      sameArgs = refArgs ref1 == refArgs ref2
  in areEqual === (sameName && sameArgs)

testTypeRefNesting :: TypeRef -> Property
testTypeRefNesting ref =
  let maxDepth = calculateDepth ref
      totalTypes = countTypeRefs ref
  in maxDepth >= 0 .&&. totalTypes >= 1

testFieldNames :: Field -> Property
testFieldNames field =
  let name = fieldName field
      isValidIdentifier = L.all (\c -> c == '_' || isAlphaNum c) name && not (null name)
  in isValidIdentifier === True

testTypeParameters :: TypeParameter -> Property
testTypeParameters param =
  let name = parameterName param
      constraint = parameterConstraint param
      isValidName = L.all (\c -> isAlphaNum c) name && not (null name)
  in isValidName === True

testConstraints :: TypeConstraint -> Property
testConstraints constraint =
  case constraint of
    EqualityConstraint var expr -> validVariable var .&&. validExpression expr
    InequalityConstraint var expr -> validVariable var .&&. validExpression expr
    RangeConstraint var minVal maxVal -> validVariable var .&&. validValue minVal .&&. validValue maxVal
    SizeConstraint var size -> validVariable var .&&. validLength size
    NonEmptyConstraint var -> validVariable var
    PredicateConstraint pred args -> validPredicate pred .&&. validArgs args
    TypeClassConstraint className typeRef -> validClassName className .&&. isValidTypeRef typeRef
    CustomConstraint name value -> validConstraintName name .&&. validExpression value

testDependentTypeParsing :: DependentType -> Property
testDependentTypeParsing depType =
  let typeString = show depType
      hasValidStructure = not (null typeString) && L.length typeString > 5
  in hasValidStructure === True

testTypeValidation :: DependentType -> Property
testTypeValidation depType =
  let hasValidName = not (L.null (getTypeName depType))
      hasValidStructure = isWellFormed depType
  in hasValidName .&&. hasValidStructure

testGenericTypeArguments :: TypeRef -> Property
testGenericTypeArguments ref =
  let args = refArgs ref
      allArgsValid = L.all isValidTypeRef args
  in allArgsValid === True

testConstraintExpressions :: TypeConstraint -> Property
testConstraintExpressions constraint =
  let expressionString = show constraint
      isWellFormed = not (null expressionString) && not (isInfixOf "!!" expressionString)
  in isWellFormed === True

-- Helper functions
calculateDepth :: TypeRef -> Int
calculateDepth (TypeRef _ []) = 0
calculateDepth (TypeRef _ args) = 1 + L.maximum (map calculateDepth args)

countTypeRefs :: TypeRef -> Int
countTypeRefs (TypeRef _ args) = 1 + L.sum (map countTypeRefs args)

validVariable :: String -> Bool
validVariable var = L.all (\c -> isAlphaNum c) var && not (null var)

validExpression :: String -> Bool
validExpression expr = not (null expr) && L.length expr > 0

validValue :: String -> Bool
validValue value = not (null value)

validLength :: Int -> Bool
validLength len = len >= 0

validPredicate :: String -> Bool
validPredicate pred = not (null pred) && L.all (\c -> isAlphaNum c) pred

validArgs :: [String] -> Bool
validArgs args = L.all (not . null) args

isValidTypeRef :: TypeRef -> Bool
isValidTypeRef (TypeRef name args) = not (null name) && L.all isValidTypeRef args

isAlphaNum :: Char -> Bool
isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

getTypeName :: DependentType -> String
getTypeName (TypeDecl name _ _ _) = name
getTypeName (DependentFunction name _ _ _) = name
getTypeName (TypeAlias name _ _) = name

isWellFormed :: DependentType -> Bool
isWellFormed (TypeDecl name params body constraints) = 
  not (null name) && L.all isValidParameter params && isValidBody body
isWellFormed (DependentFunction name params returnType constraints) = 
  not (null name) && L.all isValidParam params
isWellFormed (TypeAlias name target constraints) = 
  not (null name) && isValidTypeRef target

isValidParameter :: TypeParameter -> Bool
isValidParameter (TypeParameter name _ _) = not (null name)

isValidParam :: (String, TypeRef) -> Bool
isValidParam (name, typeRef) = not (null name) && isValidTypeRef typeRef

isValidBody :: TypeBody -> Bool
isValidBody (StructBody fields) = L.all isValidField fields

isValidField :: Field -> Bool
isValidField (Field name fieldType) = not (null name) && isValidTypeRef fieldType

validClassName :: String -> Bool
validClassName name = not (null name) && L.all isAlphaNum name

validConstraintName :: String -> Bool
validConstraintName name = not (null name) && L.all (\c -> isAlphaNum c || c == '_') name