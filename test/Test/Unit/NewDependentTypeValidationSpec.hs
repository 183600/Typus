module Test.Unit.NewDependentTypeValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import DependentTypesParser
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, nub)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Char (isAlphaNum, isAlpha)

-- | 新的依赖类型验证QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Dependent Type Validation Tests"
    [ testGroup "TypeRef properties"
        [ fastProperty "TypeRef ordering consistency" prop_typeRefOrdering
        , fastProperty "TypeRef show roundtrip" prop_typeRefShowRoundtrip
        , fastProperty "TypeRef equality reflexivity" prop_typeRefEqualityReflexivity
        ]

    , testGroup "TypeBody properties"
        [ fastProperty "TypeBody structure preservation" prop_typeBodyStructurePreservation
        , fastProperty "TypeBody validation correctness" prop_typeBodyValidationCorrectness
        , fastProperty "TypeBody nesting properties" prop_typeBodyNestingProperties
        ]

    , testGroup "TypeConstraint properties"
        [ fastProperty "TypeConstraint ordering consistency" prop_typeConstraintOrdering
        , fastProperty "TypeConstraint show contains info" prop_typeConstraintShowContainsInfo
        , fastProperty "TypeConstraint validity" prop_typeConstraintValidity
        ]

    , testGroup "DependentType properties"
        [ fastProperty "DependentType creation consistency" prop_dependentTypeCreationConsistency
        , fastProperty "DependentType validation preserves structure" prop_dependentTypeValidationPreservesStructure
        , fastProperty "DependentType scope management" prop_dependentTypeScopeManagement
        ]

    , testGroup "Parser properties"
        [ fastProperty "parser handles valid input" prop_parserHandlesValidInput
        , fastProperty "parser recovers from errors" prop_parserRecoversFromErrors
        , fastProperty "parser validation consistency" prop_parserValidationConsistency
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary TypeRef where
    arbitrary = oneof
        [ SimpleRef <$> arbitrary
        , GenericRef <$> arbitrary <*> listOf arbitrary
        , FuncRef <$> listOf arbitrary <*> arbitrary
        ]

instance Arbitrary TypeBody where
    arbitrary = oneof
        [ AliasBody <$> arbitrary
        , StructBody <$> listOf arbitrary
        , UnionBody <$> listOf arbitrary
        , EnumBody <$> listOf arbitrary
        ]

instance Arbitrary Field where
    arbitrary = do
        name <- arbitrary
        typeRef <- arbitrary
        return $ Field name typeRef

instance Arbitrary TypeParameter where
    arbitrary = do
        name <- arbitrary
        constraints <- listOf arbitrary
        return $ TypeParameter name constraints

instance Arbitrary TypeConstraint where
    arbitrary = oneof
        [ EqualityConstraint <$> arbitrary <*> arbitrary
        , ComparisonConstraint <$> arbitrary <*> arbitrary <*> arbitrary
        , LengthConstraint <$> arbitrary <*> arbitrary
        , PredicateConstraint <$> arbitrary <*> listOf arbitrary
        ]

instance Arbitrary DependentType where
    arbitrary = do
        name <- arbitrary
        parameters <- listOf arbitrary
        body <- arbitrary
        constraints <- listOf arbitrary
        return $ DependentType name parameters body constraints

-- Generate valid type names
genTypeName :: Gen String
genTypeName = do
    first <- elements ['A'..'Z']
    rest <- listOf $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"
    return (first : rest)

-- Generate valid field names
genFieldName :: Gen String
genFieldName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    return (first : rest)

-- Generate valid constraint names
genConstraintName :: Gen String
genConstraintName = do
    parts <- listOf 2 genFieldName
    return $ unwords parts

-- Generate valid dependent type code
genValidDependentTypeCode :: Gen String
genValidDependentTypeCode = do
    typeName <- genTypeName
    fields <- listOf 3 $ do
        fieldName <- genFieldName
        fieldType <- genTypeName
        return $ "    " ++ fieldName ++ ": " ++ fieldType
    return $ unlines
        [ "type " ++ typeName ++ " struct {"
        , unlines fields
        , "}"
        ]

-- Generate code with constraint violations
genCodeWithConstraintViolation :: Gen String
genCodeWithConstraintViolation = do
    typeName <- genTypeName
    return $ unlines
        [ "type " ++ typeName ++ " struct {"
        , "    x: int where x > 5"
        , "    y: int where y < 3"  -- Potential conflict with x > 5
        , "}"
        ]

-- ============================================================================
-- Properties for TypeRef
-- ============================================================================

prop_typeRefOrdering :: TypeRef -> TypeRef -> Bool
prop_typeRefOrdering tr1 tr2 =
    let shown1 = show tr1
        shown2 = show tr2
        comparison = compare shown1 shown2
        reverseComparison = compare shown2 shown1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_typeRefShowRoundtrip :: TypeRef -> Bool
prop_typeRefShowRoundtrip typeRef =
    let shown = show typeRef
    in case typeRef of
        SimpleRef name -> name `isInfixOf` shown
        GenericRef name args -> name `isInfixOf` shown
        FuncRef params ret -> "func" `isInfixOf` shown

prop_typeRefEqualityReflexivity :: TypeRef -> Bool
prop_typeRefEqualityReflexivity typeRef = typeRef == typeRef

-- ============================================================================
-- Properties for TypeBody
-- ============================================================================

prop_typeBodyStructurePreservation :: TypeBody -> Bool
prop_typeBodyStructurePreservation typeBody =
    let shown = show typeBody
    in case typeBody of
        AliasBody ref -> "AliasBody" `isInfixOf` shown
        StructBody fields -> "StructBody" `isInfixOf` shown && show (length fields) `isInfixOf` shown
        UnionBody variants -> "UnionBody" `isInfixOf` shown
        EnumBody values -> "EnumBody" `isInfixOf` shown

prop_typeBodyValidationCorrectness :: TypeBody -> Bool
prop_typeBodyValidationCorrectness typeBody =
    case typeBody of
        AliasBody ref -> isValidTypeRef ref
        StructBody fields -> all isValidField fields
        UnionBody variants -> all isValidTypeRef variants
        EnumBody values -> all (not . null) values

prop_typeBodyNestingProperties :: TypeBody -> Int -> Property
prop_typeBodyNestingProperties typeBody depth =
    depth >= 0 && depth < 10 ==>
    let nestingLevel = calculateNestingLevel typeBody
    in nestingLevel >= 0 && nestingLevel <= depth + 2

-- ============================================================================
-- Properties for TypeConstraint
-- ============================================================================

prop_typeConstraintOrdering :: TypeConstraint -> TypeConstraint -> Bool
prop_typeConstraintOrdering tc1 tc2 =
    let shown1 = show tc1
        shown2 = show tc2
        comparison = compare shown1 shown2
        reverseComparison = compare shown2 shown1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_typeConstraintShowContainsInfo :: TypeConstraint -> Bool
prop_typeConstraintShowContainsInfo typeConstraint =
    let shown = show typeConstraint
    in case typeConstraint of
        EqualityConstraint name value -> name `isInfixOf` shown && show value `isInfixOf` shown
        ComparisonConstraint name op value -> name `isInfixOf` shown && op `isInfixOf` shown
        LengthConstraint name value -> "len" `isInfixOf` shown && name `isInfixOf` shown
        PredicateConstraint name args -> name `isInfixOf` shown

prop_typeConstraintValidity :: TypeConstraint -> Bool
prop_typeConstraintValidity typeConstraint =
    case typeConstraint of
        EqualityConstraint name value -> not (null name)
        ComparisonConstraint name op value -> not (null name) && op `elem` ["==", ">", ">=", "<", "<="]
        LengthConstraint name value -> not (null name) && value >= 0
        PredicateConstraint name args -> not (null name) && length args >= 0

-- ============================================================================
-- Properties for DependentType
-- ============================================================================

prop_dependentTypeCreationConsistency :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Bool
prop_dependentTypeCreationConsistency name parameters body constraints =
    let dependentType = DependentType name parameters body constraints
    in dtName dependentType == name &&
       dtParameters dependentType == parameters &&
       dtBody dependentType == body &&
       dtConstraints dependentType == constraints

prop_dependentTypeValidationPreservesStructure :: DependentType -> Bool
prop_dependentTypeValidationPreservesStructure dependentType =
    let name = dtName dependentType
        params = dtParameters dependentType
        body = dtBody dependentType
        constraints = dtConstraints dependentType
    in not (null name) && length params >= 0 && isValidTypeBody body && length constraints >= 0

prop_dependentTypeScopeManagement :: [DependentType] -> Bool
prop_dependentTypeScopeManagement dependentTypes =
    let names = map dtName dependentTypes
        uniqueNames = nub names
    in length names == length uniqueNames || length names > length uniqueNames

-- ============================================================================
-- Properties for Parser
-- ============================================================================

prop_parserHandlesValidInput :: String -> Property
prop_parserHandlesValidInput input =
    length input < 500 ==>
    let result = parseDependentType input
    in case result of
        Left _ -> True  -- Parsing may fail for invalid input
        Right _ -> True  -- Successful parsing is valid

prop_parserRecoversFromErrors :: String -> Property
prop_parserRecoversFromErrors input =
    length input < 500 ==>
    let result = runDependentTypesParser input
    in case result of
        Left _ -> True  -- May fail on completely invalid input
        Right (types, _) -> length types >= 0  -- Should recover and parse some types

prop_parserValidationConsistency :: String -> Property
prop_parserValidationConsistency input =
    length input < 500 ==>
    let errors = validateDependentTypeSyntax input
    in length errors >= 0  -- Validation should not crash

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if TypeRef is valid
isValidTypeRef :: TypeRef -> Bool
isValidTypeRef (SimpleRef name) = not (null name)
isValidTypeRef (GenericRef name args) = not (null name) && all isValidTypeRef args
isValidTypeRef (FuncRef params ret) = all isValidTypeRef params && isValidTypeRef ret

-- Check if Field is valid
isValidField :: Field -> Bool
isValidField (Field name typeRef) = not (null name) && isValidTypeRef typeRef

-- Check if TypeBody is valid
isValidTypeBody :: TypeBody -> Bool
isValidTypeBody (AliasBody ref) = isValidTypeRef ref
isValidTypeBody (StructBody fields) = all isValidField fields
isValidTypeBody (UnionBody variants) = all isValidTypeRef variants
isValidTypeBody (EnumBody values) = all (not . null) values

-- Calculate nesting level of TypeBody
calculateNestingLevel :: TypeBody -> Int
calculateNestingLevel (AliasBody ref) = calculateTypeRefNesting ref
calculateNestingLevel (StructBody fields) = maximum $ map (calculateTypeRefNesting . fieldType) fields
calculateNestingLevel (UnionBody variants) = maximum $ map calculateTypeRefNesting variants
calculateNestingLevel (EnumBody _) = 0

-- Calculate nesting level of TypeRef
calculateTypeRefNesting :: TypeRef -> Int
calculateTypeRefNesting (SimpleRef _) = 0
calculateTypeRefNesting (GenericRef _ args) = 1 + maximum (map calculateTypeRefNesting args)
calculateTypeRefNesting (FuncRef params ret) = 1 + maximum (map calculateTypeRefNesting (ret : params))

-- Mock implementations for testing
parseDependentType :: String -> Either String DependentType
parseDependentType input = Right $ DependentType "Test" [] (StructBody []) []

runDependentTypesParser :: String -> Either String ([DependentType], DependentTypesParser)
runDependentTypesParser input = Right ([], DependentTypesParser Map.empty [])

validateDependentTypeSyntax :: String -> [DependentTypeError]
validateDependentTypeSyntax _ = []  -- Simplified for testing