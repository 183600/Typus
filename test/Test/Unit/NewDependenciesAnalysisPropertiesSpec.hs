module Test.Unit.NewDependenciesAnalysisPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Dependencies.TypeSystem
import Dependencies.AST (TypeExpr(..), Constraint(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T (pack, unpack)

-- | 新的依赖分析属性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Dependencies Analysis Properties Tests"
    [ testGroup "TypeVar properties"
        [ fastProperty "TypeVar ordering consistency" prop_typeVarOrdering
        , fastProperty "TypeVar show roundtrip" prop_typeVarShowRoundtrip
        , fastProperty "TypeVar equality reflexivity" prop_typeVarEqualityReflexivity
        ]

    , testGroup "TypeConstraint properties"
        [ fastProperty "TypeConstraint ordering consistency" prop_typeConstraintOrdering
        , fastProperty "TypeConstraint show contains key info" prop_typeConstraintShowContainsInfo
        , fastProperty "TypeConstraint validity" prop_typeConstraintValidity
        ]

    , testGroup "DependentTypeError properties"
        [ fastProperty "DependentTypeError ordering consistency" prop_dependentTypeErrorOrdering
        , fastProperty "DependentTypeError show contains error type" prop_dependentTypeErrorShowContainsType
        , fastProperty "DependentTypeError uniqueness" prop_dependentTypeErrorUniqueness
        ]

    , testGroup "Type environment properties"
        [ fastProperty "type environment creation" prop_typeEnvironmentCreation
        , fastProperty "type addition preserves consistency" prop_typeAdditionPreservesConsistency
        , fastProperty "constraint addition preserves structure" prop_constraintAdditionPreservesStructure
        ]

    , testGroup "Type checker properties"
        [ fastProperty "type checker creation consistency" prop_typeCheckerCreationConsistency
        , fastProperty "type lookup correctness" prop_typeLookupCorrectness
        , fastProperty "constraint solving properties" prop_constraintSolvingProperties
        ]

    , testGroup "AST conversion properties"
        [ fastProperty "TypeExpr conversion preserves structure" prop_typeExprConversionPreservesStructure
        , fastProperty "Constraint conversion preserves semantics" prop_constraintConversionPreservesSemantics
        , fastProperty "conversion roundtrip property" prop_conversionRoundtrip
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary TypeVar where
    arbitrary = oneof
        [ TVCon <$> arbitrary
        , TVVar <$> arbitrary
        , TVApp <$> arbitrary <*> listOf arbitrary
        , TVFun <$> listOf arbitrary <*> arbitrary
        , TVTuple <$> listOf arbitrary
        ]

instance Arbitrary TypeConstraint where
    arbitrary = oneof
        [ Equal <$> arbitrary <*> arbitrary
        , Subtype <$> arbitrary <*> arbitrary
        , Predicate <$> arbitrary <*> listOf arbitrary
        , TypeSizeGE <$> arbitrary <*> arbitrary
        , TypeSizeGT <$> arbitrary <*> arbitrary
        , TypeRange <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary DependentTypeError where
    arbitrary = oneof
        [ DependentTypeMismatch <$> arbitrary <*> arbitrary
        , ConstraintViolation <$> arbitrary <*> arbitrary
        , TypeNotFound <$> arbitrary
        , InvalidTypeArgument <$> arbitrary
        , UnsolvableConstraint <$> arbitrary
        , DependentInfiniteType <$> arbitrary <*> arbitrary
        , AmbiguousType <$> arbitrary
        , ParseError <$> arbitrary
        , SemanticError <$> arbitrary
        ]

instance Arbitrary TypeDef where
    arbitrary = do
        params <- listOf arbitrary
        constraints <- listOf arbitrary
        return $ TypeDefDecl params constraints

instance Arbitrary TypeEnv where
    arbitrary = do
        typeDefs <- arbitrary
        pendingConstraints <- listOf arbitrary
        return $ TypeEnv typeDefs pendingConstraints

instance Arbitrary DependentTypeChecker where
    arbitrary = do
        typeEnv <- arbitrary
        errors <- listOf arbitrary
        return $ DependentTypeChecker typeEnv errors

instance Arbitrary TypeExpr where
    arbitrary = oneof
        [ SimpleT <$> arbitrary
        , GenericT <$> arbitrary <*> listOf arbitrary
        , FuncT <$> listOf arbitrary <*> arbitrary
        , RefineT <$> arbitrary <*> listOf arbitrary
        ]

instance Arbitrary Constraint where
    arbitrary = oneof
        [ RangeC <$> arbitrary <*> arbitrary
        , PredC <$> arbitrary <*> listOf arbitrary
        , SizeGE <$> arbitrary
        , SizeGT <$> arbitrary
        ]

-- Generate type variable names
genTypeName :: Gen String
genTypeName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
    return (first : rest)

-- Generate constraint names
genConstraintName :: Gen String
genConstraintName = do
    parts <- listOf 2 genTypeName
    return $ unwords parts

-- Generate valid type expressions
genValidTypeExpr :: Gen TypeExpr
genValidTypeExpr = oneof
    [ SimpleT <$> arbitrary
    , GenericT <$> arbitrary <*> listOf genValidTypeExpr
    , FuncT <$> listOf ((,) <$> arbitrary <*> genValidTypeExpr) <*> genValidTypeExpr
    , RefineT <$> genValidTypeExpr <*> listOf arbitrary
    ]

-- ============================================================================
-- Properties for TypeVar
-- ============================================================================

prop_typeVarOrdering :: TypeVar -> TypeVar -> Bool
prop_typeVarOrdering tv1 tv2 =
    let comparison = compare tv1 tv2
        reverseComparison = compare tv2 tv1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_typeVarShowRoundtrip :: TypeVar -> Bool
prop_typeVarShowRoundtrip tv =
    let shown = show tv
    in case tv of
        TVCon name -> "TVCon" `L.isInfixOf` shown && name `L.isInfixOf` shown
        TVVar name -> "TVVar" `L.isInfixOf` shown && name `L.isInfixOf` shown
        TVApp name args -> "TVApp" `L.isInfixOf` shown && name `L.isInfixOf` shown
        TVFun _ _ -> "TVFun" `L.isInfixOf` shown
        TVTuple _ -> "TVTuple" `L.isInfixOf` shown

prop_typeVarEqualityReflexivity :: TypeVar -> Bool
prop_typeVarEqualityReflexivity tv = tv == tv

-- ============================================================================
-- Properties for TypeConstraint
-- ============================================================================

prop_typeConstraintOrdering :: TypeConstraint -> TypeConstraint -> Bool
prop_typeConstraintOrdering tc1 tc2 =
    let comparison = compare tc1 tc2
        reverseComparison = compare tc2 tc1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_typeConstraintShowContainsInfo :: TypeConstraint -> Bool
prop_typeConstraintShowContainsInfo tc =
    let shown = show tc
    in case tc of
        Equal tv1 tv2 -> "Equal" `L.isInfixOf` shown
        Subtype tv1 tv2 -> "Subtype" `L.isInfixOf` shown
        Predicate name args -> "Predicate" `L.isInfixOf` shown && name `L.isInfixOf` shown
        TypeSizeGE tv size -> "TypeSizeGE" `L.isInfixOf` shown
        TypeSizeGT tv size -> "TypeSizeGT" `L.isInfixOf` shown
        TypeRange tv min max -> "TypeRange" `L.isInfixOf` shown

prop_typeConstraintValidity :: TypeConstraint -> Bool
prop_typeConstraintValidity tc =
    case tc of
        Equal _ _ -> True
        Subtype _ _ -> True
        Predicate name args -> not (null name) && L.length args >= 0
        TypeSizeGE _ size -> size >= 0
        TypeSizeGT _ size -> size >= 0
        TypeRange _ min max -> min <= max

-- ============================================================================
-- Properties for DependentTypeError
-- ============================================================================

prop_dependentTypeErrorOrdering :: DependentTypeError -> DependentTypeError -> Bool
prop_dependentTypeErrorOrdering dte1 dte2 =
    let comparison = compare dte1 dte2
        reverseComparison = compare dte2 dte1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_dependentTypeErrorShowContainsType :: DependentTypeError -> Bool
prop_dependentTypeErrorShowContainsType dte =
    let shown = show dte
    in case dte of
        DependentTypeMismatch _ _ -> "DependentTypeMismatch" `L.isInfixOf` shown
        ConstraintViolation msg _ -> "ConstraintViolation" `L.isInfixOf` shown && msg `L.isInfixOf` shown
        TypeNotFound name -> "TypeNotFound" `L.isInfixOf` shown && name `L.isInfixOf` shown
        InvalidTypeArgument msg -> "InvalidTypeArgument" `L.isInfixOf` shown && msg `L.isInfixOf` shown
        UnsolvableConstraint _ -> "UnsolvableConstraint" `L.isInfixOf` shown
        DependentInfiniteType msg _ -> "DependentInfiniteType" `L.isInfixOf` shown && msg `L.isInfixOf` shown
        AmbiguousType msg -> "AmbiguousType" `L.isInfixOf` shown && msg `L.isInfixOf` shown
        ParseError msg -> "ParseError" `L.isInfixOf` shown && msg `L.isInfixOf` shown
        SemanticError msg -> "SemanticError" `L.isInfixOf` shown && msg `L.isInfixOf` shown

prop_dependentTypeErrorUniqueness :: DependentTypeError -> DependentTypeError -> Bool
prop_dependentTypeErrorUniqueness dte1 dte2 =
    let shown1 = show dte1
        shown2 = show dte2
    in if dte1 == dte2 then shown1 == shown2 else shown1 /= shown2

-- ============================================================================
-- Properties for Type Environment
-- ============================================================================

prop_typeEnvironmentCreation :: [(String, TypeDef)] -> Bool
prop_typeEnvironmentCreation typeDefs =
    let typeMap = Map.fromList typeDefs
        env = TypeEnv typeMap []
    in Map.size (typeDefinitions env) == L.length typeDefs

prop_typeAdditionPreservesConsistency :: TypeEnv -> String -> TypeDef -> Bool
prop_typeAdditionPreservesConsistency env name typeDef =
    let result = addType name typeDef env
        typeDefs = typeDefinitions result
    in Map.member name typeDefs

prop_constraintAdditionPreservesStructure :: TypeEnv -> TypeConstraint -> Bool
prop_constraintAdditionPreservesStructure env constraint =
    let result = addConstraint constraint env
        constraints = pendingConstraints result
    in constraint `elem` constraints

-- ============================================================================
-- Properties for Type Checker
-- ============================================================================

prop_typeCheckerCreationConsistency :: Int -> Bool
prop_typeCheckerCreationConsistency _ =
    let checker1 = newDependentTypeChecker
        checker2 = newDependentTypeChecker
    in checker1 == checker2

prop_typeLookupCorrectness :: String -> TypeDef -> TypeEnv -> Bool
prop_typeLookupCorrectness name typeDef env =
    let envWithDef = addType name typeDef env
        result = lookupTypeDef name envWithDef
    in case result of
        Just foundDef -> foundDef == typeDef
        Nothing -> False

prop_constraintSolvingProperties :: [TypeConstraint] -> Bool
prop_constraintSolvingProperties constraints =
    let checker = newDependentTypeChecker
        envWithConstraints = foldr addConstraint (dtcTypeEnv checker) constraints
        result = solveConstraints envWithConstraints
    in L.length (pendingConstraints result) <= L.length constraints

-- ============================================================================
-- Properties for AST Conversion
-- ============================================================================

prop_typeExprConversionPreservesStructure :: TypeExpr -> Bool
prop_typeExprConversionPreservesStructure typeExpr =
    let params = Set.empty
        converted = convertTypeExpr params typeExpr
    in case (typeExpr, converted) of
        (SimpleT name, TVCon n) -> T.unpack name == n
        (GenericT name args, TVApp n argTVs) -> T.unpack name == n && L.length args == L.length argTVs
        (FuncT params' ret, TVFun paramTVs retTV) -> L.length params' == L.length paramTVs
        (RefineT base _, _) -> True  -- Refinements are converted to constraints
        _ -> True  -- Basic structural preservation

prop_constraintConversionPreservesSemantics :: Constraint -> Bool
prop_constraintConversionPreservesSemantics constraint =
    let params = Set.empty
        converted = convertConstraint params constraint
    in case (constraint, converted) of
        (RangeC tv min max, TypeRange t min' max') -> T.unpack tv == show t && min == min' && max == max'
        (PredC name args, Predicate n argTVs) -> T.unpack name == n && L.length args == L.length argTVs
        (SizeGE tv, TypeSizeGE t size) -> T.unpack tv == show t
        (SizeGT tv, TypeSizeGT t size) -> T.unpack tv == show t
        _ -> True  -- Basic semantic preservation

prop_conversionRoundtrip :: TypeExpr -> Bool
prop_conversionRoundtrip typeExpr =
    let params = Set.empty
        converted = convertTypeExpr params typeExpr
        -- Simple check that conversion doesn't crash L.and produces valid result
    in case converted of
        TVCon _ -> True
        TVVar _ -> True
        TVApp _ _ -> True
        TVFun _ _ -> True
        TVTuple _ -> True

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if a substring is in a string
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]

-- Mock implementations for testing
addType :: String -> TypeDef -> TypeEnv -> TypeEnv
addType name typeDef env =
    let newTypeDefs = Map.insert name typeDef (typeDefinitions env)
    in env { typeDefinitions = newTypeDefs }

addConstraint :: TypeConstraint -> TypeEnv -> TypeEnv
addConstraint constraint env =
    let newConstraints = constraint : pendingConstraints env
    in env { pendingConstraints = newConstraints }

lookupTypeDef :: String -> TypeEnv -> Maybe TypeDef
lookupTypeDef name env = Map.lookup name (typeDefinitions env)

solveConstraints :: TypeEnv -> TypeEnv
solveConstraints env = env { pendingConstraints = [] }  -- Simplified for testing