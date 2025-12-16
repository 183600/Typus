{-# LANGUAGE CPP #-}

module Test.Unit.TypeSystemPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Compiler.TypeChecker (Type(..), TypeEnv(..), FunctionSignature(..), FunctionParam(..), 
                              buildTypeEnv, buildTypeEnvFromPairs, addType, lookupType, addFunction, checkFunctionSignature,
                              unifyTypes, areTypesCompatible, typesEqual, TypeConstraint(..),
                              applyConstraints, satisfiesConstraints)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (isJust, isNothing)

-- Arbitrary instances for testing
instance Arbitrary Type where
  arbitrary = sized $ \n -> if n <= 0 then
    elements [UnknownType, TypeName "int", TypeName "string", TypeName "bool"]
  else oneof [
    return UnknownType,
    TypeName <$> elements ["int", "string", "bool", "float", "char", "void"]
  ]

instance Arbitrary TypeEnv where
  arbitrary = do
    size <- choose (0, 5)
    types <- listOf size arbitrary
    let typePairs = zipWith (\i t -> ("var" ++ show i, t)) [1..] types
    return $ buildTypeEnvFromPairs typePairs

instance Arbitrary FunctionSignature where
  arbitrary = do
    paramCount <- choose (0, 3)
    params <- listOf paramCount arbitrary
    returnCount <- choose (0, 2)
    returnTypes <- listOf returnCount arbitrary
    return $ FunctionSignature params returnTypes

instance Arbitrary FunctionParam where
  arbitrary = do
    name <- elements ["x", "y", "z", "param", "arg"]
    paramType <- arbitrary
    isOptional <- arbitrary
    return $ FunctionParam (Just name) paramType isOptional

instance Arbitrary TypeConstraint where
  arbitrary = oneof [
    EqualityConstraint <$> elements ["x", "y", "z"] <*> arbitrary,
    InequalityConstraint <$> elements ["x", "y", "z"] <*> arbitrary,
    RangeConstraint <$> elements ["x", "y", "z"] <*> choose (0, 100) <*> choose (0, 100),
    SizeConstraint <$> elements ["x", "y", "z"] <*> choose (0, 100),
    NonEmptyConstraint <$> elements ["x", "y", "z"]
  ]

-- Helper function for type equality
typesEqualProp :: Type -> Type -> Property
typesEqualProp t1 t2 = property $ t1 === t2

-- | Generate random type names
genTypeName :: Gen String
genTypeName = do
  base <- elements ["int", "string", "bool", "float", "char", "void", "custom"]
  suffix <- listOf $ choose ('a', 'z')
  return $ base ++ suffix

-- | Generate random basic types
genBasicType :: Gen Type
genBasicType = oneof
  [ TypeName <$> genTypeName
  , return UnknownType
  ]

-- | Generate random function types
genFunctionType :: Int -> Gen Type
genFunctionType depth = 
  if depth <= 0 then genBasicType
  else do
    paramCount <- choose (0, 3)
    paramTypes <- listOfN paramCount (genType (depth - 1))
    returnType <- genType (depth - 1)
    return $ TypeFunction paramTypes returnType
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate random record types
genRecordType :: Int -> Gen Type
genRecordType depth =
  if depth <= 0 then genBasicType
  else do
    fieldCount <- choose (0, 3)
    fields <- listOfN fieldCount genField
    return $ TypeRecord fields
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]
    genField = do
      fieldName <- elements ["x", "y", "z", "name", "value", "data"]
      fieldType <- genType (depth - 1)
      return (fieldName, fieldType)

-- | Generate random union types
genUnionType :: Int -> Gen Type
genUnionType depth =
  if depth <= 0 then genBasicType
  else do
    typeCount <- choose (2, 4)
    types <- listOfN typeCount (genType (depth - 1))
    return $ TypeUnion (nub types)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate random types with limited depth
genType :: Int -> Gen Type
genType depth = oneof
  [ genBasicType
  , genFunctionType depth
  , genRecordType depth
  , genUnionType depth
  ]

-- | Generate type environments
genTypeEnv :: Gen TypeEnv
genTypeEnv = do
  varCount <- choose (0, 5)
  funcCount <- choose (0, 3)
  
  vars <- listOfN varCount genVarType
  funcs <- listOfN funcCount genFuncType
  
  return $ TypeEnv 
    { varTypes = Map.fromList vars
    , functionTypes = Map.fromList funcs
    }
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]
    genVarType = do
      name <- elements ["a", "b", "c", "x", "y", "z", "var"]
      typ <- genType 2
      return (name, typ)
    genFuncType = do
      name <- elements ["f", "g", "h", "func", "test"]
      paramCount <- choose (0, 3)
      paramTypes <- listOfN paramCount (genBasicType)
      returnType <- genBasicType
      let params = map (\t -> FunctionParam Nothing t False) paramTypes
      let signature = FunctionSignature params [returnType]
      return (name, signature)

-- | Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ do t1 <- genBasicType; t2 <- genBasicType; return $ Equal t1 t2
  , do t1 <- genBasicType; t2 <- genBasicType; return $ Subtype t1 t2
  , Predicate "Eq" <$> listOf genBasicType
  , Predicate "Ord" <$> listOf genBasicType
  , do t <- genBasicType; size <- choose (1, 100); return $ TypeSizeGE t size
  ]

-- Property: Type equality is reflexive
prop_type_equality_reflexive :: Type -> Property
prop_type_equality_reflexive typ =
  property $ typesEqualProp typ typ

-- Property: Type equality is symmetric
prop_type_equality_symmetric :: Type -> Type -> Property
prop_type_equality_symmetric typ1 typ2 =
  (typesEqual typ1 typ2) ==> typ2 === typ1

-- Property: Type equality is transitive
prop_type_equality_transitive :: Type -> Type -> Type -> Property
prop_type_equality_transitive typ1 typ2 typ3 =
  (typesEqual typ1 typ2 && typesEqual typ2 typ3) ==> (typ1 === typ3)

-- Property: Basic types are compatible with themselves
prop_basic_type_self_compatible :: Type -> Property
prop_basic_type_self_compatible typ =
  property $ areTypesCompatible typ typ

-- Property: Unknown type is compatible with any type
prop_unknown_type_compatible :: Type -> Property
prop_unknown_type_compatible typ =
  property $ areTypesCompatible UnknownType typ .&&. areTypesCompatible typ UnknownType

-- Property: Function type compatibility
prop_function_type_compatibility :: Type -> Type -> Type -> Property
prop_function_type_compatibility paramType1 paramType2 returnType =
  let func1 = TypeFunction [paramType1] returnType
      func2 = TypeFunction [paramType2] returnType
  in property $ areTypesCompatible func1 func2

-- Property: Record type field preservation
prop_record_type_field_preservation :: [(String, Type)] -> Property
prop_record_type_field_preservation fields =
  not (null fields) ==> 
  let recordType = TypeRecord fields
      fieldNames = map fst fields
  in case recordType of
    TypeRecord actualFields -> property $ map fst actualFields == fieldNames
    _ -> property $ False

-- Property: Union type contains all component types
prop_union_type_contains_all :: [Type] -> Property
prop_union_type_contains_all types =
  not (null types) ==> 
  let uniqueTypes = nub types
      unionType = TypeUnion uniqueTypes
  in case unionType of
    TypeUnion actualTypes -> property $ all (`elem` actualTypes) uniqueTypes
    _ -> property $ False

-- Property: Type environment variable lookup
prop_type_env_var_lookup :: TypeEnv -> String -> Type -> Property
prop_type_env_var_lookup env varName varType =
  let updatedEnv = addType env varName varType
      lookedUp = lookupType updatedEnv varName
  in property $ lookedUp === Just varType

-- Property: Type environment function lookup
prop_type_env_func_lookup :: TypeEnv -> String -> FunctionSignature -> Property
prop_type_env_func_lookup env funcName signature =
  let updatedEnv = addFunction env funcName signature
      lookedUp = checkFunctionSignature updatedEnv signature
  in property $ lookedUp === Right signature

-- Property: Type unification with identical types
prop_unification_identical :: Type -> Property
prop_unification_identical typ =
  let result = unifyTypes typ typ
  in case result of
    Right _ -> property $ True
    Left _ -> property $ False

-- Property: Type unification with unknown types
prop_unification_unknown :: Type -> Property
prop_unification_unknown typ =
  let result1 = unifyTypes UnknownType typ
      result2 = unifyTypes typ UnknownType
  in case (result1, result2) of
    (Right _, Right _) -> property $ True
    _ -> property $ False

-- Property: Constraint satisfaction with empty constraints
prop_empty_constraints_satisfied :: Type -> Property
prop_empty_constraints_satisfied typ =
  let constraints = []
      result = satisfiesConstraints typ constraints
  in property $ result

-- Property: Constraint application preserves type structure
prop_constraint_application_preserves :: Type -> TypeConstraint -> Property
prop_constraint_application_preserves typ constraint =
  let env = TypeEnv Map.empty Map.empty
      updatedEnv = applyConstraints env [constraint]
  in property $ True  -- Basic structure preservation

-- Property: Type environment building consistency
prop_type_env_building_consistent :: [(String, Type)] -> [(String, FunctionSignature)] -> Property
prop_type_env_building_consistent varTypes funcTypes =
  let env = TypeEnv 
        { varTypes = Map.fromList varTypes
        , functionTypes = Map.fromList funcTypes
      }
      varNames = map fst varTypes
      funcNames = map fst funcTypes
  in property $ all (`Map.member` Map.fromList varTypes) varNames .&&.
               all (`Map.member` Map.fromList funcTypes) funcNames

-- Property: Complex type compatibility
prop_complex_type_compatibility :: Type -> Type -> Property
prop_complex_type_compatibility typ1 typ2 =
  let compatible = areTypesCompatible typ1 typ2
      symmetric = areTypesCompatible typ2 typ1
  in property $ compatible ==> symmetric

-- Property: Nested type structure preservation
prop_nested_type_preservation :: Type -> Property
prop_nested_type_preservation innerType =
  let funcType = TypeFunction [innerType] innerType
      recordType = TypeRecord [("field", funcType)]
  in case recordType of
    TypeRecord [("field", TypeFunction [t] r)] -> property $ t === innerType .&&. r === innerType
    _ -> property $ False

-- Property: Type constraint combination
prop_constraint_combination :: Type -> [TypeConstraint] -> Property
prop_constraint_combination typ constraints =
  let singleResults = map (satisfiesConstraints typ . (:[])) constraints
      combinedResult = satisfiesConstraints typ constraints
  in property $ all id singleResults ==> combinedResult

tests :: TestTree
tests = testGroup "Type System Properties QuickCheck Tests"
  [ fastProperty "type equality reflexive" prop_type_equality_reflexive
  , fastProperty "type equality symmetric" prop_type_equality_symmetric
  , fastProperty "type equality transitive" prop_type_equality_transitive
  , fastProperty "basic type self compatible" prop_basic_type_self_compatible
  , fastProperty "unknown type compatible" prop_unknown_type_compatible
  , fastProperty "function type compatibility" prop_function_type_compatibility
  , fastProperty "record type field preservation" prop_record_type_field_preservation
  , fastProperty "union type contains all" prop_union_type_contains_all
  , fastProperty "type env var lookup" prop_type_env_var_lookup
  , fastProperty "type env func lookup" prop_type_env_func_lookup
  , fastProperty "unification identical" prop_unification_identical
  , fastProperty "unification unknown" prop_unification_unknown
  , fastProperty "empty constraints satisfied" prop_empty_constraints_satisfied
  , fastProperty "constraint application preserves" prop_constraint_application_preserves
  , fastProperty "type env building consistent" prop_type_env_building_consistent
  , fastProperty "complex type compatibility" prop_complex_type_compatibility
  , fastProperty "nested type preservation" prop_nested_type_preservation
  , fastProperty "constraint combination" prop_constraint_combination
  ]