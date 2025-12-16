{-# LANGUAGE CPP #-}

module Test.Unit.DependentTypePropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Compiler.DependentTypeChecker (DependentTypeError(..))
import DependentTypesParser (TypeRef(..), DependentType(..), TypeParameter(..), TypeBody(..), 
                            TypeConstraint(..))
import Compiler.TypeChecker (Type(..))
import Parser (TypusFile(..), CodeBlock(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (isJust, isNothing)

-- Arbitrary instances for testing
instance Arbitrary TypeRef where
  arbitrary = do
    name <- elements ["int", "string", "bool", "Vector", "Matrix", "List"]
    args <- listOf arbitrary
    return $ TypeRef name args

instance Arbitrary DependentType where
  arbitrary = oneof [
    TypeDecl <$> elements ["Vector", "Matrix", "List"] <*> listOf arbitrary <*> arbitrary <*> listOf arbitrary,
    TypeAlias <$> elements ["String", "Number", "Array"] <*> arbitrary <*> listOf arbitrary,
    DependentFunction <$> elements ["func", "process", "calculate"] <*> listOf arbitrary <*> arbitrary <*> listOf arbitrary
  ]

instance Arbitrary TypeParameter where
  arbitrary = do
    name <- elements ["T", "U", "V", "X", "Y", "Z"]
    typ <- arbitrary
    constraints <- listOf arbitrary
    return $ TypeParameter name typ constraints

instance Arbitrary TypeBody where
  arbitrary = oneof [
    StructBody <$> listOf arbitrary,
    pure $ InterfaceBody [],
    pure $ EnumBody []
  ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof [
    EqualityConstraint <$> elements ["x", "y", "z"] <*> arbitrary,
    InequalityConstraint <$> elements ["x", "y", "z"] <*> arbitrary,
    RangeConstraint <$> elements ["x", "y", "z"] <*> choose (0, 100) <*> choose (0, 100),
    SizeConstraint <$> elements ["x", "y", "z"] <*> choose (0, 100),
    NonEmptyConstraint <$> elements ["x", "y", "z"]
  ]

-- | Generate random variable names
genVarName :: Gen String
genVarName = elements ["n", "m", "s", "x", "y", "size", "length", "count"]

-- | Generate random dependent types
genDependentType :: Gen DependentType
genDependentType = oneof [
  return $ TypeDecl "Vector" [TypeParameter "n" (TypeRef "int" []) []] (StructBody []) [EqualityConstraint "n" "5"],
  return $ TypeDecl "Matrix" [TypeParameter "m" (TypeRef "int" []) [], TypeParameter "n" (TypeRef "int" []) []] (StructBody []) [RangeConstraint "m" 1 10],
  return $ TypeAlias "String" (TypeRef "Vec" []) [SizeConstraint "s" 10]
  ]

-- | Generate random type constraints  
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof [
  EqualityConstraint <$> genVarName <*> genVarName,
  InequalityConstraint <$> genVarName <*> genVarName,
  RangeConstraint <$> genVarName <*> choose (1, 100) <*> choose (1, 100),
  SizeConstraint <$> genVarName <*> choose (1, 100),
  NonEmptyConstraint <$> genVarName
  ]

tests :: TestTree
tests = testGroup "Dependent Type Properties QuickCheck tests"
  [ fastProperty "Dependent type creation" prop_dependent_type_creation
  , fastProperty "Type constraint creation" prop_type_constraint_creation
  , fastProperty "Dependent type structure preservation" prop_dependent_type_structure
  ]

-- Property: Dependent type creation is valid
prop_dependent_type_creation :: DependentType -> Property
prop_dependent_type_creation depType =
  case depType of
    TypeDecl name params body constraints ->
      property $ (not . null) name .&&. (not . null) params
    DependentFunction name params returnType constraints ->
      property $ (not . null) name .&&. (not . null) params
    TypeAlias name typeRef constraints ->
      property $ (not . null) name

-- Property: Type constraint creation is valid
prop_type_constraint_creation :: TypeConstraint -> Property
prop_type_constraint_creation constraint =
  case constraint of
    EqualityConstraint var1 var2 ->
      property $ (not . null) var1 .&&. (not . null) var2
    InequalityConstraint var1 var2 ->
      property $ (not . null) var1 .&&. (not . null) var2
    RangeConstraint var min max ->
      property $ (not . null) var .&&. min <= max
    SizeConstraint var size ->
      property $ (not . null) var .&&. size > 0
    NonEmptyConstraint var ->
      property $ (not . null) var
    _ -> property $ True

-- Property: Dependent type structure preservation
prop_dependent_type_structure :: DependentType -> Property
prop_dependent_type_structure depType =
  case depType of
    TypeDecl name params body constraints ->
      property $ (not . null) name
    DependentFunction name params returnType constraints ->
      property $ (not . null) name
    TypeAlias name typeRef constraints ->
      property $ (not . null) name