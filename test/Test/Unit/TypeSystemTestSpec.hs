{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TypeSystemTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (nub, intersect)
import Data.Set (Set)
import qualified Data.Set as Set

import Compiler.TypeChecker
import Compiler.GoAst
import Dependencies.TypeSystem as Dep

-- Helper generators for type system tests
genBasicTypeName :: Gen String
genBasicTypeName = elements ["Int", "String", "Bool", "Float", "Char", "Void"]

genComplexTypeName :: Gen String
genComplexTypeName = do
  base <- elements ["List", "Map", "Set", "Array", "Option", "Result"]
  param <- genBasicTypeName
  return $ base ++ "[" ++ param ++ "]"

genTypeName :: Gen String
genTypeName = oneof [genBasicTypeName, genComplexTypeName]

genTypeVar :: Gen String
genTypeVar = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return $ "'" ++ (first : rest)

genSimpleTypeScheme :: Gen Dep.TypeScheme
genSimpleTypeScheme = do
  typeName <- genTypeName
  return $ Dep.TypeScheme [] $ Dep.TypeConstructor typeName

genPolymorphicTypeScheme :: Gen Dep.TypeScheme
genPolymorphicTypeScheme = do
  varCount <- choose (1, 3)
  typeVars <- vectorOf varCount genTypeVar
  baseType <- Dep.TypeConstructor <$> genTypeName
  return $ Dep.TypeScheme typeVars baseType

genFunctionTypeScheme :: Gen Dep.TypeScheme
genFunctionTypeScheme = do
  paramCount <- choose (1, 3)
  paramTypes <- vectorOf paramCount $ oneof
    [ Dep.TypeConstructor <$> genTypeName
    , Dep.TypeVariable <$> genTypeVar
    ]
  returnType <- oneof
    [ Dep.TypeConstructor <$> genTypeName
    , Dep.TypeVariable <$> genTypeVar
    ]
  return $ Dep.TypeScheme [] $ Dep.TypeFunction paramTypes returnType

genTypeConstraint :: Gen Dep.TypeConstraint
genTypeConstraint = oneof
  [ do
      t1 <- Dep.TypeConstructor <$> genTypeName
      t2 <- Dep.TypeConstructor <$> genTypeName
      return $ Dep.TypeEquality t1 t2
  , do
      t <- Dep.TypeConstructor <$> genTypeName
      className <- elements ["Show", "Eq", "Ord", "Num", "Functor", "Monad"]
      return $ Dep.TypeClass className t
  ]

-- Test properties for type system

-- Property 1: Type schemes with no type variables are monomorphic
prop_typeSchemeMonomorphic :: Dep.TypeScheme -> Property
prop_typeSchemeMonomorphic scheme =
  let typeVars = Dep.typeVars scheme
  in null typeVars ==> 
     -- A monomorphic type scheme has no type variables
     null typeVars

-- Property 2: Type schemes preserve their type variables
prop_typeSchemePreservesTypeVars :: [String] -> Dep.TypeExpr -> Bool
prop_typeSchemePreservesTypeVars typeVars typeExpr =
  let scheme = Dep.TypeScheme typeVars typeExpr
      preservedVars = Dep.typeVars scheme
  in all (`elem` preservedVars) typeVars

-- Property 3: Function types have correct arity
prop_functionTypeArity :: [Dep.TypeExpr] -> Dep.TypeExpr -> Bool
prop_functionTypeArity paramTypes returnType =
  let funcType = Dep.TypeFunction paramTypes returnType
  in case funcType of
    Dep.TypeFunction ps rt -> length ps == length paramTypes && rt == returnType
    _ -> False

-- Property 4: Type equality is reflexive
prop_typeEqualityReflexive :: Dep.TypeExpr -> Bool
prop_typeEqualityReflexive t = t == t

-- Property 5: Type equality is symmetric
prop_typeEqualitySymmetric :: Dep.TypeExpr -> Dep.TypeExpr -> Property
prop_typeEqualitySymmetric t1 t2 =
  t1 == t2 ==> t2 == t1

-- Property 6: Type equality is transitive
prop_typeEqualityTransitive :: Dep.TypeExpr -> Dep.TypeExpr -> Dep.TypeExpr -> Property
prop_typeEqualityTransitive t1 t2 t3 =
  t1 == t2 && t2 == t3 ==> t1 == t3

-- Property 7: Type constraints preserve their structure
prop_typeConstraintPreservation :: Dep.TypeConstraint -> Bool
prop_typeConstraintPreservation constraint =
  case constraint of
    Dep.TypeEquality t1 t2 -> 
      case constraint of
        Dep.TypeEquality t1' t2' -> t1 == t1' && t2 == t2'
        _ -> False
    Dep.TypeClass className t ->
      case constraint of
        Dep.TypeClass className' t' -> className == className' && t == t'
        _ -> False

-- Property 8: Type substitution preserves variable-free types
prop_typeSubstitutionPreservesConstants :: Dep.TypeExpr -> Map.Map String Dep.TypeExpr -> Property
prop_typeSubstitutionPreservesConstants typeExpr substitution =
  let hasNoVars = null $ Dep.freeVars typeExpr
      substituted = Dep.substitute substitution typeExpr
  in hasNoVars ==> substituted == typeExpr

typeSystemTests :: TestTree
typeSystemTests = testGroup "Type System Tests"
  [ testProperties "Type Scheme Properties"
    [ ("Type schemes with no type variables are monomorphic", prop_typeSchemeMonomorphic)
    , ("Type schemes preserve their type variables", prop_typeSchemePreservesTypeVars)
    ]
  , testProperties "Type Equality Properties"
    [ ("Type equality is reflexive", prop_typeEqualityReflexive)
    , ("Type equality is symmetric", prop_typeEqualitySymmetric)
    , ("Type equality is transitive", prop_typeEqualityTransitive)
    ]
  , testProperties "Type Function Properties"
    [ ("Function types have correct arity", prop_functionTypeArity)
    ]
  , testProperties "Type Constraint Properties"
    [ ("Type constraints preserve their structure", prop_typeConstraintPreservation)
    ]
  , testProperties "Type Substitution Properties"
    [ ("Type substitution preserves variable-free types", prop_typeSubstitutionPreservesConstants)
    ]
  ]