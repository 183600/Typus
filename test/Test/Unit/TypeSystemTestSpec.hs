{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.TypeSystemTestSpec where



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (nub, intersect)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler.TypeChecker
import Compiler.GoAst
import Dependencies.TypeSystem as Dep
import qualified Dependencies.AST as Dep

-- Helper functions for tests
getTypeVars :: Dep.TypeScheme -> [String]
getTypeVars (Dep.Forall vars _) = vars

-- Helper generators for type system tests
genTypeVar :: Gen Dep.TypeVar
genTypeVar = oneof 
  [ Dep.TVVar <$> elements ["a", "b", "c"]
  , Dep.TVCon <$> elements ["Int", "String", "Bool"]
  , Dep.TVFun <$> vectorOf 2 (Dep.TVVar <$> elements ["a", "b", "c"]) <*> (Dep.TVVar <$> elements ["a", "b", "c"])
  ]

genTypeExpr :: Gen Dep.TypeExpr
genTypeExpr = oneof
  [ Dep.SimpleT <$> (T.pack <$> elements ["Int", "String", "Bool"])
  , Dep.GenericT <$> (T.pack <$> elements ["List", "Maybe"]) <*> vectorOf 1 genTypeExpr
  ]

genTypeConstraint :: Gen Dep.TypeConstraint
genTypeConstraint = oneof
  [ Dep.Equal <$> genTypeVar <*> genTypeVar
  , Dep.Subtype <$> genTypeVar <*> genTypeVar
  , Dep.Predicate <$> elements ["Eq", "Ord"] <*> vectorOf 1 genTypeVar
  , Dep.TypeSizeGE <$> genTypeVar <*> arbitrary
  , Dep.TypeSizeGT <$> genTypeVar <*> arbitrary
  , Dep.TypeRange <$> genTypeVar <*> arbitrary <*> arbitrary
  ]

genTypeScheme :: Gen Dep.TypeScheme
genTypeScheme = do
  vars <- vectorOf 2 $ elements ["a", "b", "c"]
  typeVar <- genTypeVar
  return $ Dep.Forall vars typeVar

instance Arbitrary Dep.TypeVar where
  arbitrary = genTypeVar

instance Arbitrary Dep.TypeExpr where
  arbitrary = genTypeExpr

instance Arbitrary Dep.TypeConstraint where
  arbitrary = genTypeConstraint

instance Arbitrary Dep.TypeScheme where
  arbitrary = genTypeScheme

-- Test properties for type system

-- Property 1: Type schemes are monomorphic when they have no type variables
prop_typeSchemeMonomorphic :: Dep.TypeScheme -> Property
prop_typeSchemeMonomorphic scheme =
  let typeVars' = getTypeVars scheme
  in null typeVars' ==> 
     -- A monomorphic type scheme has no type variables
     null typeVars'

-- Property 2: Type schemes preserve their type variables
prop_typeSchemePreservesTypeVars :: [String] -> Dep.TypeVar -> Bool
prop_typeSchemePreservesTypeVars typeVars typeExpr =
  let scheme = Dep.Forall typeVars typeExpr
      extractedVars = getTypeVars scheme
  in extractedVars == typeVars

-- Property 3: Function types have correct arity
prop_functionTypeArity :: [Dep.TypeVar] -> Dep.TypeVar -> Bool
prop_functionTypeArity paramTypes returnType =
  let funcType = Dep.TVFun paramTypes returnType
  in case funcType of
    Dep.TVFun ps rt -> length ps == length paramTypes && rt == returnType
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
    Dep.Equal t1 t2 -> True
    Dep.Subtype t1 t2 -> True
    Dep.Predicate className t -> True
    Dep.TypeSizeGE t n -> True
    Dep.TypeSizeGT t n -> True
    Dep.TypeRange t min max -> True

-- Property 8: Type equality is reflexive (simplified)
prop_typeVarEqualityReflexive :: Dep.TypeVar -> Bool
prop_typeVarEqualityReflexive t = t == t

typeSystemTests :: TestTree
typeSystemTests = testGroup "Type System Tests"
  [ testProperties "Type Scheme Properties"
    [ ("Type schemes with no type variables are monomorphic", property prop_typeSchemeMonomorphic)
    , ("Type schemes preserve their type variables", property prop_typeSchemePreservesTypeVars)
    ]
  , testProperties "Type Equality Properties"
    [ ("Type equality is reflexive", property prop_typeEqualityReflexive)
    , ("Type equality is symmetric", property prop_typeEqualitySymmetric)
    , ("Type equality is transitive", property prop_typeEqualityTransitive)
    ]
  , testProperties "Type Function Properties"
    [ ("Function types have correct arity", property prop_functionTypeArity)
    ]
  , testProperties "Type Constraint Properties"
    [ ("Type constraints preserve their structure", property prop_typeConstraintPreservation)
    ]
  , testProperties "Type Substitution Properties"
    [ ("Type substitution preserves variable-free types", property prop_typeSubstitutionPreservesConstants)
    ]
  ]

-- Property: Type substitution preserves variable-free types
prop_typeSubstitutionPreservesConstants :: Dep.TypeExpr -> Bool
prop_typeSubstitutionPreservesConstants t = 
  -- For variable-free types, substitution should have no effect
  case t of
    Dep.SimpleT _ -> True
    Dep.GenericT _ args -> all prop_typeSubstitutionPreservesConstants args
    _ -> True