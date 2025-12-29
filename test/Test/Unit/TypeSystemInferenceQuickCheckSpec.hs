{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemInferenceQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.TypeChecker
import DependentTypesParser

import Data.List (nub, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Mock type system for testing
data MockType = MockInt | MockBool | MockString | MockFunction MockType MockType | MockVar String
  deriving (Show, Eq)

data MockTypeConstraint = MockEquality MockType MockType | MockSubtype MockType MockType
  deriving (Show, Eq)

data MockTypeEnvironment = MockTypeEnvironment
  { typeBindings :: Map.Map String MockType
  , constraints :: [MockTypeConstraint]
  } deriving (Show, Eq)

-- Property: Type inference should be deterministic
prop_type_inference_deterministic :: MockTypeEnvironment -> String -> Property
prop_type_inference_deterministic env expr =
  let inferred1 = inferType env expr
      inferred2 = inferType env expr
  in property $ inferred1 === inferred2

-- Property: Type inference should respect existing bindings
prop_type_inference_respects_bindings :: MockTypeEnvironment -> String -> MockType -> Property
prop_type_inference_respects_bindings env varName varType =
  let extendedEnv = MockTypeEnvironment (Map.insert varName varType (typeBindings env)) (constraints env)
      inferred = inferType extendedEnv varName
  in property $ inferred === Just varType

-- Property: Function application should respect argument types
prop_function_application_types :: MockType -> MockType -> String -> Property
prop_function_application_types argType returnType funcName =
  let funcType = MockFunction argType returnType
      env = MockTypeEnvironment (Map.singleton funcName funcType) []
      argValue = "arg"
      application = funcName ++ " " ++ argValue
      inferred = inferType env application
  in property $ inferred === Just returnType

-- Property: Type unification should be symmetric
prop_type_unification_symmetric :: MockType -> MockType -> Property
prop_type_unification_symmetric type1 type2 =
  let unified1 = unifyTypes type1 type2
      unified2 = unifyTypes type2 type1
  in property $ unified1 === unified2

-- Property: Type unification should be associative where applicable
prop_type_unification_associative :: MockType -> MockType -> MockType -> Property
prop_type_unification_associative type1 type2 type3 =
  let unified12 = unifyTypes type1 type2
      unified23 = unifyTypes type2 type3
      final1 = case unified12 of
        Just t12 -> unifyTypes t12 type3
        Nothing -> Nothing
      final2 = case unified23 of
        Just t23 -> unifyTypes type1 t23
        Nothing -> Nothing
  in property $ final1 === final2

-- Property: Generalization should preserve type safety
prop_generalization_preserves_safety :: MockTypeEnvironment -> String -> Property
prop_generalization_preserves_safety env expr =
  let inferred = inferType env expr
      generalized = generalizeType env expr
  in case (inferred, generalized) of
    (Just t1, Just t2) -> property $ isMoreGeneral t2 t1
    _ -> property $ True

-- Property: Instantiation should respect constraints
prop_instantiation_respects_constraints :: MockTypeEnvironment -> MockType -> Property
prop_instantiation_respects_constraints env polyType =
  let instantiated = instantiateType env polyType
  in case instantiated of
    Just t -> property $ typeWellFormed env t
    Nothing -> property $ True

-- Property: Type inference should detect contradictions
prop_type_inference_detects_contradictions :: MockTypeEnvironment -> String -> Property
prop_type_inference_detects_contradictions env expr =
  let inferred = inferType env expr
      contradictoryConstraints = hasContradictoryConstraints env
  in contradictoryConstraints ==> property $ inferred === Nothing

-- Property: Subtyping should be transitive
prop_subtyping_transitive :: MockType -> MockType -> MockType -> Property
prop_subtyping_transitive type1 type2 type3 =
  let subtype12 = isSubtype type1 type2
      subtype23 = isSubtype type2 type3
      subtype13 = isSubtype type1 type3
  in (subtype12 .&&. subtype23) ==> property $ subtype13

-- Property: Type inference should handle polymorphic functions
prop_polymorphic_function_inference :: String -> Property
prop_polymorphic_function_inference funcName =
  let polyType = MockVar "a" -- Represents a polymorphic type variable
      env = MockTypeEnvironment (Map.singleton funcName polyType) []
      application = funcName ++ " x"
      inferred = inferType env application
  in property $ inferred /= Nothing

-- Property: Type inference should be consistent with substitution
prop_type_inference_substitution_consistent :: MockTypeEnvironment -> String -> String -> MockType -> Property
prop_type_inference_substitution_consistent env oldVar newVar newType =
  let substitutedEnv = substituteTypeVar oldVar newType env
      originalInferred = inferType env oldVar
      substitutedInferred = inferType substitutedEnv newVar
  in case (originalInferred, substitutedInferred) of
    (Just t1, Just t2) -> property $ t2 === substituteInType oldVar newType t1
    _ -> property $ True

-- Helper functions for mock type system operations
inferType :: MockTypeEnvironment -> String -> Maybe MockType
inferType env expr
  | Map.member expr (typeBindings env) = Map.lookup expr (typeBindings env)
  | " " `isInfixOf` expr = 
      let parts = words expr
          funcName = head parts
          arg = last parts
      in case Map.lookup funcName (typeBindings env) of
        Just (MockFunction argType returnType) -> 
          if inferType env arg == Just argType then Just returnType else Nothing
        _ -> Nothing
  | otherwise = Nothing

unifyTypes :: MockType -> MockType -> Maybe MockType
unifyTypes t1 t2
  | t1 == t2 = Just t1
  | MockVar _ <- t1 = Just t2
  | MockVar _ <- t2 = Just t1
  | MockFunction arg1 ret1 <- t1, MockFunction arg2 ret2 <- t2 = do
      unifiedArg <- unifyTypes arg1 arg2
      unifiedRet <- unifyTypes ret1 ret2
      return $ MockFunction unifiedArg unifiedRet
  | otherwise = Nothing

isMoreGeneral :: MockType -> MockType -> Bool
isMoreGeneral (MockVar _) _ = True
isMoreGeneral _ (MockVar _) = False
isMoreGeneral (MockFunction arg1 ret1) (MockFunction arg2 ret2) = 
  isMoreGeneral arg1 arg2 && isMoreGeneral ret1 ret2
isMoreGeneral t1 t2 = t1 == t2

generalizeType :: MockTypeEnvironment -> String -> Maybe MockType
generalizeType env expr = inferType env expr

instantiateType :: MockTypeEnvironment -> MockType -> Maybe MockType
instantiateType env (MockVar name) = Map.lookup name (typeBindings env)
instantiateType _ t = Just t

typeWellFormed :: MockTypeEnvironment -> MockType -> Bool
typeWellFormed _ (MockInt) = True
typeWellFormed _ (MockBool) = True
typeWellFormed _ (MockString) = True
typeWellFormed env (MockFunction arg ret) = typeWellFormed env arg && typeWellFormed env ret
typeWellFormed env (MockVar name) = Map.member name (typeBindings env)

hasContradictoryConstraints :: MockTypeEnvironment -> Bool
hasContradictoryConstraints env = any isContradiction (constraints env)
  where
    isContradiction (MockEquality t1 t2) = t1 /= t2
    isContradiction (MockSubtype t1 t2) = t1 == t2 && t1 /= t2

isSubtype :: MockType -> MockType -> Bool
isSubtype t1 t2 = t1 == t2

substituteTypeVar :: String -> MockType -> MockTypeEnvironment -> MockTypeEnvironment
substituteTypeVar oldVar newType env =
  let newBindings = Map.map (substituteInType oldVar newType) (typeBindings env)
      newConstraints = map (substituteInConstraint oldVar newType) (constraints env)
  in MockTypeEnvironment newBindings newConstraints

substituteInType :: String -> MockType -> MockType -> MockType
substituteInType oldVar newType (MockVar name)
  | name == oldVar = newType
  | otherwise = MockVar name
substituteInType oldVar newType (MockFunction arg ret) = 
  MockFunction (substituteInType oldVar newType arg) (substituteInType oldVar newType ret)
substituteInType _ _ t = t

substituteInConstraint :: String -> MockType -> MockTypeConstraint -> MockTypeConstraint
substituteInConstraint oldVar newType (MockEquality t1 t2) = 
  MockEquality (substituteInType oldVar newType t1) (substituteInType oldVar newType t2)
substituteInConstraint oldVar newType (MockSubtype t1 t2) = 
  MockSubtype (substituteInType oldVar newType t1) (substituteInType oldVar newType t2)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (length needle) s : substrings xs

tests :: TestTree
tests = testGroup "Type System Inference QuickCheck Tests"
  [ fastProperty "Type inference is deterministic" prop_type_inference_deterministic
  , fastProperty "Type inference respects bindings" prop_type_inference_respects_bindings
  , fastProperty "Function application respects argument types" prop_function_application_types
  , fastProperty "Type unification is symmetric" prop_type_unification_symmetric
  , fastProperty "Type unification is associative where applicable" prop_type_unification_associative
  , fastProperty "Generalization preserves type safety" prop_generalization_preserves_safety
  , fastProperty "Instantiation respects constraints" prop_instantiation_respects_constraints
  , fastProperty "Type inference detects contradictions" prop_type_inference_detects_contradictions
  , fastProperty "Subtyping is transitive" prop_subtyping_transitive
  , fastProperty "Polymorphic function inference" prop_polymorphic_function_inference
  , fastProperty "Type inference substitution is consistent" prop_type_inference_substitution_consistent
  ]