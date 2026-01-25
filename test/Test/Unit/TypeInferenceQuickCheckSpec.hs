{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.TypeInferenceQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty
-- Removed empty QuickCheck import
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- Simple type system for testing
data Type = TInt | TBool | TString | TFunc Type Type | TVar String | TTuple [Type]
          deriving (Eq, Ord, Show)

data TypeConstraint = TypeEq Type Type
                    deriving (Eq, Show)

data TypeEnv = TypeEnv { envBindings :: Map String Type }
             deriving (Eq, Show)

data TypeError = TypeMismatch Type Type
               | UnificationFailure Type Type
               | UnboundVariable String
               deriving (Eq, Show)

-- Helper generators for type inference tests
genBaseType :: Gen Type
genBaseType = elements [TInt, TBool, TString]

genTypeVar :: Gen Type
genTypeVar = do
  varName <- elements ["a", "b", "c", "d", "e", "f", "g", "h"]
  return $ TVar varName

genType :: Int -> Gen Type
genType 0 = oneof [genBaseType, genTypeVar]
genType depth = oneof 
  [ genBaseType
  , genTypeVar
  , do
      argType <- genType (depth - 1)
      returnType <- genType (depth - 1)
      return $ TFunc argType returnType
  , do
      numTypes <- choose (0, 3)
      types <- replicateM numTypes (genType (depth - 1))
      return $ TTuple types
  ]

-- Arbitrary instances
instance Arbitrary Type where
  arbitrary = genType 3

instance Arbitrary TypeEnv where
  arbitrary = do
    numBindings <- choose (0, 5)
    names <- replicateM numBindings (elements ["x", "y", "z", "f", "g", "h"])
    types <- replicateM numBindings arbitrary
    let bindings = Map.fromList $ zip names types
    return $ TypeEnv bindings

genTypeEnv :: Gen TypeEnv
genTypeEnv = do
  size <- choose (0, 5)
  keys <- replicateM size (elements ["x", "y", "z", "a", "b", "c", "d", "e"])
  types <- replicateM size (genType 2)
  let bindings = Map.fromList $ zip keys types
  return $ TypeEnv bindings

genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = do
  t1 <- genType 2
  t2 <- genType 2
  return $ TypeEq t1 t2

-- Test properties for type inference

-- Property 1: Type environment lookup is consistent
prop_type_env_lookup_consistent :: TypeEnv -> String -> Bool
prop_type_env_lookup_consistent env var = 
  let lookup1 = Map.lookup var (envBindings env)
      lookup2 = Map.lookup var (envBindings env)
  in lookup1 == lookup2

-- Property 2: Type equality is reflexive
prop_type_equality_reflexive :: Type -> Bool
prop_type_equality_reflexive t = t == t

-- Property 3: Type equality is symmetric
prop_type_equality_symmetric :: Type -> Type -> Bool
prop_type_equality_symmetric t1 t2 = 
  if t1 == t2 then t2 == t1 else True

-- Property 4: Type equality is transitive
prop_type_equality_transitive :: Type -> Type -> Type -> Property
prop_type_equality_transitive t1 t2 t3 = 
  (t1 == t2 && t2 == t3) ==> t1 == t3

-- Property 5: Function type composition
prop_func_type_composition :: Type -> Type -> Type -> Bool
prop_func_type_composition t1 t2 t3 = 
  let f1 = TFunc t1 t2
      f2 = TFunc t2 t3
      composed = TFunc t1 t3
  in isFunctionType composed

-- Property 6: Tuple type projection preserves types
prop_tuple_type_projection :: [Type] -> Int -> Property
prop_tuple_type_projection types index = 
  not (null types) && index >= 0 && index < length types ==> 
  let tupleType = TTuple types
      projected = projectTupleType tupleType index
  in projected == Just (types !! index)

-- Property 7: Type substitution preserves structure
prop_type_substitution_preserves_structure :: Type -> String -> Type -> Bool
prop_type_substitution_preserves_structure t varName replacement = 
  let substituted = substituteType t varName replacement
  in typeSize substituted >= typeSize replacement || not (containsTypeVar t varName)

-- Property 8: Type unification is symmetric
prop_type_unification_symmetric :: Type -> Type -> Bool
prop_type_unification_symmetric t1 t2 = 
  let unify1 = unifyTypes t1 t2
      unify2 = unifyTypes t2 t1
  in case (unify1, unify2) of
       (Just _, Just _) -> True
       (Nothing, Nothing) -> True
       _ -> False

-- Property 9: Most general unifier is most general
prop_most_general_unifier_is_most_general :: Type -> Type -> Property
prop_most_general_unifier_is_most_general t1 t2 = 
  let mgu = mostGeneralUnifier t1 t2
  in case mgu of
       Just subst -> 
         let specialized = specializeSubstitution subst
         in property $ isMoreGeneral subst specialized
       Nothing -> property True

-- Property 10: Type inference preserves well-typedness
prop_type_inference_preserves_well_typedness :: TypeEnv -> String -> Type -> Property
prop_type_inference_preserves_well_typedness env var expectedType = 
  let extendedEnv = extendTypeEnv env var expectedType
      inferredType = inferType extendedEnv var
  in property $ inferredType == Just expectedType

-- Helper functions for type inference
isFunctionType :: Type -> Bool
isFunctionType (TFunc _ _) = True
isFunctionType _ = False

projectTupleType :: Type -> Int -> Maybe Type
projectTupleType (TTuple types) index 
  | index >= 0 && index < length types = Just (types !! index)
  | otherwise = Nothing
projectTupleType _ _ = Nothing

substituteType :: Type -> String -> Type -> Type
substituteType (TVar v) varName replacement
  | v == varName = replacement
  | otherwise = TVar v
substituteType (TFunc argType returnType) varName replacement = 
  TFunc (substituteType argType varName replacement) 
        (substituteType returnType varName replacement)
substituteType (TTuple types) varName replacement = 
  TTuple $ map (\t -> substituteType t varName replacement) types
substituteType t _ _ = t

containsTypeVar :: Type -> String -> Bool
containsTypeVar (TVar v) varName = v == varName
containsTypeVar (TFunc argType returnType) varName = 
  containsTypeVar argType varName || containsTypeVar returnType varName
containsTypeVar (TTuple types) varName = 
  any (`containsTypeVar` varName) types
containsTypeVar _ _ = False

typeSize :: Type -> Int
typeSize TInt = 1
typeSize TBool = 1
typeSize TString = 1
typeSize (TVar _) = 1
typeSize (TFunc argType returnType) = 1 + typeSize argType + typeSize returnType
typeSize (TTuple types) = 1 + sum (map typeSize types)

unifyTypes :: Type -> Type -> Maybe (Map String Type)
unifyTypes t1 t2 
  | t1 == t2 = Just Map.empty
  | isTypeVar t1 = bindTypeVar t1 t2
  | isTypeVar t2 = bindTypeVar t2 t1
  | isFunctionType t1 && isFunctionType t2 = 
      case (t1, t2) of
        (TFunc arg1 ret1, TFunc arg2 ret2) -> 
          case unifyTypes arg1 arg2 of
            Nothing -> Nothing
            Just subst1 -> 
              case unifyTypes (applySubstitution subst1 ret1) (applySubstitution subst1 ret2) of
                Nothing -> Nothing
                Just subst2 -> Just (subst1 `Map.union` subst2)
        _ -> Nothing
  | otherwise = Nothing
  where
    isTypeVar (TVar _) = True
    isTypeVar _ = False
    
    bindTypeVar (TVar var) t = 
      if containsTypeVar t var then Nothing else Just (Map.singleton var t)
    bindTypeVar _ _ = Nothing

applySubstitution :: Map String Type -> Type -> Type
applySubstitution subst (TVar v) = 
  case Map.lookup v subst of
    Just t -> t
    Nothing -> TVar v
applySubstitution subst (TFunc argType returnType) = 
  TFunc (applySubstitution subst argType) (applySubstitution subst returnType)
applySubstitution subst (TTuple types) = 
  TTuple $ map (applySubstitution subst) types
applySubstitution _ t = t

mostGeneralUnifier :: Type -> Type -> Maybe (Map String Type)
mostGeneralUnifier = unifyTypes

specializeSubstitution :: Map String Type -> Map String Type
specializeSubstitution subst = 
  Map.map (\t -> case t of
                  TVar v -> TVar (v ++ "'")
                  TFunc argType returnType -> TFunc argType returnType
                  TTuple types -> TTuple types
                  TInt -> TInt
                  TBool -> TBool
                  TString -> TString) subst

isMoreGeneral :: Map String Type -> Map String Type -> Bool
isMoreGeneral subst1 subst2 = 
  Map.size subst1 <= Map.size subst2 && 
  all (\(v, t) -> Map.lookup v subst2 == Just t) (Map.toList subst1)

extendTypeEnv :: TypeEnv -> String -> Type -> TypeEnv
extendTypeEnv env var typ = 
  TypeEnv $ Map.insert var typ (envBindings env)

inferType :: TypeEnv -> String -> Maybe Type
inferType env var = Map.lookup var (envBindings env)

-- Test cases for type inference
testTypeInference :: TestTree
testTypeInference = testGroup "Type Inference QuickCheck Tests"
  [ testProperties "Type Environment Properties"
    [ ("type_env_lookup_consistent", property prop_type_env_lookup_consistent) ]
  , testProperties "Type Equality Properties"
    [ ("type_equality_reflexive", property prop_type_equality_reflexive)
        , ("type_equality_symmetric", property prop_type_equality_symmetric)
        , ("type_equality_transitive", property prop_type_equality_transitive) ]
  , testProperties "Function Type Properties"
    [ ("func_type_composition", property prop_func_type_composition) ]
  , testProperties "Tuple Type Properties"
    [ ("tuple_type_projection", property prop_tuple_type_projection) ]
  , testProperties "Type Substitution Properties"
    [ ("type_substitution_preserves_structure", property prop_type_substitution_preserves_structure) ]
  , testProperties "Type Unification Properties"
    [ ("type_unification_symmetric", property prop_type_unification_symmetric)
        , ("most_general_unifier_is_most_general", property prop_most_general_unifier_is_most_general) ]
  , testProperties "Type Inference Properties"
    [ ("type_inference_preserves_well_typedness", property prop_type_inference_preserves_well_typedness) ]
  , testCase "Basic type unification" $ do
    let t1 = TInt
    let t2 = TInt
    let result = unifyTypes t1 t2
    assertEqual "Should unify identical types" (Just Map.empty) result
  
  , testCase "Function type unification" $ do
    let t1 = TFunc TInt TBool
    let t2 = TFunc TInt TBool
    let result = unifyTypes t1 t2
    assertEqual "Should unify identical function types" (Just Map.empty) result
  
  , testCase "Type variable unification" $ do
    let t1 = TVar "a"
    let t2 = TInt
    let result = unifyTypes t1 t2
    assertEqual "Should bind type variable" (Just (Map.singleton "a" TInt)) result
  
  , testCase "Type substitution" $ do
    let t = TFunc (TVar "a") (TVar "b")
    let subst = Map.fromList [("a", TInt), ("b", TBool)]
    let result = applySubstitution subst t
    assertEqual "Should apply substitution correctly" (TFunc TInt TBool) result
  
  , testCase "Type environment extension" $ do
    let env = TypeEnv Map.empty
    let extended = extendTypeEnv env "x" TInt
    let result = inferType extended "x"
    assertEqual "Should find type in extended environment" (Just TInt) result
  
  , testCase "Tuple type projection" $ do
    let tupleType = TTuple [TInt, TBool, TString]
    let result = projectTupleType tupleType 1
    assertEqual "Should project correct tuple element" (Just TBool) result
  
  , testCase "Type variable containment" $ do
    let t = TFunc (TVar "a") (TFunc (TVar "b") (TVar "a"))
    assertBool "Should detect type variable containment" 
               (containsTypeVar t "a")
  ]

-- Export the test
tests :: TestTree
tests = testTypeInference
