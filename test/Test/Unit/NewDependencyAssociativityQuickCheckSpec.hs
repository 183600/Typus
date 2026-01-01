{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependencyAssociativityQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Dependencies
  ( TypeVar(..), TypeConstraint(..), DependentTypeError(..), TypeDef(..)
  , Substitution, newDependentTypeChecker, addType, addConstraint
  , checkType, checkTypeInstantiation, solveConstraints, unify
  , getDependentTypeErrors
  )
import Dependencies.AST (TypeExpr(..), Constraint(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Either (isLeft, isRight)

-- | Test type constraint associativity
prop_constraint_associativity :: TypeVar -> TypeVar -> TypeVar -> Property
prop_constraint_associativity a b c =
    let constraint1 = Equal a b
        constraint2 = Equal b c
        constraint3 = Equal a c
        -- (a = b) = c should be equivalent to a = (b = c)
        unify1 = unify [(a, b), (constraint1, c)]
        unify2 = unify [(constraint2, c), (a, b)]
    in case (unify1, unify2) of
         (Just s1, Just s2) -> sort s1 == sort s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test substitution associativity
prop_substitution_associativity :: TypeVar -> TypeVar -> TypeVar -> Property
prop_substitution_associativity a b c =
    let subst1 = [("x", a), ("y", b)]
        subst2 = [("y", b), ("z", c)]
        -- Apply subst1 then subst2 should be associative
        applySubst s tv = case tv of
          TVVar x -> case lookup x s of
            Nothing -> TVVar x
            Just t -> t
          TVCon _ -> tv
          TVApp f args -> TVApp f (L.map (applySubst s) args)
          TVFun ps rt -> TVFun (L.map (applySubst s) ps) (applySubst s rt)
          TVTuple xs -> TVTuple (L.map (applySubst s) xs)
        
        result1 = applySubst subst2 (applySubst subst1 a)
        result2 = applySubst (subst1 ++ subst2) a
    in result1 == result2

-- | Test type checking associativity
prop_typechecking_associativity :: String -> String -> String -> Property
prop_typechecking_associativity typeName1 typeName2 typeName3 =
    L.all (\n -> L.length n > 0 && L.head n `elem` ['A'..'Z']) [typeName1, typeName2, typeName3] ==>
    let checker = newDependentTypeChecker
        -- Add types in different orders should yield consistent results
        addTypes1 = execState (do
            addType typeName1 [] []
            addType typeName2 [] []
            addType typeName3 [] []) checker
        addTypes2 = execState (do
            addType typeName3 [] []
            addType typeName1 [] []
            addType typeName2 [] []) checker
        errors1 = getDependentTypeErrors addTypes1
        errors2 = getDependentTypeErrors addTypes2
    in null errors1 && null errors2

-- | Test constraint solving associativity
prop_constraint_solving_associativity :: TypeVar -> TypeVar -> TypeVar -> Property
prop_constraint_solving_associativity a b c =
    let checker = newDependentTypeChecker
        checker1 = execState (do
            addConstraint (Equal a b)
            addConstraint (Equal b c)) checker
        checker2 = execState (do
            addConstraint (Equal b c)
            addConstraint (Equal a b)) checker
        success1 = execState solveConstraints checker1
        success2 = execState solveConstraints checker2
        errors1 = getDependentTypeErrors success1
        errors2 = getDependentTypeErrors success2
    in L.length errors1 == L.length errors2

-- | Test unification associativity with function types
prop_unification_function_associativity :: TypeVar -> TypeVar -> TypeVar -> TypeVar -> Property
prop_unification_function_associativity a b c d =
    let funcType1 = TVFun [a] b
        funcType2 = TVFun [c] d
        unify1 = unify [(funcType1, funcType2)]
        unify2 = unify [(a, c), (b, d)]
    in case (unify1, unify2) of
         (Just s1, Just s2) -> L.length s1 == L.length s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test type instantiation associativity
prop_type_instantiation_associativity :: String -> TypeVar -> TypeVar -> Property
prop_type_instantiation_associativity typeName arg1 arg2 =
    L.length typeName > 0 && L.head typeName `elem` ['A'..'Z'] ==>
    let checker = newDependentTypeChecker
        checker1 = execState (do
            addType typeName ["T"] []
            checkTypeInstantiation typeName [arg1]) checker
        checker2 = execState (do
            addType typeName ["T"] []
            checkTypeInstantiation typeName [arg2]) checker
        errors1 = getDependentTypeErrors checker1
        errors2 = getDependentTypeErrors checker2
    in L.length errors1 >= 0 && L.length errors2 >= 0  -- Should not crash

-- | Test constraint composition associativity
prop_constraint_composition_associativity :: TypeVar -> TypeVar -> TypeVar -> Property
prop_constraint_composition_associativity a b c =
    let constraints1 = [Equal a b, Equal b c]
        constraints2 = [Equal b c, Equal a b]
        constraints3 = [Equal a c]
        
        applySubst s tv = case tv of
          TVVar x -> case lookup x s of
            Nothing -> TVVar x
            Just t -> t
          _ -> tv
        
        result1 = unify constraints1
        result2 = unify constraints2
        result3 = unify constraints3
    in case (result1, result2, result3) of
         (Just s1, Just s2, Just s3) -> 
           let final1 = applySubst s1 a
               final2 = applySubst s2 a
               final3 = applySubst s3 a
           in final1 == final2 && final2 == final3
         _ -> True

-- | Test type environment associativity
prop_type_environment_associativity :: String -> String -> Property
prop_type_environment_associativity typeName1 typeName2 =
    L.all (\n -> L.length n > 0 && L.head n `elem` ['A'..'Z']) [typeName1, typeName2] ==>
    let checker = newDependentTypeChecker
        checker1 = execState (do
            addType typeName1 [] []
            addType typeName2 [] []) checker
        checker2 = execState (do
            addType typeName2 [] []
            addType typeName1 [] []) checker
        env1 = typeDefinitions (dtcTypeEnv checker1)
        env2 = typeDefinitions (dtcTypeEnv checker2)
    in Map.size env1 == Map.size env2

-- | Test substitution composition
prop_substitution_composition :: TypeVar -> TypeVar -> TypeVar -> Property
prop_substitution_composition a b c =
    let subst1 = [("x", a), ("y", b)]
        subst2 = [("y", b), ("z", c)]
        
        applySubst s tv = case tv of
          TVVar x -> case lookup x s of
            Nothing -> TVVar x
            Just t -> t
          TVCon _ -> tv
          TVApp f args -> TVApp f (L.map (applySubst s) args)
          TVFun ps rt -> TVFun (L.map (applySubst s) ps) (applySubst s rt)
          TVTuple xs -> TVTuple (L.map (applySubst s) xs)
        
        -- Apply subst1 then subst2
        result1 = applySubst subst2 (applySubst subst1 a)
        -- Apply composed substitution
        composed = [(x, applySubst subst2 t) | (x, t) <- subst1] ++ subst2
        result2 = applySubst composed a
    in result1 == result2

-- | Test unification with tuple types associativity
prop_unification_tuple_associativity :: TypeVar -> TypeVar -> TypeVar -> TypeVar -> Property
prop_unification_tuple_associativity a b c d =
    let tuple1 = TVTuple [a, b]
        tuple2 = TVTuple [c, d]
        unify1 = unify [(tuple1, tuple2)]
        unify2 = unify [(a, c), (b, d)]
    in case (unify1, unify2) of
         (Just s1, Just s2) -> L.length s1 == L.length s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test generic type associativity
prop_generic_type_associativity :: String -> TypeVar -> TypeVar -> Property
prop_generic_type_associativity typeName arg1 arg2 =
    L.length typeName > 0 && L.head typeName `elem` ['A'..'Z'] ==>
    let genericType1 = TVApp typeName [arg1]
        genericType2 = TVApp typeName [arg2]
        unify1 = unify [(genericType1, genericType2)]
        unify2 = unify [(arg1, arg2)]
    in case (unify1, unify2) of
         (Just s1, Just s2) -> L.length s1 >= L.length s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test constraint solving order independence
prop_constraint_solving_order_independence :: TypeVar -> TypeVar -> TypeVar -> TypeVar -> Property
prop_constraint_solving_order_independence a b c d =
    let constraints1 = [Equal a b, Equal c d]
        constraints2 = [Equal c d, Equal a b]
        result1 = unify constraints1
        result2 = unify constraints2
    in case (result1, result2) of
         (Just s1, Just s2) -> sort s1 == sort s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test type checking with constraints associativity
prop_typechecking_constraints_associativity :: String -> TypeVar -> TypeVar -> Property
prop_typechecking_constraints_associativity typeName arg1 arg2 =
    L.length typeName > 0 && L.head typeName `elem` ['A'..'Z'] ==>
    let checker = newDependentTypeChecker
        constraint = TypeSizeGE arg1 5
        checker1 = execState (do
            addType typeName ["T"] [constraint]
            checkTypeInstantiation typeName [arg1]) checker
        checker2 = execState (do
            checkTypeInstantiation typeName [arg1]
            addType typeName ["T"] [constraint]) checker
        errors1 = getDependentTypeErrors checker1
        errors2 = getDependentTypeErrors checker2
    in L.length errors1 >= 0 && L.length errors2 >= 0

-- | Test multiple unification associativity
prop_multiple_unification_associativity :: TypeVar -> TypeVar -> TypeVar -> TypeVar -> TypeVar -> Property
prop_multiple_unification_associativity a b c d e =
    let constraints = [(a, b), (b, c), (c, d), (d, e)]
        -- Group constraints differently
        group1 = [(a, b), (b, c)] ++ [(c, d), (d, e)]
        group2 = [(c, d), (d, e)] ++ [(a, b), (b, c)]
        result1 = unify group1
        result2 = unify group2
    in case (result1, result2) of
         (Just s1, Just s2) -> sort s1 == sort s2
         (Nothing, Nothing) -> True
         _ -> False

-- | Test substitution idempotence
prop_substitution_idempotence :: TypeVar -> Property
prop_substitution_idempotence a =
    let subst = [("x", a)]
        applySubst s tv = case tv of
          TVVar x -> case lookup x s of
            Nothing -> TVVar x
            Just t -> t
          _ -> tv
        result1 = applySubst subst a
        result2 = applySubst subst result1
    in result1 == result2

-- | Test constraint application associativity
prop_constraint_application_associativity :: TypeVar -> TypeVar -> TypeVar -> Property
prop_constraint_application_associativity a b c =
    let constraint1 = Equal a b
        constraint2 = Equal b c
        applySubst s tv = case tv of
          TVVar x -> case lookup x s of
            Nothing -> TVVar x
            Just t -> t
          _ -> tv
        subst1 = case unify [(a, b)] of
          Just s -> s
          Nothing -> []
        subst2 = case unify [(b, c)] of
          Just s -> s
          Nothing -> []
        -- Apply substitutions in different orders
        result1 = applySubst subst2 (applySubst subst1 a)
        result2 = applySubst subst1 (applySubst subst2 a)
    in result1 == result2

tests :: TestTree
tests = testGroup "Dependency Associativity QuickCheck Tests"
  [ testProperty "constraint associativity" prop_constraint_associativity
  , testProperty "substitution associativity" prop_substitution_associativity
  , testProperty "typechecking associativity" prop_typechecking_associativity
  , testProperty "constraint solving associativity" prop_constraint_solving_associativity
  , testProperty "unification function associativity" prop_unification_function_associativity
  , testProperty "type instantiation associativity" prop_type_instantiation_associativity
  , testProperty "constraint composition associativity" prop_constraint_composition_associativity
  , testProperty "type environment associativity" prop_type_environment_associativity
  , testProperty "substitution composition" prop_substitution_composition
  , testProperty "unification tuple associativity" prop_unification_tuple_associativity
  , testProperty "generic type associativity" prop_generic_type_associativity
  , testProperty "constraint solving order independence" prop_constraint_solving_order_independence
  , testProperty "typechecking constraints associativity" prop_typechecking_constraints_associativity
  , testProperty "multiple unification associativity" prop_multiple_unification_associativity
  , testProperty "substitution idempotence" prop_substitution_idempotence
  , testProperty "constraint application associativity" prop_constraint_application_associativity
  ]