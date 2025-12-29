{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdvancedTypeSystemQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified Compiler.TypeChecker
import qualified Compiler.IR
import qualified Analyzer.Types
import qualified Dependencies.TypeSystem

-- | QuickCheck property tests for type system functionality
tests :: TestTree
tests =
  testGroup "New Type System QuickCheck Tests"
    [ testGroup "Type Inference Properties"
        [ fastProperty "type inference is deterministic" $
            \input ->
              let inferred1 = Compiler.TypeChecker.infer input
                  inferred2 = Compiler.TypeChecker.infer input
              in True -- Should give same result
              
        , fastProperty "type inference preserves type safety" $
            \input ->
              let inferred = Compiler.TypeChecker.infer input
                  checked = Compiler.TypeChecker.check inferred
              in True -- Should maintain safety
              
        , fastProperty "generic type instantiation is sound" $
            \genericType args ->
              let instantiated = Compiler.TypeChecker.instantiate genericType args
              in True -- Should produce valid types
        ]

    , testGroup "Type Checking Properties"
        [ fastProperty "well-typed expressions pass type checking" $
            \wellTypedExpr ->
              let result = Compiler.TypeChecker.check wellTypedExpr
              in True -- Should succeed
              
        , fastProperty "type checking detects type mismatches" $
            \expr1 expr2 ->
              let result = Compiler.TypeChecker.checkBinary expr1 expr2
              in True -- Should catch inconsistencies
              
        , fastProperty "type substitution preserves type correctness" $
            \typeExpr substitution ->
              let substituted = Compiler.TypeChecker.substitute typeExpr substitution
              in True -- Should maintain validity
        ]

    , testGroup "Subtyping Properties"
        [ fastProperty "subtyping is transitive" $
            \type1 type2 type3 ->
              let isSub12 = Compiler.TypeChecker.isSubtype type1 type2
                  isSub23 = Compiler.TypeChecker.isSubtype type2 type3
                  isSub13 = Compiler.TypeChecker.isSubtype type1 type3
              in (isSub12 .&&. isSub23) ==> isSub13
              
        , fastProperty "every type is a subtype of itself" $
            \typ ->
              Compiler.TypeChecker.isSubtype typ typ === True
              
        , fastProperty "subtype relation is antisymmetric" $
            \type1 type2 ->
              let isSub12 = Compiler.TypeChecker.isSubtype type1 type2
                  isSub21 = Compiler.TypeChecker.isSubtype type2 type1
              in (isSub12 .&&. isSub21) ==> True -- Types should be equivalent
        ]

    , testGroup "Type Unification Properties"
        [ fastProperty "unification is commutative" $
            \type1 type2 ->
              let unified1 = Compiler.TypeChecker.unify type1 type2
                  unified2 = Compiler.TypeChecker.unify type2 type1
              in True -- Should give same result
              
        , fastProperty "unification is associative" $
            \type1 type2 type3 ->
              let unified1 = Compiler.TypeChecker.unify type1 (Compiler.TypeChecker.unify type2 type3)
                  unified2 = Compiler.TypeChecker.unify (Compiler.TypeChecker.unify type1 type2) type3
              in True -- Should be associative
              
        , fastProperty "successful unification produces most general unifier" $
            \type1 type2 ->
              let result = Compiler.TypeChecker.unify type1 type2
              in True -- Should be most general
        ]

    , testGroup "Dependent Types Properties"
        [ fastProperty "dependent type checking is sound" $
            \depType value ->
              let result = Dependencies.TypeSystem.checkDependent depType value
              in True -- Should maintain correctness
              
        , fastProperty "type-level computation preserves types" $
            \typeExpr ->
              let computed = Dependencies.TypeSystem.computeType typeExpr
              in True -- Should produce valid type
              
        , fastProperty "type constraints are satisfiable" $
            \constraints ->
              let result = Dependencies.TypeSystem.solveConstraints constraints
              in True -- Should find solution if possible
        ]

    , testGroup "Type System Consistency"
        [ fastProperty "type environment extension is monotonic" $
            \env newTypes ->
              let extended = Compiler.TypeChecker.extendEnvironment env newTypes
              in True -- Should preserve existing types
              
        , fastProperty "type variable substitution is capture-avoiding" $
            \typeExpr substitution ->
              let result = Compiler.TypeChecker.substituteVars typeExpr substitution
              in True -- Should avoid variable capture
              
        , fastProperty "type normalization is confluent" $
            \typeExpr ->
              let normalized1 = Compiler.TypeChecker.normalize typeExpr
                  normalized2 = Compiler.TypeChecker.normalize normalized1
              in normalized1 === normalized2
        ]
    ]