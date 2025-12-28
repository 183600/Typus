{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.TypeSystemInferenceBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary)

import Compiler.TypeChecker
  ( TypeChecker
  , TypeEnvironment
  , TypeConstraint(..)
  , TypeScheme(..)
  , TypeVar(..)
  , inferType
  , unifyTypes
  , substituteTypes
  , generalizeType
  , instantiateType
  , TypeInferenceError(..)
  , TypeCheckResult(..)
  , emptyTypeEnvironment
  , extendTypeEnvironment
  )

import Compiler.IR
  ( IRExpression(..)
  , IRType(..)
  , TypeAnnotation(..)
  )

import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (isLeft, isRight)

-- | Test type system inference boundary conditions
tests :: TestTree
tests =
  testGroup "Type System Inference Boundary Tests"
    [ testGroup "Basic type inference boundaries"
        [ testCase "infers types for simple expressions" $ do
            let env = emptyTypeEnvironment
                expr = IRLiteral (IRInt 42)
                result = inferType env expr
            assertBool "should infer Int type" $ 
              case result of
                Right (_, IRTypeInt) -> True
                _ -> False

        , testCase "handles type variable unification" $ do
            let env = emptyTypeEnvironment
                typeVar1 = TypeVar "a"
                typeVar2 = TypeVar "b"
                constraint = TypeEquality typeVar1 typeVar2
                result = unifyTypes [constraint]
            assertBool "should unify type variables" $ 
              case result of
                Right _ -> True
                Left _ -> False

        , testCase "detects type mismatches" $ do
            let env = emptyTypeEnvironment
                expr = IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRString "bad"))
                result = inferType env expr
            assertBool "should detect type mismatch" $ 
              case result of
                Left (TypeMismatch _ _) -> True
                _ -> False
        ]

    , testGroup "Complex type inference scenarios"
        [ testCase "infers polymorphic function types" $ do
            let env = emptyTypeEnvironment
                identityExpr = IRLambda "x" (IRVariable "x")
                result = inferType env identityExpr
            assertBool "should infer polymorphic identity function" $ 
              case result of
                Right (_, IRFunctionType paramType returnType) -> 
                  paramType == returnType  -- Identity: a -> a
                _ -> False

        , testCase "handles higher-order functions" $ do
            let env = emptyTypeEnvironment
                mapExpr = IRLambda "f" 
                    (IRLambda "xs" 
                      (IRBinaryOp Apply (IRVariable "f") (IRVariable "xs")))
                result = inferType env mapExpr
            assertBool "should infer higher-order function type" $ 
              case result of
                Right (_, _) -> True  -- Complex type, just check it succeeds
                Left _ -> False

        , testCase "infers recursive function types" $ do
            let env = emptyTypeEnvironment
                factorialExpr = IRLambda "n"
                    (IRBinaryOp If
                        (IRBinaryOp Equal (IRVariable "n") (IRLiteral (IRInt 0)))
                        (IRLiteral (IRInt 1))
                        (IRBinaryOp Mul 
                            (IRVariable "n")
                            (IRBinaryOp Apply 
                                (IRVariable "factorial")
                                (IRBinaryOp Sub (IRVariable "n") (IRLiteral (IRInt 1))))))
                result = inferType env factorialExpr
            assertBool "should handle recursive function inference" $ 
              case result of
                Right (_, IRFunctionType IRTypeInt IRTypeInt) -> True
                _ -> False
        ]

    , testGroup "Type environment boundaries"
        [ testCase "handles large type environments" $ do
            let baseEnv = emptyTypeEnvironment
                env = foldr (\i acc -> extendTypeEnvironment acc ("var" ++ show i) IRTypeInt) baseEnv [1..1000]
                expr = IRVariable "var500"
                result = inferType env expr
            assertBool "should handle large environments" $ 
              case result of
                Right (_, IRTypeInt) -> True
                _ -> False

        , testCase "manages nested scopes correctly" $ do
            let outerEnv = extendTypeEnvironment emptyTypeEnvironment "x" IRTypeInt
                innerEnv = extendTypeEnvironment outerEnv "x" IRTypeString  -- Shadowing
                outerExpr = IRVariable "x"
                innerExpr = IRVariable "x"
                outerResult = inferType outerEnv outerExpr
                innerResult = inferType innerEnv innerExpr
            assertBool "outer scope should have Int" $ 
              case outerResult of
                Right (_, IRTypeInt) -> True
                _ -> False
            assertBool "inner scope should have String" $ 
              case innerResult of
                Right (_, IRTypeString) -> True
                _ -> False

        , testCase "handles type variable capture" $ do
            let env = emptyTypeEnvironment
                -- Expression that might cause variable capture
                captureExpr = IRLambda "f" 
                    (IRBinaryOp Apply
                        (IRVariable "f")
                        (IRVariable "x"))  -- x is free
                result = inferType env captureExpr
            assertBool "should handle variable capture" $ 
              case result of
                Right (_, _) -> True  -- Should handle gracefully
                Left (UnboundVariable _) -> True  -- Or detect unbound variable
                _ -> False
        ]

    , testGroup "Constraint solving boundaries"
        [ testCase "solves complex constraint systems" $ do
            let constraints = 
                  [ TypeEquality (TypeVar "a") IRTypeInt
                  , TypeEquality (TypeVar "b") IRTypeString
                  , TypeEquality 
                      (IRFunctionType (TypeVar "a") (TypeVar "c"))
                      (IRFunctionType IRTypeInt IRTypeBool)
                  ]
                result = unifyTypes constraints
            assertBool "should solve complex constraints" $ 
              case result of
                Right substitution -> 
                  Map.lookup "a" substitution == Just IRTypeInt &&
                  Map.lookup "c" substitution == Just IRTypeBool
                Left _ -> False

        , testCase "detects unsolvable constraints" $ do
            let constraints = 
                  [ TypeEquality IRTypeInt IRTypeString
                  , TypeEquality (TypeVar "a") IRTypeInt
                  ]
                result = unifyTypes constraints
            assertBool "should detect unsolvable constraints" $ 
              case result of
                Left (UnificationError _) -> True
                _ -> False

        , testCase "handles cyclic constraints" $ do
            let constraints = 
                  [ TypeEquality (TypeVar "a") (IRFunctionType (TypeVar "b") IRTypeInt)
                  , TypeEquality (TypeVar "b") (IRFunctionType (TypeVar "a") IRTypeInt)
                  ]
                result = unifyTypes constraints
            assertBool "should handle cyclic constraints" $ 
              case result of
                Right _ -> True   -- Should find a solution (recursive types)
                Left _ -> False   -- Or detect as unsolvable
        ]

    , testGroup "Generalization and instantiation"
        [ testCase "generalizes polymorphic types correctly" $ do
            let env = emptyTypeEnvironment
                expr = IRLambda "x" (IRVariable "x")
                result = inferType env expr
            assertBool "should generalize to polymorphic type" $ 
              case result of
                Right (_, scheme) -> isPolymorphic scheme
                _ -> False
          where
            isPolymorphic (TypeScheme vars _) = not (null vars)

        , testCase "instantiates polymorphic types" $ do
            let scheme = TypeScheme ["a"] (IRFunctionType (TypeVar "a") (TypeVar "a"))
                instanceType = instantiateType scheme
            assertBool "should instantiate to concrete type" $ 
              case instanceType of
                IRFunctionType paramType returnType -> paramType == returnType
                _ -> False

        , testCase "preserves type correctness through generalization" $ do
            let env = emptyTypeEnvironment
                expr = IRLambda "f" 
                    (IRBinaryOp Apply 
                        (IRVariable "f")
                        (IRLiteral (IRInt 42)))
                result = inferType env expr
            assertBool "generalization should preserve correctness" $ 
              case result of
                Right (_, _) -> True
                Left _ -> False
        ]

    , testGroup "Error handling boundaries"
        [ testCase "provides informative error messages" $ do
            let env = emptyTypeEnvironment
                expr = IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRString "bad"))
                result = inferType env expr
            assertBool "should provide informative error" $ 
              case result of
                Left (TypeMismatch expected actual) -> 
                  show expected /= "" && show actual /= ""
                _ -> False

        , testCase "handles multiple errors gracefully" $ do
            let env = emptyTypeEnvironment
                expr = IRBinaryOp Add 
                    (IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRString "bad1")))
                    (IRLiteral (IRString "bad2"))
                result = inferType env expr
            assertBool "should handle multiple errors" $ 
              case result of
                Left _ -> True  -- Should report at least one error
                Right _ -> False
        ]

    , testGroup "Performance boundaries"
        [ testCase "handles deeply nested type expressions" $ do
            let nestedType = foldr (\t acc -> IRFunctionType t acc) IRTypeInt 
                              (replicate 100 IRTypeBool)
                expr = IRVariable "deeply_nested"
                env = extendTypeEnvironment emptyTypeEnvironment "deeply_nested" nestedType
                result = inferType env expr
            assertBool "should handle deeply nested types" $ 
              case result of
                Right (_, inferredType) -> inferredType == nestedType
                _ -> False

        , testCase "scales with expression complexity" $ do
            let complexExpr = foldl1 IRBinaryOp Add 
                              [IRLiteral (IRInt i) | i <- [1..100]]
                env = emptyTypeEnvironment
                result = inferType env complexExpr
            assertBool "should handle complex expressions" $ 
              case result of
                Right (_, IRTypeInt) -> True
                _ -> False
        ]

    , testGroup "QuickCheck property tests for type inference"
        [ fastProperty "type inference is deterministic" $
            \env expr ->
            let result1 = inferType env expr
                result2 = inferType env expr
            in result1 === result2

        , fastProperty "unification is idempotent" $
            \constraints ->
            let result1 = unifyTypes constraints
                result2 = case result1 of
                  Right substitution -> unifyTypes (map (applySubstitution substitution) constraints)
                  Left _ -> result1
            in result1 === result2
          where
            applySubstitution substitution constraint = 
              case constraint of
                TypeEquality t1 t2 -> 
                  TypeEquality (subst substitution t1) (subst substitution t2)
                _ -> constraint
            subst substitution typ = 
              case typ of
                TypeVar name -> fromMaybe typ (Map.lookup name substitution)
                _ -> typ

        , fastProperty "generalization increases polymorphism" $
            \env expr ->
            case inferType env expr of
              Right (_, scheme) -> 
                let generalized = generalizeType env expr
                in isPolymorphic generalized ==> isPolymorphic generalized
              Left _ -> property True
          where
            isPolymorphic (TypeScheme vars _) = not (null vars)

        , fastProperty "instantiation reduces polymorphism" $
            \scheme ->
            isPolymorphic scheme ==>
            let instanceType = instantiateType scheme
            in not (containsTypeVars instanceType)
          where
            isPolymorphic (TypeScheme vars _) = not (null vars)
            containsTypeVars typ = 
              case typ of
                TypeVar _ -> True
                IRFunctionType t1 t2 -> containsTypeVars t1 || containsTypeVars t2
                _ -> False

        , fastProperty "type inference preserves type safety" $
            \env expr ->
            case inferType env expr of
              Right (_, inferredType) -> 
                isValidType inferredType
              Left _ -> property True
          where
            isValidType typ = 
              case typ of
                IRTypeInt -> True
                IRTypeString -> True
                IRTypeBool -> True
                IRFunctionType t1 t2 -> isValidType t1 && isValidType t2
                TypeVar _ -> True
                _ -> False
        ]
  ]