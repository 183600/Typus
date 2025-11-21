module Test.Unit.TypeSystemSpec (tests) where

import Control.Monad.State (execState, runState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )

import Dependencies.AST (Constraint(..), TypeExpr(..))
import qualified Dependencies.TypeSystem as TS

-- | Unit tests for the core dependent type checker primitives.
tests :: TestTree
tests =
  testGroup "Type system"
    [ testCase "prelude exposes primitive definitions" $ do
        let defs = TS.typeDefinitions (TS.dtcTypeEnv TS.newDependentTypeChecker)
        assertBool "expected int prelude type" (Map.member "int" defs)
        assertBool "expected string prelude type" (Map.member "string" defs)

    , testCase "checkType accepts known types" $ do
        let (_, checker') = runState (TS.checkType (TS.TVCon "int")) TS.newDependentTypeChecker
        TS.getDependentTypeErrors checker' @?= []

    , testCase "checkType reports unknown types" $ do
        let (_, checker') = runState (TS.checkType (TS.TVCon "Unknown")) TS.newDependentTypeChecker
        assertBool "expected unresolved type error" (not (null (TS.getDependentTypeErrors checker')))

    , testCase "checkTypeInstantiation validates type arity" $ do
        let checkerWithList = execState (TS.addType "List" ["T"] []) TS.newDependentTypeChecker
            (_, checker') = runState (TS.checkTypeInstantiation "List" [TS.TVCon "int", TS.TVCon "string"]) checkerWithList
        assertBool "expected arity error" (not (null (TS.getDependentTypeErrors checker')))

    , testCase "checkTypeInstantiation accepts valid instantiation" $ do
        let checkerWithList = execState (TS.addType "List" ["T"] []) TS.newDependentTypeChecker
            (_, checker') = runState (TS.checkTypeInstantiation "List" [TS.TVCon "int"]) checkerWithList
        TS.getDependentTypeErrors checker' @?= []

    , testCase "solveConstraints flags conflicting equalities" $ do
        let withConstraint = execState (TS.addConstraint (TS.Equal (TS.TVCon "int") (TS.TVCon "string"))) TS.newDependentTypeChecker
            (result, checker') = runState TS.solveConstraints withConstraint
        assertBool "expected solveConstraints failure" (not result)
        assertBool "expected mismatch recorded" (not (null (TS.getDependentTypeErrors checker')))

    , testCase "unify produces substitutions" $ do
        case TS.unify [(TS.TVVar "T", TS.TVCon "int")] of
          Nothing -> assertFailure "expected substitution for type variable"
          Just subst -> assertBool "missing substitution" (("T", TS.TVCon "int") `elem` subst)

    , testCase "unify enforces occurs check" $ do
        case TS.unify [(TS.TVVar "T", TS.TVApp "List" [TS.TVVar "T"])] of
          Nothing -> pure ()
          Just _ -> assertFailure "expected occurs check to reject infinite type"

    , testCase "convertTypeExprAndRefinements accumulates refinement constraints" $ do
        let params = Set.fromList ["T"]
            typeExpr =
              RefineT
                (GenericT "Vector" [SimpleT "T"])
                [ SizeGE "values" 2
                , PredC "Ordered" [SimpleT "T"]
                ]
            (tv, constraints) = TS.convertTypeExprAndRefinements params typeExpr
        tv @?= TS.TVApp "Vector" [TS.TVVar "T"]
        constraints @?=
          [ TS.TypeSizeGE (TS.TVVar "values") 2
          , TS.Predicate "Ordered" [TS.TVVar "T"]
          ]

    , testCase "convertConstraint preserves nested generic predicate arguments" $ do
        let params = Set.fromList ["Element"]
            constraint =
              PredC "EnsureOrder"
                [ GenericT "List" [SimpleT "Element"]
                , SimpleT "Standalone"
                ]
        TS.convertConstraint params constraint
          @?= TS.Predicate "EnsureOrder"
                [ TS.TVApp "List" [TS.TVVar "Element"]
                , TS.TVCon "Standalone"
                ]

    , testCase "convertTypeExprAndRefinements handles refined function returns" $ do
        let params = Set.fromList ["T"]
            funcType =
              FuncT
                [("input", SimpleT "T")]
                (RefineT (SimpleT "Result") [SizeGT "input" 0])
            (tv, constraints) = TS.convertTypeExprAndRefinements params funcType
        tv @?= TS.TVFun [TS.TVVar "T"] (TS.TVCon "Result")
        constraints @?=
          [ TS.TypeSizeGT (TS.TVVar "input") 0
          ]
    ]
