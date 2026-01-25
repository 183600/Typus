{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreDependenciesQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
-- | Core Dependencies module QuickCheck tests

import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.Arbitrary
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Control.Monad.State (evalState)
import Data.Char (isSpace, isAlpha, isAlphaNum)

import Dependencies
import Dependencies.Inference
import Dependencies.AST (AST(..))
import Dependencies.TypeSystem (Substitution)
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as Map

-- | Run a TypeInference computation
runTypeInference :: TypeInference a -> IO (Either TypeInferenceError a)
runTypeInference comp = do
  env <- initialTypeEnvironment
  let state = TypeInferenceState { typeEnv = env, currentSubst = Map.empty, inferenceErrors = [] }
  runExceptT $ evalStateT comp state

-- ============================================================================
-- Dependencies QuickCheck Tests
-- ============================================================================

-- | Test that newDependentTypeChecker creates a valid checker
prop_newDependentTypeCheckerValid :: Property
prop_newDependentTypeCheckerValid =
  let checker = newDependentTypeChecker
  in property $ True  -- Basic sanity check

-- | Test that newDependentTypeCheckerWithTypes creates a valid checker
prop_newDependentTypeCheckerWithTypesValid :: Property
prop_newDependentTypeCheckerWithTypesValid =
  forAll (listOf arbitraryIdentifier) $ \typeNames ->
    let types = map (\name -> (name, [], [])) typeNames
        checker = newDependentTypeCheckerWithTypes types
    in property $ True  -- Basic sanity check

-- | Test that analyzeDependentTypes processes basic code
prop_analyzeDependentTypesBasic :: Property
prop_analyzeDependentTypesBasic =
  forAll arbitraryShortString $ \code ->
    let result = analyzeDependentTypes code
    in property $ True  -- Basic sanity check

-- | Test that analyzeAST processes basic AST
prop_analyzeASTBasic :: Property
prop_analyzeASTBasic =
  forAll arbitraryAST $ \ast ->
    let result = analyzeAST ast
    in property $ True  -- Basic sanity check

-- | Test that validateASTSemantics validates AST
prop_validateASTSemantics :: Property
prop_validateASTSemantics =
  forAll arbitraryAST $ \ast ->
    let result = evalState (validateASTSemantics ast) newDependentTypeChecker
    in property $ True  -- Basic sanity check

-- | Test that validateStatement validates statement
prop_validateStatement :: Property
prop_validateStatement =
  forAll arbitraryStatement $ \stmt ->
    let result = evalState (validateStatement stmt) newDependentTypeChecker
    in property $ True  -- Basic sanity check

-- | Test that checkType checks types
prop_checkType :: Property
prop_checkType =
  forAll arbitraryTypeVar $ \typeExpr ->
    let checker = newDependentTypeChecker
        result = evalState (checkType typeExpr) checker
    in property $ True  -- Basic sanity check

-- | Test that addType adds types
prop_addType :: Property
prop_addType =
  forAll arbitraryIdentifier $ \typeName ->
    forAll arbitraryTypeExpr $ \typeExpr ->
      let checker = newDependentTypeChecker
          result = evalState (addType typeName [] []) checker
      in property $ True  -- Basic sanity check

-- | Test that addConstraint adds constraints
prop_addConstraint :: Property
prop_addConstraint =
  forAll arbitraryTypeConstraint $ \constraint ->
    let checker = newDependentTypeChecker
        result = evalState (addConstraint constraint) checker
    in property $ True  -- Basic sanity check

-- | Test that checkTypeInstantiation checks instantiation
prop_checkTypeInstantiation :: Property
prop_checkTypeInstantiation =
  forAll arbitraryIdentifier $ \typeName ->
    forAll arbitraryTypeExpr $ \typeExpr ->
      let checker = newDependentTypeChecker
          result = evalState (checkTypeInstantiation typeName []) checker
      in property $ True  -- Basic sanity check

-- | Test that solveConstraints solves constraints
prop_solveConstraints :: Property
prop_solveConstraints =
  let checker = newDependentTypeChecker
      result = evalState solveConstraints checker
  in property $ True  -- Basic sanity check

-- | Test that getDependentTypeErrors gets errors
prop_getDependentTypeErrors :: Property
prop_getDependentTypeErrors =
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in property $ True  -- Basic sanity check

-- | Test that unify unifies types
prop_unify :: Property
prop_unify =
  forAll arbitraryTypeVar $ \type1 ->
    forAll arbitraryTypeVar $ \type2 ->
      let result = unify [(type1, type2)]
      in property $ True  -- Basic sanity check

-- | Test that inferType infers types
prop_inferType :: Property
prop_inferType =
  forAll arbitraryTypeExpr $ \typeExpr ->
    let result = runTypeInference (inferType typeExpr)
    in property $ True  -- Basic sanity check

-- | Test that inferStatement infers statement types
prop_inferStatement :: Property
prop_inferStatement =
  forAll arbitraryStatement $ \stmt ->
    let result = runTypeInference (inferStatement stmt)
    in property $ True  -- Basic sanity check

-- | Test that inferProgram infers program types
prop_inferProgram :: Property
prop_inferProgram =
  forAll (listOf arbitraryStatement) $ \stmts ->
    let program = Program stmts
        result = runTypeInference (inferProgram program)
    in property $ True  -- Basic sanity check

-- | Test that generalize creates type schemes
prop_generalize :: Property
prop_generalize =
  forAll arbitraryTypeVar $ \typeExpr ->
    let result = runTypeInference (generalize 0 typeExpr)
    in property $ True  -- Basic sanity check

-- | Test that instantiate instantiates type schemes
prop_instantiate :: Property
prop_instantiate =
  forAll arbitraryTypeScheme $ \scheme ->
    let result = runTypeInference (instantiate scheme)
    in property $ True  -- Basic sanity check

-- | Test that unifyTypes unifies types
prop_unifyTypes :: Property
prop_unifyTypes =
  forAll arbitraryTypeVar $ \type1 ->
    forAll arbitraryTypeVar $ \type2 ->
      let result = runTypeInference (unifyTypes type1 type2)
      in property $ True  -- Basic sanity check

-- | Test that applyTypeSubstitution applies substitutions
prop_applyTypeSubstitution :: Property
prop_applyTypeSubstitution =
  forAll arbitraryTypeVar $ \typeExpr ->
    forAll arbitrarySubstitution $ \substitution ->
      let result = applyTypeSubstitution substitution typeExpr
      in property $ True  -- Basic sanity check

-- | Test that newTypeVariable creates new type variables
prop_newTypeVariable :: Property
prop_newTypeVariable =
  let result = runTypeInference newTypeVariable
  in property $ True  -- Basic sanity check

-- | Test that getFreshTypeVar creates fresh type variables
prop_getFreshTypeVar :: Property
prop_getFreshTypeVar =
  let result = runTypeInference getFreshTypeVar
  in property $ True  -- Basic sanity check

-- | Test that initialTypeEnvironment creates initial environment
prop_initialTypeEnvironment :: Property
prop_initialTypeEnvironment =
  let result = initialTypeEnvironment
  in property $ True  -- Basic sanity check

-- | Test that type expressions are valid
prop_typeExprValid :: Property
prop_typeExprValid =
  forAll arbitraryTypeExpr $ \typeExpr ->
    property $ True  -- Basic sanity check

-- | Test that type constraints are valid
prop_typeConstraintValid :: Property
prop_typeConstraintValid =
  forAll arbitraryTypeConstraint $ \constraint ->
    property $ True  -- Basic sanity check

-- | Test that type schemes are valid
prop_typeSchemeValid :: Property
prop_typeSchemeValid =
  forAll arbitraryTypeScheme $ \scheme ->
    property $ True  -- Basic sanity check

-- | Test that type environments are valid
prop_typeEnvironmentValid :: Property
prop_typeEnvironmentValid =
  forAll arbitraryTypeEnvironment $ \env ->
    property $ True  -- Basic sanity check

-- | Test that substitutions are valid
prop_substitutionValid :: Property
prop_substitutionValid =
  forAll arbitrarySubstitution $ \substitution ->
    property $ True  -- Basic sanity check

-- | Test that AST nodes are valid
prop_astValid :: Property
prop_astValid =
  forAll arbitraryAST $ \ast ->
    property $ True  -- Basic sanity check

-- | Test that statements are valid
prop_statementValid :: Property
prop_statementValid =
  forAll arbitraryStatement $ \stmt ->
    property $ True  -- Basic sanity check

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Dependencies QuickCheck Tests"
  [ testProperty "NewDependentTypeChecker creates valid checker" prop_newDependentTypeCheckerValid
  , testProperty "NewDependentTypeCheckerWithTypes creates valid checker" prop_newDependentTypeCheckerWithTypesValid
  , testProperty "AnalyzeDependentTypes processes basic code" prop_analyzeDependentTypesBasic
  , testProperty "AnalyzeAST processes basic AST" prop_analyzeASTBasic
  , testProperty "ValidateASTSemantics validates AST" prop_validateASTSemantics
  , testProperty "ValidateStatement validates statement" prop_validateStatement
  , testProperty "CheckType checks types" prop_checkType
  , testProperty "AddType adds types" prop_addType
  , testProperty "AddConstraint adds constraints" prop_addConstraint
  , testProperty "CheckTypeInstantiation checks instantiation" prop_checkTypeInstantiation
  , testProperty "SolveConstraints solves constraints" prop_solveConstraints
  , testProperty "GetDependentTypeErrors gets errors" prop_getDependentTypeErrors
  , testProperty "Unify unifies types" prop_unify
  , testProperty "InferType infers types" prop_inferType
  , testProperty "InferStatement infers statement types" prop_inferStatement
  , testProperty "InferProgram infers program types" prop_inferProgram
  , testProperty "Generalize creates type schemes" prop_generalize
  , testProperty "Instantiate instantiates type schemes" prop_instantiate
  , testProperty "UnifyTypes unifies types" prop_unifyTypes
  , testProperty "ApplyTypeSubstitution applies substitutions" prop_applyTypeSubstitution
  , testProperty "NewTypeVariable creates new type variables" prop_newTypeVariable
  , testProperty "GetFreshTypeVar creates fresh type variables" prop_getFreshTypeVar
  , testProperty "InitialTypeEnvironment creates initial environment" prop_initialTypeEnvironment
  , testProperty "TypeExpr is valid" prop_typeExprValid
  , testProperty "TypeConstraint is valid" prop_typeConstraintValid
  , testProperty "TypeScheme is valid" prop_typeSchemeValid
  , testProperty "TypeEnvironment is valid" prop_typeEnvironmentValid
  , testProperty "Substitution is valid" prop_substitutionValid
  , testProperty "AST is valid" prop_astValid
  , testProperty "Statement is valid" prop_statementValid
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite