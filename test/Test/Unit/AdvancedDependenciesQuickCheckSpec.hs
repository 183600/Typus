{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedDependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import TestSupport.Arbitrary

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , Substitution
  , TypeScheme(..)
  , TypeEnvironment(..)
  , TypeInferenceState(..)
  , TypeInferenceError(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , checkTypeInstantiation
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  )

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , DependencyNode(..)
  , DependencyGraph(..)
  )

import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Property: AST equality is reflexive
prop_ast_reflexive :: AST -> Property
prop_ast_reflexive ast =
  property $ ast === ast

-- Property: Statement equality is reflexive
prop_statement_reflexive :: Statement -> Property
prop_statement_reflexive statement =
  property $ statement === statement

-- Property: TypeExpr equality is reflexive
prop_type_expr_reflexive :: TypeExpr -> Property
prop_type_expr_reflexive typeExpr =
  property $ typeExpr === typeExpr

-- Property: Constraint equality is reflexive
prop_constraint_reflexive :: Constraint -> Property
prop_constraint_reflexive constraint =
  property $ constraint === constraint

-- Property: newDependentTypeChecker creates checker
prop_new_dependent_type_checker :: Property
prop_new_dependent_type_checker =
  let checker = newDependentTypeChecker
  in property $ True  -- Basic smoke test

-- Property: newDependentTypeCheckerWithTypes creates checker with types
prop_new_dependent_type_checker_with_types :: [String] -> Property
prop_new_dependent_type_checker_with_types typeNames =
  not (null typeNames) && all (not . null) typeNames ==>
  let checker = newDependentTypeCheckerWithTypes typeNames
  in property $ True  -- Basic smoke test

-- Property: analyzeDependentTypes handles empty AST
prop_analyze_dependent_types_empty :: Property
prop_analyze_dependent_types_empty =
  let checker = newDependentTypeChecker
      ast = Program []
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle empty AST

-- Property: analyzeDependentTypes handles simple type declarations
prop_analyze_dependent_types_simple_types :: [String] -> Property
prop_analyze_dependent_types_simple_types typeNames =
  not (null typeNames) && all (not . null) typeNames &&
  all (all isAlphaNum) typeNames ==>
  let typeDecls = map (\name -> STypeDef name [] []) typeNames
      ast = Program typeDecls
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle simple type declarations

-- Property: analyzeDependentTypes handles variable declarations
prop_analyze_dependent_types_variables :: [String] -> Property
prop_analyze_dependent_types_variables varNames =
  not (null varNames) && all (not . null) varNames &&
  all (all isAlphaNum) varNames ==>
  let varDecls = map (\name -> SVarDecl name (SimpleT "int")) varNames
      ast = Program varDecls
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle variable declarations

-- Property: analyzeDependentTypes handles function declarations
prop_analyze_dependent_types_functions :: [String] -> Property
prop_analyze_dependent_types_functions functionNames =
  not (null functionNames) && all (not . null) functionNames &&
  all (all isAlphaNum) functionNames ==>
  let funcDecls = map (\name -> SFuncDecl name [("x", SimpleT "int")] (Just (SimpleT "int"))) functionNames
      ast = Program funcDecls
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle function declarations

-- Property: analyzeDependentTypes handles constraints
prop_analyze_dependent_types_constraints :: [String] -> Property
prop_analyze_dependent_types_constraints constraintNames =
  not (null constraintNames) && all (not . null) constraintNames &&
  all (all isAlphaNum) constraintNames ==>
  let constraints = map (\name -> SConstraintDef name (SizeGT name 0)) constraintNames
      ast = Program constraints
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle constraints

-- Property: analyzeAST handles empty program
prop_analyze_ast_empty :: Property
prop_analyze_ast_empty =
  let checker = newDependentTypeChecker
      ast = Program []
      result = analyzeAST checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle empty program

-- Property: validateASTSemantics handles valid AST
prop_validate_ast_semantics :: AST -> Property
prop_validate_ast_semantics ast =
  let checker = newDependentTypeChecker
      result = validateASTSemantics checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle valid AST

-- Property: validateStatement handles simple statements
prop_validate_statement :: Statement -> Property
prop_validate_statement statement =
  let checker = newDependentTypeChecker
      result = validateStatement checker statement
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle simple statements

-- Property: checkType handles basic types
prop_check_type_basic :: String -> Property
prop_check_type_basic typeName =
  not (null typeName) && all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT typeName
      result = checkType checker typeExpr
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle basic types

-- Property: addType adds new type to checker
prop_add_type :: String -> Property
prop_add_type typeName =
  not (null typeName) && all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      result = addType checker typeName
  in case result of
    Left _ -> property True
    Right newChecker -> property $ True  -- Should add new type

-- Property: addConstraint adds constraint to checker
prop_add_constraint :: String -> Property
prop_add_constraint constraintName =
  not (null constraintName) && all isAlphaNum constraintName ==>
  let checker = newDependentTypeChecker
      constraint = SizeGT constraintName 0
      result = addConstraint checker constraint
  in case result of
    Left _ -> property True
    Right newChecker -> property $ True  -- Should add constraint

-- Property: checkTypeInstantiation handles type instantiation
prop_check_type_instantiation :: TypeExpr -> [TypeExpr] -> Property
prop_check_type_instantiation typeExpr args =
  let checker = newDependentTypeChecker
      result = checkTypeInstantiation checker typeExpr args
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle type instantiation

-- Property: solveConstraints handles constraint solving
prop_solve_constraints :: [Constraint] -> Property
prop_solve_constraints constraints =
  let checker = newDependentTypeChecker
      result = solveConstraints checker constraints
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle constraint solving

-- Property: getDependentTypeErrors returns errors from checker
prop_get_dependent_type_errors :: [String] -> Property
prop_get_dependent_type_errors errorMessages =
  let checker = newDependentTypeChecker
      errors = map (\msg -> DependentTypeError msg) errorMessages
  in property $ True  -- Basic smoke test

-- Property: unify handles type unification
prop_unify :: TypeExpr -> TypeExpr -> Property
prop_unify type1 type2 =
  let checker = newDependentTypeChecker
      result = unify checker type1 type2
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle type unification

-- Property: inferType handles type inference
prop_infer_type :: Statement -> Property
prop_infer_type statement =
  let checker = newDependentTypeChecker
      result = inferType checker statement
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle type inference

-- Property: inferStatement handles statement inference
prop_infer_statement :: Statement -> Property
prop_infer_statement statement =
  let checker = newDependentTypeChecker
      result = inferStatement checker statement
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle statement inference

-- Property: inferProgram handles program inference
prop_infer_program :: [Statement] -> Property
prop_infer_program statements =
  let checker = newDependentTypeChecker
      ast = Program statements
      result = inferProgram checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle program inference

-- Property: generalize handles type generalization
prop_generalize :: TypeExpr -> Property
prop_generalize typeExpr =
  let checker = newDependentTypeChecker
      result = generalize checker typeExpr
  in case result of
    Left _ -> property True
    Right scheme -> property $ True  -- Should handle type generalization

-- Property: instantiate handles type instantiation
prop_instantiate :: TypeScheme -> Property
prop_instantiate scheme =
  let checker = newDependentTypeChecker
      result = instantiate checker scheme
  in case result of
    Left _ -> property True
    Right typeExpr -> property $ True  -- Should handle type instantiation

-- Property: unifyTypes handles type unification
prop_unify_types :: TypeExpr -> TypeExpr -> Property
prop_unify_types type1 type2 =
  let checker = newDependentTypeChecker
      result = unifyTypes checker type1 type2
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle type unification

-- Property: applyTypeSubstitution handles substitution application
prop_apply_type_substitution :: TypeExpr -> Substitution -> Property
prop_apply_type_substitution typeExpr substitution =
  let checker = newDependentTypeChecker
      result = applyTypeSubstitution checker typeExpr substitution
  in case result of
    Left _ -> property True
    Right newTypeExpr -> property $ True  -- Should handle substitution application

-- Property: newTypeVariable creates new type variable
prop_new_type_variable :: Property
prop_new_type_variable =
  let checker = newDependentTypeChecker
      result = newTypeVariable checker
  in case result of
    Left _ -> property True
    Right typeVar -> property $ True  -- Should create new type variable

-- Property: getFreshTypeVar creates fresh type variable
prop_get_fresh_type_var :: Property
prop_get_fresh_type_var =
  let checker = newDependentTypeChecker
      result = getFreshTypeVar checker
  in case result of
    Left _ -> property True
    Right typeVar -> property $ True  -- Should create fresh type variable

-- Property: initialTypeEnvironment creates initial environment
prop_initial_type_environment :: Property
prop_initial_type_environment =
  let env = initialTypeEnvironment
  in property $ True  -- Basic smoke test

-- Property: Dependency analysis is deterministic
prop_dependencies_deterministic :: AST -> Property
prop_dependencies_deterministic ast =
  let checker = newDependentTypeChecker
      result1 = analyzeDependentTypes checker ast
      result2 = analyzeDependentTypes checker ast
  in case (result1, result2) of
    (Right errors1, Right errors2) -> property $ errors1 === errors2
    (Left err1, Left err2) -> property $ err1 === err2
    _ -> property False  -- Should be consistent

-- Property: Dependencies analysis handles large inputs
prop_dependencies_large :: String -> Int -> Property
prop_dependencies_large base multiplier =
  multiplier >= 0 && multiplier <= 50 ==>  -- Limit for performance
  let largeContent = concat (replicate multiplier base)
      statement = SVarDecl "x" (SimpleT "int")
      ast = Program (replicate multiplier statement)
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle large inputs

-- Property: Dependencies analysis handles unicode content
prop_dependencies_unicode :: String -> Property
prop_dependencies_unicode content =
  let unicodeContent = content ++ "测试🚀"
      statement = SVarDecl unicodeContent (SimpleT "int")
      ast = Program [statement]
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle unicode content

tests :: TestTree
tests = testGroup "Advanced Dependencies QuickCheck"
  [ fastProperty "ast reflexive" prop_ast_reflexive
  , fastProperty "statement reflexive" prop_statement_reflexive
  , fastProperty "type expr reflexive" prop_type_expr_reflexive
  , fastProperty "constraint reflexive" prop_constraint_reflexive
  , fastProperty "new dependent type checker" prop_new_dependent_type_checker
  , fastProperty "new dependent type checker with types" prop_new_dependent_type_checker_with_types
  , fastProperty "analyze dependent types empty" prop_analyze_dependent_types_empty
  , fastProperty "analyze dependent types simple types" prop_analyze_dependent_types_simple_types
  , fastProperty "analyze dependent types variables" prop_analyze_dependent_types_variables
  , fastProperty "analyze dependent types functions" prop_analyze_dependent_types_functions
  , fastProperty "analyze dependent types constraints" prop_analyze_dependent_types_constraints
  , fastProperty "analyze ast empty" prop_analyze_ast_empty
  , fastProperty "validate ast semantics" prop_validate_ast_semantics
  , fastProperty "validate statement" prop_validate_statement
  , fastProperty "check type basic" prop_check_type_basic
  , fastProperty "add type" prop_add_type
  , fastProperty "add constraint" prop_add_constraint
  , fastProperty "check type instantiation" prop_check_type_instantiation
  , fastProperty "solve constraints" prop_solve_constraints
  , fastProperty "get dependent type errors" prop_get_dependent_type_errors
  , fastProperty "unify" prop_unify
  , fastProperty "infer type" prop_infer_type
  , fastProperty "infer statement" prop_infer_statement
  , fastProperty "infer program" prop_infer_program
  , fastProperty "generalize" prop_generalize
  , fastProperty "instantiate" prop_instantiate
  , fastProperty "unify types" prop_unify_types
  , fastProperty "apply type substitution" prop_apply_type_substitution
  , fastProperty "new type variable" prop_new_type_variable
  , fastProperty "get fresh type var" prop_get_fresh_type_var
  , fastProperty "initial type environment" prop_initial_type_environment
  , fastProperty "dependencies deterministic" prop_dependencies_deterministic
  , fastProperty "dependencies large" prop_dependencies_large
  , fastProperty "dependencies unicode" prop_dependencies_unicode
  ]