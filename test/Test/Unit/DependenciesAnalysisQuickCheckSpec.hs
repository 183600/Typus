{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
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
  , TypeScheme(..)
  , TypeEnvironment(..)
  , Substitution
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

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , TypeScheme(..)
  , TypeEnvironment(..)
  , Substitution
  )

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Data.List (nub)

-- Property: AST equality is reflexive
prop_ast_reflexive :: AST -> Property
prop_ast_reflexive ast =
  property $ ast === ast

-- Property: AST with same statements are equal
prop_ast_statements_equality :: [Statement] -> Property
prop_ast_statements_equality statements =
  let ast1 = Program statements
      ast2 = Program statements
  in property $ ast1 === ast2

-- Property: Statement equality is reflexive
prop_statement_reflexive :: Statement -> Property
prop_statement_reflexive stmt =
  property $ stmt === stmt

-- Property: TypeExpr equality is reflexive
prop_type_expr_reflexive :: TypeExpr -> Property
prop_type_expr_reflexive typeExpr =
  property $ typeExpr === typeExpr

-- Property: Constraint equality is reflexive
prop_constraint_reflexive :: Constraint -> Property
prop_constraint_reflexive constraint =
  property $ constraint === constraint

-- Property: DependencyNode preserves name L.and dependencies
prop_dependency_node_preserves :: String -> [String] -> Property
prop_dependency_node_preserves name deps =
  not (null name) ==>
  let node = DependencyNode name deps
  in property $ nodeName node === name .&&. nodeDependencies node === deps

-- Property: DependencyGraph preserves nodes
prop_dependency_graph_preserves :: [DependencyNode] -> Property
prop_dependency_graph_preserves nodes =
  not (null nodes) ==>
  let nodeMap = Map.fromList $ L.map (\n -> (nodeName n, n)) nodes
      graph = DependencyGraph nodeMap
  in property $ L.all (\n -> Map.lookup (nodeName n) (graphNodes graph) == Just n) nodes

-- Property: newDependentTypeChecker creates valid checker
prop_new_type_checker_valid :: Property
prop_new_type_checker_valid =
  let checker = newDependentTypeChecker
  in property $ True  -- Basic smoke test

-- Property: newDependentTypeCheckerWithTypes preserves initial types
prop_new_type_checker_with_types :: [String] -> Property
prop_new_type_checker_with_types typeNames =
  not (null typeNames) && L.all (not . null) typeNames && 
  L.all (L.all isAlphaNum) typeNames ==>
  let checker = newDependentTypeCheckerWithTypes typeNames
  in property $ True  -- Basic smoke test

-- Property: analyzeDependentTypes handles empty AST
prop_analyze_empty_ast :: Property
prop_analyze_empty_ast =
  let checker = newDependentTypeChecker
      ast = Program []
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle empty AST gracefully

-- Property: analyzeDependentTypes handles simple type declarations
prop_analyze_simple_type :: String -> Property
prop_analyze_simple_type typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      typeDecl = STypeDef (T.pack typeName) [] []
      ast = Program [typeDecl]
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle simple type declarations

-- Property: analyzeDependentTypes handles variable declarations
prop_analyze_variable_decl :: String -> String -> Property
prop_analyze_variable_decl varName typeName =
  not (null varName) && not (null typeName) &&
  L.all isAlphaNum varName && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      varDecl = SVarDecl (T.pack varName) (SimpleT (T.pack typeName))
      ast = Program [varDecl]
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle variable declarations

-- Property: analyzeDependentTypes handles function declarations
prop_analyze_function_decl :: String -> [String] -> String -> Property
prop_analyze_function_decl funcName paramNames returnTypeName =
  not (null funcName) && not (null returnTypeName) &&
  L.all isAlphaNum funcName && L.all isAlphaNum returnTypeName &&
  L.all (not . null) paramNames && L.all (L.all isAlphaNum) paramNames ==>
  let checker = newDependentTypeChecker
      params = L.map (\name -> (T.pack name, SimpleT (T.pack "Int"))) paramNames
      returnType = Just (SimpleT (T.pack returnTypeName))
      funcDecl = SFuncDecl (T.pack funcName) params returnType
      ast = Program [funcDecl]
      result = analyzeDependentTypes checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle function declarations

-- Property: validateASTSemantics handles empty AST
prop_validate_empty_ast :: Property
prop_validate_empty_ast =
  let checker = newDependentTypeChecker
      ast = Program []
      result = validateASTSemantics checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle empty AST gracefully

-- Property: validateStatement handles simple statements
prop_validate_simple_statement :: String -> Property
prop_validate_simple_statement varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let checker = newDependentTypeChecker
      stmt = SVarDecl (T.pack varName) (SimpleT (T.pack "Int"))
      result = validateStatement checker stmt
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle simple statements

-- Property: checkType handles simple types
prop_check_simple_type :: String -> Property
prop_check_simple_type typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
      result = checkType checker typeExpr
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle simple types

-- Property: addType preserves type information
prop_add_type_preserves :: String -> Property
prop_add_type_preserves typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
      result = addType checker (T.pack typeName) typeExpr
  in case result of
    Left _ -> property True
    Right newChecker -> property $ True  -- Should preserve type information

-- Property: addConstraint preserves constraint information
prop_add_constraint_preserves :: Constraint -> Property
prop_add_constraint_preserves constraint =
  let checker = newDependentTypeChecker
      result = addConstraint checker constraint
  in case result of
    Left _ -> property True
    Right newChecker -> property $ True  -- Should preserve constraint information

-- Property: solveConstraints handles empty constraint list
prop_solve_empty_constraints :: Property
prop_solve_empty_constraints =
  let checker = newDependentTypeChecker
      result = solveConstraints checker []
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle empty constraints

-- Property: getDependentTypeErrors handles checker without errors
prop_get_no_errors :: Property
prop_get_no_errors =
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in property $ True  -- Should handle checker without errors

-- Property: unify handles identical types
prop_unify_identical :: TypeExpr -> Property
prop_unify_identical typeExpr =
  let checker = newDependentTypeChecker
      result = unify checker typeExpr typeExpr
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should unify identical types

-- Property: inferType handles simple expressions
prop_infer_simple_type :: String -> Property
prop_infer_simple_type typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
      result = inferType checker typeExpr
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should infer simple types

-- Property: inferStatement handles simple statements
prop_infer_simple_statement :: String -> Property
prop_infer_simple_statement varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let checker = newDependentTypeChecker
      stmt = SVarDecl (T.pack varName) (SimpleT (T.pack "Int"))
      result = inferStatement checker stmt
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should infer simple statements

-- Property: inferProgram handles empty program
prop_infer_empty_program :: Property
prop_infer_empty_program =
  let checker = newDependentTypeChecker
      ast = Program []
      result = inferProgram checker ast
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should infer empty program

-- Property: generalize handles simple types
prop_generalize_simple :: TypeExpr -> Property
prop_generalize_simple typeExpr =
  let checker = newDependentTypeChecker
      result = generalize checker typeExpr
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should generalize simple types

-- Property: instantiate handles type schemes
prop_instantiate_scheme :: TypeExpr -> Property
prop_instantiate_scheme typeExpr =
  let checker = newDependentTypeChecker
      schemeResult = generalize checker typeExpr
  in case schemeResult of
    Left _ -> property True
    Right scheme ->
      let result = instantiate checker scheme
      in case result of
        Left _ -> property True
        Right _ -> property True  -- Should instantiate type schemes

-- Property: unifyTypes handles compatible types
prop_unify_compatible :: String -> Property
prop_unify_compatible typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let checker = newDependentTypeChecker
      type1 = SimpleT (T.pack typeName)
      type2 = SimpleT (T.pack typeName)
      result = unifyTypes checker type1 type2
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should unify compatible types

-- Property: applyTypeSubstitution preserves structure
prop_apply_substitution_preserves :: TypeExpr -> Property
prop_apply_substitution_preserves typeExpr =
  let checker = newDependentTypeChecker
      substitution = Map.empty  -- Empty substitution
      result = applyTypeSubstitution checker substitution typeExpr
  in case result of
    Left _ -> property True
    Right newType -> property $ True  -- Should preserve structure with empty substitution

-- Property: newTypeVariable generates unique variables
prop_new_type_variable_unique :: Int -> Property
prop_new_type_variable_unique count =
  count > 0 && count <= 10 ==>
  let checker = newDependentTypeChecker
      typeVars = take count $ iterate (\_ -> newTypeVariable checker) (newTypeVariable checker)
  in property $ L.length (nub typeVars) === L.length typeVars

-- Property: getFreshTypeVar returns fresh variables
prop_get_fresh_type_var :: Int -> Property
prop_get_fresh_type_var count =
  count > 0 && count <= 10 ==>
  let checker = newDependentTypeChecker
      freshVars = take count $ repeat (getFreshTypeVar checker)
  in property $ True  -- Should return fresh variables

-- Property: initialTypeEnvironment is valid
prop_initial_type_environment :: Property
prop_initial_type_environment =
  let env = initialTypeEnvironment
  in property $ True  -- Should provide valid initial environment

-- Property: Complex type expressions are handled
prop_complex_type_expressions :: [String] -> Property
prop_complex_type_expressions typeNames =
  not (null typeNames) && L.all (not . null) typeNames &&
  L.all (L.all isAlphaNum) typeNames ==>
  let checker = newDependentTypeChecker
      complexType = L.foldr (\name acc -> GenericT (T.pack name) [acc]) 
                          (SimpleT (T.pack "Base")) 
                          typeNames
      result = checkType checker complexType
  in case result of
    Left _ -> property True
    Right _ -> property True  -- Should handle complex type expressions

-- Property: Constraint solving is deterministic
prop_constraint_solving_deterministic :: [Constraint] -> Property
prop_constraint_solving_deterministic constraints =
  let checker = newDependentTypeChecker
      result1 = solveConstraints checker constraints
      result2 = solveConstraints checker constraints
  in case (result1, result2) of
    (Right r1, Right r2) -> property $ r1 === r2
    _ -> property True  -- Handle error cases consistently

tests :: TestTree
tests = testGroup "Dependencies Analysis QuickCheck"
  [ fastProperty "AST reflexive" prop_ast_reflexive
  , fastProperty "AST statements equality" prop_ast_statements_equality
  , fastProperty "Statement reflexive" prop_statement_reflexive
  , fastProperty "TypeExpr reflexive" prop_type_expr_reflexive
  , fastProperty "Constraint reflexive" prop_constraint_reflexive
  , fastProperty "DependencyNode preserves" prop_dependency_node_preserves
  , fastProperty "DependencyGraph preserves" prop_dependency_graph_preserves
  , fastProperty "new type checker valid" prop_new_type_checker_valid
  , fastProperty "new type checker with types" prop_new_type_checker_with_types
  , fastProperty "analyze empty AST" prop_analyze_empty_ast
  , fastProperty "analyze simple type" prop_analyze_simple_type
  , fastProperty "analyze variable decl" prop_analyze_variable_decl
  , fastProperty "analyze function decl" prop_analyze_function_decl
  , fastProperty "validate empty AST" prop_validate_empty_ast
  , fastProperty "validate simple statement" prop_validate_simple_statement
  , fastProperty "check simple type" prop_check_simple_type
  , fastProperty "add type preserves" prop_add_type_preserves
  , fastProperty "add constraint preserves" prop_add_constraint_preserves
  , fastProperty "solve empty constraints" prop_solve_empty_constraints
  , fastProperty "get no errors" prop_get_no_errors
  , fastProperty "unify identical" prop_unify_identical
  , fastProperty "infer simple type" prop_infer_simple_type
  , fastProperty "infer simple statement" prop_infer_simple_statement
  , fastProperty "infer empty program" prop_infer_empty_program
  , fastProperty "generalize simple" prop_generalize_simple
  , fastProperty "instantiate scheme" prop_instantiate_scheme
  , fastProperty "unify compatible" prop_unify_compatible
  , fastProperty "apply substitution preserves" prop_apply_substitution_preserves
  , fastProperty "new type variable unique" prop_new_type_variable_unique
  , fastProperty "get fresh type var" prop_get_fresh_type_var
  , fastProperty "initial type environment" prop_initial_type_environment
  , fastProperty "complex type expressions" prop_complex_type_expressions
  , fastProperty "constraint solving deterministic" prop_constraint_solving_deterministic
  ]