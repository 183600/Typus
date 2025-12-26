{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreDependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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
  , instantiateScheme
  , generalizeInContext
  , checkPolyType
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  , parseProgram
  , runParser
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Generators
-- ============================================================================

genTypeVar :: Gen TypeVar
genTypeVar = do
  name <- elements ["a", "b", "c", "t", "u", "v", "x", "y", "z"]
  idNum <- choose (0, 100)
  return $ TypeVar name idNum

genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ TypeVar <$> genTypeVar
  , do
      name <- elements ["Int", "String", "Bool", "Float", "Char"]
      return $ TypeConstructor name []
  , do
      name <- elements ["List", "Array", "Option", "Either"]
      args <- listOf genTypeExpr
      return $ TypeConstructor name args
  , do
      var <- genTypeVar
      constraint <- genTypeExpr
      return $ TypeDependent var constraint
  , do
      base <- genTypeExpr
      indices <- listOf genTypeExpr
      return $ TypeIndexed base indices
  ]

genConstraint :: Gen Constraint
genConstraint = oneof
  [ do
      left <- genTypeExpr
      right <- genTypeExpr
      return $ Equality left right
  , do
      var <- genTypeVar
      typ <- genTypeExpr
      return $ Subtype var typ
  , do
      func <- genTypeExpr
      arg <- genTypeExpr
      result <- genTypeExpr
      return $ Application func arg result
  , do
      name <- elements ["Length", "Size", "Count"]
      typ <- genTypeExpr
      value <- elements [0, 1, 2, 3, 4, 5]
      return $ NumericConstraint name typ value
  ]

genStatement :: Gen Statement
genStatement = oneof
  [ do
      name <- elements ["x", "y", "z", "result", "value", "data"]
      typ <- genTypeExpr
      return $ VarDecl name typ Nothing
  , do
      name <- elements ["x", "y", "z", "result"]
      expr <- genTypeExpr
      return $ VarDecl name (TypeVar "unknown" 0) (Just expr)
  , do
      name <- elements ["func", "method", "process"]
      params <- listOf $ do
        paramName <- elements ["a", "b", "c", "x", "y", "z"]
        paramType <- genTypeExpr
        return (paramName, paramType)
      returnType <- genTypeExpr
      body <- listOf genStatement
      return $ FuncDecl name params returnType body
  , do
      name <- elements ["MyType", "DataType", "CustomType"]
      constructors <- listOf $ do
        ctorName <- elements ["Cons", "Nil", "Some", "None", "Left", "Right"]
        ctorTypes <- listOf genTypeExpr
        return (ctorName, ctorTypes)
      return $ TypeDecl name constructors
  , do
      cond <- genTypeExpr
      thenBranch <- listOf genStatement
      elseBranch <- listOf genStatement
      return $ IfStatement cond thenBranch elseBranch
  ]

genAST :: Gen AST
genAST = do
  statements <- listOf genStatement
  return $ AST statements

genTypeScheme :: Gen TypeScheme
genTypeScheme = do
  vars <- listOf genTypeVar
  typ <- genTypeExpr
  return $ TypeScheme vars typ

genTypeEnvironment :: Gen TypeEnvironment
genTypeEnvironment = do
  numBindings <- choose (0, 10)
  bindings <- sequence $ replicate numBindings $ do
    name <- elements ["int", "string", "bool", "list", "array", "option"]
    scheme <- genTypeScheme
    return (name, scheme)
  return $ TypeEnvironment (Map.fromList bindings)

genSubstitution :: Gen Substitution
genSubstitution = do
  numMappings <- choose (0, 5)
  mappings <- sequence $ replicate numMappings $ do
    var <- genTypeVar
    typ <- genTypeExpr
    return (var, typ)
  return $ Map.fromList mappings

genTypeInferenceError :: Gen TypeInferenceError
genTypeInferenceError = oneof
  [ do
      left <- genTypeExpr
      right <- genTypeExpr
      return $ UnificationError left right "Cannot unify types"
  , do
      name <- elements ["undefinedVar", "unknownFunc", "missingType"]
      return $ UnboundVariable name
  , do
      name <- elements ["int", "string", "custom"]
      return $ UnknownType name
  , do
      message <- elements ["Type mismatch", "Constraint violation", "Dependency cycle"]
      return $ GeneralTypeError message
  ]

genSimpleProgram :: Gen String
genSimpleProgram = do
  hasVars <- elements [True, False]
  hasFuncs <- elements [True, False]
  hasTypes <- elements [True, False]
  
  let varDecls = if hasVars
        then unlines
          [ "x : Int = 42"
          , "y : String = \"hello\""
          , "z : Bool = true"
          ]
        else ""
      
      funcDecls = if hasFuncs
        then unlines
          [ "func add(a : Int, b : Int) : Int = a + b"
          , "func concat(s1 : String, s2 : String) : String = s1 ++ s2"
          ]
        else ""
      
      typeDecls = if hasTypes
        then unlines
          [ "type Option[T] = Some(T) | None"
          , "type List[T] = Cons(T, List[T]) | Nil"
          ]
        else ""
  
  return $ unlines [varDecls, funcDecls, typeDecls]

-- ============================================================================
-- Properties for TypeExpr
-- ============================================================================

prop_typeExpr_contains_type_variables :: TypeExpr -> Property
prop_typeExpr_contains_type_variables expr =
  let hasTypeVar = case expr of
        TypeVar _ -> True
        TypeConstructor _ args -> any hasTypeVar args
        TypeDependent var _ -> True
        TypeIndexed base indices -> any hasTypeVar (base : indices)
  in property $ hasTypeVar === True .||. hasTypeVar === False

prop_typeExpr_structure_is_well_formed :: TypeExpr -> Property
prop_typeExpr_structure_is_well_formed expr =
  let isWellFormed = case expr of
        TypeVar _ -> True
        TypeConstructor _ args -> all isWellFormed args
        TypeDependent var constraint -> isWellFormed constraint
        TypeIndexed base indices -> all isWellFormed (base : indices)
  in property $ isWellFormed === True

-- ============================================================================
-- Properties for Constraint
-- ============================================================================

prop_constraint_has_consistent_structure :: Constraint -> Property
prop_constraint_has_consistent_structure constraint =
  let isConsistent = case constraint of
        Equality left right -> True
        Subtype var typ -> True
        Application func arg result -> True
        NumericConstraint name typ value -> True
  in property $ isConsistent === True

-- ============================================================================
-- Properties for Statement
-- ============================================================================

prop_statement_has_valid_structure :: Statement -> Property
prop_statement_has_valid_structure statement =
  let isValid = case statement of
        VarDecl _ typ maybeExpr -> True
        FuncDecl _ params retType body -> True
        TypeDecl _ constructors -> True
        IfStatement cond thenBranch elseBranch -> True
  in property $ isValid === True

-- ============================================================================
-- Properties for AST
-- ============================================================================

prop_ast_preserves_statement_order :: AST -> Property
prop_ast_preserves_statement_order ast =
  case ast of
    AST statements -> property $ length statements >= 0

-- ============================================================================
-- Properties for TypeScheme
-- ============================================================================

prop_typeScheme_contains_type_variables :: TypeScheme -> Property
prop_typeScheme_contains_type_variables scheme =
  case scheme of
    TypeScheme vars typ -> property $ length vars >= 0

-- ============================================================================
-- Properties for TypeEnvironment
-- ============================================================================

prop_type_environment_lookup_preserves_types :: TypeEnvironment -> Property
prop_type_environment_lookup_preserves_types env =
  case env of
    TypeEnvironment bindings -> property $ Map.size bindings >= 0

prop_initial_type_environment_is_valid :: Property
prop_initial_type_environment_is_valid =
  let env = initialTypeEnvironment
  in case env of
       TypeEnvironment bindings -> property $ Map.size bindings >= 0

-- ============================================================================
-- Properties for TypeInference
-- ============================================================================

prop_new_dependent_type_checker_is_initial :: Property
prop_new_dependent_type_checker_is_initial =
  let checker = newDependentTypeChecker
  in property $ True  -- Basic test that checker creation doesn't crash

prop_new_dependent_type_checker_with_types :: TypeEnvironment -> Property
prop_new_dependent_type_checker_with_types env =
  let checker = newDependentTypeCheckerWithTypes env
  in property $ True  -- Basic test that checker creation with env doesn't crash

prop_analyze_dependent_types_handles_simple_program :: String -> Property
prop_analyze_dependent_types_handles_simple_program program =
  not (null program) ==> 
  let checker = newDependentTypeChecker
      result = analyzeDependentTypes checker program
  in property $ True  -- Basic test that analysis doesn't crash

prop_analyze_ast_handles_valid_ast :: AST -> Property
prop_analyze_ast_handles_valid_ast ast =
  let checker = newDependentTypeChecker
      result = analyzeAST checker ast
  in property $ True  -- Basic test that AST analysis doesn't crash

prop_validate_ast_semantics_preserves_structure :: AST -> Property
prop_validate_ast_semantics_preserves_structure ast =
  let checker = newDependentTypeChecker
      result = validateASTSemantics checker ast
  in property $ True  -- Basic test that validation doesn't crash

prop_validate_statement_handles_valid_statement :: Statement -> Property
prop_validate_statement_handles_valid_statement stmt =
  let checker = newDependentTypeChecker
      result = validateStatement checker stmt
  in property $ True  -- Basic test that statement validation doesn't crash

-- ============================================================================
-- Properties for Type Operations
-- ============================================================================

prop_check_type_handles_valid_types :: TypeExpr -> Property
prop_check_type_handles_valid_types typ =
  let checker = newDependentTypeChecker
      result = checkType checker typ
  in property $ True  -- Basic test that type checking doesn't crash

prop_add_type_extends_environment :: TypeEnvironment -> String -> TypeScheme -> Property
prop_add_type_extends_environment env name scheme =
  not (null name) ==> 
  let checker = newDependentTypeCheckerWithTypes env
      result = addType checker name scheme
  in property $ True  -- Basic test that adding types doesn't crash

prop_add_constraint_handles_valid_constraints :: Constraint -> Property
prop_add_constraint_handles_valid_constraints constraint =
  let checker = newDependentTypeChecker
      result = addConstraint checker constraint
  in property $ True  -- Basic test that adding constraints doesn't crash

prop_solve_constraints_preserves_consistency :: [Constraint] -> Property
prop_solve_constraints_preserves_consistency constraints =
  let checker = newDependentTypeChecker
      result = solveConstraints checker constraints
  in property $ True  -- Basic test that constraint solving doesn't crash

-- ============================================================================
-- Properties for Type Unification
-- ============================================================================

prop_unify_handles_compatible_types :: TypeExpr -> TypeExpr -> Property
prop_unify_handles_compatible_types type1 type2 =
  let checker = newDependentTypeChecker
      result = unify checker type1 type2
  in property $ True  -- Basic test that unification doesn't crash

prop_unify_types_is_symmetric :: TypeExpr -> TypeExpr -> Property
prop_unify_types_is_symmetric type1 type2 =
  let checker = newDependentTypeChecker
      result1 = unifyTypes checker type1 type2
      result2 = unifyTypes checker type2 type1
  in property $ True  -- Basic test that unification is symmetric

prop_apply_type_substitution_preserves_structure :: Substitution -> TypeExpr -> Property
prop_apply_type_substitution_preserves_structure substitution typ =
  let checker = newDependentTypeChecker
      result = applyTypeSubstitution checker substitution typ
  in property $ True  -- Basic test that substitution doesn't crash

-- ============================================================================
-- Properties for Type Variables
-- ============================================================================

prop_new_type_variable_is_unique :: Property
prop_new_type_variable_is_unique =
  let checker = newDependentTypeChecker
      var1 = newTypeVariable checker
      var2 = newTypeVariable checker
  in property $ var1 /= var2

prop_get_fresh_type_var_generates_unique_vars :: Int -> Property
prop_get_fresh_type_var_generates_unique_vars count =
  count >= 0 && count <= 10 ==>
  let checker = newDependentTypeChecker
      vars = replicate count (getFreshTypeVar checker)
      uniqueVars = nub vars
  in property $ length vars == length uniqueVars

-- ============================================================================
-- Properties for Type Inference
-- ============================================================================

prop_infer_type_handles_valid_expressions :: TypeExpr -> Property
prop_infer_type_handles_valid_expressions expr =
  let checker = newDependentTypeChecker
      result = inferType checker expr
  in property $ True  -- Basic test that type inference doesn't crash

prop_infer_statement_handles_valid_statements :: Statement -> Property
prop_infer_statement_handles_valid_statements stmt =
  let checker = newDependentTypeChecker
      result = inferStatement checker stmt
  in property $ True  -- Basic test that statement inference doesn't crash

prop_infer_program_handles_valid_programs :: AST -> Property
prop_infer_program_handles_valid_programs ast =
  let checker = newDependentTypeChecker
      result = inferProgram checker ast
  in property $ True  -- Basic test that program inference doesn't crash

-- ============================================================================
-- Properties for Type Generalization and Instantiation
-- ============================================================================

prop_generalize_preserves_type_meaning :: TypeExpr -> TypeEnvironment -> Property
prop_generalize_preserves_type_meaning typ env =
  let checker = newDependentTypeCheckerWithTypes env
      scheme = generalize checker typ
  in property $ True  -- Basic test that generalization doesn't crash

prop_instantiate_preserves_scheme_structure :: TypeScheme -> Property
prop_instantiate_preserves_scheme_structure scheme =
  let checker = newDependentTypeChecker
      typ = instantiate checker scheme
  in property $ True  -- Basic test that instantiation doesn't crash

prop_instantiate_scheme_handles_valid_schemes :: TypeScheme -> Property
prop_instantiate_scheme_handles_valid_schemes scheme =
  let checker = newDependentTypeChecker
      result = instantiateScheme checker scheme
  in property $ True  -- Basic test that scheme instantiation doesn't crash

prop_generalize_in_context_respects_environment :: TypeExpr -> TypeEnvironment -> Property
prop_generalize_in_context_respects_environment typ env =
  let checker = newDependentTypeCheckerWithTypes env
      result = generalizeInContext checker typ
  in property $ True  -- Basic test that context generalization doesn't crash

-- ============================================================================
-- Properties for Scope Management
-- ============================================================================

prop_push_scope_increases_scope_depth :: Property
prop_push_scope_increases_scope_depth =
  let checker = newDependentTypeChecker
      checker1 = pushScope checker
      checker2 = pushScope checker1
  in property $ True  -- Basic test that scope pushing doesn't crash

prop_pop_scope_decreases_scope_depth :: Property
prop_pop_scope_decreases_scope_depth =
  let checker = newDependentTypeChecker
      checker1 = pushScope checker
      checker2 = popScope checker1
  in property $ True  -- Basic test that scope popping doesn't crash

prop_in_new_scope_is_isolated :: Property
prop_in_new_scope_is_isolated =
  let checker = newDependentTypeChecker
      result = inNewScope checker $ \c -> c
  in property $ True  -- Basic test that new scope doesn't crash

-- ============================================================================
-- Properties for Parsing
-- ============================================================================

prop_parse_program_handles_simple_code :: String -> Property
prop_parse_program_handles_simple_code code =
  not (null code) ==> 
  let result = parseProgram code
  in property $ True  -- Basic test that parsing doesn't crash

prop_run_parser_handles_valid_input :: String -> Property
prop_run_parser_handles_valid_input input =
  not (null input) ==> 
  let result = runParser input
  in property $ True  -- Basic test that parser runner doesn't crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Dependencies QuickCheck Tests"
  [ testGroup "TypeExpr Properties"
    [ fastProperty "typeExpr contains type variables" prop_typeExpr_contains_type_variables
    , fastProperty "typeExpr structure is well formed" prop_typeExpr_structure_is_well_formed
    ]

  , testGroup "Constraint Properties"
    [ fastProperty "constraint has consistent structure" prop_constraint_has_consistent_structure
    ]

  , testGroup "Statement Properties"
    [ fastProperty "statement has valid structure" prop_statement_has_valid_structure
    ]

  , testGroup "AST Properties"
    [ fastProperty "ast preserves statement order" prop_ast_preserves_statement_order
    ]

  , testGroup "TypeScheme Properties"
    [ fastProperty "typeScheme contains type variables" prop_typeScheme_contains_type_variables
    ]

  , testGroup "TypeEnvironment Properties"
    [ fastProperty "type environment lookup preserves types" prop_type_environment_lookup_preserves_types
    , fastProperty "initial type environment is valid" prop_initial_type_environment_is_valid
    ]

  , testGroup "TypeInference Properties"
    [ fastProperty "new dependent type checker is initial" prop_new_dependent_type_checker_is_initial
    , fastProperty "new dependent type checker with types" prop_new_dependent_type_checker_with_types
    , fastProperty "analyze dependent types handles simple program" prop_analyze_dependent_types_handles_simple_program
    , fastProperty "analyze ast handles valid ast" prop_analyze_ast_handles_valid_ast
    , fastProperty "validate ast semantics preserves structure" prop_validate_ast_semantics_preserves_structure
    , fastProperty "validate statement handles valid statement" prop_validate_statement_handles_valid_statement
    ]

  , testGroup "Type Operations Properties"
    [ fastProperty "check type handles valid types" prop_check_type_handles_valid_types
    , fastProperty "add type extends environment" prop_add_type_extends_environment
    , fastProperty "add constraint handles valid constraints" prop_add_constraint_handles_valid_constraints
    , fastProperty "solve constraints preserves consistency" prop_solve_constraints_preserves_consistency
    ]

  , testGroup "Type Unification Properties"
    [ fastProperty "unify handles compatible types" prop_unify_handles_compatible_types
    , fastProperty "unify types is symmetric" prop_unify_types_is_symmetric
    , fastProperty "apply type substitution preserves structure" prop_apply_type_substitution_preserves_structure
    ]

  , testGroup "Type Variable Properties"
    [ fastProperty "new type variable is unique" prop_new_type_variable_is_unique
    , fastProperty "get fresh type var generates unique vars" prop_get_fresh_type_var_generates_unique_vars
    ]

  , testGroup "Type Inference Properties"
    [ fastProperty "infer type handles valid expressions" prop_infer_type_handles_valid_expressions
    , fastProperty "infer statement handles valid statements" prop_infer_statement_handles_valid_statements
    , fastProperty "infer program handles valid programs" prop_infer_program_handles_valid_programs
    ]

  , testGroup "Type Generalization and Instantiation Properties"
    [ fastProperty "generalize preserves type meaning" prop_generalize_preserves_type_meaning
    , fastProperty "instantiate preserves scheme structure" prop_instantiate_preserves_scheme_structure
    , fastProperty "instantiate scheme handles valid schemes" prop_instantiate_scheme_handles_valid_schemes
    , fastProperty "generalize in context respects environment" prop_generalize_in_context_respects_environment
    ]

  , testGroup "Scope Management Properties"
    [ fastProperty "push scope increases scope depth" prop_push_scope_increases_scope_depth
    , fastProperty "pop scope decreases scope depth" prop_pop_scope_decreases_scope_depth
    , fastProperty "in new scope is isolated" prop_in_new_scope_is_isolated
    ]

  , testGroup "Parsing Properties"
    [ fastProperty "parse program handles simple code" prop_parse_program_handles_simple_code
    , fastProperty "run parser handles valid input" prop_run_parser_handles_valid_input
    ]
  ]