{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
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

import Dependencies.TypeSystem
  ( TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , preludeTypeDefs
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  , convertTypeExpr
  , convertTypeExprAndRefinements
  , convertConstraint
  )

import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  , Statement(..)
  , AST(..)
  )

import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (length, isPrefixOf)
import Data.List (sort, null, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- TypeVar Properties
-- ============================================================================

-- Property: New type variables are unique
prop_new_type_variables_unique :: Int -> Property
prop_new_type_variables_unique count =
  count > 0 && count <= 100 ==>
  let checker = newDependentTypeChecker
      typeVars = take count (iterate (const (newTypeVariable checker)) (newTypeVariable checker))
  in property $ L.length (nub typeVars) === L.length typeVars

-- Property: Fresh type variables are different
prop_fresh_type_variables_different :: Property
prop_fresh_type_variables_different =
  let checker = newDependentTypeChecker
      var1 = getFreshTypeVar checker
      var2 = getFreshTypeVar checker
  in property $ var1 /= var2

-- ============================================================================
-- TypeExpr Properties
-- ============================================================================

-- Property: Simple type expressions are equal if names are equal
prop_simple_type_equality :: String -> Property
prop_simple_type_equality typeName =
  not (null typeName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) typeName ==>
  let type1 = SimpleT (T.pack typeName)
      type2 = SimpleT (T.pack typeName)
  in property $ type1 === type2

-- Property: Generic type expressions preserve parameters
prop_generic_type_preserves_params :: String -> [String] -> Property
prop_generic_type_preserves_params baseName params =
  not (null baseName) && L.all (not . null) params && L.all (L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"])) params ==>
  let baseType = SimpleT (T.pack baseName)
      paramTypes = L.map (\p -> SimpleT (T.pack p)) params
      genericType = GenericT baseType paramTypes
  in property $ L.length (typeArgs genericType) === L.length params

-- Property: Function type expressions preserve domain L.and codomain
prop_function_type_preserves_domain_codomain :: String -> String -> Property
prop_function_type_preserves_domain_codomain domain codomain =
  not (null domain) && not (null codomain) ==>
  let domainType = SimpleT (T.pack domain)
      codomainType = SimpleT (T.pack codomain)
      funcType = FuncT domainType codomainType
  in property $ funcDomain funcType === domainType .&&.
             funcCodomain funcType === codomainType

-- ============================================================================
-- Constraint Properties
-- ============================================================================

-- Property: Range constraints preserve bounds
prop_range_constraint_preserves_bounds :: Int -> Int -> Property
prop_range_constraint_preserves_bounds lower upper =
  let range = RangeC (fromIntegral lower) (fromIntegral upper)
  in property $ rangeLower range === fromIntegral lower .&&.
             rangeUpper range === fromIntegral upper

-- Property: Predicate constraints preserve predicate name
prop_predicate_constraint_preserves_name :: String -> Property
prop_predicate_constraint_preserves_name predName =
  not (null predName) ==>
  let pred = PredC (T.pack predName)
  in property $ predName === T.unpack (predName' pred)

-- Property: Size constraints preserve threshold
prop_size_constraint_preserves_threshold :: Int -> Property
prop_size_constraint_preserves_threshold threshold =
  let sizeGE = SizeGE (fromIntegral threshold)
      sizeGT = SizeGT (fromIntegral threshold)
  in property $ sizeThreshold sizeGE === fromIntegral threshold .&&.
             sizeThreshold sizeGT === fromIntegral threshold

-- ============================================================================
-- TypeEnvironment Properties
-- ============================================================================

-- Property: Initial type environment contains prelude types
prop_initial_env_contains_prelude :: Property
prop_initial_env_contains_prelude =
  let env = initialTypeEnvironment
      preludeNames = Map.keys preludeTypeDefs
  in property $ L.all (`Map.member` typeDefs env) preludeNames

-- Property: Adding type to environment makes it available
prop_add_type_makes_available :: String -> Property
prop_add_type_makes_available typeName =
  not (null typeName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) typeName ==>
  let checker = newDependentTypeChecker
      typeDef = TypeDef (T.pack typeName) [] Nothing
      checker' = addType typeDef checker
  in property $ isJust (lookupTypeDef (T.pack typeName) checker')

-- Property: Adding constraint preserves existing types
prop_add_constraint_preserves_types :: String -> String -> Property
prop_add_constraint_preserves_types typeName constraintName =
  not (null typeName) && not (null constraintName) ==>
  let checker = newDependentTypeChecker
      typeDef = TypeDef (T.pack typeName) [] Nothing
      constraint = TypeConstraint (T.pack constraintName) []
      checker1 = addType typeDef checker
      checker2 = addConstraint constraint checker1
  in property $ isJust (lookupTypeDef (T.pack typeName) checker2)

-- ============================================================================
-- Type Checking Properties
-- ============================================================================

-- Property: Checking simple type in prelude succeeds
prop_check_simple_prelude_type :: String -> Property
prop_check_simple_prelude_type typeName =
  typeName `elem` ["Int", "String", "Bool"] ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
  in property $ checkType typeExpr checker

-- Property: Checking undefined type fails
prop_check_undefined_type_fails :: String -> Property
prop_check_undefined_type_fails typeName =
  not (null typeName) && not (typeName `elem` ["Int", "String", "Bool"]) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) typeName ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
  in property $ not (checkType typeExpr checker)

-- Property: Type instantiation preserves type structure
prop_type_instantiation_preserves_structure :: String -> Property
prop_type_instantiation_preserves_structure typeName =
  typeName `elem` ["Int", "String", "Bool"] ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
      result = checkTypeInstantiation typeExpr checker
  in property $ isJust result

-- ============================================================================
-- Constraint Solving Properties
-- ============================================================================

-- Property: Solving empty constraints returns empty substitution
prop_solve_empty_constraints :: Property
prop_solve_empty_constraints =
  let checker = newDependentTypeChecker
      substitution = solveConstraints [] checker
  in property $ null substitution

-- Property: Solving consistent constraints succeeds
prop_solve_consistent_constraints :: String -> Property
prop_solve_consistent_constraints typeName =
  typeName `elem` ["Int", "String", "Bool"] ==>
  let checker = newDependentTypeChecker
      typeExpr = SimpleT (T.pack typeName)
      constraint = TypeConstraint (T.pack "size") [typeExpr]
      substitution = solveConstraints [constraint] checker
  in property $ not (null substitution)

-- ============================================================================
-- Unification Properties
-- ============================================================================

-- Property: Unifying identical types succeeds
prop_unify_identical_types :: String -> Property
prop_unify_identical_types typeName =
  typeName `elem` ["Int", "String", "Bool"] ==>
  let checker = newDependentTypeChecker
      type1 = SimpleT (T.pack typeName)
      type2 = SimpleT (T.pack typeName)
      result = unify type1 type2 checker
  in property $ isJust result

-- Property: Unification is symmetric
prop_unification_symmetric :: TypeExpr -> TypeExpr -> Property
prop_unification_symmetric type1 type2 =
  let checker = newDependentTypeChecker
      result1 = unify type1 type2 checker
      result2 = unify type2 type1 checker
  in property $ isJust result1 === isJust result2

-- Property: Unification with type variables succeeds
prop_unification_with_type_var :: Property
prop_unification_with_type_var =
  let checker = newDependentTypeChecker
      typeVar = newTypeVariable checker
      concreteType = SimpleT (T.pack "Int")
      result = unify typeVar concreteType checker
  in property $ isJust result

-- ============================================================================
-- AST Analysis Properties
-- ============================================================================

-- Property: Analyzing empty AST succeeds
prop_analyze_empty_ast :: Property
prop_analyze_empty_ast =
  let ast = AST { astStatements = [], astImports = [] }
      checker = newDependentTypeChecker
      result = analyzeAST ast checker
  in property $ isJust result

-- Property: Analyzing AST with simple statements works
prop_analyze_simple_ast :: String -> Property
prop_analyze_simple_ast varName =
  not (null varName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) varName ==>
  let statement = VariableDeclaration (T.pack varName) (SimpleT (T.pack "Int")) Nothing
      ast = AST { astStatements = [statement], astImports = [] }
      checker = newDependentTypeChecker
      result = analyzeAST ast checker
  in property $ isJust result

-- Property: Validating simple statement works
prop_validate_simple_statement :: String -> Property
prop_validate_simple_statement varName =
  not (null varName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) varName ==>
  let statement = VariableDeclaration (T.pack varName) (SimpleT (T.pack "Int")) Nothing
      checker = newDependentTypeChecker
      result = validateStatement statement checker
  in property $ isJust result

-- ============================================================================
-- Type Inference Properties
-- ============================================================================

-- Property: Inferring type of simple expression works
prop_infer_simple_expression :: String -> Property
prop_infer_simple_expression varName =
  not (null varName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) varName ==>
  let env = initialTypeEnvironment
      expr = Variable (T.pack varName)
      result = inferType expr env
  in property $ isJust result

-- Property: Generalizing type preserves type variables
prop_generalize_preserves_type_vars :: TypeExpr -> Property
prop_generalize_preserves_type_vars typeExpr =
  let env = initialTypeEnvironment
      scheme = generalize typeExpr env
  in property $ isJust scheme

-- Property: Instantiating generalized type produces concrete type
prop_instantiate_generalized_type :: TypeExpr -> Property
prop_instantiate_generalized_type typeExpr =
  let env = initialTypeEnvironment
      maybeScheme = generalize typeExpr env
  in case maybeScheme of
       Nothing -> property False
       Just scheme -> 
         let result = instantiate scheme env
         in property $ isJust result

-- ============================================================================
-- Type Substitution Properties
-- ============================================================================

-- Property: Applying empty substitution returns original type
prop_apply_empty_substitution :: TypeExpr -> Property
prop_apply_empty_substitution typeExpr =
  let substitution = []
      result = applyTypeSubstitution substitution typeExpr
  in property $ result === typeExpr

-- Property: Applying substitution twice is idempotent
prop_apply_substitution_idempotent :: TypeExpr -> TypeExpr -> Property
prop_apply_substitution_idempotent type1 type2 =
  let substitution = [(type1, type2)]
      result1 = applyTypeSubstitution substitution type1
      result2 = applyTypeSubstitution substitution result1
  in property $ result1 === result2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Type errors contain location information
prop_type_errors_have_location :: String -> SourcePos -> Property
prop_type_errors_have_location errorMsg pos =
  not (null errorMsg) ==>
  let error = DependentTypeError (T.pack errorMsg) pos
  in property $ errorPosition error === pos

-- Property: Collecting errors preserves error messages
prop_collect_errors_preserves_messages :: [String] -> Property
prop_collect_errors_preserves_messages errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let checker = newDependentTypeChecker
      errors = L.map (\msg -> DependentTypeError (T.pack msg) startPos) errorMessages
      checker' = L.foldr (\err acc -> addTypeError err acc) checker errors
      collectedErrors = getDependentTypeErrors checker'
  in property $ L.length collectedErrors === L.length errorMessages

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies New QuickCheck Tests"
  [ testGroup "TypeVar"
    [ fastProperty "new type variables unique" prop_new_type_variables_unique
    , fastProperty "fresh type variables different" prop_fresh_type_variables_different
    ]
  , testGroup "TypeExpr"
    [ fastProperty "simple type equality" prop_simple_type_equality
    , fastProperty "generic type preserves params" prop_generic_type_preserves_params
    , fastProperty "function type preserves domain codomain" prop_function_type_preserves_domain_codomain
    ]
  , testGroup "Constraint"
    [ fastProperty "range constraint preserves bounds" prop_range_constraint_preserves_bounds
    , fastProperty "predicate constraint preserves name" prop_predicate_constraint_preserves_name
    , fastProperty "size constraint preserves threshold" prop_size_constraint_preserves_threshold
    ]
  , testGroup "TypeEnvironment"
    [ fastProperty "initial env contains prelude" prop_initial_env_contains_prelude
    , fastProperty "add type makes available" prop_add_type_makes_available
    , fastProperty "add constraint preserves types" prop_add_constraint_preserves_types
    ]
  , testGroup "TypeChecking"
    [ fastProperty "check simple prelude type" prop_check_simple_prelude_type
    , fastProperty "check undefined type fails" prop_check_undefined_type_fails
    , fastProperty "type instantiation preserves structure" prop_type_instantiation_preserves_structure
    ]
  , testGroup "ConstraintSolving"
    [ fastProperty "solve empty constraints" prop_solve_empty_constraints
    , fastProperty "solve consistent constraints" prop_solve_consistent_constraints
    ]
  , testGroup "Unification"
    [ fastProperty "unify identical types" prop_unify_identical_types
    , fastProperty "unification symmetric" prop_unification_symmetric
    , fastProperty "unification with type var" prop_unification_with_type_var
    ]
  , testGroup "ASTAnalysis"
    [ fastProperty "analyze empty AST" prop_analyze_empty_ast
    , fastProperty "analyze simple AST" prop_analyze_simple_ast
    , fastProperty "validate simple statement" prop_validate_simple_statement
    ]
  , testGroup "TypeInference"
    [ fastProperty "infer simple expression" prop_infer_simple_expression
    , fastProperty "generalize preserves type vars" prop_generalize_preserves_type_vars
    , fastProperty "instantiate generalized type" prop_instantiate_generalized_type
    ]
  , testGroup "TypeSubstitution"
    [ fastProperty "apply empty substitution" prop_apply_empty_substitution
    , fastProperty "apply substitution idempotent" prop_apply_substitution_idempotent
    ]
  , testGroup "ErrorHandling"
    [ fastProperty "type errors have location" prop_type_errors_have_location
    , fastProperty "collect errors preserves messages" prop_collect_errors_preserves_messages
    ]
  ]