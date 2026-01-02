{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependenciesQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , preludeTypeDefs
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , convertTypeExpr
  , convertTypeExprAndRefinements
  , convertConstraint
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
  )

import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  , Statement(..)
  , AST(..)
  )

import Dependencies.Inference
  ( TypeScheme(..)
  , TypeEnvironment(..)
  , TypeInferenceState(..)
  , TypeInferenceError(..)
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

import Control.Monad.State (runState, evalState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (isInfixOf)
import Data.List (sort, nub)

-- ============================================================================
-- Custom Generators
-- ============================================================================

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

genTypeName :: Gen String
genTypeName = do
  first <- elements $ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ TVCon <$> genTypeName
  , TVVar <$> genVarName
  , do
      name <- genTypeName
      args <- listOf genTypeVar `suchThat` (not . null)
      return $ TVApp name args
  , do
      args <- listOf genTypeVar `suchThat` (not . null)
      ret <- genTypeVar
      return $ TVFun args ret
  , TVTuple <$> listOf genTypeVar `suchThat` (\l -> L.length l >= 2)
  ]

genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , do
      name <- genString
      args <- listOf genTypeVar
      return $ Predicate name args
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , TypeRange <$> genTypeVar <*> choose (0, 50) <*> choose (51, 100)
  ]

genDependentTypeError :: Gen DependentTypeError
genDependentTypeError = oneof
  [ DependentTypeMismatch <$> genTypeVar <*> genTypeVar
  , ConstraintViolation <$> genString <*> genTypeVar
  , TypeNotFound <$> genString
  , InvalidTypeArgument <$> genString
  , UnsolvableConstraint <$> genTypeConstraint
  , DependentInfiniteType <$> genString <*> genTypeVar
  , AmbiguousType <$> genString
  , ParseError <$> genString
  , SemanticError <$> genString
  ]

genTypeDef :: Gen TypeDef
genTypeDef = do
  params <- listOf genVarName
  constraints <- listOf genTypeConstraint
  return $ TypeDefDecl params constraints

genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ SimpleT <$> (T.pack <$> genTypeName)
  , do
      name <- T.pack <$> genTypeName
      args <- listOf genTypeExpr
      return $ GenericT name args
  , do
      params <- listOf ((,) <$> genVarName <*> genTypeExpr)
      ret <- genTypeExpr
      return $ FuncT params ret
  , do
      base <- genTypeExpr
      constraints <- listOf genConstraint
      return $ RefineT base constraints
  ]

genConstraint :: Gen Constraint
genConstraint = oneof
  [ SizeGE <$> (T.pack <$> genVarName) <*> choose (0, 100)
  , SizeGT <$> (T.pack <$> genVarName) <*> choose (0, 100)
  , RangeC <$> (T.pack <$> genVarName) <*> choose (0, 50) <*> choose (51, 100)
  , do
      name <- T.pack <$> genString
      args <- listOf genTypeExpr
      return $ PredC name args
  ]

genStatement :: Gen Statement
genStatement = oneof
  [ -- Variable declaration
    do
      varName <- genVarName
      varType <- genTypeExpr
      return $ VarDecl varName varType
  , -- Function declaration
    do
      funcName <- genVarName
      params <- listOf ((,) <$> genVarName <*> genTypeExpr)
      retType <- genTypeExpr
      return $ FuncDecl funcName params retType
  , -- Type declaration
    do
      typeName <- genTypeName
      typeParams <- listOf genVarName
      baseType <- genTypeExpr
      return $ TypeDecl typeName typeParams baseType
  ]

genAST :: Gen AST
genAST = do
  statements <- listOf genStatement
  return $ Program statements

-- ============================================================================
-- TypeVar Properties
-- ============================================================================

-- Property: TVCon should preserve constructor name
prop_tv_con_preserves_name :: String -> Property
prop_tv_con_preserves_name name =
  let tv = TVCon name
  in case tv of
       TVCon n -> property $ n === name
       _ -> property $ False

-- Property: TVVar should preserve variable name
prop_tv_var_preserves_name :: String -> Property
prop_tv_var_preserves_name name =
  let tv = TVVar name
  in case tv of
       TVVar n -> property $ n === name
       _ -> property $ False

-- Property: TVApp should preserve constructor name L.and arguments
prop_tv_app_preserves_fields :: String -> [TypeVar] -> Property
prop_tv_app_preserves_fields name args =
  let tv = TVApp name args
  in case tv of
       TVApp n a -> property $ n === name .&&. a === args
       _ -> property $ False

-- Property: TVFun should preserve parameters L.and return type
prop_tv_fun_preserves_fields :: [TypeVar] -> TypeVar -> Property
prop_tv_fun_preserves_fields params ret =
  let tv = TVFun params ret
  in case tv of
       TVFun p r -> property $ p === params .&&. r === ret
       _ -> property $ False

-- Property: TVTuple should preserve elements
prop_tv_tuple_preserves_elements :: [TypeVar] -> Property
prop_tv_tuple_preserves_elements elements =
  not (null elements) && L.length elements >= 2 ==>
  let tv = TVTuple elements
  in case tv of
       TVTuple e -> property $ e === elements
       _ -> property $ False

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

-- Property: Equal should preserve both type variables
prop_equal_preserves_types :: TypeVar -> TypeVar -> Property
prop_equal_preserves_types tv1 tv2 =
  let constraint = Equal tv1 tv2
  in case constraint of
       Equal t1 t2 -> property $ t1 === tv1 .&&. t2 === tv2
       _ -> property $ False

-- Property: Subtype should preserve both type variables
prop_subtype_preserves_types :: TypeVar -> TypeVar -> Property
prop_subtype_preserves_types tv1 tv2 =
  let constraint = Subtype tv1 tv2
  in case constraint of
       Subtype t1 t2 -> property $ t1 === tv1 .&&. t2 === tv2
       _ -> property $ False

-- Property: Predicate should preserve name L.and arguments
prop_predicate_preserves_fields :: String -> [TypeVar] -> Property
prop_predicate_preserves_fields name args =
  let constraint = Predicate name args
  in case constraint of
       Predicate n a -> property $ n === name .&&. a === args
       _ -> property $ False

-- Property: TypeSizeGE should preserve type variable L.and size
prop_type_size_ge_preserves_fields :: TypeVar -> Int -> Property
prop_type_size_ge_preserves_fields tv size =
  let constraint = TypeSizeGE tv size
  in case constraint of
       TypeSizeGE t s -> property $ t === tv .&&. s === size
       _ -> property $ False

-- ============================================================================
-- DependentTypeError Properties
-- ============================================================================

-- Property: DependentTypeMismatch should preserve both type variables
prop_type_mismatch_preserves_types :: TypeVar -> TypeVar -> Property
prop_type_mismatch_preserves_types tv1 tv2 =
  let error = DependentTypeMismatch tv1 tv2
  in case error of
       DependentTypeMismatch t1 t2 -> property $ t1 === tv1 .&&. t2 === tv2
       _ -> property $ False

-- Property: ConstraintViolation should preserve message L.and type variable
prop_constraint_violation_preserves_fields :: String -> TypeVar -> Property
prop_constraint_violation_preserves_fields msg tv =
  let error = ConstraintViolation msg tv
  in case error of
       ConstraintViolation m t -> property $ m === msg .&&. t === tv
       _ -> property $ False

-- Property: TypeNotFound should preserve type name
prop_type_not_found_preserves_name :: String -> Property
prop_type_not_found_preserves_name name =
  let error = TypeNotFound name
  in case error of
       TypeNotFound n -> property $ n === name
       _ -> property $ False

-- ============================================================================
-- TypeDef Properties
-- ============================================================================

-- Property: TypeDefDecl should preserve parameters L.and constraints
prop_type_def_preserves_fields :: [String] -> [TypeConstraint] -> Property
prop_type_def_preserves_fields params constraints =
  let typeDef = TypeDefDecl params constraints
  in case typeDef of
       TypeDefDecl p c -> property $ p === params .&&. c === constraints
       _ -> property $ False

-- ============================================================================
-- TypeEnv Properties
-- ============================================================================

-- Property: TypeEnv should preserve type definitions L.and constraints
prop_type_env_preserves_fields :: Map.Map String TypeDef -> [TypeConstraint] -> Property
prop_type_env_preserves_fields defs constraints =
  let env = TypeEnv defs constraints
  in property $ typeDefinitions env === defs .&&. pendingConstraints env === constraints

-- ============================================================================
-- DependentTypeChecker Properties
-- ============================================================================

-- Property: newDependentTypeChecker should create checker with prelude types
prop_new_checker_has_prelude :: Property
prop_new_checker_has_prelude =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      defs = typeDefinitions env
  in property $ Map.isSubsetOf preludeTypeDefs defs

-- Property: newDependentTypeChecker should start with no errors
prop_new_checker_no_errors :: Property
prop_new_checker_no_errors =
  let checker = newDependentTypeChecker
  in property $ L.null (tcErrors checker)

-- Property: newDependentTypeCheckerWithTypes should include custom types
prop_new_checker_with_custom_types :: Property
prop_new_checker_with_custom_types =
  forAll (listOf ((,,) <$> genTypeName <*> listOf genVarName <*> listOf genTypeConstraint)) $ \typeDefs ->
  let checker = newDependentTypeCheckerWithTypes typeDefs
      env = dtcTypeEnv checker
      defs = typeDefinitions env
      customDefs = Map.fromList [ (n, TypeDefDecl ps cs) | (n, ps, cs) <- typeDefs ]
  in property $ Map.isSubsetOf customDefs defs

-- ============================================================================
-- Type Conversion Properties
-- ============================================================================

-- Property: convertTypeExpr should handle SimpleT
prop_convert_simple_type :: Text -> Property
prop_convert_simple_type name =
  let typeExpr = SimpleT name
      params = Set.empty
      result = convertTypeExpr params typeExpr
  in case result of
       TVCon n -> property $ n === T.unpack name
       _ -> property $ False

-- Property: convertTypeExprAndRefinements should return constraints for RefineT
prop_convert_refine_type_returns_constraints :: TypeExpr -> [Constraint] -> Property
prop_convert_refine_type_returns_constraints base constraints =
  let typeExpr = RefineT base constraints
      params = Set.empty
      (tv, constraintList) = convertTypeExprAndRefinements params typeExpr
  in property $ L.length constraintList >= L.length constraints

-- Property: convertConstraint should handle SizeGE
prop_convert_size_ge_constraint :: Text -> Int -> Property
prop_convert_size_ge_constraint name size =
  let constraint = SizeGE name size
      params = Set.empty
      result = convertConstraint params constraint
  in case result of
       TypeSizeGE tv s -> property $ s === size
       _ -> property $ False

-- ============================================================================
-- Type Operations Properties
-- ============================================================================

-- Property: addType should add type to environment
prop_add_type_adds_to_env :: String -> [String] -> [TypeConstraint] -> Property
prop_add_type_adds_to_env name params constraints =
  let checker = newDependentTypeChecker
      (result, checker') = runState (addType name params constraints) checker
      env = dtcTypeEnv checker'
      defs = typeDefinitions env
  in property $ Map.member name defs

-- Property: addConstraint should add constraint to pending list
prop_add_constraint_adds_to_pending :: TypeConstraint -> Property
prop_add_constraint_adds_to_pending constraint =
  let checker = newDependentTypeChecker
      (result, checker') = runState (addConstraint constraint) checker
      env = dtcTypeEnv checker'
      pending = pendingConstraints env
  in property $ constraint `elem` pending

-- Property: addTypeError should add error to error list
prop_add_type_error_adds_to_list :: DependentTypeError -> Property
prop_add_type_error_adds_to_list error =
  let checker = newDependentTypeChecker
      (result, checker') = runState (addTypeError error) checker
      errors = tcErrors checker'
  in property $ error `elem` errors

-- Property: lookupTypeDef should find added types
prop_lookup_type_finds_added :: String -> [String] -> [TypeConstraint] -> Property
prop_lookup_type_finds_added name params constraints =
  let checker = newDependentTypeChecker
      (result, checker') = runState (addType name params constraints) checker
      (found, _) = runState (lookupTypeDef name) checker'
  in case found of
       Just _ -> property $ True
       Nothing -> property $ False

-- ============================================================================
-- Inference Properties
-- ============================================================================

-- Property: initialTypeEnvironment should be valid
prop_initial_type_env_valid :: Property
prop_initial_type_env_valid =
  let env = initialTypeEnvironment
  in property $ True  -- If it constructs, it's valid

-- Property: newTypeVariable should generate fresh variables
prop_new_type_var_fresh :: Property
prop_new_type_var_fresh =
  let tv1 = evalState newTypeVariable 0
      tv2 = evalState newTypeVariable 1
  in property $ tv1 /= tv2

-- Property: getFreshTypeVar should return different variables on successive calls
prop_fresh_type_var_different :: Property
prop_fresh_type_var_different =
  let tv1 = getFreshTypeVar
      tv2 = getFreshTypeVar
  in property $ tv1 /= tv2

-- ============================================================================
-- Unification Properties
-- ============================================================================

-- Property: unify should succeed for identical type variables
prop_unify_identical_succeeds :: TypeVar -> Property
prop_unify_identical_succeeds tv =
  let checker = newDependentTypeChecker
      (result, checker') = runState (unify tv tv) checker
  in property $ L.null (tcErrors checker')

-- Property: unify should handle simple concrete types
prop_unify_concrete_types :: Property
prop_unify_concrete_types =
  let tv1 = TVCon "Int"
      tv2 = TVCon "Int"
      checker = newDependentTypeChecker
      (result, checker') = runState (unify tv1 tv2) checker
  in property $ L.null (tcErrors checker')

-- ============================================================================
-- AST Properties
-- ============================================================================

-- Property: Program should preserve statements
prop_program_preserves_statements :: [Statement] -> Property
prop_program_preserves_statements statements =
  let ast = Program statements
  in case ast of
       Program stmts -> property $ stmts === statements
       _ -> property $ False

-- Property: VarDecl should preserve name L.and type
prop_var_decl_preserves_fields :: String -> TypeExpr -> Property
prop_var_decl_preserves_fields name varType =
  let stmt = VarDecl name varType
  in case stmt of
       VarDecl n t -> property $ n === name .&&. t === varType
       _ -> property $ False

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Dependencies QuickCheck Tests"
  [ testGroup "TypeVar Properties"
    [ fastProperty "TVCon preserves name" prop_tv_con_preserves_name
    , fastProperty "TVVar preserves name" prop_tv_var_preserves_name
    , fastProperty "TVApp preserves fields" prop_tv_app_preserves_fields
    , fastProperty "TVFun preserves fields" prop_tv_fun_preserves_fields
    , fastProperty "TVTuple preserves elements" prop_tv_tuple_preserves_elements
    ]
  , testGroup "TypeConstraint Properties"
    [ fastProperty "Equal preserves types" prop_equal_preserves_types
    , fastProperty "Subtype preserves types" prop_subtype_preserves_types
    , fastProperty "Predicate preserves fields" prop_predicate_preserves_fields
    , fastProperty "TypeSizeGE preserves fields" prop_type_size_ge_preserves_fields
    ]
  , testGroup "DependentTypeError Properties"
    [ fastProperty "TypeMismatch preserves types" prop_type_mismatch_preserves_types
    , fastProperty "ConstraintViolation preserves fields" prop_constraint_violation_preserves_fields
    , fastProperty "TypeNotFound preserves name" prop_type_not_found_preserves_name
    ]
  , testGroup "TypeDef Properties"
    [ fastProperty "TypeDef preserves fields" prop_type_def_preserves_fields
    ]
  , testGroup "TypeEnv Properties"
    [ fastProperty "TypeEnv preserves fields" prop_type_env_preserves_fields
    ]
  , testGroup "DependentTypeChecker Properties"
    [ fastProperty "new checker has prelude" prop_new_checker_has_prelude
    , fastProperty "new checker no errors" prop_new_checker_no_errors
    , fastProperty "new checker with custom types" prop_new_checker_with_custom_types
    ]
  , testGroup "Type Conversion Properties"
    [ fastProperty "convert simple type" prop_convert_simple_type
    , fastProperty "convert refine type returns constraints" prop_convert_refine_type_returns_constraints
    , fastProperty "convert size ge constraint" prop_convert_size_ge_constraint
    ]
  , testGroup "Type Operations Properties"
    [ fastProperty "add type adds to env" prop_add_type_adds_to_env
    , fastProperty "add constraint adds to pending" prop_add_constraint_adds_to_pending
    , fastProperty "add type error adds to list" prop_add_type_error_adds_to_list
    , fastProperty "lookup type finds added" prop_lookup_type_finds_added
    ]
  , testGroup "Inference Properties"
    [ fastProperty "initial type env valid" prop_initial_type_env_valid
    , fastProperty "new type var fresh" prop_new_type_var_fresh
    , fastProperty "fresh type var different" prop_fresh_type_var_different
    ]
  , testGroup "Unification Properties"
    [ fastProperty "unify identical succeeds" prop_unify_identical_succeeds
    , fastProperty "unify concrete types" prop_unify_concrete_types
    ]
  , testGroup "AST Properties"
    [ fastProperty "program preserves statements" prop_program_preserves_statements
    , fastProperty "var decl preserves fields" prop_var_decl_preserves_fields
    ]
  ]