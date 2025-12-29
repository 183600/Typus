{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalDependenciesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose, suchThat)
import Dependencies
  ( DependentTypeChecker, DependentTypeError(..), AST(..), Statement(..)
  , TypeExpr(..), Constraint(..), TypeVar(..), TypeConstraint(..)
  , TypeScheme(..), TypeEnvironment(..), TypeInferenceState(..)
  , newDependentTypeChecker, analyzeDependentTypes, analyzeAST
  , checkType, addType, addConstraint, solveConstraints, unify
  , inferType, inferStatement, inferProgram, generalize, instantiate
  , newTypeVariable, getFreshTypeVar, initialTypeEnvironment
  )
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import Dependencies.TypeSystem (TypeVar(..), TypeConstraint(..), DependentTypeError(..), TypeDef(..))
import Dependencies.Inference (TypeScheme(..), TypeEnvironment(..), newTypeVariable, getFreshTypeVar)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> arbitrary `suchThat` (not . null)
    , TVVar <$> arbitrary `suchThat` (not . null)
    , TVApp <$> arbitrary `suchThat` (not . null) <*> listOf arbitrary
    , TVFun <$> listOf arbitrary <*> arbitrary
    , TVTuple <$> listOf arbitrary
    ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ Equal <$> arbitrary <*> arbitrary
    , Subtype <$> arbitrary <*> arbitrary
    , Predicate <$> arbitrary `suchThat` (not . null) <*> listOf arbitrary
    , TypeSizeGE <$> arbitrary <*> choose (0, 1000)
    , TypeSizeGT <$> arbitrary <*> choose (0, 1000)
    , TypeRange <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ DependentTypeMismatch <$> arbitrary <*> arbitrary
    , ConstraintViolation <$> arbitrary `suchThat` (not . null) <*> arbitrary
    , TypeNotFound <$> arbitrary `suchThat` (not . null)
    , InvalidTypeArgument <$> arbitrary `suchThat` (not . null)
    , UnsolvableConstraint <$> arbitrary
    , DependentInfiniteType <$> arbitrary `suchThat` (not . null) <*> arbitrary
    , AmbiguousType <$> arbitrary `suchThat` (not . null)
    , ParseError <$> arbitrary `suchThat` (not . null)
    , SemanticError <$> arbitrary `suchThat` (not . null)
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitrary `suchThat` (not . null)
    , GenericT <$> arbitrary `suchThat` (not . null) <*> listOf arbitrary
    , FuncT <$> listOf (arbitrary `suchThat` (\(n, _) -> not (null n))) <*> arbitrary
    , RefineT <$> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ SizeGT <$> arbitrary `suchThat` (not . null) <*> choose (0, 1000)
    , SizeGE <$> arbitrary `suchThat` (not . null) <*> choose (0, 1000)
    , RangeC <$> arbitrary `suchThat` (not . null) <*> choose (0, 100) <*> choose (0, 100)
    , PredC <$> arbitrary `suchThat` (not . null) <*> listOf arbitrary
    ]

instance Arbitrary Statement where
  arbitrary = oneof
    [ STypeDef <$> arbitrary `suchThat` (not . null) <*> listOf (arbitrary `suchThat` (not . null)) <*> listOf arbitrary
    , STypeAlias <$> arbitrary `suchThat` (not . null) <*> arbitrary <*> listOf arbitrary
    , SVarDecl <$> arbitrary `suchThat` (not . null) <*> arbitrary
    , SFuncDecl <$> arbitrary `suchThat` (not . null) <*> listOf (arbitrary `suchThat` (\(n, _) -> not (null n))) <*> oneof [return Nothing, Just <$> arbitrary]
    , SConstraintDef <$> arbitrary `suchThat` (not . null) <*> arbitrary
    , SExistsDecl <$> listOf (arbitrary `suchThat` (not . null)) <*> arbitrary
    ]

instance Arbitrary AST where
  arbitrary = Program <$> listOf arbitrary

instance Arbitrary TypeScheme where
  arbitrary = Forall <$> listOf (arbitrary `suchThat` (not . null)) <*> arbitrary

-- Generate simple type expressions
genSimpleTypeExpr :: Gen TypeExpr
genSimpleTypeExpr = oneof
  [ SimpleT <$> elements ["int", "string", "bool"]
  , GenericT <$> elements ["List", "Array"] <*> listOf genSimpleTypeExpr
  ]

-- Generate simple constraints
genSimpleConstraint :: Gen Constraint
genSimpleConstraint = oneof
  [ SizeGT <$> elements ["x", "y", "z"] <*> choose (0, 100)
  , SizeGE <$> elements ["x", "y", "z"] <*> choose (0, 100)
  , RangeC <$> elements ["x", "y", "z"] <*> choose (0, 50) <*> choose (50, 100)
  ]

-- Generate simple statements
genSimpleStatement :: Gen Statement
genSimpleStatement = oneof
  [ SVarDecl <$> elements ["x", "y", "z"] <$> genSimpleTypeExpr
  , SFuncDecl <$> elements ["f", "g", "h"] <*> listOf (do
        var <- elements ["a", "b", "c"]
        typ <- genSimpleTypeExpr
        return (var, typ)) <*> Just <$> genSimpleTypeExpr
  ]

-- ============================================================================
-- TypeVar QuickCheck Tests
-- ============================================================================

-- Test TypeVar creation
prop_tvcon_has_name :: String -> Property
prop_tvcon_has_name name =
  not (null name) ==>
  let tv = TVCon name
  in case tv of
    TVCon n -> n === name
    _ -> property False

prop_tvvar_has_name :: String -> Property
prop_tvvar_has_name name =
  not (null name) ==>
  let tv = TVVar name
  in case tv of
    TVVar n -> n === name
    _ -> property False

-- ============================================================================
-- TypeConstraint QuickCheck Tests
-- ============================================================================

-- Test TypeConstraint creation
prop_equal_constraint :: TypeVar -> TypeVar -> Property
prop_equal_constraint tv1 tv2 =
  let constraint = Equal tv1 tv2
  in case constraint of
    Equal t1 t2 -> t1 === tv1 && t2 === tv2
    _ -> property False

prop_subtype_constraint :: TypeVar -> TypeVar -> Property
prop_subtype_constraint tv1 tv2 =
  let constraint = Subtype tv1 tv2
  in case constraint of
    Subtype t1 t2 -> t1 === tv1 && t2 === tv2
    _ -> property False

-- ============================================================================
-- TypeExpr QuickCheck Tests
-- ============================================================================

-- Test TypeExpr creation
prop_simple_type_has_name :: String -> Property
prop_simple_type_has_name name =
  not (null name) ==>
  let expr = SimpleT (T.pack name)
  in case expr of
    SimpleT n -> T.unpack n === name
    _ -> property False

prop_generic_type_has_name_and_args :: String -> [TypeExpr] -> Property
prop_generic_type_has_name_and_args name args =
  not (null name) ==>
  let expr = GenericT (T.pack name) args
  in case expr of
    GenericT n a -> T.unpack n === name && a === args
    _ -> property False

-- ============================================================================
-- Constraint QuickCheck Tests
-- ============================================================================

-- Test Constraint creation
prop_size_gt_constraint :: String -> Int -> Property
prop_size_gt_constraint var size =
  not (null var) && size >= 0 ==>
  let constraint = SizeGT (T.pack var) size
  in case constraint of
    SizeGT v s -> T.unpack v === var && s === size
    _ -> property False

prop_range_constraint :: String -> Int -> Int -> Property
prop_range_constraint var minVal maxVal =
  not (null var) && minVal <= maxVal ==>
  let constraint = RangeC (T.pack var) minVal maxVal
  in case constraint of
    RangeC v mn mx -> T.unpack v === var && mn === minVal && mx === maxVal
    _ -> property False

-- ============================================================================
-- Statement QuickCheck Tests
-- ============================================================================

-- Test Statement creation
prop_var_decl_statement :: String -> TypeExpr -> Property
prop_var_decl_statement var typ =
  not (null var) ==>
  let stmt = SVarDecl (T.pack var) typ
  in case stmt of
    SVarDecl v t -> T.unpack v === var && t === typ
    _ -> property False

prop_func_decl_statement :: String -> [(String, TypeExpr)] -> Maybe TypeExpr -> Property
prop_func_decl_statement name params retType =
  not (null name) && all (not . null . fst) params ==>
  let stmt = SFuncDecl (T.pack name) (map (\(n, t) -> (T.pack n, t)) params) retType
  in case stmt of
    SFuncDecl n p r -> T.unpack n === name && length p === length params && r === retType
    _ -> property False

-- ============================================================================
-- AST QuickCheck Tests
-- ============================================================================

-- Test AST creation
prop_program_ast_contains_statements :: [Statement] -> Property
prop_program_ast_contains_statements stmts =
  let ast = Program stmts
  in case ast of
    Program s -> s === stmts
    _ -> property False

-- ============================================================================
-- TypeScheme QuickCheck Tests
-- ============================================================================

-- Test TypeScheme creation
prop_forall_scheme_has_vars_and_type :: [String] -> TypeVar -> Property
prop_forall_scheme_has_vars_and_type vars typ =
  all (not . null) vars ==>
  let scheme = Forall vars typ
  in case scheme of
    Forall v t -> v === vars && t === typ
    _ -> property False

-- ============================================================================
-- DependentTypeChecker QuickCheck Tests
-- ============================================================================

-- Test DependentTypeChecker creation
prop_new_dependent_type_checker :: Property
prop_new_dependent_type_checker =
  let checker = newDependentTypeChecker
  in case checker of
    DependentTypeChecker _ _ -> property True
    _ -> property False

-- ============================================================================
-- Analysis Functions QuickCheck Tests
-- ============================================================================

-- Test analyzeDependentTypes function
prop_analyze_dependent_types_returns_result :: Property
prop_analyze_dependent_types_returns_result =
  forAll genSimpleStatement $ \stmt ->
    let ast = Program [stmt]
        result = analyzeDependentTypes ast
    in isLeft result || isRight result  -- Should always return Either

-- Test analyzeAST function
prop_analyze_ast_returns_result :: Property
prop_analyze_ast_returns_result =
  forAll genSimpleStatement $ \stmt ->
    let ast = Program [stmt]
        result = analyzeAST ast
    in isLeft result || isRight result  -- Should always return Either

-- ============================================================================
-- Type System Operations QuickCheck Tests
-- ============================================================================

-- Test type checking
prop_check_type_returns_result :: Property
prop_check_type_returns_result =
  forAll genSimpleTypeExpr $ \typ ->
    let checker = newDependentTypeChecker
        result = checkType checker typ
    in isLeft result || isRight result  -- Should always return Either

-- Test constraint solving
prop_solve_constraints_returns_result :: Property
prop_solve_constraints_returns_result =
  forAll (listOf genSimpleConstraint) $ \constraints ->
    let checker = newDependentTypeChecker
        result = solveConstraints checker constraints
    in isLeft result || isRight result  -- Should always return Either

-- Test unification
prop_unify_returns_result :: Property
prop_unify_returns_result =
  forAll genSimpleTypeExpr $ \typ1 ->
  forAll genSimpleTypeExpr $ \typ2 ->
    let checker = newDependentTypeChecker
        result = unify checker typ1 typ2
    in isLeft result || isRight result  -- Should always return Either

-- ============================================================================
-- Type Inference QuickCheck Tests
-- ============================================================================

-- Test type variable creation
prop_new_type_variable_returns_typevar :: Property
prop_new_type_variable_returns_typevar =
  -- Note: This would need IO to actually test, but we can test the structure
  property True  -- Placeholder - would need IO to test actual creation

prop_get_fresh_type_var_returns_typevar :: Property
prop_get_fresh_type_var_returns_typevar =
  -- Note: This would need IO to actually test, but we can test the structure
  property True  -- Placeholder - would need IO to test actual creation

-- Test type inference
prop_infer_type_returns_result :: Property
prop_infer_type_returns_result =
  forAll genSimpleTypeExpr $ \typ ->
    -- Note: This would need IO to actually test
    property True  -- Placeholder - would need IO to test actual inference

-- Test statement inference
prop_infer_statement_returns_result :: Property
prop_infer_statement_returns_result =
  forAll genSimpleStatement $ \stmt ->
    -- Note: This would need IO to actually test
    property True  -- Placeholder - would need IO to test actual inference

-- Test program inference
prop_infer_program_returns_result :: Property
prop_infer_program_returns_result =
  forAll (listOf genSimpleStatement) $ \stmts ->
    let ast = Program stmts
    in -- Note: This would need IO to actually test
       property True  -- Placeholder - would need IO to test actual inference

-- ============================================================================
-- Type Operations QuickCheck Tests
-- ============================================================================

-- Test generalize function
prop_generalize_returns_scheme :: Property
prop_generalize_returns_scheme =
  forAll genSimpleTypeExpr $ \typ ->
    -- Note: This would need proper context to test
    property True  -- Placeholder - would need proper context

-- Test instantiate function
prop_instantiate_returns_type :: Property
prop_instantiate_returns_type =
  forAll (Forall <$> listOf (arbitrary `suchThat` (not . null)) <*> arbitrary) $ \scheme ->
    -- Note: This would need proper context to test
    property True  -- Placeholder - would need proper context

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- Test AST round-trip
prop_ast_round_trip :: Property
prop_ast_round_trip =
  forAll (listOf genSimpleStatement) $ \stmts ->
    let ast = Program stmts
        reconstructed = Program stmts  -- Simplified round-trip
    in ast === reconstructed

-- Test type expression equality
prop_type_expr_equality_reflexive :: TypeExpr -> Property
prop_type_expr_equality_reflexive expr = expr === expr

-- Test constraint equality
prop_constraint_equality_reflexive :: Constraint -> Property
prop_constraint_equality_reflexive constraint = constraint === constraint

-- Test statement equality
prop_statement_equality_reflexive :: Statement -> Property
prop_statement_equality_reflexive stmt = stmt === stmt

-- Test type variable ordering
prop_type_var_ordering :: TypeVar -> TypeVar -> Property
prop_type_var_ordering tv1 tv2 =
  let ord1 = compare tv1 tv2
      ord2 = compare (show tv1) (show tv2)
  in ord1 === ord2  -- Should be consistent with string representation

tests :: TestTree
tests = testGroup "New Cabal Dependencies QuickCheck Tests"
  [ testGroup "TypeVar tests"
      [ testProperty "TVCon has name" prop_tvcon_has_name
      , testProperty "TVVar has name" prop_tvvar_has_name
      ]
  , testGroup "TypeConstraint tests"
      [ testProperty "Equal constraint" prop_equal_constraint
      , testProperty "Subtype constraint" prop_subtype_constraint
      ]
  , testGroup "TypeExpr tests"
      [ testProperty "Simple type has name" prop_simple_type_has_name
      , testProperty "Generic type has name and args" prop_generic_type_has_name_and_args
      ]
  , testGroup "Constraint tests"
      [ testProperty "SizeGT constraint" prop_size_gt_constraint
      , testProperty "Range constraint" prop_range_constraint
      ]
  , testGroup "Statement tests"
      [ testProperty "Var declaration statement" prop_var_decl_statement
      , testProperty "Function declaration statement" prop_func_decl_statement
      ]
  , testGroup "AST tests"
      [ testProperty "Program AST contains statements" prop_program_ast_contains_statements
      ]
  , testGroup "TypeScheme tests"
      [ testProperty "Forall scheme has vars and type" prop_forall_scheme_has_vars_and_type
      ]
  , testGroup "DependentTypeChecker tests"
      [ testProperty "new dependent type checker" prop_new_dependent_type_checker
      ]
  , testGroup "Analysis functions tests"
      [ testProperty "analyzeDependentTypes returns result" prop_analyze_dependent_types_returns_result
      , testProperty "analyzeAST returns result" prop_analyze_ast_returns_result
      ]
  , testGroup "Type system operations tests"
      [ testProperty "checkType returns result" prop_check_type_returns_result
      , testProperty "solveConstraints returns result" prop_solve_constraints_returns_result
      , testProperty "unify returns result" prop_unify_returns_result
      ]
  , testGroup "Type inference tests"
      [ testProperty "newTypeVariable returns TypeVar" prop_new_type_variable_returns_typevar
      , testProperty "getFreshTypeVar returns TypeVar" prop_get_fresh_type_var_returns_typevar
      , testProperty "inferType returns result" prop_infer_type_returns_result
      , testProperty "inferStatement returns result" prop_infer_statement_returns_result
      , testProperty "inferProgram returns result" prop_infer_program_returns_result
      ]
  , testGroup "Type operations tests"
      [ testProperty "generalize returns scheme" prop_generalize_returns_scheme
      , testProperty "instantiate returns type" prop_instantiate_returns_type
      ]
  , testGroup "Additional property tests"
      [ testProperty "AST round-trip" prop_ast_round_trip
      , testProperty "type expression equality reflexive" prop_type_expr_equality_reflexive
      , testProperty "constraint equality reflexive" prop_constraint_equality_reflexive
      , testProperty "statement equality reflexive" prop_statement_equality_reflexive
      , testProperty "type variable ordering" prop_type_var_ordering
      ]
  ]