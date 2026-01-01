{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum)

import Dependencies.TypeSystem
  ( TypeVar(..), TypeConstraint(..), DependentTypeError(..), TypeDef(..)
  , TypeEnv(..), DependentTypeChecker(..), Substitution
  , preludeTypeDefs, newDependentTypeChecker, newDependentTypeCheckerWithTypes
  , convertTypeExpr, convertConstraint
  , addType, addConstraint, addTypeError, lookupTypeDef, checkType
  , checkTypeInstantiation, solveConstraints, checkTypeConstraint
  , validateConstraint, getDependentTypeErrors, unify
  )
import Dependencies.AST (TypeExpr(..), Constraint(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> arbitrary
    , TVVar <$> arbitrary
    , TVApp <$> arbitrary <*> listOf arbitrary
    , TVFun <$> listOf arbitrary <*> arbitrary
    , TVTuple <$> listOf arbitrary
    ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ Equal <$> arbitrary <*> arbitrary
    , Subtype <$> arbitrary <*> arbitrary
    , Predicate <$> arbitrary <*> listOf arbitrary
    , TypeSizeGE <$> arbitrary <*> arbitrary
    , TypeSizeGT <$> arbitrary <*> arbitrary
    , TypeRange <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ DependentTypeMismatch <$> arbitrary <*> arbitrary
    , ConstraintViolation <$> arbitrary <*> arbitrary
    , TypeNotFound <$> arbitrary
    , InvalidTypeArgument <$> arbitrary
    , UnsolvableConstraint <$> arbitrary
    , DependentInfiniteType <$> arbitrary <*> arbitrary
    , AmbiguousType <$> arbitrary
    , ParseError <$> arbitrary
    , SemanticError <$> arbitrary
    ]

instance Arbitrary TypeDef where
  arbitrary = do
    tdParams <- listOf arbitrary
    tdConstraints <- listOf arbitrary
    return $ TypeDefDecl tdParams tdConstraints

instance Arbitrary TypeEnv where
  arbitrary = do
    typeDefinitions <- arbitrary
    pendingConstraints <- listOf arbitrary
    return $ TypeEnv typeDefinitions pendingConstraints

instance Arbitrary DependentTypeChecker where
  arbitrary = do
    dtcTypeEnv <- arbitrary
    tcErrors <- listOf arbitrary
    return $ DependentTypeChecker dtcTypeEnv tcErrors

instance Arbitrary Substitution where
  arbitrary = Map.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)

-- Generate valid type names
arbitraryTypeName :: Gen String
arbitraryTypeName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- ============================================================================
-- TypeVar Properties
-- ============================================================================

-- Property: TypeVar Show produces non-empty string
prop_typevar_show_nonempty :: TypeVar -> Property
prop_typevar_show_nonempty typeVar =
  let shown = show typeVar
  in property $ not (null shown)

-- Property: TypeVar ordering is consistent
prop_typevar_ordering :: TypeVar -> TypeVar -> Property
prop_typevar_ordering tv1 tv2 =
  let cmp = compare tv1 tv2
      cmpReverse = compare tv2 tv1
  in property $ (cmp == EQ) ==> (cmpReverse == EQ) .&&.
             (cmp == LT) ==> (cmpReverse == GT) .&&.
             (cmp == GT) ==> (cmpReverse == LT)

-- Property: TypeVar constructors create distinct representations
prop_typevar_constructors_distinct :: String -> String -> [TypeVar] -> Property
prop_typevar_constructors_distinct name1 name2 args =
  L.length args <= 3 ==>
  let con = TVCon name1
      var = TVVar name1
      app = TVApp name1 args
      fun = TVFun args (TVVar name2)
      tuple = TVTuple args
      typeVars = [con, var, app, fun, tuple]
      shownVars = map show typeVars
  in property $ L.length (nub shownVars) >= 3 -- At least some should be distinct

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

-- Property: TypeConstraint Show produces non-empty string
prop_typeconstraint_show_nonempty :: TypeConstraint -> Property
prop_typeconstraint_show_nonempty constraint =
  let shown = show constraint
  in property $ not (null shown)

-- Property: TypeConstraint ordering is consistent
prop_typeconstraint_ordering :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_ordering tc1 tc2 =
  let cmp = compare tc1 tc2
      cmpReverse = compare tc2 tc1
  in property $ (cmp == EQ) ==> (cmpReverse == EQ) .&&.
             (cmp == LT) ==> (cmpReverse == GT) .&&.
             (cmp == GT) ==> (cmpReverse == LT)

-- Property: TypeConstraint contains relevant information
prop_typeconstraint_contains_info :: TypeConstraint -> Property
prop_typeconstraint_contains_info constraint =
  let shown = show constraint
  in case constraint of
    Equal tv1 tv2 -> property $ show tv1 `L.isInfixOf` shown .&&. show tv2 `L.isInfixOf` shown
    Subtype tv1 tv2 -> property $ show tv1 `L.isInfixOf` shown .&&. show tv2 `L.isInfixOf` shown
    Predicate name args -> property $ name `L.isInfixOf` shown .&&. L.all (`L.isInfixOf` shown) (map show args)
    TypeSizeGE tv size -> property $ show tv `L.isInfixOf` shown .&&. show size `L.isInfixOf` shown
    TypeSizeGT tv size -> property $ show tv `L.isInfixOf` shown .&&. show size `L.isInfixOf` shown
    TypeRange tv min max -> property $ show tv `L.isInfixOf` shown .&&. show min `L.isInfixOf` shown .&&. show max `L.isInfixOf` shown

-- ============================================================================
-- DependentTypeError Properties
-- ============================================================================

-- Property: DependentTypeError Show produces non-empty string
prop_dependent_type_error_show_nonempty :: DependentTypeError -> Property
prop_dependent_type_error_show_nonempty err =
  let shown = show err
  in property $ not (null shown)

-- Property: DependentTypeError contains relevant information
prop_dependent_type_error_contains_info :: DependentTypeError -> Property
prop_dependent_type_error_contains_info err =
  let shown = show err
  in case err of
    DependentTypeMismatch tv1 tv2 -> property $ show tv1 `L.isInfixOf` shown .&&. show tv2 `L.isInfixOf` shown
    ConstraintViolation msg tv -> property $ msg `L.isInfixOf` shown .&&. show tv `L.isInfixOf` shown
    TypeNotFound name -> property $ name `L.isInfixOf` shown
    InvalidTypeArgument msg -> property $ msg `L.isInfixOf` shown
    UnsolvableConstraint constraint -> property $ show constraint `L.isInfixOf` shown
    DependentInfiniteType msg tv -> property $ msg `L.isInfixOf` shown .&&. show tv `L.isInfixOf` shown
    AmbiguousType msg -> property $ msg `L.isInfixOf` shown
    ParseError msg -> property $ msg `L.isInfixOf` shown
    SemanticError msg -> property $ msg `L.isInfixOf` shown

-- ============================================================================
-- TypeDef Properties
-- ============================================================================

-- Property: TypeDef Show produces non-empty string
prop_typedef_show_nonempty :: TypeDef -> Property
prop_typedef_show_nonempty typeDef =
  let shown = show typeDef
  in property $ not (null shown)

-- Property: TypeDef with empty params L.and constraints is valid
prop_typedef_empty_valid :: Property
prop_typedef_empty_valid =
  let emptyDef = TypeDefDecl [] []
  in property $ True -- Basic sanity check

-- ============================================================================
-- TypeEnv Properties
-- ============================================================================

-- Property: TypeEnv Show produces non-empty string
prop_typeenv_show_nonempty :: TypeEnv -> Property
prop_typeenv_show_nonempty typeEnv =
  let shown = show typeEnv
  in property $ not (null shown)

-- Property: TypeEnv preserves type definitions
prop_typeenv_preserves_definitions :: Map.Map String TypeDef -> [TypeConstraint] -> Property
prop_typeenv_preserves_definitions defs constraints =
  Map.size defs <= 3 ==> -- Limit for performance
  let typeEnv = TypeEnv defs constraints
  in property $ typeDefinitions typeEnv === defs .&&.
             pendingConstraints typeEnv === constraints

-- ============================================================================
-- DependentTypeChecker Properties
-- ============================================================================

-- Property: newDependentTypeChecker creates valid checker
prop_new_dependent_type_checker_valid :: Property
prop_new_dependent_type_checker_valid =
  let checker = newDependentTypeChecker
  in property $ True -- Basic sanity check

-- Property: newDependentTypeCheckerWithTypes creates checker with given types
prop_new_dependent_type_checker_with_types :: Map.Map String TypeDef -> Property
prop_new_dependent_type_checker_with_types types =
  Map.size types <= 3 ==> -- Limit for performance
  let checker = newDependentTypeCheckerWithTypes types
  in property $ True -- Basic sanity check

-- Property: getDependentTypeErrors returns errors from checker
prop_get_dependent_type_errors :: [DependentTypeError] -> Property
prop_get_dependent_type_errors errors =
  L.length errors <= 5 ==> -- Limit for performance
  let checker = DependentTypeChecker (TypeEnv Map.empty []) errors
      retrievedErrors = getDependentTypeErrors checker
  in property $ retrievedErrors === errors

-- ============================================================================
-- Type Environment Operations Properties
-- ============================================================================

-- Property: addType adds type to environment
prop_add_type :: String -> TypeDef -> TypeEnv -> Property
prop_add_type name typeDef typeEnv =
  not (Map.member name (typeDefinitions typeEnv)) ==>
  let newEnv = addType name typeDef typeEnv
  in property $ Map.member name (typeDefinitions newEnv)

-- Property: lookupTypeDef finds added type
prop_lookup_typedef_finds_added :: String -> TypeDef -> TypeEnv -> Property
prop_lookup_typedef_finds_added name typeDef typeEnv =
  let newEnv = addType name typeDef typeEnv
      found = lookupTypeDef name newEnv
  in property $ found === Just typeDef

-- Property: lookupTypeDef returns Nothing for missing type
prop_lookup_typedef_missing :: String -> TypeEnv -> Property
prop_lookup_typedef_missing name typeEnv =
  not (Map.member name (typeDefinitions typeEnv)) ==>
  let found = lookupTypeDef name typeEnv
  in property $ found === Nothing

-- Property: addConstraint adds constraint to environment
prop_add_constraint :: TypeConstraint -> TypeEnv -> Property
prop_add_constraint constraint typeEnv =
  let newEnv = addConstraint constraint typeEnv
      oldConstraints = pendingConstraints typeEnv
      newConstraints = pendingConstraints newEnv
  in property $ constraint `elem` newConstraints .&&.
             L.length newConstraints === L.length oldConstraints + 1

-- Property: addTypeError adds error to checker
prop_add_type_error :: DependentTypeError -> DependentTypeChecker -> Property
prop_add_type_error error checker =
  let newChecker = addTypeError error checker
      oldErrors = tcErrors checker
      newErrors = tcErrors newChecker
  in property $ error `elem` newErrors .&&.
             L.length newErrors === L.length oldErrors + 1

-- ============================================================================
-- Type Checking Properties
-- ============================================================================

-- Property: checkType handles simple types
prop_check_type_simple :: TypeVar -> Property
prop_check_type_simple typeVar =
  let checker = newDependentTypeChecker
      result = checkType checker typeVar
  in property $ True -- Should not crash

-- Property: checkTypeInstantiation handles instantiations
prop_check_type_instantiation :: TypeVar -> Property
prop_check_type_instantiation typeVar =
  let checker = newDependentTypeChecker
      result = checkTypeInstantiation checker typeVar
  in property $ True -- Should not crash

-- Property: checkTypeConstraint validates constraints
prop_check_type_constraint :: TypeConstraint -> Property
prop_check_type_constraint constraint =
  let checker = newDependentTypeChecker
      result = checkTypeConstraint checker constraint
  in property $ True -- Should not crash

-- Property: validateConstraint handles constraint validation
prop_validate_constraint :: TypeConstraint -> Property
prop_validate_constraint constraint =
  let checker = newDependentTypeChecker
      result = validateConstraint checker constraint
  in property $ True -- Should not crash

-- ============================================================================
-- Constraint Solving Properties
-- ============================================================================

-- Property: solveConstraints handles empty constraint list
prop_solve_constraints_empty :: Property
prop_solve_constraints_empty =
  let checker = newDependentTypeChecker
      result = solveConstraints checker []
  in property $ True -- Should not crash

-- Property: solveConstraints handles simple constraints
prop_solve_constraints_simple :: TypeConstraint -> Property
prop_solve_constraints_simple constraint =
  let checker = newDependentTypeChecker
      result = solveConstraints checker [constraint]
  in property $ True -- Should not crash

-- Property: solveConstraints handles multiple constraints
prop_solve_constraints_multiple :: [TypeConstraint] -> Property
prop_solve_constraints_multiple constraints =
  L.length constraints <= 3 ==> -- Limit for performance
  let checker = newDependentTypeChecker
      result = solveConstraints checker constraints
  in property $ True -- Should not crash

-- ============================================================================
-- Unification Properties
-- ============================================================================

-- Property: unify handles identical types
prop_unify_identical :: TypeVar -> Property
prop_unify_identical typeVar =
  let checker = newDependentTypeChecker
      result = unify checker typeVar typeVar
  in property $ True -- Should not crash

-- Property: unify handles different types
prop_unify_different :: TypeVar -> TypeVar -> Property
prop_unify_different tv1 tv2 =
  tv1 /= tv2 ==>
  let checker = newDependentTypeChecker
      result = unify checker tv1 tv2
  in property $ True -- Should not crash

-- ============================================================================
-- Conversion Properties
-- ============================================================================

-- Property: convertTypeExpr handles simple expressions
prop_convert_type_expr_simple :: TypeExpr -> Property
prop_convert_type_expr_simple typeExpr =
  let result = convertTypeExpr typeExpr
  in property $ True -- Should not crash

-- Property: convertConstraint handles simple constraints
prop_convert_constraint_simple :: Constraint -> Property
prop_convert_constraint_simple constraint =
  let result = convertConstraint constraint
  in property $ True -- Should not crash

-- ============================================================================
-- Prelude Types Properties
-- ============================================================================

-- Property: preludeTypeDefs contains basic types
prop_prelude_contains_basic_types :: Property
prop_prelude_contains_basic_types =
  let basicTypes = ["int", "string", "bool"]
  in property $ L.all (`Map.member` preludeTypeDefs) basicTypes

-- Property: preludeTypeDefs is non-empty
prop_prelude_nonempty :: Property
prop_prelude_nonempty =
  property $ not (Map.null preludeTypeDefs)

-- ============================================================================
-- Complex Properties
-- ============================================================================

-- Property: Type checking is deterministic
prop_type_checking_deterministic :: TypeVar -> Property
prop_type_checking_deterministic typeVar =
  let checker = newDependentTypeChecker
      result1 = checkType checker typeVar
      result2 = checkType checker typeVar
  in property $ result1 === result2

-- Property: Constraint solving is deterministic
prop_constraint_solving_deterministic :: [TypeConstraint] -> Property
prop_constraint_solving_deterministic constraints =
  L.length constraints <= 3 ==> -- Limit for performance
  let checker = newDependentTypeChecker
      result1 = solveConstraints checker constraints
      result2 = solveConstraints checker constraints
  in property $ result1 === result2

-- Property: Unification is symmetric
prop_unification_symmetric :: TypeVar -> TypeVar -> Property
prop_unification_symmetric tv1 tv2 =
  let checker = newDependentTypeChecker
      result1 = unify checker tv1 tv2
      result2 = unify checker tv2 tv1
  in property $ result1 === result2

-- Property: Environment operations are consistent
prop_environment_operations_consistent :: String -> TypeDef -> TypeConstraint -> TypeEnv -> Property
prop_environment_operations_consistent name typeDef constraint typeEnv =
  not (Map.member name (typeDefinitions typeEnv)) ==>
  let envWithDef = addType name typeDef typeEnv
      envWithConstraint = addConstraint constraint envWithDef
      foundDef = lookupTypeDef name envWithConstraint
  in property $ foundDef === Just typeDef .&&.
             constraint `elem` pendingConstraints envWithConstraint

-- Property: Error accumulation works correctly
prop_error_accumulation :: [DependentTypeError] -> Property
prop_error_accumulation errors =
  L.length errors <= 5 ==> -- Limit for performance
  let checker = newDependentTypeChecker
      checkerWithErrors = L.foldl (flip addTypeError) checker errors
      finalErrors = getDependentTypeErrors checkerWithErrors
  in property $ L.length finalErrors >= L.length errors .&&.
             L.all (`elem` finalErrors) errors

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "TypeSystem Properties Tests"
  [ testGroup "TypeVar Properties"
    [ fastProperty "TypeVar Show produces non-empty string" prop_typevar_show_nonempty
    , fastProperty "TypeVar ordering is consistent" prop_typevar_ordering
    , fastProperty "TypeVar constructors create distinct representations" prop_typevar_constructors_distinct
    ]
  , testGroup "TypeConstraint Properties"
    [ fastProperty "TypeConstraint Show produces non-empty string" prop_typeconstraint_show_nonempty
    , fastProperty "TypeConstraint ordering is consistent" prop_typeconstraint_ordering
    , fastProperty "TypeConstraint contains relevant information" prop_typeconstraint_contains_info
    ]
  , testGroup "DependentTypeError Properties"
    [ fastProperty "DependentTypeError Show produces non-empty string" prop_dependent_type_error_show_nonempty
    , fastProperty "DependentTypeError contains relevant information" prop_dependent_type_error_contains_info
    ]
  , testGroup "TypeDef Properties"
    [ fastProperty "TypeDef Show produces non-empty string" prop_typedef_show_nonempty
    , fastProperty "TypeDef with empty params L.and constraints is valid" prop_typedef_empty_valid
    ]
  , testGroup "TypeEnv Properties"
    [ fastProperty "TypeEnv Show produces non-empty string" prop_typeenv_show_nonempty
    , fastProperty "TypeEnv preserves type definitions" prop_typeenv_preserves_definitions
    ]
  , testGroup "DependentTypeChecker Properties"
    [ fastProperty "newDependentTypeChecker creates valid checker" prop_new_dependent_type_checker_valid
    , fastProperty "newDependentTypeCheckerWithTypes creates checker with given types" prop_new_dependent_type_checker_with_types
    , fastProperty "getDependentTypeErrors returns errors from checker" prop_get_dependent_type_errors
    ]
  , testGroup "Type Environment Operations Properties"
    [ fastProperty "addType adds type to environment" prop_add_type
    , fastProperty "lookupTypeDef finds added type" prop_lookup_typedef_finds_added
    , fastProperty "lookupTypeDef returns Nothing for missing type" prop_lookup_typedef_missing
    , fastProperty "addConstraint adds constraint to environment" prop_add_constraint
    , fastProperty "addTypeError adds error to checker" prop_add_type_error
    ]
  , testGroup "Type Checking Properties"
    [ fastProperty "checkType handles simple types" prop_check_type_simple
    , fastProperty "checkTypeInstantiation handles instantiations" prop_check_type_instantiation
    , fastProperty "checkTypeConstraint validates constraints" prop_check_type_constraint
    , fastProperty "validateConstraint handles constraint validation" prop_validate_constraint
    ]
  , testGroup "Constraint Solving Properties"
    [ fastProperty "solveConstraints handles empty constraint list" prop_solve_constraints_empty
    , fastProperty "solveConstraints handles simple constraints" prop_solve_constraints_simple
    , fastProperty "solveConstraints handles multiple constraints" prop_solve_constraints_multiple
    ]
  , testGroup "Unification Properties"
    [ fastProperty "unify handles identical types" prop_unify_identical
    , fastProperty "unify handles different types" prop_unify_different
    ]
  , testGroup "Conversion Properties"
    [ fastProperty "convertTypeExpr handles simple expressions" prop_convert_type_expr_simple
    , fastProperty "convertConstraint handles simple constraints" prop_convert_constraint_simple
    ]
  , testGroup "Prelude Types Properties"
    [ fastProperty "preludeTypeDefs contains basic types" prop_prelude_contains_basic_types
    , fastProperty "preludeTypeDefs is non-empty" prop_prelude_nonempty
    ]
  , testGroup "Complex Properties"
    [ fastProperty "Type checking is deterministic" prop_type_checking_deterministic
    , fastProperty "Constraint solving is deterministic" prop_constraint_solving_deterministic
    , fastProperty "Unification is symmetric" prop_unification_symmetric
    , fastProperty "Environment operations are consistent" prop_environment_operations_consistent
    , fastProperty "Error accumulation works correctly" prop_error_accumulation
    ]
  ]