{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependenciesQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen(..), vectorOf)

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , preludeTypeDefs
  , addType
  , addConstraint
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )

import Dependencies.AST (TypeExpr(..), Constraint(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isLeft, isRight)
import Data.List (sort, nub)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = do
    oneof
      [ TVCon <$> listOf (elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
      , TVVar <$> listOf (elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
      , TVApp <$> listOf (elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']) <*> listOf arbitrary
      , TVFun <$> listOf arbitrary <*> arbitrary
      , TVTuple <$> listOf arbitrary
      ]

instance Arbitrary TypeConstraint where
  arbitrary = do
    oneof
      [ Equal <$> arbitrary <*> arbitrary
      , Subtype <$> arbitrary <*> arbitrary
      , Predicate <$> listOf (elements ['a'..'z'] ++ ['A'..'Z']) <*> listOf arbitrary
      , TypeSizeGE <$> arbitrary <*> choose (0, 100)
      , TypeSizeGT <$> arbitrary <*> choose (0, 100)
      , TypeRange <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
      ]

instance Arbitrary DependentTypeError where
  arbitrary = do
    oneof
      [ DependentTypeMismatch <$> arbitrary <*> arbitrary
      , ConstraintViolation <$> listOf (elements ['a'..'z']) <*> arbitrary
      , TypeNotFound <$> listOf (elements ['a'..'z'])
      , InvalidTypeArgument <$> listOf (elements ['a'..'z'])
      , UnsolvableConstraint <$> arbitrary
      , DependentInfiniteType <$> listOf (elements ['a'..'z']) <*> arbitrary
      , AmbiguousType <$> listOf (elements ['a'..'z'])
      , ParseError <$> listOf (elements ['a'..'z'] ++ [' '])
      , SemanticError <$> listOf (elements ['a'..'z'] ++ [' '])
      ]

instance Arbitrary TypeDef where
  arbitrary = do
    params <- listOf $ listOf (elements ['a'..'z'])
    constraints <- listOf arbitrary
    return $ TypeDefDecl params constraints

instance Arbitrary TypeEnv where
  arbitrary = do
    typeDefs <- arbitrary
    pendingConstraints <- listOf arbitrary
    return $ TypeEnv typeDefs pendingConstraints

instance Arbitrary DependentTypeChecker where
  arbitrary = do
    typeEnv <- arbitrary
    errors <- listOf arbitrary
    return $ DependentTypeChecker typeEnv errors

instance Arbitrary Substitution where
  arbitrary = Map.fromList <$> listOf ((,) <$> listOf (elements ['a'..'z']) <*> arbitrary)

-- Generate valid type name
validTypeName :: Gen String
validTypeName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate valid constraint name
validConstraintName :: Gen String
validConstraintName = listOf $ elements ['a'..'z'] ++ ['A'..'Z']

-- Generate valid error message
validErrorMessage :: Gen String
validErrorMessage = listOf $ elements ['a'..'z'] ++ [' ']

-- ============================================================================
-- TypeVar Property Tests
-- ============================================================================

-- Property: TypeVar equality is reflexive
prop_typevar_equality_reflexive :: TypeVar -> Property
prop_typevar_equality_reflexive tv =
  tv === tv

-- Property: TypeVar equality is symmetric
prop_typevar_equality_symmetric :: TypeVar -> TypeVar -> Property
prop_typevar_equality_symmetric tv1 tv2 =
  (tv1 == tv2) === (tv2 == tv1)

-- Property: TypeVar equality is transitive
prop_typevar_equality_transitive :: TypeVar -> TypeVar -> TypeVar -> Property
prop_typevar_equality_transitive tv1 tv2 tv3 =
  (tv1 == tv2 && tv2 == tv3) ==> (tv1 == tv3)

-- Property: TypeVar ordering is consistent
prop_typevar_ordering_consistent :: TypeVar -> TypeVar -> Property
prop_typevar_ordering_consistent tv1 tv2 =
  let ord1 = compare tv1 tv2
      ord2 = compare (show tv1) (show tv2)
  in property $ (ord1 == EQ) === (ord2 == EQ) .&&.
               (ord1 == LT) === (ord2 == LT) .&&.
               (ord1 == GT) === (ord2 == GT)

-- Property: TVCon preserves constructor name
prop_tvcon_preserves_name :: String -> Property
prop_tvcon_preserves_name name =
  let tv = TVCon name
  in case tv of
    TVCon n -> n === name
    _ -> property False

-- Property: TVVar preserves variable name
prop_tvvar_preserves_name :: String -> Property
prop_tvvar_preserves_name name =
  let tv = TVVar name
  in case tv of
    TVVar n -> n === name
    _ -> property False

-- Property: TVApp preserves constructor name and arguments
prop_tvapp_preserves_name_args :: String -> [TypeVar] -> Property
prop_tvapp_preserves_name_args name args =
  let tv = TVApp name args
  in case tv of
    TVApp n a -> n === name .&&. a === args
    _ -> property False

-- Property: TVFun preserves parameters and return type
prop_tvfun_preserves_params_return :: [TypeVar] -> TypeVar -> Property
prop_tvfun_preserves_params_return params ret =
  let tv = TVFun params ret
  in case tv of
    TVFun p r -> p === params .&&. r === ret
    _ -> property False

-- Property: TVTuple preserves elements
prop_tvtuple_preserves_elements :: [TypeVar] -> Property
prop_tvtuple_preserves_elements elems =
  let tv = TVTuple elems
  in case tv of
    TVTuple e -> e === elems
    _ -> property False

-- ============================================================================
-- TypeConstraint Property Tests
-- ============================================================================

-- Property: TypeConstraint equality is reflexive
prop_typeconstraint_equality_reflexive :: TypeConstraint -> Property
prop_typeconstraint_equality_reflexive tc =
  tc === tc

-- Property: TypeConstraint equality is symmetric
prop_typeconstraint_equality_symmetric :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_equality_symmetric tc1 tc2 =
  (tc1 == tc2) === (tc2 == tc1)

-- Property: TypeConstraint equality is transitive
prop_typeconstraint_equality_transitive :: TypeConstraint -> TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_equality_transitive tc1 tc2 tc3 =
  (tc1 == tc2 && tc2 == tc3) ==> (tc1 == tc3)

-- Property: Equal constraint preserves both types
prop_equal_preserves_types :: TypeVar -> TypeVar -> Property
prop_equal_preserves_types tv1 tv2 =
  let tc = Equal tv1 tv2
  in case tc of
    Equal t1 t2 -> t1 === tv1 .&&. t2 === tv2
    _ -> property False

-- Property: Subtype constraint preserves both types
prop_subtype_preserves_types :: TypeVar -> TypeVar -> Property
prop_subtype_preserves_types tv1 tv2 =
  let tc = Subtype tv1 tv2
  in case tc of
    Subtype t1 t2 -> t1 === tv1 .&&. t2 === tv2
    _ -> property False

-- Property: Predicate constraint preserves name and types
prop_predicate_preserves_name_types :: String -> [TypeVar] -> Property
prop_predicate_preserves_name_types name tvs =
  let tc = Predicate name tvs
  in case tc of
    Predicate n t -> n === name .&&. t === tvs
    _ -> property False

-- Property: TypeSizeGE constraint preserves type and size
prop_typesizege_preserves_type_size :: TypeVar -> Int -> Property
prop_typesizege_preserves_type_size tv size =
  let tc = TypeSizeGE tv size
  in case tc of
    TypeSizeGE t s -> t === tv .&&. s === size
    _ -> property False

-- Property: TypeSizeGT constraint preserves type and size
prop_typesizegt_preserves_type_size :: TypeVar -> Int -> Property
prop_typesizegt_preserves_type_size tv size =
  let tc = TypeSizeGT tv size
  in case tc of
    TypeSizeGT t s -> t === tv .&&. s === size
    _ -> property False

-- Property: TypeRange constraint preserves type and range
prop_typerange_preserves_type_range :: TypeVar -> Int -> Int -> Property
prop_typerange_preserves_type_range tv min max ->
  let tc = TypeRange tv min max
  in case tc of
    TypeRange t mn mx -> t === tv .&&. mn === min .&&. mx === max
    _ -> property False

-- ============================================================================
-- DependentTypeError Property Tests
-- ============================================================================

-- Property: DependentTypeError equality is reflexive
prop_dependenttypeerror_equality_reflexive :: DependentTypeError -> Property
prop_dependenttypeerror_equality_reflexive err =
  err === err

-- Property: DependentTypeError equality is symmetric
prop_dependenttypeerror_equality_symmetric :: DependentTypeError -> DependentTypeError -> Property
prop_dependenttypeerror_equality_symmetric err1 err2 =
  (err1 == err2) === (err2 == err1)

-- Property: DependentTypeError equality is transitive
prop_dependenttypeerror_equality_transitive :: DependentTypeError -> DependentTypeError -> DependentTypeError -> Property
prop_dependenttypeerror_equality_transitive err1 err2 err3 =
  (err1 == err2 && err2 == err3) ==> (err1 == err3)

-- Property: DependentTypeMismatch preserves both types
prop_dependenttypemismatch_preserves_types :: TypeVar -> TypeVar -> Property
prop_dependenttypemismatch_preserves_types tv1 tv2 =
  let err = DependentTypeMismatch tv1 tv2
  in case err of
    DependentTypeMismatch t1 t2 -> t1 === tv1 .&&. t2 === tv2
    _ -> property False

-- Property: ConstraintViolation preserves constraint and type
prop_constraintviolation_preserves_constraint_type :: String -> TypeVar -> Property
prop_constraintviolation_preserves_constraint_type constraint tv =
  let err = ConstraintViolation constraint tv
  in case err of
    ConstraintViolation c t -> c === constraint .&&. t === tv
    _ -> property False

-- Property: TypeNotFound preserves type name
prop_typenotfound_preserves_name :: String -> Property
prop_typenotfound_preserves_name name =
  let err = TypeNotFound name
  in case err of
    TypeNotFound n -> n === name
    _ -> property False

-- Property: InvalidTypeArgument preserves argument
prop_invalidtypeargument_preserves_argument :: String -> Property
prop_invalidtypeargument_preserves_argument arg =
  let err = InvalidTypeArgument arg
  in case err of
    InvalidTypeArgument a -> a === arg
    _ -> property False

-- Property: UnsolvableConstraint preserves constraint
prop_unsolvableconstraint_preserves_constraint :: TypeConstraint -> Property
prop_unsolvableconstraint_preserves_constraint tc =
  let err = UnsolvableConstraint tc
  in case err of
    UnsolvableConstraint c -> c === tc
    _ -> property False

-- ============================================================================
-- TypeDef Property Tests
-- ============================================================================

-- Property: TypeDef equality is reflexive
prop_typedef_equality_reflexive :: TypeDef -> Property
prop_typedef_equality_reflexive td =
  td === td

-- Property: TypeDef equality is symmetric
prop_typedef_equality_symmetric :: TypeDef -> TypeDef -> Property
prop_typedef_equality_symmetric td1 td2 =
  (td1 == td2) === (td2 == td1)

-- Property: TypeDef equality is transitive
prop_typedef_equality_transitive :: TypeDef -> TypeDef -> TypeDef -> Property
prop_typedef_equality_transitive td1 td2 td3 =
  (td1 == td2 && td2 == td3) ==> (td1 == td3)

-- Property: TypeDef preserves parameters and constraints
prop_typedef_preserves_params_constraints :: [String] -> [TypeConstraint] -> Property
prop_typedef_preserves_params_constraints params constraints =
  let td = TypeDefDecl params constraints
  in case td of
    TypeDefDecl p c -> p === params .&&. c === constraints
    _ -> property False

-- ============================================================================
-- TypeEnv Property Tests
-- ============================================================================

-- Property: TypeEnv equality is reflexive
prop_typeenv_equality_reflexive :: TypeEnv -> Property
prop_typeenv_equality_reflexive env =
  env === env

-- Property: TypeEnv equality is symmetric
prop_typeenv_equality_symmetric :: TypeEnv -> TypeEnv -> Property
prop_typeenv_equality_symmetric env1 env2 =
  (env1 == env2) === (env2 == env1)

-- Property: TypeEnv equality is transitive
prop_typeenv_equality_transitive :: TypeEnv -> TypeEnv -> TypeEnv -> Property
prop_typeenv_equality_transitive env1 env2 env3 =
  (env1 == env2 && env2 == env3) ==> (env1 == env3)

-- Property: TypeEnv preserves type definitions and constraints
prop_typeenv_preserves_defs_constraints :: Map.Map String TypeDef -> [TypeConstraint] -> Property
prop_typeenv_preserves_defs_constraints typeDefs constraints =
  let env = TypeEnv typeDefs constraints
  in case env of
    TypeEnv td pc -> td === typeDefs .&&. pc === constraints
    _ -> property False

-- ============================================================================
-- DependentTypeChecker Property Tests
-- ============================================================================

-- Property: DependentTypeChecker equality is reflexive
prop_dependenttypechecker_equality_reflexive :: DependentTypeChecker -> Property
prop_dependenttypechecker_equality_reflexive dtc =
  dtc === dtc

-- Property: DependentTypeChecker equality is symmetric
prop_dependenttypechecker_equality_symmetric :: DependentTypeChecker -> DependentTypeChecker -> Property
prop_dependenttypechecker_equality_symmetric dtc1 dtc2 =
  (dtc1 == dtc2) === (dtc2 == dtc1)

-- Property: DependentTypeChecker equality is transitive
prop_dependenttypechecker_equality_transitive :: DependentTypeChecker -> DependentTypeChecker -> DependentTypeChecker -> Property
prop_dependenttypechecker_equality_transitive dtc1 dtc2 dtc3 =
  (dtc1 == dtc2 && dtc2 == dtc3) ==> (dtc1 == dtc3)

-- Property: DependentTypeChecker preserves type environment and errors
prop_dependenttypechecker_preserves_env_errors :: TypeEnv -> [DependentTypeError] -> Property
prop_dependenttypechecker_preserves_env_errors typeEnv errors =
  let dtc = DependentTypeChecker typeEnv errors
  in case dtc of
    DependentTypeChecker te e -> te === typeEnv .&&. e === errors
    _ -> property False

-- ============================================================================
-- Constructor Property Tests
-- ============================================================================

-- Property: newDependentTypeChecker creates checker with prelude types
prop_new_dependenttypechecker_prelude :: Property
prop_new_dependenttypechecker_prelude =
  let dtc = newDependentTypeChecker
      typeEnv = dtcTypeEnv dtc
      typeDefs = typeDefinitions typeEnv
  in property $ Map.member "int" typeDefs .&&.
               Map.member "string" typeDefs .&&.
               Map.member "bool" typeDefs .&&.
               Map.member "float64" typeDefs .&&.
               null (tcErrors dtc)

-- Property: newDependentTypeCheckerWithTypes creates checker with custom types
prop_new_dependenttypechecker_with_types :: [(String, [String], [TypeConstraint])] -> Property
prop_new_dependenttypechecker_with_types typeDefs =
  let dtc = newDependentTypeCheckerWithTypes typeDefs
      typeEnv = dtcTypeEnv dtc
      allDefs = typeDefinitions typeEnv
  in property $ all (\(name, _, _) -> Map.member name allDefs) typeDefs .&&.
               null (tcErrors dtc)

-- Property: preludeTypeDefs contains expected types
prop_prelude_typedefs_contains_expected :: Property
prop_prelude_typedefs_contains_expected =
  property $ Map.member "int" preludeTypeDefs .&&.
               Map.member "string" preludeTypeDefs .&&.
               Map.member "bool" preludeTypeDefs .&&.
               Map.member "float64" preludeTypeDefs

-- ============================================================================
-- Advanced Property Tests
-- ============================================================================

-- Property: Different TypeVar constructors create unequal values
prop_typevar_different_constructors_unequal :: String -> [TypeVar] -> TypeVar -> Property
prop_typevar_different_constructors_unequal name args ret =
  let con = TVCon name
      app = TVApp name args
      fun = TVFun args ret
  in property $ con /= app .&&. con /= fun .&&. app /= fun

-- Property: Different TypeConstraint constructors create unequal values
prop_typeconstraint_different_constructors_unequal :: TypeVar -> TypeVar -> String -> [TypeVar] -> Int -> Property
prop_typeconstraint_different_constructors_unequal tv1 tv2 name tvs size =
  let equal = Equal tv1 tv2
      subtype = Subtype tv1 tv2
      predicate = Predicate name tvs
      sizeGE = TypeSizeGE tv1 size
  in property $ equal /= subtype .&&. equal /= predicate .&&. equal /= sizeGE .&&.
               subtype /= predicate .&&. subtype /= sizeGE .&&. predicate /= sizeGE

-- Property: Different DependentTypeError constructors create unequal values
prop_dependenttypeerror_different_constructors_unequal :: TypeVar -> TypeVar -> String -> Property
prop_dependenttypeerror_different_constructors_unequal tv1 tv2 name =
  let mismatch = DependentTypeMismatch tv1 tv2
      violation = ConstraintViolation name tv1
      notFound = TypeNotFound name
  in property $ mismatch /= violation .&&. mismatch /= notFound .&&. violation /= notFound

-- Property: TypeVar ordering is total
prop_typevar_ordering_total :: TypeVar -> TypeVar -> Property
prop_typevar_ordering_total tv1 tv2 =
  let ord = compare tv1 tv2
  in property $ ord == EQ || ord == LT || ord == GT

-- Property: TypeConstraint ordering is total
prop_typeconstraint_ordering_total :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_ordering_total tc1 tc2 =
  let ord = compare tc1 tc2
  in property $ ord == EQ || ord == LT || ord == GT

-- Property: DependentTypeError ordering is total
prop_dependenttypeerror_ordering_total :: DependentTypeError -> DependentTypeError -> Property
prop_dependenttypeerror_ordering_total err1 err2 =
  let ord = compare err1 err2
  in property $ ord == EQ || ord == LT || ord == GT

-- Property: TypeDef ordering is total
prop_typedef_ordering_total :: TypeDef -> TypeDef -> Property
prop_typedef_ordering_total td1 td2 =
  let ord = compare td1 td2
  in property $ ord == EQ || ord == LT || ord == GT

-- Property: TypeEnv ordering is total
prop_typeenv_ordering_total :: TypeEnv -> TypeEnv -> Property
prop_typeenv_ordering_total env1 env2 =
  let ord = compare env1 env2
  in property $ ord == EQ || ord == LT || ord == GT

-- Property: DependentTypeChecker ordering is total
prop_dependenttypechecker_ordering_total :: DependentTypeChecker -> DependentTypeChecker -> Property
prop_dependenttypechecker_ordering_total dtc1 dtc2 =
  let ord = compare dtc1 dtc2
  in property $ ord == EQ || ord == LT || ord == GT

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Dependencies QuickCheck Tests"
  [ fastProperty "TypeVar equality is reflexive" prop_typevar_equality_reflexive
  , fastProperty "TypeVar equality is symmetric" prop_typevar_equality_symmetric
  , fastProperty "TypeVar equality is transitive" prop_typevar_equality_transitive
  , fastProperty "TypeVar ordering is consistent" prop_typevar_ordering_consistent
  , fastProperty "TVCon preserves constructor name" prop_tvcon_preserves_name
  , fastProperty "TVVar preserves variable name" prop_tvvar_preserves_name
  , fastProperty "TVApp preserves constructor name and arguments" prop_tvapp_preserves_name_args
  , fastProperty "TVFun preserves parameters and return type" prop_tvfun_preserves_params_return
  , fastProperty "TVTuple preserves elements" prop_tvtuple_preserves_elements
  , fastProperty "TypeConstraint equality is reflexive" prop_typeconstraint_equality_reflexive
  , fastProperty "TypeConstraint equality is symmetric" prop_typeconstraint_equality_symmetric
  , fastProperty "TypeConstraint equality is transitive" prop_typeconstraint_equality_transitive
  , fastProperty "Equal constraint preserves both types" prop_equal_preserves_types
  , fastProperty "Subtype constraint preserves both types" prop_subtype_preserves_types
  , fastProperty "Predicate constraint preserves name and types" prop_predicate_preserves_name_types
  , fastProperty "TypeSizeGE constraint preserves type and size" prop_typesizege_preserves_type_size
  , fastProperty "TypeSizeGT constraint preserves type and size" prop_typesizegt_preserves_type_size
  , fastProperty "TypeRange constraint preserves type and range" prop_typerange_preserves_type_range
  , fastProperty "DependentTypeError equality is reflexive" prop_dependenttypeerror_equality_reflexive
  , fastProperty "DependentTypeError equality is symmetric" prop_dependenttypeerror_equality_symmetric
  , fastProperty "DependentTypeError equality is transitive" prop_dependenttypeerror_equality_transitive
  , fastProperty "DependentTypeMismatch preserves both types" prop_dependenttypemismatch_preserves_types
  , fastProperty "ConstraintViolation preserves constraint and type" prop_constraintviolation_preserves_constraint_type
  , fastProperty "TypeNotFound preserves type name" prop_typenotfound_preserves_name
  , fastProperty "InvalidTypeArgument preserves argument" prop_invalidtypeargument_preserves_argument
  , fastProperty "UnsolvableConstraint preserves constraint" prop_unsolvableconstraint_preserves_constraint
  , fastProperty "TypeDef equality is reflexive" prop_typedef_equality_reflexive
  , fastProperty "TypeDef equality is symmetric" prop_typedef_equality_symmetric
  , fastProperty "TypeDef equality is transitive" prop_typedef_equality_transitive
  , fastProperty "TypeDef preserves parameters and constraints" prop_typedef_preserves_params_constraints
  , fastProperty "TypeEnv equality is reflexive" prop_typeenv_equality_reflexive
  , fastProperty "TypeEnv equality is symmetric" prop_typeenv_equality_symmetric
  , fastProperty "TypeEnv equality is transitive" prop_typeenv_equality_transitive
  , fastProperty "TypeEnv preserves type definitions and constraints" prop_typeenv_preserves_defs_constraints
  , fastProperty "DependentTypeChecker equality is reflexive" prop_dependenttypechecker_equality_reflexive
  , fastProperty "DependentTypeChecker equality is symmetric" prop_dependenttypechecker_equality_symmetric
  , fastProperty "DependentTypeChecker equality is transitive" prop_dependenttypechecker_equality_transitive
  , fastProperty "DependentTypeChecker preserves type environment and errors" prop_dependenttypechecker_preserves_env_errors
  , fastProperty "newDependentTypeChecker creates checker with prelude types" prop_new_dependenttypechecker_prelude
  , fastProperty "newDependentTypeCheckerWithTypes creates checker with custom types" prop_new_dependenttypechecker_with_types
  , fastProperty "preludeTypeDefs contains expected types" prop_prelude_typedefs_contains_expected
  , fastProperty "Different TypeVar constructors create unequal values" prop_typevar_different_constructors_unequal
  , fastProperty "Different TypeConstraint constructors create unequal values" prop_typeconstraint_different_constructors_unequal
  , fastProperty "Different DependentTypeError constructors create unequal values" prop_dependenttypeerror_different_constructors_unequal
  , fastProperty "TypeVar ordering is total" prop_typevar_ordering_total
  , fastProperty "TypeConstraint ordering is total" prop_typeconstraint_ordering_total
  , fastProperty "DependentTypeError ordering is total" prop_dependenttypeerror_ordering_total
  , fastProperty "TypeDef ordering is total" prop_typedef_ordering_total
  , fastProperty "TypeEnv ordering is total" prop_typeenv_ordering_total
  , fastProperty "DependentTypeChecker ordering is total" prop_dependenttypechecker_ordering_total
  ]