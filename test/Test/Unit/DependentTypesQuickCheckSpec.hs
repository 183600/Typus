{-# LANGUAGE CPP #-}

module Test.Unit.DependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  )
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

-- Property: TVCon preserves constructor name
prop_tvcon_preserves_name :: String -> Property
prop_tvcon_preserves_name name =
  let typeVar = TVCon name
  in case typeVar of
    TVCon n -> n === name
    _ -> property False

-- Property: TVVar preserves variable name
prop_tvvar_preserves_name :: String -> Property
prop_tvvar_preserves_name name =
  let typeVar = TVVar name
  in case typeVar of
    TVVar n -> n === name
    _ -> property False

-- Property: TVApp preserves constructor and args
prop_tvapp_preserves :: String -> [TypeVar] -> Property
prop_tvapp_preserves name args =
  let typeVar = TVApp name args
  in case typeVar of
    TVApp n a -> n === name && a === args
    _ -> property False

-- Property: TVFun preserves params and result
prop_tvfun_preserves :: [TypeVar] -> TypeVar -> Property
prop_tvfun_preserves params result =
  let typeVar = TVFun params result
  in case typeVar of
    TVFun p r -> p === params && r === result
    _ -> property False

-- Property: TVTuple preserves elements
prop_tvtuple_preserves :: [TypeVar] -> Property
prop_tvtuple_preserves elements =
  let typeVar = TVTuple elements
  in case typeVar of
    TVTuple e -> e === elements
    _ -> property False

-- Property: TypeVar equality
prop_typevar_eq :: TypeVar -> TypeVar -> Property
prop_typevar_eq tv1 tv2 =
  (tv1 == tv2) === case (tv1, tv2) of
    (TVCon n1, TVCon n2) -> n1 == n2
    (TVVar n1, TVVar n2) -> n1 == n2
    (TVApp n1 a1, TVApp n2 a2) -> n1 == n2 && a1 == a2
    (TVFun p1 r1, TVFun p2 r2) -> p1 == p2 && r1 == r2
    (TVTuple e1, TVTuple e2) -> e1 == e2
    _ -> False

-- Property: TypeVar ordering
prop_typevar_ordering :: TypeVar -> TypeVar -> Property
prop_typevar_ordering tv1 tv2 =
  let result = compare tv1 tv2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeVar show
prop_typevar_show :: TypeVar -> Property
prop_typevar_show typeVar =
  let shown = show typeVar
  in not (null shown)

-- Property: TypeVar show contains name for simple types
prop_typevar_show_contains_name :: String -> Property
prop_typevar_show_contains_name name =
  let con = TVCon name
      var = TVVar name
      shownCon = show con
      shownVar = show var
  in name `isInfixOf` shownCon &&
     name `isInfixOf` shownVar

-- Property: Equal constraint preserves types
prop_equal_preserves :: TypeVar -> TypeVar -> Property
prop_equal_preserves tv1 tv2 =
  let constraint = Equal tv1 tv2
  in case constraint of
    Equal t1 t2 -> t1 === tv1 && t2 === tv2
    _ -> property False

-- Property: Subtype constraint preserves types
prop_subtype_preserves :: TypeVar -> TypeVar -> Property
prop_subtype_preserves tv1 tv2 =
  let constraint = Subtype tv1 tv2
  in case constraint of
    Subtype t1 t2 -> t1 === tv1 && t2 === tv2
    _ -> property False

-- Property: Predicate constraint preserves name and args
prop_predicate_preserves :: String -> [TypeVar] -> Property
prop_predicate_preserves name args =
  let constraint = Predicate name args
  in case constraint of
    Predicate n a -> n === name && a === args
    _ -> property False

-- Property: TypeSizeGE constraint preserves type and size
prop_typesizege_preserves :: TypeVar -> Int -> Property
prop_typesizege_preserves tv size =
  let constraint = TypeSizeGE tv size
  in case constraint of
    TypeSizeGE t s -> t === tv && s === size
    _ -> property False

-- Property: TypeSizeGT constraint preserves type and size
prop_typesizegt_preserves :: TypeVar -> Int -> Property
prop_typesizegt_preserves tv size =
  let constraint = TypeSizeGT tv size
  in case constraint of
    TypeSizeGT t s -> t === tv && s === size
    _ -> property False

-- Property: TypeRange constraint preserves type and bounds
prop_typerange_preserves :: TypeVar -> Int -> Int -> Property
prop_typerange_preserves tv min max =
  let constraint = TypeRange tv min max
  in case constraint of
    TypeRange t mn mx -> t === tv && mn === min && mx === max
    _ -> property False

-- Property: TypeConstraint equality
prop_typeconstraint_eq :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_eq tc1 tc2 =
  (tc1 == tc2) === case (tc1, tc2) of
    (Equal t1 t2, Equal t1' t2') -> t1 == t1' && t2 == t2'
    (Subtype t1 t2, Subtype t1' t2') -> t1 == t1' && t2 == t2'
    (Predicate n1 a1, Predicate n2 a2) -> n1 == n2 && a1 == a2
    (TypeSizeGE t1 s1, TypeSizeGE t2 s2) -> t1 == t2 && s1 == s2
    (TypeSizeGT t1 s1, TypeSizeGT t2 s2) -> t1 == t2 && s1 == s2
    (TypeRange t1 mn1 mx1, TypeRange t2 mn2 mx2) -> t1 == t2 && mn1 == mn2 && mx1 == mx2
    _ -> False

-- Property: TypeConstraint ordering
prop_typeconstraint_ordering :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_ordering tc1 tc2 =
  let result = compare tc1 tc2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeConstraint show
prop_typeconstraint_show :: TypeConstraint -> Property
prop_typeconstraint_show constraint =
  let shown = show constraint
  in not (null shown)

-- Property: DependentTypeMismatch error preserves types
prop_dependenttypemismatch_preserves :: TypeVar -> TypeVar -> Property
prop_dependenttypemismatch_preserves tv1 tv2 =
  let err = DependentTypeMismatch tv1 tv2
  in case err of
    DependentTypeMismatch t1 t2 -> t1 === tv1 && t2 === tv2
    _ -> property False

-- Property: ConstraintViolation error preserves name and type
prop_constraintviolation_preserves :: String -> TypeVar -> Property
prop_constraintviolation_preserves name tv =
  let err = ConstraintViolation name tv
  in case err of
    ConstraintViolation n t -> n === name && t === tv
    _ -> property False

-- Property: TypeNotFound error preserves name
prop_typenotfound_preserves :: String -> Property
prop_typenotfound_preserves name =
  let err = TypeNotFound name
  in case err of
    TypeNotFound n -> n === name
    _ -> property False

-- Property: InvalidTypeArgument error preserves name
prop_invalidtypeargument_preserves :: String -> Property
prop_invalidtypeargument_preserves name =
  let err = InvalidTypeArgument name
  in case err of
    InvalidTypeArgument n -> n === name
    _ -> property False

-- Property: UnsolvableConstraint error preserves constraint
prop_unsolvableconstraint_preserves :: TypeConstraint -> Property
prop_unsolvableconstraint_preserves constraint =
  let err = UnsolvableConstraint constraint
  in case err of
    UnsolvableConstraint c -> c === constraint
    _ -> property False

-- Property: DependentInfiniteType error preserves name and type
prop_dependentinfinitetype_preserves :: String -> TypeVar -> Property
prop_dependentinfinitetype_preserves name tv =
  let err = DependentInfiniteType name tv
  in case err of
    DependentInfiniteType n t -> n === name && t === tv
    _ -> property False

-- Property: AmbiguousType error preserves name
prop_ambiguoustype_preserves :: String -> Property
prop_ambiguoustype_preserves name =
  let err = AmbiguousType name
  in case err of
    AmbiguousType n -> n === name
    _ -> property False

-- Property: ParseError error preserves message
prop_parseerror_preserves :: String -> Property
prop_parseerror_preserves message =
  let err = ParseError message
  in case err of
    ParseError m -> m === message
    _ -> property False

-- Property: SemanticError error preserves message
prop_semanticerror_preserves :: String -> Property
prop_semanticerror_preserves message =
  let err = SemanticError message
  in case err of
    SemanticError m -> m === message
    _ -> property False

-- Property: DependentTypeError equality
prop_dependenttypeerror_eq :: DependentTypeError -> DependentTypeError -> Property
prop_dependenttypeerror_eq err1 err2 =
  (err1 == err2) === case (err1, err2) of
    (DependentTypeMismatch t1 t2, DependentTypeMismatch t1' t2') -> t1 == t1' && t2 == t2'
    (ConstraintViolation n1 t1, ConstraintViolation n2 t2) -> n1 == n2 && t1 == t2
    (TypeNotFound n1, TypeNotFound n2) -> n1 == n2
    (InvalidTypeArgument n1, InvalidTypeArgument n2) -> n1 == n2
    (UnsolvableConstraint c1, UnsolvableConstraint c2) -> c1 == c2
    (DependentInfiniteType n1 t1, DependentInfiniteType n2 t2) -> n1 == n2 && t1 == t2
    (AmbiguousType n1, AmbiguousType n2) -> n1 == n2
    (ParseError m1, ParseError m2) -> m1 == m2
    (SemanticError m1, SemanticError m2) -> m1 == m2
    _ -> False

-- Property: TypeDef preserves params and constraints
prop_typedef_preserves :: [String] -> [TypeConstraint] -> Property
prop_typedef_preserves params constraints =
  let typeDef = TypeDefDecl params constraints
  in case typeDef of
    TypeDefDecl p c -> p === params && c === constraints
    _ -> property False

-- Property: TypeEnv preserves maps and constraints
prop_typeenv_preserves :: [(String, TypeDef)] -> [TypeConstraint] -> Property
prop_typeenv_preserves pairs constraints =
  let typeDefs = Map.fromList pairs
      env = TypeEnv typeDefs constraints
  in typeDefinitions env === typeDefs &&
     pendingConstraints env === constraints

-- Property: TypeEnv with empty collections
prop_typeenv_empty :: Property
prop_typeenv_empty =
  let env = TypeEnv Map.empty []
  in Map.null (typeDefinitions env) &&
     null (pendingConstraints env)

-- Property: DependentTypeChecker preserves env and errors
prop_dependenttypechecker_preserves :: TypeEnv -> [DependentTypeError] -> Property
prop_dependenttypechecker_preserves env errors =
  let checker = DependentTypeChecker env errors
  in dtcTypeEnv checker === env &&
     tcErrors checker === errors

-- Property: DependentTypeChecker with empty collections
prop_dependenttypechecker_empty :: Property
prop_dependenttypechecker_empty =
  let env = TypeEnv Map.empty []
      checker = DependentTypeChecker env []
  in Map.null (typeDefinitions (dtcTypeEnv checker)) &&
     null (pendingConstraints (dtcTypeEnv checker)) &&
     null (tcErrors checker)

-- Property: TVApp with empty args
prop_tvapp_empty_args :: String -> Property
prop_tvapp_empty_args name =
  let typeVar = TVApp name []
  in case typeVar of
    TVApp n a -> n === name && null a
    _ -> property False

-- Property: TVFun with empty params
prop_tvfun_empty_params :: TypeVar -> Property
prop_tvfun_empty_params result =
  let typeVar = TVFun [] result
  in case typeVar of
    TVFun p r -> null p && r === result
    _ -> property False

-- Property: TVTuple with empty elements
prop_tvtuple_empty :: Property
prop_tvtuple_empty =
  let typeVar = TVTuple []
  in case typeVar of
    TVTuple e -> null e
    _ -> property False

-- Property: TypeDef with empty params
prop_typedef_empty_params :: [TypeConstraint] -> Property
prop_typedef_empty_params constraints =
  let typeDef = TypeDefDecl [] constraints
  in case typeDef of
    TypeDefDecl p c -> null p && c === constraints
    _ -> property False

-- Property: TypeDef with empty constraints
prop_typedef_empty_constraints :: [String] -> Property
prop_typedef_empty_constraints params =
  let typeDef = TypeDefDecl params []
  in case typeDef of
    TypeDefDecl p c -> p === params && null c
    _ -> property False

-- Property: TypeDef with both empty
prop_typedef_empty_both :: Property
prop_typedef_empty_both =
  let typeDef = TypeDefDecl [] []
  in case typeDef of
    TypeDefDecl p c -> null p && null c
    _ -> property False

-- Property: Predicate with empty args
prop_predicate_empty_args :: String -> Property
prop_predicate_empty_args name =
  let constraint = Predicate name []
  in case constraint of
    Predicate n a -> n === name && null a
    _ -> property False

-- Property: TypeRange with same bounds
prop_typerange_same_bounds :: TypeVar -> Int -> Property
prop_typerange_same_bounds tv bound =
  let constraint = TypeRange tv bound bound
  in case constraint of
    TypeRange t mn mx -> t === tv && mn === bound && mx === bound
    _ -> property False

-- Property: TypeVar with special characters
prop_typevar_special_chars :: Property
prop_typevar_special_chars =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      con = TVCon specialChars
      var = TVVar specialChars
  in case (con, var) of
    (TVCon name, TVVar vname) -> name === specialChars && vname === specialChars
    _ -> property False

-- Property: TypeVar with Unicode characters
prop_typevar_unicode :: Property
prop_typevar_unicode =
  let unicode = "测试变量名🚀"
      con = TVCon unicode
      var = TVVar unicode
  in case (con, var) of
    (TVCon name, TVVar vname) -> name === unicode && vname === unicode
    _ -> property False

-- Property: DependentTypeError with empty message
prop_dependenttypeerror_empty_message :: Property
prop_dependenttypeerror_empty_message =
  let parseError = ParseError ""
      semanticError = SemanticError ""
  in case (parseError, semanticError) of
    (ParseError m, SemanticError s) -> m === "" && s === ""
    _ -> property False

-- Property: DependentTypeError with Unicode message
prop_dependenttypeerror_unicode :: Property
prop_dependenttypeerror_unicode =
  let unicode = "测试错误信息🚀"
      parseError = ParseError unicode
      semanticError = SemanticError unicode
  in case (parseError, semanticError) of
    (ParseError m, SemanticError s) -> m === unicode && s === unicode
    _ -> property False

tests :: TestTree
tests = testGroup "DependentTypes QuickCheck tests"
  [ fastProperty "TVCon preserves constructor name" prop_tvcon_preserves_name
  , fastProperty "TVVar preserves variable name" prop_tvvar_preserves_name
  , fastProperty "TVApp preserves constructor and args" prop_tvapp_preserves
  , fastProperty "TVFun preserves params and result" prop_tvfun_preserves
  , fastProperty "TVTuple preserves elements" prop_tvtuple_preserves
  , fastProperty "TypeVar equality" prop_typevar_eq
  , fastProperty "TypeVar ordering" prop_typevar_ordering
  , fastProperty "TypeVar show" prop_typevar_show
  , fastProperty "TypeVar show contains name for simple types" prop_typevar_show_contains_name
  , fastProperty "Equal constraint preserves types" prop_equal_preserves
  , fastProperty "Subtype constraint preserves types" prop_subtype_preserves
  , fastProperty "Predicate constraint preserves name and args" prop_predicate_preserves
  , fastProperty "TypeSizeGE constraint preserves type and size" prop_typesizege_preserves
  , fastProperty "TypeSizeGT constraint preserves type and size" prop_typesizegt_preserves
  , fastProperty "TypeRange constraint preserves type and bounds" prop_typerange_preserves
  , fastProperty "TypeConstraint equality" prop_typeconstraint_eq
  , fastProperty "TypeConstraint ordering" prop_typeconstraint_ordering
  , fastProperty "TypeConstraint show" prop_typeconstraint_show
  , fastProperty "DependentTypeMismatch error preserves types" prop_dependenttypemismatch_preserves
  , fastProperty "ConstraintViolation error preserves name and type" prop_constraintviolation_preserves
  , fastProperty "TypeNotFound error preserves name" prop_typenotfound_preserves
  , fastProperty "InvalidTypeArgument error preserves name" prop_invalidtypeargument_preserves
  , fastProperty "UnsolvableConstraint error preserves constraint" prop_unsolvableconstraint_preserves
  , fastProperty "DependentInfiniteType error preserves name and type" prop_dependentinfinitetype_preserves
  , fastProperty "AmbiguousType error preserves name" prop_ambiguoustype_preserves
  , fastProperty "ParseError error preserves message" prop_parseerror_preserves
  , fastProperty "SemanticError error preserves message" prop_semanticerror_preserves
  , fastProperty "DependentTypeError equality" prop_dependenttypeerror_eq
  , fastProperty "TypeDef preserves params and constraints" prop_typedef_preserves
  , fastProperty "TypeEnv preserves maps and constraints" prop_typeenv_preserves
  , fastProperty "TypeEnv with empty collections" prop_typeenv_empty
  , fastProperty "DependentTypeChecker preserves env and errors" prop_dependenttypechecker_preserves
  , fastProperty "DependentTypeChecker with empty collections" prop_dependenttypechecker_empty
  , fastProperty "TVApp with empty args" prop_tvapp_empty_args
  , fastProperty "TVFun with empty params" prop_tvfun_empty_params
  , fastProperty "TVTuple with empty elements" prop_tvtuple_empty
  , fastProperty "TypeDef with empty params" prop_typedef_empty_params
  , fastProperty "TypeDef with empty constraints" prop_typedef_empty_constraints
  , fastProperty "TypeDef with both empty" prop_typedef_empty_both
  , fastProperty "Predicate with empty args" prop_predicate_empty_args
  , fastProperty "TypeRange with same bounds" prop_typerange_same_bounds
  , fastProperty "TypeVar with special characters" prop_typevar_special_chars
  , fastProperty "TypeVar with Unicode characters" prop_typevar_unicode
  , fastProperty "DependentTypeError with empty message" prop_dependenttypeerror_empty_message
  , fastProperty "DependentTypeError with Unicode message" prop_dependenttypeerror_unicode
  ]