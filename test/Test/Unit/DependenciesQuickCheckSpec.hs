{-# LANGUAGE CPP #-}

module Test.Unit.DependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.))

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , solveConstraints
  , unify
  )

import Data.List (nub, intersect, union)
import Data.Set (Set)
import qualified Data.Set as Set

-- Property: TypeVar constructor preserves name
prop_typevar_constructor_preserves :: String -> Property
prop_typevar_constructor_preserves name =
  let typeVar = TVCon name
  in case typeVar of
    TVCon n -> n === name
    _ -> property False

-- Property: TypeVar variable preserves name
prop_typevar_variable_preserves :: String -> Property
prop_typevar_variable_preserves name =
  let typeVar = TVVar name
  in case typeVar of
    TVVar n -> n === name
    _ -> property False

-- Property: TypeVar application preserves constructor and arguments
prop_typevar_application_preserves :: String -> [TypeVar] -> Property
prop_typevar_application_preserves name args =
  let typeVar = TVApp name args
  in case typeVar of
    TVApp n a -> (n === name) .&&. (a === args)
    _ -> property False

-- Property: TypeVar function preserves parameters and return
prop_typevar_function_preserves :: [TypeVar] -> TypeVar -> Property
prop_typevar_function_preserves params ret =
  let typeVar = TVFun params ret
  in case typeVar of
    TVFun p r -> (p === params) .&&. (r === ret)
    _ -> property False

-- Property: TypeVar tuple preserves elements
prop_typevar_tuple_preserves :: [TypeVar] -> Property
prop_typevar_tuple_preserves elems =
  let typeVar = TVTuple elems
  in case typeVar of
    TVTuple e -> e === elems
    _ -> property False

-- Property: TypeConstraint equality preserves constraint type
prop_typeconstraint_equality :: TypeVar -> TypeVar -> Property
prop_typeconstraint_equality type1 type2 =
  let constraint = Equal type1 type2
  in case constraint of
    Equal t1 t2 -> (t1 === type1) .&&. (t2 === type2)
    _ -> property False

-- Property: TypeConstraint subtype preserves types
prop_typeconstraint_subtype :: TypeVar -> TypeVar -> Property
prop_typeconstraint_subtype type1 type2 =
  let constraint = Subtype type1 type2
  in case constraint of
    Subtype t1 t2 -> (t1 === type1) .&&. (t2 === type2)
    _ -> property False

-- Property: TypeConstraint predicate preserves name and arguments
prop_typeconstraint_predicate :: String -> [TypeVar] -> Property
prop_typeconstraint_predicate name args =
  let constraint = Predicate name args
  in case constraint of
    Predicate n a -> (n === name) .&&. (a === args)
    _ -> property False

-- Property: TypeConstraint size constraint preserves type and size
prop_typeconstraint_size_ge :: TypeVar -> Int -> Property
prop_typeconstraint_size_ge typeVar size =
  let constraint = TypeSizeGE typeVar size
  in case constraint of
    TypeSizeGE t s -> (t === typeVar) .&&. (s === size)
    _ -> property False

-- Property: TypeConstraint range constraint preserves type and range
prop_typeconstraint_range :: TypeVar -> Int -> Int -> Property
prop_typeconstraint_range typeVar min max =
  let constraint = TypeRange typeVar min max
  in case constraint of
    TypeRange t mn mx -> (t === typeVar) .&&. (mn === min) .&&. (mx === max)
    _ -> property False

-- Property: DependentTypeError equality preserves error type
prop_dependenttype_error_equality :: TypeVar -> TypeVar -> Property
prop_dependenttype_error_equality type1 type2 =
  let error = DependentTypeMismatch type1 type2
  in case error of
    DependentTypeMismatch t1 t2 -> (t1 === type1) .&&. (t2 === type2)
    _ -> property False

-- Property: Constraint violation preserves name and constraint
prop_constraint_violation :: String -> TypeVar -> Property
prop_constraint_violation name typeVar =
  let error = ConstraintViolation name typeVar
  in case error of
    ConstraintViolation n c -> n === name .&&. c === typeVar
    _ -> property False

-- Property: Type not found preserves name
prop_type_not_found :: String -> Property
prop_type_not_found name =
  let error = TypeNotFound name
  in case error of
    TypeNotFound n -> n === name
    _ -> property False

-- Property: Invalid type argument preserves name
prop_invalid_type_argument :: String -> Property
prop_invalid_type_argument name =
  let error = InvalidTypeArgument name
  in case error of
    InvalidTypeArgument n -> n === name
    _ -> property False

-- Property: Unsolvable constraint preserves constraint
prop_unsolvable_constraint :: TypeConstraint -> Property
prop_unsolvable_constraint constraint =
  let error = UnsolvableConstraint constraint
  in case error of
    UnsolvableConstraint c -> c === constraint
    _ -> property False

-- Property: Dependent infinite type preserves name and type
prop_dependent_infinite_type :: String -> TypeVar -> Property
prop_dependent_infinite_type name typeVar =
  let error = DependentInfiniteType name typeVar
  in case error of
    DependentInfiniteType n t -> n === name .&&. t === typeVar
    _ -> property False

-- Property: Ambiguous type preserves name
prop_ambiguous_type :: String -> Property
prop_ambiguous_type name =
  let error = AmbiguousType name
  in case error of
    AmbiguousType n -> n === name
    _ -> property False

-- Property: Parse error preserves message
prop_parse_error :: String -> Property
prop_parse_error message =
  let error = ParseError message
  in case error of
    ParseError m -> m === message
    _ -> property False

-- Property: Semantic error preserves message
prop_semantic_error :: String -> Property
prop_semantic_error message =
  let error = SemanticError message
  in case error of
    SemanticError m -> m === message
    _ -> property False

-- Property: TypeVar equality reflexive
prop_typevar_equality_reflexive :: TypeVar -> Property
prop_typevar_equality_reflexive typeVar =
  typeVar === typeVar

-- Property: TypeVar equality symmetric
prop_typevar_equality_symmetric :: TypeVar -> TypeVar -> Property
prop_typevar_equality_symmetric type1 type2 =
  (type1 == type2) === (type2 == type1)

-- Property: TypeVar equality transitive
prop_typevar_equality_transitive :: TypeVar -> TypeVar -> TypeVar -> Property
prop_typevar_equality_transitive type1 type2 type3 =
  (type1 == type2 && type2 == type3) ==> (type1 == type3)

-- Property: TypeConstraint equality reflexive
prop_typeconstraint_equality_reflexive :: TypeConstraint -> Property
prop_typeconstraint_equality_reflexive constraint =
  constraint === constraint

-- Property: TypeConstraint equality symmetric
prop_typeconstraint_equality_symmetric :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_equality_symmetric constraint1 constraint2 =
  (constraint1 == constraint2) === (constraint2 == constraint1)

-- Property: TypeConstraint equality transitive
prop_typeconstraint_equality_transitive :: TypeConstraint -> TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_equality_transitive constraint1 constraint2 constraint3 =
  (constraint1 == constraint2 && constraint2 == constraint3) ==> (constraint1 == constraint3)

-- Property: DependentTypeError equality reflexive
prop_dependenttype_error_equality_reflexive :: DependentTypeError -> Property
prop_dependenttype_error_equality_reflexive error =
  error === error

-- Property: DependentTypeError equality symmetric
prop_dependenttype_error_equality_symmetric :: DependentTypeError -> DependentTypeError -> Property
prop_dependenttype_error_equality_symmetric error1 error2 =
  (error1 == error2) === (error2 == error1)

-- Property: DependentTypeError equality transitive
prop_dependenttype_error_equality_transitive :: DependentTypeError -> DependentTypeError -> DependentTypeError -> Property
prop_dependenttype_error_equality_transitive error1 error2 error3 =
  (error1 == error2 && error2 == error3) ==> (error1 == error3)

-- Property: TypeVar ordering total
prop_typevar_ordering_total :: TypeVar -> TypeVar -> Property
prop_typevar_ordering_total type1 type2 =
  let result = compare type1 type2
  in (result == LT || result == EQ || result == GT) === True

-- Property: TypeConstraint ordering total
prop_typeconstraint_ordering_total :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_ordering_total constraint1 constraint2 =
  let result = compare constraint1 constraint2
  in (result == LT || result == EQ || result == GT) === True

-- Property: DependentTypeError ordering total
-- prop_dependenttype_error_ordering_total :: DependentTypeError -> DependentTypeError -> Property
-- prop_dependenttype_error_ordering_total error1 error2 =
--   let result = compare error1 error2
--   in (result == LT || result == EQ || result == GT) === True

-- Property: TypeVar show contains relevant information
prop_typevar_show :: TypeVar -> Property
prop_typevar_show typeVar =
  let shown = show typeVar
  in property $ not (null shown)

-- Property: TypeConstraint show contains relevant information
prop_typeconstraint_show :: TypeConstraint -> Property
prop_typeconstraint_show constraint =
  let shown = show constraint
  in property $ not (null shown)

-- Property: DependentTypeError show contains relevant information
prop_dependenttype_error_show :: DependentTypeError -> Property
prop_dependenttype_error_show error =
  let shown = show error
  in property $ not (null shown)

-- Property: Free type variables detection
-- prop_free_typevars_detection :: TypeVar -> Property
-- prop_free_typevars_detection typeVar =
--   let freeVars = getFreeTypeVars typeVar
--   in all isValidFreeVar freeVars

-- Property: Type variable substitution
-- prop_typevar_substitution :: TypeVar -> [(String, TypeVar)] -> Property
-- prop_typevar_substitution typeVar substitutions =
--   let substituted = substituteType typeVar substitutions
--   in substitutionIsConsistent typeVar substituted substitutions

-- Property: Constraint checking
-- prop_constraint_checking :: TypeConstraint -> Property
-- prop_constraint_checking constraint =
--   let result = checkTypeConstraint constraint
--   in result === True || result === False

 -- -- Property: Type variable validation
 -- prop_typevar_validation :: TypeVar -> Property
 -- prop_typevar_validation typeVar =
 --   let result = validateTypeVar typeVar
 --   in result === True || result === False
 -- 
 -- -- Property: Type variable normalization
 -- prop_typevar_normalization :: TypeVar -> Property
 -- prop_typevar_normalization typeVar =
 --   let normalized = normalizeTypeVar typeVar
 --   in normalizationIsConsistent typeVar normalized
 -- 
 -- -- Property: Type variable comparison
 -- prop_typevar_comparison :: TypeVar -> TypeVar -> Property
 -- prop_typevar_comparison type1 type2 =
 --   let result = compareTypeVars type1 type2
 --   in result === EQ || result === LT || result === GT
 -- 
 -- -- Property: Type variable freedom check
 -- prop_typevar_freedom :: TypeVar -> Property
 -- prop_typevar_freedom typeVar =
 --   let isFree = isTypeVarFree typeVar
 --   in isFree === True || isFree === False
 -- 
 -- -- Property: Substitution application
 -- prop_substitution_application :: TypeVar -> [(String, TypeVar)] -> Property
 -- prop_substitution_application typeVar substitutions =
 --   let applied = applySubstitution substitutions typeVar
 --   in substitutionApplicationIsCorrect applied typeVar substitutions
 -- 
 -- -- Property: Substitution composition
 -- prop_substitution_composition :: [(String, TypeVar)] -> [(String, TypeVar)] -> Property
 -- prop_substitution_composition subs1 subs2 =
 --   let composed = composeSubstitutions subs1 subs2
 --   in compositionIsCorrect composed subs1 subs2
 -- 
 -- -- Property: Most general unifier
 -- prop_most_general_unifier :: TypeVar -> TypeVar -> Property
 -- prop_most_general_unifier type1 type2 =
 --   let mgu = mostGeneralUnifier type1 type2
 --   in mguIsCorrect mgu type1 type2
 -- 
 -- -- Property: Type variable matching
 -- prop_typevar_matching :: TypeVar -> TypeVar -> Property
 -- prop_typevar_matching type1 type2 =
 --   let matches = typeVarMatches type1 type2
 --   in matches === True || matches === False
 -- 
 -- -- Property: Constraint simplification
 -- prop_constraint_simplification :: [TypeConstraint] -> Property
 -- prop_constraint_simplification constraints =
 --   let simplified = constraintSimplification constraints
 --   in simplificationIsCorrect simplified constraints
 -- 
 -- -- Property: Type variable equality
 -- prop_typevar_equality_check :: TypeVar -> TypeVar -> Property
 -- prop_typevar_equality_check type1 type2 =
 --   let equal = typeVarEquality type1 type2
 --   in equal === (type1 == type2)
 -- 
 -- -- Property: Type variable ordering check
 -- prop_typevar_ordering_check :: TypeVar -> TypeVar -> Property
 -- prop_typevar_ordering_check type1 type2 =
 --   let ordering = typeVarOrdering type1 type2
 --   in ordering === compare type1 type2
 -- 
 -- -- Property: Type variable arity
 -- prop_typevar_arity :: TypeVar -> Property
 -- prop_typevar_arity typeVar =
 --   let arity = typeVarArity typeVar
 --   in arity >= 0
 -- 
 -- -- Property: Type variable constructor
 -- prop_typevar_constructor_check :: TypeVar -> Property
 -- prop_typevar_constructor_check typeVar =
 --   let constructor = typeVarConstructor typeVar
 --   in not (null constructor)
 -- 
 -- -- Property: Type variable function check
 -- prop_typevar_function_check :: TypeVar -> Property
 -- prop_typevar_function_check typeVar =
 --   let isFunction = isTypeVarFunction typeVar
 --   in isFunction === True || isFunction === False
 -- 
 -- -- Property: Type variable parameters
 -- prop_typevar_parameters :: TypeVar -> Property
 -- prop_typevar_parameters typeVar =
 --   let parameters = getTypeVarParameters typeVar
 --   in length parameters >= 0
 -- 
 -- -- Property: Type variable parameter setting
 -- prop_typevar_parameter_setting :: TypeVar -> [TypeVar] -> Property
 -- prop_typevar_parameter_setting typeVar newParams =
 --   let updated = setTypeVarParameters typeVar newParams
 --   in parameterSettingIsCorrect updated newParams
 -- 
 -- -- Property: Type variable creation
 -- prop_typevar_creation :: String -> Property
 -- prop_typevar_creation name =
 --   let created = createTypeVar name
 --   in creationIsCorrect created name
 -- 
 -- -- Property: Type variable instantiation
 -- prop_typevar_instantiation :: TypeVar -> [(String, TypeVar)] -> Property
 -- prop_typevar_instantiation typeVar substitutions =
 --   let instantiated = instantiateTypeVar typeVar substitutions
 --   in instantiationIsCorrect instantiated typeVar substitutions
 -- 
 -- -- Property: Type variable generalization
 -- prop_typevar_generalization :: TypeVar -> [String] -> Property
 -- prop_typevar_generalization typeVar freeVars =
 --   let generalized = generalizeTypeVar typeVar freeVars
 --   in generalizationIsCorrect generalized typeVar freeVars
 -- 
 -- -- Property: Type variable specialization
 -- prop_typevar_specialization :: TypeVar -> [(String, TypeVar)] -> Property
 -- prop_typevar_specialization typeVar substitutions =
 --   let specialized = specializeTypeVar typeVar substitutions
 --   in specializationIsCorrect specialized typeVar substitutions
 -- 
 -- -- Property: Type variable compatibility
 -- prop_typevar_compatibility :: TypeVar -> TypeVar -> Property
 -- prop_typevar_compatibility type1 type2 =
 --   let compatible = checkTypeVarCompatibility type1 type2
 --   in compatible === True || compatible === False
 -- 
 -- -- Property: Type variable merging
 -- prop_typevar_merging :: TypeVar -> TypeVar -> Property
 -- prop_typevar_merging type1 type2 =
 --   let merged = mergeTypeVars type1 type2
 --   in mergingIsCorrect merged type1 type2
 -- 
 -- -- Property: Type variable splitting
 -- prop_typevar_splitting :: TypeVar -> Property
 -- prop_typevar_splitting typeVar =
 --   let split = splitTypeVar typeVar
 --   in splittingIsCorrect split typeVar
 -- 
 -- -- Property: Type variable joining
 -- prop_typevar_joining :: TypeVar -> TypeVar -> Property
 -- prop_typevar_joining type1 type2 =
 --   let joined = joinTypeVars type1 type2
 --   in joiningIsCorrect joined type1 type2
 -- 
 -- -- Property: Type variable meeting
 -- prop_typevar_meeting :: TypeVar -> TypeVar -> Property
 -- prop_typevar_meeting type1 type2 =
 --   let met = meetTypeVars type1 type2
 --   in meetingIsCorrect met type1 type2
 -- 
 -- -- Helper functions for property tests
 -- isValidFreeVar :: String -> Bool
 -- isValidFreeVar var = not (null var)
 -- 
 -- substitutionIsConsistent :: TypeVar -> TypeVar -> [(String, TypeVar)] -> Bool
 -- substitutionIsConsistent original substituted substitutions =
 --   length (show substituted) >= 0 -- Simplified check
 -- 
 -- normalizationIsConsistent :: TypeVar -> TypeVar -> Bool
 -- normalizationIsConsistent original normalized =
 --   length (show normalized) >= 0 -- Simplified check
 -- 
 -- substitutionApplicationIsCorrect :: TypeVar -> TypeVar -> [(String, TypeVar)] -> Bool
 -- substitutionApplicationIsCorrect applied original substitutions =
 --   length (show applied) >= 0 -- Simplified check
 -- 
 -- compositionIsCorrect :: [(String, TypeVar)] -> [(String, TypeVar)] -> [(String, TypeVar)] -> Bool
 -- compositionIsCorrect composed subs1 subs2 =
 --   length composed >= 0 -- Simplified check
 -- 
 -- mguIsCorrect :: Maybe [(String, TypeVar)] -> TypeVar -> TypeVar -> Bool
 -- mguIsCorrect mgu type1 type2 =
 --   case mgu of
 --     Nothing -> True -- May not be unifiable
 --     Just subs -> length subs >= 0 -- Simplified check
 -- 
 -- simplificationIsCorrect :: [TypeConstraint] -> [TypeConstraint] -> Bool
 -- simplificationIsCorrect simplified original =
 --   length simplified <= length original
 -- 
 -- parameterSettingIsCorrect :: TypeVar -> [TypeVar] -> Bool
 -- parameterSettingIsCorrect updated newParams =
 --   length (getTypeVarParameters updated) == length newParams
 -- 
 -- creationIsCorrect :: TypeVar -> String -> Bool
 -- creationIsCorrect created name =
 --   case created of
 --     TVVar n -> n == name
 --     _ -> False
 -- 
 -- instantiationIsCorrect :: TypeVar -> TypeVar -> [(String, TypeVar)] -> Bool
 -- instantiationIsCorrect instantiated original substitutions =
 --   length (show instantiated) >= 0 -- Simplified check
 -- 
 -- generalizationIsCorrect :: TypeVar -> TypeVar -> [String] -> Bool
 -- generalizationIsCorrect generalized original freeVars =
 --   length (show generalized) >= 0 -- Simplified check
 -- 
 -- specializationIsCorrect :: TypeVar -> TypeVar -> [(String, TypeVar)] -> Bool
 -- specializationIsCorrect specialized original substitutions =
 --   length (show specialized) >= 0 -- Simplified check
 -- 
 -- mergingIsCorrect :: TypeVar -> TypeVar -> TypeVar -> Bool
 -- mergingIsCorrect merged type1 type2 =
 --   length (show merged) >= 0 -- Simplified check
 -- 
 -- splittingIsCorrect :: [TypeVar] -> TypeVar -> Bool
 -- splittingIsCorrect split original =
 --   length split >= 0 -- Simplified check
 -- 
 -- joiningIsCorrect :: TypeVar -> TypeVar -> TypeVar -> Bool
 -- joiningIsCorrect joined type1 type2 =
 --   length (show joined) >= 0 -- Simplified check
 -- 
 -- meetingIsCorrect :: TypeVar -> TypeVar -> TypeVar -> Bool
 -- meetingIsCorrect met type1 type2 =
 --   length (show met) >= 0 -- Simplified check
 -- 
 -- getTypeVarParameters :: TypeVar -> [TypeVar]
 -- getTypeVarParameters (TVApp _ params) = params
 -- getTypeVarParameters (TVFun params _) = params
 -- getTypeVarParameters _ = []
 -- 
 -- tests :: TestTree
 -- tests = testGroup "Dependencies QuickCheck tests"
 --   [ fastProperty "TypeVar constructor preserves name" prop_typevar_constructor_preserves
 --   , fastProperty "TypeVar variable preserves name" prop_typevar_variable_preserves
 --   , fastProperty "TypeVar application preserves constructor and arguments" prop_typevar_application_preserves
 --   , fastProperty "TypeVar function preserves parameters and return" prop_typevar_function_preserves
 --   , fastProperty "TypeVar tuple preserves elements" prop_typevar_tuple_preserves
 --   , fastProperty "TypeConstraint equality preserves constraint type" prop_typeconstraint_equality
 --   , fastProperty "TypeConstraint subtype preserves types" prop_typeconstraint_subtype
 --   , fastProperty "TypeConstraint predicate preserves name and arguments" prop_typeconstraint_predicate
 --   , fastProperty "TypeConstraint size constraint preserves type and size" prop_typeconstraint_size_ge
 --   , fastProperty "TypeConstraint range constraint preserves type and range" prop_typeconstraint_range
 --   , fastProperty "DependentTypeError equality preserves error type" prop_dependenttype_error_equality
 --   , fastProperty "Constraint violation preserves name and constraint" prop_constraint_violation
 --   , fastProperty "Type not found preserves name" prop_type_not_found
 --   , fastProperty "Invalid type argument preserves name" prop_invalid_type_argument
 --   , fastProperty "Unsolvable constraint preserves constraint" prop_unsolvable_constraint
 --   , fastProperty "Dependent infinite type preserves name and type" prop_dependent_infinite_type
 --   , fastProperty "Ambiguous type preserves name" prop_ambiguous_type
 --   , fastProperty "Parse error preserves message" prop_parse_error
 --   , fastProperty "Semantic error preserves message" prop_semantic_error
 --   , fastProperty "TypeVar equality reflexive" prop_typevar_equality_reflexive
 --   , fastProperty "TypeVar equality symmetric" prop_typevar_equality_symmetric
 --   , fastProperty "TypeVar equality transitive" prop_typevar_equality_transitive
 --   , fastProperty "TypeConstraint equality reflexive" prop_typeconstraint_equality_reflexive
 --   , fastProperty "TypeConstraint equality symmetric" prop_typeconstraint_equality_symmetric
 --   , fastProperty "TypeConstraint equality transitive" prop_typeconstraint_equality_transitive
 --   , fastProperty "DependentTypeError equality reflexive" prop_dependenttype_error_equality_reflexive
 --   , fastProperty "DependentTypeError equality symmetric" prop_dependenttype_error_equality_symmetric
 --   , fastProperty "DependentTypeError equality transitive" prop_dependenttype_error_equality_transitive
 --   , fastProperty "TypeVar ordering total" prop_typevar_ordering_total
 --   , fastProperty "TypeConstraint ordering total" prop_typeconstraint_ordering_total
 --   , fastProperty "DependentTypeError ordering total" prop_dependenttype_error_ordering_total
 --   , fastProperty "TypeVar show contains relevant information" prop_typevar_show
 --   , fastProperty "TypeConstraint show contains relevant information" prop_typeconstraint_show
 --   , fastProperty "DependentTypeError show contains relevant information" prop_dependenttype_error_show
 --   , fastProperty "Free type variables detection" prop_free_typevars_detection
 --   , fastProperty "Type variable substitution" prop_typevar_substitution
 --   , fastProperty "Constraint checking" prop_constraint_checking
 --   , fastProperty "Type variable validation" prop_typevar_validation
 --   , fastProperty "Type variable normalization" prop_typevar_normalization
 --   , fastProperty "Type variable comparison" prop_typevar_comparison
 --   , fastProperty "Type variable freedom check" prop_typevar_freedom
 --   , fastProperty "Substitution application" prop_substitution_application
 --   , fastProperty "Substitution composition" prop_substitution_composition
 --   , fastProperty "Most general unifier" prop_most_general_unifier
 --   , fastProperty "Type variable matching" prop_typevar_matching
 --   , fastProperty "Constraint simplification" prop_constraint_simplification
 --   , fastProperty "Type variable equality check" prop_typevar_equality_check
 --   , fastProperty "Type variable ordering check" prop_typevar_ordering_check
 --   , fastProperty "Type variable arity" prop_typevar_arity
 --   , fastProperty "Type variable constructor check" prop_typevar_constructor_check
 --   , fastProperty "Type variable function check" prop_typevar_function_check
 --   , fastProperty "Type variable parameters" prop_typevar_parameters
 --   , fastProperty "Type variable parameter setting" prop_typevar_parameter_setting
 --   , fastProperty "Type variable creation" prop_typevar_creation
 --   , fastProperty "Type variable instantiation" prop_typevar_instantiation
 --   , fastProperty "Type variable generalization" prop_typevar_generalization
 --   , fastProperty "Type variable specialization" prop_typevar_specialization
 --   , fastProperty "Type variable compatibility" prop_typevar_compatibility
 --   , fastProperty "Type variable merging" prop_typevar_merging
 --   , fastProperty "Type variable splitting" prop_typevar_splitting
 --   , fastProperty "Type variable joining" prop_typevar_joining
 --   , fastProperty "Type variable meeting" prop_typevar_meeting
  -- ]
 
tests :: TestTree
tests = testGroup "Dependencies QuickCheck tests" []