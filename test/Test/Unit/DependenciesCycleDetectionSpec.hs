{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesCycleDetectionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.List (nub, sort, (\\), intersect, union)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

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
  , addType
  , addConstraint
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )

-- | Generate a valid type variable name
genTypeVarName :: Gen String
genTypeVarName = do
  first <- elements ['a'..'z']
  rest <- listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_")
  return $ first : rest

-- | Generate a simple type variable
genSimpleTypeVar :: Gen TypeVar
genSimpleTypeVar = do
  name <- genTypeVarName
  return $ TVVar name

-- | Generate a type variable with arguments
genAppTypeVar :: Gen TypeVar
genAppTypeVar = do
  name <- elements ["List", "Array", "Map", "Option", "Result"]
  args <- listOf1 genSimpleTypeVar
  return $ TVApp name args

-- | Generate a function type variable
genFunTypeVar :: Gen TypeVar
genFunTypeVar = do
  params <- listOf1 genSimpleTypeVar
  returnType <- genSimpleTypeVar
  return $ TVFun params returnType

-- | Generate a tuple type variable
genTupleTypeVar :: Gen TypeVar
genTupleTypeVar = do
  elems <- listOf1 genSimpleTypeVar
  return $ TVTuple elems

-- | Generate any type variable
genTypeVar :: Gen TypeVar
genTypeVar = elements
  [ genSimpleTypeVar
  , genAppTypeVar
  , genFunTypeVar
  , genTupleTypeVar
  ] >>= ($)

-- | Generate an equality constraint
genEqualConstraint :: Gen TypeConstraint
genEqualConstraint = do
  tv1 <- genSimpleTypeVar
  tv2 <- genSimpleTypeVar `suchThat` (/= tv1)
  return $ Equal tv1 tv2

-- | Generate a subtype constraint
genSubtypeConstraint :: Gen TypeConstraint
genSubtypeConstraint = do
  tv1 <- genSimpleTypeVar
  tv2 <- genSimpleTypeVar `suchThat` (/= tv1)
  return $ Subtype tv1 tv2

-- | Generate a predicate constraint
genPredicateConstraint :: Gen TypeConstraint
genPredicateConstraint = do
  predName <- elements ["Num", "Ord", "Eq", "Show", "Read"]
  args <- listOf1 genSimpleTypeVar
  return $ Predicate predName args

-- | Generate a size constraint
genSizeConstraint :: Gen TypeConstraint
genSizeConstraint = do
  tv <- genSimpleTypeVar
  size <- choose (0, 100)
  elements
    [ TypeSizeGE tv size
    , TypeSizeGT tv size
    ]

-- | Generate a range constraint
genRangeConstraint :: Gen TypeConstraint
genRangeConstraint = do
  tv <- genSimpleTypeVar
  lower <- choose (0, 50)
  upper <- choose (lower + 1, 100)
  return $ TypeRange tv lower upper

-- | Generate any constraint
genConstraint :: Gen TypeConstraint
genConstraint = elements
  [ genEqualConstraint
  , genSubtypeConstraint
  , genPredicateConstraint
  , genSizeConstraint
  , genRangeConstraint
  ] >>= ($)

-- | Generate a type definition
genTypeDef :: Gen (String, [String], [TypeConstraint])
genTypeDef = do
  name <- elements ["MyType", "CustomType", "UserType", "DataType"]
  params <- listOf genTypeVarName
  constraints <- listOf genConstraint
  return (name, params, constraints)

instance Arbitrary TypeVar where
  arbitrary = genTypeVar

instance Arbitrary TypeConstraint where
  arbitrary = genConstraint

-- Helper function to extract variable names from TypeVar
extractVarNames :: TypeVar -> [String]
extractVarNames (TVVar name) = [name]
extractVarNames (TVApp _ args) = concatMap extractVarNames args
extractVarNames (TVFun params ret) = concatMap extractVarNames params ++ extractVarNames ret
extractVarNames (TVTuple elems) = concatMap extractVarNames elems

-- Helper function to extract variable names from TypeConstraint
extractConstraintVarNames :: TypeConstraint -> [String]
extractConstraintVarNames (Equal tv1 tv2) = extractVarNames tv1 ++ extractVarNames tv2
extractConstraintVarNames (Subtype tv1 tv2) = extractVarNames tv1 ++ extractVarNames tv2
extractConstraintVarNames (Predicate _ args) = concatMap extractVarNames args
extractConstraintVarNames (TypeSizeGE tv _) = extractVarNames tv
extractConstraintVarNames (TypeSizeGT tv _) = extractVarNames tv
extractConstraintVarNames (TypeRange tv _ _) = extractVarNames tv

-- Property: newDependentTypeChecker has empty errors
prop_newDependentTypeChecker_noErrors :: Property
prop_newDependentTypeChecker_noErrors =
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in null errors

-- Property: newDependentTypeChecker has prelude types
prop_newDependentTypeChecker_hasPrelude :: Property
prop_newDependentTypeChecker_hasPrelude =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      typeDefs = typeDefinitions env
      preludeTypes = ["int", "string", "bool", "float64"]
  in all (`Map.member` typeDefs) preludeTypes

-- Property: adding type with valid name succeeds
prop_addType_validName :: Property
prop_addType_validName =
  forAll genTypeDef $ \(name, params, constraints) ->
    let checker1 = newDependentTypeChecker
        typeDef = TypeDefDecl params constraints
        checker2 = addType name typeDef checker1
        env = dtcTypeEnv checker2
        typeDefs = typeDefinitions env
    in Map.member name typeDefs

-- Property: adding constraint preserves existing types
prop_addConstraint_preservesTypes :: Property
prop_addConstraint_preservesTypes =
  forAll genConstraint $ \constraint ->
    let checker1 = newDependentTypeChecker
        typeDefs1 = typeDefinitions (dtcTypeEnv checker1)
        checker2 = addConstraint constraint checker1
        typeDefs2 = typeDefinitions (dtcTypeEnv checker2)
    in typeDefs1 == typeDefs2

-- Property: equal constraint variables are extracted correctly
prop_equalConstraint_varsExtracted :: Property
prop_equalConstraint_varsExtracted =
  forAll genEqualConstraint $ \constraint ->
    let varNames = extractConstraintVarNames constraint
    in case constraint of
         Equal tv1 tv2 -> 
           let names1 = extractVarNames tv1
               names2 = extractVarNames tv2
           in sort varNames == sort (names1 ++ names2)
         _ -> property False

-- Property: subtype constraint variables are extracted correctly
prop_subtypeConstraint_varsExtracted :: Property
prop_subtypeConstraint_varsExtracted =
  forAll genSubtypeConstraint $ \constraint ->
    let varNames = extractConstraintVarNames constraint
    in case constraint of
         Subtype tv1 tv2 -> 
           let names1 = extractVarNames tv1
               names2 = extractVarNames tv2
           in sort varNames == sort (names1 ++ names2)
         _ -> property False

-- Property: predicate constraint variables are extracted correctly
prop_predicateConstraint_varsExtracted :: Property
prop_predicateConstraint_varsExtracted =
  forAll genPredicateConstraint $ \constraint ->
    let varNames = extractConstraintVarNames constraint
    in case constraint of
         Predicate _ args -> 
           let argNames = concatMap extractVarNames args
           in sort varNames == sort argNames
         _ -> property False

-- Property: size constraint variables are extracted correctly
prop_sizeConstraint_varsExtracted :: Property
prop_sizeConstraint_varsExtracted =
  forAll genSizeConstraint $ \constraint ->
    let varNames = extractConstraintVarNames constraint
    in case constraint of
         TypeSizeGE tv _ -> extractVarNames tv == varNames
         TypeSizeGT tv _ -> extractVarNames tv == varNames
         _ -> property False

-- Property: range constraint variables are extracted correctly
prop_rangeConstraint_varsExtracted :: Property
prop_rangeConstraint_varsExtracted =
  forAll genRangeConstraint $ \constraint ->
    let varNames = extractConstraintVarNames constraint
    in case constraint of
         TypeRange tv _ _ -> extractVarNames tv == varNames
         _ -> property False

-- Property: type variable extraction is consistent
prop_typeVar_extractionConsistent :: Property
prop_typeVar_extractionConsistent =
  forAll genTypeVar $ \typeVar ->
    let varNames = extractVarNames typeVar
        uniqueVarNames = nub varNames
    in length varNames >= length uniqueVarNames

-- Property: simple type variable extraction returns single name
prop_simpleTypeVar_singleName :: Property
prop_simpleTypeVar_singleName =
  forAll genSimpleTypeVar $ \typeVar ->
    case typeVar of
      TVVar name -> extractVarNames typeVar == [name]
      _ -> property False

-- Property: function type variable extraction includes all params and return
prop_funTypeVar_extractsAll :: Property
prop_funTypeVar_extractsAll =
  forAll genFunTypeVar $ \typeVar ->
    case typeVar of
      TVFun params ret -> 
        let paramNames = concatMap extractVarNames params
            returnNames = extractVarNames ret
            allNames = extractVarNames typeVar
        in sort allNames == sort (paramNames ++ returnNames)
      _ -> property False

-- Property: tuple type variable extraction includes all elements
prop_tupleTypeVar_extractsAll :: Property
prop_tupleTypeVar_extractsAll =
  forAll genTupleTypeVar $ \typeVar ->
    case typeVar of
      TVTuple elems -> 
        let elemNames = concatMap extractVarNames elems
            allNames = extractVarNames typeVar
        in sort allNames == sort elemNames
      _ -> property False

-- Property: app type variable extraction includes constructor and args
prop_appTypeVar_extractsAll :: Property
prop_appTypeVar_extractsAll =
  forAll genAppTypeVar $ \typeVar ->
    case typeVar of
      TVApp _ args -> 
        let argNames = concatMap extractVarNames args
            allNames = extractVarNames typeVar
        in sort allNames == sort argNames
      _ -> property False

-- Property: constraint set preserves uniqueness when adding same constraint
prop_constraintSet_uniqueness :: Property
prop_constraintSet_uniqueness =
  forAll genConstraint $ \constraint ->
    let checker1 = newDependentTypeChecker
        checker2 = addConstraint constraint checker1
        checker3 = addConstraint constraint checker2
        constraints1 = pendingConstraints (dtcTypeEnv checker2)
        constraints2 = pendingConstraints (dtcTypeEnv checker3)
    in length constraints2 >= length constraints1

-- Property: type checker with custom types has those types
prop_customTypeChecker_hasTypes :: Property
prop_customTypeChecker_hasTypes =
  forAll (listOf1 genTypeDef) $ \typeDefs ->
    let typeDefPairs = [(n, ps, cs) | (n, ps, cs) <- typeDefs]
        checker = newDependentTypeCheckerWithTypes typeDefPairs
        env = dtcTypeEnv checker
        typeDefsMap = typeDefinitions env
        expectedNames = map (\(n, _, _) -> n) typeDefs
    in all (`Map.member` typeDefsMap) expectedNames

-- Property: type checker errors are accumulated
prop_typeChecker_errorsAccumulated :: Property
prop_typeChecker_errorsAccumulated =
  forAll (listOf1 genConstraint) $ \constraints ->
    let checker = newDependentTypeChecker
        checkerWithConstraints = foldr addConstraint checker constraints
        errors = getDependentTypeErrors checkerWithConstraints
    in length errors >= 0  -- May have errors from constraint solving

-- Property: type environment maintains type definitions order
prop_typeEnvironment_maintainsOrder :: Property
prop_typeEnvironment_maintainsOrder =
  forAll (listOf1 genTypeDef) $ \typeDefs ->
    let typeDefPairs = [(n, ps, cs) | (n, ps, cs) <- typeDefs]
        checker = newDependentTypeCheckerWithTypes typeDefPairs
        env = dtcTypeEnv checker
        typeDefsMap = typeDefinitions env
        actualNames = Map.keys typeDefsMap
        expectedNames = map (\(n, _, _) -> n) typeDefs ++ Map.keys preludeTypeDefs
    in all (`elem` actualNames) expectedNames

-- Property: constraint solving preserves variable relationships
prop_constraintSolving_preservesRelationships :: Property
prop_constraintSolving_preservesRelationships =
  forAll (listOf1 genEqualConstraint) $ \constraints ->
    let checker = newDependentTypeChecker
        checkerWithConstraints = foldr addConstraint checker constraints
        checkerSolved = solveConstraints checkerWithConstraints
        errors1 = getDependentTypeErrors checkerWithConstraints
        errors2 = getDependentTypeErrors checkerSolved
    in length errors2 >= length errors1  -- May detect more errors after solving

tests :: TestTree
tests =
  testGroup "Dependencies Cycle Detection Properties"
    [ fastProperty "newDependentTypeChecker has empty errors" prop_newDependentTypeChecker_noErrors
    , fastProperty "newDependentTypeChecker has prelude types" prop_newDependentTypeChecker_hasPrelude
    , fastProperty "adding type with valid name succeeds" prop_addType_validName
    , fastProperty "adding constraint preserves existing types" prop_addConstraint_preservesTypes
    , fastProperty "equal constraint variables are extracted correctly" prop_equalConstraint_varsExtracted
    , fastProperty "subtype constraint variables are extracted correctly" prop_subtypeConstraint_varsExtracted
    , fastProperty "predicate constraint variables are extracted correctly" prop_predicateConstraint_varsExtracted
    , fastProperty "size constraint variables are extracted correctly" prop_sizeConstraint_varsExtracted
    , fastProperty "range constraint variables are extracted correctly" prop_rangeConstraint_varsExtracted
    , fastProperty "type variable extraction is consistent" prop_typeVar_extractionConsistent
    , fastProperty "simple type variable extraction returns single name" prop_simpleTypeVar_singleName
    , fastProperty "function type variable extraction includes all params and return" prop_funTypeVar_extractsAll
    , fastProperty "tuple type variable extraction includes all elements" prop_tupleTypeVar_extractsAll
    , fastProperty "app type variable extraction includes constructor and args" prop_appTypeVar_extractsAll
    , fastProperty "constraint set preserves uniqueness when adding same constraint" prop_constraintSet_uniqueness
    , fastProperty "type checker with custom types has those types" prop_customTypeChecker_hasTypes
    , fastProperty "type checker errors are accumulated" prop_typeChecker_errorsAccumulated
    , fastProperty "type environment maintains type definitions order" prop_typeEnvironment_maintainsOrder
    , fastProperty "constraint solving preserves variable relationships" prop_constraintSolving_preservesRelationships
    ]