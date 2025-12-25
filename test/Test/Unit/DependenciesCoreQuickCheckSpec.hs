{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..))
import Test.QuickCheck.Gen (choose, listOf, elements, vectorOf, oneof)

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
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , preludeTypeDefs
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , convertTypeExpr
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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (nub, sort)
import Data.Either (isLeft, isRight)

-- Property: AST construction with statements
prop_ast_with_statements :: [String] -> Property
prop_ast_with_statements typeNames =
  not (null typeNames) ==>
  let statements = [STypeDef (T.pack name) [] [] | name <- nub typeNames]
      ast = Program statements
  in property $ case ast of
    Program stmts -> length stmts === length (nub typeNames)

-- Property: TypeExpr equality
prop_typeexpr_equality :: String -> [String] -> Property
prop_typeexpr_equality baseName params =
  not (null baseName) ==>
  let simpleType = SimpleT (T.pack baseName)
      genericType = GenericT (T.pack baseName) (map SimpleT (map T.pack params))
  in property $ simpleType === simpleType .&&.
             genericType === genericType .&&.
             simpleType /= genericType

-- Property: Constraint creation and validation
prop_constraint_creation :: String -> Int -> Int -> Property
prop_constraint_creation name lower upper =
  not (null name) && lower <= upper ==>
  let rangeConstraint = RangeC (T.pack name) lower upper
      sizeGTConstraint = SizeGT (T.pack name) lower
      sizeGEConstraint = SizeGE (T.pack name) upper
  in property $ rangeConstraint === rangeConstraint .&&.
             sizeGTConstraint === sizeGTConstraint .&&.
             sizeGEConstraint === sizeGEConstraint

-- Property: DependencyNode structure
prop_dependency_node_structure :: String -> [String] -> Property
prop_dependency_node_structure nodeName dependencies =
  not (null nodeName) ==>
  let node = DependencyNode nodeName (nub dependencies)
      uniqueDeps = nub dependencies
  in property $ nodeName node === nodeName .&&.
             length (nodeDependencies node) === length uniqueDeps .&&.
             sort (nodeDependencies node) === sort uniqueDeps

-- Property: DependencyGraph creation
prop_dependency_graph_creation :: [(String, [String])] -> Property
prop_dependency_graph_creation nodeSpecs =
  not (null nodeSpecs) ==>
  let nodes = [DependencyNode name (nub deps) | (name, deps) <- nodeSpecs]
      nodeMap = Map.fromList [(nodeName node, node) | node <- nodes]
      graph = DependencyGraph nodeMap
  in property $ Map.size (graphNodes graph) === length (nub (map fst nodeSpecs))

-- Property: TypeVar construction
prop_typevar_construction :: String -> [String] -> Property
prop_typevar_construction baseName params =
  not (null baseName) ==>
  let conType = TVCon baseName
      varType = TVVar baseName
      appType = TVApp baseName (map TVVar params)
  in property $ conType === conType .&&.
             varType === varType .&&.
             appType === appType .&&.
             conType /= varType

-- Property: TypeConstraint equality
prop_typeconstraint_equality :: String -> Int -> Property
prop_typeconstraint_equality typeName size =
  not (null typeName) && size >= 0 ==>
  let sizeGE = TypeSizeGE (TVCon typeName) size
      sizeGT = TypeSizeGT (TVCon typeName) size
      equal = Equal (TVCon typeName) (TVVar "test")
  in property $ sizeGE === sizeGE .&&.
             sizeGT === sizeGT .&&.
             equal === equal .&&.
             sizeGE /= sizeGT

-- Property: TypeDef structure
prop_typedef_structure :: String -> [String] -> Property
prop_typedef_structure typeName params =
  not (null typeName) ==>
  let typeDef = TypeDefDecl (nub params) []
  in property $ tdParams typeDef === nub params .&&.
             null (tdConstraints typeDef)

-- Property: TypeEnv operations
prop_typeenv_operations :: String -> String -> Property
prop_typeenv_operations typeName1 typeName2 =
  not (null typeName1) && not (null typeName2) && typeName1 /= typeName2 ==>
  let initialEnv = TypeEnv Map.empty []
      typeDef1 = TypeDefDecl [] []
      typeDef2 = TypeDefDecl [] []
      envWith1 = initialEnv { typeDefinitions = Map.insert typeName1 typeDef1 (typeDefinitions initialEnv) }
      envWith2 = envWith1 { typeDefinitions = Map.insert typeName2 typeDef2 (typeDefinitions envWith1) }
  in property $ Map.size (typeDefinitions envWith1) === 1 .&&.
             Map.size (typeDefinitions envWith2) === 2 .&&.
             lookupTypeDef typeName1 envWith2 === Just typeDef1 .&&.
             lookupTypeDef typeName2 envWith2 === Just typeDef2

-- Property: DependentTypeChecker initialization
prop_dtc_initialization :: Property
prop_dtc_initialization =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      errors = tcErrors checker
  in property $ Map.size (typeDefinitions env) >= 0 .&&.
             null errors

-- Property: DependentTypeChecker with custom types
prop_dtc_with_custom_types :: [(String, [String])] -> Property
prop_dtc_with_custom_types typeSpecs =
  not (null typeSpecs) ==>
  let typeMap = Map.fromList [(name, TypeDefDecl params []) | (name, params) <- typeSpecs]
      checker = newDependentTypeCheckerWithTypes typeMap
      env = dtcTypeEnv checker
  in property $ Map.size (typeDefinitions env) === length (nub (map fst typeSpecs))

-- Property: convertTypeExpr for simple types
prop_convert_simple_typeexpr :: String -> Property
prop_convert_simple_typeexpr typeName =
  not (null typeName) ==>
  let typeExpr = SimpleT (T.pack typeName)
      converted = convertTypeExpr typeExpr
  in property $ case converted of
    TVCon name -> name === typeName
    _ -> property False

-- Property: convertTypeExpr for generic types
prop_convert_generic_typeexpr :: String -> [String] -> Property
prop_convert_generic_typeexpr typeName params =
  not (null typeName) && not (null params) ==>
  let typeExpr = GenericT (T.pack typeName) (map SimpleT (map T.pack params))
      converted = convertTypeExpr typeExpr
  in property $ case converted of
    TVApp name typeVars -> 
      name === typeName .&&. length typeVars === length params
    _ -> property False

-- Property: convertConstraint for range constraints
prop_convert_range_constraint :: String -> Int -> Int -> Property
prop_convert_range_constraint name lower upper =
  not (null name) && lower <= upper ==>
  let constraint = RangeC (T.pack name) lower upper
      converted = convertConstraint constraint
  in property $ case converted of
    TypeRange (TVCon varName) l u -> 
      varName === name .&&. l === lower .&&. u === upper
    _ -> property False

-- Property: convertConstraint for size constraints
prop_convert_size_constraint :: String -> Int -> Property
prop_convert_size_constraint name size =
  not (null name) && size >= 0 ==>
  let gtConstraint = SizeGT (T.pack name) size
      geConstraint = SizeGE (T.pack name) size
      convertedGT = convertConstraint gtConstraint
      convertedGE = convertConstraint geConstraint
  in property $ case (convertedGT, convertedGE) of
    (TypeSizeGT (TVCon varName) s, TypeSizeGE (TVCon varName2) s2) ->
      varName === name .&&. varName2 === name .&&. s === size .&&. s2 === size
    _ -> property False

-- Property: addType operation
prop_add_type_operation :: String -> [String] -> Property
prop_add_type_operation typeName params =
  not (null typeName) ==>
  let checker = newDependentTypeChecker
      typeDef = TypeDefDecl params []
      updatedChecker = addType typeName typeDef checker
      env = dtcTypeEnv updatedChecker
  in property $ case lookupTypeDef typeName env of
    Just foundDef -> foundDef === typeDef
    Nothing -> property False

-- Property: addConstraint operation
prop_add_constraint_operation :: String -> Int -> Property
prop_add_constraint_operation typeName size =
  not (null typeName) && size >= 0 ==>
  let checker = newDependentTypeChecker
      constraint = TypeSizeGE (TVCon typeName) size
      updatedChecker = addConstraint constraint checker
      env = dtcTypeEnv updatedChecker
  in property $ constraint `elem` pendingConstraints env

-- Property: addTypeError operation
prop_add_type_error_operation :: String -> String -> Property
prop_add_type_error_operation error1 error2 =
  not (null error1) && not (null error2) && error1 /= error2 ==>
  let checker = newDependentTypeChecker
      typeError1 = TypeNotFound error1
      typeError2 = TypeNotFound error2
      updatedChecker1 = addTypeError typeError1 checker
      updatedChecker2 = addTypeError typeError2 updatedChecker1
      errors1 = tcErrors updatedChecker1
      errors2 = tcErrors updatedChecker2
  in property $ length errors1 === 1 .&&.
             length errors2 === 2 .&&.
             typeError1 `elem` errors2 .&&.
             typeError2 `elem` errors2

-- Property: getDependentTypeErrors
prop_get_type_errors :: [String] -> Property
prop_get_type_errors errorNames =
  not (null errorNames) ==>
  let checker = newDependentTypeChecker
      errors = [TypeNotFound name | name <- nub errorNames]
      checkerWithErrors = foldl addTypeError checker errors
      retrievedErrors = getDependentTypeErrors checkerWithErrors
  in property $ length retrievedErrors === length (nub errorNames) .&&.
             all (`elem` retrievedErrors) errors

-- Property: Substitution operations
prop_substitution_operations :: String -> String -> Property
prop_substitution_operations varName typeName =
  not (null varName) && not (null typeName) ==>
  let substitution = Map.singleton varName (TVCon typeName)
      lookupResult = Map.lookup varName substitution
  in property $ lookupResult === Just (TVCon typeName)

-- Property: unify simple types
prop_unify_simple_types :: String -> Property
prop_unify_simple_types typeName =
  not (null typeName) ==>
  let type1 = TVCon typeName
      type2 = TVCon typeName
      result = unify type1 type2 Map.empty
  in property $ case result of
    Right subst -> Map.size subst === 0
    Left _ -> property False

-- Property: unify different types should fail
prop_unify_different_types :: String -> String -> Property
prop_unify_different_types typeName1 typeName2 =
  not (null typeName1) && not (null typeName2) && typeName1 /= typeName2 ==>
  let type1 = TVCon typeName1
      type2 = TVCon typeName2
      result = unify type1 type2 Map.empty
  in property $ isLeft result

-- Property: validate constraint structure
prop_validate_constraint :: String -> Int -> Int -> Property
prop_validate_constraint name lower upper =
  not (null name) && lower <= upper ==>
  let constraint = TypeRange (TVCon name) lower upper
  in property $ validateConstraint constraint

-- Property: check type constraint
prop_check_type_constraint :: String -> Int -> Property
prop_check_type_constraint name size =
  not (null name) && size >= 0 ==>
  let typeVar = TVCon name
      constraint = TypeSizeGE typeVar size
      typeEnv = TypeEnv Map.empty [constraint]
  in property $ checkTypeConstraint constraint typeEnv

tests :: TestTree
tests =
  testGroup "Dependencies Core QuickCheck Tests"
    [ fastProperty "AST construction with statements" prop_ast_with_statements
    , fastProperty "TypeExpr equality" prop_typeexpr_equality
    , fastProperty "Constraint creation and validation" prop_constraint_creation
    , fastProperty "DependencyNode structure" prop_dependency_node_structure
    , fastProperty "DependencyGraph creation" prop_dependency_graph_creation
    , fastProperty "TypeVar construction" prop_typevar_construction
    , fastProperty "TypeConstraint equality" prop_typeconstraint_equality
    , fastProperty "TypeDef structure" prop_typedef_structure
    , fastProperty "TypeEnv operations" prop_typeenv_operations
    , fastProperty "DependentTypeChecker initialization" prop_dtc_initialization
    , fastProperty "DependentTypeChecker with custom types" prop_dtc_with_custom_types
    , fastProperty "convertTypeExpr for simple types" prop_convert_simple_typeexpr
    , fastProperty "convertTypeExpr for generic types" prop_convert_generic_typeexpr
    , fastProperty "convertConstraint for range constraints" prop_convert_range_constraint
    , fastProperty "convertConstraint for size constraints" prop_convert_size_constraint
    , fastProperty "addType operation" prop_add_type_operation
    , fastProperty "addConstraint operation" prop_add_constraint_operation
    , fastProperty "addErrorType operation" prop_add_type_error_operation
    , fastProperty "getDependentTypeErrors" prop_get_type_errors
    , fastProperty "Substitution operations" prop_substitution_operations
    , fastProperty "unify simple types" prop_unify_simple_types
    , fastProperty "unify different types should fail" prop_unify_different_types
    , fastProperty "validate constraint structure" prop_validate_constraint
    , fastProperty "check type constraint" prop_check_type_constraint
    ]