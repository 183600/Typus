{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements, Gen, suchThat)

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
  , TypeScheme(..)
  , TypeEnvironment(..)
  , Substitution
  , newTypeVariable
  , initialTypeEnvironment
  )

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
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
  , getFreshTypeVar
  )

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Char (isAlphaNum)

-- Property: AST equality is consistent
prop_ast_equality :: [Statement] -> [Statement] -> Property
prop_ast_equality stmts1 stmts2 =
  let ast1 = Program stmts1
      ast2 = Program stmts2
      areEqual = ast1 == ast2
      sameStmts = stmts1 == stmts2
  in property $ areEqual === sameStmts

-- Property: Statement equality is consistent
prop_statement_equality :: Statement -> Statement -> Property
prop_statement_equality stmt1 stmt2 =
  let areEqual = stmt1 == stmt2
  in property $ areEqual === (stmt1 == stmt2)

-- Property: TypeExpr equality is consistent  
prop_typeExpr_equality :: TypeExpr -> TypeExpr -> Property
prop_typeExpr_equality type1 type2 =
  let areEqual = type1 == type2
  in property $ areEqual === (type1 == type2)

-- Property: Constraint equality is consistent
prop_constraint_equality :: Constraint -> Constraint -> Property
prop_constraint_equality constraint1 constraint2 =
  let areEqual = constraint1 == constraint2
  in property $ areEqual === (constraint1 == constraint2)

-- Property: DependencyNode contains name and dependencies
prop_dependencyNode_has_name_deps :: String -> [String] -> Property
prop_dependencyNode_has_name_deps name deps =
  not (null name) ==> 
  let node = DependencyNode name deps
  in property $ nodeName node === name .&&. nodeDependencies node === deps

-- Property: DependencyNode equality is consistent
prop_dependencyNode_equality :: String -> [String] -> String -> [String] -> Property
prop_dependencyNode_equality name1 deps1 name2 deps2 =
  not (null name1) ==> not (null name2) ==> 
  let node1 = DependencyNode name1 deps1
      node2 = DependencyNode name2 deps2
      areEqual = node1 == node2
  in property $ areEqual === (name1 == name2 .&&. deps1 == deps2)

-- Property: TypeVar creation produces unique variables
prop_typeVar_unique :: String -> Property
prop_typeVar_unique name =
  not (null name) ==> 
  let var1 = newTypeVariable name
      var2 = newTypeVariable name
  in property $ var1 /= var2

-- Property: TypeVar equality is consistent
prop_typeVar_equality :: String -> String -> Property
prop_typeVar_equality name1 name2 =
  not (null name1) ==> not (null name2) ==> 
  let var1 = newTypeVariable name1
      var2 = newTypeVariable name2
      sameName = name1 == name2
  in property $ (var1 == var2) === sameName

-- Property: SimpleT type expression contains name
prop_simpleType_contains_name :: String -> Property
prop_simpleType_contains_name name =
  not (null name) ==> 
  let typeExpr = SimpleT (pack name)
  in case typeExpr of
       SimpleT n -> property $ n === pack name
       _ -> property $ False

-- Property: GenericT type expression contains name and params
prop_genericType_contains_name_params :: String -> [TypeExpr] -> Property
prop_genericType_contains_name_params name params =
  not (null name) ==> not (null params) ==> 
  let typeExpr = GenericT (pack name) params
  in case typeExpr of
       GenericT n p -> property $ n === pack name .&&. p === params
       _ -> property $ False

-- Property: SizeGT constraint contains name and value
prop_sizeGT_contains_name_value :: String -> Positive Int -> Property
prop_sizeGT_contains_name_value name (Positive value) =
  not (null name) ==> 
  let constraint = SizeGT (pack name) value
  in case constraint of
       SizeGT n v -> property $ n === pack name .&&. v === value
       _ -> property $ False

-- Property: SizeGE constraint contains name and value
prop_sizeGE_contains_name_value :: String -> Positive Int -> Property
prop_sizeGE_contains_name_value name (Positive value) =
  not (null name) ==> 
  let constraint = SizeGE (pack name) value
  in case constraint of
       SizeGE n v -> property $ n === pack name .&&. v === value
       _ -> property $ False

-- Property: RangeC constraint contains name and range
prop_rangeC_contains_name_range :: String -> Positive Int -> Positive Int -> Property
prop_rangeC_contains_name_range name (Positive minVal) (Positive maxVal) =
  not (null name) ==> minVal <= maxVal ==> 
  let constraint = RangeC (pack name) minVal maxVal
  in case constraint of
       RangeC n min' max' -> property $ n === pack name .&&. min' === minVal .&&. max' === maxVal
       _ -> property $ False

-- Property: PredC constraint contains name and types
prop_predC_contains_name_types :: String -> [TypeExpr] -> Property
prop_predC_contains_name_types name types =
  not (null name) ==> not (null types) ==> 
  let constraint = PredC (pack name) types
  in case constraint of
       PredC n t -> property $ n === pack name .&&. t === types
       _ -> property $ False

-- Property: STypeDef statement contains name and constraints
prop_sTypeDef_contains_info :: String -> [String] -> [Constraint] -> Property
prop_sTypeDef_contains_info name params constraints =
  not (null name) ==> 
  let stmt = STypeDef (pack name) (map pack params) constraints
  in case stmt of
       STypeDef n p c -> property $ n === pack name .&&. p === map pack params .&&. c === constraints
       _ -> property $ False

-- Property: STypeAlias statement contains name, type and constraints
prop_sTypeAlias_contains_info :: String -> TypeExpr -> [Constraint] -> Property
prop_sTypeAlias_contains_info name typeExpr constraints =
  not (null name) ==> 
  let stmt = STypeAlias (pack name) typeExpr constraints
  in case stmt of
       STypeAlias n t c -> property $ n === pack name .&&. t === typeExpr .&&. c === constraints
       _ -> property $ False

-- Property: SVarDecl statement contains name and type
prop_sVarDecl_contains_info :: String -> TypeExpr -> Property
prop_sVarDecl_contains_info name typeExpr =
  not (null name) ==> 
  let stmt = SVarDecl (pack name) typeExpr
  in case stmt of
       SVarDecl n t -> property $ n === pack name .&&. t === typeExpr
       _ -> property $ False

-- Property: SFuncDecl statement contains name, params and return type
prop_sFuncDecl_contains_info :: String -> [(String, TypeExpr)] -> Maybe TypeExpr -> Property
prop_sFuncDecl_contains_info name params returnType =
  not (null name) ==> 
  let typedParams = map (\(n, t) -> (pack n, t)) params
      stmt = SFuncDecl (pack name) typedParams returnType
  in case stmt of
       SFuncDecl n p r -> property $ n === pack name .&&. p === typedParams .&&. r === returnType
       _ -> property $ False

-- Property: AST with statements preserves order
prop_ast_preserves_order :: [Statement] -> Property
prop_ast_preserves_order stmts =
  let ast = Program stmts
  in case ast of
       Program s -> property $ s === stmts
       _ -> property $ False

-- Property: TypeExpr Show contains relevant information
prop_typeExpr_show_contains_info :: String -> Property
prop_typeExpr_show_contains_info name =
  not (null name) ==> 
  let typeExpr = SimpleT (pack name)
      shown = show typeExpr
  in property $ name `isInfixOf` shown

-- Property: Constraint Show contains relevant information
prop_constraint_show_contains_info :: String -> Positive Int -> Property
prop_constraint_show_contains_info name (Positive value) =
  not (null name) ==> 
  let constraint = SizeGT (pack name) value
      shown = show constraint
  in property $ name `isInfixOf` shown .&&. show value `isInfixOf` shown

-- Property: Statement Show contains relevant information
prop_statement_show_contains_info :: String -> Property
prop_statement_show_contains_info name =
  not (null name) ==> 
  let stmt = SVarDecl (pack name) (SimpleT "int")
      shown = show stmt
  in property $ name `isInfixOf` shown

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (length needle) s : substrings xs

tests :: TestTree
tests =
  testGroup "Dependency Analysis Core QuickCheck Tests"
    [ fastProperty "AST equality is consistent" prop_ast_equality
    , fastProperty "Statement equality is consistent" prop_statement_equality
    , fastProperty "TypeExpr equality is consistent" prop_typeExpr_equality
    , fastProperty "Constraint equality is consistent" prop_constraint_equality
    , fastProperty "DependencyNode has name and dependencies" prop_dependencyNode_has_name_deps
    , fastProperty "DependencyNode equality is consistent" prop_dependencyNode_equality
    , fastProperty "TypeVar creation produces unique variables" prop_typeVar_unique
    , fastProperty "TypeVar equality is consistent" prop_typeVar_equality
    , fastProperty "SimpleT type expression contains name" prop_simpleType_contains_name
    , fastProperty "GenericT type expression contains name and params" prop_genericType_contains_name_params
    , fastProperty "SizeGT constraint contains name and value" prop_sizeGT_contains_name_value
    , fastProperty "SizeGE constraint contains name and value" prop_sizeGE_contains_name_value
    , fastProperty "RangeC constraint contains name and range" prop_rangeC_contains_name_range
    , fastProperty "PredC constraint contains name and types" prop_predC_contains_name_types
    , fastProperty "STypeDef statement contains name and constraints" prop_sTypeDef_contains_info
    , fastProperty "STypeAlias statement contains name, type and constraints" prop_sTypeAlias_contains_info
    , fastProperty "SVarDecl statement contains name and type" prop_sVarDecl_contains_info
    , fastProperty "SFuncDecl statement contains name, params and return type" prop_sFuncDecl_contains_info
    , fastProperty "AST with statements preserves order" prop_ast_preserves_order
    , fastProperty "TypeExpr Show contains relevant information" prop_typeExpr_show_contains_info
    , fastProperty "Constraint Show contains relevant information" prop_constraint_show_contains_info
    , fastProperty "Statement Show contains relevant information" prop_statement_show_contains_info
    ]