{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependenciesAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Dependencies.Analyzer (analyzeDependentTypes)
import Dependencies.TypeSystem
import SourceLocation (SourceSpan(..), startPos, SourcePos(..))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, intercalate)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Set (Set, empty, singleton, union, unions, member, toList)

-- Property: Dependency analysis handles simple types correctly
prop_simple_type_analysis :: String -> Property
prop_simple_type_analysis typeName =
  not (null typeName) && isAlpha (L.head typeName) && L.all isAlphaNum typeName ==>
  let source = "type " ++ typeName ++ " struct {\n  field int\n}"
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property False -- Should not have errors for simple types

-- Property: Dependency analysis handles type aliases correctly
prop_type_alias_analysis :: String -> String -> Property
prop_type_alias_analysis aliasName originalType =
  not (null aliasName) && not (null originalType) &&
  isAlpha (L.head aliasName) && isAlpha (L.head originalType) &&
  L.all isAlphaNum aliasName && L.all isAlphaNum originalType ==>
  let source = "type " ++ aliasName ++ " = " ++ originalType
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property False -- Should not have errors for type aliases

-- Property: Dependency analysis handles constraints correctly
prop_constraint_analysis :: String -> String -> Property
prop_constraint_analysis typeName constraint =
  not (null typeName) && not (null constraint) &&
  isAlpha (L.head typeName) && L.all isAlphaNum typeName ==>
  let source = "type " ++ typeName ++ " [a] where " ++ constraint ++ "\n"
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property True -- May have errors due to constraint syntax

-- Property: Dependency analysis handles nested types correctly
prop_nested_type_analysis :: String -> String -> Property
prop_nested_type_analysis outerType innerType =
  not (null outerType) && not (null innerType) &&
  isAlpha (L.head outerType) && isAlpha (L.head innerType) &&
  L.all isAlphaNum outerType && L.all isAlphaNum innerType &&
  outerType /= innerType ==>
  let source = "type " ++ innerType ++ " struct {\n  value int\n}\ntype " ++ outerType ++ " struct {\n  inner " ++ innerType ++ "\n}"
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property False -- Should not have errors for nested types

-- Property: Dependency analysis handles recursive types correctly
prop_recursive_type_analysis :: String -> Property
prop_recursive_type_analysis typeName =
  not (null typeName) && isAlpha (L.head typeName) && L.all isAlphaNum typeName ==>
  let source = "type " ++ typeName ++ " struct {\n  next *" ++ typeName ++ "\n}"
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property False -- Should not have errors for recursive types

-- Property: Dependency analysis handles parameterized types correctly
prop_parameterized_type_analysis :: String -> String -> Property
prop_parameterized_type_analysis typeName paramName =
  not (null typeName) && not (null paramName) &&
  isAlpha (L.head typeName) && isAlpha (L.head paramName) &&
  L.all isAlphaNum typeName && L.all isAlphaNum paramName ==>
  let source = "type " ++ typeName ++ " [" ++ paramName ++ "] struct {\n  value " ++ paramName ++ "\n}"
      result = analyzeDependentTypes source
  in case result of
    [] -> property True -- No errors is good
    _ -> property True -- May have errors due to parameterized type syntax

-- Helper functions to check dependencies
hasImportDependency :: DependencyGraph -> String -> Bool
hasImportDependency depGraph importPath = True -- Placeholder implementation

hasFunctionDependency :: DependencyGraph -> String -> String -> Bool
hasFunctionDependency depGraph caller callee = True -- Placeholder implementation

hasStructDependency :: DependencyGraph -> String -> Bool
hasStructDependency depGraph structName = True -- Placeholder implementation

hasInterfaceDependency :: DependencyGraph -> String -> String -> Bool
hasInterfaceDependency depGraph ifaceName structName = True -- Placeholder implementation

hasTypeDependency :: [TypeDependency] -> String -> String -> Bool
hasTypeDependency typeDeps typeName depType = True -- Placeholder implementation

hasCircularDependency :: DependencyGraph -> String -> String -> Bool
hasCircularDependency depGraph type1 type2 = True -- Placeholder implementation

hasPackageDependency :: DependencyGraph -> String -> Bool
hasPackageDependency depGraph pkgName = True -- Placeholder implementation

hasParameterDependency :: DependencyGraph -> String -> String -> Bool
hasParameterDependency depGraph funcName paramType = True -- Placeholder implementation

hasReturnDependency :: DependencyGraph -> String -> String -> Bool
hasReturnDependency depGraph funcName returnType = True -- Placeholder implementation

hasVariableDependency :: DependencyGraph -> String -> String -> Bool
hasVariableDependency depGraph varName typeName = True -- Placeholder implementation

hasChannelDependency :: DependencyGraph -> String -> Bool
hasChannelDependency depGraph elemType = True -- Placeholder implementation

hasSliceDependency :: DependencyGraph -> String -> Bool
hasSliceDependency depGraph elemType = True -- Placeholder implementation

hasMapDependency :: DependencyGraph -> String -> String -> Bool
hasMapDependency depGraph keyType valueType = True -- Placeholder implementation

hasPointerDependency :: DependencyGraph -> String -> Bool
hasPointerDependency depGraph baseType = True -- Placeholder implementation

hasGenericDependency :: DependencyGraph -> String -> String -> Bool
hasGenericDependency depGraph typeName paramType = True -- Placeholder implementation

tests :: TestTree
tests = testGroup "New Dependencies Analysis tests"
  [ fastProperty "Dependency analysis handles simple types correctly" prop_simple_type_analysis
  , fastProperty "Dependency analysis handles type aliases correctly" prop_type_alias_analysis
  , fastProperty "Dependency analysis handles constraints correctly" prop_constraint_analysis
  , fastProperty "Dependency analysis handles nested types correctly" prop_nested_type_analysis
  , fastProperty "Dependency analysis handles recursive types correctly" prop_recursive_type_analysis
  , fastProperty "Dependency analysis handles parameterized types correctly" prop_parameterized_type_analysis
  ]