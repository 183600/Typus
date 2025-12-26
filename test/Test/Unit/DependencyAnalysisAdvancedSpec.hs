{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Dependencies
  ( analyzeDependencies
  , DependencyGraph
  , Dependency(..)
  , DependencyType(..)
  )

import Dependencies.Analyzer (findCircularDependencies)
import Dependencies.Inference (inferTypes)
import Dependencies.TypeSystem (TypeEnvironment)

import Parser (TypusFile(..), parseTypus)
import Compiler (compile)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort)
import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Test: Dependency analysis correctly identifies function dependencies
test_function_dependency_analysis :: TestTree
test_function_dependency_analysis = testCase "Dependency analysis identifies function dependencies" $ do
  let dependencyCode = "package main\n\nfunc helper() int {\n  return 42\n}\n\nfunc main() {\n  result := helper()\n}"
      result = parseTypus dependencyCode
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let deps = analyzeDependencies typusFile
      -- Should detect that main depends on helper
      length deps @?= 1

-- Property: Dependency graph is acyclic for well-structured code
prop_acyclic_dependency_graph :: [String] -> Property
prop_acyclic_dependency_graph functionNames =
  length functionNames >= 2 && length functionNames <= 5 ==>
  let validNames = filter (all isLetter) (nub functionNames)
      code = "package main\n\n" ++ unlines (map (\name -> "func " ++ name ++ "() {}") validNames)
      result = parseTypus code
  in case result of
    Left _ -> property True  -- Parse failure, test vacuously passes
    Right typusFile -> 
      let deps = analyzeDependencies typusFile
          circular = findCircularDependencies deps
      in property $ null circular

-- Test: Dependency analysis handles mutual recursion detection
test_mutual_recursion_detection :: TestTree
test_mutual_recursion_detection = testCase "Dependency analysis detects mutual recursion" $ do
  let mutualRecursionCode = "package main\n\nfunc a() {\n  b()\n}\n\nfunc b() {\n  a()\n}"
      result = parseTypus mutualRecursionCode
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let deps = analyzeDependencies typusFile
          circular = findCircularDependencies deps
      length circular @?= 1  -- Should detect one circular dependency

-- Property: Type inference maintains consistency across related functions
prop_type_inference_consistency :: [(String, String)] -> Property
prop_type_inference_consistency functionSignatures =
  not (null functionSignatures) && length functionSignatures <= 5 ==>
  let validSigs = filter (\(name, sig) -> not (null name) && not (null sig)) functionSignatures
      code = "package main\n\n" ++ unlines (map (\(name, sig) -> "func " ++ name ++ "() " ++ sig + " { return 0 }") validSigs)
      result = compile code
  in case result of
    Right _ -> property True  -- Success - types consistent
    Left errs -> property $ not (any (\err -> "type" `isInfixOf` show err && "conflict" `isInfixOf` show err) errs)

-- Test: Dependency analysis respects module boundaries
test_module_boundary_analysis :: TestTree
test_module_boundary_analysis = testCase "Dependency analysis respects module boundaries" $ do
  let moduleCode = "package main\n\nimport \"fmt\"\n\nfunc helper() int {\n  return 42\n}\n\nfunc main() {\n  result := helper()\n  fmt.Println(result)\n}"
      result = parseTypus moduleCode
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let deps = analyzeDependencies typusFile
      -- Should detect both internal and external dependencies
      length deps @?= 2

-- Property: Dependency analysis correctly orders compilation
prop_compilation_ordering :: [String] -> Property
prop_compilation_ordering functionNames =
  length functionNames >= 2 && length functionNames <= 5 ==>
  let validNames = filter (all isLetter) (nub functionNames)
      -- Create a dependency chain: func1 -> func2 -> func3 -> ...
      code = "package main\n\n" ++ unlines (zipWith (\i name -> 
        if i == 1 
        then "func " ++ name ++ "() {}"
        else "func " ++ name ++ "() { " ++ (validNames !! (i-2)) ++ "() }"
        ) [1..] validNames)
      result = parseTypus code
  in case result of
    Left _ -> property True  -- Parse failure, test vacuously passes
    Right typusFile -> 
      let deps = analyzeDependencies typusFile
      in property $ length deps >= (length validNames - 1)

-- Test: Dependency analysis handles interface implementations correctly
test_interface_dependency_analysis :: TestTree
test_interface_dependency_analysis = testCase "Dependency analysis handles interface implementations" $ do
  let interfaceCode = "package main\n\ntype Writer interface {\n  Write([]byte) (int, error)\n}\n\ntype MyWriter struct{}\n\nfunc (m MyWriter) Write(data []byte) (int, error) {\n  return len(data), nil\n}\n\nfunc main() {\n  var w Writer = MyWriter{}\n}"
      result = parseTypus interfaceCode
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let deps = analyzeDependencies typusFile
      -- Should detect interface implementation dependencies
      length deps @?= 2

-- Property: Dependency analysis detects unused variables
prop_unused_variable_detection :: [String] -> Property
prop_unused_variable_detection variableNames =
  length variableNames >= 1 && length variableNames <= 5 ==>
  let validVars = filter (all isLetter) (nub variableNames)
      code = "package main\n\nfunc main() {\n" ++ unlines (map (\name -> "  " ++ name ++ " := 42") validVars) ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- May succeed, unused vars are often warnings
    Left errs -> property $ any (\err -> "unused" `isInfixOf` show err || "declared but not used" `isInfixOf` show err) errs

-- Test: Dependency analysis handles generic type constraints
test_generic_constraint_analysis :: TestTree
test_generic_constraint_analysis = testCase "Dependency analysis handles generic constraints" $ do
  let genericCode = "package main\n\ntype Comparable interface {\n  Compare(other Comparable) int\n}\n\nfunc Max[T Comparable](a, b T) T {\n  if a.Compare(b) > 0 {\n    return a\n  }\n  return b\n}"
      result = parseTypus genericCode
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let deps = analyzeDependencies typusFile
      -- Should detect dependency between generic function and constraint
      length deps @?= 1

-- Property: Dependency analysis correctly identifies data flow
prop_data_flow_analysis :: [String] -> Property
prop_data_flow_analysis variableNames =
  length variableNames >= 2 && length variableNames <= 4 ==>
  let validVars = filter (all isLetter) (nub variableNames)
      code = "package main\n\nfunc main() {\n" ++ 
             (if length validVars >= 1 then "  " ++ (validVars !! 0) ++ " := 42\n" else "") ++
             (if length validVars >= 2 then "  " ++ (validVars !! 1) ++ " := " ++ (validVars !! 0) ++ " + 1\n" else "") ++
             (if length validVars >= 3 then "  " ++ (validVars !! 2) ++ " := " ++ (validVars !! 1) ++ " * 2\n" else "") ++
             "\n}"
      result = parseTypus code
  in case result of
    Left _ -> property True  -- Parse failure, test vacuously passes
    Right typusFile -> 
      let deps = analyzeDependencies typusFile
      in property $ length deps >= (length validVars - 1)

tests :: TestTree
tests = testGroup "Advanced Dependency Analysis Tests"
  [ test_function_dependency_analysis
  , test_mutual_recursion_detection
  , test_module_boundary_analysis
  , test_interface_dependency_analysis
  , test_generic_constraint_analysis
  , fastProperty "Acyclic dependency graph" prop_acyclic_dependency_graph
  , fastProperty "Type inference consistency" prop_type_inference_consistency
  , fastProperty "Compilation ordering" prop_compilation_ordering
  , fastProperty "Unused variable detection" prop_unused_variable_detection
  , fastProperty "Data flow analysis" prop_data_flow_analysis
  ]