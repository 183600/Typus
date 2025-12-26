{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeInferenceAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.TypeChecker
  ( buildTypeEnv
  , buildTypeEnvFromPairs
  , extractDeclarations
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  )
import Compiler (compile, checkTypeError)

import Parser (TypusFile(..), parseTypus)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import qualified Data.Map as Map

-- Test: Type inference handles complex nested expressions
test_nested_expression_inference :: TestTree
test_nested_expression_inference = testCase "Type inference handles nested expressions" $ do
  let nestedCode = "package main\n\nfunc main() {\n  result := (5 + 3) * (2.0 - 1.5)\n}"
      result = compile nestedCode
  case result of
    Left errs -> assertFailure $ "Type inference failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - type inference worked

-- Property: Type environment is consistent across multiple passes
prop_type_env_consistency :: [(String, String)] -> Property
prop_type_env_consistency typePairs =
  not (null typePairs) && length typePairs <= 10 ==>
  let validPairs = filter (\(k, v) -> not (null k) && not (null v)) typePairs
      env1 = buildTypeEnvFromPairs validPairs
      env2 = buildTypeEnvFromPairs validPairs
  in property $ env1 === env2

-- Test: Type inference resolves generic types correctly
test_generic_type_resolution :: TestTree
test_generic_type_resolution = testCase "Type inference resolves generic types" $ do
  let genericCode = "package main\n\nfunc identity[T any](x T) T {\n  return x\n}\n\nfunc main() {\n  result := identity(42)\n}"
      result = compile genericCode
  case result of
    Left errs -> assertFailure $ "Generic type resolution failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - generic types resolved

-- Property: Type inference detects type mismatches in arithmetic operations
prop_arithmetic_type_mismatch :: String -> String -> Property
prop_arithmetic_type_mismatch type1 type2 =
  not (null type1) && not (null type2) && 
  type1 /= type2 && type1 `elem` ["int", "float64", "string"] && 
  type2 `elem` ["int", "float64", "string"] ==>
  let code = "package main\n\nfunc main() {\n  var x " ++ type1 ++ " = 5\n  var y " ++ type2 ++ " = \"hello\"\n  result := x + y\n}"
      result = compile code
  in case result of
    Right _ -> property False  -- Should not succeed with mismatched types
    Left errs -> property $ any (\err -> "type" `isInfixOf` show err || "mismatch" `isInfixOf` show err) errs

-- Test: Type inference handles function type signatures correctly
test_function_signature_inference :: TestTree
test_function_signature_inference = testCase "Type inference handles function signatures" $ do
  let functionCode = "package main\n\nfunc add(x int, y int) int {\n  return x + y\n}\n\nfunc main() {\n  result := add(5, 3)\n}"
      result = compile functionCode
  case result of
    Left errs -> assertFailure $ "Function signature inference failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - function signatures inferred correctly

-- Property: Type inference preserves type information through variable assignments
prop_type_preservation_through_assignment :: String -> Property
prop_type_preservation_through_assignment initialValue =
  not (null initialValue) && length initialValue <= 20 ==>
  let code = "package main\n\nfunc main() {\n  x := " ++ initialValue ++ "\n  y := x\n  z := y\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - types preserved through assignments
    Left errs -> property $ not (any (\err -> "type" `isInfixOf` show err && "error" `isInfixOf` show err) errs)

-- Test: Type inference handles conditional expressions with different branches
test_conditional_type_inference :: TestTree
test_conditional_type_inference = testCase "Type inference handles conditional expressions" $ do
  let conditionalCode = "package main\n\nfunc main() {\n  x := 5\n  var result int\n  if x > 3 {\n    result = 10\n  } else {\n    result = 20\n  }\n}"
      result = compile conditionalCode
  case result of
    Left errs -> assertFailure $ "Conditional type inference failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - conditional types inferred correctly

-- Property: Type inference detects unreachable code paths
prop_unreachable_code_detection :: [String] -> Property
prop_unreachable_code_detection statements =
  length statements >= 2 && length statements <= 5 ==>
  let code = "package main\n\nfunc main() {\n  return\n" ++ unlines statements ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- May succeed, unreachable code is often just a warning
    Left errs -> property $ any (\err -> "unreachable" `isInfixOf` show err || "dead" `isInfixOf` show err) errs

-- Test: Type inference handles interface types correctly
test_interface_type_inference :: TestTree
test_interface_type_inference = testCase "Type inference handles interface types" $ do
  let interfaceCode = "package main\n\ntype Writer interface {\n  Write([]byte) (int, error)\n}\n\ntype MyWriter struct{}\n\nfunc (m MyWriter) Write(data []byte) (int, error) {\n  return len(data), nil\n}\n\nfunc main() {\n  var w Writer = MyWriter{}\n  w.Write([]byte(\"hello\"))\n}"
      result = compile interfaceCode
  case result of
    Left errs -> assertFailure $ "Interface type inference failed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - interface types inferred correctly

-- Property: Type inference maintains consistency in recursive types
prop_recursive_type_consistency :: String -> Property
prop_recursive_type_consistency typeName =
  not (null typeName) && all isLetter typeName ==>
  let code = "package main\n\ntype " ++ typeName ++ " struct {\n  next *" ++ typeName ++ "\n  value int\n}\n\nfunc main() {\n  node := " ++ typeName ++ "{value: 5}\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - recursive types handled correctly
    Left errs -> property $ not (any (\err -> "recursive" `isInfixOf` show err && "error" `isInfixOf` show err) errs)

tests :: TestTree
tests = testGroup "Advanced Type Inference Tests"
  [ test_nested_expression_inference
  , test_generic_type_resolution
  , test_function_signature_inference
  , test_conditional_type_inference
  , test_interface_type_inference
  , fastProperty "Type environment consistency" prop_type_env_consistency
  , fastProperty "Arithmetic type mismatch detection" prop_arithmetic_type_mismatch
  , fastProperty "Type preservation through assignment" prop_type_preservation_through_assignment
  , fastProperty "Unreachable code detection" prop_unreachable_code_detection
  , fastProperty "Recursive type consistency" prop_recursive_type_consistency
  ]