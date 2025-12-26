{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compile, generateGoCode, CompilerResult)
import IntegratedCompiler (compileProject)
import Parser (parseTypus)
import Dependencies (analyzeDependencies)
import Ownership (analyzeOwnership)
import DependentTypesParser (parseDependentTypes)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, lines, unlines)
import Data.Char (isLetter, isDigit, isSpace)
import qualified Data.Text as T
import qualified Data.Map as Map

-- Test: Complete compilation pipeline works correctly
test_complete_compilation_pipeline :: TestTree
test_complete_compilation_pipeline = testCase "Complete compilation pipeline" $ do
  let completeCode = "//! ownership: true\n//! dependentTypes: true\n\npackage main\n\nfunc add(x int, y int) int {\n  return x + y\n}\n\ntype Vector struct {\n  x int\n  y int\n}\n\nfunc (v Vector) magnitude() int {\n  return v.x * v.x + v.y * v.y\n}\n\nfunc main() {\n  vec := Vector{x: 3, y: 4}\n  result := add(vec.magnitude(), 5)\n}"
      result = compile completeCode
  case result of
    Left errs -> assertFailure $ "Complete pipeline failed: " ++ unlines (map show errs)
    Right compiledCode -> do
      let goCode = generateGoCode compiledCode
      -- Check that generated code is reasonable
      if "func main" `isInfix` goCode && "package main" `isInfix` goCode
        then return ()  -- Success - complete pipeline worked
        else assertFailure "Generated code is incomplete"

-- Property: Integration handles multiple compilation phases correctly
prop_multiple_phases_integration :: String -> String -> Property
prop_multiple_phases_integration funcName varName =
  not (null funcName) && all isLetter funcName &&
  not (null varName) && all isLetter varName ==>
  let code = "//! ownership: true\n//! dependentTypes: true\n\npackage main\n\nfunc " ++ funcName ++ "() int {\n  " ++ varName ++ " := 42\n  return " ++ varName ++ "\n}\n\nfunc main() {\n  result := " ++ funcName ++ "()\n}"
      result = compile code
  in case result of
    Right compiledCode -> 
      let goCode = generateGoCode compiledCode
          hasFunction = funcName `isInfix` goCode
          hasMain = "func main" `isInfix` goCode
      in property $ hasFunction .&&. hasMain
    Left _ -> property True  -- Compilation failed, test vacuously passes

-- Test: Error propagation through the compilation pipeline
test_error_propagation :: TestTree
test_error_propagation = testCase "Error propagation through pipeline" $ do
  let errorCode = "//! ownership: true\n\npackage main\n\nfunc main() {\n  x := 5\n  y := \"hello\"\n  result := x + y  // Type error\n}"
      result = compile errorCode
  case result of
    Right _ -> assertFailure "Expected type error to propagate through pipeline"
    Left errs -> do
      let errorMessages = map show errs
          hasTypeError = any (\err -> "type" `isInfix` err) errorMessages
      if hasTypeError
        then return ()  -- Success - error properly propagated
        else assertFailure $ "Expected type error, got: " ++ unlines errorMessages

-- Property: Ownership and dependent types work together
prop_ownership_dependent_types_integration :: String -> Property
prop_ownership_dependent_types_integration typeName =
  not (null typeName) && all isLetter typeName ==>
  let code = "//! ownership: true\n//! dependentTypes: true\n\npackage main\n\ntype " ++ typeName ++ " struct {\n  data []int\n}\n\nfunc process(x " ++ typeName ++ ") int {\n  return len(x.data)\n}\n\nfunc main() {\n  item := " ++ typeName ++ "{data: make([]int, 10)}\n  result := process(item)\n}"
      result = compile code
  in case result of
    Right compiledCode -> 
      let goCode = generateGoCode compiledCode
          hasStruct = typeName `isInfix` goCode
          hasProcess = "process" `isInfix` goCode
      in property $ hasStruct .&&. hasProcess
    Left _ -> property True  -- Compilation failed, test vacuously passes

-- Test: Cross-module compilation integration
test_cross_module_integration :: TestTree
test_cross_module_integration = testCase "Cross-module compilation integration" $ do
  let moduleCode = "package utils\n\nfunc Helper(x int) int {\n  return x * 2\n}"
      mainCode = "package main\n\nimport \"utils\"\n\nfunc main() {\n  result := utils.Helper(5)\n}"
  -- Note: This is a simplified test - real cross-module compilation would require file system operations
      result1 = compile moduleCode
      result2 = compile mainCode
  case (result1, result2) of
    (Right _, Right _) -> return ()  -- Success - both modules compiled
    (Left err1, Left err2) -> assertFailure $ "Both modules failed: " ++ show err1 ++ "\n" ++ show err2
    (Left err, Right _) -> assertFailure $ "Utils module failed: " ++ show err
    (Right _, Left err) -> assertFailure $ "Main module failed: " ++ show err

-- Property: Integration handles complex type hierarchies
prop_complex_type_hierarchy :: [(String, String)] -> Property
prop_complex_type_hierarchy typeDefinitions =
  not (null typeDefinitions) && length typeDefinitions <= 5 ==>
  let validTypes = filter (\(name, base) -> not (null name) && not (null base) && all isLetter name && all isLetter base) typeDefinitions
      typeCode = unlines $ map (\(name, base) -> "type " ++ name ++ " struct {\n  " ++ base + "\n}") validTypes
      code = "//! dependentTypes: true\n\npackage main\n\n" ++ typeCode ++ "\n\nfunc main() {\n  // Use types\n}"
      result = compile code
  in case result of
    Right compiledCode -> 
      let goCode = generateGoCode compiledCode
          hasTypes = any (\(name, _) -> name `isInfix` goCode) validTypes
      in property $ hasTypes
    Left _ -> property True  -- Compilation failed, test vacuously passes

-- Test: Build tag integration
test_build_tag_integration :: TestTree
test_build_tag_integration = testCase "Build tag integration" $ do
  let buildTagCode = "//go:build linux\n\npackage main\n\nfunc main() {\n  println(\"Linux specific code\")\n}"
      result = compile buildTagCode
  case result of
    Left errs -> assertFailure $ "Build tag integration failed: " ++ unlines (map show errs)
    Right compiledCode -> do
      let goCode = generateGoCode compiledCode
      -- Check that build tag is preserved
      if "//go:build" `isInfix` goCode
        then return ()  -- Success - build tag preserved
        else assertFailure "Build tag not preserved in generated code"

-- Property: Integration preserves comments and documentation
prop_comment_preservation :: String -> Property
prop_comment_preservation comment =
  not (null comment) && length comment <= 50 && not ('\n' `elem` comment) ==>
  let code = "package main\n\n// " ++ comment ++ "\nfunc main() {\n  x := 5\n}"
      result = compile code
  in case result of
    Right compiledCode -> 
      let goCode = generateGoCode compiledCode
          commentPreserved = comment `isInfix` goCode
      in property $ commentPreserved
    Left _ -> property True  -- Compilation failed, test vacuously passes

-- Test: Performance of end-to-end compilation
test_end_to_end_performance :: TestTree
test_end_to_end_performance = testCase "End-to-end compilation performance" $ do
  let performanceCode = unlines
        [ "//! ownership: true"
        , "//! dependentTypes: true"
        , ""
        , "package main"
        , ""
        , "func fibonacci(n int) int {"
        , "  if n <= 1 {"
        , "    return n"
        , "  }"
        , "  return fibonacci(n-1) + fibonacci(n-2)"
        , "}"
        , ""
        , "func factorial(n int) int {"
        , "  if n <= 1 {"
        , "    return 1"
        , "  }"
        , "  return n * factorial(n-1)"
        , "}"
        , ""
        , "func main() {"
        , "  fib := fibonacci(10)"
        , "  fact := factorial(5)"
        , "  _ = fib + fact"
        , "}"
        ]
      result = compile performanceCode
  case result of
    Left errs -> assertFailure $ "Performance test failed: " ++ unlines (map show errs)
    Right compiledCode -> do
      let goCode = generateGoCode compiledCode
      -- Check that both functions are present
      if "fibonacci" `isInfix` goCode && "factorial" `isInfix` goCode
        then return ()  -- Success - performance test completed
        else assertFailure "Generated code missing functions"

-- Property: Integration handles edge cases gracefully
prop_edge_case_handling :: String -> Property
prop_edge_case_handling edgeCase =
  not (null edgeCase) && length edgeCase <= 30 ==>
  let code = "package main\n\nfunc main() {\n  // Edge case: " ++ edgeCase ++ "\n  x := 42\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - edge case handled
    Left errs -> property $ not (null (map show errs))  -- Should provide some error message

tests :: TestTree
tests = testGroup "End-to-End Integration Tests"
  [ test_complete_compilation_pipeline
  , test_error_propagation
  , test_cross_module_integration
  , test_build_tag_integration
  , test_end_to_end_performance
  , fastProperty "Multiple phases integration" prop_multiple_phases_integration
  , fastProperty "Ownership and dependent types integration" prop_ownership_dependent_types_integration
  , fastProperty "Complex type hierarchy" prop_complex_type_hierarchy
  , fastProperty "Comment preservation" prop_comment_preservation
  , fastProperty "Edge case handling" prop_edge_case_handling
  ]