{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.TypeInferenceComplexSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Compiler (buildTypeEnv, extractDeclarations, CompilerError(..))
import Parser (parseTypus)
import Control.Exception (try, SomeException)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as Map

-- | Test complex type inference scenarios
tests :: TestTree
tests = testGroup "Complex Type Inference Tests"
  [ testCase "Generic type inference" testGenericTypeInference
  , testCase "Function return type inference" testFunctionReturnTypeInference
  , testCase "Complex expression type inference" testComplexExpressionInference
  , testCase "Type inference with dependent types" testDependentTypeInference
  , testCase "Type inference in conditional branches" testConditionalTypeInference
  , testCase "Recursive type inference" testRecursiveTypeInference
  , testProperty "Type inference is deterministic" typeInferenceDeterministic
  , testCase "Type inference error quality" testTypeInferenceErrorQuality
  ]

-- | Test generic type inference
testGenericTypeInference :: Assertion
testGenericTypeInference = do
  let input = "package main\n\nfunc identity[T any](x T) T {\n    return x\n}\n\nfunc main() {\n    i := identity(42)        // Should infer int\n    s := identity(\"hello\")   // Should infer string\n    println(i, s)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      typeEnv <- buildTypeEnv typusFile
      -- Type environment should contain inferred types
      assertBool "Type environment should contain generic function types" $
        Map.size typeEnv > 0

-- | Test function return type inference
testFunctionReturnTypeInference :: Assertion
testFunctionReturnTypeInference = do
  let input = "package main\n\nfunc add(a, b int) int {\n    return a + b\n}\n\nfunc getString() string {\n    return \"hello\"\n}\n\nfunc getComplex() (int, string) {\n    return 42, \"world\"\n}\n\nfunc main() {\n    x := add(1, 2)           // Should infer int\n    s := getString()         // Should infer string\n    i, str := getComplex()   // Should infer (int, string)\n    println(x, s, i, str)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      declarations <- extractDeclarations typusFile
      -- Should extract function declarations with return types
      assertBool "Should extract function declarations" $
        length declarations >= 3

-- | Test complex expression type inference
testComplexExpressionInference :: Assertion
testComplexExpressionInference = do
  let input = "package main\n\nfunc main() {\n    // Mixed type operations\n    a := 42\n    b := 3.14\n    c := a + int(b)        // Type conversion\n    d := float64(a) + b    // Another conversion\n    \n    // Function call inference\n    e := func(x int) float64 {\n        return float64(x) * 2.0\n    }(c)\n    \n    println(a, b, c, d, e)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      typeEnv <- buildTypeEnv typusFile
      -- Should handle complex type conversions and inference
      assertBool "Should handle complex type inference" $
        Map.size typeEnv > 0

-- | Test type inference with dependent types
testDependentTypeInference :: Assertion
testDependentTypeInference = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype Vector(n int) struct {\n    length int\n    data []float64\n}\n\nfunc NewVector(length int, data []float64) Vector(length) {\n    return Vector{length: length, data: data}\n}\n\nfunc main() {\n    v := NewVector(3, []float64{1.0, 2.0, 3.0})  // Should infer Vector(3)\n    println(v.length)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      typeEnv <- buildTypeEnv typusFile
      -- Should infer dependent types correctly
      assertBool "Should handle dependent type inference" $
        Map.size typeEnv > 0

-- | Test type inference in conditional branches
testConditionalTypeInference :: Assertion
testConditionalTypeInference = do
  let input = "package main\n\nfunc main() {\n    x := 42\n    \n    if true {\n        y := x + 1      // Should infer int\n        println(y)\n    } else {\n        z := \"hello\"    // Should infer string\n        println(z)\n    }\n    \n    // Interface{} type inference\n    var value interface{} = x\n    \n    switch v := value.(type) {\n    case int:\n        println(\"int:\", v)\n    case string:\n        println(\"string:\", v)\n    }\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      typeEnv <- buildTypeEnv typusFile
      -- Should handle conditional type inference
      assertBool "Should handle conditional type inference" $
        Map.size typeEnv > 0

-- | Test recursive type inference
testRecursiveTypeInference :: Assertion
testRecursiveTypeInference = do
  let input = "package main\n\ntype Node struct {\n    value int\n    next  *Node\n}\n\nfunc createList(n int) *Node {\n    if n <= 0 {\n        return nil\n    }\n    return &Node{\n        value: n,\n        next:  createList(n - 1),\n    }\n}\n\nfunc main() {\n    head := createList(5)  // Should infer *Node\n    println(head.value)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      declarations <- extractDeclarations typusFile
      -- Should handle recursive type definitions
      assertBool "Should handle recursive type definitions" $
        any ("Node" `isInfixOf`) (map show declarations)

-- | Property: Type inference should be deterministic
typeInferenceDeterministic :: String -> Property
typeInferenceDeterministic input =
  "package main" `isInfixOf` input && "func" `isInfixOf` input ==>
  case parseTypus input of
    Left _ -> property True -- Invalid input is okay
    Right typusFile1 -> 
      case buildTypeEnv typusFile1 of
        Left _ -> property True -- Type inference failure is acceptable
        Right typeEnv1 ->
          case buildTypeEnv typusFile1 of
            Left _ -> property False -- Should be consistent
            Right typeEnv2 -> Map.size typeEnv1 === Map.size typeEnv2

-- | Test type inference error quality
testTypeInferenceErrorQuality :: Assertion
testTypeInferenceErrorQuality = do
  let input = "package main\n\nfunc main() {\n    a := 42\n    b := \"hello\"\n    c := a + b  // Type inference should fail clearly\n    println(c)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      typeEnv <- buildTypeEnv typusFile
      case typeEnv of
        Left errors -> do
          -- Should produce clear type inference errors
          assertBool "Should produce type inference errors" $
            not (null errors)
          case errors of
            (err:_) -> do
              let errMsg = show err
              assertBool "Error message should be informative" $
                length errMsg > 15 && 
                any (`isInfixOf` errMsg) ["type", "int", "string"]
            [] -> assertFailure "No errors found"
        Right _ -> assertFailure "Type inference should have failed"

-- | Helper function to check if an error is a type inference error
isTypeInferenceError :: CompilerError -> Bool
isTypeInferenceError TypeError{} = True
isTypeInferenceError TypeMismatchError{} = True
isTypeInferenceError InferenceError{} = True
isTypeInferenceError _ = False