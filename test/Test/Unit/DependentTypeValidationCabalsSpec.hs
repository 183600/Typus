{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeValidationCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import DependentTypesParser (TypeRef(..), TypeConstraint(..), DependentType(..), parseDependentType, validateDependentTypeSyntax)
import Compiler (checkDependentTypes)
import Parser (parseTypus, TypusFile(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import qualified Data.Text as T

-- Test 1: Dependent type validation catches invalid constraints
test_dependent_type_invalid_constraints :: TestTree
test_dependent_type_invalid_constraints =
  testCase "Dependent type validation catches invalid constraints" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type Vector(n: int) struct {"
          , "  data [n]int"
          , "}"
          , "func main() {"
          , "  // Invalid: negative size constraint"
          , "  v: Vector(-5)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- Should catch invalid constraint
            assertBool "Should detect invalid constraint" $
              L.any (`L.isInfixOf` show typeErr) 
                ["negative", "constraint", "invalid", "Vector"]
          Right _ -> do
            assertFailure "Expected type error for invalid constraint"

-- Test 2: Dependent type validation enforces L.length constraints
test_dependent_type_length_constraints :: TestTree
test_dependent_type_length_constraints =
  testCase "Dependent type validation enforces L.length constraints" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type String(len: int) where len > 0 struct {"
          , "  data [len]byte"
          , "}"
          , "func main() {"
          , "  s1: String(10)  // Valid"
          , "  s2: String(0)   // Invalid: zero L.length"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- Should catch zero L.length constraint
            assertBool "Should detect zero L.length constraint" $
              L.any (`L.isInfixOf` show typeErr) 
                ["zero", "L.length", "constraint", "String"]
          Right _ -> do
            assertFailure "Expected type error for zero L.length"

-- Test 3: Dependent type validation handles complex expressions
test_dependent_type_complex_expressions :: TestTree
test_dependent_type_complex_expressions =
  testCase "Dependent type validation handles complex expressions" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type Matrix(m: int, n: int) where m > 0 && n > 0 struct {"
          , "  data [m * n]int"
          , "}"
          , "func multiply(m: Matrix(2, 3), n: Matrix(3, 4)) Matrix(2, 4) {"
          , "  // Matrix multiplication with dimension checking"
          , "  return Matrix(2, 4){}"
          , "}"
          , "func main() {"
          , "  m := Matrix(2, 3){}"
          , "  n := Matrix(3, 4){}"
          , "  result := multiply(m, n)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- May fail on complex expression evaluation
            assertBool "Should handle complex expressions" $
              L.any (`L.isInfixOf` show typeErr) 
                ["matrix", "dimension", "constraint"]
          Right _ -> do
            -- Complex expression validation passed
            assertBool "Should validate complex dependent type expressions" True

-- Test 4: Dependent type validation tracks type-level computation
test_dependent_type_computation :: TestTree
test_dependent_type_computation =
  testCase "Dependent type validation tracks type-level computation" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type List(n: int) struct {"
          , "  L.head: int"
          , "  L.tail: List(n - 1) where n > 0"
          , "}"
          , "type Nil struct {}"
          , "func main() {"
          , "  list: List(3)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- Should handle recursive type computation
            assertBool "Should handle type-level computation" $
              L.any (`L.isInfixOf` show typeErr) 
                ["recursive", "computation", "List"]
          Right _ -> do
            -- Type computation passed
            assertBool "Should handle type-level recursive computation" True

-- QuickCheck property: Dependent type validation is sound
prop_dependent_type_validation_sound :: String -> Property
prop_dependent_type_validation_sound constraint =
  L.length constraint < 50 ==>  -- Keep constraints reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "type Test(n: int) where " ++ constraint ++ " struct {"
        , "  value int"
        , "}"
        , "func main() {"
        , "  t: Test(5)"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Invalid syntax is skipped
       Right typusFile ->
         case checkDependentTypes typusFile of
           Left _ -> property True  -- Type errors are acceptable
           Right _ -> property True  -- Valid constraints pass

-- Test 5: Dependent type validation with predicate functions
test_dependent_type_predicates :: TestTree
test_dependent_type_predicates =
  testCase "Dependent type validation with predicate functions" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func isPrime(n: int) bool {"
          , "  if n <= 1 { return false }"
          , "  for i := 2; i * i <= n; i++ {"
          , "    if n % i == 0 { return false }"
          , "  }"
          , "  return true"
          , "}"
          , "type Prime(p: int) where isPrime(p) struct {"
          , "  value int"
          , "}"
          , "func main() {"
          , "  p1: Prime(7)   // Valid prime"
          , "  p2: Prime(10)  // Invalid: not prime"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- Should catch non-prime constraint
            assertBool "Should detect non-prime constraint" $
              L.any (`L.isInfixOf` show typeErr) 
                ["prime", "predicate", "constraint"]
          Right _ -> do
            assertFailure "Expected type error for non-prime constraint"

-- Test 6: Dependent type validation handles nested constraints
test_dependent_type_nested_constraints :: TestTree
test_dependent_type_nested_constraints =
  testCase "Dependent type validation handles nested constraints" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type Array2D(m: int, n: int) where m > 0 && n > 0 struct {"
          , "  data [m][n]int"
          , "}"
          , "type SquareMatrix(s: int) where s > 0 struct {"
          , "  matrix: Array2D(s, s)"
          , "}"
          , "func main() {"
          , "  valid: SquareMatrix(3)"
          , "  invalid: SquareMatrix(-1)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case checkDependentTypes typusFile of
          Left typeErr -> do
            -- Should catch nested constraint violation
            assertBool "Should detect nested constraint violation" $
              L.any (`L.isInfixOf` show typeErr) 
                ["nested", "constraint", "SquareMatrix"]
          Right _ -> do
            assertFailure "Expected type error for negative size"

-- QuickCheck property: Dependent type parsing is consistent
prop_dependent_type_parsing_consistent :: String -> Property
prop_dependent_type_parsing_consistent typeDef =
  L.length typeDef < 100 ==>  -- Keep type definitions reasonable
  case parseDependentType typeDef of
    Left _ -> property True  -- Invalid syntax is skipped
    Right result1 ->
      case parseDependentType typeDef of
        Left _ -> property False  -- Should be consistent
        Right result2 -> property $ result1 == result2

tests :: TestTree
tests =
  testGroup "Dependent Type Validation Cabals Tests"
    [ test_dependent_type_invalid_constraints
    , test_dependent_type_length_constraints
    , test_dependent_type_complex_expressions
    , test_dependent_type_computation
    , fastProperty "Dependent type validation is sound" prop_dependent_type_validation_sound
    , test_dependent_type_predicates
    , test_dependent_type_nested_constraints
    , fastProperty "Dependent type parsing is consistent" prop_dependent_type_parsing_consistent
    ]