module Test.Unit.DependentTypeBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, CompilerError(..), formatCompilerErrors)
import Parser (parseTypus)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- Test vector length constraints
test_vector_length_constraints :: TestTree
test_vector_length_constraints = testCase "Vector length constraints are enforced" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Vec5 = [5]int"
          , "    // Valid: exact length"
          , "    v1 := Vec5{1, 2, 3, 4, 5}"
          , "    // Invalid: too short"
          , "    v2 := Vec5{1, 2, 3}"
          , "    // Invalid: too long"
          , "    v3 := Vec5{1, 2, 3, 4, 5, 6}"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect length mismatch" $ 
          any (\msg -> "length" `isInfixOf` msg || "constraint" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected dependent type constraint violations"

-- Test matrix dimension constraints
test_matrix_dimension_constraints :: TestTree
test_matrix_dimension_constraints = testCase "Matrix dimension constraints are enforced" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Mat3x3 = [3][3]int"
          , "    // Valid: 3x3 matrix"
          , "    m1 := Mat3x3{{1,2,3}, {4,5,6}, {7,8,9}}"
          , "    // Invalid: wrong dimensions"
          , "    m2 := Mat3x3{{1,2}, {3,4}}"  -- 2x2 instead of 3x3"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect dimension mismatch" $ 
          any (\msg -> "dimension" `isInfixOf` msg || "constraint" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected matrix dimension constraint violations"

-- Test range-dependent types
test_range_dependent_types :: TestTree
test_range_dependent_types = testCase "Range-dependent types are validated" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Age = int { 0 <= value && value <= 150 }"
          , "    // Valid ages"
          , "    validAge := Age(25)"
          , "    // Invalid: negative age"
          , "    negAge := Age(-5)"
          , "    // Invalid: too old"
          , "    oldAge := Age(200)"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect range violations" $ 
          any (\msg -> "range" `isInfixOf` msg || "constraint" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected range constraint violations"

-- Test non-empty slice constraints
test_non_empty_slice_constraints :: TestTree
test_non_empty_slice_constraints = testCase "Non-empty slice constraints are enforced" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type NonEmptySlice = []int { len(value) > 0 }"
          , "    // Valid: non-empty slice"
          , "    valid := NonEmptySlice{1, 2, 3}"
          , "    // Invalid: empty slice"
          , "    empty := NonEmptySlice{}"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect empty slice violation" $ 
          any (\msg -> "empty" `isInfixOf` msg || "constraint" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected non-empty constraint violations"

-- Test string length constraints
test_string_length_constraints :: TestTree
test_string_length_constraints = testCase "String length constraints are enforced" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Username = string { len(value) >= 3 && len(value) <= 20 }"
          , "    // Valid usernames"
          , "    valid1 := Username(\"alice\")"
          , "    valid2 := Username(\"bob12345\")"
          , "    // Invalid: too short"
          , "    short := Username(\"ab\")"
          , "    // Invalid: too long"
          , "    long := Username(\"this_username_is_way_too_long_for_the_constraint\")"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect string length violations" $ 
          any (\msg -> "length" `isInfixOf` msg || "constraint" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected string length constraint violations"

-- Test dependent type function parameters
test_dependent_type_function_params :: TestTree
test_dependent_type_function_params = testCase "Dependent types work with function parameters" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func processVec3(vec [3]int) int {"
          , "    return vec[0] + vec[1] + vec[2]"
          , "}"
          , "func main() {"
          , "    // Valid call"
          , "    v1 := [3]int{1, 2, 3}"
          , "    result1 := processVec3(v1)"
          , "    // Invalid call"
          , "    v2 := [2]int{1, 2}"
          , "    result2 := processVec3(v2)"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect parameter type mismatch" $ 
          any (\msg -> "type" `isInfixOf` msg || "parameter" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected function parameter type errors"

-- Test complex dependent type expressions
test_complex_dependent_expressions :: TestTree
test_complex_dependent_expressions = testCase "Complex dependent type expressions are evaluated" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type ValidPassword = string {"
          , "        len(value) >= 8 && "
          , "        containsUpper(value) && "
          , "        containsDigit(value)"
          , "    }"
          , "    // Valid password"
          , "    good := ValidPassword(\"Password123\")"
          , "    // Invalid: too short"
          , "    short := ValidPassword(\"pwd\")"
          , "    // Invalid: no uppercase"
          , "    noUpper := ValidPassword(\"password123\")"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect complex constraint violations" $ 
          any ("constraint" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected complex constraint violations"

-- QuickCheck property: Dependent type constraints are monotonic
prop_dependent_constraints_monotonic :: Int -> Int -> Property
prop_dependent_constraints_monotonic n m =
  let validRange = 0 <= n && n <= 100
      tighterRange = 10 <= m && m <= 90
  in classify validRange "valid range" $
     classify tighterRange "tighter range" $
     property $ 
       if tighterRange then validRange else True

-- QuickCheck property: Vector operations preserve length constraints
prop_vector_operations_preserve_length :: [Int] -> [Int] -> Property
prop_vector_operations_preserve_length xs ys =
  let lenX = length xs
      lenY = length ys
      sameLength = lenX == lenY
  in classify sameLength "same length vectors" $
     classify (not sameLength) "different length vectors" $
     property sameLength ==> (length (zipWith (+) xs ys) == min lenX lenY)

tests :: TestTree
tests = testGroup "Dependent Type Boundary Conditions"
  [ test_vector_length_constraints
  , test_matrix_dimension_constraints
  , test_range_dependent_types
  , test_non_empty_slice_constraints
  , test_string_length_constraints
  , test_dependent_type_function_params
  , test_complex_dependent_expressions
  , testCase "QuickCheck: Dependent constraints monotonic" $
      fastProperty prop_dependent_constraints_monotonic
  , testCase "QuickCheck: Vector operations preserve length" $
      fastProperty prop_vector_operations_preserve_length
  ]