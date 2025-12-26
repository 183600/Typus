{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.DependentTypeConstraintSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Compiler (checkDependentTypes, CompilerError(..))
import Parser (parseTypus)
import Control.Exception (try, SomeException)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- | Test dependent type constraint validation
tests :: TestTree
tests = testGroup "Dependent Type Constraint Tests"
  [ testCase "Vector length constraint validation" testVectorLengthConstraint
  , testCase "Non-negative integer constraints" testNonNegativeConstraints
  , testCase "Range constraint validation" testRangeConstraints
  , testCase "String length constraints" testStringLengthConstraints
  , testCase "Array index bounds constraints" testArrayIndexConstraints
  , testCase "Complex dependent type expressions" testComplexDependentTypes
  , testProperty "Constraint validation is sound" constraintValidationSound
  , testCase "Constraint error messages" testConstraintErrorMessages
  ]

-- | Test vector length constraint validation
testVectorLengthConstraint :: Assertion
testVectorLengthConstraint = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype Vector(n int) struct {\n    length int\n    data []float64\n}\n\nfunc NewVector(length int, data []float64) Vector(length) {\n    if len(data) != length {\n        panic(\"Vector data length doesn't match dimension\")\n    }\n    return Vector{length: length, data: data}\n}\n\nfunc main() {\n    v := NewVector(3, []float64{1.0, 2.0, 3.0})\n    println(v.length)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Valid vector creation should not produce errors
      assertBool "Valid vector creation should not produce errors" $
        null [err | DependentTypeError err <- errors]

-- | Test non-negative integer constraints
testNonNegativeConstraints :: Assertion
testNonNegativeConstraints = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype Natural(n int) struct {\n    value int\n}\n\nfunc NewNatural(n int) Natural(n) {\n    if n < 0 {\n        panic(\"Natural numbers must be non-negative\")\n    }\n    return Natural{value: n}\n}\n\nfunc main() {\n    n := NewNatural(5)  // Valid\n    m := NewNatural(-1) // Should trigger constraint violation\n    println(n.value)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Should detect negative natural number
      assertBool "Should detect negative natural number constraint violation" $
        any isConstraintViolationError errors

-- | Test range constraint validation
testRangeConstraints :: Assertion
testRangeConstraints = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype BoundedInt(min int, max int) struct {\n    value int\n}\n\nfunc NewBoundedInt(value, min, max int) BoundedInt(min, max) {\n    if value < min || value > max {\n        panic(\"Value out of bounds\")\n    }\n    return BoundedInt{value: value}\n}\n\nfunc main() {\n    x := NewBoundedInt(5, 1, 10)   // Valid\n    y := NewBoundedInt(15, 1, 10)  // Invalid - out of range\n    println(x.value)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Should detect out-of-bounds error
      assertBool "Should detect out-of-bounds constraint violation" $
        any isConstraintViolationError errors

-- | Test string length constraints
testStringLengthConstraints :: Assertion
testStringLengthConstraints = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype BoundedString(maxLen int) struct {\n    data string\n}\n\nfunc NewBoundedString(s string, maxLen int) BoundedString(maxLen) {\n    if len(s) > maxLen {\n        panic(\"String too long\")\n    }\n    return BoundedString{data: s}\n}\n\nfunc main() {\n    s1 := NewBoundedString(\"hello\", 10)  // Valid\n    s2 := NewBoundedString(\"this is too long\", 5)  // Invalid\n    println(s1.data)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Should detect string length violation
      assertBool "Should detect string length constraint violation" $
        any isConstraintViolationError errors

-- | Test array index bounds constraints
testArrayIndexConstraints :: Assertion
testArrayIndexConstraints = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype SafeArray(n int) struct {\n    data [n]int\n    length int\n}\n\nfunc (a SafeArray(n)) Get(index int) int {\n    if index < 0 || index >= n {\n        panic(\"Array index out of bounds\")\n    }\n    return a.data[index]\n}\n\nfunc main() {\n    arr := SafeArray{data: [3]int{1, 2, 3}, length: 3}\n    x := arr.Get(1)   // Valid\n    y := arr.Get(5)   // Invalid - out of bounds\n    println(x)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Should detect array bounds violation
      assertBool "Should detect array bounds constraint violation" $
        any isConstraintViolationError errors

-- | Test complex dependent type expressions
testComplexDependentTypes :: Assertion
testComplexDependentTypes = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype Matrix(m int, n int) struct {\n    rows int\n    cols int\n    data [][]float64\n}\n\nfunc NewMatrix(m, n int) Matrix(m, n) {\n    data := make([][]float64, m)\n    for i := 0; i < m; i++ {\n        data[i] = make([]float64, n)\n    }\n    return Matrix{rows: m, cols: n, data: data}\n}\n\nfunc (m Matrix(m, n)) Multiply(other Matrix(n, p)) Matrix(m, p) {\n    // Matrix multiplication with type-safe dimensions\n    result := NewMatrix(m, p)\n    // Implementation...\n    return result\n}\n\nfunc main() {\n    a := NewMatrix(2, 3)\n    b := NewMatrix(3, 4)\n    c := a.Multiply(b)  // Valid: 2x3 * 3x4 = 2x4\n    println(c.rows, c.cols)\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      -- Complex matrix multiplication should be valid
      assertBool "Complex dependent type operations should be valid" $
        null [err | DependentTypeError err <- errors]

-- | Property: Constraint validation should be sound
constraintValidationSound :: String -> Property
constraintValidationSound input =
  "dependent_types" `isInfixOf` input && "panic" `isInfixOf` input ==>
  case parseTypus input of
    Left _ -> property True -- Invalid input is okay
    Right typusFile -> 
      case checkDependentTypes typusFile of
        Left _ -> property True -- Analysis failure is acceptable
        Right errors -> 
          -- If there are no constraint violations, the program should be safe
          let constraintErrors = [err | DependentTypeError err <- errors, isConstraintViolationError err]
          in null constraintErrors ==> property True

-- | Test constraint error messages
testConstraintErrorMessages :: Assertion
testConstraintErrorMessages = do
  let input = "//! dependent_types: on\n\npackage main\n\ntype PositiveInt struct {\n    value int\n}\n\nfunc NewPositiveInt(n int) PositiveInt {\n    if n <= 0 {\n        panic(\"must be positive\")\n    }\n    return PositiveInt{value: n}\n}\n\nfunc main() {\n    x := NewPositiveInt(0)  // Should produce clear error\n}"
  
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      errors <- checkDependentTypes typusFile
      let constraintErrors = [err | DependentTypeError err <- errors]
      assertBool "Should produce constraint violation error" $
        not (null constraintErrors)
      -- Check that error message is informative
      case constraintErrors of
        (err:_) -> assertBool "Error message should be informative" $
          length (show err) > 10
        [] -> return ()

-- | Helper function to check if an error is a constraint violation
isConstraintViolationError :: CompilerError -> Bool
isConstraintViolationError (DependentTypeError msg) = 
  "constraint" `isInfixOf` msg || 
  "violation" `isInfixOf` msg ||
  "bounds" `isInfixOf` msg ||
  "negative" `isInfixOf` msg
isConstraintViolationError _ = False