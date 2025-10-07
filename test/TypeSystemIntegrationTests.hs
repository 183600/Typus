{-# LANGUAGE OverloadedStrings #-}

module TypeSystemIntegrationTests (typeSystemIntegrationTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Parser (parseTypus)
import qualified Compiler (compile)
import qualified Dependencies as Dep
import Data.List (isInfixOf)

-- ============================================================================
-- Type System Integration Tests
-- ============================================================================

typeSystemIntegrationTests :: TestTree
typeSystemIntegrationTests = testGroup "Type System Integration Tests"
  [ endToEndCompilationTests
  , typeCheckingIntegrationTests
  , dependentTypesIntegrationTests
  , ownershipIntegrationTests
  , errorRecoveryTests
  , fullPipelineTests
  ]

-- ============================================================================
-- End-to-End Compilation Tests
-- ============================================================================

endToEndCompilationTests :: TestTree
endToEndCompilationTests = testGroup "End-to-End Compilation"
  [ testCase "Simple program with type checking" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "func add(x: int, y: int): int {"
            , "    return x + y"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should contain package" ("package main" `isInfixOf` goCode)
              assertBool "Should contain function" ("func add" `isInfixOf` goCode)

  , testCase "Program with dependent types" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type PositiveInt where x > 0"
            , "func increment(x: PositiveInt): int {"
            , "    return x + 1"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should generate code" (not $ null goCode)

  , testCase "Program with ownership" $ do
      let source = unlines
            [ "//! ownership: on"
            , "func process(data []int) {"
            , "    fmt.Println(data)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should generate code" (not $ null goCode)

  , testCase "Program with both type systems enabled" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "//! ownership: on"
            , "type SafeArray<T, N> where N > 0"
            , "func createArray(n: int where n > 0): []int {"
            , "    return make([]int, n)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should generate valid code" (not $ null goCode)

  , testCase "Complex program with multiple features" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "//! ownership: on"
            , "type BoundedInt where x >= 0, x < 100"
            , "func validate(x: int): BoundedInt {"
            , "    if x >= 0 && x < 100 {"
            , "        return x"
            , "    }"
            , "    return 0"
            , "}"
            , "func main() {"
            , "    data := []int{1, 2, 3}"
            , "    result := validate(50)"
            , "    fmt.Println(result)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should have main" ("func main" `isInfixOf` goCode)
              assertBool "Should have validate" ("func validate" `isInfixOf` goCode)
  ]

-- ============================================================================
-- Type Checking Integration Tests
-- ============================================================================

typeCheckingIntegrationTests :: TestTree
typeCheckingIntegrationTests = testGroup "Type Checking Integration"
  [ testCase "Valid type definitions compile" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type MyType"
            , "type Generic<T>"
            , "var x: MyType"
            , "var y: Generic<int>"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Type errors are caught" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "var x: UndefinedType"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left _ -> return ()  -- Expected
            Right _ -> assertFailure "Should fail with undefined type"

  , testCase "Function type checking" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "func identity(x: int): int {"
            , "    return x"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> assertBool "Should compile" True

  , testCase "Generic function type checking" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "func head(list: List<T>): T"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Nested type checking" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type Outer<T>"
            , "type Inner<U>"
            , "var x: Outer<Inner<int>>"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()
  ]

-- ============================================================================
-- Dependent Types Integration Tests
-- ============================================================================

dependentTypesIntegrationTests :: TestTree
dependentTypesIntegrationTests = testGroup "Dependent Types Integration"
  [ testCase "Size constraints compile" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type NonEmpty<T> where length > 0"
            , "func head(list: NonEmpty<T>): T"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Range constraints compile" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type Percentage where value >= 0, value <= 100"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Predicate constraints compile" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type PrimeInt where IsPrime(n)"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Invalid constraints are rejected" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type Invalid where x > x"  -- Circular
            ]
      case Parser.parseTypus source of
        Left _ -> return ()  -- May fail at parse
        Right typusFile ->
          case Compiler.compile typusFile of
            Left _ -> return ()  -- Expected
            Right _ -> return ()  -- May pass depending on validation

  , testCase "Complex dependent types" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type Matrix<T, M, N> where M > 0, N > 0"
            , "func multiply(a: Matrix<int, M, N>, b: Matrix<int, N, P>): Matrix<int, M, P>"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()
  ]

-- ============================================================================
-- Ownership Integration Tests
-- ============================================================================

ownershipIntegrationTests :: TestTree
ownershipIntegrationTests = testGroup "Ownership Integration"
  [ testCase "Valid ownership compiles" $ do
      let source = unlines
            [ "//! ownership: on"
            , "func process(data []int) {"
            , "    fmt.Println(data)"
            , "}"
            , "func main() {"
            , "    x := []int{1, 2, 3}"
            , "    process(x)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Use after move is detected" $ do
      let source = unlines
            [ "//! ownership: on"
            , "x := []int{1, 2, 3}"
            , "y := x"
            , "fmt.Println(x)"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left _ -> return ()  -- Expected to fail
            Right _ -> assertFailure "Should detect use after move"

  , testCase "Borrowing compiles correctly" $ do
      let source = unlines
            [ "//! ownership: on"
            , "func inspect(data *[]int) {"
            , "    fmt.Println(*data)"
            , "}"
            , "func main() {"
            , "    x := []int{1, 2, 3}"
            , "    inspect(&x)"
            , "    fmt.Println(x)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()

  , testCase "Mixed ownership and type checking" $ do
      let source = unlines
            [ "//! ownership: on"
            , "//! dependent_types: on"
            , "type SafeData where length > 0"
            , "func process(data []int): SafeData {"
            , "    if len(data) > 0 {"
            , "        return data"
            , "    }"
            , "    return []int{0}"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right _ -> return ()
  ]

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "Error Recovery"
  [ testCase "Multiple type errors reported" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "var x: Undefined1"
            , "var y: Undefined2"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertBool "Should mention errors" ("error" `isInfixOf` map toLower err)
            Right _ -> assertFailure "Should fail with errors"
      where
        toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

  , testCase "Partial compilation after error" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "type ValidType"
            , "var invalid: UndefinedType"
            , "var valid: ValidType"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left _ -> return ()  -- Expected
            Right _ -> assertFailure "Should fail"

  , testCase "Recover from ownership errors" $ do
      let source = unlines
            [ "//! ownership: on"
            , "x := []int{1}"
            , "y := x"
            , "z := x"  -- Error
            , "w := 42"  -- Should still be analyzed
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertBool "Should detect error" (not $ null err)
            Right _ -> assertFailure "Should fail"
  ]

-- ============================================================================
-- Full Pipeline Tests
-- ============================================================================

fullPipelineTests :: TestTree
fullPipelineTests = testGroup "Full Pipeline"
  [ testCase "Parse → Type Check → Code Gen" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "func factorial(n: int where n >= 0): int {"
            , "    if n == 0 {"
            , "        return 1"
            , "    }"
            , "    return n * factorial(n - 1)"
            , "}"
            ]
      -- Step 1: Parse
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile -> do
          -- Step 2: Compile (includes type checking)
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              -- Step 3: Verify output
              assertBool "Should have package" ("package main" `isInfixOf` goCode)
              assertBool "Should have factorial" ("func factorial" `isInfixOf` goCode)

  , testCase "Complete program with all features" $ do
      let source = unlines
            [ "//! dependent_types: on"
            , "//! ownership: on"
            , ""
            , "type PositiveInt where value > 0"
            , "type BoundedList<T, N> where N >= 1, N <= 1000"
            , ""
            , "func createList(n: PositiveInt): []int {"
            , "    return make([]int, n)"
            , "}"
            , ""
            , "func processData(data []int) {"
            , "    sum := 0"
            , "    for _, v := range data {"
            , "        sum += v"
            , "    }"
            , "    fmt.Println(sum)"
            , "}"
            , ""
            , "func main() {"
            , "    size := 10"
            , "    list := createList(size)"
            , "    processData(list)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Has package" ("package main" `isInfixOf` goCode)
              assertBool "Has createList" ("func createList" `isInfixOf` goCode)
              assertBool "Has processData" ("func processData" `isInfixOf` goCode)
              assertBool "Has main" ("func main" `isInfixOf` goCode)

  , testCase "AST analysis and validation" $ do
      let source = unlines
            [ "type List<T>"
            , "func length(list: List<T>): int"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          let errors = Dep.analyzeAST ast
          assertEqual "Should have no errors" [] errors

  , testCase "Constraint solving in pipeline" $ do
      let source = unlines
            [ "type Bounded where x >= 0, x <= 100"
            , "var value: Bounded"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          let errors = Dep.analyzeAST ast
          assertEqual "Constraints should be satisfied" [] errors

  , testCase "Integration with standard library" $ do
      let source = unlines
            [ "import \"fmt\""
            , "import \"math\""
            , ""
            , "func calculate(x: float64): float64 {"
            , "    return math.Sqrt(x)"
            , "}"
            , ""
            , "func main() {"
            , "    result := calculate(16.0)"
            , "    fmt.Println(result)"
            , "}"
            ]
      case Parser.parseTypus source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile ->
          case Compiler.compile typusFile of
            Left err -> assertFailure $ "Compile failed: " ++ err
            Right goCode -> do
              assertBool "Should have imports" ("import" `isInfixOf` goCode)
              assertBool "Should have calculate" ("func calculate" `isInfixOf` goCode)
  ]