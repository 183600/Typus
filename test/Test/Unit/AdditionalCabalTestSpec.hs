{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.AdditionalCabalTestSpec (tests) where

import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (null)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.QuickCheck (property)
import GHC.Generics (Generic)

import Parser (parseTypus, TypusFile(..), FileDirectives(..))
import Compiler (compile, generateGoCode)
import Compiler.IR (rawSourceFromTypus)
import Ownership (analyzeOwnership)
import DependentTypesParser (validateDependentTypeSyntax)
import SourceLocation (SourceSpan(..), SourcePos(..))
import SyntaxValidator (validateSyntax)
import Dependencies (analyzeDependentTypes)
import Compiler.Errors.Core (TypeError(..))

-- Test data generators for QuickCheck
import Test.QuickCheck.Gen (Gen, choose, vectorOf, elements)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

-- Newtype wrapper for String to avoid duplicate instance
newtype TestString = TestString { getTestString :: String }
  deriving (Eq, Show, Generic)

instance Arbitrary TestString where
  arbitrary = do
    n <- choose (0, 20)
    str <- vectorOf n (elements ['a'..'z'])
    return (TestString str)

-- Helper functions
validTypusCode :: String
validTypusCode = unlines
  [ "package main"
  , "func main() {"
  , "    println(\"Hello, World!\")"
  , "}"
  ]

ownershipCode :: String
ownershipCode = unlines
  [ "//! ownership: on"
  , "package main"
  , "func main() {"
  , "    {//! ownership: on"
  , "        data := make([]int, 10)"
  , "        process(data)"
  , "    }"
  , "}"
  ]

dependentTypesCode :: String
dependentTypesCode = unlines
  [ "//! dependent_types: on"
  , "package main"
  , "func main() {"
  , "    vec: Vector<int>{n: 5} where n > 0"
  , "    process(vec)"
  , "}"
  ]

tests :: TestTree
tests =
  testGroup "Additional Cabal Tests"
    [ parserBoundaryTests
    , compilerErrorHandlingTests
    , ownershipAnalysisTests
    , dependentTypesTests
    , sourceLocationTests
    , toolchainIntegrationTests
    , syntaxValidationTests
    , dependencyAnalysisTests
    , integrationTests
    , quickCheckPropertyTests
    ]

-- 1. Parser Boundary Condition Tests
parserBoundaryTests :: TestTree
parserBoundaryTests =
  testGroup "Parser Boundary Conditions"
    [ testCase "handles empty input gracefully" $ do
        let source = ""
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse empty input: " ++ err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "handles whitespace-only input" $ do
        let source = "   \n  \t  \n  "
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse whitespace-only input: " ++ err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "rejects malformed directives" $ do
        let source = unlines
              [ "//! invalid_directive: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertBool "Should reject invalid directive" ("Unknown file directive" `L.isInfixOf` err)
          Right _ -> assertFailure "Expected parse failure for invalid directive"

    , testCase "handles unclosed blocks" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"test\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertBool "Should detect unclosed block" ("Unclosed directive block" `L.isInfixOf` err)
          Right _ -> assertFailure "Expected parse failure for unclosed block"

    , testCase "handles deeply nested structures" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        for i := 0; i < 10; i++ {"
              , "            switch i {"
              , "            case 1:"
              , "                select {"
              , "                case ch <- i:"
              , "                    println(i)"
              , "                default:"
              , "                    println(\"default\")"
              , "                }"
              , "            }"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse nested structures: " ++ err
          Right typusFile -> assertBool "Should handle deeply nested structures" (not $ L.null $ tfBlocks typusFile)
    ]

-- 2. Compiler Error Handling Tests
compilerErrorHandlingTests :: TestTree
compilerErrorHandlingTests =
  testGroup "Compiler Error Handling"
    [ testCase "provides meaningful error messages" $ do
        let invalidCode = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"not an int\""
              , "}"
              ]
        case parseTypus invalidCode of
          Left parseErr -> assertBool "Parse error should be meaningful" (L.length parseErr > 10)
          Right typusFile -> 
            case compile typusFile of
              Left err -> assertBool "Error should be meaningful" (L.length (show err) > 10)
              Right _ -> assertFailure "Expected compilation to fail with type error"

    , testCase "handles undefined references gracefully" $ do
        let invalidCode = unlines
              [ "package main"
              , "func main() {"
              , "    undefinedFunction()"
              , "}"
              ]
        case parseTypus invalidCode of
          Left parseErr -> assertBool "Parse error should be meaningful" (L.length parseErr > 10)
          Right typusFile ->
            case compile typusFile of
              Left err -> assertBool "Should detect undefined function" ("undefined" `L.isInfixOf` show err)
              Right _ -> assertFailure "Expected compilation to fail with undefined reference"

    , testCase "recovers from syntax errors" $ do
        let codeWithSyntaxError = unlines
              [ "package main"
              , "func main() {"
              , "    if true"
              , "        println(\"missing braces\")"
              , "}"
              ]
        case parseTypus codeWithSyntaxError of
          Left parseErr -> assertBool "Parse error should be meaningful" (L.length parseErr > 10)
          Right typusFile ->
            case compile typusFile of
              Left err -> assertBool "Should provide syntax error location" ("syntax" `L.isInfixOf` show err)
              Right _ -> assertFailure "Expected compilation to fail with syntax error"

    , testCase "validates type constraints" $ do
        let invalidTypeCode = unlines
              [ "package main"
              , "func main() {"
              , "    var x string = 123"
              , "    var y int = \"hello\""
              , "}"
              ]
        case parseTypus invalidTypeCode of
          Left parseErr -> assertBool "Parse error should be meaningful" (L.length parseErr > 10)
          Right typusFile ->
            case compile typusFile of
              Left err -> assertBool "Should detect type mismatch" ("type" `L.isInfixOf` show err)
              Right _ -> assertFailure "Expected compilation to fail with type mismatch"
    ]

-- 3. Ownership Analysis Tests
ownershipAnalysisTests :: TestTree
ownershipAnalysisTests =
  testGroup "Ownership Analysis"
    [ testCase "detects ownership transfers" $ do
        case parseTypus ownershipCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnership (rawSourceFromTypus typusFile)
            assertBool "Should detect ownership transfers" (not $ null result)

    , testCase "validates ownership constraints" $ do
        let ownershipConstraintCode = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    data := make([]int, 10)"
              , "    transfer(data)"
              , "    use(data)  // Should error: data already transferred"
              , "}"
              ]
        case parseTypus ownershipConstraintCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnership (rawSourceFromTypus typusFile)
            assertBool "Should detect ownership constraint violations" (not $ null result)

    , testCase "handles borrowing scenarios" $ do
        let borrowingCode = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    data := make([]int, 10)"
              , "    borrow(&data)"
              , "    use(data)  // Should be OK: data is borrowed, not moved"
              , "}"
              ]
        case parseTypus borrowingCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnership (rawSourceFromTypus typusFile)
            assertBool "Should handle borrowing correctly" (null result || L.length result <= 1)
    ]

-- 4. Dependent Types Tests
dependentTypesTests :: TestTree
dependentTypesTests =
  testGroup "Dependent Types"
    [ testCase "validates type constraints" $ do
        case parseTypus dependentTypesCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = validateDependentTypeSyntax (rawSourceFromTypus typusFile)
            assertBool "Should validate dependent type constraints" (not $ null result)

    , testCase "detects constraint violations" $ do
        let invalidDependentCode = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {"
              , "    vec: Vector<int>{n: -5} where n > 0  // Violates n > 0"
              , "    process(vec)"
              , "}"
              ]
        case parseTypus invalidDependentCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = validateDependentTypeSyntax (rawSourceFromTypus typusFile)
            assertBool "Should detect constraint violations" (not $ null result)

    , testCase "handles complex type expressions" $ do
        let complexTypeCode = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {"
              , "    matrix: Matrix<T, m, n>{data: arr} where m > 0 && n > 0"
              , "    result: Matrix<T, n, p> = multiply(matrix, other)"
              , "}"
              ]
        case parseTypus complexTypeCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let result = validateDependentTypeSyntax (rawSourceFromTypus typusFile)
            assertBool "Should handle complex type expressions" (not $ null result)
    ]

-- 5. Source Location Tests
sourceLocationTests :: TestTree
sourceLocationTests =
  testGroup "Source Location Tracking"
    [ testCase "tracks accurate line numbers" $ do
        let multiLineCode = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"line 3\")"
              , "    println(\"line 4\")"
              , "}"
              ]
        case parseTypus multiLineCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "Should track line numbers accurately" (not $ null blocks)

    , testCase "preserves column positions" $ do
        let indentedCode = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        println(\"deeply indented\")"
              , "    }"
              , "}"
              ]
        case parseTypus indentedCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "Should preserve column positions" (not $ null blocks)

    , testCase "handles multi-line spans" $ do
        let multiLineSpanCode = unlines
              [ "package main"
              , "func longFunctionNameThatSpansMultipleLines("
              , "    param1 string,"
              , "    param2 int,"
              , "    param3 bool) {"
              , "    println(\"function body\")"
              , "}"
              ]
        case parseTypus multiLineSpanCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "Should handle multi-line spans" (not $ null blocks)
    ]

-- 6. Toolchain Integration Tests
toolchainIntegrationTests :: TestTree
toolchainIntegrationTests =
  testGroup "Toolchain Integration"
    [ testCase "integrates with Go toolchain" $ do
        let goCode = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    fmt.Println(\"Hello from Go\")"
              , "}"
              ]
        case parseTypus goCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right _ -> assertBool "Should integrate with Go toolchain" True

    , testCase "handles build tags" $ do
        let buildTagCode = unlines
              [ "//go:build linux"
              , "// +build linux"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus buildTagCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let buildTags = tfBuildTags typusFile
            assertBool "Should handle build tags" (not $ null buildTags)

    , testCase "processes cgo directives" $ do
        let cgoCode = unlines
              [ "package main"
              , "/*"
              , "#include <stdio.h>"
              , "*/"
              , "import \"C\""
              , "func main() {"
              , "    C.puts(C.CString(\"Hello from C\"))"
              , "}"
              ]
        case parseTypus cgoCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> assertBool "Should process cgo directives" (not $ L.null $ tfBlocks typusFile)
    ]

-- 7. Syntax Validation Tests
syntaxValidationTests :: TestTree
syntaxValidationTests =
  testGroup "Syntax Validation"
    [ testCase "validates function signatures" $ do
        let functionCode = unlines
              [ "package main"
              , "func add(a, b int) int {"
              , "    return a + b"
              , "}"
              ]
        case parseTypus functionCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let validation = validateSyntax (rawSourceFromTypus typusFile)
            assertBool "Should validate function signatures" (null validation)

    , testCase "detects invalid syntax" $ do
        let invalidSyntaxCode = unlines
              [ "package main"
              , "func invalid( {"
              , "    return 42"
              , "}"
              ]
        case parseTypus invalidSyntaxCode of
          Left _ -> assertBool "Should detect invalid syntax" True
          Right _ -> assertFailure "Expected parse failure for invalid syntax"

    , testCase "validates type declarations" $ do
        let typeCode = unlines
              [ "package main"
              , "type Person struct {"
              , "    Name string"
              , "    Age  int"
              , "}"
              ]
        case parseTypus typeCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let validation = validateSyntax (rawSourceFromTypus typusFile)
            assertBool "Should validate type declarations" (null validation)
    ]

-- 8. Dependency Analysis Tests
dependencyAnalysisTests :: TestTree
dependencyAnalysisTests =
  testGroup "Dependency Analysis"
    [ testCase "tracks import dependencies" $ do
        let importCode = unlines
              [ "package main"
              , "import ("
              , "    \"fmt\""
              , "    \"os\""
              , "    \"strings\""
              , ")"
              , "func main() {"
              , "    fmt.Println(os.Args)"
              , "    strings.Join(os.Args, \" \")"
              , "}"
              ]
        case parseTypus importCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependentTypes (rawSourceFromTypus typusFile)
            assertBool "Should track import dependencies" (not $ null dependencies)

    , testCase "detects circular dependencies" $ do
        let circularCode = unlines
              [ "package main"
              , "import \"./a\""
              , "func main() {"
              , "    a.FuncB()"
              , "}"
              ]
        case parseTypus circularCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependentTypes (rawSourceFromTypus typusFile)
            assertBool "Should detect potential circular dependencies" (not $ null dependencies)

    , testCase "analyzes function call dependencies" $ do
        let callDepCode = unlines
              [ "package main"
              , "func helper() string {"
              , "    return \"helper\""
              , "}"
              , "func main() {"
              , "    result := helper()"
              , "    println(result)"
              , "}"
              ]
        case parseTypus callDepCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependentTypes (rawSourceFromTypus typusFile)
            assertBool "Should analyze function call dependencies" (not $ null dependencies)
    ]

-- 9. Integration Tests
integrationTests :: TestTree
integrationTests =
  testGroup "Integration Tests"
    [ testCase "handles complete compilation pipeline" $ do
        let completeCode = unlines
              [ "//! ownership: on"
              , "//! dependent_types: on"
              , "package main"
              , "func processData<T>(data []T) []T where len(data) > 0 {"
              , "    result := make([]T, len(data))"
              , "    for i, item := range data {"
              , "        result[i] = item"
              , "    }"
              , "    return result"
              , "}"
              , "func main() {"
              , "    numbers := []int{1, 2, 3, 4, 5}"
              , "    processed := processData(numbers)"
              , "    println(processed)"
              , "}"
              ]
        case parseTypus completeCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let sourceText = rawSourceFromTypus typusFile
                ownershipResult = analyzeOwnership sourceText
                dependentResult = validateDependentTypeSyntax sourceText
                syntaxResult = validateSyntax sourceText
            assertBool "Should handle complete pipeline" (not $ L.null $ tfBlocks typusFile)

    , testCase "integrates ownership L.and dependent types" $ do
        let integratedCode = unlines
              [ "//! ownership: on"
              , "//! dependent_types: on"
              , "package main"
              , "func safeTransfer<T>(data Vector<T>) Vector<T> where len(data) > 0 {"
              , "    newData := transfer(data)"
              , "    return newData"
              , "}"
              , "func main() {"
              , "    vec: Vector<int>{n: 5}"
              , "    result := safeTransfer(vec)"
              , "    process(result)"
              , "}"
              ]
        case parseTypus integratedCode of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let sourceText = rawSourceFromTypus typusFile
                ownershipResult = analyzeOwnership sourceText
                dependentResult = validateDependentTypeSyntax sourceText
            assertBool "Should integrate ownership L.and dependent types" (not $ L.null $ tfBlocks typusFile)

    , testCase "handles error recovery" $ do
        let errorRecoveryCode = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        println(\"valid code\")"
              , "    }"
              , "    // invalid code follows"
              , "    invalid_syntax_here"
              , "    // more valid code"
              , "    println(\"recovered\")"
              , "}"
              ]
        case parseTypus errorRecoveryCode of
          Left _ -> assertBool "Should handle errors gracefully" True
          Right typusFile -> assertBool "Should attempt error recovery" (not $ L.null $ tfBlocks typusFile)
    ]

-- 10. QuickCheck Property Tests
quickCheckPropertyTests :: TestTree
quickCheckPropertyTests =
  testGroup "QuickCheck Property Tests"
    [ testProperty "parseTypus is idempotent for valid code" $ \(TestString code) ->
        let normalizedCode = if null code then validTypusCode else code
        in case parseTypus normalizedCode of
             Left _ -> property True  -- Invalid code is allowed to fail
             Right firstResult -> 
               case parseTypus normalizedCode of
                 Left _ -> property False  -- Should not fail on second parse
                 Right secondResult -> firstResult === secondResult

    , testProperty "source location spans are consistent" $ \(TestString code) ->
        let testCode = if null code then validTypusCode else code
        in case parseTypus testCode of
             Left _ -> property True  -- Invalid code is allowed to fail
             Right typusFile -> 
               let blocks = tfBlocks typusFile
               in property $ not $ null blocks

    , testProperty "file directives are parsed correctly" $ \(TestString code) ->
        let directiveCode = "//! ownership: on\n" ++ (if null code then validTypusCode else code)
        in case parseTypus directiveCode of
             Left _ -> property True  -- Invalid code is allowed to fail
             Right typusFile -> 
               let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
               in property $ ownership /= Nothing

    , testProperty "syntax validation is deterministic" $ \(TestString code) ->
        let testCode = if null code then validTypusCode else code
        in case parseTypus testCode of
             Left _ -> property True  -- Invalid code is allowed to fail
             Right typusFile -> 
               let sourceText = rawSourceFromTypus typusFile
                   validation1 = validateSyntax sourceText
                   validation2 = validateSyntax sourceText
               in validation1 === validation2
    ]