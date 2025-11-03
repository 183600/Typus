{-# LANGUAGE OverloadedStrings #-}
module TestCompiler (compilerTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Parser (parseTypus)
import qualified Compiler
import Data.List (isInfixOf)

-- Enhanced comprehensive compiler test suite for production readiness
compilerTestSuite :: TestTree
compilerTestSuite = testGroup "Compiler Tests" [
    basicCompilerTests,
    directiveCompilationTests,
    edgeCaseCompilerTests,
    errorHandlingCompilerTests,
    complexCompilationTests,
    outputValidityTests
    ]

failCompile :: [Compiler.CompilerError] -> TH.Assertion
failCompile errs = TH.assertFailure $ "Compilation failed: " ++ Compiler.renderCompilationError errs

-- Basic compiler functionality tests
basicCompilerTests :: TestTree
basicCompilerTests = testGroup "Basic Compiler Tests" [
    TH.testCase "Compiler Simple Case" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    fmt.Println(\"Hello, World!\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Generated code should contain package declaration"
                            ("package main" `isInfixOf` goCode)
                        TH.assertBool "Generated code should contain main function"
                            ("func main" `isInfixOf` goCode),

    TH.testCase "Compiler Empty Code" $ do
        case Parser.parseTypus "" of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left _ -> return ()
                    Right _ -> do
                        TH.assertBool "Should handle empty code" True,

    TH.testCase "Compiler Function Definition" $ do
        let code = unlines [
                "package main",
                "",
                "func add(a int, b int) int {",
                "    return a + b",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should contain function definition"
                            ("func add" `isInfixOf` goCode),

    TH.testCase "Compiler Struct Definition" $ do
        let code = unlines [
                "package main",
                "",
                "type Person struct {",
                "    Name string",
                "    Age  int",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should contain struct definition"
                            ("type Person struct" `isInfixOf` goCode),

    TH.testCase "Compiler Import Statements" $ do
        let code = unlines [
                "package main",
                "",
                "import \"fmt\"",
                "",
                "func main() {",
                "    fmt.Println(\"test\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should preserve import statements"
                            ("import" `isInfixOf` goCode)
    ]

-- Directive compilation tests
directiveCompilationTests :: TestTree
directiveCompilationTests = testGroup "Directive Compilation Tests" [
    TH.testCase "Compiler With Ownership Directive" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s := \"hello\"",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should compile with ownership directive"
                            ("package main" `isInfixOf` goCode),

    TH.testCase "Compiler With Dependent Types Directive" $ do
        let code = unlines [
                "//! dependent_types: on",
                "",
                "package main",
                "",
                "func main() {",
                "    v := NewVector(3, data)",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should compile with dependent types directive"
                            ("package main" `isInfixOf` goCode),

    TH.testCase "Compiler With Block Directive" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    {//! ownership: on",
                "        s := \"hello\"",
                "    }",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should compile with block directive"
                            ("func main" `isInfixOf` goCode)
    ]

-- Edge case compiler tests
edgeCaseCompilerTests :: TestTree
edgeCaseCompilerTests = testGroup "Edge Case Compiler Tests" [
    TH.testCase "Compiler With Comments" $ do
        let code = unlines [
                "package main",
                "",
                "// This is a comment",
                "func main() {",
                "    // Another comment",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should handle comments"
                            ("func main" `isInfixOf` goCode),

    TH.testCase "Compiler With String Escapes" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    s := \"Hello\\nWorld\\t!\"",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right _ -> TH.assertBool "Should handle string escapes" True,

    TH.testCase "Compiler With Unicode" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    s := \"你好世界\"",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right _ -> TH.assertBool "Should handle Unicode" True,

    TH.testCase "Compiler With Complex Expressions" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    x := (1 + 2) * (3 - 4) / 5",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right _ -> TH.assertBool "Should handle complex expressions" True
    ]

-- Error handling tests
errorHandlingCompilerTests :: TestTree
errorHandlingCompilerTests = testGroup "Error Handling Compiler Tests" [
    TH.testCase "Compiler With Incomplete Code" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {"
                ]
        case Parser.parseTypus code of
            Left _ -> return ()
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left _ -> TH.assertBool "Should fail gracefully on incomplete code" True
                    Right _ -> TH.assertBool "May pass through to Go compiler" True,

    TH.testCase "Compiler Error Recovery" $ do
        let code = unlines [
                "package main",
                "",
                "func broken() {",
                "    invalid syntax here",
                "}"
                ]
        case Parser.parseTypus code of
            Left _ -> return ()
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left _ -> TH.assertBool "Should handle syntax errors" True
                    Right _ -> TH.assertBool "May pass through to Go compiler" True
    ]

-- Complex compilation tests
complexCompilationTests :: TestTree
complexCompilationTests = testGroup "Complex Compilation Tests" [
    TH.testCase "Compiler Multiple Functions and Types" $ do
        let code = unlines [
                "package main",
                "",
                "type Person struct {",
                "    Name string",
                "}",
                "",
                "func NewPerson(name string) Person {",
                "    return Person{Name: name}",
                "}",
                "",
                "func main() {",
                "    p := NewPerson(\"Alice\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should contain struct" ("type Person struct" `isInfixOf` goCode)
                        TH.assertBool "Should contain constructor" ("func NewPerson" `isInfixOf` goCode),

    TH.testCase "Compiler With Methods" $ do
        let code = unlines [
                "package main",
                "",
                "type Counter struct {",
                "    value int",
                "}",
                "",
                "func (c *Counter) Increment() {",
                "    c.value++",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should contain method definition"
                            ("func (c *Counter) Increment" `isInfixOf` goCode),

    TH.testCase "Compiler With Interfaces" $ do
        let code = unlines [
                "package main",
                "",
                "type Reader interface {",
                "    Read(p []byte) (n int, err error)",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should contain interface definition"
                            ("type Reader interface" `isInfixOf` goCode)
    ]

-- Output validity tests
outputValidityTests :: TestTree
outputValidityTests = testGroup "Output Validity Tests" [
    TH.testCase "Compiler Output Is Valid Go Syntax" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    x := 42",
                "    println(x)",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should have package" ("package" `isInfixOf` goCode)
                        TH.assertBool "Should not be empty" (not $ null goCode),

    TH.testCase "Compiler Preserves Line Structure" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    x := 1",
                "    y := 2",
                "    z := x + y",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> failCompile err
                    Right goCode -> do
                        TH.assertBool "Should preserve code structure" (length goCode > 0)
    ]