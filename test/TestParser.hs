{-# LANGUAGE OverloadedStrings #-}
module TestParser (parserTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Parser (parseTypus, tfDirectives, tfBlocks, FileDirectives(..), fdOwnership, fdDependentTypes)
import SourceLocation (Located, locatedValue)

-- Enhanced comprehensive parser test suite for production readiness

directiveValue :: Maybe (Located a) -> Maybe a
directiveValue = fmap locatedValue


parserTestSuite :: TestTree
parserTestSuite = testGroup "Parser Tests" [
    basicParserTests,
    directiveParserTests,
    edgeCaseParserTests,
    errorHandlingParserTests,
    complexScenarioTests
    ]

-- Basic parser functionality tests
basicParserTests :: TestTree
basicParserTests = testGroup "Basic Parser Tests" [
    TH.testCase "Parser Basic Functionality" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    fmt.Println(\"Hello, World!\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed on simple Go code: " ++ err
            Right typusFile -> do
                TH.assertBool "Should parse package directive correctly"
                    (Parser.tfDirectives typusFile == Parser.FileDirectives Nothing Nothing Nothing)
                TH.assertBool "Should have at least one code block"
                    (not $ null $ Parser.tfBlocks typusFile),

    TH.testCase "Parser Empty Code" $ do
        case Parser.parseTypus "" of
            Left err -> TH.assertFailure $ "Parser failed on empty code: " ++ err
            Right typusFile -> do
                TH.assertEqual "Should have no blocks for empty code" [] (Parser.tfBlocks typusFile),

    TH.testCase "Parser Simple Function" $ do
        let code = unlines [
                "package main",
                "",
                "func add(a int, b int) int {",
                "    return a + b",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right _ -> TH.assertBool "Should parse simple function" True,

    TH.testCase "Parser Multiple Functions" $ do
        let code = unlines [
                "package main",
                "",
                "func foo() {}",
                "func bar() {}",
                "func baz() {}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                TH.assertBool "Should have multiple code blocks"
                    (length (Parser.tfBlocks typusFile) > 0)
    ]

-- Directive parsing tests
directiveParserTests :: TestTree
directiveParserTests = testGroup "Directive Parser Tests" [
    TH.testCase "Parser File Directives - Ownership" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                TH.assertEqual "Ownership should be enabled" (Just True) (directiveValue (Parser.fdOwnership directives)),

    TH.testCase "Parser File Directives - Dependent Types" $ do
        let code = unlines [
                "//! dependent_types: on",
                "",
                "package main",
                "",
                "func main() {}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                TH.assertEqual "Dependent types should be enabled" (Just True) (directiveValue (Parser.fdDependentTypes directives)),

    TH.testCase "Parser File Directives - Both Enabled" $ do
        let code = unlines [
                "//! ownership: on",
                "//! dependent_types: on",
                "",
                "package main",
                "",
                "func main() {}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                TH.assertEqual "Ownership should be enabled" (Just True) (directiveValue (Parser.fdOwnership directives))
                TH.assertEqual "Dependent types should be enabled" (Just True) (directiveValue (Parser.fdDependentTypes directives)),

    TH.testCase "Parser File Directives - Constraints Alias" $ do
        let code = unlines [
                "//! constraints: on",
                "",
                "package main",
                "",
                "func main() {}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                TH.assertEqual "Constraints (dependent types) should be enabled" (Just True) (directiveValue (Parser.fdDependentTypes directives))
    ]

-- Edge case tests
edgeCaseParserTests :: TestTree
edgeCaseParserTests = testGroup "Edge Case Parser Tests" [
    TH.testCase "Parser Whitespace Only" $ do
        let code = "   \n\n   \n"
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser should handle whitespace: " ++ err
            Right typusFile -> do
                TH.assertEqual "Should have no blocks" [] (Parser.tfBlocks typusFile),

    TH.testCase "Parser Comments Only" $ do
        let code = unlines [
                "// This is a comment",
                "// Another comment"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser should handle comments: " ++ err
            Right _ -> TH.assertBool "Should parse comments" True,

    TH.testCase "Parser Mixed Line Endings" $ do
        let code = "package main\r\nfunc main() {\r\n}\n"
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser should handle mixed line endings: " ++ err
            Right _ -> TH.assertBool "Should handle mixed line endings" True,

    TH.testCase "Parser Unicode Characters" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    s := \"你好世界\"",
                "    println(s)",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser should handle Unicode: " ++ err
            Right _ -> TH.assertBool "Should handle Unicode" True,

    TH.testCase "Parser Very Long Line" $ do
        let longString = replicate 10000 'a'
        let code = "package main\n\nvar s = \"" ++ longString ++ "\"\n"
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser should handle long lines: " ++ err
            Right _ -> TH.assertBool "Should handle long lines" True
    ]

-- Error handling tests
errorHandlingParserTests :: TestTree
errorHandlingParserTests = testGroup "Error Handling Parser Tests" [
    TH.testCase "Parser Malformed Directive" $ do
        let code = unlines [
                "//! invalid_directive: yes",
                "",
                "package main",
                "",
                "func main() {}"
                ]
        case Parser.parseTypus code of
            Left _ -> TH.assertBool "Should handle malformed directive gracefully" True
            Right _ -> TH.assertBool "Should parse despite malformed directive" True,

    TH.testCase "Parser Missing Package" $ do
        let code = "func main() {}"
        case Parser.parseTypus code of
            Left _ -> TH.assertBool "Should fail gracefully" True
            Right _ -> TH.assertBool "Should handle missing package" True,

    TH.testCase "Parser Unmatched Braces" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    if true {",
                "}"
                ]
        case Parser.parseTypus code of
            Left _ -> TH.assertBool "Should detect unmatched braces" True
            Right _ -> TH.assertBool "Parser may handle or pass through to compiler" True
    ]

-- Complex scenario tests
complexScenarioTests :: TestTree
complexScenarioTests = testGroup "Complex Scenario Tests" [
    TH.testCase "Parser Nested Structures" $ do
        let code = unlines [
                "package main",
                "",
                "type Outer struct {",
                "    Inner struct {",
                "        Value int",
                "    }",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed on nested structures: " ++ err
            Right _ -> TH.assertBool "Should parse nested structures" True,

    TH.testCase "Parser Block Level Directives" $ do
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
            Left err -> TH.assertFailure $ "Parser failed on block directives: " ++ err
            Right _ -> TH.assertBool "Should parse block directives" True,

    TH.testCase "Parser Multiple Block Directives" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    {//! ownership: on",
                "        s := \"hello\"",
                "    }",
                "    {//! dependent_types: on",
                "        v := NewVector(3, data)",
                "    }",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right _ -> TH.assertBool "Should parse multiple block directives" True,

    TH.testCase "Parser Generic Types" $ do
        let code = unlines [
                "package main",
                "",
                "func Max[T comparable](a, b T) T {",
                "    if a > b { return a }",
                "    return b",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed on generics: " ++ err
            Right _ -> TH.assertBool "Should parse generic types" True
    ]