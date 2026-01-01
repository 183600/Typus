module Test.Unit.CoreSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, elements, listOf)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import SimpleSyntaxValidator

-- | Core functionality tests for SimpleSyntaxValidator module
tests :: TestTree
tests =
  testGroup "Core SyntaxValidator Tests"
    [ testGroup "Basic syntax validation"
        [ testCase "validateSyntaxSimple handles empty input" $ do
            let errors = validateSyntaxSimple ""
            null errors @?= True

        , testCase "validateSyntaxSimple detects missing braces" $ do
            let content = "func main() {\n  fmt.Println(\"hello\")\n"  -- missing closing brace
                errors = validateSyntaxSimple content
            assertBool "should detect missing brace" $ L.any (\e -> errorType e == MissingBrace) errors

        , testCase "validateSyntaxSimple detects missing parentheses" $ do
            let content = "func main( {\n  fmt.Println(\"hello\")\n}"  -- missing closing parenthesis
                errors = validateSyntaxSimple content
            assertBool "should detect missing parenthesis" $ L.any (\e -> errorType e == MissingParenthesis) errors

        , testCase "validateSyntaxSimple detects missing brackets" $ do
            let content = "arr := [1, 2, 3\n  fmt.Println(arr)"  -- missing closing bracket
                errors = validateSyntaxSimple content
            assertBool "should detect missing bracket" $ L.any (\e -> errorType e == MissingBracket) errors
        ]

    , testGroup "Go structure validation"
        [ testCase "validates package declarations" $ do
            let valid = "package main\n"
                invalid1 = "package\n"  -- missing name
                invalid2 = "package main extra\n"  -- extra content
            validateSyntaxSimple valid @?= []
            assertBool "should detect missing package name" $ 
                L.any (\e -> errorType e == MissingPackageDeclaration) (validateSyntaxSimple invalid1)
            assertBool "should detect invalid package declaration" $ 
                L.any (\e -> errorType e == InvalidStatement) (validateSyntaxSimple invalid2)

        , testCase "validates import statements" $ do
            let valid1 = "import \"fmt\"\n"
                valid2 = "import (\n  \"fmt\"\n  \"os\"\n)\n"
                invalid = "import fmt\n"  -- missing quotes
            validateSyntaxSimple valid1 @?= []
            validateSyntaxSimple valid2 @?= []
            assertBool "should detect invalid import" $ 
                L.any (\e -> errorType e == InvalidImport) (validateSyntaxSimple invalid)

        , testCase "validates function declarations" $ do
            let valid = "func main() {\n}\n"
                invalid = "func main\n{\n}\n"  -- missing parameters
            validateSyntaxSimple valid @?= []
            assertBool "should detect invalid function declaration" $ 
                L.any (\e -> errorType e == InvalidFunctionDeclaration) (validateSyntaxSimple invalid)

        , testCase "validates variable declarations" $ do
            let valid1 = "var x int = 5\n"
                valid2 = "var (\n  x int = 5\n  y string = \"hello\"\n)\n"
                invalid = "var x\n"  -- incomplete declaration
            validateSyntaxSimple valid1 @?= []
            validateSyntaxSimple valid2 @?= []
            assertBool "should detect invalid variable declaration" $ 
                L.any (\e -> errorType e == InvalidStatement) (validateSyntaxSimple invalid)

        , testCase "validates type declarations" $ do
            let valid = "type MyInt int\n"
                invalid = "type MyInt\n"  -- incomplete declaration
            validateSyntaxSimple valid @?= []
            assertBool "should detect invalid type declaration" $ 
                L.any (\e -> errorType e == InvalidTypeDeclaration) (validateSyntaxSimple invalid)
        ]

    , testGroup "String L.and comment handling"
        [ testCase "validates strings with quotes correctly" $ do
            let content = "text := \"hello // not a comment\"\nvar x := \"/* not block comment */\"\n"
                errors = validateSyntaxSimple content
            assertBool "should not flag comment markers in strings" $ 
                L.all (\e -> not (InvalidStatement `elem` [errorType e])) errors

        , testCase "validates character literals correctly" $ do
            let content = "char := '/' // this is a comment\n"
                errors = validateSyntaxSimple content
            assertBool "should handle character literals correctly" $ 
                null errors

        , testCase "handles multi-line strings correctly" $ do
            let content = "text := `multi-line\nstring with // not comment`\n"
                errors = validateSyntaxSimple content
            null errors @?= True
        ]

    , testGroup "Bracket counting"
        [ testCase "countBraces returns zero for balanced code" $ do
            let content = "func main() {\n  if true {\n    fmt.Println(\"hello\")\n  }\n}\n"
            countBraces content @?= 0

        , testCase "countBraces handles nested structures" $ do
            let content = "func outer() {\n  func inner() {\n    // nested\n  }\n}\n"
            countBraces content @?= 0

        , testCase "countBraces detects imbalance" $ do
            let content1 = "func main() {\n  fmt.Println(\"hello\")\n"  -- missing closing
                content2 = "fmt.Println(\"hello\")\n}\n"  -- extra closing
            countBraces content1 @?= 1
            countBraces content2 @?= (-1)

        , testProperty "countBraces is additive for concatenated code" $
            \code1 code2 -> countBraces (code1 ++ code2) == countBraces code1 + countBraces code2
        ]

    , testGroup "Error reporting"
        [ testCase "syntax errors include correct location information" $ do
            let content = "func main( {\n  fmt.Println(\"hello\")\n}"
                errors = validateSyntaxSimple content
                missingParenthesis = L.filter (\e -> errorType e == MissingParenthesis) errors
            assertBool "should have missing parenthesis error" $ not (null missingParenthesis)
            let err = L.head missingParenthesis
            lineNumber err @?= 1
            columnNumber err @?= 11  -- position of opening parenthesis

        , testCase "syntax errors include descriptive messages" $ do
            let content = "func main() {\n  fmt.Println(\"hello\")\n"
                errors = validateSyntaxSimple content
                missingBrace = L.filter (\e -> errorType e == MissingBrace) errors
            assertBool "should have missing brace error" $ not (null missingBrace)
            let err = L.head missingBrace
            assertBool "message should be descriptive" $ 
                "Unclosed brace" `L.isInfixOf` message err
        ]

    , testGroup "Edge cases L.and robustness"
        [ testCase "handles very long lines" $ do
            let longLine = "func main() { " ++ replicate 1000 'a' ++ " }\n"
                errors = validateSyntaxSimple longLine
            null errors @?= True

        , testCase "handles empty lines L.and whitespace" $ do
            let content = "\n  \n\t\nfunc main() {\n}\n  \n"
                errors = validateSyntaxSimple content
            null errors @?= True

        , testCase "handles Unicode characters" $ do
            let content = "func main() {\n  fmt.Println(\"你好世界\")\n}\n"
                errors = validateSyntaxSimple content
            null errors @?= True

        , testCase "handles deeply nested brackets" $ do
            let content = "func main() {\n  if true {\n    for {\n      switch {\n      case true:\n        // deep nesting\n      }\n    }\n  }\n}\n"
                errors = validateSyntaxSimple content
            null errors @?= True
        ]

    , testGroup "Property-based tests"
        [ testProperty "balanced code has zero brace count" $
            \code -> countBraces code == 0 ==> L.null (L.filter (\e -> errorType e == MissingBrace) (validateSyntaxSimple code))

        , testProperty "valid package declaration passes validation" $
            \name -> not (null name) ==> 
                let content = "package " ++ name ++ "\n"
                    errors = validateSyntaxSimple content
                in null [e | e <- errors, errorType e == MissingPackageDeclaration]

        , testProperty "empty content has no validation errors" $
            \code -> null code ==> L.null (validateSyntaxSimple code)
        ]
    ]