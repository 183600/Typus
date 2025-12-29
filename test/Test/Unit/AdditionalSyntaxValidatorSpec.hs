module Test.Unit.AdditionalSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import SyntaxValidator
  ( SyntaxValidator
  , SyntaxError(..)
  , ErrorType(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  )
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Additional unit tests for SyntaxValidator module
tests :: TestTree
tests =
  testGroup "Additional SyntaxValidator tests"
    [ testGroup "Basic validator operations"
        [ testCase "newSyntaxValidator creates empty validator" $ do
            let validator = newSyntaxValidator
                errors = getSyntaxErrors validator
            length errors @?= 0

        , testCase "validateSyntax with valid code" $ do
            let validator = newSyntaxValidator
                validCode = "function test() { return 42; }"
                result = validateSyntax validator validCode
                errors = getSyntaxErrors result
            length errors @?= 0

        , testCase "validateSyntax with invalid code" $ do
            let validator = newSyntaxValidator
                invalidCode = "function test( { return 42; }"  -- Missing parenthesis
                result = validateSyntax validator invalidCode
                errors = getSyntaxErrors result
            assertBool "Should detect syntax error" (length errors > 0)
        ]

    , testGroup "Bracket and parenthesis validation"
        [ testCase "detects missing closing brace" $ do
            let validator = newSyntaxValidator
                code = "function test() { return 42;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasMissingBrace = any (\e -> errorType e == MissingBrace) errors
            assertBool "Should detect missing brace" hasMissingBrace

        , testCase "detects missing closing parenthesis" $ do
            let validator = newSyntaxValidator
                code = "function test( { return 42; }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasMissingParen = any (\e -> errorType e == MissingParenthesis) errors
            assertBool "Should detect missing parenthesis" hasMissingParen

        , testCase "detects missing closing bracket" $ do
            let validator = newSyntaxValidator
                code = "var arr = [1, 2, 3;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasMissingBracket = any (\e -> errorType e == MissingBracket) errors
            assertBool "Should detect missing bracket" hasMissingBracket

        , testCase "handles nested brackets correctly" $ do
            let validator = newSyntaxValidator
                code = "function test() { var arr = [1, [2, 3]]; }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "String and comment validation"
        [ testCase "detects unclosed string" $ do
            let validator = newSyntaxValidator
                code = "var message = \"Hello world;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasUnclosedString = any (\e -> errorType e == UnclosedString) errors
            assertBool "Should detect unclosed string" hasUnclosedString

        , testCase "detects unclosed comment" $ do
            let validator = newSyntaxValidator
                code = "/* This comment is not closed"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasUnclosedComment = any (\e -> errorType e == UnclosedComment) errors
            assertBool "Should detect unclosed comment" hasUnclosedComment

        , testCase "handles escaped quotes in strings" $ do
            let validator = newSyntaxValidator
                code = "var message = \"Hello \\\"world\\\"\";"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "Identifier and declaration validation"
        [ testCase "detects invalid identifiers" $ do
            let validator = newSyntaxValidator
                code = "var 123invalid = 42;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasInvalidId = any (\e -> errorType e == InvalidIdentifier) errors
            assertBool "Should detect invalid identifier" hasInvalidId

        , testCase "detects duplicate declarations" $ do
            let validator = newSyntaxValidator
                code = "var x = 1;\nvar x = 2;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasDuplicate = any (\e -> errorType e == DuplicateDeclaration) errors
            assertBool "Should detect duplicate declaration" hasDuplicate

        , testCase "validates valid identifiers" $ do
            let validator = newSyntaxValidator
                code = "var validName = 42;\nvar _private = 24;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "Function and type validation"
        [ testCase "detects invalid function declarations" $ do
            let validator = newSyntaxValidator
                code = "function 123invalid() { return 42; }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasInvalidFunc = any (\e -> errorType e == InvalidFunctionDeclaration) errors
            assertBool "Should detect invalid function declaration" hasInvalidFunc

        , testCase "detects invalid type declarations" $ do
            let validator = newSyntaxValidator
                code = "type 123Invalid = string;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasInvalidType = any (\e -> errorType e == InvalidTypeDeclaration) errors
            assertBool "Should detect invalid type declaration" hasInvalidType

        , testCase "validates correct declarations" $ do
            let validator = newSyntaxValidator
                code = "function validFunc() { return 42; }\ntype ValidType = string;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "Statement and block validation"
        [ testCase "detects invalid statements" $ do
            let validator = newSyntaxValidator
                code = "123invalid = 42;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasInvalidStmt = any (\e -> errorType e == InvalidStatement) errors
            assertBool "Should detect invalid statement" hasInvalidStmt

        , testCase "detects unterminated blocks" $ do
            let validator = newSyntaxValidator
                code = "if (true) { console.log('test');"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
                hasUnterminated = any (\e -> errorType e == UnterminatedBlock) errors
            assertBool "Should detect unterminated block" hasUnterminated

        , testCase "validates nested blocks" $ do
            let validator = newSyntaxValidator
                code = "if (true) { if (false) { console.log('nested'); } }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "Error formatting"
        [ testCase "formatSyntaxError produces meaningful output" $ do
            let pos = SourcePos 1 5 10
                span = SourceSpan pos pos
                error = SyntaxError MissingBrace span "Missing closing brace"
                formatted = formatSyntaxError error
            assertBool "Formatted error should contain error type" ("MissingBrace" `isInfixOf` formatted)
            assertBool "Formatted error should contain message" ("Missing closing brace" `isInfixOf` formatted)

        , testCase "formatSyntaxError includes location information" $ do
            let pos = SourcePos 2 10 25
                span = SourceSpan pos pos
                error = SyntaxError MissingParenthesis span "Missing closing parenthesis"
                formatted = formatSyntaxError error
            assertBool "Formatted error should include line number" ("2" `isInfixOf` formatted)
            assertBool "Formatted error should include column number" ("10" `isInfixOf` formatted)
        ]

    , testGroup "File validation"
        [ testCase "validateFile handles empty file" $ do
            let validator = newSyntaxValidator
                result = validateFile validator ""
                errors = getSyntaxErrors result
            length errors @?= 0

        , testCase "validateFile handles whitespace-only file" $ do
            let validator = newSyntaxValidator
                content = "   \n\t  \n  "
                result = validateFile validator content
                errors = getSyntaxErrors result
            length errors @?= 0

        , testCase "validateFile handles complex valid code" $ do
            let validator = newSyntaxValidator
                content = unlines
                    [ "function calculate(x, y) {"
                    , "  if (x > 0) {"
                    , "    return x + y;"
                    , "  } else {"
                    , "    return y - x;"
                    , "  }"
                    , "}"
                    ]
                result = validateFile validator content
                errors = getSyntaxErrors result
            length errors @?= 0
        ]

    , testGroup "Edge cases and complex scenarios"
        [ testCase "handles mixed bracket types" $ do
            let validator = newSyntaxValidator
                code = "function test() { var arr = [1, (2 + 3)]; }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0

        , testCase "detects multiple errors in same file" $ do
            let validator = newSyntaxValidator
                code = "function test( { var arr = [1, 2, 3;"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            assertBool "Should detect multiple errors" (length errors >= 2)

        , testCase "handles Unicode content" $ do
            let validator = newSyntaxValidator
                code = "function 测试() { return '你好世界'; }"
                result = validateSyntax validator code
                errors = getSyntaxErrors result
            length errors @?= 0
        ]
    ]
  where
    isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack