module Test.Unit.AdditionalSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.List as L
import Data.List (isInfixOf)

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
        [ testCase "validateSyntax handles empty string" $ do
            let emptyCode = ""
                errors = validateSyntax emptyCode
            L.length errors @?= 0

        , testCase "validateSyntax with valid code" $ do
            let validCode = "function test() { return 42; }"
                errors = validateSyntax validCode
            L.length errors @?= 0

        , testCase "validateSyntax with invalid code" $ do
            let invalidCode = "function test( { return 42; }"  -- Missing parenthesis
                errors = validateSyntax invalidCode
            assertBool "Should detect syntax error" (L.length errors > 0)
        ]

    , testGroup "Bracket L.and parenthesis validation"
        [ testCase "detects missing closing brace" $ do
            let code = "function test() { return 42;"
                errors = validateSyntax code
                hasMissingBrace = L.any (\e -> errorType e == MissingBrace) errors
            assertBool "Should detect missing brace" hasMissingBrace

        , testCase "detects missing closing parenthesis" $ do
            let code = "function test( { return 42; }"
                errors = validateSyntax code
                hasMissingParen = L.any (\e -> errorType e == MissingParenthesis) errors
            assertBool "Should detect missing parenthesis" hasMissingParen

        , testCase "detects missing closing bracket" $ do
            let code = "var arr = [1, 2, 3;"
                errors = validateSyntax code
                hasMissingBracket = L.any (\e -> errorType e == MissingBracket) errors
            assertBool "Should detect missing bracket" hasMissingBracket

        , testCase "handles nested brackets correctly" $ do
            let code = "function test() { var arr = [1, [2, 3]]; }"
                errors = validateSyntax code
            L.length errors @?= 0
        ]

    , testGroup "String L.and comment validation"
        [ testCase "detects unclosed string" $ do
            let code = "var message = \"Hello world;"
                errors = validateSyntax code
                hasUnclosedString = L.any (\e -> errorType e == UnclosedString) errors
            assertBool "Should detect unclosed string" hasUnclosedString

        , testCase "detects unclosed comment" $ do
            let code = "/* This comment is not closed"
                errors = validateSyntax code
                hasUnclosedComment = L.any (\e -> errorType e == UnclosedComment) errors
            assertBool "Should detect unclosed comment" hasUnclosedComment

        , testCase "handles escaped quotes in strings" $ do
            let code = "var message = \"Hello \\\"world\\\"\";"
                errors = validateSyntax code
            L.length errors @?= 0
        ]

    , testGroup "Identifier L.and declaration validation"
        [ testCase "detects invalid identifiers" $ do
            let code = "var 123invalid = 42;"
                errors = validateSyntax code
                hasInvalidId = L.any (\e -> errorType e == InvalidIdentifier) errors
            assertBool "Should detect invalid identifier" hasInvalidId

        , testCase "detects duplicate declarations" $ do
            let code = "var x = 1;\nvar x = 2;"
                errors = validateSyntax code
                hasDuplicate = L.any (\e -> errorType e == DuplicateDeclaration) errors
            assertBool "Should detect duplicate declaration" hasDuplicate

        , testCase "validates valid identifiers" $ do
            let code = "var validName = 42;\nvar _private = 24;"
                errors = validateSyntax code
            L.length errors @?= 0
        ]

    , testGroup "Function L.and type validation"
        [ testCase "detects invalid function declarations" $ do
            let code = "function 123invalid() { return 42; }"
                errors = validateSyntax code
                hasInvalidFunc = L.any (\e -> errorType e == InvalidFunctionDeclaration) errors
            assertBool "Should detect invalid function declaration" hasInvalidFunc

        , testCase "detects invalid type declarations" $ do
            let code = "type 123Invalid = string;"
                errors = validateSyntax code
                hasInvalidType = L.any (\e -> errorType e == InvalidTypeDeclaration) errors
            assertBool "Should detect invalid type declaration" hasInvalidType

        , testCase "validates correct declarations" $ do
            let code = "function validFunc() { return 42; }\ntype ValidType = string;"
                errors = validateSyntax code
            L.length errors @?= 0
        ]

    , testGroup "Statement L.and block validation"
        [ testCase "detects invalid statements" $ do
            let code = "123invalid = 42;"
                errors = validateSyntax code
                hasInvalidStmt = L.any (\e -> errorType e == InvalidStatement) errors
            assertBool "Should detect invalid statement" hasInvalidStmt

        , testCase "detects unterminated blocks" $ do
            let code = "if (true) { console.log('test');"
                errors = validateSyntax code
                hasUnterminated = L.any (\e -> errorType e == UnterminatedBlock) errors
            assertBool "Should detect unterminated block" hasUnterminated

        , testCase "validates nested blocks" $ do
            let code = "if (true) { if (false) { console.log('nested'); } }"
                errors = validateSyntax code
            L.length errors @?= 0
        ]

    , testGroup "Error formatting"
        [ testCase "formatSyntaxError produces meaningful output" $ do
            let error = SyntaxError MissingBrace "Missing closing brace" 1 5 "function test() { return 42;"
                formatted = formatSyntaxError error
            assertBool "Formatted error should contain error type" ("MissingBrace" `L.isInfixOf` formatted)
            assertBool "Formatted error should contain message" ("Missing closing brace" `L.isInfixOf` formatted)

        , testCase "formatSyntaxError includes location information" $ do
            let error = SyntaxError MissingParenthesis "Missing closing parenthesis" 2 10 "function test( { return 42; }"
                formatted = formatSyntaxError error
            assertBool "Formatted error should include line number" ("2" `L.isInfixOf` formatted)
            assertBool "Formatted error should include column number" ("10" `L.isInfixOf` formatted)
        ]

    , testGroup "File validation"
        [ testCase "validateFile handles empty file" $ do
            let errors = validateFile ""
            L.length errors @?= 0

        , testCase "validateFile handles whitespace-only file" $ do
            let content = "   \n\t  \n  "
                errors = validateFile content
            L.length errors @?= 0

        , testCase "validateFile handles complex valid code" $ do
            let content = unlines
                    [ "function calculate(x, y) {"
                    , "  if (x > 0) {"
                    , "    return x + y;"
                    , "  } else {"
                    , "    return y - x;"
                    , "  }"
                    , "}"
                    ]
                errors = validateFile content
            L.length errors @?= 0
        ]

    , testGroup "Edge cases L.and complex scenarios"
        [ testCase "handles mixed bracket types" $ do
            let code = "function test() { var arr = [1, (2 + 3)]; }"
                errors = validateSyntax code
            L.length errors @?= 0

        , testCase "detects multiple errors in same file" $ do
            let code = "function test( { var arr = [1, 2, 3;"
                errors = validateSyntax code
            assertBool "Should detect multiple errors" (L.length errors >= 2)

        , testCase "handles Unicode content" $ do
            let code = "function 测试() { return '你好世界'; }"
                errors = validateSyntax code
            L.length errors @?= 0
        ]
    ]
  where
    -- Using the imported L.isInfixOf function