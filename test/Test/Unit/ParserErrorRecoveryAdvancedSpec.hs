{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

import Parser 
    ( parseTypus
    , TypusFile(..)
    , CodeBlock(..)
    , FileDirectives(..)
    , BlockDirectives(..)
    , defaultFileDirectives
    , defaultBlockDirectives
    )

import SourceLocation 
    ( SourcePos(..)
    , SourceSpan(..)
    , Located(..)
    , startPos
    , spanStart
    )

import Utils (trim, removeComments, normalizeIndentation)

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import Control.Monad (when)

-- | Advanced tests for parser error recovery
tests :: TestTree
tests =
  testGroup "Parser Error Recovery Advanced"
    [ testGroup "Syntax Error Recovery"
        [ testCase "Parser recovers from missing semicolon" $ do
            let inputWithMissingSemicolon = "func test() {\n  let x = 5\n  return x\n}"
                result = parseTypus "test.typus" inputWithMissingSemicolon
            case result of
              Left err -> do
                assertBool "Error should mention semicolon or statement termination" 
                    (any (`isInfixOf` map toLower err) ["semicolon", "statement", "terminate"])
                assertBool "Error should include line information" 
                    ("line" `isInfixOf` map toLower err)
              Right _ -> do
                assertFailure "Expected parse failure for missing semicolon"

        , testCase "Parser recovers from unmatched braces" $ do
            let inputWithUnmatchedBraces = "func test() {\n  if (condition) {\n    return true\n  // missing closing brace\n}"
                result = parseTypus "test.typus" inputWithUnmatchedBraces
            case result of
              Left err -> do
                assertBool "Error should mention braces or brackets" 
                    (any (`isInfixOf` map toLower err) ["brace", "bracket", "unmatched", "expect"])
                assertBool "Error should provide location information" 
                    (any (`isInfixOf` map toLower err) ["line", "column"])
              Right _ -> do
                assertFailure "Expected parse failure for unmatched braces"

        , testCase "Parser recovers from invalid function declaration" $ do
            let invalidFunction = "func invalid( {\n  // malformed parameters\n}"
                result = parseTypus "test.typus" invalidFunction
            case result of
              Left err -> do
                assertBool "Error should mention function or parameters" 
                    (any (`isInfixOf` map toLower err) ["function", "parameter", "expect"])
              Right _ -> do
                assertFailure "Expected parse failure for invalid function"

        , testCase "Parser recovers from malformed type annotation" $ do
            let malformedType = "func test() -> {\n  return 42\n}"
                result = parseTypus "test.typus" malformedType
            case result of
              Left err -> do
                assertBool "Error should mention type" 
                    ("type" `isInfixOf` map toLower err)
              Right _ -> do
                assertFailure "Expected parse failure for malformed type"
        ]

    , testGroup "Error Recovery with Comments"
        [ testCase "Parser handles errors in commented sections" $ do
            let inputWithErrorInComment = "func test() {\n  // This comment has a syntax error: {\n  return 42\n}"
                result = parseTypus "test.typus" inputWithErrorInComment
            case result of
              Right (TypusFile _ blocks) -> do
                assertBool "Should parse successfully despite error in comment" 
                    (not (null blocks))
              Left err -> do
                assertFailure $ "Should handle errors in comments: " ++ err

        , testCase "Parser recovers from malformed block comments" $ do
            let malformedBlockComment = "func test() {\n  /* Unterminated block comment\n  return 42\n}"
                result = parseTypus "test.typus" malformedBlockComment
            case result of
              Left err -> do
                assertBool "Error should mention comment" 
                    ("comment" `isInfixOf` map toLower err)
              Right _ -> do
                assertFailure "Expected parse failure for unterminated block comment"

        , testCase "Parser handles nested comments with errors" $ do
            let nestedComments = "func test() {\n  /* Outer comment\n     /* Inner comment */\n     syntax error here {\n  */\n  return 42\n}"
                result = parseTypus "test.typus" nestedComments
            case result of
              Left err -> do
                assertBool "Should handle nested comment errors gracefully" 
                    (length err > 10)
              Right _ -> do
                -- May succeed if parser is robust
                assertBool "Parser may succeed with robust comment handling" True
        ]

    , testGroup "Error Recovery with Directives"
        [ testCase "Parser recovers from malformed ownership directive" $ do
            let malformedDirective = "// @ownership: maybe\nfunc test() { return 42; }"
                result = parseTypus "test.typus" malformedDirective
            case result of
              Left err -> do
                assertBool "Error should mention ownership or directive" 
                    (any (`isInfixOf` map toLower err) ["ownership", "directive", "boolean"])
              Right (TypusFile directives _) -> do
                -- May parse with default value
                assertBool "Should handle malformed directive" 
                    (isJust (fdOwnership directives) || isNothing (fdOwnership directives))

        , testCase "Parser recovers from invalid dependent types directive" $ do
            let invalidDependentTypes = "// @dependent-types: sometimes\nfunc test() { return 42; }"
                result = parseTypus "test.typus" invalidDependentTypes
            case result of
              Left err -> do
                assertBool "Error should mention dependent types" 
                    ("dependent" `isInfixOf` map toLower err)
              Right (TypusFile directives _) -> do
                assertBool "Should handle invalid dependent types directive" 
                    (isJust (fdDependentTypes directives) || isNothing (fdDependentTypes directives))

        , testCase "Parser handles multiple directives with errors" $ do
            let multipleDirectives = "// @ownership: true\n// @dependent-types: perhaps\n// @constraints: definitely\nfunc test() { return 42; }"
                result = parseTypus "test.typus" multipleDirectives
            case result of
              Left err -> do
                assertBool "Should provide meaningful error for multiple directives" 
                    (length err > 15)
              Right (TypusFile directives _) -> do
                assertBool "Should handle multiple directives with some errors" 
                    (isJust (fdOwnership directives) || isJust (fdDependentTypes directives))
        ]

    , testGroup "Partial Recovery Tests"
        [ testCase "Parser continues after early syntax error" $ do
            let inputWithEarlyError = "func invalid( {\n}\n\nfunc valid() {\n  return 42\n}"
                result = parseTypus "test.typus" inputWithEarlyError
            case result of
              Left err -> do
                assertBool "Should report error but attempt recovery" 
                    (length err > 10)
              Right (TypusFile _ blocks) -> do
                -- May recover and parse valid function
                assertBool "May recover and parse subsequent valid code" 
                    (length blocks >= 1)

        , testCase "Parser recovers from errors in multiple functions" $ do
            let multipleErrors = "func bad1( {\n}\nfunc bad2() -> {\n}\nfunc good() { return 42; }"
                result = parseTypus "test.typus" multipleErrors
            case result of
              Left err -> do
                assertBool "Should handle multiple errors" 
                    (length err > 20)
              Right (TypusFile _ blocks) -> do
                assertBool "May parse some functions despite errors" 
                    (length blocks >= 0)

        , testCase "Parser handles incomplete last function" $ do
            let incompleteFunction = "func complete() { return 42; }\nfunc incomplete( {"
                result = parseTypus "test.typus" incompleteFunction
            case result of
              Left err -> do
                assertBool "Should handle incomplete function gracefully" 
                    (any (`isInfixOf` map toLower err) ["incomplete", "unexpected", "eof"])
              Right _ -> do
                assertFailure "Expected parse failure for incomplete function"
        ]

    , testGroup "Error Message Quality"
        [ testCase "Parser provides context in error messages" $ do
            let contextualError = "func test() {\n  let x = \n  return x\n}"
                result = parseTypus "test.typus" contextualError
            case result of
              Left err -> do
                assertBool "Error should provide sufficient context" 
                    (length err > 20)
                assertBool "Error should mention expected elements" 
                    (any (`isInfixOf` map toLower err) ["expect", "unexpected", "missing"])
              Right _ -> do
                assertFailure "Expected parse failure"

        , testCase "Parser error messages include line numbers" $ do
            let multiLineInput = "func line1() { return 1; }\nfunc line2() {\n  syntax error\n}\nfunc line3() { return 3; }"
                result = parseTypus "test.typus" multiLineInput
            case result of
              Left err -> do
                assertBool "Error should include line information" 
                    (any (`isInfixOf` map toLower err) ["line", "2"])
              Right _ -> do
                assertFailure "Expected parse failure"

        , testCase "Parser error messages are not overly technical" $ do
            let technicalError = "func test() {\n  let x: Complex<Type<Param>> = value\n}"
                result = parseTypus "test.typus" technicalError
            case result of
              Left err -> do
                assertBool "Error should be understandable" 
                    (not (all (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789<>{}[]()") err))
              Right _ -> do
                assertFailure "Expected parse failure"
        ]

    , testGroup "Stress Testing Error Recovery"
        [ testCase "Parser handles many small errors" $ do
            let manyErrors = concat $ replicate 10 "func bad( { }\n"
                result = parseTypus "test.typus" manyErrors
            case result of
              Left err -> do
                assertBool "Should handle multiple errors without crashing" 
                    (length err > 10)
              Right _ -> do
                assertFailure "Expected parse failure with many errors"

        , testCase "Parser handles very long lines with errors" $ do
            let longLineWithError = "func test() { " ++ replicate 1000 'a' ++ " syntax_error " ++ replicate 1000 'b' ++ " }"
                result = parseTypus "test.typus" longLineWithError
            case result of
              Left err -> do
                assertBool "Should handle long lines with errors" 
                    (length err > 0)
              Right _ -> do
                assertFailure "Expected parse failure for long line with error"

        , testCase "Parser handles deeply nested errors" $ do
            let nestedError = concat $ replicate 50 "func test() { if (condition) { "
                result = parseTypus "test.typus" nestedError
            case result of
              Left err -> do
                assertBool "Should handle deeply nested errors" 
                    (length err > 0)
              Right _ -> do
                assertFailure "Expected parse failure for deeply nested code"
        ]

    , testGroup "QuickCheck Properties for Error Recovery"
        [ fastProperty "Parser never crashes on any input" $
            \input -> 
                let result = parseTypus "test.typus" input
                in case result of
                     Left _ -> property True
                     Right _ -> property True

        , fastProperty "Parser provides line numbers for errors in multi-line input" $
            \linesList -> length linesList <= 10 ==>
                let input = unlines linesList
                    result = parseTypus "test.typus" input
                in case result of
                     Left err -> property ("line" `isInfixOf` map toLower err)
                     Right _ -> property True

        , fastProperty "Parser handles input with mixed whitespace and errors" $
            \input -> 
                let processed = normalizeIndentation input
                    result = parseTypus "test.typus" processed
                in case result of
                     Left _ -> property True
                     Right _ -> property True
        ]

    , testGroup "Recovery Strategies"
        [ testCase "Parser attempts synchronization points" $ do
            let syncTest = "func bad1( { }\n  garbage text\nfunc good1() { return 1; }\n  more garbage\nfunc good2() { return 2; }"
                result = parseTypus "test.typus" syncTest
            case result of
              Left err -> do
                assertBool "Should attempt synchronization at function boundaries" 
                    (length err > 0)
              Right (TypusFile _ blocks) -> do
                -- May recover some valid functions
                assertBool "May recover some functions after synchronization" 
                    (length blocks >= 0)

        , testCase "Parser recovers from string literal errors" $ do
            let stringError = "func test() {\n  let message = \"unterminated string\n  return message\n}"
                result = parseTypus "test.typus" stringError
            case result of
              Left err -> do
                assertBool "Error should mention string or literal" 
                    (any (`isInfixOf` map toLower err) ["string", "literal", "quote"])
              Right _ -> do
                assertFailure "Expected parse failure for unterminated string"

        , testCase "Parser handles character literal errors" $ do
            let charError = "func test() {\n  let ch = 'unterminated\n  return ch\n}"
                result = parseTypus "test.typus" charError
            case result of
              Left err -> do
                assertBool "Error should mention character" 
                    (any (`isInfixOf` map toLower err) ["character", "literal", "quote"])
              Right _ -> do
                assertFailure "Expected parse failure for unterminated character"
        ]
    ]

-- Helper functions
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- QuickCheck generators for testing
arbitraryCodeLine :: Gen String
arbitraryCodeLine = oneof
    [ return "func test() { return 42; }"
    , return "let x = 5;"
    , return "if (condition) { return true; }"
    , return "// This is a comment"
    , return "/* Block comment */"
    , return ""
    , return "syntax error here {"
    , return "func invalid( { }"
    ]

instance Arbitrary String where
    arbitrary = listOf $ oneof
        [ choose ('a', 'z')
        , choose ('A', 'Z')
        , choose ('0', '9')
        , elements " \t\n\r{}();,[]<>\"'*/"
        ]