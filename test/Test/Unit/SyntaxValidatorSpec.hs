module Test.Unit.SyntaxValidatorSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import SyntaxValidator
  ( ErrorType(..)
  , SyntaxError(..)
  , formatSyntaxError
  , validateFile
  , validateSyntax
  )

-- | High-signal regression tests that exercise the public SyntaxValidator API.
tests :: TestTree
tests =
  testGroup "SyntaxValidator"
    [ testCase "returns no errors for an empty file" $ do
        validateSyntax "" @?= []

    , testCase "detects missing package declarations in Go files" $ do
        let source = unlines
              [ "func main() {"
              , "    println(\"hello\")"
              , "}"
              ]
            errors = validateSyntax source
        map errorType errors @?= [MissingPackageDeclaration]
        case errors of
          [SyntaxError { lineNumber = line', columnNumber = column', errorMessage = msg }] -> do
            line' @?= 1
            column' @?= 1
            msg @?= "Go file missing package declaration"
          _ -> assertFailure "expected exactly one missing-package error"

    , testCase "reports unclosed braces with the original opening location" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hi\")"
              ]
            errors = validateSyntax source
        map errorType errors @?= [MissingBrace]
        case errors of
          [SyntaxError { lineNumber = line', columnNumber = column' }] -> do
            line' @?= 2
            column' @?= 13
          _ -> assertFailure "expected exactly one missing-brace error"

    , testCase "flags malformed Typus directives" $ do
        let source = unlines
              [ "package main"
              , "//! ownership on"
              , "func main() {}"
              ]
            errors = validateSyntax source
        map errorType errors @?= [InvalidStatement]
        case errors of
          [SyntaxError { lineNumber = line', columnNumber = column', errorMessage = msg }] -> do
            line' @?= 2
            column' @?= 1
            assertBool "error message should reference the directive" ("directive" `isInfixOf` msg)
          _ -> assertFailure "expected exactly one directive validation error"

    , testCase "detects duplicate variable declarations in the same scope" $ do
        let source = unlines
              [ "package main"
              , "var answer int"
              , "var answer string"
              , "func main() {}"
              ]
            errors = validateSyntax source
        map errorType errors @?= [DuplicateDeclaration]
        case errors of
          [SyntaxError { lineNumber = line', columnNumber = column', errorMessage = msg }] -> do
            line' @?= 3
            column' @?= 1
            assertBool "duplicate error should mention the identifier" ("answer" `isInfixOf` msg)
          _ -> assertFailure "expected a single duplicate declaration error"

    , testCase "requires braces after control-flow keywords" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true"
              , "        println(\"oops\")"
              , "}"
              ]
            errors = validateSyntax source
        map errorType errors @?= [UnterminatedBlock]
        case errors of
          [SyntaxError { lineNumber = line', columnNumber = column' }] -> do
            line' @?= 3
            column' @?= 5
          _ -> assertFailure "expected a single unterminated block error"

    , testCase "validateFile delegates to validateSyntax" $ do
        let snippet = "func main() {}"
        validateFile snippet @?= validateSyntax snippet

    , testCase "ignores import formatting errors that are filtered as false positives" $ do
        let source = unlines
              [ "package main"
              , "import fmt"
              , "func main() {"
              , "    fmt.Println(\"hi\")"
              , "}"
              ]
        validateSyntax source @?= []

    , testCase "successfully validates a minimal, well-formed Go file" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"ok\")"
              , "}"
              ]
        validateSyntax source @?= []

    , testCase "formatSyntaxError includes the location prefix and context line" $ do
        let err = SyntaxError MissingBrace "unclosed block" 4 2 "    body"
            rendered = formatSyntaxError err
        rendered @?= "Line 4:2 [MissingBrace] unclosed block\n        body"
    ]
