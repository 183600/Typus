{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import qualified Data.Text as T
import Data.List (isInfixOf)

import Compiler (CompilerError(..), CompilationPhase(..), renderCompilationError, formatCompilerErrors)
import ErrorHandler (ErrorHandler, handleErrors, ErrorContext(..))
import EnhancedErrorHandler (EnhancedErrorHandler, createEnhancedHandler, processBatchErrors)
import Parser (parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- | Comprehensive error handling tests covering multiple error types and scenarios
tests :: TestTree
tests = testGroup "Comprehensive Error Handling Tests"
  [ testGroup "Error Classification and Recovery"
      [ testCase "syntax error classification" $ do
          let malformedInput = "func { broken syntax"
              expectedErrorType = SyntaxError
              result = parseTypus malformedInput
          case result of
            Left err -> assertEqual "Should be syntax error" expectedErrorType (errorType err)
            Right _ -> assertBool "Should have failed parsing" False

      , testCase "type error propagation" $ do
          let typeErrorInput = "func test() { x := \"string\" + 42; }"
              expectedPhase = TypeChecking
              result = parseTypus typeErrorInput
          case result of
            Right parsedFile -> do
              let compileResult = Compiler.compile parsedFile
              case compileResult of
                Left errs -> assertEqual "Should fail in type checking" expectedPhase (errorPhase $ head errs)
                Right _ -> assertBool "Should have failed type checking" False
            Left _ -> assertBool "Should parse successfully" False

      , testCase "ownership error detection" $ do
          let ownershipErrorInput = unlines
                [ "func moveTest() {"
                , "  data := createData()"
                , "  moved := move(data)"
                , "  use(data)  // Error: data already moved"
                , "}"
                ]
              result = parseTypus ownershipErrorInput
          case result of
            Right parsedFile -> do
              let compileResult = Compiler.compile parsedFile
              case compileResult of
                Left errs -> 
                  let hasOwnershipError = any (\e -> errorType e == OwnershipError) errs
                  in assertBool "Should detect ownership error" hasOwnershipError
                Right _ -> assertBool "Should fail with ownership error" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Error Context and Source Location"
      [ testCase "error context preservation" $ do
          let input = unlines
                [ "func main() {"
                , "  x := 1"
                , "  y := x + undefined_var"
                , "}"
                ]
              startPos = SourcePos 3 9 20  -- Position of undefined_var
              expectedContext = ErrorContext startPos "undefined identifier"
              result = parseTypus input
          case result of
            Right parsedFile -> do
              let compileResult = Compiler.compile parsedFile
              case compileResult of
                Left errs -> do
                  let context = errorContext $ head errs
                  assertEqual "Should preserve source location" startPos (contextPos context)
                Right _ -> assertBool "Should fail with undefined variable" False
            Left _ -> assertBool "Should parse successfully" False

      , testCase "multi-line error span calculation" $ do
          let multilineInput = unlines
                [ "func complex() {"
                , "  if condition {"
                , "    // nested block"
                , "    problematic_function_call("
                , "      arg1,"
                , "      arg2,"
                , "    )"
                , "  }"
                , "}"
                ]
              startPos = SourcePos 4 5 45
              endPos = SourcePos 7 6 80
              expectedSpan = SourceSpan startPos endPos
              result = parseTypus multilineInput
          case result of
            Right parsedFile -> do
              let compileResult = Compiler.compile parsedFile
              case compileResult of
                Left errs -> do
                  let span = errorSpan $ head errs
                  assertEqual "Should calculate correct multiline span" expectedSpan span
                Right _ -> assertBool "Should detect error in multiline context" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Error Recovery and Batch Processing"
      [ testCase "multiple errors in single file" $ do
          let multipleErrorsInput = unlines
                [ "func multipleErrors() {"
                , "  x := \"string\" + 42"  -- Type error
                , "  y := undefined_var"    -- Undefined variable
                , "  z := move(x)"          -- Ownership error (after move)
                , "  use(x)"                -- Use after move
                , "}"
                ]
              result = parseTypus multipleErrorsInput
          case result of
            Right parsedFile -> do
              let compileResult = Compiler.compile parsedFile
              case compileResult of
                Left errs -> do
                  assertBool "Should detect multiple errors" (length errs >= 3)
                  let hasTypeError = any (\e -> errorType e == TypeError) errs
                      hasUndefError = any (\e -> errorType e == UndefinedVariable) errs
                      hasOwnershipError = any (\e -> errorType e == OwnershipError) errs
                  assertBool "Should have type error" hasTypeError
                  assertBool "Should have undefined variable error" hasUndefError
                  assertBool "Should have ownership error" hasOwnershipError
                Right _ -> assertBool "Should fail with multiple errors" False
            Left _ -> assertBool "Should parse successfully" False

      , testCase "error recovery across modules" $ do
          let moduleA = "func moduleA() { return 42; }"
              moduleB = "func moduleB() { result := moduleA() + invalid; }"
              handler = createEnhancedHandler
              resultA = parseTypus moduleA
              resultB = parseTypus moduleB
          case (resultA, resultB) of
            (Right parsedA, Right parsedB) -> do
              let compileA = Compiler.compile parsedA
                  compileB = Compiler.compile parsedB
                  batchResults = processBatchErrors handler [compileA, compileB]
              assertBool "Should handle batch processing" (not $ null batchResults)
            _ -> assertBool "Both modules should parse" False
      ]

  , testGroup "Error Message Formatting"
      [ testCase "readable error messages" $ do
          let error = CompilerError 
                { errorType = TypeError
                , errorPhase = TypeChecking
                , errorMessage = "Cannot add string and integer"
                , errorSpan = SourceSpan (SourcePos 1 10 9) (SourcePos 1 25 24)
                , errorContext = ErrorContext (SourcePos 1 15 14) "type mismatch"
                }
              formatted = renderCompilationError error
              expectedKeywords = ["TypeError", "type mismatch", "Cannot add string and integer"]
              hasAllKeywords = all (`isInfixOf` formatted) expectedKeywords
          assertBool "Error message should contain key information" hasAllKeywords

      , testCase "error formatting preserves source context" $ do
          let errors = 
                [ CompilerError TypeError TypeChecking "First error" 
                    (SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)) 
                    (ErrorContext (SourcePos 1 5 4) "context1")
                , CompilerError OwnershipError Runtime "Second error"
                    (SourceSpan (SourcePos 2 1 15) (SourcePos 2 15 29))
                    (ErrorContext (SourcePos 2 8 22) "context2")
                ]
              formatted = formatCompilerErrors errors
              hasLineNumbers = "1" `isInfixOf` formatted && "2" `isInfixOf` formatted
              hasErrorTypes = "TypeError" `isInfixOf` formatted && "OwnershipError" `isInfixOf` formatted
          assertBool "Should format multiple errors with line numbers" hasLineNumbers
          assertBool "Should include error types in formatting" hasErrorTypes
      ]

  , testGroup "QuickCheck Properties for Error Handling"
      [ testProperty "error messages are non-empty" $ fastProperty $
          \input -> 
            let result = parseTypus input
            in case result of
              Left err -> not $ T.null $ T.pack $ renderCompilationError err
              Right _ -> property True

      , testProperty "error spans are valid" $ fastProperty $
          \input ->
            let result = parseTypus input
            in case result of
              Left err -> 
                let span = errorSpan err
                    start = spanStart span
                    end = spanEnd span
                in posLine start <= posLine end && 
                   (posLine start < posLine end || posColumn start <= posColumn end)
              Right _ -> property True
      ]
  ]

-- Helper functions for error testing
errorType :: CompilerError -> ErrorType
errorType (CompilerError et _ _ _ _) = et

errorPhase :: CompilerError -> CompilationPhase  
errorPhase (CompilerError _ ep _ _ _) = ep

errorSpan :: CompilerError -> SourceSpan
errorSpan (CompilerError _ _ _ es _) = es

errorContext :: CompilerError -> ErrorContext
errorContext (CompilerError _ _ _ _ ec) = ec

contextPos :: ErrorContext -> SourcePos
contextPos (ErrorContext pos _) = pos

data ErrorType = SyntaxError | TypeError | OwnershipError | UndefinedVariable | RuntimeError
  deriving (Show, Eq)

data ErrorContext = ErrorContext SourcePos String
  deriving (Show, Eq)
