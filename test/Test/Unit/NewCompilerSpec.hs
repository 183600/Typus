{-# LANGUAGE CPP #-}
module Test.Unit.NewCompilerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf)

import Parser
  ( parseTypus
  , TypusFile(..)
  )
import Compiler
  ( compile
  , CompilerError(..)
  , CompilationPhase(..)
  , renderCompilationError
  , generateGoCode
  , ensureSourceIR
  , typeDiagnosticToCompilerError
  , TypeCheckDiagnostic(..)
  )
import qualified Compiler.IR as IR
import Compiler.Errors
  ( ErrorCategory(..)
  , ErrorSeverity(..)
  )
import SourceLocation (defaultSpan)

tests :: TestTree
tests =
  testGroup "New Compiler Tests"
    [ testCase "compiles simple package declaration" $ do
        let source = "package main\nfunc main() {}"
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)
                assertBool "contains main function" ("func main()" `isInfixOf` goCode)

    , testCase "compiles with ownership directive" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)

    , testCase "compiles with dependent_types directive" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)

    , testCase "compiles with block directives" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on}"
              , "        println(\"hello\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)
                assertBool "contains println" ("println" `isInfixOf` goCode)

    , testCase "handles type mismatch error" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> do
                length errs @?= 1
                let err = head errs
                errorCode err @?= "CP0003"
                errorPhase err @?= TypeCheckingPhase
                errorCategory err @?= TypeChecking
                errorSeverity err @?= Error
                assertBool "error message mentions type error" ("type error" `isInfixOf` show (errorMessage err))
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "ensures source IR for valid syntax" $ do
        let source = unlines
              [ "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case ensureSourceIR typusFile of
              Left errs -> assertFailure $ "ensureSourceIR failed: " ++ show errs
              Right sourceIR -> do
                let retrievedFile = IR.sourceTypusFile sourceIR
                tfDirectives retrievedFile @?= tfDirectives typusFile

    , testCase "fails source IR creation for malformed syntax" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    // Unclosed brace"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case ensureSourceIR typusFile of
              Left errs -> do
                length errs @?= 1
                let err = head errs
                errorCode err @?= "CP0001"
                errorPhase err @?= ParsingPhase
                errorCategory err @?= Parsing
                errorSeverity err @?= Error
                assertBool "error mentions malformed syntax" ("Malformed syntax" `isInfixOf` show (errorMessage err))
              Right _ -> assertFailure "expected ensureSourceIR to fail"

    , testCase "generates Go code fallback for compilation failures" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let goCode = generateGoCode typusFile
            assertBool "contains original source" ("var x int = \"string\"" `isInfixOf` goCode)

    , testCase "generates Go code for successful compilation" $ do
        let source = unlines
              [ "package main"
              , "func greet(name string) string {"
              , "    return \"Hello, \" + name"
              , "}"
              , "func main() {"
              , "    println(greet(\"World\"))"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains greet function" ("func greet" `isInfixOf` goCode)
                assertBool "contains main function" ("func main" `isInfixOf` goCode)

    , testCase "converts type diagnostic to compiler error" $ do
        let diagnostic = TypeCheckDiagnostic (Just "main") "cannot use string as int"
            compilerError = typeDiagnosticToCompilerError diagnostic
        errorCode compilerError @?= "CP0002"
        errorPhase compilerError @?= TypeCheckingPhase
        errorCategory compilerError @?= TypeChecking
        errorSeverity compilerError @?= Error
        assertBool "error mentions context" ("Type error in 'main'" `isInfixOf` show (errorMessage compilerError))
        assertBool "error mentions detail" ("cannot use string as int" `isInfixOf` show (errorMessage compilerError))

    , testCase "converts type diagnostic without context" $ do
        let diagnostic = TypeCheckDiagnostic Nothing "undefined variable"
            compilerError = typeDiagnosticToCompilerError diagnostic
        errorCode compilerError @?= "CP0002"
        assertBool "error mentions type error without context" ("Type error: undefined variable" `isInfixOf` show (errorMessage compilerError))

    , testCase "renders compilation errors" $ do
        let errors = 
              [ CompilerError "CP0001" "Syntax error" ParsingPhase Parsing Error (Just defaultSpan) Nothing [] [] Nothing
              , CompilerError "CP0002" "Type error" TypeCheckingPhase TypeChecking Error (Just defaultSpan) Nothing [] [] Nothing
              ]
            rendered = renderCompilationError errors
        assertBool "contains CP0001" ("CP0001" `isInfixOf` rendered)
        assertBool "contains CP0002" ("CP0002" `isInfixOf` rendered)
        assertBool "contains syntax error" ("Syntax error" `isInfixOf` rendered)
        assertBool "contains type error" ("Type error" `isInfixOf` rendered)

    , testCase "handles complex function compilation" $ do
        let source = unlines
              [ "package main"
              , "func calculate(x, y int) int {"
              , "    return x + y"
              , "}"
              , "func main() {"
              , "    result := calculate(10, 20)"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains calculate function" ("func calculate" `isInfixOf` goCode)
                assertBool "contains function call" ("calculate(10, 20)" `isInfixOf` goCode)

    , testCase "compiles with build tags" $ do
        let source = unlines
              [ "//go:build ignore"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)

    , testCase "handles multiple directives" $ do
        let source = unlines
              [ "//! ownership: on, dependent_types: on"
              , "package main"
              , "func main() {"
              , "    {//! constraints: off}"
              , "        x := 42"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package declaration" ("package main" `isInfixOf` goCode)

    , testCase "preserves comments in generated code" $ do
        let source = unlines
              [ "package main"
              , "// This is a comment"
              , "func main() {"
              , "    // Inline comment"
              , "    println(\"hello\") /* block comment */"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains line comment" ("// This is a comment" `isInfixOf` goCode)
                assertBool "contains inline comment" ("// Inline comment" `isInfixOf` goCode)

    , testCase "handles empty file" $ do
        let source = ""
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "empty result is acceptable" (null goCode || goCode == "")
    ]