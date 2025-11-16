module Test.Unit.CompilerSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import qualified Compiler
import qualified Compiler.DependentTypeChecker as DepChecker
import qualified Parser

tests :: TestTree
tests =
  testGroup "Compiler"
    [ testCase "generates Go code for valid Typus input" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "compiled code should start with a package declaration" ("package main" `isInfixOf` goCode)
        assertBool "compiled code should contain the main function" ("func main" `isInfixOf` goCode)

    , testCase "fails when dependent type blocks contain errors" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {}"
              , ""
              , "{//! dependent_types: on}"
              , "alias Broken"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> assertBool "error should mention dependent type checking" ("DependentTypeCheckingPhase" `isInfixOf` Compiler.renderCompilationError err)
          Right _  -> assertFailure "expected dependent type error"

    , testCase "file-level dependent type directives are enforced without blocks" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "alias Broken"
              ]
        typusFile <- expectParse source
        case DepChecker.checkDependentTypes typusFile of
          Left errs -> assertBool "expected at least one dependent type error" (not $ null errs)
          Right _   -> assertFailure "expected dependent type errors when file directive is enabled"

    , testCase "rejects malformed syntax with unbalanced braces" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> assertBool "error should mention malformed syntax" ("Malformed syntax detected" `isInfixOf` Compiler.renderCompilationError err)
          Right _  -> assertFailure "expected malformed syntax to be rejected"
    ]

expectParse :: String -> IO Parser.TypusFile
expectParse source =
  case Parser.parseTypus source of
    Left err     -> assertFailure ("parseTypus failed: " <> err)
    Right parsed -> pure parsed

expectCompile :: Parser.TypusFile -> IO String
expectCompile typusFile =
  case Compiler.compile typusFile of
    Left err      -> assertFailure ("compile failed: " <> Compiler.renderCompilationError err)
    Right goCode  -> pure goCode
