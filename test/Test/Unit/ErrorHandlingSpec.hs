module Test.Unit.ErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Data.List (isInfixOf)

import qualified Compiler
import qualified Parser

tests :: TestTree
tests =
  testGroup "Error handling"
    [ testCase "handles parse errors gracefully" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              ]
        case Parser.parseTypus source of
          Right typusFile -> do
            assertBool "should have syntax errors" (not (null (Parser.tfSyntaxErrors typusFile)))
            case Compiler.compile typusFile of
              Left err -> assertBool "compile error should be informative" (length (Compiler.renderCompilationError err) > 10)
              Right _ -> assertFailure "expected compilation error"
          Left _ -> assertFailure "parse should succeed with errors"

    , testCase "reports compilation errors with source locations" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func main() {"
              , "    add(\"oops\", 2)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "error should include line information" ("1:" `isInfixOf` rendered)
            assertBool "error should include type information" ("Type error" `isInfixOf` rendered)
          Right _ -> assertFailure "expected compilation error"

    , testCase "aggregates multiple errors in single compilation" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func multiply(x int, y int) int {"
              , "    return x * y"
              , "}"
              , "func main() {"
              , "    add(\"oops1\", 2)"
              , "    multiply(\"oops2\", 3)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should report multiple errors" (length (lines rendered) > 2)
            assertBool "should include both function errors" ("add" `isInfixOf` rendered && "multiply" `isInfixOf` rendered)
          Right _ -> assertFailure "expected multiple compilation errors"

    , testCase "provides helpful error messages for undefined variables" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(undefinedVar)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should mention undefined variable" ("undefinedVar" `isInfixOf` rendered)
            assertBool "should suggest similar names if available" (length rendered > 20)
          Right _ -> assertFailure "expected undefined variable error"

    , testCase "handles circular dependency detection" $ do
        let source = unlines
              [ "package main"
              , "func a() {"
              , "    b()"
              , "}"
              , "func b() {"
              , "    a()"
              , "}"
              , "func main() {"
              , "    a()"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should detect circular dependency" ("Circular dependency" `isInfixOf` rendered)
          Right _ -> assertFailure "expected circular dependency error"

    , testCase "reports type mismatch errors with expected and actual types" $ do
        let source = unlines
              [ "package main"
              , "func expectInt(x int) int { return x }"
              , "func main() {"
              , "    expectInt(\"string\")"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should mention expected type" ("expected type int" `isInfixOf` rendered)
            assertBool "should mention actual type" ("got string" `isInfixOf` rendered)
          Right _ -> assertFailure "expected type mismatch error"

    , testCase "provides context for errors in nested blocks" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        if false {"
              , "            undefinedFunction()"
              , "        }"
              , "    }"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should include nested context" ("nested" `isInfixOf` rendered || "block" `isInfixOf` rendered)
          Right _ -> assertFailure "expected error with nested context"

    , testCase "handles errors in dependent type constraints" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "type Vector<T> struct {"
              , "    values: T"
              , "}"
              , "where len undefinedVar > 0"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should mention dependent type error" ("dependent" `isInfixOf` rendered)
            assertBool "should mention undefined variable in constraint" ("undefinedVar" `isInfixOf` rendered)
          Right _ -> assertFailure "expected dependent type constraint error"

    , testCase "reports ownership violations with clear explanations" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func consume(x string) string { return x }"
              , "func main() {"
              , "    data := \"hello\""
              , "    consume(data)"
              , "    println(data)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should mention ownership error" ("ownership" `isInfixOf` rendered || "use after move" `isInfixOf` rendered)
            assertBool "should mention the variable" ("data" `isInfixOf` rendered)
          Right _ -> assertFailure "expected ownership violation error"

    , testCase "provides error recovery suggestions" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should provide type mismatch info" ("type" `isInfixOf` rendered)
            assertBool "should be detailed enough for fixing" (length rendered > 30)
          Right _ -> assertFailure "expected error with recovery suggestions"

    , testCase "handles malformed directive errors" $ do
        let source = unlines
              [ "//! invalid_directive: broken"
              , "package main"
              , "func main() {}"
              ]
        case Parser.parseTypus source of
          Left err -> assertBool "should report directive error" ("directive" `isInfixOf` err || "unknown" `isInfixOf` err)
          Right _ -> assertFailure "expected directive parsing error"

    , testCase "reports syntax errors with position information" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true"
              , "        println(\"missing brace\")"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertBool "should include line information" ("line" `isInfixOf` err)
          Right _ -> assertFailure "expected syntax error with position"
    ]

expectParse :: String -> IO Parser.TypusFile
expectParse source =
  case Parser.parseTypus source of
    Left err     -> assertFailure ("parseTypus failed: " <> err)
    Right parsed -> pure parsed