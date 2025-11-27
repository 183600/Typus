module Test.Unit.SyntaxValidatorSpec (tests) where

import Data.List (find, isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified SimpleSyntaxValidator as Simple
import qualified SyntaxValidator as Full

-- | Coverage-focused tests for the two syntax validator implementations.
tests :: TestTree
tests =
  testGroup "Syntax validators"
    [ simpleValidatorTests
    , fullValidatorTests
    ]

simpleValidatorTests :: TestTree
simpleValidatorTests =
  testGroup "SimpleSyntaxValidator"
    [ testCase "detects missing package declaration" $ do
        let snippet = unlines
              [ "package"
              , "func main() {}"
              ]
            errors = Simple.validateSyntaxSimple snippet
        err <- expectSimpleError Simple.MissingPackageDeclaration errors
        case err of
          Simple.SyntaxError _ message line _ _ -> do
            line @?= 1
            assertBool "message should mention the package requirement"
              ("package" `isInfixOf` message)

    , testCase "rejects import statements without quoted package paths" $ do
        let snippet = unlines
              [ "package main"
              , "import fmt"
              , "func main() {}"
              ]
            errors = Simple.validateSyntaxSimple snippet
        err <- expectSimpleError Simple.InvalidImport errors
        case err of
          Simple.SyntaxError _ _ line _ lineContent -> do
            line @?= 2
            lineContent @?= "import fmt"

    , testCase "flags invalid triple-plus operator usage" $ do
        let snippet = unlines
              [ "package main"
              , "func main() {"
              , "    counter +++"
              , "}"
              ]
            errors = Simple.validateSyntaxSimple snippet
        err <- expectSimpleError Simple.InvalidOperator errors
        case err of
          Simple.SyntaxError _ _ line _ lineContent -> do
            line @?= 3
            assertBool "line content should include the offending operator"
              ("+++" `isInfixOf` lineContent)

    , testCase "ignores braces inside literals when reporting missing closures" $ do
        let snippet = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"}\") // } closes only in literals"
              ]
            errors = Simple.validateSyntaxSimple snippet
        err <- expectSimpleError Simple.MissingBrace errors
        case err of
          Simple.SyntaxError _ _ line _ _ -> line @?= 2

    , testCase "countBraces treats braces inside literals as inert" $ do
        let snippet = unlines
              [ "func leak() {"
              , "    println(\"}\") // } only inside literal/comment"
              ]
        Simple.countBraces snippet @?= 1

    , testCase "countBraces returns zero when real braces are balanced" $ do
        let snippet = unlines
              [ "func stable() {"
              , "    /* } { */"
              , "}"
              ]
        Simple.countBraces snippet @?= 0
    ]

fullValidatorTests :: TestTree
fullValidatorTests =
  testGroup "SyntaxValidator"
    [ testCase "reports missing package declarations for Go code" $ do
        let snippet = unlines
              [ "func helper() {}"
              ]
            errors = Full.validateSyntax snippet
        err <- expectFullError Full.MissingPackageDeclaration errors
        case err of
          Full.SyntaxError _ message line column _ -> do
            line @?= 1
            column @?= 1
            assertBool "message should mention Go files" ("Go file" `isInfixOf` message)

    , testCase "rejects Typus directives that lack a colon" $ do
        let snippet = unlines
              [ "//! ownership on"
              , "func main() {}"
              ]
            errors = Full.validateSyntax snippet
        err <- expectFullError Full.InvalidStatement errors
        case err of
          Full.SyntaxError _ message line _ lineContent -> do
            line @?= 1
            assertBool "message should mention directive formatting" ("directive" `isInfixOf` message)
            lineContent @?= "//! ownership on"

    , testCase "identifies control-flow blocks that omit braces" $ do
        let snippet = unlines
              [ "package main"
              , "func main() {"
              , "    if true"
              , "        println(\"oops\")"
              , "}"
              ]
            errors = Full.validateSyntax snippet
        err <- expectFullError Full.UnterminatedBlock errors
        case err of
          Full.SyntaxError _ message line _ _ -> do
            line @?= 3
            assertBool "message should mention the if statement" ("if" `isInfixOf` message)

    , testCase "detects missing closing braces at end of file" $ do
        let snippet = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hi\")"
              ]
            errors = Full.validateSyntax snippet
        err <- expectFullError Full.MissingBrace errors
        case err of
          Full.SyntaxError _ message line _ _ -> do
            line @?= 2
            assertBool "message should mention the unmatched brace" ("Unclosed" `isInfixOf` message)

    , testCase "reports incomplete type declarations" $ do
        let snippet = unlines
              [ "package main"
              , "type"
              , "func main() {}"
              ]
            errors = Full.validateSyntax snippet
        err <- expectFullError Full.InvalidTypeDeclaration errors
        case err of
          Full.SyntaxError _ _ line _ _ -> line @?= 2

    , testCase "formatSyntaxError annotates location and source line" $ do
        let err = Full.SyntaxError Full.InvalidOperator "Triple plus not allowed" 5 9 "value +++"
            rendered = Full.formatSyntaxError err
        rendered @?= "Line 5:9 [InvalidOperator] Triple plus not allowed\n    value +++"
    ]

expectSimpleError :: Simple.ErrorType -> [Simple.SyntaxError] -> IO Simple.SyntaxError
expectSimpleError expected errors =
  case findSimpleError expected errors of
    Just err -> pure err
    Nothing -> assertFailure $ "Expected simple syntax error of type " ++ show expected

expectFullError :: Full.ErrorType -> [Full.SyntaxError] -> IO Full.SyntaxError
expectFullError expected errors =
  case findFullError expected errors of
    Just err -> pure err
    Nothing -> assertFailure $ "Expected syntax validator error of type " ++ show expected

findSimpleError :: Simple.ErrorType -> [Simple.SyntaxError] -> Maybe Simple.SyntaxError
findSimpleError expected = find matches
  where
    matches (Simple.SyntaxError actual _ _ _ _) = actual == expected

findFullError :: Full.ErrorType -> [Full.SyntaxError] -> Maybe Full.SyntaxError
findFullError expected = find matches
  where
    matches (Full.SyntaxError actual _ _ _ _) = actual == expected
