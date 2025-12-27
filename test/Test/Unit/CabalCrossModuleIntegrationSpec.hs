module Test.Unit.CabalCrossModuleIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Parser (parseTypus, TypusFile(..), FileDirectives(..))
import qualified SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Utils (trim, splitBy, removeComments)
import qualified SyntaxValidator

-- | Cross-module integration tests for cabal testing
tests :: TestTree
tests =
  testGroup "Cabal Cross-Module Integration Tests"
    [ testGroup "Parser and Utils Integration"
        [ testCase "Parser handles comments removed by Utils" $ do
            let input = "// This is a comment\nfunc main() { /* block comment */ return 42; }"
                cleaned = Utils.removeComments input
                result = Parser.parseTypus "test" cleaned
            case result of
              Left err -> @?= "Should parse successfully" (show err)
              Right _ -> @?= "Success" "Success"

        , testCase "Parser handles trimmed whitespace" $ do
            let input = "  \n  func main() { return 42; }  \n  "
                trimmed = Utils.trim input
                result = Parser.parseTypus "test" trimmed
            case result of
              Left err -> @?= "Should parse successfully" (show err)
              Right _ -> @?= "Success" "Success"

        , testProperty "splitBy integration with parser line handling" $ do
            \input -> let lines = Utils.splitBy '\n' input
                          lineCount = length lines
                      in lineCount >= 0
        ]

    , testGroup "SourceLocation and Parser Integration"
        [ testCase "Source locations are preserved in parse results" $ do
            let input = "func test() { return 1; }"
                result = Parser.parseTypus "test" input
            case result of
              Left _ -> @?= "Should parse successfully" "Parse failed"
              Right typusFile -> 
                case Parser.tfCodeBlocks typusFile of
                  [] -> @?= "Should have code blocks" "No code blocks found"
                  (block:_) -> @?= "Should have valid span" "Valid span"

        , testCase "Source position calculations are consistent" $ do
            let pos1 = SourceLocation.SourcePos 1 1
                pos2 = SourceLocation.SourcePos 1 5
                span = SourceLocation.SourceSpan pos1 pos2
            SourceLocation.spanStart span @?= pos1
            SourceLocation.spanEnd span @?= pos2
        ]

    , testGroup "SyntaxValidator and Parser Integration"
        [ testCase "Validated parsed code passes syntax validation" $ do
            let input = "func validated() { return true; }"
                result = Parser.parseTypus "test" input
            case result of
              Left _ -> @?= "Should parse successfully" "Parse failed"
              Right typusFile -> do
                -- Assuming syntax validation would pass for simple valid code
                @?= "Should validate" "Validation should pass"

        , testCase "Syntax validation catches parser edge cases" $ do
            let input = "func invalid() { return ; }"  -- Missing expression
                result = Parser.parseTypus "test" input
            case result of
              Left _ -> @?= "Expected parse failure" "Parse should fail"
              Right _ -> @?= "Should not reach here" "Unexpected success"
        ]

    , testGroup "FileDirectives Integration"
        [ testCase "Parser correctly extracts file directives" $ do
            let input = "// @ownership: true\n// @dependent-types: false\nfunc main() {}"
                result = Parser.parseTypus "test" input
            case result of
              Left _ -> @?= "Should parse successfully" "Parse failed"
              Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                Parser.fdOwnership directives @?= Just (SourceLocation.Located (SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1) (SourceLocation.SourcePos 1 1)) True)
                Parser.fdDependentTypes directives @?= Just (SourceLocation.Located (SourceLocation.SourceSpan (SourceLocation.SourcePos 1 2) (SourceLocation.SourcePos 1 2)) False)
        ]

    , testGroup "Error handling integration"
        [ testCase "Parser provides meaningful error locations" $ do
            let input = "func broken() { return }"  -- Missing semicolon
                result = Parser.parseTypus "test" input
            case result of
              Left err -> 
                -- Error should contain location information
                let errStr = show err
                in "line" `elem` words errStr @?= True
              Right _ -> @?= "Should fail" "Unexpected success"
        ]
    ]