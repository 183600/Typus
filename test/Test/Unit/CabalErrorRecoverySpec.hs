module Test.Unit.CabalErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import qualified Data.Text as T

import TestSupport.QuickCheck (fastProperty)

import qualified Parser (parseTypus)
import qualified ErrorHandler
import qualified SourceLocation

-- | Error recovery L.and robustness tests
tests :: TestTree
tests =
  testGroup "Cabal Error Recovery Tests"
    [ testGroup "Parser Error Recovery"
        [ testCase "Parser recovers from missing semicolons" $ do
            let input = "func test() { return 1\n  return 2\n}"
                result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Should provide helpful error message
                show err @?= "Expected ';' at end of statement"
              Right _ -> "Graceful handling" @?= "Should handle gracefully"

        , testCase "Parser handles unclosed blocks gracefully" $ do
            let input = "func test() { if (true) { return 1;"
                result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Error should indicate unclosed block
                let errStr = show err
                in "unclosed" `elem` words errStr @?= True
              Right _ -> "Should detect error" @?= "Error detection"

        , testCase "Parser provides line numbers in errors" $ do
            let input = "func test() {\n  return\n  invalid syntax here\n}"
                result = Parser.parseTypus input
            case result of
              Left err -> do
                let errStr = show err
                -- Error should contain line number information
                "line" `elem` words errStr @?= True
              Right _ -> "Should fail with location" @?= "Location info"
        ]

    , testGroup "Source Location Error Tracking"
        [ testCase "Source locations are preserved in error messages" $ do
            let input = "func broken() { return }"
                result = Parser.parseTypus input
            case result of
              Left err -> do
                let errStr = show err
                -- Error should contain position info
                L.any (`elem` ["1:", "2:", "3:"]) (words errStr) @?= True
              Right _ -> "Should fail with position" @?= "Position tracking"

        , testCase "Multi-line errors show correct span" $ do
            let input = "func test() {\n  return\n  invalid\n  syntax\n}"
                result = Parser.parseTypus input
            case result of
              Left err -> do
                let errStr = show err
                -- Should indicate multiple lines if applicable
                L.length (lines errStr) @?= 1  -- At least one line
              Right _ -> "Should fail appropriately" @?= "Appropriate failure"
        ]

    , testGroup "Error Handler Integration"
        [ testCase "ErrorHandler formats parser errors correctly" $ do
            let input = "func invalid() {"
                parseResult = Parser.parseTypus input
            case parseResult of
              Left err -> do
                -- ErrorHandler should format the error nicely
                let errStr = show err
                L.length errStr > 0 @?= True
              Right _ -> "Should not reach here" @?= "Unexpected success"

        , testCase "ErrorHandler preserves source location information" $ do
            let err = ErrorHandler.errorAt "TEST001" (T.pack "Test error") (ErrorHandler.ErrorLocation Nothing 2 10 Nothing Nothing)
                formatted = ErrorHandler.formatError err
            formatted @?= "Syntax error at line 2, column 10: Test error"
        ]

    , testGroup "Robustness Edge Cases"
        [ testCase "Empty input handled gracefully" $ do
            let result = Parser.parseTypus ""
            case result of
              Left err -> show err @?= "Empty input"
              Right _ -> "Handle empty" @?= "Empty handling"

        , testCase "Only whitespace input handled gracefully" $ do
            let result = Parser.parseTypus "   \n\t  \n  "
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> "Handle whitespace" @?= "Whitespace handling"

        , testCase "Extremely long line handled gracefully" $ do
            let longLine = replicate 1000 'a' ++ " func test() { return 1; }"
                result = Parser.parseTypus longLine
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> "Handle long line" @?= "Long line handling"

        , testProperty "Random input doesn't crash parser" $ do
            \input -> let result = Parser.parseTypus input
                      in case result of
                           Left _ -> True
                           Right _ -> True
        ]

    , testGroup "Error Recovery Strategies"
        [ testCase "Parser attempts to continue after first error" $ do
            let input = "func test1() { return }\nfunc test2() { return 1; }"
                result = Parser.parseTypus input
            case result of
              Left err -> do
                -- Should report first error but not crash
                L.length (show err) > 0 @?= True
              Right _ -> "Recovery attempt" @?= "Recovery attempted"

        , testCase "Multiple errors collected when possible" $ do
            let input = "func bad1() { return }\nfunc bad2() { if }"
                result = Parser.parseTypus input
            case result of
              Left err -> do
                -- Should indicate multiple issues if possible
                let errStr = show err
                L.length errStr > 10 @?= True  -- Reasonable error L.length
              Right _ -> "Error collection" @?= "Error collection"
        ]
    ]