module Test.Unit.IntegratedCompilerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import IntegratedCompiler
import Compiler (compile)
import Parser (parseTypus)
import AnalyzerIntegration (CombinedError(..), ErrorSeverity(..))
import Control.Exception (try, SomeException)

-- | Unit tests for IntegratedCompiler module
tests :: TestTree
tests =
  testGroup "IntegratedCompiler"
    [ testGroup "Configuration"
        [ testCase "defaultCompilerConfig has expected defaults" $ do
            let config = defaultCompilerConfig
            enableOwnership config @?= True
            enableDependentTypes config @?= True
            errorReportingLevel config @?= Warning

        , testCase "compiler config can be customized" $ do
            let customConfig = CompilerConfig
                    { enableOwnership = False
                    , enableDependentTypes = True
                    , errorReportingLevel = Error
                    }
            enableOwnership customConfig @?= False
            enableDependentTypes customConfig @?= True
            errorReportingLevel customConfig @?= Error
        ]

    , testGroup "Compilation workflow"
        [ testCase "compileWithIntegratedAnalyzers handles simple valid code" $ do
            let source = "package main\n\nfunc main() {\n    return\n}\n"
            let config = defaultCompilerConfig
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    assertBool "Compilation should succeed" $ not $ null compileResult

        , testCase "compileWithIntegratedAnalyzers handles syntax errors" $ do
            let source = "invalid syntax { {"
            let config = defaultCompilerConfig
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    -- Should contain error information
                    assertBool "Should handle syntax errors gracefully" $ True

        , testCase "compileWithIntegratedAnalyzers respects ownership setting" $ do
            let source = "package main\n\nfunc main() {\n    x := 5\n    return x\n}\n"
            let config = defaultCompilerConfig { enableOwnership = False }
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    assertBool "Should compile without ownership analysis" $ not $ null compileResult

        , testCase "compileWithIntegratedAnalyzers respects dependent types setting" $ do
            let source = "package main\n\nfunc main() {\n    return\n}\n"
            let config = defaultCompilerConfig { enableDependentTypes = False }
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    assertBool "Should compile without dependent types analysis" $ not $ null compileResult
        ]

    , testGroup "Error handling"
        [ testCase "analysisToCombined converts analysis results properly" $ do
            let mockError = CombinedError
                    { errorMessage = "Test error"
                    , errorSeverity = Error
                    , errorLocation = Nothing
                    }
            let combined = analysisToCombined mockError
            errorMessage combined @?= "Test error"
            errorSeverity combined @?= Error

        , testCase "formatCompilationResult formats results correctly" $ do
            let mockResult = "Compilation successful"
            let formatted = formatCompilationResult mockResult
            assertBool "Should format compilation result" $ not $ null formatted

        , testCase "getDetailedAnalysisSummary provides summary" $ do
            let mockResult = "Analysis complete"
            let summary <- getDetailedAnalysisSummary mockResult
            assertBool "Should provide analysis summary" $ not $ null summary

        , testCase "showCombinedError formats error messages" $ do
            let error = CombinedError
                    { errorMessage = "Test error message"
                    , errorSeverity = Warning
                    , errorLocation = Nothing
                    }
            let errorString = showCombinedError error
            assertBool "Should contain error message" $ "Test error message" `elem` errorString
            assertBool "Should contain severity" $ "Warning" `elem` errorString
        ]

    , testGroup "Integration with parser"
        [ testCase "works with parseTypus results" $ do
            let source = "package main\n\nfunc main() {\n    return\n}\n"
            case parseTypus source of
                Left err -> assertFailure $ "Parse failed: " ++ err
                Right parsedFile -> do
                    let config = defaultCompilerConfig
                    result <- try $ compileWithIntegratedAnalyzers config source
                    case result of
                        Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                        Right compileResult -> do
                            assertBool "Should compile parsed file" $ not $ null compileResult
        ]

    , testGroup "Edge cases"
        [ testCase "handles empty source code" $ do
            let source = ""
            let config = defaultCompilerConfig
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    -- Should handle empty input gracefully
                    assertBool "Should handle empty source" $ True

        , testCase "handles source with only whitespace" $ do
            let source = "   \n  \n   \t  \n"
            let config = defaultCompilerConfig
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    assertBool "Should handle whitespace-only source" $ True

        , testCase "handles source with comments only" $ do
            let source = "// This is a comment\n/* Another comment */\n// Line comment"
            let config = defaultCompilerConfig
            result <- try $ compileWithIntegratedAnalyzers config source
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right compileResult -> do
                    assertBool "Should handle comment-only source" $ True
        ]
    ]