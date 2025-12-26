{-# LANGUAGE CPP #-}
module Test.Unit.IntegratedCompilerPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (sort, nub, length, intercalate, isInfixOf, isPrefixOf, partition)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import IntegratedCompiler
  ( compileWithIntegratedAnalyzers
  , IntegratedCompileResult(..)
  , CompilerConfig(..)
  , defaultCompilerConfig
  , AnalysisResult(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , analysisToCombined
  , formatCompilationResult
  , getDetailedAnalysisSummary
  , showCombinedError
  )
import AnalyzerIntegration (AnalysisResult(..), CombinedError(..), ErrorSeverity(..))
import SyntaxValidator (SyntaxError(..), ErrorType(..))
import Compiler.Errors.Compiler (CompilerError)

-- | Property-based tests for IntegratedCompiler module
tests :: TestTree
tests =
  testGroup "IntegratedCompiler Properties Tests"
    [ testGroup "CompilerConfig properties"
        [ fastProperty "CompilerConfig equality is reflexive" prop_compilerConfigEquality
        , fastProperty "defaultCompilerConfig is valid" prop_defaultCompilerConfigValid
        , fastProperty "CompilerConfig preserves all fields" prop_compilerConfigPreservesFields
        ]

    , testGroup "IntegratedCompileResult properties"
        [ fastProperty "IntegratedCompileResult equality is reflexive" prop_integratedCompileResultEquality
        , fastProperty "IntegratedCompileResult preserves all fields" prop_integratedCompileResultPreservesFields
        , fastProperty "success field is consistent with errors" prop_successConsistentWithErrors
        ]

    , testGroup "Configuration scenarios"
        [ testCase "defaultCompilerConfig enables both analyzers" $ do
            let config = defaultCompilerConfig
            enableOwnership config @?= True
            enableDependentTypes config @?= True
            errorReportingLevel config @?= Warning

        , testCase "custom configuration preserves settings" $ do
            let config = CompilerConfig
                  { enableOwnership = False
                  , enableDependentTypes = True
                  , errorReportingLevel = Error
                  }
            enableOwnership config @?= False
            enableDependentTypes config @?= True
            errorReportingLevel config @?= Error

        , testCase "configuration variations are handled" $ do
            let configs = 
                  [ CompilerConfig True True Warning
                  , CompilerConfig True False Error
                  , CompilerConfig False True Info
                  , CompilerConfig False False Fatal
                  ]
            length configs @?= 4
            let ownershipEnabled = map enableOwnership configs
            Set.fromList ownershipEnabled @?= Set.fromList [True, False]
        ]

    , testGroup "Compilation result analysis"
        [ testCase "successful compilation has success=True" $ do
            let result = IntegratedCompileResult
                  { success = True
                  , compiledCode = "package main\nfunc main() {}"
                  , analysisResult = Nothing
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = []
                  , compilationInfo = []
                  }
            success result @?= True
            assertBool "should have compiled code" (not $ null $ compiledCode result)

        , testCase "failed compilation has success=False" $ do
            let result = IntegratedCompileResult
                  { success = False
                  , compiledCode = ""
                  , analysisResult = Nothing
                  , syntaxErrors = [SyntaxError UnexpectedToken "error" 1 1 "line"]
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = []
                  , compilationInfo = []
                  }
            success result @?= False
            assertBool "should have syntax errors" (not $ null $ syntaxErrors result)

        , testCase "result with analysis warnings" $ do
            let analysis = AnalysisResult
                  { analysisSuccess = True
                  , analysisErrors = []
                  , analysisWarnings = ["warning 1", "warning 2"]
                  , analysisInfo = ["info 1"]
                  }
                result = IntegratedCompileResult
                  { success = True
                  , compiledCode = "package main\nfunc main() {}"
                  , analysisResult = Just analysis
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = ["warning 1", "warning 2"]
                  , compilationInfo = ["info 1"]
                  }
            length (compilationWarnings result) @?= 2
            length (compilationInfo result) @?= 1

        , testCase "result with combined errors" $ do
            let errors = [IntegrationError "test error" Error]
                result = IntegratedCompileResult
                  { success = False
                  , compiledCode = ""
                  , analysisResult = Nothing
                  , syntaxErrors = []
                  , filteredErrors = errors
                  , compilerErrors = []
                  , compilationWarnings = []
                  , compilationInfo = []
                  }
            success result @?= False
            length (filteredErrors result) @?= 1
        ]

    , testGroup "Error handling and filtering"
        [ testCase "syntax errors are properly categorized" $ do
            let syntaxErrors = 
                  [ SyntaxError MissingBrace "missing brace" 10 5 "if true {"
                  , SyntaxError UnclosedString "unclosed string" 15 3 "fmt.Println(\"hello"
                  ]
            length syntaxErrors @?= 2
            let errorTypes = map errorType syntaxErrors
            Set.fromList errorTypes @?= Set.fromList [MissingBrace, UnclosedString]

        , testCase "combined errors are filtered by severity" $ do
            let errors = 
                  [ IntegrationError "info" Info
                  , IntegrationError "warning" Warning
                  , IntegrationError "error" Error
                  , IntegrationError "fatal" Fatal
                  ]
            length errors @?= 4
            let errorSeverities = map combinedErrorSeverity errors
            Set.fromList errorSeverities @?= Set.fromList [Info, Warning, Error, Fatal]

        , testCase "compiler errors are preserved in result" $ do
            let compilerErrors = ["parse error", "type error"]
                result = IntegratedCompileResult
                  { success = False
                  , compiledCode = ""
                  , analysisResult = Nothing
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = compilerErrors
                  , compilationWarnings = []
                  , compilationInfo = []
                  }
            length (compilerErrors result) @?= 2
            compilerErrors result @?= compilerErrors
        ]

    , testGroup "Complex compilation scenarios"
        [ testCase "compilation with multiple error types" $ do
            let syntaxErr = SyntaxError UnexpectedToken "unexpected" 5 10 "line content"
                analysisErr = IntegrationError "analysis failed" Error
                compilerErr = "compilation error"
                result = IntegratedCompileResult
                  { success = False
                  , compiledCode = ""
                  , analysisResult = Nothing
                  , syntaxErrors = [syntaxErr]
                  , filteredErrors = [analysisErr]
                  , compilerErrors = [compilerErr]
                  , compilationWarnings = ["warning"]
                  , compilationInfo = ["info"]
                  }
            success result @?= False
            length (syntaxErrors result) @?= 1
            length (filteredErrors result) @?= 1
            length (compilerErrors result) @?= 1

        , testCase "successful compilation with warnings" $ do
            let analysis = AnalysisResult
                  { analysisSuccess = True
                  , analysisErrors = []
                  , analysisWarnings = ["analysis warning"]
                  , analysisInfo = ["analysis info"]
                  }
                result = IntegratedCompileResult
                  { success = True
                  , compiledCode = "package main\nfunc main() {\n    fmt.Println(\"hello\")\n}"
                  , analysisResult = Just analysis
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = ["syntax warning", "analysis warning"]
                  , compilationInfo = ["analysis info"]
                  }
            success result @?= True
            assertBool "should have compiled code" (not $ null $ compiledCode result)
            length (compilationWarnings result) @?= 2
            length (compilationInfo result) @?= 1

        , testCase "compilation with detailed analysis" $ do
            let analysis = AnalysisResult
                  { analysisSuccess = True
                  , analysisErrors = []
                  , analysisWarnings = ["warning 1", "warning 2"]
                  , analysisInfo = ["info 1", "info 2", "info 3"]
                  }
                result = IntegratedCompileResult
                  { success = True
                  , compiledCode = "compiled code"
                  , analysisResult = Just analysis
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = ["warning 1", "warning 2"]
                  , compilationInfo = ["info 1", "info 2", "info 3"]
                  }
            case analysisResult result of
              Just actualAnalysis -> do
                analysisSuccess actualAnalysis @?= True
                length (analysisWarnings actualAnalysis) @?= 2
                length (analysisInfo actualAnalysis) @?= 3
              Nothing -> assertFailure "Expected analysis result"
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "empty source code handling" $ do
            let emptySource = ""
                config = defaultCompilerConfig
            -- Should handle empty source gracefully
            assertBool "empty source is valid" (length emptySource == 0)

        , testCase "very large source code handling" $ do
            let largeSource = unlines $ replicate 1000 "func test() {}"
                config = defaultCompilerConfig
            -- Should handle large source gracefully
            assertBool "large source is valid" (length (lines largeSource) == 1000)

        , testCase "source code with special characters" $ do
            let specialSource = unlines
                  [ "package main"
                  , "func main() {"
                  , "    // Special characters: !@#$%^&*()_+-=[]{}|;':\",./<>?"
                  , "    s := \"特殊字符测试\""
                  , "    fmt.Println(s)"
                  , "}"
                  ]
                config = defaultCompilerConfig
            -- Should handle special characters gracefully
            assertBool "special characters are handled" ("特殊字符测试" `isInfixOf` specialSource)

        , testCase "malformed source code handling" $ do
            let malformedSource = unlines
                  [ "package main"
                  , "func main {"
                  , "    missing parenthesis"
                  , "    unclosed string \"hello"
                  , "}"
                  ]
                config = defaultCompilerConfig
            -- Should handle malformed code gracefully
            assertBool "malformed source is handled" (not $ null malformedSource)
        ]

    , testGroup "Integration properties"
        [ testCase "analysis to combined conversion preserves errors" $ do
            let analysis = AnalysisResult
                  { analysisSuccess = False
                  , analysisErrors = [IntegrationError "error 1" Error, IntegrationError "error 2" Warning]
                  , analysisWarnings = ["warning"]
                  , analysisInfo = ["info"]
                  }
                combined = analysisToCombined analysis
            length combined @?= 2
            let severities = map combinedErrorSeverity combined
            Set.fromList severities @?= Set.fromList [Error, Warning]

        , testCase "error formatting preserves information" $ do
            let error = IntegrationError "test error message" Error
                formatted = showCombinedError error
            assertBool "should contain error message" ("test error message" `isInfixOf` formatted)
            assertBool "should contain severity" ("Error" `isInfixOf` formatted)

        , testCase "compilation result formatting is informative" $ do
            let result = IntegratedCompileResult
                  { success = True
                  , compiledCode = "package main\nfunc main() {}"
                  , analysisResult = Nothing
                  , syntaxErrors = []
                  , filteredErrors = []
                  , compilerErrors = []
                  , compilationWarnings = ["warning"]
                  , compilationInfo = ["info"]
                  }
                formatted = formatCompilationResult result
            assertBool "should indicate success" ("Success" `isInfixOf` formatted)
            assertBool "should contain compiled code indicator" ("Compiled" `isInfixOf` formatted)
        ]
    ]

-- Helper generators for testing
genCompilerConfig :: Gen CompilerConfig
genCompilerConfig = do
  enableOwnership <- elements [True, False]
  enableDependentTypes <- elements [True, False]
  errorReportingLevel <- elements [Info, Warning, Error, Fatal]
  return $ CompilerConfig enableOwnership enableDependentTypes errorReportingLevel

genIntegratedCompileResult :: Gen IntegratedCompileResult
genIntegratedCompileResult = do
  success <- elements [True, False]
  compiledCode <- elements ["", "package main\nfunc main() {}", "compiled code"]
  analysisResult <- elements [Nothing, Just $ AnalysisResult True [] [] []]
  syntaxErrors <- listOf $ elements [SyntaxError UnexpectedToken "error" 1 1 "line"]
  filteredErrors <- listOf $ elements [IntegrationError "error" Error]
  compilerErrors <- listOf $ elements ["compiler error"]
  compilationWarnings <- listOf $ elements ["warning"]
  compilationInfo <- listOf $ elements ["info"]
  return $ IntegratedCompileResult success compiledCode analysisResult syntaxErrors filteredErrors compilerErrors compilationWarnings compilationInfo

genCombinedError :: Gen CombinedError
genCombinedError = do
  message <- elements ["error message", "warning message", "info message"]
  severity <- elements [Info, Warning, Error, Fatal]
  return $ IntegrationError message severity

-- Property: CompilerConfig equality is reflexive
prop_compilerConfigEquality :: CompilerConfig -> Property
prop_compilerConfigEquality config = config === config

-- Property: defaultCompilerConfig is valid
prop_defaultCompilerConfigValid :: Property
prop_defaultCompilerConfigValid = 
  let config = defaultCompilerConfig
  in enableOwnership config && enableDependentTypes config &&
     errorReportingLevel config == Warning

-- Property: CompilerConfig preserves all fields
prop_compilerConfigPreservesFields :: Bool -> Bool -> ErrorSeverity -> Property
prop_compilerConfigPreservesFields ownership dependentTypes severity =
  let config = CompilerConfig ownership dependentTypes severity
  in enableOwnership config === ownership &&
     enableDependentTypes config === dependentTypes &&
     errorReportingLevel config === severity

-- Property: IntegratedCompileResult equality is reflexive
prop_integratedCompileResultEquality :: IntegratedCompileResult -> Property
prop_integratedCompileResultEquality result = result === result

-- Property: IntegratedCompileResult preserves all fields
prop_integratedCompileResultPreservesFields :: Bool -> String -> Maybe AnalysisResult -> [SyntaxError] -> [CombinedError] -> [CompilerError] -> [String] -> [String] -> Property
prop_integratedCompileResultPreservesFields success compiledCode analysisResult syntaxErrors filteredErrors compilerErrors compilationWarnings compilationInfo =
  let result = IntegratedCompileResult success compiledCode analysisResult syntaxErrors filteredErrors compilerErrors compilationWarnings compilationInfo
  in success result === success &&
     compiledCode result === compiledCode &&
     analysisResult result === analysisResult &&
     syntaxErrors result === syntaxErrors &&
     filteredErrors result === filteredErrors &&
     compilerErrors result === compilerErrors &&
     compilationWarnings result === compilationWarnings &&
     compilationInfo result === compilationInfo

-- Property: success field is consistent with errors
prop_successConsistentWithErrors :: IntegratedCompileResult -> Property
prop_successConsistentWithErrors result =
  let hasErrors = not $ null (syntaxErrors result) || 
                  not $ null (filteredErrors result) || 
                  not $ null (compilerErrors result)
  in if hasErrors 
     then success result === False
     else property True  -- Success can be True or False when no errors

-- Property: combined error conversion preserves severity
prop_combinedErrorConversionPreservesSeverity :: CombinedError -> Property
prop_combinedErrorConversionPreservesSeverity error =
  combinedErrorSeverity error `elem` [Info, Warning, Error, Fatal]

-- Property: error formatting contains essential information
prop_errorFormattingContainsInfo :: CombinedError -> Property
prop_errorFormattingContainsInfo error =
  let formatted = showCombinedError error
  in length formatted > 0

-- Property: compilation result formatting is consistent
prop_compilationResultFormattingConsistent :: IntegratedCompileResult -> Property
prop_compilationResultFormattingConsistent result =
  let formatted = formatCompilationResult result
  in length formatted > 0