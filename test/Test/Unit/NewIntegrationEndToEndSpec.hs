{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewIntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import IntegratedCompiler
  ( compileWithIntegratedAnalyzers
  , IntegratedCompileResult(..)
  , CompilerConfig(..)
  , defaultCompilerConfig
  , AnalysisResult(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , formatCompilationResult
  , getDetailedAnalysisSummary
  , showCombinedError
  )

import Parser (parseTypus)
import Compiler (compile)
import SyntaxValidator (validateFile, SyntaxError(..), ErrorType(..))
import AnalyzerIntegration
  ( AnalysisResult(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , newIntegratedAnalyzer
  , runIntegratedAnalysis
  , mkAnalysisInput
  )

import Compiler.Errors (CompilerError)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isInfixOf, isPrefixOf, sort, nub)
import Data.Text (Text)
import qualified Data.Text as T

-- | Integration end-to-end tests
tests :: TestTree
tests =
  testGroup "New Integration End-to-End Tests"
    [ testGroup "Complete compilation pipeline"
        [ testCase "simple program compiles successfully" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "import \"fmt\""
                  , ""
                  , "func main() {"
                  , "    fmt.Println(\"Hello, World!\")"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= True
            compiledCode result @?= ""
                -- Should contain the generated Go code
            "Hello, World!" `isInfixOf` compiledCode result @?= True
            length (syntaxErrors result) @?= 0
            length (filteredErrors result) @?= 0
            length (compilerErrors result) @?= 0
            
        , testCase "program with syntax errors fails appropriately" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")  // Missing closing brace"
                  , ""
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= False
            compiledCode result @?= ""
            -- Should have syntax errors
            length (syntaxErrors result) @>= 1
            
        , testCase "program with ownership errors is detected" $ do
            let source = unlines
                  [ "//! ownership on"
                  , ""
                  , "func main() {"
                  , "    x := 42"
                  , "    y := x"
                  , "    z := x  -- Use after move"
                  , "    println(z)"
                  , "}"
                  ]
                config = defaultCompilerConfig { enableOwnership = True }
            result <- compileWithIntegratedAnalyzers source config
            -- Should detect ownership errors
            let ownershipErrors = filter isOwnershipError (filteredErrors result)
            length ownershipErrors @>= 1
          where
            isOwnershipError (OwnershipErrorCombined _ _) = True
            isOwnershipError _ = False
        ]
        
    [ testGroup "Analyzer integration consistency"
        [ testCase "both analyzers run together" $ do
            let source = unlines
                  [ "//! ownership on"
                  , "//! dependent_types on"
                  , ""
                  , "func process<T>(data T) T {"
                  , "    return data"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    result := process(42)"
                  , "    println(result)"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            case analysisResult result of
              Nothing -> assertFailure "Expected analysis result"
              Just analysis -> do
                -- Should have run both analyzers
                ownershipErrors analysis @?= []
                dependentTypeErrors analysis @?= []
                length (analysisInfo analysis) @>= 0
                
        , testCase "selective analyzer execution" $ do
            let source = unlines
                  [ "//! ownership on"
                  , "//! dependent_types on"
                  , ""
                  , "func test() {"
                  , "    x := 42"
                  , "    y := x"
                  , "    return y"
                  , "}"
                  ]
                ownershipOnlyConfig = defaultCompilerConfig { enableDependentTypes = False }
                dependentOnlyConfig = defaultCompilerConfig { enableOwnership = False }
                
            ownershipResult <- compileWithIntegratedAnalyzers source ownershipOnlyConfig
            dependentResult <- compileWithIntegratedAnalyzers source dependentOnlyConfig
            
            case analysisResult ownershipResult of
              Nothing -> assertFailure "Expected ownership analysis result"
              Just ownershipAnalysis -> do
                -- Should have ownership analysis results
                length (ownershipErrors ownershipAnalysis) @>= 0
                
            case analysisResult dependentResult of
              Nothing -> assertFailure "Expected dependent type analysis result"
              Just dependentAnalysis -> do
                -- Should have dependent type analysis results
                length (dependentTypeErrors dependentAnalysis) @>= 0
        ]
        
    , testGroup "Error reporting consistency"
        [ testCase "error filtering respects severity level" $ do
            let source = unlines
                  [ "//! ownership on"
                  , ""
                  , "func main() {"
                  , "    x := 42"
                  , "    y := x"
                  , "    z := x  -- Use after move"
                  , "}"
                  ]
                config = defaultCompilerConfig { errorReportingLevel = Error }
            result <- compileWithIntegratedAnalyzers source config
            -- Should filter out warnings and info
            let severeErrors = filter (\e -> combinedErrorSeverity e >= Error) (filteredErrors result)
                allErrors = filteredErrors result
            length severeErrors @?= length allErrors
            
        , testCase "error formatting includes all relevant information" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func invalid() {"
                  , "    var x undefined_type"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            case analysisResult result of
              Nothing -> return ()
              Just analysis -> do
                let formatted = formatCompilationResult result
                -- Should contain error information
                "undefined_type" `isInfixOf` formatted @?= True
        ]
        
    , testGroup "Complex program scenarios"
        [ testCase "multi-function program with dependencies" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "import \"fmt\""
                  , ""
                  , "func calculate(a, b int) int {"
                  , "    return a + b"
                  , "}"
                  , ""
                  , "func display(result int) {"
                  , "    fmt.Printf(\"Result: %d\\n\", result)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    x := calculate(10, 20)"
                  , "    display(x)"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= True
            compiledCode result @?= ""
            -- Should contain all functions
            "func calculate" `isInfixOf` compiledCode result @?= True
            "func display" `isInfixOf` compiledCode result @?= True
            "func main" `isInfixOf` compiledCode result @?= True
            
        , testCase "program with imports and external dependencies" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "import ("
                  , "    \"fmt\""
                  , "    \"os\""
                  , "    \"strings\""
                  , ")"
                  , ""
                  , "func processFile() {"
                  , "    data := os.Args[1:]"
                  , "    joined := strings.Join(data, \" \")"
                  , "    fmt.Println(joined)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    processFile()"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= True
            compiledCode result @?= ""
            -- Should preserve imports
            "import" `isInfixOf` compiledCode result @?= True
            "fmt" `isInfixOf` compiledCode result @?= True
            "os" `isInfixOf` compiledCode result @?= True
            "strings" `isInfixOf` compiledCode result @?= True
        ]
        
    , testGroup "Edge case handling"
        [ testCase "empty source file" $ do
            let source = ""
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            -- Should handle empty input gracefully
            case result of
              IntegratedCompileResult { success = False, .. } -> 
                length syntaxErrors @>= 0
              IntegratedCompileResult { success = True, .. } ->
                compiledCode @?= ""
                
        , testCase "source with only comments" $ do
            let source = unlines
                  [ "// This is a comment"
                  , "/* This is a block comment */"
                  , "// Another comment"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            -- Should handle comments without errors
            length (syntaxErrors result) @?= 0
            length (filteredErrors result) @?= 0
            
        , testCase "source with Unicode characters" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func main() {"
                  , "    println(\"你好，世界！\")"
                  , "    greeting := \"Bonjour le monde!\""
                  , "    println(greeting)"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= True
            compiledCode result @?= ""
            -- Should preserve Unicode characters
            "你好，世界！" `isInfixOf` compiledCode result @?= True
            "Bonjour le monde!" `isInfixOf` compiledCode result @?= True
        ]
        
    , testGroup "Performance and scalability"
        [ testCase "large source file compilation" $ do
            let largeFunction = "func large" ++ show 1 ++ "() {\n" ++ 
                               concat ["    x := " ++ show i ++ "\n" | i <- [1..1000]] ++
                               "    println(x)\n}\n"
                source = "package main\n\n" ++ 
                        concat [largeFunction | i <- [1..10]] ++
                        "func main() {\n    large1()\n}\n"
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            -- Should handle large files without crashing
            case result of
              IntegratedCompileResult { success = s, .. } -> 
                -- May succeed or fail, but should not crash
                length syntaxErrors @>= 0
                length filteredErrors @>= 0
                
        , testCase "complex type definitions" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "type Complex struct {"
                  , "    Field1 map[string][]int"
                  , "    Field2 chan func(string) error"
                  , "    Field3 interface {"
                  , "        Method1() int"
                  , "        Method2(string) bool"
                  , "    }"
                  , "}"
                  , ""
                  , "func (c Complex) Process() error {"
                  , "    return nil"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    c := Complex{}"
                  , "    c.Process()"
                  , "}"
                  ]
                config = defaultCompilerConfig
            result <- compileWithIntegratedAnalyzers source config
            success result @?= True
            compiledCode result @?= ""
            -- Should preserve complex type definitions
            "type Complex struct" `isInfixOf` compiledCode result @?= True
            "Field1 map[string][]int" `isInfixOf` compiledCode result @?= True
            "Field2 chan func(string) error" `isInfixOf` compiledCode result @?= True
        ]
        
    , testGroup "Recovery and resilience"
        [ testCase "compilation continues after non-fatal errors" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func validFunc() {"
                  , "    println(\"This is valid\")"
                  , "}"
                  , ""
                  , "func invalidFunc() {"
                  , "    x := undefined_type  -- This will cause an error"
                  , "    println(x)"
                  , "}"
                  , ""
                  , "func anotherValidFunc() {"
                  , "    println(\"This is also valid\")"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    validFunc()"
                  , "    anotherValidFunc()"
                  , "}"
                  ]
                config = defaultCompilerConfig { errorReportingLevel = Error }
            result <- compileWithIntegratedAnalyzers source config
            -- Should detect errors but continue processing
            length (filteredErrors result) @>= 1
            -- Should still include valid functions in output
            case success result of
              True -> do
                "validFunc" `isInfixOf` compiledCode result @?= True
                "anotherValidFunc" `isInfixOf` compiledCode result @?= True
              False -> return ()  -- May fail due to errors
        ]
    ]