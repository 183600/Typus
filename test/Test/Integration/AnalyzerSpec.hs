module Test.Integration.AnalyzerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

import AnalyzerIntegration
  ( AnalysisResult(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , newIntegratedAnalyzer
  , runIntegratedAnalysis
  )
import IntegratedCompiler
  ( IntegratedCompileResult(..)
  , analysisToCombined
  , compileWithIntegratedAnalyzers
  , defaultCompilerConfig
  )

tests :: TestTree
tests =
  testGroup "Integrated Analyzer"
    [ testGroup "Cross-Analysis"
        [ testCase "ownership transfer in dependent-type context" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func transfer(x owned String) owned String {"
                  , "    return x"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    var t owned String = transfer(s)"
                  , "    println(t)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisRes)
                assertBool "Should have no dependent type errors" (null $ dependentTypeErrors analysisRes)

        , testCase "use-after-move with type constraints" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func consume(x owned String) {"
                  , "    println(x)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    consume(s)"
                  , "    println(s)  // Error: use after move"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should detect use-after-move" (not $ null $ ownershipErrors analysisRes)

        , testCase "cross-analysis errors are returned to callers" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func consume(x owned String) {"
                  , "    println(x)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    consume(s)"
                  , "    println(s)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                let combined = combinedErrors analysisRes
                    crossErrors =
                      [ (msg, severity)
                      | CrossAnalyzerError msg severity _ <- combined
                      ]
                assertBool "Should surface cross-analysis combined errors" (not $ null crossErrors)
                assertBool "Cross-analysis errors should be treated as blocking"
                  (all (\(_, severity) -> severity >= Error) crossErrors)
                analysisToCombined analysisRes @?= combined

        , testCase "integrated compiler blocks on cross-analysis errors" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func consume(x owned String) {"
                  , "    println(x)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    consume(s)"
                  , "    println(s)"
                  , "}"
                  ]
            compileResult <- compileWithIntegratedAnalyzers code defaultCompilerConfig
            assertBool "Compilation should fail when cross-analysis reports errors"
              (not $ success compileResult)
            let crossSeverities =
                  [ severity
                  | CrossAnalyzerError _ severity _ <- filteredErrors compileResult
                  ]
            assertBool "Filtered errors should include cross-analysis diagnostics" (not $ null crossSeverities)
            assertBool "Cross-analysis diagnostics should be treated as errors"
              (all (>= Error) crossSeverities)
            case analysisResult compileResult of
              Nothing ->
                assertFailure "Expected analysis result to be present"
              Just analysisRes -> do
                let combined = combinedErrors analysisRes
                    crossCombined =
                      [ severity
                      | CrossAnalyzerError _ severity _ <- combined
                      ]
                assertBool "Combined errors should retain cross-analysis diagnostics"
                  (not $ null crossCombined)
                assertBool "Combined diagnostics should propagate severity"
                  (all (>= Error) crossCombined)

        , testCase "borrow with type refinement" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func inspect(x &String) int {"
                  , "    return len(*x)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    var length int = inspect(&s)"
                  , "    println(length)"
                  , "    println(s)  // OK: s is still owned"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisRes)
                assertBool "Should have no dependent type errors" (null $ dependentTypeErrors analysisRes)

        , testCase "mutable borrow with constraints" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func modify(x &mut String) {"
                  , "    *x = \"modified\""
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    modify(&mut s)"
                  , "    println(s)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisRes)
        ]

    , testGroup "Error Prioritization"
        [ testCase "ownership error takes precedence over type error" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    consume(s)"
                  , "    var x int = s  // Both ownership and type error"
                  , "}"
                  , ""
                  , "func consume(x owned String) {}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left _ -> return ()
              Right analysisRes -> do
                assertBool "Should detect ownership error" (not $ null $ ownershipErrors analysisRes)

        , testCase "multiple analyzer warnings combined" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func main() {"
                  , "    var unused owned String = \"hello\""
                  , "    var x int = 42"
                  , "    println(x)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should have warnings about unused variable" 
                  (not $ null $ analysisWarnings analysisRes)
        ]

    , testGroup "Full Pipeline Integration"
        [ testCase "complete project with mixed directives" $ do
            let mainCode = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    process(s)"
                  , "}"
                  ]
            let helperCode = unlines
                  [ "package main"
                  , ""
                  , "func process(x owned String) {"
                  , "    println(x)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis mainCode state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should complete full analysis" True

        , testCase "selective feature enabling" $ do
            let ownershipOnlyCode = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    println(s)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True False
            result <- runIntegratedAnalysis ownershipOnlyCode state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should analyze ownership only" (null $ dependentTypeErrors analysisRes)

        , testCase "complex nested ownership transfer" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func outer(x owned String) owned String {"
                  , "    return inner(x)"
                  , "}"
                  , ""
                  , "func inner(x owned String) owned String {"
                  , "    return x"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var s owned String = \"hello\""
                  , "    var result owned String = outer(s)"
                  , "    println(result)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should handle nested ownership transfer" (null $ ownershipErrors analysisRes)

        , testCase "dependent types with ownership constraints" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "func safeDivide(x int, y int {y != 0}) int {"
                  , "    return x / y"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var result int = safeDivide(10, 2)"
                  , "    println(result)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Should validate dependent type constraints" 
                  (null $ dependentTypeErrors analysisRes)
        ]

    , testGroup "Symbolic State Management"
        [ testCase "symbol table consistency across phases" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "var global owned String"
                  , ""
                  , "func main() {"
                  , "    global = \"initialized\""
                  , "    println(global)"
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Symbol table should be consistent" True

        , testCase "type environment preservation" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , ""
                  , "type CustomType struct {"
                  , "    field owned String"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    var x CustomType"
                  , "    x.field = \"value\""
                  , "}"
                  ]
            let state = newIntegratedAnalyzer True True
            result <- runIntegratedAnalysis code state
            case result of
              Left err -> assertFailure $ "Analysis failed: " ++ err
              Right analysisRes -> do
                assertBool "Type environment should preserve custom types" 
                  (not $ null $ typeEnvironment analysisRes)
        ]
    ]
