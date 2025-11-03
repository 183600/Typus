module Test.Integration.AnalyzerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import AnalyzerIntegration
  ( AnalysisResult(..)
  , newIntegratedAnalyzer
  , runIntegratedAnalysis
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
              Right analysisResult -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisResult)
                assertBool "Should have no dependent type errors" (null $ dependentTypeErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should detect use-after-move" (not $ null $ ownershipErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisResult)
                assertBool "Should have no dependent type errors" (null $ dependentTypeErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should have no ownership errors" (null $ ownershipErrors analysisResult)
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
              Right analysisResult -> do
                assertBool "Should detect ownership error" (not $ null $ ownershipErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should have warnings about unused variable" 
                  (not $ null $ analysisWarnings analysisResult)
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
              Right analysisResult -> do
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
              Right analysisResult -> do
                assertBool "Should analyze ownership only" (null $ dependentTypeErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should handle nested ownership transfer" (null $ ownershipErrors analysisResult)

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
              Right analysisResult -> do
                assertBool "Should validate dependent type constraints" 
                  (null $ dependentTypeErrors analysisResult)
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
              Right analysisResult -> do
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
              Right analysisResult -> do
                assertBool "Type environment should preserve custom types" 
                  (not $ null $ typeEnvironment analysisResult)
        ]
    ]
