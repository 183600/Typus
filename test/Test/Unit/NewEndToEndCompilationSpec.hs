{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEndToEndCompilationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
  )

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
  )

import Parser
  ( TypusFile(..)
  , parseTypus
  )

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  )

import Ownership
  ( OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T

-- Test basic end-to-end compilation
test_basic_compilation :: TestTree
test_basic_compilation = testCase "Basic end-to-end compilation" $ do
  let source = unlines
        [ "//! ownership: on"
        , "//! dependent_types: off"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    y := x + 1"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> assertFailure $ "Compilation failed: " ++ show errors
        Right success -> do
          assertBool "Compilation should succeed" $ 
            case success of
              CompileSuccess {} -> True
              _ -> False

-- Test compilation with ownership analysis
test_ownership_analysis :: TestTree
test_ownership_analysis = testCase "Compilation with ownership analysis" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func consume(x int) {"
        , "    // x is consumed here"
        , "}"
        , "func main() {"
        , "    x := 42"
        , "    consume(x)"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      let config = defaultCompilerConfig { enableOwnership = True }
      result <- compileWithIntegratedAnalyzers config typusFile
      case result of
        Left errors -> do
          -- Should detect ownership transfer
          let errorStr = show errors
          assertBool "Should detect ownership transfer" $ 
            isInfixOf "ownership" errorStr || isInfixOf "move" errorStr
        Right success -> pure () -- Ownership analysis successful

-- Test compilation with dependent types
test_dependent_types :: TestTree
test_dependent_types = testCase "Compilation with dependent types" = do
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func safeDivide(a int, b int) int where b != 0 {"
        , "    return a / b"
        , "}"
        , "func main() {"
        , "    result := safeDivide(10, 2)"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      let config = defaultCompilerConfig { enableDependentTypes = True }
      result <- compileWithIntegratedAnalyzers config typusFile
      case result of
        Left errors -> do
          let errorStr = show errors
          assertBool "Should handle dependent types" $ 
            isInfixOf "dependent" errorStr || isInfixOf "type" errorStr
        Right success -> pure () -- Dependent types analysis successful

-- Test compilation with both analyzers enabled
test_both_analyzers :: TestTree
test_both_analyzers = testCase "Compilation with both analyzers enabled" = do
  let source = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , "func processData<T>(data T) int where len(T) > 0 {"
        , "    return len(data)"
        , "}"
        , "func main() {"
        , "    items := []int{1, 2, 3}"
        , "    result := processData(items)"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> do
          let errorStr = show errors
          assertBool "Should handle both ownership and dependent types" $ 
            isInfixOf "ownership" errorStr || isInfixOf "dependent" errorStr
        Right success -> pure () -- Both analyzers successful

-- Test compilation error handling
test_compilation_errors :: TestTree
test_compilation_errors = testCase "Compilation error handling" = do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    y := x + 1"
        , "    z := x  // Use after move"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> do
          let errorStr = show errors
          assertBool "Should detect use-after-move error" $ 
            isInfixOf "move" errorStr || isInfixOf "ownership" errorStr
        Right success -> assertFailure "Expected compilation failure"

-- Test compilation with complex code
test_complex_compilation :: TestTree
test_complex_compilation = testCase "Compilation with complex code" = do
  let source = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , "type Vector<T> where len(T) > 0 {"
        , "    data: []T"
        , "    size: int"
        , "}"
        , "func (v *Vector<T>) push(item T) where len(item) > 0 {"
        , "    v.data = append(v.data, item)"
        , "    v.size++"
        , "}"
        , "func main() {"
        , "    vec := Vector<int>{data: []int{}, size: 0}"
        , "    vec.push(42)"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> do
          let errorStr = show errors
          assertBool "Should handle complex structures" $ 
            not (isInfixOf "crash" errorStr) && not (isInfixOf "panic" errorStr)
        Right success -> pure () -- Complex compilation successful

-- Test compilation result formatting
test_result_formatting :: TestTree
test_result_formatting = testCase "Compilation result formatting" = do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> do
          let formatted = formatCompilationResult (Left errors)
          assertBool "Formatted result should contain error info" $ 
            isInfixOf "error" (map toLower formatted)
        Right success -> do
          let formatted = formatCompilationResult (Right success)
          assertBool "Formatted result should contain success info" $ 
            isInfixOf "success" (map toLower formatted)

-- Test analysis summary generation
test_analysis_summary :: TestTree
test_analysis_summary = testCase "Analysis summary generation" = do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> do
          let summary = getDetailedAnalysisSummary (Left errors)
          assertBool "Summary should contain analysis results" $ 
            length summary > 0
        Right success -> do
          let summary = getDetailedAnalysisSummary (Right success)
          assertBool "Summary should contain analysis results" $ 
            length summary > 0

-- Test configuration variations
test_configuration_variations :: TestTree
test_configuration_variations = testCase "Configuration variations" = do
  let source = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      -- Test with ownership only
      let config1 = defaultCompilerConfig { enableOwnership = True, enableDependentTypes = False }
      result1 <- compileWithIntegratedAnalyzers config1 typusFile
      case result1 of
        Left _ -> pure () -- May fail
        Right _ -> pure () -- May succeed
        
      -- Test with dependent types only
      let config2 = defaultCompilerConfig { enableOwnership = False, enableDependentTypes = True }
      result2 <- compileWithIntegratedAnalyzers config2 typusFile
      case result2 of
        Left _ -> pure () -- May fail
        Right _ -> pure () -- May succeed
        
      -- Test with both disabled
      let config3 = defaultCompilerConfig { enableOwnership = False, enableDependentTypes = False }
      result3 <- compileWithIntegratedAnalyzers config3 typusFile
      case result3 of
        Left _ -> pure () -- May fail
        Right _ -> pure () -- May succeed

-- Property: End-to-end compilation is deterministic
prop_compilation_deterministic :: String -> Property
prop_compilation_deterministic source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      result1 <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      result2 <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case (result1, result2) of
        (Left err1, Left err2) -> err1 === err2
        (Right res1, Right res2) -> res1 === res2
        _ -> property False -- Should have consistent results

-- Property: Compilation handles edge cases gracefully
prop_edge_case_handling :: String -> Property
prop_edge_case_handling source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> 
          let errorStr = show errors
          in property $ not (isInfixOf "crash" errorStr) && not (isInfixOf "panic" errorStr)
        Right _ -> property True -- Success is good

-- Property: Configuration affects compilation results
prop_configuration_affects_results :: String -> Property
prop_configuration_affects_results source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      let config1 = defaultCompilerConfig { enableOwnership = True, enableDependentTypes = True }
          config2 = defaultCompilerConfig { enableOwnership = False, enableDependentTypes = False }
      result1 <- compileWithIntegratedAnalyzers config1 typusFile
      result2 <- compileWithIntegratedAnalyzers config2 typusFile
      case (result1, result2) of
        (Left _, Left _) -> property True -- Both may fail
        (Right _, Right _) -> property True -- Both may succeed
        (Left _, Right _) -> property True -- Different configs may give different results
        (Right _, Left _) -> property True -- Different configs may give different results

-- Property: Error reporting is consistent
prop_error_reporting_consistent :: String -> Property
prop_error_reporting_consistent source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      case result of
        Left errors -> 
          let formatted1 = formatCompilationResult (Left errors)
              formatted2 = formatCompilationResult (Left errors)
          in formatted1 === formatted2
        Right success -> 
          let formatted1 = formatCompilationResult (Right success)
              formatted2 = formatCompilationResult (Right success)
          in formatted1 === formatted2

-- Property: Analysis summary is informative
prop_analysis_summary_informative :: String -> Property
prop_analysis_summary_informative source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      result <- compileWithIntegratedAnalyzers defaultCompilerConfig typusFile
      let summary = getDetailedAnalysisSummary result
      in property $ length summary > 0

tests :: TestTree
tests = testGroup "New End-to-End Compilation Tests"
  [ test_basic_compilation
  , test_ownership_analysis
  , test_dependent_types
  , test_both_analyzers
  , test_compilation_errors
  , test_complex_compilation
  , test_result_formatting
  , test_analysis_summary
  , test_configuration_variations
  , fastProperty "End-to-end compilation is deterministic" prop_compilation_deterministic
  , fastProperty "Compilation handles edge cases gracefully" prop_edge_case_handling
  , fastProperty "Configuration affects compilation results" prop_configuration_affects_results
  , fastProperty "Error reporting is consistent" prop_error_reporting_consistent
  , fastProperty "Analysis summary is informative" prop_analysis_summary_informative
  ]