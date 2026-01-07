module Test.Unit.AnalyzerTypesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.Types

-- Test analyzer state creation
prop_analyzer_state_creation :: Property
prop_analyzer_state_creation =
  let state1 = createAnalyzerState
      state2 = createAnalyzerState
  in property $ getAnalyzerStateId state1 /= getAnalyzerStateId state2

-- Test analysis context management
prop_analysis_context_management :: String -> Property
prop_analysis_context_management contextInfo =
  let state = createAnalyzerState
      stateWithContext = setAnalysisContext state contextInfo
      retrievedContext = getAnalysisContext stateWithContext
  in property $ retrievedContext === contextInfo

-- Test analysis result accumulation
prop_analysis_result_accumulation :: [String] -> Property
prop_analysis_result_accumulation results =
  let state = createAnalyzerState
      stateWithResults = foldl addAnalysisResult state results
      allResults = getAllAnalysisResults stateWithResults
  in property $ allResults === results

-- Test analysis error handling
prop_analysis_error_handling :: String -> Property
prop_analysis_error_handling errorMsg =
  let state = createAnalyzerState
      error = createAnalysisError errorMsg
      stateWithError = addAnalysisError state error
      errors = getAnalysisErrors stateWithError
  in property $ error `elem` errors

-- Test analysis warnings
prop_analysis_warnings :: [String] -> Property
prop_analysis_warnings warningMessages =
  let state = createAnalyzerState
      warnings = map createAnalysisWarning warningMessages
      stateWithWarnings = foldl addAnalysisWarning state warnings
      retrievedWarnings = getAnalysisWarnings stateWithWarnings
  in property $ retrievedWarnings === warnings

tests :: TestTree
tests = testGroup "Analyzer Types Tests"
  [ testProperty "analyzer state creation" prop_analyzer_state_creation
  , testProperty "analysis context management" prop_analysis_context_management
  , testProperty "analysis result accumulation" prop_analysis_result_accumulation
  , testProperty "analysis error handling" prop_analysis_error_handling
  , testProperty "analysis warnings" prop_analysis_warnings
  ]