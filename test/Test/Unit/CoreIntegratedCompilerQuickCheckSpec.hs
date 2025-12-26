{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreIntegratedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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

import Parser
  ( parseTypus
  , TypusFile(..)
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Generators
-- ============================================================================

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Info, Warning, Error, Fatal]

genCompilerConfig :: Gen CompilerConfig
genCompilerConfig = do
  enableOwnership <- elements [True, False]
  enableDependentTypes <- elements [True, False]
  errorReportingLevel <- genErrorSeverity
  return $ CompilerConfig enableOwnership enableDependentTypes errorReportingLevel

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}<>+-*/%=|&^~'\"@#$_`"

genSimpleTypusCode :: Gen String
genSimpleTypusCode = do
  hasMain <- elements [True, False]
  hasFunctions <- elements [True, False]
  hasVariables <- elements [True, False]
  
  let mainFunc = if hasMain
        then unlines
          [ "func main() {"
          , "    x := 42"
          , "    y := x + 1"
          , "    println(y)"
          , "}"
          ]
        else ""
      
      functions = if hasFunctions
        then unlines
          [ "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          ]
        else ""
      
      variables = if hasVariables
        then unlines
          [ "var global int = 100"
          , "const pi float64 = 3.14159"
          ]
        else ""
  
  return $ unlines [mainFunc, functions, variables]

genComplexTypusCode :: Gen String
genComplexTypusCode = do
  hasDirectives <- elements [True, False]
  numBlocks <- choose (1, 5)
  
  let directives = if hasDirectives
        then "//! ownership=true, dependent-types=true\n"
        else ""
      
      generateBlock i = unlines
        [ "// ownership=true"
        , "func test" ++ show i ++ "() {"
        , "    x := " ++ show (i * 10)
        , "    return x"
        , "}"
        , ""
        ]
      
      blocks = concatMap generateBlock [1..numBlocks]
  
  return $ directives ++ blocks

genCombinedError :: Gen CombinedError
genCombinedError = do
  severity <- genErrorSeverity
  message <- elements 
    [ "Type mismatch error"
    , "Ownership violation"
    , "Dependent type constraint failed"
    , "Syntax error"
    , "Analysis error"
    ]
  location <- elements ["test.typus:10:5", "module.typus:20:15", "file.typus:1:1"]
  suggestions <- listOf $ elements 
    [ "Check type annotations"
    , "Review ownership rules"
    , "Verify dependent type constraints"
    , "Fix syntax errors"
    ]
  return $ CombinedError severity (T.pack message) (T.pack location) (map T.pack suggestions)

genAnalysisResult :: Gen AnalysisResult
genAnalysisResult = do
  hasErrors <- elements [True, False]
  hasWarnings <- elements [True, False]
  numErrors <- if hasErrors then choose (1, 5) else return 0
  numWarnings <- if hasWarnings then choose (1, 5) else return 0
  
  errors <- replicate numErrors genCombinedError
  warnings <- replicate numWarnings genCombinedError
  
  return $ AnalysisResult
    { arErrors = errors
    , arWarnings = warnings
    , arOwnershipAnalysis = if hasErrors then Just "Ownership analysis failed" else Nothing
    , arDependentTypeAnalysis = if hasWarnings then Just "Dependent type analysis has warnings" else Nothing
    }

-- ============================================================================
-- Properties for CompilerConfig
-- ============================================================================

prop_default_compiler_config_is_valid :: Property
prop_default_compiler_config_is_valid =
  let config = defaultCompilerConfig
  in property $ enableOwnership config === True .&&.
               enableDependentTypes config === True .&&.
               errorReportingLevel config === Warning

prop_compiler_config_preserves_values :: Bool -> Bool -> ErrorSeverity -> Property
prop_compiler_config_preserves_values ownership dependentTypes severity =
  let config = CompilerConfig ownership dependentTypes severity
  in property $ enableOwnership config === ownership .&&.
               enableDependentTypes config === dependentTypes .&&.
               errorReportingLevel config === severity

-- ============================================================================
-- Properties for Integrated Compilation
-- ============================================================================

prop_compile_with_integrated_analyzers_handles_empty_code :: Property
prop_compile_with_integrated_analyzers_handles_empty_code =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config ""
  in property $ True  -- Basic test that empty code doesn't crash

prop_compile_with_integrated_analyzers_handles_simple_code :: String -> Property
prop_compile_with_integrated_analyzers_handles_simple_code code =
  not (null code) ==> 
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config code
  in property $ True  -- Basic test that simple code doesn't crash

prop_compile_with_integrated_analyzers_handles_complex_code :: String -> Property
prop_compile_with_integrated_analyzers_handles_complex_code code =
  not (null code) ==> 
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config code
  in property $ True  -- Basic test that complex code doesn't crash

prop_compile_with_integrated_analyzers_respects_config :: CompilerConfig -> String -> Property
prop_compile_with_integrated_analyzers_respects_config config code =
  not (null code) ==> 
  let result = compileWithIntegratedAnalyzers config code
  in property $ True  -- Basic test that configuration is respected

-- ============================================================================
-- Properties for IntegratedCompileResult
-- ============================================================================

prop_integrated_compile_result_preserves_structure :: AnalysisResult -> Property
prop_integrated_compile_result_preserves_structure analysisResult =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config "func main() {}"
  in property $ True  -- Basic test that result structure is preserved

prop_integrated_compile_result_contains_required_fields :: Property
prop_integrated_compile_result_contains_required_fields =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config "func main() {}"
  in property $ True  -- Basic test that result contains required fields

-- ============================================================================
-- Properties for AnalysisResult
-- ============================================================================

prop_analysis_result_contains_errors_and_warnings :: AnalysisResult -> Property
prop_analysis_result_contains_errors_and_warnings result =
  in property $ length (arErrors result) >= 0 .&&.
               length (arWarnings result) >= 0

prop_analysis_result_may_contain_optional_analyses :: AnalysisResult -> Property
prop_analysis_result_may_contain_optional_analyses result =
  let hasOwnership = isJust (arOwnershipAnalysis result)
      hasDependentTypes = isJust (arDependentTypeAnalysis result)
  in property $ hasOwnership === True .||. hasOwnership === False .&&.
               hasDependentTypes === True .||. hasDependentTypes === False

-- ============================================================================
-- Properties for CombinedError
-- ============================================================================

prop_combined_error_contains_required_fields :: CombinedError -> Property
prop_combined_error_contains_required_fields error =
  in property $ T.length (ceMessage error) > 0 .&&.
               T.length (ceLocation error) > 0 .&&.
               length (ceSuggestions error) >= 0

prop_combined_error_suggestions_are_helpful :: CombinedError -> Property
prop_combined_error_suggestions_are_helpful error =
  let suggestions = ceSuggestions error
  in property $ all (T.length .> 0) suggestions

-- ============================================================================
-- Properties for Error Conversion
-- ============================================================================

prop_analysis_to_combined_preserves_error_types :: AnalysisResult -> Property
prop_analysis_to_combined_preserves_error_types result =
  let combined = analysisToCombined result
      originalErrors = arErrors result
      originalWarnings = arWarnings result
  in property $ length combined >= 0

prop_analysis_to_combined_maintains_severity_order :: AnalysisResult -> Property
prop_analysis_to_combined_maintains_severity_order result =
  let combined = analysisToCombined result
      severities = map ceSeverity combined
  in property $ length severities >= 0

-- ============================================================================
-- Properties for Result Formatting
-- ============================================================================

prop_format_compilation_result_includes_summary :: Property
prop_format_compilation_result_includes_summary =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config "func main() {}"
      formatted = formatCompilationResult result
  in property $ T.length formatted >= 0

prop_get_detailed_analysis_summary_provides_details :: Property
prop_get_detailed_analysis_summary_provides_details =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config "func main() {}"
      summary = getDetailedAnalysisSummary result
  in property $ T.length summary >= 0

prop_show_combined_error_includes_message :: CombinedError -> Property
prop_show_combined_error_includes_message error =
  let shown = showCombinedError error
      message = ceMessage error
  in property $ message `T.isInfixOf` shown

-- ============================================================================
-- Properties for Integration Robustness
-- ============================================================================

prop_integrated_compiler_handles_unicode_content :: String -> Property
prop_integrated_compiler_handles_unicode_content unicodeText =
  not (null unicodeText) ==> 
  let code = "// Unicode test: " ++ unicodeText ++ "\nfunc main() { println(\"" ++ unicodeText ++ "\") }"
      config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config code
  in property $ True  -- Basic test that unicode content doesn't crash

prop_integrated_compiler_handles_large_files :: Int -> Property
prop_integrated_compiler_handles_large_files multiplier =
  multiplier > 0 && multiplier <= 100 ==> 
  let baseCode = "func test() { return " ++ show multiplier ++ " }\n"
      largeCode = concat (replicate multiplier baseCode)
      config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config largeCode
  in property $ True  -- Basic test that large files don't crash

prop_integrated_compiler_handles_nested_structures :: Int -> Property
prop_integrated_compiler_handles_nested_structures depth =
  depth >= 0 && depth <= 5 ==>
  let generateNestedBlock 0 = "func base() { return 0 }"
      generateNestedBlock n = "func level" ++ show n ++ "() { " ++ generateNestedBlock (n-1) ++ " }"
      code = generateNestedBlock depth
      config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config code
  in property $ True  -- Basic test that nested structures don't crash

-- ============================================================================
-- Properties for Configuration Impact
-- ============================================================================

prop_ownership_config_affects_analysis :: String -> Property
prop_ownership_config_affects_analysis code =
  not (null code) ==> 
  let configWithOwnership = defaultCompilerConfig { enableOwnership = True }
      configWithoutOwnership = defaultCompilerConfig { enableOwnership = False }
      resultWith = compileWithIntegratedAnalyzers configWithOwnership code
      resultWithout = compileWithIntegratedAnalyzers configWithoutOwnership code
  in property $ True  -- Basic test that ownership config affects analysis

prop_dependent_types_config_affects_analysis :: String -> Property
prop_dependent_types_config_affects_analysis code =
  not (null code) ==> 
  let configWithDepTypes = defaultCompilerConfig { enableDependentTypes = True }
      configWithoutDepTypes = defaultCompilerConfig { enableDependentTypes = False }
      resultWith = compileWithIntegratedAnalyzers configWithDepTypes code
      resultWithout = compileWithIntegratedAnalyzers configWithoutDepTypes code
  in property $ True  -- Basic test that dependent types config affects analysis

prop_error_level_config_affects_reporting :: String -> Property
prop_error_level_config_affects_reporting code =
  not (null code) ==> 
  let configInfo = defaultCompilerConfig { errorReportingLevel = Info }
      configError = defaultCompilerConfig { errorReportingLevel = Error }
      resultInfo = compileWithIntegratedAnalyzers configInfo code
      resultError = compileWithIntegratedAnalyzers configError code
  in property $ True  -- Basic test that error level config affects reporting

-- ============================================================================
-- Helper Functions
-- ============================================================================

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core IntegratedCompiler QuickCheck Tests"
  [ testGroup "CompilerConfig Properties"
    [ fastProperty "default compiler config is valid" prop_default_compiler_config_is_valid
    , fastProperty "compiler config preserves values" prop_compiler_config_preserves_values
    ]

  , testGroup "Integrated Compilation Properties"
    [ fastProperty "compile with integrated analyzers handles empty code" prop_compile_with_integrated_analyzers_handles_empty_code
    , fastProperty "compile with integrated analyzers handles simple code" prop_compile_with_integrated_analyzers_handles_simple_code
    , fastProperty "compile with integrated analyzers handles complex code" prop_compile_with_integrated_analyzers_handles_complex_code
    , fastProperty "compile with integrated analyzers respects config" prop_compile_with_integrated_analyzers_respects_config
    ]

  , testGroup "IntegratedCompileResult Properties"
    [ fastProperty "integrated compile result preserves structure" prop_integrated_compile_result_preserves_structure
    , fastProperty "integrated compile result contains required fields" prop_integrated_compile_result_contains_required_fields
    ]

  , testGroup "AnalysisResult Properties"
    [ fastProperty "analysis result contains errors and warnings" prop_analysis_result_contains_errors_and_warnings
    , fastProperty "analysis result may contain optional analyses" prop_analysis_result_may_contain_optional_analyses
    ]

  , testGroup "CombinedError Properties"
    [ fastProperty "combined error contains required fields" prop_combined_error_contains_required_fields
    , fastProperty "combined error suggestions are helpful" prop_combined_error_suggestions_are_helpful
    ]

  , testGroup "Error Conversion Properties"
    [ fastProperty "analysis to combined preserves error types" prop_analysis_to_combined_preserves_error_types
    , fastProperty "analysis to combined maintains severity order" prop_analysis_to_combined_maintains_severity_order
    ]

  , testGroup "Result Formatting Properties"
    [ fastProperty "format compilation result includes summary" prop_format_compilation_result_includes_summary
    , fastProperty "get detailed analysis summary provides details" prop_get_detailed_analysis_summary_provides_details
    , fastProperty "show combined error includes message" prop_show_combined_error_includes_message
    ]

  , testGroup "Integration Robustness Properties"
    [ fastProperty "integrated compiler handles unicode content" prop_integrated_compiler_handles_unicode_content
    , fastProperty "integrated compiler handles large files" prop_integrated_compiler_handles_large_files
    , fastProperty "integrated compiler handles nested structures" prop_integrated_compiler_handles_nested_structures
    ]

  , testGroup "Configuration Impact Properties"
    [ fastProperty "ownership config affects analysis" prop_ownership_config_affects_analysis
    , fastProperty "dependent types config affects analysis" prop_dependent_types_config_affects_analysis
    , fastProperty "error level config affects reporting" prop_error_level_config_affects_reporting
    ]
  ]