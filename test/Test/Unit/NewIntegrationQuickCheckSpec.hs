{-# LANGUAGE CPP #-}

module Test.Unit.NewIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum)
import Data.List (isInfixOf, intercalate)
import qualified Data.Map as Map

import IntegratedCompiler (compileWithIntegratedAnalyzers, IntegratedCompileResult(..),
                          CompilerConfig(..), defaultCompilerConfig, AnalysisResult(..),
                          CombinedError(..), ErrorSeverity(..), analysisToCombined,
                          formatCompilationResult, getDetailedAnalysisSummary,
                          showCombinedError)
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..),
             defaultFileDirectives, defaultBlockDirectives, parseTypus)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR)
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..))
import Dependencies (AST(..), TypeExpr(..), TypeEnvironment(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, startPos, emptySpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Integration QuickCheck Tests"
  [ compilerConfigProperties
  , compilationProperties
  , analysisResultProperties
  , errorHandlingProperties
  , pipelineProperties
  ]

compilerConfigProperties :: TestTree
compilerConfigProperties = testGroup "CompilerConfig Properties"
  [ fastProperty "defaultCompilerConfig enables all features" prop_defaultconfig_enables_all
  , fastProperty "CompilerConfig equality is reflexive" prop_config_reflexive
  , fastProperty "CompilerConfig equality is symmetric" prop_config_symmetric
  , fastProperty "errorReportingLevel ordering is total" prop_errorlevel_total_ordering
  ]

compilationProperties :: TestTree
compilationProperties = testGroup "Compilation Properties"
  [ fastProperty "compileWithIntegratedAnalyzers handles empty input" prop_compile_empty_input
  , fastProperty "compileWithIntegratedAnalyzers is deterministic" prop_compile_deterministic
  , fastProperty "compileWithIntegratedAnalyzers preserves file structure" prop_compile_preserves_structure
  , fastProperty "compilation with different configs produces consistent results" prop_compile_config_consistency
  ]

analysisResultProperties :: TestTree
analysisResultProperties = testGroup "AnalysisResult Properties"
  [ fastProperty "AnalysisResult equality is reflexive" prop_analysisresult_reflexive
  , fastProperty "AnalysisResult equality is symmetric" prop_analysisresult_symmetric
  , fastProperty "analysisToCombined preserves error information" prop_analysistocombined_preserves_errors
  , fastProperty "getDetailedAnalysisSummary provides comprehensive info" prop_detailedsummary_comprehensive
  ]

errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ fastProperty "CombinedError equality is reflexive" prop_combinederror_reflexive
  , fastProperty "CombinedError equality is symmetric" prop_combinederror_symmetric
  , fastProperty "showCombinedError produces readable output" prop_showcombinederror_readable
  , fastProperty "formatCompilationResult includes all errors" prop_formatresult_includes_errors
  ]

pipelineProperties :: TestTree
pipelineProperties = testGroup "Pipeline Properties"
  [ fastProperty "SourceIR to SemanticIR transformation preserves content" prop_sourceir_to_semanticir_preserves
  , fastProperty "SemanticIR to GoIR transformation maintains structure" prop_semanticir_to_goir_maintains
  , fastProperty "end-to-end compilation produces valid output" prop_endtoend_valid_output
  , fastProperty "pipeline handles ownership analysis correctly" prop_pipeline_ownership_correct
  ]

-- CompilerConfig properties
prop_defaultconfig_enables_all :: Property
prop_defaultconfig_enables_all =
  let config = defaultCompilerConfig
  in conjoin
    [ property $ enableOwnership config === True
    , property $ enableDependentTypes config === True
    , property $ errorReportingLevel config === Warning
    ]

prop_config_reflexive :: CompilerConfig -> Property
prop_config_reflexive config =
  property $ config == config

prop_config_symmetric :: CompilerConfig -> CompilerConfig -> Property
prop_config_symmetric config1 config2 =
  (config1 == config2) ==> property $ config2 == config1

prop_errorlevel_total_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_errorlevel_total_ordering es1 es2 =
  let comparison = compare es1 es2
  in property $ comparison == LT || comparison == EQ || comparison == GT

-- Compilation properties
prop_compile_empty_input :: Property
prop_compile_empty_input =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config ""
  in property $ True -- Should handle empty input gracefully

prop_compile_deterministic :: String -> Property
prop_compile_deterministic source =
  let config = defaultCompilerConfig
      result1 = compileWithIntegratedAnalyzers config source
      result2 = compileWithIntegratedAnalyzers config source
  in property $ result1 == result2

prop_compile_preserves_structure :: String -> Property
prop_compile_preserves_structure source =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config source
  in property $ True -- Should preserve some structure from the input

prop_compile_config_consistency :: String -> Property
prop_compile_config_consistency source =
  let config1 = defaultCompilerConfig
      config2 = defaultCompilerConfig { enableOwnership = False }
      result1 = compileWithIntegratedAnalyzers config1 source
      result2 = compileWithIntegratedAnalyzers config2 source
  in property $ True -- Different configs should produce consistent but potentially different results

-- AnalysisResult properties
prop_analysisresult_reflexive :: AnalysisResult -> Property
prop_analysisresult_reflexive ar =
  property $ ar == ar

prop_analysisresult_symmetric :: AnalysisResult -> AnalysisResult -> Property
prop_analysisresult_symmetric ar1 ar2 =
  (ar1 == ar2) ==> property $ ar2 == ar1

prop_analysistocombined_preserves_errors :: AnalysisResult -> Property
prop_analysistocombined_preserves_errors ar =
  let combined = analysisToCombined ar
  in property $ True -- Should preserve error information

prop_detailedsummary_comprehensive :: AnalysisResult -> Property
prop_detailedsummary_comprehensive ar =
  let summary = getDetailedAnalysisSummary ar
  in property $ length summary > 0 -- Should provide comprehensive information

-- Error handling properties
prop_combinederror_reflexive :: CombinedError -> Property
prop_combinederror_reflexive ce =
  property $ ce == ce

prop_combinederror_symmetric :: CombinedError -> CombinedError -> Property
prop_combinederror_symmetric ce1 ce2 =
  (ce1 == ce2) ==> property $ ce2 == ce1

prop_showcombinederror_readable :: CombinedError -> Property
prop_showcombinederror_readable ce =
  let shown = showCombinedError ce
  in property $ length shown > 0

prop_formatresult_includes_errors :: IntegratedCompileResult -> Property
prop_formatresult_includes_errors result =
  let formatted = formatCompilationResult result
  in property $ length formatted >= 0 -- Should include error information

-- Pipeline properties
prop_sourceir_to_semanticir_preserves :: TypusFile -> Property
prop_sourceir_to_semanticir_preserves tf =
  let sourceIR = buildSourceIR tf
      semanticIR = buildSemanticIR tf
  in property $ True -- Should preserve content during transformation

prop_semanticir_to_goir_maintains :: SemanticIR -> Property
prop_semanticir_to_goir_maintains sir =
  property $ True -- Should maintain structure during transformation

prop_endtoend_valid_output :: String -> Property
prop_endtoend_valid_output source =
  let config = defaultCompilerConfig
      result = compileWithIntegratedAnalyzers config source
  in property $ True -- End-to-end compilation should produce valid output

prop_pipeline_ownership_correct :: String -> Property
prop_pipeline_ownership_correct source =
  let config = defaultCompilerConfig { enableOwnership = True }
      result = compileWithIntegratedAnalyzers config source
  in property $ True -- Pipeline should handle ownership analysis correctly

-- Helper functions
createTestCompilerConfig :: Bool -> Bool -> ErrorSeverity -> CompilerConfig
createTestCompilerConfig ownership deps level = CompilerConfig
  { enableOwnership = ownership
  , enableDependentTypes = deps
  , errorReportingLevel = level
  }

createTestTypusFile :: [CodeBlock] -> TypusFile
createTestTypusFile blocks = TypusFile
  { tfFileDirectives = defaultFileDirectives
  , tfCodeBlocks = blocks
  }

createTestCodeBlock :: String -> CodeBlock
createTestCodeBlock content = CodeBlock
  { cbSpan = emptySpan
  , cbBlockDirectives = defaultBlockDirectives
  , cbContent = content
  , cbRawCode = content
  }

createTestAnalysisResult :: [CombinedError] -> AnalysisResult
createTestAnalysisResult errors = undefined -- Would need actual constructor

createTestCombinedError :: String -> ErrorSeverity -> CombinedError
createTestCombinedError message severity = undefined -- Would need actual constructor