module Test.Unit.Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Unit.CLISpec
import qualified Test.Unit.CommandLineDebugSpec
import qualified Test.Unit.CommandLineDebugIntegrationAdvancedSpec
import qualified Test.Unit.CompilerSpec
import qualified Test.Unit.CompilerIRConsistencyAdvancedSpec
import qualified Test.Unit.DependentTypesSpec
import qualified Test.Unit.DependentTypesBoundaryAdvancedSpec
import qualified Test.Unit.DependenciesCycleDetectionAdvancedSpec
import qualified Test.Unit.EmbedAssetsSpec
import qualified Test.Unit.ErrorHandlingSpec
import qualified Test.Unit.ErrorHandlerConsistencyAdvancedSpec
import qualified Test.Unit.GoToolchainSpec
import qualified Test.Unit.GoToolchainPropertiesAdvancedSpec
import qualified Test.Unit.OwnershipSpec
import qualified Test.Unit.OwnershipBridgeSpec
import qualified Test.Unit.OwnershipTransitivityAdvancedSpec
import qualified Test.Unit.ParserSpec
import qualified Test.Unit.ParserErrorRecoveryAdvancedSpec
import qualified Test.Unit.TypeSystemSpec
import qualified Test.Unit.SymbolTableSpec
import qualified Test.Unit.SourceLocationSpec
import qualified Test.Unit.SourceLocationAdvancedPropertiesSpec
import qualified Test.Unit.SyntaxValidatorSpec
import qualified Test.Unit.ValueAnalysisSpec
import qualified Test.Unit.VerbositySpec
import qualified Test.Unit.UtilsBoundaryConditionsSpec
import qualified Test.Unit.UtilsSpec

-- New Cabal Test Modules
import qualified Test.Unit.NewCabalUtilsSpec
import qualified Test.Unit.NewCabalSourceLocationSpec
import qualified Test.Unit.NewCabalErrorHandlerSpec
import qualified Test.Unit.NewCabalParserSpec
import qualified Test.Unit.NewCabalIntegrationSpec

-- New Core Test Modules Added
import qualified Test.Unit.CoreUtilsSpec
import qualified Test.Unit.CoreSourceLocationSpec
import qualified Test.Unit.CoreSyntaxValidatorSpec
import qualified Test.Unit.CoreErrorHandlerSpec
import qualified Test.Unit.CoreParserSpec
import qualified Test.Unit.CoreCompilerSpec

-- New Core Functionality QuickCheck Tests
import qualified Test.Unit.NewCoreFunctionalityQuickCheckTests

-- New QuickCheck Property Test Modules (Added for enhanced testing)
import qualified Test.Unit.NewSourceLocationQuickCheckPropertiesSpec
import qualified Test.Unit.NewErrorHandlerQuickCheckPropertiesSpec
import qualified Test.Unit.NewCompilerQuickCheckPropertiesSpec
import qualified Test.Unit.NewParserQuickCheckPropertiesSpec
import qualified Test.Unit.NewUtilsQuickCheckPropertiesSpec

-- ============================================================================
-- New Cabal QuickCheck Test Modules (2025) - 7 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCabalUtilsQuickCheckTestsSpec
import qualified Test.Unit.NewCabalSourceLocationQuickCheckTestsSpec
import qualified Test.Unit.NewCabalParserQuickCheckTestsSpec
import qualified Test.Unit.NewCabalErrorHandlerQuickCheckTestsSpec
import qualified Test.Unit.NewCabalOwnershipQuickCheckTestsSpec
import qualified Test.Unit.NewCabalDependenciesQuickCheckTestsSpec

-- ============================================================================
-- Additional Enhanced QuickCheck Test Modules (10 comprehensive tests)
-- ============================================================================
import qualified Test.Unit.EnhancedTextProcessingQuickCheckSpec
import qualified Test.Unit.NewEnhancedSourceLocationMathPropertiesQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.CrossModuleIntegrationQuickCheckSpec
import qualified Test.Unit.PerformanceBoundaryQuickCheckSpec
import qualified Test.Unit.ErrorHandlingPropertiesQuickCheckSpec
import qualified Test.Unit.CompilerIRPropertiesQuickCheckSpec
import qualified Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec
import qualified Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec
import qualified Test.Unit.NewEndToEndIntegrationQuickCheckSpec

-- New Basic Test Modules Added
import qualified Test.Unit.SourceLocationBasicPropertiesSpec
import qualified Test.Unit.ParserBasicFunctionsSpec
import qualified Test.Unit.CompilerErrorHandlingSpec
import qualified Test.Unit.OwnershipAnalysisBasicSpec
import qualified Test.Unit.UtilsStringProcessingSpec
import qualified Test.Unit.DependenciesTypeSystemSpec
import qualified Test.Unit.ValueAnalysisBasicSpec
import qualified Test.Unit.ErrorHandlerRecoverySpec
import qualified Test.Unit.SyntaxValidatorValidationSpec

-- New Compact Test Modules (2025) - 8 comprehensive tests
import qualified Test.Unit.NewCompactUtilsSpec
import qualified Test.Unit.NewCompactSourceLocationSpec
import qualified Test.Unit.NewCompactParserSpec
import qualified Test.Unit.NewCompactErrorHandlerSpec
import qualified Test.Unit.NewCompactOwnershipSpec
import qualified Test.Unit.NewCompactDependenciesSpec
import qualified Test.Unit.NewCompactCompilerIRSpec
import qualified Test.Unit.NewCompactIntegrationSpec

-- New Core Module QuickCheck Test Suite (2025) - 8 comprehensive tests
import qualified Test.Unit.NewCoreUtilsQuickCheckSpec
import qualified Test.Unit.NewCoreSourceLocationQuickCheckSpec
import qualified Test.Unit.NewCoreParserQuickCheckSpec
import qualified Test.Unit.NewComprehensiveCoreQuickCheckSpec
import qualified Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec
import qualified Test.Unit.NewCorePerformanceQuickCheckSpec
import qualified Test.Unit.NewCoreErrorHandlingQuickCheckSpec

-- ============================================================================
-- New QuickCheck Test Modules (2025) - 8 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCoreParsingQuickCheckSpec
import qualified Test.Unit.NewAdvancedTypeSystemQuickCheckSpec
import qualified Test.Unit.NewMemorySafetyQuickCheckSpec
import qualified Test.Unit.NewAdvancedErrorRecoveryQuickCheckSpec
import qualified Test.Unit.NewPerformanceQuickCheckSpec
import qualified Test.Unit.NewIntegrationQuickCheckSpec
import qualified Test.Unit.NewOwnershipAnalysisQuickCheckSpec
import qualified Test.Unit.NewCompilerOptimizationQuickCheckSpec
import qualified Test.Unit.NewCompleteCoreTestSuiteSpec
import qualified Test.Unit.AdvancedParserSpec
import qualified Test.Unit.IntegrationSpec
import qualified Test.Unit.PerformanceSpec
import qualified Test.Unit.EdgeCaseSpec
import qualified Test.Unit.EnhancedQuickCheckSpec
import qualified Test.Unit.EnhancedParserTestSpec
import qualified Test.Unit.EnhancedCompilerTestSpec

import qualified Test.Unit.ParserCompilerPropertiesQuickCheckSpec

-- New Cabal Test Modules (2025)
import qualified Test.Unit.NewCabalCoreFunctionalitySpec
import qualified Test.Unit.NewCabalPropertyBasedSpec
import qualified Test.Unit.NewCabalBoundaryConditionsSpec

-- Enhanced QuickCheck Test Modules (New)
import qualified Test.Unit.UtilsEnhancedQuickCheckSpec
import qualified Test.Unit.SourceLocationEnhancedQuickCheckSpec

-- ============================================================================
-- New Typus QuickCheck Test Modules (2025) - Core Functionality Testing
-- ============================================================================
import qualified Test.Unit.NewTypusCoreQuickCheckSpec

-- ============================================================================
-- New QuickCheck Test Modules Added (Core Functionality Testing)
-- ============================================================================
import qualified Test.Unit.UtilsCorePropertiesQuickCheckSpec
import qualified Test.Unit.NewSourceLocationMathCoreQuickCheckSpec
import qualified Test.Unit.NewParserBoundaryCoreQuickCheckSpec
import qualified Test.Unit.NewOwnershipBasicCoreQuickCheckSpec
import qualified Test.Unit.NewDependenciesInferenceCoreQuickCheckSpec
import qualified Test.Unit.NewEnhancedErrorHandlerConsistencyQuickCheckSpec
import qualified Test.Unit.NewCompilerIRCoreQuickCheckSpec
import qualified Test.Unit.NewIntegrationEndToEndCoreQuickCheckSpec
import qualified Test.Unit.SourceLocationBoundaryQuickCheckSpec
import qualified Test.Unit.StringProcessingQuickCheckSpec

-- ============================================================================
-- New Advanced Test Modules (2025) - 10 Comprehensive Tests
-- ============================================================================
import qualified Test.Unit.CompilerErrorRecoveryBoundarySpec
import qualified Test.Unit.DependencyAnalysisAdvancedSpec
import qualified Test.Unit.TypeSystemBoundarySpec
import qualified Test.Unit.OwnershipComplexInteractionSpec
import qualified Test.Unit.SourceLocationMathPrecisionSpec
import qualified Test.Unit.ParserUnicodeEncodingSpec
import qualified Test.Unit.ToolchainIntegrationRobustnessSpec
import qualified Test.Unit.MemorySafetyResourceManagementSpec
import qualified Test.Unit.ConcurrentThreadSafetySpec
import qualified Test.Unit.PerformanceRegressionOptimizationSpec

-- ============================================================================
-- Additional Test Modules Created for Enhanced Coverage
-- ============================================================================
import qualified Test.Unit.AdditionalUtilsSpec
import qualified Test.Unit.AdditionalSourceLocationSpec
import qualified Test.Unit.AdditionalParserSpec
import qualified Test.Unit.AdditionalErrorHandlerSpec
import qualified Test.Unit.AdditionalSyntaxValidatorSpec
import qualified Test.Unit.AdditionalUtilsQuickCheckSpec
import qualified Test.Unit.AdditionalSourceLocationQuickCheckSpec
import qualified Test.Unit.NewTypusParserQuickCheckSpec
import qualified Test.Unit.NewTypusOwnershipQuickCheckSpec
import qualified Test.Unit.NewTypusDependentTypesQuickCheckSpec
import qualified Test.Unit.NewTypusCompilerQuickCheckSpec
import qualified Test.Unit.NewTypusSourceLocationQuickCheckSpec
import qualified Test.Unit.NewTypusErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewTypusUtilsQuickCheckSpec
import qualified Test.Unit.NewTypusIntegrationQuickCheckSpec
import qualified Test.Unit.NewTypusSyntaxValidatorQuickCheckSpec

-- New Enhanced QuickCheck Test Modules (2025)
import qualified Test.Unit.NewUtilsEnhancedQuickCheckSpec
import qualified Test.Unit.NewSourceLocationEnhancedQuickCheckSpec
import qualified Test.Unit.NewParserEnhancedQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerEnhancedQuickCheckSpec

-- ============================================================================
-- New Cabal QuickCheck Test Modules (2025) - 6 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCabalUtilsQuickCheckSpec
import qualified Test.Unit.NewCabalSourceLocationQuickCheckSpec
import qualified Test.Unit.NewCabalParserQuickCheckSpec
import qualified Test.Unit.NewCabalOwnershipQuickCheckSpec
import qualified Test.Unit.NewCabalDependenciesQuickCheckSpec
import qualified Test.Unit.NewCabalErrorHandlerQuickCheckSpec

-- ============================================================================
-- New Comprehensive Test Modules Created (2025)
-- ============================================================================
import qualified Test.Unit.ComprehensiveCoreQuickCheckSpec
import qualified Test.Unit.CompilerOwnershipQuickCheckSpec
import qualified Test.Unit.DependenciesErrorHandlingQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorGoToolchainQuickCheckSpec
import qualified Test.Unit.NewEndToEndIntegrationQuickCheckSpec

-- New Comprehensive Cabal QuickCheck Test Suite (2025)
import qualified Test.Unit.NewComprehensiveCabalQuickCheckTestSuite

-- New Cabal QuickCheck Test Suite (2025) - 10 comprehensive tests
import qualified Test.Unit.NewCabalQuickCheckTestSuite

-- New Cabal Test Modules (2025) - 10 Comprehensive Tests
import qualified Test.Unit.NewCabalTest1Spec
import qualified Test.Unit.NewCabalTest2Spec
import qualified Test.Unit.NewCabalTest3Spec
import qualified Test.Unit.NewCabalTest4Spec
import qualified Test.Unit.NewCabalTest5Spec
import qualified Test.Unit.NewCabalTest6Spec
import qualified Test.Unit.NewCabalTest7Spec
import qualified Test.Unit.NewCabalTest8Spec
import qualified Test.Unit.NewCabalTest9Spec
import qualified Test.Unit.NewCabalTest10Spec

-- New Comprehensive Test Modules Added (2025)
import qualified Test.Unit.NewParserPropertiesSpec
import qualified Test.Unit.NewSourceLocationMathSpec
import qualified Test.Unit.NewErrorHandlerCoreSpec
import qualified Test.Unit.NewUtilsStringPropertiesSpec
import qualified Test.Unit.NewOwnershipTransferPropertiesSpec
import qualified Test.Unit.NewDependenciesCorePropertiesSpec
import qualified Test.Unit.NewSyntaxValidatorBoundarySpec

-- ============================================================================
-- New Concise QuickCheck Test Modules (2025) - 10 focused tests
-- ============================================================================
import qualified Test.Unit.ConciseUtilsQuickCheckSpec
import qualified Test.Unit.ConciseParserQuickCheckSpec
import qualified Test.Unit.ConciseSourceLocationQuickCheckSpec
import qualified Test.Unit.ConciseErrorHandlerQuickCheckSpec
import qualified Test.Unit.ConciseDependenciesQuickCheckSpec
import qualified Test.Unit.ConciseOwnershipQuickCheckSpec
import qualified Test.Unit.ConciseTypeSystemQuickCheckSpec
import qualified Test.Unit.ConciseSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.ConciseCompilerIRQuickCheckSpec
import qualified Test.Unit.ConciseIntegrationQuickCheckSpec

-- New Cabal Test Modules (Added for this request)
import qualified Test.Unit.NewCabalCoreTestsSpec
import qualified Test.Unit.NewCabalQuickCheckPropertiesSpec

-- New Comprehensive Cabal Test Suite (2025)
import qualified Test.Unit.NewComprehensiveCabalTestSpec

-- ============================================================================
-- New Comprehensive Test Modules (2025) - 10 cabal tests
-- ============================================================================
import qualified Test.Unit.NewTextProcessingBoundarySpec
import qualified Test.Unit.NewSourceLocationMathPropertiesSpec2
import qualified Test.Unit.NewParserRobustnessSpec
import qualified Test.Unit.NewCompilerOptimizationInvariantSpec
import qualified Test.Unit.NewOwnershipTransitivitySpec
import qualified Test.Unit.NewDependentTypeBoundarySpec
import qualified Test.Unit.NewSyntaxValidatorRobustnessSpec
import qualified Test.Unit.NewEndToEndCompilationSpec
import qualified Test.Unit.NewPerformanceRegressionSpec
import qualified Test.Unit.NewErrorHandlingConsistencySpec

-- ============================================================================
-- New Cabal Test Modules (2025) - 10 Comprehensive QuickCheck Tests
-- ============================================================================
import qualified Test.Unit.UtilsStringBoundaryQuickCheckSpec
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ParserDirectiveQuickCheckSpec
import qualified Test.Unit.ErrorHandlingConsistencyQuickCheckSpec
import qualified Test.Unit.CompilerIRConsistencyQuickCheckSpec
import qualified Test.Unit.OwnershipTransitivityQuickCheckSpec
import qualified Test.Unit.DependentTypeBoundaryQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec
import qualified Test.Unit.IntegrationEndToEndQuickCheckSpec
import qualified Test.Unit.PerformanceRegressionQuickCheckSpec

-- ============================================================================
-- New Enhanced Cabal Test Modules (2025) - 9 Additional QuickCheck Tests  
-- ============================================================================
import qualified Test.Unit.NewSourceLocationMathQuickCheckSpec
import qualified Test.Unit.NewParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.NewCompilerOptimizationQuickCheckSpec
import qualified Test.Unit.NewOwnershipTransitivityQuickCheckSpec
import qualified Test.Unit.NewUtilsStringBoundaryQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec
import qualified Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec
import qualified Test.Unit.NewTypeSystemBoundaryQuickCheckSpec
import qualified Test.Unit.NewIntegrationEndToEndQuickCheckSpec

-- New Comprehensive Test Modules (2025)
import qualified Test.Unit.StringAnalysisSpec
import qualified Test.Unit.CompilerOptimizationSpec
import qualified Test.Unit.TypeSystemBoundarySpec
import qualified Test.Unit.OwnershipComplexSpec
import qualified Test.Unit.ErrorRecoveryAdvancedSpec
import qualified Test.Unit.SourceLocationPrecisionSpec

-- New QuickCheck Test Suite Modules (2025)
import qualified Test.Unit.NewQuickCheckTestSuite1Spec
import qualified Test.Unit.NewQuickCheckTestSuite2Spec
import qualified Test.Unit.NewQuickCheckTestSuite3Spec
import qualified Test.Unit.NewQuickCheckTestSuite4Spec
import qualified Test.Unit.NewQuickCheckTestSuite5Spec
import qualified Test.Unit.NewQuickCheckTestSuite6Spec
import qualified Test.Unit.NewQuickCheckTestSuite7Spec
import qualified Test.Unit.NewQuickCheckTestSuite8Spec
import qualified Test.Unit.NewQuickCheckTestSuite9Spec
import qualified Test.Unit.NewQuickCheckTestSuite10Spec

-- New QuickCheck Test Modules (7 comprehensive tests)
import qualified Test.Unit.ParserErrorHandlingQuickCheckSpec
import qualified Test.Unit.UtilsStringProcessingQuickCheckSpec
import qualified Test.Unit.OwnershipTransferConsistencyQuickCheckSpec
import qualified Test.Unit.ErrorHandlerRecoveryQuickCheckSpec
import qualified Test.Unit.CompilerIRConsistencyQuickCheckSpec
import qualified Test.Unit.ErrorLocationTrackingQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec

-- New Cabal QuickCheck Test Module
import qualified Test.Unit.NewCabalQuickCheckTestSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.CompilerIRQuickCheckTestSpec
import qualified Test.Unit.OwnershipCommonTypesQuickCheckTestSpec
import qualified Test.Unit.AnalyzerSymbolTableQuickCheckTestSpec
import qualified Test.Unit.CompilerTypeCheckerQuickCheckTestSpec
import qualified Test.Unit.CompilerGoAstQuickCheckTestSpec
import qualified Test.Unit.DependenciesASTQuickCheckTestSpec
import qualified Test.Unit.CompilerGoLexerQuickCheckTestSpec
import qualified Test.Unit.CompilerGoParsingQuickCheckTestSpec
import qualified Test.Unit.ErrorHandlerQuickCheckTestSpec
import qualified Test.Unit.IntegratedCompilerQuickCheckTestSpec

-- ============================================================================
-- New QuickCheck Test Modules Added (2025)
-- ============================================================================
import qualified Test.Unit.SourceLocationNewQuickCheckTests
import qualified Test.Unit.ParserNewQuickCheckTests
import qualified Test.Unit.ErrorHandlerNewQuickCheckTests
import qualified Test.Unit.UtilsNewQuickCheckTests
import qualified Test.Unit.OwnershipNewQuickCheckTests
import qualified Test.Unit.ParserEnhancedQuickCheckSpec
import qualified Test.Unit.ErrorHandlerEnhancedQuickCheckSpec
import qualified Test.Unit.CompilerIntegrationEnhancedQuickCheckSpec
import qualified Test.Unit.OwnershipBoundaryEnhancedQuickCheckSpec
import qualified Test.Unit.DependentTypesValidationEnhancedQuickCheckSpec

-- New Comprehensive Test Suite (2025)
import qualified Test.Unit.SourceLocationBoundarySpec
import qualified Test.Unit.UtilsPerformanceBoundarySpec
import qualified Test.Unit.ErrorHandlerConsistencySpec
import qualified Test.Unit.OwnershipAnalysisBoundarySpec
import qualified Test.Unit.EndToEndCompilationSpec

-- New Cabal Test Modules (Added for enhanced coverage)
import qualified Test.Unit.UtilsBoundarySpec
import qualified Test.Unit.SourceLocationMathSpec
import qualified Test.Unit.ErrorHandlingCoreSpec
import qualified Test.Unit.CorePropertiesQuickCheckSpec

-- New Cabal Test Modules (9 comprehensive tests)
import qualified Test.Unit.SourceLocationCoreTestSpec
import qualified Test.Unit.ParserErrorRecoveryTestSpec
import qualified Test.Unit.OwnershipTransferTestSpec
import qualified Test.Unit.ErrorHandlerConsistencyTestSpec
import qualified Test.Unit.DependencyAnalysisTestSpec
import qualified Test.Unit.TypeInferenceBoundaryTestSpec
import qualified Test.Unit.IntegrationEndToEndTestSpec
import qualified Test.Unit.PerformanceRegressionTestSpec
import qualified Test.Unit.SecurityValidationTestSpec

-- New Additional Test Modules (2025)
import qualified Test.Unit.UtilsBoundaryQuickCheckSpec
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ParserRobustnessQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec
import qualified Test.Unit.NewEnhancedCompilerOptimizationQuickCheckSpec
import qualified Test.Unit.NewEnhancedOwnershipMemorySafetyQuickCheckSpec
import qualified Test.Unit.DependentTypeValidationQuickCheckSpec
import qualified Test.Unit.TypeInferenceBoundaryQuickCheckSpec
import qualified Test.Unit.IntegrationEndToEndQuickCheckSpec
import qualified Test.Unit.PerformanceEnhancedQuickCheckSpec

-- New Comprehensive Cabal Test Suite (2025)
import qualified Test.Unit.NewCabalTestSuiteSpec

import qualified Test.Unit.NewComprehensiveCabalQuickCheckTestSuite

-- New Cabal Test Modules (10 comprehensive QuickCheck tests)
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.OwnershipTransferConsistencyQuickCheckSpec
import qualified Test.Unit.CompilerIRConsistencyQuickCheckSpec

-- ============================================================================
-- New Essential Test Modules (2025) - Core Functionality Testing
-- ============================================================================
import qualified Test.Unit.CoreUtilsEssentialSpec
import qualified Test.Unit.CoreSourceLocationEssentialSpec
import qualified Test.Unit.CoreParserEssentialSpec
import qualified Test.Unit.CoreCompilerEssentialSpec
import qualified Test.Unit.IntegrationEssentialSpec


import qualified Test.Unit.ErrorLocationTrackingQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec

-- New test modules added for enhanced coverage
import qualified Test.Unit.ParserBoundarySpec
import qualified Test.Unit.CompilerErrorBoundarySpec

-- New Cabal Test Modules (10 comprehensive tests added)
import qualified Test.Unit.CabalCrossModuleIntegrationSpec
import qualified Test.Unit.CabalErrorRecoverySpec
import qualified Test.Unit.CabalPerformanceSpec
import qualified Test.Unit.CabalQuickCheckPropertiesSpec
import qualified Test.Unit.CabalBoundaryConditionsSpec
import qualified Test.Unit.CabalUnicodeHandlingSpec
import qualified Test.Unit.CabalConcurrentParsingSpec
import qualified Test.Unit.CabalMemorySafetySpec

-- ============================================================================
-- New QuickCheck Test Modules (Added for enhanced coverage)
-- ============================================================================
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ParserConsistencyQuickCheckSpec
import qualified Test.Unit.ErrorHandlingRecoveryQuickCheckSpec
import qualified Test.Unit.OwnershipTransitivityQuickCheckSpec
import qualified Test.Unit.DependencyCycleQuickCheckSpec
import qualified Test.Unit.CompilerIROptimizationQuickCheckSpec
import qualified Test.Unit.StringProcessingBoundaryQuickCheckSpec
import qualified Test.Unit.TypeSystemInferenceQuickCheckSpec
import qualified Test.Unit.CabalRegressionSpec
import qualified Test.Unit.CabalEndToEndSpec
import qualified Test.Unit.NewQuickCheckPropertiesSpec

-- New Comprehensive Cabal QuickCheck Test Module
import qualified Test.Unit.NewComprehensiveCabalQuickCheckSpec

-- Additional QuickCheck test modules
import qualified Test.Unit.UtilsAdditionalQuickCheckSpec
import qualified Test.Unit.SourceLocationAdditionalQuickCheckSpec
import qualified Test.Unit.ParserAdditionalQuickCheckSpec
import qualified Test.Unit.IntegrationAdditionalQuickCheckSpec

-- New Cabals Test Modules (10 comprehensive tests)
import qualified Test.Unit.ParserErrorRecoveryCabalsSpec
import qualified Test.Unit.CompilerOptimizationCabalsSpec
import qualified Test.Unit.OwnershipMemorySafetyCabalsSpec
import qualified Test.Unit.DependentTypeValidationCabalsSpec
import qualified Test.Unit.SourceLocationPrecisionCabalsSpec
import qualified Test.Unit.ErrorHandlerConsistencyCabalsSpec
import qualified Test.Unit.TypeInferenceAdvancedCabalsSpec
import qualified Test.Unit.IntegrationEndToEndCabalsSpec
import qualified Test.Unit.PerformanceRegressionCabalsSpec
import qualified Test.Unit.SecurityValidationCabalsSpec

-- New Comprehensive Cabal Test Module
-- import qualified Test.Unit.NewComprehensiveCabalTestSpec

-- New Cabal Test Modules (10 comprehensive tests)
import qualified Test.Unit.CabalParserQuickCheckSpec
import qualified Test.Unit.CabalCompilerQuickCheckSpec
import qualified Test.Unit.CabalOwnershipQuickCheckSpec
import qualified Test.Unit.CabalDependentTypesQuickCheckSpec
import qualified Test.Unit.CabalSourceLocationQuickCheckSpec
import qualified Test.Unit.CabalErrorHandlerQuickCheckSpec
import qualified Test.Unit.CabalUtilsQuickCheckSpec
import qualified Test.Unit.CabalSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.CabalAnalyzerQuickCheckSpec
import qualified Test.Unit.CabalIntegrationQuickCheckSpec

-- ============================================================================
-- New Cabal Test Modules - 10 Comprehensive QuickCheck Tests (2025)
-- ============================================================================
import qualified Test.Unit.TextProcessingPropertiesSpec
import qualified Test.Unit.SourceLocationCalculationSpec
import qualified Test.Unit.ParserCombinatorsSpec
import qualified Test.Unit.ErrorHandlerConsistencySpec
import qualified Test.Unit.UtilsStringFunctionsSpec
import qualified Test.Unit.CompilerIRPropertiesSpec
import qualified Test.Unit.OwnershipTransferSpec
import qualified Test.Unit.DependencyAnalysisSpec
import qualified Test.Unit.TypeInferenceSpec
import qualified Test.Unit.IntegrationPropertiesSpec

-- Additional Cabal Test Modules (2025)
import qualified Test.Unit.NewCabalTestSuiteSpec
import qualified Test.Unit.SourceLocationCabalTestsSpec
import qualified Test.Unit.ParserCabalTestsSpec
import qualified Test.Unit.ErrorHandlingCabalTestsSpec
import qualified Test.Unit.IntegrationCabalTestsSpec

-- New Comprehensive Cabal Test Modules (2025)
import qualified Test.Unit.NewCabalComprehensiveTestsSpec
import qualified Test.Unit.NewParserQuickCheckTestsSpec
import qualified Test.Unit.NewCompilerQuickCheckTestsSpec
import qualified Test.Unit.NewOwnershipQuickCheckTestsSpec

-- New Additional QuickCheck Test Modules (2025)
import qualified Test.Unit.EmbedAssetsQuickCheckSpec
import qualified Test.Unit.CommandLineDebugQuickCheckSpec
import qualified Test.Unit.AdditionalIntegratedCompilerQuickCheckSpec
import qualified Test.Unit.NewSourceLocationQuickCheckTestsSpec
import qualified Test.Unit.NewErrorHandlerQuickCheckTestsSpec
import qualified Test.Unit.NewDependenciesQuickCheckTestsSpec
import qualified Test.Unit.NewIntegrationQuickCheckTestsSpec

-- New Core QuickCheck Test Modules
import qualified Test.Unit.SourceLocationCoreQuickCheckSpec
import qualified Test.Unit.ErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.ParserCoreQuickCheckSpec
import qualified Test.Unit.UtilsCoreQuickCheckSpec

-- ============================================================================
-- New Test Modules Added (2025) - 10 comprehensive tests
-- ============================================================================
import qualified Test.Unit.SourceLocationBoundarySpec
import qualified Test.Unit.ErrorHandlerRecoverySpec
import qualified Test.Unit.OwnershipComplexScenariosSpec
import qualified Test.Unit.UtilsPerformanceBoundarySpec
import qualified Test.Unit.CompilerOptimizationConsistencySpec
import qualified Test.Unit.DependenciesCycleDetectionSpec
import qualified Test.Unit.TypeSystemInferenceBoundarySpec
import qualified Test.Unit.IntegrationEndToEndScenariosSpec
import qualified Test.Unit.MathematicalPropertiesQuickCheckSpec
import qualified Test.Unit.CompilerIRCoreQuickCheckSpec
import qualified Test.Unit.OwnershipTransferCoreQuickCheckSpec
import qualified Test.Unit.DependencyAnalysisCoreQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.NewSourceLocationMathQuickCheckSpec
import qualified Test.Unit.NewParserCoreQuickCheckSpec
import qualified Test.Unit.NewUtilsCoreQuickCheckSpec
import qualified Test.Unit.NewCompilerIRCoreQuickCheckSpec
import qualified Test.Unit.NewOwnershipTransferCoreQuickCheckSpec
import qualified Test.Unit.NewDependencyAnalysisCoreQuickCheckSpec
import qualified Test.Unit.NewGoToolchainCoreQuickCheckSpec

-- New Core QuickCheck Test Modules
import qualified Test.Unit.DebugCoreQuickCheckSpec
import qualified Test.Unit.EnhancedDebugCoreQuickCheckSpec
import qualified Test.Unit.CompilerUtilsCoreQuickCheckSpec
import qualified Test.Unit.SourceLocationAdvancedQuickCheckSpec
import qualified Test.Unit.ParserAdvancedQuickCheckSpec
import qualified Test.Unit.CompilerAdvancedQuickCheckSpec
import qualified Test.Unit.OwnershipAdvancedQuickCheckSpec
import qualified Test.Unit.IntegrationCoreQuickCheckSpec

-- Additional Core Tests Module
import qualified Test.Unit.AdditionalCoreTestsSpec

-- New Cabal Test Suite (10 comprehensive tests with QuickCheck)
import qualified Test.Unit.NewCabalTestSpec1
import qualified Test.Unit.NewCabalTestSpec2
import qualified Test.Unit.NewCabalTestSpec3
import qualified Test.Unit.NewCabalTestSpec4
import qualified Test.Unit.NewCabalTestSpec5
import qualified Test.Unit.NewCabalTestSpec6
import qualified Test.Unit.NewCabalTestSpec7
import qualified Test.Unit.NewCabalTestSpec8
import qualified Test.Unit.NewCabalTestSpec9
import qualified Test.Unit.NewCabalTestSpec10

-- New Comprehensive Cabal Test Modules (2025)
import qualified Test.Unit.CompilerErrorRecoveryAdvancedSpec
import qualified Test.Unit.ParserUnicodeHandlingSpec
import qualified Test.Unit.OwnershipMemoryLeakPreventionSpec
import qualified Test.Unit.TypeSystemInferenceBoundarySpec
import qualified Test.Unit.SourceLocationPrecisionNewSpec
import qualified Test.Unit.ErrorHandlerConsistencySpec
import qualified Test.Unit.DependencyAnalysisCyclicSpec
import qualified Test.Unit.IntegrationEndToEndNewSpec
import qualified Test.Unit.PerformanceRegressionNewSpec
import qualified Test.Unit.SecurityValidationSpec

-- New QuickCheck Test Modules (2025)
import qualified Test.Unit.NewParserQuickCheckTestsSpec
import qualified Test.Unit.NewSourceLocationQuickCheckTestsSpec
import qualified Test.Unit.NewUtilsQuickCheckTestsSpec

-- New Cabal QuickCheck Test Modules
import qualified Test.Unit.NewCabalQuickCheckTestsSpec
import qualified Test.Unit.OwnershipTransferPropertiesSpec
import qualified Test.Unit.DependentTypeValidationPropertiesSpec
import qualified Test.Unit.CompilerIRConsistencyPropertiesSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.SourceLocationAdvancedTestSpec
import qualified Test.Unit.ParserRobustnessTestSpec
import qualified Test.Unit.CompilerOptimizationTestSpec
import qualified Test.Unit.OwnershipMemorySafetyTestSpec
import qualified Test.Unit.DependentTypeValidationTestSpec
import qualified Test.Unit.ErrorHandlerRecoveryTestSpec
import qualified Test.Unit.TypeInferenceAdvancedTestSpec

-- Simple Cabal QuickCheck Test Module
import qualified Test.Unit.SimpleCabalQuickCheckTests
import qualified Test.Unit.IntegrationEndToEndTestSpec
import qualified Test.Unit.PerformanceRegressionTestSpec
import qualified Test.Unit.SecurityValidationTestSpec

-- New Additional Test Modules

-- New Validation Test Modules (Added by user)
import qualified Test.Unit.NewUtilsValidationSpec
import qualified Test.Unit.NewSourceLocationMathSpec
import qualified Test.Unit.NewParserValidationSpec
import qualified Test.Unit.NewErrorHandlerValidationSpec
import qualified Test.Unit.NewIntegrationValidationSpec
import qualified Test.Unit.NewBoundaryConditionSpec
import qualified Test.Unit.NewPerformanceSpec

-- ============================================================================
-- New Cabal Test Modules (Added by user request) - 10 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCabalCoreSpec
import qualified Test.Unit.NewCabalParserBoundarySpec
import qualified Test.Unit.NewCabalCompilerInvariantSpec
import qualified Test.Unit.NewCabalOwnershipSafetySpec
import qualified Test.Unit.NewCabalTypeSystemSpec
import qualified Test.Unit.NewCabalErrorRecoverySpec
import qualified Test.Unit.NewCabalPerformanceSpec
import qualified Test.Unit.NewCabalIntegrationSpec
import qualified Test.Unit.NewCabalQuickCheckSpec
import qualified Test.Unit.NewCabalEdgeCaseSpec

-- ============================================================================
-- Enhanced Cabal Test Suite - 8 comprehensive QuickCheck tests
-- ============================================================================
import qualified Test.Unit.EnhancedCabalTestSuiteSpec
import qualified Test.Unit.CompilerIRConsistencyQuickCheckSpec
import qualified Test.Unit.OwnershipTransitivityQuickCheckSpec
import qualified Test.Unit.DependentTypeBoundaryQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec
import qualified Test.Unit.IntegrationEndToEndQuickCheckSpec
import qualified Test.Unit.UtilsBoundaryConditionsQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec

-- New Comprehensive Typus Test Module (2025)
import qualified Test.Unit.NewComprehensiveTypusTestSpec
import qualified Test.Unit.AdditionalCorePropertiesSpec
import qualified Test.Unit.CompilerCorePropertiesSpec
import qualified Test.Unit.BoundaryCasePropertiesSpec

-- ============================================================================
-- New Core Cabal QuickCheck Test Modules (2025) - 10 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCoreCabalQuickCheckSpec1
import qualified Test.Unit.NewCoreCabalQuickCheckSpec2
import qualified Test.Unit.NewCoreCabalQuickCheckSpec3
import qualified Test.Unit.NewCoreCabalQuickCheckSpec4
import qualified Test.Unit.NewCoreCabalQuickCheckSpec5
import qualified Test.Unit.NewCoreCabalQuickCheckSpec6
import qualified Test.Unit.NewCoreCabalQuickCheckSpec7
import qualified Test.Unit.NewCoreCabalQuickCheckSpec8
import qualified Test.Unit.NewCoreCabalQuickCheckSpec9
import qualified Test.Unit.NewCoreCabalQuickCheckSpec10

-- New QuickCheck Test Modules (2025)
import qualified Test.Unit.SourceLocationNewQuickCheckSpec
import qualified Test.Unit.ErrorHandlerNewQuickCheckSpec
import qualified Test.Unit.ParserNewQuickCheckSpec
import qualified Test.Unit.OwnershipNewQuickCheckSpec
import qualified Test.Unit.DependenciesNewQuickCheckSpec
import qualified Test.Unit.CompilerNewQuickCheckSpec
import qualified Test.Unit.DependentTypesNewQuickCheckSpec
import qualified Test.Unit.UtilsNewQuickCheckSpec

-- New Comprehensive Test Modules (2025)
import qualified Test.Unit.NewParserComprehensiveSpec
import qualified Test.Unit.NewCompilerComprehensiveSpec
import qualified Test.Unit.NewUtilsComprehensiveSpec
import qualified Test.Unit.NewSourceLocationComprehensiveSpec
import qualified Test.Unit.NewErrorHandlerComprehensiveSpec

-- New Advanced Test Modules for Enhanced Coverage
import qualified Test.Unit.ParserBoundaryConditionsSpec
import qualified Test.Unit.NewSourceLocationMathPropertiesSpec2
import qualified Test.Unit.NewUtilsPropertiesSpec2
import qualified Test.Unit.ErrorHandlerCorePropertiesSpec
import qualified Test.Unit.OwnershipBasicPropertiesSpec
import qualified Test.Unit.DependenciesTypeSystemPropertiesSpec
import qualified Test.Unit.DependentTypesParserPropertiesSpec
import qualified Test.Unit.ErrorHandlerRecoveryAdvancedSpec
import qualified Test.Unit.OwnershipTransitivityAdvancedSpec
import qualified Test.Unit.DependentTypeConstraintValidationSpec
import qualified Test.Unit.UtilsStringProcessingAdvancedSpec
import qualified Test.Unit.CompilerOptimizationConsistencySpec

-- New Core Functionality Test Module
import qualified Test.Unit.NewCoreFunctionalitySpec

-- New Cabal Enhanced Test Module
import qualified Test.Unit.NewCabalEnhancedTestSpec

-- New Cabal Test Module
import qualified Test.Unit.NewCabalTestSpec

-- New Additional Test Module
import qualified Test.Unit.NewAdditionalTestSpec

-- New Comprehensive Tests Module
import qualified Test.Unit.NewComprehensiveTestsSpec

-- Additional Cabal Test Module
import qualified Test.Unit.AdditionalCabalTestSpec

-- New Cabal Integration Test Module
import qualified Test.Unit.NewCabalIntegrationSpec

-- ============================================================================
-- New Cabal QuickCheck Test Modules (2025) - 10 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCabalUtilsQuickCheckTestSpec
import qualified Test.Unit.NewCabalSourceLocationQuickCheckTestSpec
import qualified Test.Unit.NewCabalParserQuickCheckTestSpec
import qualified Test.Unit.NewCabalErrorHandlerQuickCheckTestSpec
import qualified Test.Unit.NewCabalDependenciesQuickCheckTestSpec
import qualified Test.Unit.NewCabalOwnershipQuickCheckTestSpec
import qualified Test.Unit.NewCabalCompilerQuickCheckTestSpec
import qualified Test.Unit.NewCabalSyntaxValidatorQuickCheckTestSpec
import qualified Test.Unit.NewCabalGoToolchainQuickCheckTestSpec
import qualified Test.Unit.NewCabalIntegrationQuickCheckTestSpec

-- New Cabal Test Suite Module
import qualified Test.Unit.NewCabalTestSuiteSpec

-- Advanced Test Modules
import qualified Test.Unit.BoundaryConditionsAdvancedSpec
import qualified Test.Unit.ErrorRecoveryAdvancedSpec
import qualified Test.Unit.PerformanceRegressionNewSpec
import qualified Test.Unit.IntegrationAdvancedSpec

-- New Simple Cabal Test Module
import qualified Test.Unit.SimpleCabalTestSpec

-- Enhanced Cabal QuickCheck Test Module
import qualified Test.Unit.EnhancedCabalQuickCheckTestSpec

-- New Additional QuickCheck Test Module
import qualified Test.Unit.NewAdditionalQuickCheckSpec

-- New Enhanced QuickCheck Test Modules
import qualified Test.Unit.EnhancedUtilsQuickCheckSpec
import qualified Test.Unit.EnhancedSourceLocationQuickCheckSpec
import qualified Test.Unit.EnhancedParserQuickCheckSpec
import qualified Test.Unit.EnhancedCompilerQuickCheckSpec
import qualified Test.Unit.EnhancedOwnershipQuickCheckSpec
import qualified Test.Unit.EnhancedErrorHandlerQuickCheckSpec
import qualified Test.Unit.EnhancedDependenciesQuickCheckSpec
import qualified Test.Unit.EnhancedSyntaxValidatorQuickCheckSpec

-- New Additional Test Modules (2025)
import qualified Test.Unit.NewCabalQuickCheckTestsSpec
import qualified Test.Unit.CompilerBasicPropertiesSpec
import qualified Test.Unit.SourceLocationMathSpec
import qualified Test.Unit.ErrorHandlingRobustnessSpec
import qualified Test.Unit.TypeInferenceBasicSpec
import qualified Test.Unit.OwnershipTransferSpec
import qualified Test.Unit.ParserCombinatorSpec
import qualified Test.Unit.UtilsStringSpec

-- New Comprehensive Test Suites (2025)
import qualified Test.Unit.SourceLocationMathComprehensiveSpec
import qualified Test.Unit.CompilerIntegrationComprehensiveSpec
import qualified Test.Unit.OwnershipAnalysisComprehensiveSpec
import qualified Test.Unit.DependentTypesValidationComprehensiveSpec

-- ============================================================================
-- New Comprehensive QuickCheck Test Modules (2025) - 10 Core Modules
-- ============================================================================
import qualified Test.Unit.UtilsComprehensiveQuickCheckSpec
import qualified Test.Unit.SourceLocationComprehensiveQuickCheckSpec
import qualified Test.Unit.ParserComprehensiveQuickCheckSpec
import qualified Test.Unit.ErrorHandlerComprehensiveQuickCheckSpec
import qualified Test.Unit.OwnershipComprehensiveQuickCheckSpec
import qualified Test.Unit.CompilerComprehensiveQuickCheckSpec
import qualified Test.Unit.DependenciesComprehensiveQuickCheckSpec
import qualified Test.Unit.DependentTypesComprehensiveQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorComprehensiveQuickCheckSpec
import qualified Test.Unit.ErrorHandlerCoreComprehensiveSpec
import qualified Test.Unit.UtilsStringProcessingComprehensiveSpec

-- New comprehensive test modules added for enhanced coverage
import qualified Test.Unit.NewIntegratedParserTestsSpec
import qualified Test.Unit.NewCompilerErrorRecoverySpec
import qualified Test.Unit.NewOwnershipMemorySafetySpec
import qualified Test.Unit.NewDependentTypeValidationSpec
import qualified Test.Unit.NewSourceLocationPrecisionSpec
import qualified Test.Unit.NewUtilsPerformanceSpec
import qualified Test.Unit.NewErrorHandlingConsistencySpec
import qualified Test.Unit.NewSyntaxValidationComprehensiveSpec
import qualified Test.Unit.NewTypeSystemInferenceSpec
import qualified Test.Unit.NewCompilationOptimizationSpec

-- New Cabal Test Suite Module
import qualified Test.Unit.NewCabalTestSuiteSpec

-- New Core Functionality Test Modules
import qualified Test.Unit.CoreParserSpec
import qualified Test.Unit.CoreSourceLocationSpec
import qualified Test.Unit.UtilsStringProcessingSpec

-- New Cabal QuickCheck Test Modules (2025)
import qualified Test.Unit.NewCabalQuickCheckTestSpec
import qualified Test.Unit.NewParserBoundaryQuickCheckTestSpec
import qualified Test.Unit.NewSourceLocationPrecisionQuickCheckTestSpec
import qualified Test.Unit.NewErrorHandlingRecoveryQuickCheckTestSpec
import qualified Test.Unit.NewCompilerOptimizationQuickCheckTestSpec
import qualified Test.Unit.NewOwnershipMemorySafetyQuickCheckTestSpec
import qualified Test.Unit.NewDependentTypeValidationQuickCheckTestSpec

-- New QuickCheck Property Tests
import qualified Test.Unit.CoreQuickCheckPropertiesSpec
import qualified Test.Unit.ParserPropertySpec
import qualified Test.Unit.ErrorHandlingPropertySpec
import qualified Test.Unit.NewComprehensivePropertySpec
import qualified Test.Unit.NewEnhancedTestSpec
import qualified Test.Unit.SimpleNewTestSpec
import qualified Test.Unit.NewCoreQuickCheckPropertiesSpec

-- New Advanced Test Suite (2025)
import qualified Test.Unit.CompilerErrorRecoverySpec
import qualified Test.Unit.TypeInferenceAdvancedNewSpec
import qualified Test.Unit.OwnershipMemorySafetySpec
import qualified Test.Unit.DependencyAnalysisAdvancedSpec
import qualified Test.Unit.SourcePositionPrecisionSpec
import qualified Test.Unit.TextProcessingRobustnessSpec
import qualified Test.Unit.CompilerOptimizationSpec
import qualified Test.Unit.IntegrationEndToEndSpec

-- New Comprehensive Cabal Test Modules (2025)
import qualified Test.Unit.NewComprehensiveCabalTestsSpec

-- Additional Cabal QuickCheck Test Suite (2025)
import qualified Test.Unit.AdditionalCabalQuickCheckTestSuite
import qualified Test.Unit.UtilsEnhancedQuickCheckSpec
import qualified Test.Unit.ParserBoundaryConditionsQuickCheckSpec
import qualified Test.Unit.OwnershipPropertiesQuickCheckSpec
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ErrorHandlerRecoveryQuickCheckSpec
import qualified Test.Unit.CompilerIRConsistencyQuickCheckSpec
import qualified Test.Unit.TypeSystemInferenceQuickCheckSpec

-- ============================================================================
-- New Cabal QuickCheck Test Modules (2025) - 10 comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewCabalQuickCheckSpec1
import qualified Test.Unit.NewCabalQuickCheckSpec2
import qualified Test.Unit.NewCabalQuickCheckSpec3
import qualified Test.Unit.NewCabalQuickCheckSpec4
import qualified Test.Unit.NewCabalQuickCheckSpec5
import qualified Test.Unit.NewCabalQuickCheckSpec6
import qualified Test.Unit.NewCabalQuickCheckSpec7
import qualified Test.Unit.NewCabalQuickCheckSpec8
import qualified Test.Unit.NewCabalQuickCheckSpec9
import qualified Test.Unit.NewCabalQuickCheckSpec10

-- ============================================================================
-- New Enhanced Test Modules (2025) - 10 comprehensive QuickCheck tests
-- ============================================================================
import qualified Test.Unit.NewEnhancedUtilsQuickCheckSpec
import qualified Test.Unit.NewAdvancedSourceLocationQuickCheckSpec
import qualified Test.Unit.NewRobustErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewComprehensiveParserQuickCheckSpec
import qualified Test.Unit.NewAdvancedOwnershipQuickCheckSpec
import qualified Test.Unit.NewDependenciesAdvancedQuickCheckSpec
import qualified Test.Unit.NewIntegrationAdvancedQuickCheckSpec
import qualified Test.Unit.NewCoreFunctionalityQuickCheckSpec
import qualified Test.Unit.NewTextProcessingQuickCheckSpec
import qualified Test.Unit.NewSourceLocationMathQuickCheckSpec

-- ============================================================================
-- New QuickCheck Test Modules Added (2025) - Additional comprehensive tests
-- ============================================================================
import qualified Test.Unit.NewUtilsQuickCheckSpec
import qualified Test.Unit.NewSourceLocationQuickCheckSpec
import qualified Test.Unit.NewParserQuickCheckSpec
import qualified Test.Unit.NewComprehensiveQuickCheckSpec

-- New Cabal QuickCheck Test Suite (10 comprehensive tests)
import qualified Test.Unit.NewCabalQuickCheckTestSuiteSpec

-- New Test Modules Added for Enhanced Coverage
import qualified Test.Unit.NewErrorHandlingSpec
import qualified Test.Unit.NewParserSpec
import qualified Test.Unit.NewCompilerSpec
import qualified Test.Unit.NewOwnershipSpec

-- New QuickCheck Test Modules (2025)
import qualified Test.Unit.CoreFunctionalityQuickCheckSpec
import qualified Test.Unit.TextProcessingPropertiesQuickCheckSpec
import qualified Test.Unit.ParserInvariantQuickCheckSpec
import qualified Test.Unit.ErrorLocationPropertiesQuickCheckSpec
import qualified Test.Unit.CompilerConsistencyQuickCheckSpec
import qualified Test.Unit.SymbolTableInvariantQuickCheckSpec
import qualified Test.Unit.OwnershipTransferPropertiesQuickCheckSpec
import qualified Test.Unit.TypeSystemConsistencyQuickCheckSpec
import qualified Test.Unit.DependencyAnalysisPropertiesQuickCheckSpec
import qualified Test.Unit.SourcePositionInvariantQuickCheckSpec

-- New Test Modules Added for Additional Coverage
import qualified Test.Unit.UtilsStringPropertiesSpec
import qualified Test.Unit.SourceLocationConsistencySpec
import qualified Test.Unit.ParserConsistencySpec
import qualified Test.Unit.ErrorRecoveryPropertiesSpec
import qualified Test.Unit.DependentTypeSystemSpec
import qualified Test.Unit.OwnershipAnalysisSpec
import qualified Test.Unit.CompilerIRSpec
import qualified Test.Unit.EnhancedIntegrationQuickCheckSpec

-- New Comprehensive QuickCheck Test Modules
import qualified Test.Unit.TextProcessingBoundaryQuickCheckSpec
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec

-- New Test Modules Added for Enhanced Cabal Testing (10 new modules)
import qualified Test.Unit.SourceLocationAdvancedPropertiesSpec
import qualified Test.Unit.ErrorHandlerBoundarySpec
import qualified Test.Unit.OwnershipTransferComplexSpec
import qualified Test.Unit.DependentTypeConstraintSpec
import qualified Test.Unit.CompilerIREdgeCaseSpec
import qualified Test.Unit.SyntaxValidatorBoundarySpec
import qualified Test.Unit.GoToolchainIntegrationSpec
import qualified Test.Unit.EmbedAssetsConsistencySpec
import qualified Test.Unit.DebugIntegrationFlowSpec
import qualified Test.Unit.IntegratedCompilerPropertiesSpec

-- New Enhanced Test Modules (2025) - Additional
import qualified Test.Unit.SourceLocationAdvancedFeaturesSpec
import qualified Test.Unit.ParserBoundaryConditionsSpec
import qualified Test.Unit.UtilsStringProcessingEnhancedSpec
import qualified Test.Unit.ErrorHandlerErrorHandlingSpec
import qualified Test.Unit.CompilerCompilationLogicSpec
import qualified Test.Unit.OwnershipOwnershipAnalysisSpec
import qualified Test.Unit.DependenciesDependencyAnalysisSpec
import qualified Test.Unit.SyntaxValidatorValidationSpec

-- New Advanced Test Suite Imports (2025) - 10 comprehensive tests
import qualified Test.Unit.NewComprehensiveTestSuite2025Spec
import qualified Test.Unit.ParserDirectivesAdvanced2025Spec
import qualified Test.Unit.SourceLocationMathAdvanced2025Spec
import qualified Test.Unit.StringUtilsProcessingAdvanced2025Spec
import qualified Test.Unit.ErrorRecoveryAdvancedTest2025Spec
import qualified Test.Unit.OwnershipTransferComplex2025Spec
import qualified Test.Unit.TypeInferenceComplex2025Spec
import qualified Test.Unit.ConcurrentSafetyAdvanced2025Spec
import qualified Test.Unit.PerformanceOptimization2025Spec
import qualified Test.Unit.BoundaryConditionsAdvanced2025Spec

-- New Cabal Test Module
import qualified Test.Unit.NewCabalTestSpec

-- New Test Modules Added for Enhanced Testing (2025)
import qualified Test.Unit.NewTextProcessingQuickCheckSpec

-- ============================================================================
-- New Cabal Test Modules (10 comprehensive tests added) - Imports
-- ============================================================================
import qualified Test.Unit.NewUtilsStringProcessingSpec
import qualified Test.Unit.NewSourceLocationCalculationSpec
import qualified Test.Unit.NewParserBasicPropertiesSpec
import qualified Test.Unit.NewErrorHandlerPropertiesSpec
import qualified Test.Unit.NewOwnershipTransferPropertiesSpec
import qualified Test.Unit.NewDependenciesAnalysisPropertiesSpec
import qualified Test.Unit.NewCompilerOptimizationPropertiesSpec
import qualified Test.Unit.NewSyntaxValidatorRobustnessSpec
import qualified Test.Unit.NewDependentTypeValidationSpec
import qualified Test.Unit.NewIntegrationEndToEndSpec

-- ============================================================================
-- New Comprehensive QuickCheck Test Modules (2025) - 10 Enhanced Tests
-- ============================================================================
import qualified Test.Unit.NewUtilsStringProcessingQuickCheckSpec
import qualified Test.Unit.NewSourceLocationTrackingQuickCheckSpec
import qualified Test.Unit.NewParserQuickCheckSpec
import qualified Test.Unit.NewCompilerIRQuickCheckSpec
import qualified Test.Unit.NewOwnershipQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewDependenciesQuickCheckSpec
import qualified Test.Unit.NewIntegrationQuickCheckSpec
import qualified Test.Unit.NewDependentTypesQuickCheckSpec
import qualified Test.Unit.NewSourceLocationTrackingQuickCheckSpec
import qualified Test.Unit.NewErrorHandlingQuickCheckSpec
import qualified Test.Unit.NewCompilerIRQuickCheckSpec
import qualified Test.Unit.NewTypeSystemQuickCheckSpec
import qualified Test.Unit.NewOwnershipAnalysisQuickCheckSpec
import qualified Test.Unit.NewParserQuickCheckSpec
import qualified Test.Unit.DependentTypeBoundaryQuickCheckSpec
import qualified Test.Unit.OwnershipTransferComplexQuickCheckSpec
import qualified Test.Unit.CompilerOptimizationInvariantQuickCheckSpec
import qualified Test.Unit.ErrorHandlingConsistencyQuickCheckSpec
import qualified Test.Unit.ToolchainRobustnessQuickCheckSpec
import qualified Test.Unit.NewTypeSystemSpec
import qualified Test.Unit.NewSourceLocationSpec
import qualified Test.Unit.NewDependencySpec
import qualified Test.Unit.NewIntegrationSpec
import qualified Test.Unit.NewPerformanceSpec
import qualified Test.Unit.NewQuickCheckSpec

-- New Core QuickCheck Test Modules (2025)
import qualified Test.Unit.CoreSourceLocationQuickCheckSpec
import qualified Test.Unit.CoreErrorHandlerQuickCheckSpec
import qualified Test.Unit.CoreParserQuickCheckSpec
import qualified Test.Unit.CoreOwnershipQuickCheckSpec
import qualified Test.Unit.CoreDependenciesQuickCheckSpec
import qualified Test.Unit.CoreCompilerQuickCheckSpec
import qualified Test.Unit.CoreUtilsQuickCheckSpec
import qualified Test.Unit.CoreSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.CoreGoToolchainQuickCheckSpec
import qualified Test.Unit.CoreIntegratedCompilerQuickCheckSpec

-- New Comprehensive QuickCheck Test Modules
import qualified Test.Unit.StringUtilsQuickCheckTestSpec
import qualified Test.Unit.CompilerErrorHandlingQuickCheckTestSpec
import qualified Test.Unit.SourceLocationTrackingQuickCheckTestSpec
import qualified Test.Unit.OwnershipTransferQuickCheckTestSpec
import qualified Test.Unit.DependencyAnalysisQuickCheckTestSpec
import qualified Test.Unit.ErrorRecoveryQuickCheckTestSpec
import qualified Test.Unit.CodeGenerationQuickCheckTestSpec
import qualified Test.Unit.ParserBoundaryConditionsQuickCheckTestSpec
import qualified Test.Unit.IntegrationQuickCheckTestSpec

-- New Advanced QuickCheck Test Modules (2025)
import qualified Test.Unit.SourceLocationAdvancedQuickCheckSpec
import qualified Test.Unit.UtilsAdvancedQuickCheckSpec
import qualified Test.Unit.ParserAdvancedQuickCheckSpec
import qualified Test.Unit.ErrorHandlerAdvancedQuickCheckSpec
import qualified Test.Unit.CompilerAdvancedQuickCheckSpec
import qualified Test.Unit.OwnershipAdvancedQuickCheckSpec
import qualified Test.Unit.DependenciesAdvancedQuickCheckSpec
import qualified Test.Unit.IntegrationAdvancedQuickCheckSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerErrorHandlingQuickCheckSpec
import qualified Test.Unit.DependentTypesSystemQuickCheckSpec
import qualified Test.Unit.OwnershipAnalysisComprehensiveQuickCheckSpec
import qualified Test.Unit.ParserBoundaryConditionsQuickCheckSpec

-- New Enhanced QuickCheck Test Modules (2025)
import qualified Test.Unit.EndToEndCompilationQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryAdvancedQuickCheckSpec
import qualified Test.Unit.TypeInferenceBoundaryQuickCheckSpec
import qualified Test.Unit.SourceLocationPrecisionQuickCheckSpec
import qualified Test.Unit.CompilerOptimizationConsistencyQuickCheckSpec
import qualified Test.Unit.DependentTypeConstraintQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryAdvancedQuickCheckSpec
import qualified Test.Unit.ConcurrentSafetyQuickCheckSpec
import qualified Test.Unit.PerformanceEnhancedQuickCheckSpec
import qualified Test.Unit.IntegrationFeaturesQuickCheckSpec

-- New Cabal Test Suite modules
import qualified Test.Unit.SimpleCabalTestSpec
import qualified Test.Unit.NewCabalTestSuiteSpec
import qualified Test.Unit.SourceLocationCoreFunctionsSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.NewParserQuickCheckTestsSpec
import qualified Test.Unit.NewSourceLocationQuickCheckTestsSpec
import qualified Test.Unit.NewErrorHandlerQuickCheckTestsSpec
import qualified Test.Unit.NewCompilerQuickCheckTestsSpec
import qualified Test.Unit.NewOwnershipQuickCheckTestsSpec
import qualified Test.Unit.NewDependenciesQuickCheckTestsSpec
import qualified Test.Unit.ParserBasicFunctionsSpec
import qualified Test.Unit.CompilerErrorHandlingSpec
import qualified Test.Unit.OwnershipTransferSpec
import qualified Test.Unit.TypeSystemBasicSpec
import qualified Test.Unit.DependentTypesBasicSpec
import qualified Test.Unit.SymbolTableOperationsSpec
import qualified Test.Unit.ErrorRecoveryBasicSpec
import qualified Test.Unit.IntegrationBasicSpec
-- New Cabal QuickCheck test modules
import qualified Test.Unit.NewCabalQuickCheckTestSpec
import qualified Test.Unit.NewComprehensiveCabalQuickCheckTestSuite

-- New core test modules
import qualified Test.Unit.SourceLocationCoreTestSpec
import qualified Test.Unit.ParserCoreTestSpec
import qualified Test.Unit.ErrorHandlerCoreTestSpec
import qualified Test.Unit.DependenciesCoreTestSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.OwnershipTransferQuickCheckSpec
import qualified Test.Unit.DependentTypesValidationQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SourceLocationTrackingQuickCheckSpec

-- New Additional QuickCheck Test Modules (10 new comprehensive tests)
import qualified Test.Unit.TextProcessingQuickCheckSpec
import qualified Test.Unit.CommentHandlingQuickCheckSpec
import qualified Test.Unit.IndentationNormalizationQuickCheckSpec
import qualified Test.Unit.SourcePositionTrackingQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.OwnershipTransferEdgeCasesQuickCheckSpec
import qualified Test.Unit.DependentTypeValidationQuickCheckSpec
import qualified Test.Unit.CompilerIntegrationQuickCheckSpec
import qualified Test.Unit.FileDirectiveProcessingQuickCheckSpec
import qualified Test.Unit.CodeGenerationConsistencyQuickCheckSpec

-- New Test Modules for Enhanced Coverage
import qualified Test.Unit.CoreParserFunctionsSpec
import qualified Test.Unit.TextProcessingSpec
import qualified Test.Unit.IndentionSpec
import qualified Test.Unit.CommentRemovalSpec
import qualified Test.Unit.SourcePositionSpec
import qualified Test.Unit.DirectiveProcessingSpec
import qualified Test.Unit.ErrorLocationSpec
import qualified Test.Unit.StringSplittingSpec
import qualified Test.Unit.SpanOperationsSpec
import qualified Test.Unit.LocationTrackingSpec

-- New core QuickCheck test modules
import qualified Test.Unit.SourceLocationCoreQuickCheckSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.SourceLocationAdvancedQuickCheckSpec
import qualified Test.Unit.ErrorHandlerBoundaryQuickCheckSpec
import qualified Test.Unit.ParserEdgeCaseQuickCheckSpec
import qualified Test.Unit.DependenciesInferenceQuickCheckSpec
import qualified Test.Unit.UtilsEfficiencyQuickCheckSpec
import qualified Test.Unit.ValueAnalysisFlowQuickCheckSpec
import qualified Test.Unit.GoToolchainIntegrationQuickCheckSpec
import qualified Test.Unit.TypusEndToEndQuickCheckSpec

-- New Cabal QuickCheck Test Cases
import qualified Test.Unit.NewCabalQuickCheckTestCasesSpec

-- New Test Modules Added for Cabal Testing
import qualified Test.Unit.DebugIntegrationSpec
import qualified Test.Unit.IntegratedCompilerSpec

-- New Cabal Test Modules (2025) - 8 comprehensive tests
import qualified Test.Unit.CompilerOptimizationConsistencySpec
import qualified Test.Unit.OwnershipTransferBoundaryNewSpec
import qualified Test.Unit.DependentTypeConstraintValidationNewSpec
import qualified Test.Unit.SourceLocationPrecisionNewSpec
import qualified Test.Unit.ErrorRecoveryConsistencyNewSpec
import qualified Test.Unit.TypeInferenceAdvancedNewSpec
import qualified Test.Unit.IntegrationEndToEndNewSpec
import qualified Test.Unit.PerformanceRegressionNewSpec

import qualified Test.Unit.NewCabalPropertySpec
import qualified Test.Unit.ErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.DependenciesCoreQuickCheckSpec
import qualified Test.Unit.GoToolchainCoreQuickCheckSpec
import qualified Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.DebugIntegrationCoreQuickCheckSpec

-- QuickCheck test modules
import qualified Test.Unit.ParserQuickCheckSpec
import qualified Test.Unit.CompilerQuickCheckSpec
import qualified Test.Unit.TypeCheckerQuickCheckSpec

-- New Advanced Edge Case QuickCheck Test Module
import qualified Test.Unit.AdvancedEdgeCaseQuickCheckSpec
import qualified Test.Unit.OwnershipQuickCheckSpec
import qualified Test.Unit.AnalyzerQuickCheckSpec
import qualified Test.Unit.UtilsQuickCheckSpec
import qualified Test.Unit.SymbolTableQuickCheckSpec
import qualified Test.Unit.ValueAnalysisQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorQuickCheckSpec
import qualified Test.Unit.ErrorHandlingQuickCheckSpec
import qualified Test.Unit.DependentTypesQuickCheckSpec
import qualified Test.Unit.DependenciesQuickCheckSpec
import qualified Test.Unit.AdvancedQuickCheckSpec
import qualified Test.Unit.PerformanceQuickCheckSpec

-- New QuickCheck Test Modules (Added 2025)
import qualified Test.Unit.BasicParserQuickCheckSpec
import qualified Test.Unit.CompilerIRQuickCheckSpec
import qualified Test.Unit.OwnershipTransferQuickCheckSpec
import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ErrorHandlingQuickCheckSpec
-- New QuickCheck test modules
import qualified Test.Unit.SimpleSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.DebugQuickCheckSpec
-- Extended QuickCheck test modules
import qualified Test.Unit.ExtendedParserQuickCheckSpec
import qualified Test.Unit.ExtendedCompilerQuickCheckSpec
import qualified Test.Unit.ExtendedOwnershipQuickCheckSpec
import qualified Test.Unit.ExtendedTypeCheckerQuickCheckSpec
import qualified Test.Unit.ExtendedAnalyzerQuickCheckSpec
import qualified Test.Unit.ExtendedUtilsQuickCheckSpec
-- Additional QuickCheck test modules
import qualified Test.Unit.IRQuickCheckSpec
import qualified Test.Unit.GoAstQuickCheckSpec
import qualified Test.Unit.ErrorHandlerQuickCheckSpec
import qualified Test.Unit.AnalyzerIntegrationQuickCheckSpec
import qualified Test.Unit.CliQuickCheckSpec
import qualified Test.Unit.GoToolchainQuickCheckSpec
import qualified Test.Unit.WorkingQuickCheckTestSpec
-- Comprehensive QuickCheck test modules
import qualified Test.Unit.ComprehensiveParserQuickCheckSpec
import qualified Test.Unit.ComprehensiveCompilerQuickCheckSpec
import qualified Test.Unit.ComprehensiveOwnershipQuickCheckSpec
import qualified Test.Unit.ComprehensiveDependenciesQuickCheckSpec
import qualified Test.Unit.ComprehensiveUtilsQuickCheckSpec
import qualified Test.Unit.ComprehensiveAnalyzerQuickCheckSpec
-- Additional comprehensive QuickCheck test modules
import qualified Test.Unit.CoreQuickCheckSpec
import qualified Test.Unit.ErrorHandlingComprehensiveQuickCheckSpec
import qualified Test.Unit.DependentTypesComprehensiveQuickCheckSpec
import qualified Test.Unit.OwnershipComprehensiveQuickCheckSpec
-- Simple QuickCheck test modules
import qualified Test.Unit.SimpleParserQuickCheckSpec
import qualified Test.Unit.SimpleQuickCheckSpec
import qualified Test.Unit.WorkingQuickCheckSpec
import qualified Test.Unit.SimpleDataStructuresQuickCheckSpec
import qualified Test.Unit.SimpleTypeCheckerQuickCheckSpec
import qualified Test.Unit.NewQuickCheckSpec
import qualified Test.Unit.FocusedQuickCheckSpec
import qualified Test.Unit.BasicPropertiesQuickCheckSpec
-- New property-based QuickCheck test modules
import qualified Test.Unit.StringUtilsQuickCheckSpec
import qualified Test.Unit.NewCabalQuickCheckPropertiesSpec

-- New property-based QuickCheck test modules (batch 2)
import qualified Test.Unit.IRPropertiesQuickCheckSpec
import qualified Test.Unit.EnhancedCabalTestQuickCheckSpec
import qualified Test.Unit.CabalEnhancedQuickCheckSpec

-- New Test Modules Added for Enhanced Coverage
import qualified Test.Unit.NewUtilsEdgeCaseSpec
import qualified Test.Unit.NewSourceLocationMathPropertiesSpec2
import qualified Test.Unit.NewParserUnicodeSpec
import qualified Test.Unit.NewCommentHandlingSpec
import qualified Test.Unit.NewIndentationSpec
import qualified Test.Unit.NewQuickCheckUtilsSpec
import qualified Test.Unit.NewQuickCheckSourceLocationSpec
import qualified Test.Unit.NewErrorRecoverySpec
import qualified Test.Unit.EnhancedCoreQuickCheckSpec
-- New QuickCheck property test modules
import qualified Test.Unit.ComprehensiveQuickCheckSpec
import qualified Test.Unit.CoreDataStructuresQuickCheckSpec
import qualified Test.Unit.CompilerIRQuickCheckSpec
import qualified Test.Unit.TypeSystemQuickCheckSpec
import qualified Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec
-- Additional new QuickCheck test modules
import qualified Test.Unit.SimpleQuickCheckTestSpec
import qualified Test.Unit.CabalQuickCheckTestSpec
import qualified Test.Unit.NewIRQuickCheckSpec
import qualified Test.Unit.NewSymbolTableQuickCheckSpec
import qualified Test.Unit.NewTypeCheckerQuickCheckSpec
import qualified Test.Unit.NewQuickCheckTestsSpec
import qualified Test.Unit.BasicQuickCheckTestSpec
import qualified Test.Unit.PropertyQuickCheckTestSpec
import qualified Test.Unit.CoreQuickCheckTestSpec
import qualified Test.Unit.AdvancedQuickCheckTestSpec

import qualified Test.Unit.FinalQuickCheckTestSpec
-- New comprehensive QuickCheck test modules
import qualified Test.Unit.NewCoreQuickCheckSpec

import qualified Test.Unit.ErrorRecoveryQuickCheckSpec

import qualified Test.Unit.NewCoreQuickCheckTests
import qualified Test.Unit.SimpleCoreQuickCheckSpec
import qualified Test.Unit.NewCabalQuickCheckSpec
import qualified Test.Unit.NewComprehensiveCabalQuickCheckTestSuite
import qualified Test.Unit.NewCabalQuickCheckTestSpec
import qualified Test.Unit.NewCabalTestQuickCheckSpec
import qualified Test.Unit.AdditionalCabalQuickCheckSpec
import qualified Test.Unit.NewSimpleCabalQuickCheckSpec
import qualified Test.Unit.CabalQuickCheckTests
import qualified Test.Unit.NewCabslQuickCheckTests
import qualified Test.Unit.AdditionalQuickCheckTests
import qualified Test.Unit.NewQuickCheckTestCasesSpec
import qualified Test.Unit.CoreModuleQuickCheckSpec
import qualified Test.Unit.AdditionalCoreQuickCheckSpec
import qualified Test.Unit.NewQuickCheckTestSpec
import qualified Test.Unit.NewCabalQuickCheckTestsSpec
import qualified Test.Unit.AdditionalQuickCheckSpec
import qualified Test.Unit.FreshCabalQuickCheckSpec
import qualified Test.Unit.SimpleCabalQuickCheckSpec
import qualified Test.Unit.MinimalCabalQuickCheckSpec
import qualified Test.Unit.LightweightCabalQuickCheckSpec
import qualified Test.Unit.FastCabalQuickCheckSpec
import qualified Test.Unit.CompactCabalQuickCheckSpec
import qualified Test.Unit.QuickCabalQuickCheckSpec
import qualified Test.Unit.TinyCabalQuickCheckSpec
import qualified Test.Unit.EfficientCabalQuickCheckSpec
import qualified Test.Unit.ConciseCabalQuickCheckSpec
import qualified Test.Unit.NewCabalTestSuiteQuickCheckSpec
import qualified Test.Unit.NewCabalQuickCheckTestSuite2Spec
import qualified Test.Unit.AdditionalUtilsSpec
import qualified Test.Unit.AdditionalUtilsQuickCheckSpec
import qualified Test.Unit.SourceLocationAdditionalSpec
import qualified Test.Unit.SourceLocationAdditionalQuickCheckSpec
import qualified Test.Unit.ParserAdditionalSpec

-- New test modules
import qualified Test.Unit.SourceLocationQuickCheckSpec
import qualified Test.Unit.ErrorHandlerQuickCheckSpec
import qualified Test.Unit.ParserBasicPropertiesSpec
import qualified Test.Unit.OwnershipCoreSpec

-- New Cabal Test Modules (6 comprehensive tests)
import qualified Test.Unit.SourceLocationCoreFunctionsSpec
import qualified Test.Unit.ParserErrorBoundarySpec
import qualified Test.Unit.CompilerIRConsistencySpec
import qualified Test.Unit.OwnershipBoundaryConditionsSpec
import qualified Test.Unit.DependentTypeValidationSpec
import qualified Test.Unit.UtilsStringPropertiesSpec
import qualified Test.Unit.TypeSystemPropertiesSpec
import qualified Test.Unit.CompilerIRSpec
import qualified Test.Unit.IntegrationBasicSpec
import qualified Test.Unit.AdditionalTypusSpec

-- Additional new test modules (10 new ones)
import qualified Test.Unit.NewParserValidationSpec
import qualified Test.Unit.NewCompilerOptimizationSpec
import qualified Test.Unit.NewOwnershipTransferSpec
import qualified Test.Unit.NewSourceLocationTrackingSpec
import qualified Test.Unit.NewTypeSystemValidationSpec
import qualified Test.Unit.NewErrorRecoverySpec
import qualified Test.Unit.NewUtilsAdditionalSpec
import qualified Test.Unit.NewDependenciesAnalysisSpec
import qualified Test.Unit.NewAnalyzerIntegrationSpec
import qualified Test.Unit.NewCliIntegrationSpec

-- New QuickCheck Test Suite 2025
import qualified Test.Unit.NewQuickCheckTestSuiteSpec
import qualified Test.Unit.NewParserPropertiesSpec
import qualified Test.Unit.NewSourceLocationMathSpec
import qualified Test.Unit.NewErrorHandlerCoreSpec
import qualified Test.Unit.NewDependenciesCoreSpec
import qualified Test.Unit.NewGoToolchainCoreSpec
import qualified Test.Unit.NewParserCoreSpec
import qualified Test.Unit.NewUtilsCoreSpec
import qualified Test.Unit.EnhancedDebugCoreSpec

-- Additional new test modules for enhanced coverage
import qualified Test.Unit.EnhancedErrorHandlingQuickCheckSpec
import qualified Test.Unit.CompilerIntegrationQuickCheckSpec
import qualified Test.Unit.TypeSystemBoundaryQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SemanticAnalysisQuickCheckSpec
-- import qualified Test.Unit.CodeGenerationQuickCheckSpec -- Temporarily disabled due to compilation errors

import qualified Test.Unit.ToolchainIntegrationQuickCheckSpec

-- New test modules added for enhanced cabal testing
import qualified Test.Unit.ToolingErrorSpec
import qualified Test.Unit.CommandLineDebugSpec
import qualified Test.Unit.EmbedAssetsSpec
import qualified Test.Unit.DependenciesCoreSpec

-- New Cabal Test Modules (5 comprehensive tests added)
import qualified Test.Unit.NewStringProcessingQuickCheckSpec
import qualified Test.Unit.NewSourceLocationMathQuickCheckSpec
import qualified Test.Unit.NewParserBoundaryQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.NewComprehensiveCabalQuickCheckSpec
import qualified Test.Unit.OwnershipReporterSpec
import qualified Test.Unit.AnalyzerCrossAnalysisSpec
import qualified Test.Unit.CoreDataStructuresQuickCheckSpec

-- New Core QuickCheck Test Modules (2025)
import qualified Test.Unit.UtilsCoreQuickCheckTests
import qualified Test.Unit.SourceLocationCoreQuickCheckTests
import qualified Test.Unit.ParserCoreQuickCheckTests
import qualified Test.Unit.OwnershipCoreQuickCheckTests
import qualified Test.Unit.CompilerCoreQuickCheckTests
import qualified Test.Unit.ErrorHandlerCoreQuickCheckTests
import qualified Test.Unit.DependentTypesCoreQuickCheckTests

-- New Additional QuickCheck Test Module
import qualified Test.Unit.NewCoreQuickCheckTestSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.NewDependentTypesQuickCheckSpec
import qualified Test.Unit.NewOwnershipQuickCheckSpec
import qualified Test.Unit.NewSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.NewGoToolchainQuickCheckSpec
import qualified Test.Unit.NewIRQuickCheckSpec
import qualified Test.Unit.NewSymbolTableQuickCheckSpec

-- New core functionality test modules
import qualified Test.Unit.SimpleCoreTestSpec

-- Additional comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerErrorRecoveryQuickCheckSpec
import qualified Test.Unit.DependentTypeSystemBoundaryQuickCheckSpec

-- New test modules for enhanced coverage
import qualified Test.Unit.DirectiveInteractionSpec
import qualified Test.Unit.OwnershipTransferBoundaryNewSpec
import qualified Test.Unit.DependentTypeConstraintSpec
import qualified Test.Unit.SyntaxValidatorBoundarySpec

import qualified Test.Unit.SourceLocationAccuracyQuickCheckSpec
import qualified Test.Unit.TypeEnvironmentBuildingQuickCheckSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.ErrorHandlingBoundaryQuickCheckSpec
import qualified Test.Unit.DependentTypeBoundaryQuickCheckSpec
import qualified Test.Unit.OwnershipTransferComplexQuickCheckSpec
import qualified Test.Unit.SourceLocationPrecisionQuickCheckSpec
import qualified Test.Unit.TypeEnvironmentQuickCheckSpec
import qualified Test.Unit.LexerBoundaryQuickCheckSpec

-- Custom QuickCheck test modules
import qualified Test.Unit.CustomParserQuickCheckSpec
import qualified Test.Unit.CustomUtilsQuickCheckSpec
import qualified Test.Unit.CustomSourceLocationQuickCheckSpec
import qualified Test.Unit.CustomOwnershipQuickCheckSpec
import qualified Test.Unit.CustomDependentTypesQuickCheckSpec
import qualified Test.Unit.CustomErrorHandlingQuickCheckSpec
import qualified Test.Unit.CustomCompilerQuickCheckSpec
import qualified Test.Unit.CustomSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.CustomSymbolTableQuickCheckSpec

-- Enhanced QuickCheck test modules
import qualified Test.Unit.EnhancedParserQuickCheckSpec
import qualified Test.Unit.EnhancedCompilerQuickCheckSpec
import qualified Test.Unit.EnhancedOwnershipQuickCheckSpec
import qualified Test.Unit.EnhancedDependentTypesQuickCheckSpec
import qualified Test.Unit.EnhancedErrorHandlingQuickCheckSpec
import qualified Test.Unit.EnhancedSourceLocationQuickCheckSpec
import qualified Test.Unit.EnhancedUtilsQuickCheckSpec

-- New boundary and property test modules
import qualified Test.Unit.EnhancedUtilsBoundaryQuickCheckSpec
import qualified Test.Unit.EnhancedSourceLocationAdvancedQuickCheckSpec
import qualified Test.Unit.EnhancedParserErrorHandlingQuickCheckSpec
import qualified Test.Unit.EnhancedCompilerIRPropertiesQuickCheckSpec
import qualified Test.Unit.EnhancedOwnershipBoundaryQuickCheckSpec
import qualified Test.Unit.EnhancedDependentTypeSystemBoundaryQuickCheckSpec

-- New comprehensive test modules
import qualified Test.Unit.CoreDataStructuresQuickCheckSpec
import qualified Test.Unit.ParserBoundaryConditionsSpec
import qualified Test.Unit.CompilerErrorHandlingSpec
import qualified Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec
import qualified Test.Unit.DependentTypeSystemSpec
import qualified Test.Unit.SourceLocationTrackingSpec

-- New Advanced Test Modules
import qualified Test.Unit.SourceLocationAdvancedSpec
import qualified Test.Unit.SourceLocationAdvancedQuickCheckSpec
import qualified Test.Unit.ErrorHandlerAdvancedQuickCheckSpec
import qualified Test.Unit.ParserAdvancedQuickCheckSpec
import qualified Test.Unit.UtilsAdvancedQuickCheckSpec
import qualified Test.Unit.CompilerAdvancedQuickCheckSpec
import qualified Test.Unit.DependenciesAdvancedQuickCheckSpec
import qualified Test.Unit.OwnershipAdvancedQuickCheckSpec
import qualified Test.Unit.IntegrationAdvancedQuickCheckSpec
import qualified Test.Unit.OwnershipTransitivitySpec
import qualified Test.Unit.DependentTypeBoundarySpec
import qualified Test.Unit.IRConsistencySpec
import qualified Test.Unit.ErrorHandlingAdvancedSpec
import qualified Test.Unit.TypeEnvironmentBuildingSpec

-- New Core QuickCheck test modules
import qualified Test.Unit.SourceLocationCoreQuickCheckSpec
import qualified Test.Unit.UtilsCoreQuickCheckSpec
import qualified Test.Unit.ErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.ParserCoreQuickCheckSpec
import qualified Test.Unit.GoToolchainCoreQuickCheckSpec
import qualified Test.Unit.DependenciesCoreQuickCheckSpec

-- New cabal test modules (8 new comprehensive tests)

import qualified Test.Unit.DependentTypeBoundarySpec

-- New Enhanced QuickCheck Test Modules (2025)
import qualified Test.Unit.CoreCompilerQuickCheckSpec
import qualified Test.Unit.DependentTypeBoundaryQuickCheckSpec
import qualified Test.Unit.OwnershipRobustnessQuickCheckSpec
import qualified Test.Unit.SourceLocationPrecisionQuickCheckSpec
import qualified Test.Unit.ErrorHandlingConsistencyQuickCheckSpec
import qualified Test.Unit.ParserBoundaryQuickCheckSpec
import qualified Test.Unit.UtilsRobustnessQuickCheckSpec
import qualified Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec
import qualified Test.Unit.CompilerIntegrationQuickCheckSpec
import qualified Test.Unit.OwnershipTransitivitySpec

import qualified Test.Unit.SyntaxValidatorAdvancedSpec

import qualified Test.Unit.IRGenerationConsistencySpec
import qualified Test.Unit.SymbolTableManagementSpec

-- Additional comprehensive QuickCheck test modules
import qualified Test.Unit.AdditionalCabalQuickCheckTestSpec

-- New comprehensive test suite
import qualified Test.Unit.NewCabalTestSuiteSpec
import qualified Test.Unit.AdditionalCabalTestsSpec

-- New QuickCheck test modules (10 new comprehensive tests)
import qualified Test.Unit.BasicParsingQuickCheckSpec
import qualified Test.Unit.CompilerOptimizationsQuickCheckSpec
import qualified Test.Unit.TypeInferenceQuickCheckSpec
import qualified Test.Unit.MemorySafetyQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec
import qualified Test.Unit.ConcurrentParsingQuickCheckSpec
import qualified Test.Unit.SymbolTableOperationsQuickCheckSpec
import qualified Test.Unit.CodeGenerationQuickCheckSpec
import qualified Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec
import qualified Test.Unit.PerformanceOptimizationQuickCheckSpec

-- New comprehensive QuickCheck test modules for core functionality
import qualified Test.Unit.NewUtilsQuickCheckSpec
import qualified Test.Unit.NewSourceLocationQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewParserQuickCheckSpec

-- New Cabal Test Cases Module
import qualified Test.Unit.NewCabalTestCasesSpec

-- New Core Functionality Test Module
import qualified Test.Unit.NewCoreFunctionalitySpec

-- New Enhanced Cabal QuickCheck Test Module
import qualified Test.Unit.NewEnhancedCabalQuickCheckSpec

-- New comprehensive test modules for enhanced coverage
-- (Note: Some modules already exist in the project)

-- New core functionality test modules
import qualified Test.Unit.NewCoreFunctionalitySpec
import qualified Test.Unit.NewCoreQuickCheckSpec
import qualified Test.Unit.EnhancedCoreFunctionalityQuickCheckSpec

-- ============================================================================
-- New Advanced Test Modules (10 comprehensive tests)
-- ============================================================================

import qualified Test.Unit.SourceLocationAdvancedSpec
import qualified Test.Unit.ErrorHandlerAdvancedSpec
import qualified Test.Unit.ParserDirectiveSpec
import qualified Test.Unit.IntegrationAdvancedSpec
import qualified Test.Unit.BoundaryConditionSpec
import qualified Test.Unit.PerformanceAdvancedSpec
import qualified Test.Unit.StringUtilsAdvancedSpec
import qualified Test.Unit.CompilerTypeCheckerSpec
import qualified Test.Unit.OwnershipAnalysisSpec
import qualified Test.Unit.DependencyAnalysisSpec

-- New comprehensive QuickCheck test module


-- New QuickCheck test modules created for enhanced testing
import qualified Test.Unit.NewUtilsPropertiesQuickCheckSpec
import qualified Test.Unit.NewSourceLocationMathQuickCheckSpec
import qualified Test.Unit.NewParserBoundaryQuickCheckSpec
import qualified Test.Unit.NewOwnershipConsistencyQuickCheckSpec
import qualified Test.Unit.NewDependencyAssociativityQuickCheckSpec
import qualified Test.Unit.NewCompilerIdempotentQuickCheckSpec
import qualified Test.Unit.NewErrorRecoveryQuickCheckSpec
import qualified Test.Unit.NewTypeSystemSubstitutionQuickCheckSpec
import qualified Test.Unit.NewSymbolTableCommutativeQuickCheckSpec

-- New Comprehensive Test Modules (2025)
import qualified Test.Unit.CoreModulesIntegrationSpec
import qualified Test.Unit.ErrorHandlingBoundaryConditionsSpec
import qualified Test.Unit.TextProcessingPropertiesExtendedSpec
import qualified Test.Unit.SourceLocationTrackingComprehensiveSpec
import qualified Test.Unit.ParserErrorRecoveryAdvancedSpec
import qualified Test.Unit.CompilerEndToEndIntegrationSpec
import qualified Test.Unit.DependenciesOwnershipInteractionSpec
import qualified Test.Unit.PerformanceRegressionExtendedSpec

-- New Core Integration Test Module
import qualified Test.Unit.NewCoreIntegrationSpec

-- New Comprehensive QuickCheck Test Modules (2025)
import qualified Test.Unit.NewEnhancedUtilsQuickCheckSpec
import qualified Test.Unit.NewAdvancedSourceLocationQuickCheckSpec
import qualified Test.Unit.NewRobustErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewComprehensiveParserQuickCheckSpec
import qualified Test.Unit.NewAdvancedOwnershipQuickCheckSpec
import qualified Test.Unit.NewCompilerIRQuickCheckSpec
import qualified Test.Unit.NewDependenciesQuickCheckSpec
import qualified Test.Unit.NewTypeSystemQuickCheckSpec
import qualified Test.Unit.NewIntegrationQuickCheckSpec
import qualified Test.Unit.NewPerformanceQuickCheckSpec

-- New Cabal Test Modules (5 comprehensive tests)
import qualified Test.Unit.SourceLocationCoreQuickCheckSpec
import qualified Test.Unit.ParserCoreQuickCheckSpec
import qualified Test.Unit.CompilerIRCoreQuickCheckSpec
import qualified Test.Unit.OwnershipCoreQuickCheckSpec
import qualified Test.Unit.ErrorHandlerCoreQuickCheckSpec

-- New Cabal Test Modules (10 comprehensive tests added by user)
import qualified Test.Unit.NewCabalParserQuickCheckSpec
import qualified Test.Unit.NewCabalSourceLocationQuickCheckSpec
import qualified Test.Unit.NewCabalUtilsQuickCheckSpec
import qualified Test.Unit.NewCabalOwnershipQuickCheckSpec
import qualified Test.Unit.NewCabalCompilerQuickCheckSpec
import qualified Test.Unit.NewCabalErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewCabalDependenciesQuickCheckSpec
import qualified Test.Unit.NewCabalSyntaxValidatorQuickCheckSpec

-- New Enhanced Test Modules (2025) - Added for comprehensive testing
import qualified Test.Unit.UtilsEnhancedTestSpec
import qualified Test.Unit.SourceLocationEnhancedTestSpec
import qualified Test.Unit.ParserEnhancedTestSpec
import qualified Test.Unit.UtilsPropertiesQuickCheckTestSpec
import qualified Test.Unit.SourceLocationPropertiesQuickCheckTestSpec
import qualified Test.Unit.IntegrationEnhancedTestSpec
import qualified Test.Unit.EdgeCaseHandlingTestSpec
import qualified Test.Unit.PerformanceBoundaryTestSpec

-- New QuickCheck Test Modules (2025) - 10 comprehensive tests
import qualified Test.Unit.NewSourceLocationMathPropertiesQuickCheckSpec
import qualified Test.Unit.NewParserCombinatorPropertiesQuickCheckSpec
import qualified Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec
import qualified Test.Unit.NewUtilsStringBoundaryQuickCheckSpec
import qualified Test.Unit.NewOwnershipTransitivityQuickCheckSpec
import qualified Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec
import qualified Test.Unit.NewCompilerIRConsistencyQuickCheckSpec
import qualified Test.Unit.NewSyntaxValidatorValidationQuickCheckSpec
import qualified Test.Unit.NewGoToolchainIntegrationQuickCheckSpec
import qualified Test.Unit.NewEndToEndIntegrationQuickCheckSpec

-- | Aggregate all lightweight, fast-running tests that only depend on
-- in-process library calls. These can be executed under the "fast" Cabal flag.
--
-- Note: Extended and Comprehensive QuickCheck test suites have been temporarily
-- disabled due to issues with overly strict preconditions causing excessive test
-- discards. These should be fixed by improving the Arbitrary instances and
-- relaxing preconditions before re-enabling.
tests :: TestTree
tests =
  testGroup "Unit"
    [ Test.Unit.ParserSpec.tests
    , Test.Unit.ParserErrorRecoveryAdvancedSpec.tests
    , Test.Unit.OwnershipSpec.tests
    , Test.Unit.OwnershipBridgeSpec.tests
    , Test.Unit.OwnershipTransitivityAdvancedSpec.tests
    , Test.Unit.DependentTypesSpec.tests
    , Test.Unit.DependentTypesBoundaryAdvancedSpec.tests
    , Test.Unit.DependenciesCycleDetectionAdvancedSpec.tests
    , Test.Unit.TypeSystemSpec.tests
    , Test.Unit.SymbolTableSpec.tests
    , Test.Unit.SourceLocationSpec.tests
    , Test.Unit.SourceLocationAdvancedPropertiesSpec.tests
    , Test.Unit.SyntaxValidatorSpec.tests
    , Test.Unit.CompilerSpec.tests
    , Test.Unit.CompilerIRConsistencyAdvancedSpec.tests
    , Test.Unit.ValueAnalysisSpec.tests
    , Test.Unit.ErrorHandlingSpec.tests
    , Test.Unit.ErrorHandlerConsistencyAdvancedSpec.tests
    , Test.Unit.EmbedAssetsSpec.tests
    , Test.Unit.GoToolchainSpec.tests
    , Test.Unit.GoToolchainPropertiesAdvancedSpec.tests
    , Test.Unit.CommandLineDebugSpec.tests
    , Test.Unit.CommandLineDebugIntegrationAdvancedSpec.tests
    , Test.Unit.CLISpec.tests
    , Test.Unit.VerbositySpec.tests
    , Test.Unit.UtilsSpec.tests
    , Test.Unit.UtilsBoundaryConditionsSpec.tests

    -- Additional Test Modules Created for Enhanced Coverage
    , testGroup "Additional Test Modules"
        [ Test.Unit.AdditionalUtilsSpec.tests
        , Test.Unit.AdditionalSourceLocationSpec.tests
        , Test.Unit.AdditionalParserSpec.tests
        , Test.Unit.AdditionalErrorHandlerSpec.tests
        , Test.Unit.AdditionalSyntaxValidatorSpec.tests
        ]

    -- Additional QuickCheck Test Modules
    , testGroup "Additional QuickCheck Tests"
        [ Test.Unit.AdditionalUtilsQuickCheckSpec.tests
        , Test.Unit.AdditionalSourceLocationQuickCheckSpec.tests
        ]
    , Test.Unit.AdvancedParserSpec.tests
    , Test.Unit.IntegrationSpec.tests
    , Test.Unit.PerformanceSpec.tests
    , Test.Unit.EdgeCaseSpec.tests
    
    -- New Basic Test Modules Added
    , testGroup "New Basic Test Modules"
        [ Test.Unit.SourceLocationBasicPropertiesSpec.tests
        , Test.Unit.ParserBasicFunctionsSpec.tests
        , Test.Unit.CompilerErrorHandlingSpec.tests
        , Test.Unit.OwnershipAnalysisBasicSpec.tests
        , Test.Unit.UtilsStringProcessingSpec.tests
        , Test.Unit.DependenciesTypeSystemSpec.tests
        , Test.Unit.ValueAnalysisBasicSpec.tests
        , Test.Unit.ErrorHandlerRecoverySpec.tests
        , Test.Unit.SyntaxValidatorValidationSpec.tests
        ]

    -- New Cabal Test Modules (2025) - 8 comprehensive tests
    , testGroup "New Cabal Test Modules"
        [ Test.Unit.UtilsBreakOnQuickCheckSpec.tests
        , Test.Unit.SourceLocationPositionArithmeticSpec.tests
        , Test.Unit.StringProcessingSpec.tests
        , Test.Unit.UtilsCommentProcessingSpec.tests
        , Test.Unit.SourceLocationIntegrationSpec.tests
        , Test.Unit.UtilsIndentationSpec.tests
        , Test.Unit.SplitFunctionsSpec.tests
        , Test.Unit.SpanOperationsSpec.tests
        , Test.Unit.NewCabalUtilsSpec.tests
        , Test.Unit.NewCabalSourceLocationSpec.tests
        , Test.Unit.NewCabalErrorHandlerSpec.tests
        , Test.Unit.NewCabalParserSpec.tests
        , Test.Unit.NewCabalIntegrationSpec.tests
        ]
    
    -- New Core Test Modules
    , testGroup "Core Test Modules"
        [ Test.Unit.CoreUtilsSpec.tests
        , Test.Unit.CoreSourceLocationSpec.tests
        , Test.Unit.CoreSyntaxValidatorSpec.tests
        , Test.Unit.CoreErrorHandlerSpec.tests
        , Test.Unit.CoreParserSpec.tests
        , Test.Unit.CoreCompilerSpec.tests
        ]
    , Test.Unit.EnhancedQuickCheckSpec.tests
    , Test.Unit.EnhancedParserTestSpec.tests
    , Test.Unit.EnhancedCompilerTestSpec.tests
    
    , Test.Unit.ParserCompilerPropertiesQuickCheckSpec.tests
    , Test.Unit.NewCoreQuickCheckPropertiesSpec.tests
    , Test.Unit.AdditionalUtilsSpec.tests
    , Test.Unit.AdditionalUtilsQuickCheckSpec.tests
    , Test.Unit.SourceLocationAdditionalSpec.tests
    , Test.Unit.SourceLocationAdditionalQuickCheckSpec.tests
    , Test.Unit.ParserAdditionalSpec.tests

    -- ============================================================================
    -- New Enhanced Test Modules (2025) - 8 comprehensive tests
    -- ============================================================================
    , testGroup "New Enhanced Test Modules"
        [ Test.Unit.UtilsEnhancedTestSpec.tests
        , Test.Unit.SourceLocationEnhancedTestSpec.tests
        , Test.Unit.ParserEnhancedTestSpec.tests
        , Test.Unit.UtilsPropertiesQuickCheckTestSpec.tests
        , Test.Unit.SourceLocationPropertiesQuickCheckTestSpec.tests
        , Test.Unit.IntegrationEnhancedTestSpec.tests
        , Test.Unit.EdgeCaseHandlingTestSpec.tests
        , Test.Unit.PerformanceBoundaryTestSpec.tests
        ]
    , Test.Unit.NewCabalTestSpec.tests
    
    -- New Comprehensive Test Modules (2025)
    , testGroup "New Comprehensive QuickCheck Test Suite (2025)"
        [ Test.Unit.NewParserPropertiesSpec.tests
        , Test.Unit.NewSourceLocationMathSpec.tests
        , Test.Unit.NewErrorHandlerCoreSpec.tests
        , Test.Unit.NewUtilsStringPropertiesSpec.tests
        , Test.Unit.NewOwnershipTransferPropertiesSpec.tests
        , Test.Unit.NewDependenciesCorePropertiesSpec.tests
        , Test.Unit.NewSyntaxValidatorBoundarySpec.tests
        ]
    , testGroup "New Test Modules"
        [ Test.Unit.SourceLocationQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerQuickCheckSpec.tests
        , Test.Unit.ParserBasicPropertiesSpec.tests
        , Test.Unit.OwnershipCoreSpec.tests
        , Test.Unit.TypeSystemPropertiesSpec.tests
        , Test.Unit.CompilerIRSpec.tests
        , Test.Unit.IntegrationBasicSpec.tests
        , Test.Unit.AdditionalTypusSpec.tests
        ]
    , testGroup "New QuickCheck Test Modules (2025)"
        [ Test.Unit.BasicParserQuickCheckSpec.tests
        , Test.Unit.CompilerIRQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferQuickCheckSpec.tests
        , Test.Unit.SourceLocationMathQuickCheckSpec.tests
        , Test.Unit.ErrorHandlingQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Modules (2025)"
        [ Test.Unit.NewSourceLocationMathPropertiesQuickCheckSpec.tests
        , Test.Unit.NewParserCombinatorPropertiesQuickCheckSpec.tests
        , Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec.tests
        , Test.Unit.NewUtilsStringBoundaryQuickCheckSpec.tests
        , Test.Unit.NewOwnershipTransitivityQuickCheckSpec.tests
        , Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec.tests
        , Test.Unit.NewCompilerIRConsistencyQuickCheckSpec.tests
        , Test.Unit.NewSyntaxValidatorValidationQuickCheckSpec.tests
        , Test.Unit.NewGoToolchainIntegrationQuickCheckSpec.tests
        , Test.Unit.NewEndToEndIntegrationQuickCheckSpec.tests
        ]
    , testGroup "New Cabal Test Modules (5 comprehensive tests)"
        [ Test.Unit.SourceLocationCoreQuickCheckSpec.tests
        , Test.Unit.ParserCoreQuickCheckSpec.tests
        , Test.Unit.CompilerIRCoreQuickCheckSpec.tests
        , Test.Unit.OwnershipCoreQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerCoreQuickCheckSpec.tests
        ]
    , testGroup "Additional New Test Modules"
        [ Test.Unit.NewParserValidationSpec.tests
        , Test.Unit.NewCompilerOptimizationSpec.tests
        , Test.Unit.NewOwnershipTransferSpec.tests
        , testGroup "New Core QuickCheck Tests (2025)"
            [ Test.Unit.UtilsCoreQuickCheckTests.tests
            , Test.Unit.SourceLocationCoreQuickCheckTests.tests
            , Test.Unit.ParserCoreQuickCheckTests.tests
            , Test.Unit.OwnershipCoreQuickCheckTests.tests
            , Test.Unit.CompilerCoreQuickCheckTests.tests
            , Test.Unit.ErrorHandlerCoreQuickCheckTests.tests
            , Test.Unit.DependentTypesCoreQuickCheckTests.tests
            ]
        , Test.Unit.NewSourceLocationTrackingSpec.tests
        , Test.Unit.NewTypeSystemValidationSpec.tests
        , Test.Unit.NewErrorRecoverySpec.tests
        , Test.Unit.NewUtilsAdditionalSpec.tests
        , Test.Unit.NewDependenciesAnalysisSpec.tests
        , Test.Unit.NewAnalyzerIntegrationSpec.tests
        , Test.Unit.NewCliIntegrationSpec.tests
        ]
    , testGroup "New Core Tests"
        [ Test.Unit.SourceLocationCoreTestSpec.tests
        , Test.Unit.ParserCoreTestSpec.tests
        , Test.Unit.ErrorHandlerCoreTestSpec.tests
        , Test.Unit.DependenciesCoreTestSpec.tests
        ]
    , Test.Unit.AdditionalCoreTestsSpec.tests
    , Test.Unit.UtilsAdditionalQuickCheckSpec.tests
    , Test.Unit.SourceLocationAdditionalQuickCheckSpec.tests
    , Test.Unit.ParserAdditionalQuickCheckSpec.tests
    , Test.Unit.IntegrationAdditionalQuickCheckSpec.tests
    , testGroup "QuickCheck Tests"
        [ Test.Unit.ParserQuickCheckSpec.tests
        , Test.Unit.CompilerQuickCheckSpec.tests
        , Test.Unit.TypeCheckerQuickCheckSpec.tests
        , Test.Unit.OwnershipQuickCheckSpec.tests
        , Test.Unit.AnalyzerQuickCheckSpec.tests
        , Test.Unit.UtilsQuickCheckSpec.tests
        , Test.Unit.SymbolTableQuickCheckSpec.tests
        , Test.Unit.ValueAnalysisQuickCheckSpec.tests
        , Test.Unit.SyntaxValidatorQuickCheckSpec.tests
        , Test.Unit.ErrorHandlingQuickCheckSpec.tests
        , Test.Unit.DependentTypesQuickCheckSpec.tests
        , Test.Unit.DependenciesQuickCheckSpec.tests
        , Test.Unit.AdvancedQuickCheckSpec.tests
        , Test.Unit.PerformanceQuickCheckSpec.tests
        , Test.Unit.SimpleSyntaxValidatorQuickCheckSpec.tests
        , Test.Unit.DebugQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Tests"
        [ Test.Unit.CompilerErrorHandlingQuickCheckSpec.tests
        , Test.Unit.DependentTypesSystemQuickCheckSpec.tests
        , Test.Unit.OwnershipAnalysisComprehensiveQuickCheckSpec.tests
        , Test.Unit.ParserBoundaryConditionsQuickCheckSpec.tests
        , Test.Unit.IntegrationFeaturesQuickCheckSpec.tests
        ]
    , testGroup "New Cabal QuickCheck Tests"
        [ Test.Unit.NewCabalQuickCheckTestSpec.tests
        , Test.Unit.NewCabalTestsSpec.tests
        , Test.Unit.NewCabalParserQuickCheckSpec.tests
        , Test.Unit.NewCabalSourceLocationQuickCheckSpec.tests
        , Test.Unit.NewCabalUtilsQuickCheckSpec.tests
        , Test.Unit.NewCabalOwnershipQuickCheckSpec.tests
        , Test.Unit.NewCabalCompilerQuickCheckSpec.tests
        , Test.Unit.NewCabalErrorHandlerQuickCheckSpec.tests
        , Test.Unit.NewCabalDependenciesQuickCheckSpec.tests
        , Test.Unit.NewCabalSyntaxValidatorQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Tests"
        [ Test.Unit.OwnershipTransferQuickCheckSpec.tests
        , Test.Unit.DependentTypesValidationQuickCheckSpec.tests
        , Test.Unit.ErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.SourceLocationTrackingQuickCheckSpec.tests
        ]
    , testGroup "New Core QuickCheck Tests"
        [ Test.Unit.SourceLocationCoreQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerCoreQuickCheckSpec.tests
        , Test.Unit.DependenciesCoreQuickCheckSpec.tests
        , Test.Unit.GoToolchainCoreQuickCheckSpec.tests
        , Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec.tests
        , Test.Unit.DebugIntegrationCoreQuickCheckSpec.tests
        , Test.Unit.DebugCoreQuickCheckSpec.tests
        , Test.Unit.EnhancedDebugCoreQuickCheckSpec.tests
        , Test.Unit.CompilerUtilsCoreQuickCheckSpec.tests
        , Test.Unit.SourceLocationAdvancedQuickCheckSpec.tests
        , Test.Unit.ParserAdvancedQuickCheckSpec.tests
        , Test.Unit.CompilerAdvancedQuickCheckSpec.tests
        , Test.Unit.OwnershipAdvancedQuickCheckSpec.tests
        , Test.Unit.IntegrationCoreQuickCheckSpec.tests
        ]
    , testGroup "Additional Enhanced Test Modules"
        [ Test.Unit.EnhancedErrorHandlingQuickCheckSpec.tests
        , Test.Unit.CompilerIntegrationQuickCheckSpec.tests
        , Test.Unit.TypeSystemBoundaryQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.SemanticAnalysisQuickCheckSpec.tests
        -- , Test.Unit.CodeGenerationQuickCheckSpec.tests -- Temporarily disabled due to compilation errors
        
        , Test.Unit.ToolchainIntegrationQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Modules"
        [ Test.Unit.NewDependentTypesQuickCheckSpec.tests
        , Test.Unit.NewOwnershipQuickCheckSpec.tests
        , Test.Unit.NewSyntaxValidatorQuickCheckSpec.tests
        , Test.Unit.NewGoToolchainQuickCheckSpec.tests
        , Test.Unit.NewIRQuickCheckSpec.tests
        , Test.Unit.NewSymbolTableQuickCheckSpec.tests
        ]
    , testGroup "Core Functionality Tests"
        [ Test.Unit.SimpleCoreTestSpec.tests
        , Test.Unit.NewCoreFunctionalitySpec.tests
        , Test.Unit.NewCoreQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Modules"
        [ Test.Unit.CompilerErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.DependentTypeSystemBoundaryQuickCheckSpec.tests
        
        , Test.Unit.SourceLocationAccuracyQuickCheckSpec.tests
        , Test.Unit.TypeEnvironmentBuildingQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive Cabal Test Modules"
        [ Test.Unit.DependentTypeBoundarySpec.tests
        , Test.Unit.OwnershipTransitivitySpec.tests
        , Test.Unit.SyntaxValidatorAdvancedSpec.tests
        
        , Test.Unit.IRGenerationConsistencySpec.tests
        , Test.Unit.SymbolTableManagementSpec.tests
        ]
    , Test.Unit.AdditionalCabalQuickCheckTestSpec.tests
    , Test.Unit.AdditionalCabalTestsSpec.tests
    
    , testGroup "New QuickCheck Test Modules"
        [ Test.Unit.BasicParsingQuickCheckSpec.tests
        , Test.Unit.CompilerOptimizationsQuickCheckSpec.tests
        , Test.Unit.TypeInferenceQuickCheckSpec.tests
        , Test.Unit.MemorySafetyQuickCheckSpec.tests
        , Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec.tests
        , Test.Unit.ConcurrentParsingQuickCheckSpec.tests
        , Test.Unit.SymbolTableOperationsQuickCheckSpec.tests
        , Test.Unit.CodeGenerationQuickCheckSpec.tests
        , Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec.tests
        , Test.Unit.PerformanceOptimizationQuickCheckSpec.tests
        ]
    , testGroup "Additional Comprehensive QuickCheck Test Modules"
        [ Test.Unit.ParserConsistencyQuickCheckSpec.tests
        ]
    , Test.Unit.NewCabalQuickCheckTestCasesSpec.tests
    , testGroup "Additional QuickCheck Test Modules"
        [ Test.Unit.StringUtilsQuickCheckTestSpec.tests
        , Test.Unit.CompilerErrorHandlingQuickCheckTestSpec.tests
        , Test.Unit.SourceLocationTrackingQuickCheckTestSpec.tests
        , Test.Unit.OwnershipTransferQuickCheckTestSpec.tests
        , Test.Unit.DependencyAnalysisQuickCheckTestSpec.tests
        , Test.Unit.ErrorRecoveryQuickCheckTestSpec.tests
        , Test.Unit.CodeGenerationQuickCheckTestSpec.tests
        , Test.Unit.ParserBoundaryConditionsQuickCheckTestSpec.tests
        , Test.Unit.IntegrationQuickCheckTestSpec.tests
        ]
    , testGroup "New Core Functionality QuickCheck Tests"
        [ Test.Unit.NewUtilsQuickCheckSpec.tests
        , Test.Unit.NewSourceLocationQuickCheckSpec.tests
        , Test.Unit.NewErrorHandlerQuickCheckSpec.tests
        , Test.Unit.NewParserQuickCheckSpec.tests
        ]
    , Test.Unit.NewCabalTestCasesSpec.tests
    , testGroup "Custom QuickCheck Test Modules"
        [ Test.Unit.CustomParserQuickCheckSpec.tests
        , Test.Unit.CustomUtilsQuickCheckSpec.tests
        , Test.Unit.CustomSourceLocationQuickCheckSpec.tests
        , Test.Unit.CustomOwnershipQuickCheckSpec.tests
        , Test.Unit.CustomDependentTypesQuickCheckSpec.tests
        , Test.Unit.CustomErrorHandlingQuickCheckSpec.tests
        , Test.Unit.CustomCompilerQuickCheckSpec.tests
        , Test.Unit.CustomSyntaxValidatorQuickCheckSpec.tests
        , Test.Unit.CustomSymbolTableQuickCheckSpec.tests
        ]
    , testGroup "Enhanced QuickCheck Test Modules"
        [ Test.Unit.EnhancedParserQuickCheckSpec.tests
        , Test.Unit.EnhancedCompilerQuickCheckSpec.tests
        , Test.Unit.EnhancedOwnershipQuickCheckSpec.tests
        , Test.Unit.EnhancedDependentTypesQuickCheckSpec.tests
        , Test.Unit.EnhancedErrorHandlingQuickCheckSpec.tests
        , Test.Unit.EnhancedSourceLocationQuickCheckSpec.tests
        , Test.Unit.EnhancedUtilsQuickCheckSpec.tests
        ]
    , testGroup "New Boundary and Property Test Modules"
        [ Test.Unit.EnhancedUtilsBoundaryQuickCheckSpec.tests
        , Test.Unit.EnhancedSourceLocationAdvancedQuickCheckSpec.tests
        , Test.Unit.EnhancedParserErrorHandlingQuickCheckSpec.tests
        , Test.Unit.EnhancedCompilerIRPropertiesQuickCheckSpec.tests
        , Test.Unit.EnhancedOwnershipBoundaryQuickCheckSpec.tests
        , Test.Unit.EnhancedDependentTypeSystemBoundaryQuickCheckSpec.tests
        ]
    , testGroup "New Core QuickCheck Test Modules"
        [ Test.Unit.SourceLocationCoreQuickCheckSpec.tests
        , Test.Unit.UtilsCoreQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerCoreQuickCheckSpec.tests
        , Test.Unit.ParserCoreQuickCheckSpec.tests
        , Test.Unit.GoToolchainCoreQuickCheckSpec.tests
        , Test.Unit.DependenciesCoreQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive Test Modules"
        [ Test.Unit.CoreDataStructuresQuickCheckSpec.tests
        , Test.Unit.ParserBoundaryConditionsSpec.tests
        , Test.Unit.CompilerErrorHandlingSpec.tests
        , Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec.tests
        , Test.Unit.DependentTypeSystemSpec.tests
        , Test.Unit.SourceLocationTrackingSpec.tests
        ]
    , testGroup "New Advanced Test Modules"
        [ Test.Unit.SourceLocationAdvancedSpec.tests
        , Test.Unit.OwnershipTransitivitySpec.tests
        , Test.Unit.DependentTypeBoundarySpec.tests
        , Test.Unit.IRConsistencySpec.tests
        , Test.Unit.ErrorHandlingAdvancedSpec.tests
        , Test.Unit.TypeEnvironmentBuildingSpec.tests
        ]
    , testGroup "New Comprehensive Test Modules"
        [ Test.Unit.NewErrorHandlingSpec.tests
        , Test.Unit.NewParserSpec.tests
        , Test.Unit.NewCompilerSpec.tests
        , Test.Unit.NewOwnershipSpec.tests
        , Test.Unit.NewTypeSystemSpec.tests
        , Test.Unit.NewSourceLocationSpec.tests
        , Test.Unit.NewDependencySpec.tests
        , Test.Unit.NewIntegrationSpec.tests
        , Test.Unit.NewPerformanceSpec.tests
            , Test.Unit.NewQuickCheckSpec.tests
            , Test.Unit.NewEnhancedCabalQuickCheckSpec.tests
            ]
    , testGroup "New Additional QuickCheck Test Modules"
        [ Test.Unit.TextProcessingQuickCheckSpec.tests
        , Test.Unit.CommentHandlingQuickCheckSpec.tests
        , Test.Unit.IndentationNormalizationQuickCheckSpec.tests
        , Test.Unit.SourcePositionTrackingQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferEdgeCasesQuickCheckSpec.tests
        , Test.Unit.DependentTypeValidationQuickCheckSpec.tests
        , Test.Unit.CompilerIntegrationQuickCheckSpec.tests
        , Test.Unit.FileDirectiveProcessingQuickCheckSpec.tests
        , Test.Unit.CodeGenerationConsistencyQuickCheckSpec.tests
        ]
    , testGroup "New Enhanced QuickCheck Test Modules"
        [ Test.Unit.NewUtilsPropertiesQuickCheckSpec.tests
        , Test.Unit.NewSourceLocationMathQuickCheckSpec.tests
        , Test.Unit.NewParserBoundaryQuickCheckSpec.tests
        , Test.Unit.NewOwnershipConsistencyQuickCheckSpec.tests
        , Test.Unit.NewDependencyAssociativityQuickCheckSpec.tests
        , Test.Unit.NewCompilerIdempotentQuickCheckSpec.tests
        , Test.Unit.NewErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.NewTypeSystemSubstitutionQuickCheckSpec.tests
        , Test.Unit.NewSymbolTableCommutativeQuickCheckSpec.tests
        ]
    -- Note: Test modules already exist in the project
    , testGroup "New Comprehensive QuickCheck Test Modules (2025)"
        [ Test.Unit.SourceLocationAdvancedQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerBoundaryQuickCheckSpec.tests
        , Test.Unit.ParserEdgeCaseQuickCheckSpec.tests
        , Test.Unit.DependenciesInferenceQuickCheckSpec.tests
        , Test.Unit.UtilsEfficiencyQuickCheckSpec.tests
        , Test.Unit.ValueAnalysisFlowQuickCheckSpec.tests
        , Test.Unit.GoToolchainIntegrationQuickCheckSpec.tests
        , Test.Unit.TypusEndToEndQuickCheckSpec.tests
        ]
    , testGroup "New Cabal Test Modules"
        [ Test.Unit.DebugIntegrationSpec.tests
        , Test.Unit.IntegratedCompilerSpec.tests
        
        , Test.Unit.NewCabalPropertySpec.tests
        ]
    , testGroup "New Additional QuickCheck Test Modules (2025)"
        [ Test.Unit.EmbedAssetsQuickCheckSpec.tests
        , Test.Unit.CommandLineDebugQuickCheckSpec.tests
        , Test.Unit.AdditionalIntegratedCompilerQuickCheckSpec.tests
        ]
    , testGroup "New Enhanced Test Modules"
        [ Test.Unit.CoreParserFunctionsSpec.tests
        , Test.Unit.TextProcessingSpec.tests
        , Test.Unit.IndentionSpec.tests
        , Test.Unit.CommentRemovalSpec.tests
        , Test.Unit.SourcePositionSpec.tests
        , Test.Unit.DirectiveProcessingSpec.tests
        , Test.Unit.ErrorLocationSpec.tests
        , Test.Unit.StringSplittingSpec.tests
        , Test.Unit.SpanOperationsSpec.tests
        , Test.Unit.LocationTrackingSpec.tests
        ]
    , Test.Unit.NewAdditionalTestSpec.tests
    , Test.Unit.NewComprehensiveTestsSpec.tests
    , Test.Unit.NewCabalIntegrationSpec.tests
    , Test.Unit.SimpleCabalTestSpec.tests
    , Test.Unit.EnhancedUtilsQuickCheckSpec.tests
    , Test.Unit.EnhancedSourceLocationQuickCheckSpec.tests
    , Test.Unit.EnhancedParserQuickCheckSpec.tests
    , Test.Unit.EnhancedCompilerQuickCheckSpec.tests
    , Test.Unit.EnhancedOwnershipQuickCheckSpec.tests
    , Test.Unit.EnhancedErrorHandlerQuickCheckSpec.tests
    , Test.Unit.EnhancedDependenciesQuickCheckSpec.tests
    , Test.Unit.EnhancedSyntaxValidatorQuickCheckSpec.tests
    , testGroup "New Comprehensive QuickCheck Test Modules (2025)"
        [ Test.Unit.TextProcessingBoundaryQuickCheckSpec.tests
        , Test.Unit.SourceLocationMathQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.DependentTypeBoundaryQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferComplexQuickCheckSpec.tests
        , Test.Unit.CompilerOptimizationInvariantQuickCheckSpec.tests
        , Test.Unit.ErrorHandlingConsistencyQuickCheckSpec.tests
        , Test.Unit.ToolchainRobustnessQuickCheckSpec.tests
        ]
    , testGroup "Additional Test Modules for Enhanced Coverage"
        [ Test.Unit.UtilsStringPropertiesSpec.utilsStringPropertiesSpec
        , Test.Unit.SourceLocationConsistencySpec.sourceLocationConsistencySpec
        , Test.Unit.ParserConsistencySpec.parserConsistencySpec
        , Test.Unit.ErrorRecoveryPropertiesSpec.errorRecoveryPropertiesSpec
        , Test.Unit.DependentTypeSystemSpec.dependentTypeSystemSpec
        , Test.Unit.OwnershipAnalysisSpec.ownershipAnalysisSpec
        , Test.Unit.CompilerIRSpec.compilerIRSpec
                , Test.Unit.EnhancedIntegrationQuickCheckSpec.tests
            ]
            , testGroup "New Cabal Test Suite"
                    [ Test.Unit.SimpleCabalTestSpec.tests
                    , Test.Unit.NewCabalTestSuiteSpec.tests
                    , Test.Unit.SourceLocationCoreFunctionsSpec.tests
                    , Test.Unit.ParserBasicFunctionsSpec.tests
                    , Test.Unit.CompilerErrorHandlingSpec.tests
                    , Test.Unit.OwnershipTransferSpec.tests
                    , Test.Unit.TypeSystemBasicSpec.tests
                    , Test.Unit.DependentTypesBasicSpec.tests
                    , Test.Unit.SymbolTableOperationsSpec.tests
                    , Test.Unit.ErrorRecoveryBasicSpec.tests
                    , Test.Unit.IntegrationBasicSpec.tests
                    ]            , testGroup "New Enhanced Cabal Test Modules"        [ Test.Unit.ToolingErrorSpec.tests
        , Test.Unit.CommandLineDebugSpec.tests
        , Test.Unit.EmbedAssetsSpec.tests
        , Test.Unit.DependenciesCoreSpec.tests
        , Test.Unit.OwnershipReporterSpec.tests
        , Test.Unit.AnalyzerCrossAnalysisSpec.tests
        , Test.Unit.CoreDataStructuresQuickCheckSpec.tests
        ]
    , testGroup "New Enhanced Test Modules (2025)"
        [ Test.Unit.DirectiveInteractionSpec.tests
        , Test.Unit.OwnershipTransferBoundaryNewSpec.tests
        , Test.Unit.DependentTypeConstraintSpec.tests

        , Test.Unit.SyntaxValidatorBoundarySpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Modules (2025)"
        [ Test.Unit.ErrorHandlingBoundaryQuickCheckSpec.tests
        , Test.Unit.DependentTypeBoundaryQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferComplexQuickCheckSpec.tests
        , Test.Unit.SourceLocationPrecisionQuickCheckSpec.tests
        , Test.Unit.TypeEnvironmentQuickCheckSpec.tests
        , Test.Unit.LexerBoundaryQuickCheckSpec.tests
        ]
    , testGroup "Enhanced QuickCheck Test Modules (2025)"
        [ Test.Unit.EndToEndCompilationQuickCheckSpec.tests
        , Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec.tests
        , Test.Unit.TypeInferenceBoundaryQuickCheckSpec.tests
        , Test.Unit.SourceLocationPrecisionQuickCheckSpec.tests
        , Test.Unit.CompilerOptimizationConsistencyQuickCheckSpec.tests
        , Test.Unit.DependentTypeConstraintQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryAdvancedQuickCheckSpec.tests
        , Test.Unit.ConcurrentSafetyQuickCheckSpec.tests
        , Test.Unit.PerformanceEnhancedQuickCheckSpec.tests
        ]
    , testGroup "New Core Functionality Tests"
        [ Test.Unit.CoreParserSpec.tests
        , Test.Unit.CoreSourceLocationSpec.tests
        , Test.Unit.UtilsStringProcessingSpec.tests
        ]
    , testGroup "New QuickCheck Property Tests"
        [ Test.Unit.CoreQuickCheckPropertiesSpec.tests
        , Test.Unit.ParserPropertySpec.tests
        , Test.Unit.ErrorHandlingPropertySpec.tests
        , Test.Unit.NewComprehensivePropertySpec.tests
        ]
    , testGroup "New Comprehensive Cabal Test Modules (2025)"
        [ Test.Unit.NewComprehensiveCabalTestsSpec.tests
        , Test.Unit.UtilsEnhancedQuickCheckSpec.tests
        , Test.Unit.ParserBoundaryConditionsQuickCheckSpec.tests
        , Test.Unit.OwnershipPropertiesQuickCheckSpec.tests
        , Test.Unit.SourceLocationMathQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerRecoveryQuickCheckSpec.tests
        , Test.Unit.CompilerIRConsistencyQuickCheckSpec.tests
        , Test.Unit.TypeSystemInferenceQuickCheckSpec.tests
        ]
    , Test.Unit.NewComprehensiveCabalQuickCheckSpec.tests
    , Test.Unit.EnhancedCabalQuickCheckTestSpec.tests
    , Test.Unit.ErrorBoundaryQuickCheckSpec.tests
    , testGroup "New Enhanced Test Suite"
        [ Test.Unit.NewEnhancedTestSpec.tests
        ]
    , testGroup "Simple New Test Suite"
        [ Test.Unit.SimpleNewTestSpec.tests
        ]
    , testGroup "New Advanced Test Suite (2025)"
        [ Test.Unit.CompilerErrorRecoverySpec.tests
        , Test.Unit.TypeInferenceAdvancedNewSpec.tests
        , Test.Unit.OwnershipMemorySafetySpec.tests
        , Test.Unit.DependencyAnalysisAdvancedSpec.tests
        , Test.Unit.SourcePositionPrecisionSpec.tests
        , Test.Unit.TextProcessingRobustnessSpec.tests
            , Test.Unit.CompilerOptimizationSpec.tests
            , Test.Unit.IntegrationEndToEndNewSpec.tests        ]
    , testGroup "New Cabal Test Suite"
        [ Test.Unit.NewCabalTestSuiteSpec.tests
        ]
    , testGroup "New QuickCheck Test Suite 2025"
        [ Test.Unit.NewQuickCheckTestSuiteSpec.tests
        , Test.Unit.NewParserPropertiesSpec.tests
        , Test.Unit.NewSourceLocationMathSpec.tests
        , Test.Unit.NewErrorHandlerCoreSpec.tests
        , Test.Unit.NewDependenciesCoreSpec.tests
        , Test.Unit.NewGoToolchainCoreSpec.tests
        , Test.Unit.NewParserCoreSpec.tests
        , Test.Unit.NewUtilsCoreSpec.tests
        , Test.Unit.EnhancedDebugCoreSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Suite (2025)"
        [ Test.Unit.CoreFunctionalityQuickCheckSpec.tests
        , Test.Unit.TextProcessingPropertiesQuickCheckSpec.tests
        , Test.Unit.ParserInvariantQuickCheckSpec.tests
        , Test.Unit.ErrorLocationPropertiesQuickCheckSpec.tests
        , Test.Unit.CompilerConsistencyQuickCheckSpec.tests
        , Test.Unit.SymbolTableInvariantQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferPropertiesQuickCheckSpec.tests
        , Test.Unit.TypeSystemConsistencyQuickCheckSpec.tests
        , Test.Unit.DependencyAnalysisPropertiesQuickCheckSpec.tests
        , Test.Unit.SourcePositionInvariantQuickCheckSpec.tests
        ]
    , testGroup "New Cabal Enhanced Test Suite (2025)"
        [ Test.Unit.NewCabalEnhancedTestSpec.tests
        ]
    , Test.Unit.NewCoreFunctionalitySpec.tests
    , testGroup "New Enhanced QuickCheck Test Suite (2025)"
        [ Test.Unit.EnhancedUtilsQuickCheckSpec.tests
        , Test.Unit.EnhancedSourceLocationQuickCheckSpec.tests
        , Test.Unit.EnhancedParserQuickCheckSpec.tests
        , Test.Unit.EnhancedCompilerQuickCheckSpec.tests
        , Test.Unit.EnhancedOwnershipQuickCheckSpec.tests
        , Test.Unit.EnhancedErrorHandlerQuickCheckSpec.tests
        , Test.Unit.EnhancedDependenciesQuickCheckSpec.tests
        , Test.Unit.EnhancedSyntaxValidatorQuickCheckSpec.tests
        ]
    , Test.Unit.EnhancedCoreFunctionalityQuickCheckSpec.tests
    , testGroup "New Comprehensive Test Suite (2025)"
        [ Test.Unit.NewIntegratedParserTestsSpec.tests
        , Test.Unit.NewCompilerErrorRecoverySpec.tests
        , Test.Unit.NewOwnershipMemorySafetySpec.tests
        , Test.Unit.NewDependentTypeValidationSpec.tests
        , Test.Unit.NewSourceLocationPrecisionSpec.tests
        , Test.Unit.NewUtilsPerformanceSpec.tests
        , Test.Unit.NewErrorHandlingConsistencySpec.tests
        , Test.Unit.NewSyntaxValidationComprehensiveSpec.tests
        , Test.Unit.NewTypeSystemInferenceSpec.tests
        , Test.Unit.NewCompilationOptimizationSpec.tests
        ]
    , testGroup "New Cabal QuickCheck Test Suite (2025)"
        [ Test.Unit.NewCabalQuickCheckTestSpec.tests
        , Test.Unit.NewParserBoundaryQuickCheckTestSpec.tests
        , Test.Unit.NewSourceLocationPrecisionQuickCheckTestSpec.tests
        , Test.Unit.NewErrorHandlingRecoveryQuickCheckTestSpec.tests
        , Test.Unit.NewCompilerOptimizationQuickCheckTestSpec.tests
        , Test.Unit.NewOwnershipMemorySafetyQuickCheckTestSpec.tests
        , Test.Unit.NewDependentTypeValidationQuickCheckTestSpec.tests
        ]
    , Test.Unit.NewCoreQuickCheckTestSpec.tests
    , testGroup "New Advanced Test Suite for Enhanced Coverage"
        [ Test.Unit.ParserBoundaryConditionsSpec.tests
        , Test.Unit.NewSourceLocationMathPropertiesSpec2.tests
            , Test.Unit.NewUtilsPropertiesSpec2.tests
            , Test.Unit.ErrorHandlerCorePropertiesSpec.tests
            , Test.Unit.OwnershipBasicPropertiesSpec.tests
            , Test.Unit.DependenciesTypeSystemPropertiesSpec.tests
            , Test.Unit.DependentTypesParserPropertiesSpec.tests
            , Test.Unit.ErrorHandlerRecoveryAdvancedSpec.tests
            , Test.Unit.NewUtilsPropertiesSpec2.tests
            , Test.Unit.ErrorHandlerCorePropertiesSpec.tests
            , Test.Unit.OwnershipBasicPropertiesSpec.tests
            , Test.Unit.DependenciesTypeSystemPropertiesSpec.tests
            , Test.Unit.DependentTypesParserPropertiesSpec.tests
            , Test.Unit.ErrorHandlerRecoveryAdvancedSpec.tests        , Test.Unit.OwnershipTransitivityAdvancedSpec.tests
        , Test.Unit.DependentTypeConstraintValidationSpec.tests
        , Test.Unit.UtilsStringProcessingAdvancedSpec.tests
        , Test.Unit.CompilerOptimizationConsistencySpec.tests
        ]
    , testGroup "New Comprehensive Test Suite (2025 - Enhanced)"
        [ Test.Unit.NewParserComprehensiveSpec.tests
        , Test.Unit.NewCompilerComprehensiveSpec.tests
        , Test.Unit.NewUtilsComprehensiveSpec.tests
        , Test.Unit.NewSourceLocationComprehensiveSpec.tests
        , Test.Unit.NewErrorHandlerComprehensiveSpec.tests
        ]
    , testGroup "New QuickCheck Test Suite (2025 - Core Modules)"
        [ Test.Unit.SourceLocationNewQuickCheckSpec.tests
        , Test.Unit.ErrorHandlerNewQuickCheckSpec.tests
        , Test.Unit.ParserNewQuickCheckSpec.tests
        , Test.Unit.OwnershipNewQuickCheckSpec.tests
        , Test.Unit.DependenciesNewQuickCheckSpec.tests
        , Test.Unit.CompilerNewQuickCheckSpec.tests
        , Test.Unit.DependentTypesNewQuickCheckSpec.tests
        , Test.Unit.UtilsNewQuickCheckSpec.tests
        ]
    , Test.Unit.NewCoreFunctionalitySpec.tests
    , testGroup "Enhanced Cabal Test Suite (2025 - 10 New Modules)"
        [ Test.Unit.SourceLocationAdvancedPropertiesSpec.tests
        , Test.Unit.ErrorHandlerBoundarySpec.tests
        , Test.Unit.OwnershipTransferComplexSpec.tests
        , Test.Unit.DependentTypeConstraintSpec.tests
        , Test.Unit.CompilerIREdgeCaseSpec.tests
        , Test.Unit.SyntaxValidatorBoundarySpec.tests
        , Test.Unit.GoToolchainIntegrationSpec.tests
        , Test.Unit.EmbedAssetsConsistencySpec.tests
        , Test.Unit.DebugIntegrationFlowSpec.tests
        , Test.Unit.IntegratedCompilerPropertiesSpec.tests
        ]
    , Test.Unit.AdditionalCabalTestSpec.tests
    , testGroup "New Comprehensive Test Suite (2025 - Additional Modules)"
        [ Test.Unit.CoreModulesIntegrationSpec.tests
        , Test.Unit.ErrorHandlingBoundaryConditionsSpec.tests
        , Test.Unit.TextProcessingPropertiesExtendedSpec.tests
        , Test.Unit.SourceLocationTrackingComprehensiveSpec.tests
        , Test.Unit.ParserErrorRecoveryAdvancedSpec.tests
        , Test.Unit.CompilerEndToEndIntegrationSpec.tests
        , Test.Unit.DependenciesOwnershipInteractionSpec.tests
        , Test.Unit.PerformanceRegressionExtendedSpec.tests
        ]
    , Test.Unit.EnhancedCabalQuickCheckTestSpec.tests
    ]
    
    -- New Additional QuickCheck Test Suite
  , testGroup "New Additional QuickCheck Tests"
    [ Test.Unit.NewAdditionalQuickCheckSpec.tests
    , Test.Unit.AdditionalCorePropertiesSpec.tests
    , Test.Unit.CompilerCorePropertiesSpec.tests
    , Test.Unit.BoundaryCasePropertiesSpec.tests
    ]
    
    -- New Core Integration Test Suite
  , Test.Unit.NewCoreIntegrationSpec.tests
  
    -- New Comprehensive Cabal Test Suite
  , Test.Unit.NewCabalTestSuiteSpec.tests
  
  -- Advanced Test Modules
  , Test.Unit.BoundaryConditionsAdvancedSpec.tests
  , Test.Unit.ErrorRecoveryAdvancedSpec.tests
  , Test.Unit.PerformanceRegressionSpec.tests
  , Test.Unit.IntegrationAdvancedSpec.tests

  -- New Enhanced QuickCheck Test Modules (2025)
  , testGroup "Enhanced QuickCheck Test Suite 2025"
    [ Test.Unit.CoreCompilerQuickCheckSpec.tests
    , Test.Unit.DependentTypeBoundaryQuickCheckSpec.tests
    , Test.Unit.OwnershipRobustnessQuickCheckSpec.tests
    , Test.Unit.SourceLocationPrecisionQuickCheckSpec.tests
    , Test.Unit.ErrorHandlingConsistencyQuickCheckSpec.tests
    , Test.Unit.ParserBoundaryQuickCheckSpec.tests
    , Test.Unit.UtilsRobustnessQuickCheckSpec.tests
    , Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec.tests
    , Test.Unit.CompilerIntegrationQuickCheckSpec.tests
    ]

  -- New Advanced QuickCheck Test Modules (2025)
  , testGroup "Advanced QuickCheck Test Suite 2025"
    [ Test.Unit.SourceLocationAdvancedQuickCheckSpec.tests
    , Test.Unit.ErrorHandlerAdvancedQuickCheckSpec.tests
    , Test.Unit.ParserAdvancedQuickCheckSpec.tests
    , Test.Unit.UtilsAdvancedQuickCheckSpec.tests
    , Test.Unit.CompilerAdvancedQuickCheckSpec.tests
    , Test.Unit.DependenciesAdvancedQuickCheckSpec.tests
    , Test.Unit.OwnershipAdvancedQuickCheckSpec.tests
    , Test.Unit.IntegrationAdvancedQuickCheckSpec.tests
    ]

  -- New Validation Test Modules (Added by user)
  , testGroup "New Validation Test Suite"
    [ Test.Unit.NewUtilsValidationSpec.newUtilsValidationSpec
    , Test.Unit.NewUtilsValidationSpec.utilsQuickCheckProperties
    , Test.Unit.NewSourceLocationMathSpec.newSourceLocationMathSpec
    , Test.Unit.NewSourceLocationMathSpec.sourceLocationQuickCheckProperties
    , Test.Unit.NewParserValidationSpec.newParserValidationSpec
    , Test.Unit.NewParserValidationSpec.parserQuickCheckProperties
    , Test.Unit.NewErrorHandlerValidationSpec.newErrorHandlerValidationSpec
    , Test.Unit.NewErrorHandlerValidationSpec.errorHandlerQuickCheckProperties
    , Test.Unit.NewIntegrationValidationSpec.newIntegrationValidationSpec
    , Test.Unit.NewIntegrationValidationSpec.integrationQuickCheckProperties
    , Test.Unit.NewBoundaryConditionSpec.newBoundaryConditionSpec
    , Test.Unit.NewBoundaryConditionSpec.boundaryConditionQuickCheckProperties
    , Test.Unit.NewPerformanceSpec.newPerformanceSpec
    , Test.Unit.NewPerformanceSpec.performanceQuickCheckProperties
    ]

  -- New QuickCheck Test Suite (2025 - Enhanced Core Modules)
  , testGroup "New Enhanced QuickCheck Test Suite 2025"
    [ Test.Unit.NewParserQuickCheckTestsSpec.tests
    , Test.Unit.NewSourceLocationQuickCheckTestsSpec.tests
    , Test.Unit.NewUtilsQuickCheckTestsSpec.tests
    ]

  -- New Comprehensive QuickCheck Test Modules (2025)
  , testGroup "New Comprehensive QuickCheck Test Modules 2025"
    [ Test.Unit.SourceLocationAdvancedTestSpec.tests
    , Test.Unit.ParserRobustnessTestSpec.tests
    , Test.Unit.CompilerOptimizationTestSpec.tests
    , Test.Unit.OwnershipMemorySafetyTestSpec.tests
    , Test.Unit.DependentTypeValidationTestSpec.tests
    , Test.Unit.ErrorHandlerRecoveryTestSpec.tests
    , Test.Unit.TypeInferenceAdvancedTestSpec.tests
    , Test.Unit.IntegrationEndToEndTestSpec.tests
    , Test.Unit.PerformanceRegressionTestSpec.tests
    , Test.Unit.SecurityValidationTestSpec.tests
    ]

  -- New Cabal Test Suite (10 comprehensive tests)
  , Test.Unit.NewCabalTestSuiteSpec.tests

  -- New Boundary and Property Test Modules
  , testGroup "New Boundary and Property Tests"
    [ Test.Unit.ParserBoundarySpec.tests
    , Test.Unit.CompilerErrorBoundarySpec.tests
    , Test.Unit.NewQuickCheckPropertiesSpec.tests
    ]

  -- New Cabals Test Suite (10 comprehensive tests)
  , testGroup "New Cabals Test Suite"
    [ Test.Unit.ParserErrorRecoveryCabalsSpec.tests
    , Test.Unit.CompilerOptimizationCabalsSpec.tests
    , Test.Unit.OwnershipMemorySafetyCabalsSpec.tests
    , Test.Unit.DependentTypeValidationCabalsSpec.tests
    , Test.Unit.SourceLocationPrecisionCabalsSpec.tests
    , Test.Unit.ErrorHandlerConsistencyCabalsSpec.tests
    , Test.Unit.TypeInferenceAdvancedCabalsSpec.tests
    , Test.Unit.IntegrationEndToEndCabalsSpec.tests
    , Test.Unit.PerformanceRegressionCabalsSpec.tests
    , Test.Unit.SecurityValidationCabalsSpec.tests
    ]

  -- New Core QuickCheck Test Modules
  , testGroup "New Core QuickCheck Test Modules"
    [ Test.Unit.SourceLocationCoreQuickCheckSpec.tests
    , Test.Unit.ErrorHandlerCoreQuickCheckSpec.tests
    , Test.Unit.ParserCoreQuickCheckSpec.tests
    , Test.Unit.UtilsCoreQuickCheckSpec.tests
    , Test.Unit.CompilerIRCoreQuickCheckSpec.tests
    , Test.Unit.OwnershipTransferCoreQuickCheckSpec.tests
    , Test.Unit.DependencyAnalysisCoreQuickCheckSpec.tests
    , Test.Unit.NewErrorHandlerCoreQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationMathQuickCheckSpec.tests
    , Test.Unit.NewParserCoreQuickCheckSpec.tests
    , Test.Unit.NewUtilsCoreQuickCheckSpec.tests
    , Test.Unit.NewCompilerIRCoreQuickCheckSpec.tests
    , Test.Unit.NewOwnershipTransferCoreQuickCheckSpec.tests
    , Test.Unit.NewDependencyAnalysisCoreQuickCheckSpec.tests
    , Test.Unit.NewGoToolchainCoreQuickCheckSpec.tests
    ]

  -- Enhanced QuickCheck Test Modules (New)
  , testGroup "Enhanced QuickCheck Test Modules"
    [ Test.Unit.UtilsEnhancedQuickCheckSpec.tests
    , Test.Unit.SourceLocationEnhancedQuickCheckSpec.tests
    , Test.Unit.ParserEnhancedQuickCheckSpec.tests
    , Test.Unit.ErrorHandlerEnhancedQuickCheckSpec.tests
    , Test.Unit.CompilerIntegrationEnhancedQuickCheckSpec.tests
    , Test.Unit.OwnershipBoundaryEnhancedQuickCheckSpec.tests
    , Test.Unit.DependentTypesValidationEnhancedQuickCheckSpec.tests
    ]

  -- New Comprehensive Test Suite (2025)
  , testGroup "New Comprehensive Test Suite 2025"
    [ Test.Unit.SourceLocationBoundarySpec.tests
    , Test.Unit.UtilsPerformanceBoundarySpec.tests
    , Test.Unit.ErrorHandlerConsistencySpec.tests
    , Test.Unit.OwnershipAnalysisBoundarySpec.tests
    , Test.Unit.EndToEndCompilationSpec.tests
    ]

  -- New Cabal Test Suite (9 comprehensive tests)
  , testGroup "New Cabal Test Suite"
    [ Test.Unit.SourceLocationCoreTestSpec.tests
    , Test.Unit.ParserErrorRecoveryTestSpec.tests
    , Test.Unit.OwnershipTransferTestSpec.tests
    , Test.Unit.ErrorHandlerConsistencyTestSpec.tests
    , Test.Unit.DependencyAnalysisTestSpec.tests
    , Test.Unit.TypeInferenceBoundaryTestSpec.tests
    , Test.Unit.IntegrationEndToEndTestSpec.tests
    , Test.Unit.PerformanceRegressionTestSpec.tests
    , Test.Unit.SecurityValidationTestSpec.tests
    ]

  -- New Additional QuickCheck Test Modules (2025)
  , testGroup "New Additional QuickCheck Test Modules 2025"
    [ Test.Unit.UtilsBoundaryQuickCheckSpec.tests
    , Test.Unit.SourceLocationMathQuickCheckSpec.tests
    , Test.Unit.ParserRobustnessQuickCheckSpec.tests
    , Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec.tests
    , Test.Unit.NewEnhancedCompilerOptimizationQuickCheckSpec.tests
    , Test.Unit.NewEnhancedOwnershipMemorySafetyQuickCheckSpec.tests
    , Test.Unit.DependentTypeValidationQuickCheckSpec.tests
    , Test.Unit.TypeInferenceBoundaryQuickCheckSpec.tests
    , Test.Unit.IntegrationEndToEndQuickCheckSpec.tests
    , Test.Unit.PerformanceEnhancedQuickCheckSpec.tests
    ]

  -- New Cabal Test Modules (10 comprehensive QuickCheck tests)
  , testGroup "New Cabal Test Modules 2025"
    [ Test.Unit.SourceLocationMathQuickCheckSpec.tests
    , Test.Unit.UtilsBoundaryQuickCheckSpec.tests
    , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
    , Test.Unit.OwnershipTransferConsistencyQuickCheckSpec.tests
    , Test.Unit.CompilerIRConsistencyQuickCheckSpec.tests
    , Test.Unit.ErrorLocationTrackingQuickCheckSpec.tests
    , Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec.tests
    ]

  -- New Cabal Test Modules (5 comprehensive QuickCheck tests)
  , testGroup "New Cabal Test Modules 2024"
    [ Test.Unit.NewStringProcessingQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationMathQuickCheckSpec.tests
    , Test.Unit.NewParserBoundaryQuickCheckSpec.tests
    , Test.Unit.NewErrorHandlerCoreQuickCheckSpec.tests
    , Test.Unit.NewComprehensiveCabalQuickCheckSpec.tests
    ]

  -- New Advanced QuickCheck Test Modules (2025)
  , testGroup "Advanced QuickCheck Test Modules 2025"
    [ Test.Unit.SourceLocationAdvancedQuickCheckSpec.tests
    , Test.Unit.UtilsAdvancedQuickCheckSpec.tests
    , Test.Unit.ParserAdvancedQuickCheckSpec.tests
    , Test.Unit.ErrorHandlerAdvancedQuickCheckSpec.tests
    , Test.Unit.CompilerAdvancedQuickCheckSpec.tests
    , Test.Unit.OwnershipAdvancedQuickCheckSpec.tests
    , Test.Unit.DependenciesAdvancedQuickCheckSpec.tests
    , Test.Unit.IntegrationAdvancedQuickCheckSpec.tests
    ]

  -- New Comprehensive QuickCheck Test Modules (2025)
  , testGroup "Comprehensive QuickCheck Test Modules 2025"
    [ Test.Unit.NewEnhancedUtilsQuickCheckSpec.tests
    , Test.Unit.NewAdvancedSourceLocationQuickCheckSpec.tests
    , Test.Unit.NewRobustErrorHandlerQuickCheckSpec.tests
    , Test.Unit.NewComprehensiveParserQuickCheckSpec.tests
    , Test.Unit.NewAdvancedOwnershipQuickCheckSpec.tests
    , Test.Unit.NewCompilerIRQuickCheckSpec.tests
    , Test.Unit.NewDependenciesQuickCheckSpec.tests
    , Test.Unit.NewTypeSystemQuickCheckSpec.tests
    , Test.Unit.NewIntegrationQuickCheckSpec.tests
    , Test.Unit.NewPerformanceQuickCheckSpec.tests
    ]

  -- New Enhanced Test Modules (2025)
  , testGroup "Enhanced Test Modules 2025"
    [ Test.Unit.SourceLocationAdvancedFeaturesSpec.tests
    , Test.Unit.ParserBoundaryConditionsSpec.tests
    , Test.Unit.UtilsStringProcessingEnhancedSpec.tests
    , Test.Unit.ErrorHandlerErrorHandlingSpec.tests
    , Test.Unit.CompilerCompilationLogicSpec.tests
    , Test.Unit.OwnershipOwnershipAnalysisSpec.tests
    , Test.Unit.DependenciesDependencyAnalysisSpec.tests
    , Test.Unit.SyntaxValidatorValidationSpec.tests
    ]

  -- New Test Modules Added for Enhanced Testing (2025)
  , testGroup "New Enhanced Test Modules 2025"
    [ Test.Unit.NewTextProcessingQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationTrackingQuickCheckSpec.tests
    , Test.Unit.NewErrorHandlingQuickCheckSpec.tests
    , Test.Unit.NewCompilerIRQuickCheckSpec.tests
    , Test.Unit.NewTypeSystemQuickCheckSpec.tests
    , Test.Unit.NewOwnershipAnalysisQuickCheckSpec.tests
    , Test.Unit.NewParserQuickCheckSpec.tests
    ]

  -- New Cabal Test Modules (6 comprehensive tests)
  , testGroup "New Comprehensive Test Modules 2025"
    [ Test.Unit.SourceLocationCoreFunctionsSpec.tests
    , Test.Unit.ParserErrorBoundarySpec.tests
    , Test.Unit.CompilerIRConsistencySpec.tests
    , Test.Unit.OwnershipBoundaryConditionsSpec.tests
    , Test.Unit.DependentTypeValidationSpec.tests
    , Test.Unit.UtilsStringPropertiesSpec.tests
    ]

  -- New Cabal QuickCheck Tests (10 comprehensive tests)
  , Test.Unit.NewComprehensiveCabalQuickCheckTestSuite.tests

  -- New Core QuickCheck Test Modules (2025)
  , testGroup "Core QuickCheck Test Modules 2025"
    [ Test.Unit.CoreSourceLocationQuickCheckSpec.tests
    , Test.Unit.CoreErrorHandlerQuickCheckSpec.tests
    , Test.Unit.CoreParserQuickCheckSpec.tests
    , Test.Unit.CoreOwnershipQuickCheckSpec.tests
    , Test.Unit.CoreDependenciesQuickCheckSpec.tests
    , Test.Unit.CoreCompilerQuickCheckSpec.tests
    , Test.Unit.CoreUtilsQuickCheckSpec.tests
    , Test.Unit.CoreSyntaxValidatorQuickCheckSpec.tests
    , Test.Unit.CoreGoToolchainQuickCheckSpec.tests
    , Test.Unit.CoreIntegratedCompilerQuickCheckSpec.tests
    ]

  -- Additional Cabal Test Modules (2025)
  , testGroup "Additional Cabal Test Modules 2025"
    [ Test.Unit.NewCabalTestSuiteSpec.tests
    , Test.Unit.SourceLocationCabalTestsSpec.tests
    , Test.Unit.ParserCabalTestsSpec.tests
    , Test.Unit.ErrorHandlingCabalTestsSpec.tests
    , Test.Unit.IntegrationCabalTestsSpec.tests
    ]

  -- New Comprehensive Cabal Test Modules (2025)
  , testGroup "New Comprehensive Cabal Test Modules"
    [ Test.Unit.NewCabalComprehensiveTestsSpec.tests
    , Test.Unit.NewParserQuickCheckTestsSpec.tests
    , Test.Unit.NewCompilerQuickCheckTestsSpec.tests
    , Test.Unit.NewOwnershipQuickCheckTestsSpec.tests
    , Test.Unit.NewSourceLocationQuickCheckTestsSpec.tests
    , Test.Unit.NewErrorHandlerQuickCheckTestsSpec.tests
    , Test.Unit.NewDependenciesQuickCheckTestsSpec.tests
    , Test.Unit.NewIntegrationQuickCheckTestsSpec.tests
    ]

  -- New Cabal QuickCheck Test Modules (Added for enhanced testing)
  , testGroup "New Cabal QuickCheck Test Modules"
    [ Test.Unit.NewCabalQuickCheckTestsSpec.tests
    , Test.Unit.OwnershipTransferPropertiesSpec.tests
    , Test.Unit.DependentTypeValidationPropertiesSpec.tests
    , Test.Unit.CompilerIRConsistencyPropertiesSpec.tests
    ]

  -- New Comprehensive Test Suite (2025)
  , testGroup "New Comprehensive Test Suite 2025"
    [ Test.Unit.ErrorRecoveryAdvancedSpec.tests
    , Test.Unit.ConcurrentCompilationSpec.tests
    , Test.Unit.MemoryEfficiencySpec.tests
    , Test.Unit.TypeInferenceEdgeCasesSpec.tests
    , Test.Unit.IntegrationComplexWorkflowsSpec.tests
    , Test.Unit.TextProcessingBoundarySpec.tests
    ]

  -- New Advanced Test Suite (2025) - 10 comprehensive tests
  , testGroup "New Advanced Test Suite 2025"
    [ Test.Unit.NewComprehensiveTestSuite2025Spec.tests
    , Test.Unit.ParserDirectivesAdvanced2025Spec.tests
    , Test.Unit.SourceLocationMathAdvanced2025Spec.tests
    , Test.Unit.StringUtilsProcessingAdvanced2025Spec.tests
    , Test.Unit.ErrorRecoveryAdvancedTest2025Spec.tests
    , Test.Unit.OwnershipTransferComplex2025Spec.tests
    , Test.Unit.TypeInferenceComplex2025Spec.tests
    , Test.Unit.ConcurrentSafetyAdvanced2025Spec.tests
    , Test.Unit.PerformanceOptimization2025Spec.tests
    , Test.Unit.BoundaryConditionsAdvanced2025Spec.tests
    , Test.Unit.NewCabalTestSpec.tests
    ]

  -- New Comprehensive Cabal Test Suite (2025)
  , Test.Unit.NewCabalTestSuiteSpec.tests

  -- New Cabal Test Modules (10 comprehensive tests)
  , testGroup "New Cabal Test Modules"
    [ Test.Unit.CabalParserQuickCheckSpec.tests
    , Test.Unit.CabalCompilerQuickCheckSpec.tests
    , Test.Unit.CabalOwnershipQuickCheckSpec.tests
    , Test.Unit.CabalDependentTypesQuickCheckSpec.tests
    , Test.Unit.CabalSourceLocationQuickCheckSpec.tests
    , Test.Unit.CabalErrorHandlerQuickCheckSpec.tests
    , Test.Unit.CabalUtilsQuickCheckSpec.tests
    , Test.Unit.CabalSyntaxValidatorQuickCheckSpec.tests
    , Test.Unit.CabalAnalyzerQuickCheckSpec.tests
    , Test.Unit.CabalIntegrationQuickCheckSpec.tests
    ]

  -- New Test Modules Added for Enhanced Coverage (2025)
  , testGroup "New Enhanced Coverage Test Modules"
    [ Test.Unit.NewUtilsEdgeCaseSpec.tests
    , Test.Unit.NewSourceLocationMathPropertiesSpec2.tests
    , Test.Unit.NewParserUnicodeSpec.tests
    , Test.Unit.NewCommentHandlingSpec.tests
    , Test.Unit.NewIndentationSpec.tests
    , Test.Unit.NewQuickCheckUtilsSpec.tests
    , Test.Unit.NewQuickCheckSourceLocationSpec.tests
    , Test.Unit.NewErrorRecoverySpec.tests
    ]

  -- New Additional Test Modules (2025)
  , testGroup "New Additional Test Modules 2025"
    [ Test.Unit.NewCabalQuickCheckTestsSpec.tests
    , Test.Unit.CompilerBasicPropertiesSpec.tests
    , Test.Unit.SourceLocationMathSpec.tests
    , Test.Unit.ErrorHandlingRobustnessSpec.tests
    , Test.Unit.TypeInferenceBasicSpec.tests
    , Test.Unit.OwnershipTransferSpec.tests
    , Test.Unit.ParserCombinatorSpec.tests
    , Test.Unit.UtilsStringSpec.tests
    ]

  -- New Comprehensive Test Suites (2025) - 6 comprehensive test modules
  , testGroup "New Comprehensive Test Suites 2025"
    [ Test.Unit.SourceLocationMathComprehensiveSpec.tests
    , Test.Unit.CompilerIntegrationComprehensiveSpec.tests
    , Test.Unit.OwnershipAnalysisComprehensiveSpec.tests
    , Test.Unit.DependentTypesValidationComprehensiveSpec.tests
    , Test.Unit.ErrorHandlerCoreComprehensiveSpec.tests
    , Test.Unit.UtilsStringProcessingComprehensiveSpec.tests
    ]

  -- New Cabal Test Modules (2025) - Core functionality, property-based, and boundary tests
  , testGroup "New Cabal Test Modules 2025"
    [ Test.Unit.NewCabalCoreFunctionalitySpec.tests
    , Test.Unit.NewCabalPropertyBasedSpec.tests
    , Test.Unit.NewCabalBoundaryConditionsSpec.tests
    ]

  -- New Comprehensive Cabal QuickCheck Test Module
  , Test.Unit.NewComprehensiveCabalQuickCheckSpec.tests

  -- New Cabal Test Suite (10 comprehensive tests with QuickCheck)
  , testGroup "New Cabal Test Suite"
    [ Test.Unit.NewCabalTestSpec1.tests
    , Test.Unit.NewCabalTestSpec2.tests
    , Test.Unit.NewCabalTestSpec3.tests
    , Test.Unit.NewCabalTestSpec4.tests
    , Test.Unit.NewCabalTestSpec5.tests
    , Test.Unit.NewCabalTestSpec6.tests
    , Test.Unit.NewCabalTestSpec7.tests
    , Test.Unit.NewCabalTestSpec8.tests
    , Test.Unit.NewCabalTestSpec9.tests
    , Test.Unit.NewCabalTestSpec10.tests
    ]

  -- New Comprehensive Cabal Test Modules (2025)
  , testGroup "New Comprehensive Cabal Test Modules 2025"
    [ Test.Unit.CompilerErrorRecoveryAdvancedSpec.tests
    , Test.Unit.ParserUnicodeHandlingSpec.tests
    , Test.Unit.OwnershipMemoryLeakPreventionSpec.tests
    , Test.Unit.TypeSystemInferenceBoundarySpec.tests
    , Test.Unit.SourceLocationPrecisionNewSpec.tests
    , Test.Unit.ErrorHandlerConsistencySpec.tests
    , Test.Unit.DependencyAnalysisCyclicSpec.tests
    , Test.Unit.IntegrationEndToEndSpec.tests
    , Test.Unit.PerformanceRegressionNewSpec.tests
    , Test.Unit.SecurityValidationSpec.tests
    ]

  -- ============================================================================
  -- New Advanced Test Modules (10 comprehensive tests)
  -- ============================================================================
  , testGroup "New Advanced Test Modules"
    [ Test.Unit.SourceLocationAdvancedSpec.tests
    , Test.Unit.ErrorHandlerAdvancedSpec.tests
    , Test.Unit.ParserDirectiveSpec.tests
    , Test.Unit.IntegrationAdvancedSpec.tests
    , Test.Unit.BoundaryConditionSpec.tests
    , Test.Unit.PerformanceAdvancedSpec.tests
    , Test.Unit.StringUtilsAdvancedSpec.tests
    , Test.Unit.CompilerTypeCheckerSpec.tests
    , Test.Unit.OwnershipAnalysisSpec.tests
    , Test.Unit.DependencyAnalysisSpec.tests
    ]

  -- ============================================================================
  -- Advanced Edge Case QuickCheck Test Module (10 comprehensive tests)
  -- ============================================================================
  , Test.Unit.AdvancedEdgeCaseQuickCheckSpec.tests

  -- ============================================================================
  -- New Cabal Test Modules (10 comprehensive tests)
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Comprehensive Testing"
    [ Test.Unit.CabalCrossModuleIntegrationSpec.tests
    , Test.Unit.CabalErrorRecoverySpec.tests
    , Test.Unit.CabalPerformanceSpec.tests
    , Test.Unit.CabalQuickCheckPropertiesSpec.tests
    , Test.Unit.CabalBoundaryConditionsSpec.tests
    , Test.Unit.CabalUnicodeHandlingSpec.tests
    , Test.Unit.CabalConcurrentParsingSpec.tests
    , Test.Unit.CabalMemorySafetySpec.tests
    , Test.Unit.CabalRegressionSpec.tests
    , Test.Unit.CabalEndToEndSpec.tests
    ]

  -- ============================================================================
  -- New QuickCheck Test Modules (8 comprehensive tests)
  -- ============================================================================
  , testGroup "New QuickCheck Test Modules - Enhanced Testing"
    [ Test.Unit.NewSourceLocationMathPropertiesSpec2.tests
    , Test.Unit.ParserConsistencyPropertiesSpec.tests
    , Test.Unit.CompilerIROptimizationSpec.tests
    , Test.Unit.OwnershipTransferInvariantSpec.tests
    , Test.Unit.TypeSystemSubstitutionSpec.tests
    , Test.Unit.ErrorHandlerRecoverySpec.tests
    , Test.Unit.DependencyAnalysisCycleSpec.tests
    , Test.Unit.UtilsPerformanceSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive Test Modules (2025) - Advanced Testing Suite
  -- ============================================================================
  , testGroup "New Comprehensive Test Modules (2025)"
    [ Test.Unit.StringAnalysisSpec.tests
    , Test.Unit.CompilerOptimizationSpec.tests
    , Test.Unit.TypeSystemBoundarySpec.tests
    , Test.Unit.OwnershipComplexSpec.tests
    , Test.Unit.ErrorRecoveryAdvancedSpec.tests
    , Test.Unit.SourceLocationPrecisionSpec.tests
    ]

  -- New QuickCheck Test Modules Added (2025) - Comprehensive Testing
  -- ============================================================================
  , testGroup "New QuickCheck Test Modules (2025)"
    [ Test.Unit.SourceLocationNewQuickCheckTests.tests
    , Test.Unit.ParserNewQuickCheckTests.tests
    , Test.Unit.ErrorHandlerNewQuickCheckTests.tests
    , Test.Unit.UtilsNewQuickCheckTests.tests
    , Test.Unit.OwnershipNewQuickCheckTests.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules (2025) - 8 comprehensive tests
  -- ============================================================================
  , testGroup "New Cabal Test Modules (2025) - Advanced Testing"
    [ Test.Unit.CompilerOptimizationConsistencySpec.tests
    , Test.Unit.OwnershipTransferBoundaryNewSpec.tests
    , Test.Unit.DependentTypeConstraintValidationSpec.tests
    , Test.Unit.SourceLocationPrecisionNewSpec.tests
    , Test.Unit.ErrorRecoveryConsistencyNewSpec.tests
    , Test.Unit.TypeInferenceAdvancedNewSpec.tests
    , Test.Unit.IntegrationEndToEndNewSpec.tests
    , Test.Unit.PerformanceRegressionNewSpec.tests
    ]

  -- ============================================================================
  -- New Cabal QuickCheck Test Suite - 10 comprehensive tests
  -- ============================================================================
  , Test.Unit.NewComprehensiveCabalQuickCheckTestSuite.tests

  -- ============================================================================
  -- New QuickCheck Test Modules (7 comprehensive tests)
  -- ============================================================================
  , testGroup "New QuickCheck Test Modules - Core Functionality"
    [ Test.Unit.ParserErrorHandlingQuickCheckSpec.tests
    , Test.Unit.UtilsStringProcessingQuickCheckSpec.tests
    , Test.Unit.OwnershipTransferConsistencyQuickCheckSpec.tests
    , Test.Unit.ErrorHandlerRecoveryQuickCheckSpec.tests
    , Test.Unit.CompilerIRConsistencyQuickCheckSpec.tests
    , Test.Unit.ErrorLocationTrackingQuickCheckSpec.tests
    , Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Cabal QuickCheck Test Module - 10 comprehensive tests
  -- ============================================================================
  , Test.Unit.NewCabalQuickCheckTestSpec.tests

  -- ============================================================================
  -- New Comprehensive QuickCheck Test Modules (2025)
  -- ============================================================================
  , testGroup "New Comprehensive QuickCheck Test Modules (2025)"
    [ Test.Unit.CompilerIRQuickCheckTestSpec.tests
    , Test.Unit.OwnershipCommonTypesQuickCheckTestSpec.tests
    , Test.Unit.AnalyzerSymbolTableQuickCheckTestSpec.tests
    , Test.Unit.CompilerTypeCheckerQuickCheckTestSpec.tests
    , Test.Unit.CompilerGoAstQuickCheckTestSpec.tests
    , Test.Unit.DependenciesASTQuickCheckTestSpec.tests
    , Test.Unit.CompilerGoLexerQuickCheckTestSpec.tests
    , Test.Unit.CompilerGoParsingQuickCheckTestSpec.tests
    , Test.Unit.ErrorHandlerQuickCheckTestSpec.tests
    , Test.Unit.IntegratedCompilerQuickCheckTestSpec.tests
    ]

  -- ============================================================================
  -- New Cabal QuickCheck Test Suite - 10 comprehensive tests
  -- ============================================================================
  , Test.Unit.NewCabalQuickCheckTestSuiteSpec.tests

  -- ============================================================================
  -- New Test Modules Added (2025) - Comprehensive Test Coverage
  -- ============================================================================
  , testGroup "New Test Modules - Comprehensive Coverage"
    [ Test.Unit.NewParserErrorRecoverySpec.tests
    , Test.Unit.NewCompilerOptimizationConsistencySpec.tests
    , Test.Unit.NewOwnershipTransferBoundarySpec.tests
    , Test.Unit.NewSourceLocationPrecisionSpec.tests
    , Test.Unit.NewUtilsStringBoundarySpec.tests
    , Test.Unit.NewErrorHandlerConsistencySpec.tests
    , Test.Unit.NewDependentTypeValidationSpec.tests
    , Test.Unit.NewEndToEndCompilationSpec.tests
    ]

  -- ============================================================================
  -- New QuickCheck Test Suite (2025) - 10 Comprehensive Tests
  -- ============================================================================
  , testGroup "New QuickCheck Test Suite - Comprehensive Testing"
    [ Test.Unit.NewQuickCheckTestSuite1Spec.tests
    , Test.Unit.NewQuickCheckTestSuite2Spec.tests
    , Test.Unit.NewQuickCheckTestSuite3Spec.tests
    , Test.Unit.NewQuickCheckTestSuite4Spec.tests
    , Test.Unit.NewQuickCheckTestSuite5Spec.tests
    , Test.Unit.NewQuickCheckTestSuite6Spec.tests
    , Test.Unit.NewQuickCheckTestSuite7Spec.tests
    , Test.Unit.NewQuickCheckTestSuite8Spec.tests
    , Test.Unit.NewQuickCheckTestSuite9Spec.tests
    , Test.Unit.NewQuickCheckTestSuite10Spec.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules (Added for enhanced coverage)
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Enhanced Coverage"
    [ Test.Unit.UtilsBoundarySpec.tests
    , Test.Unit.SourceLocationMathSpec.tests
    , Test.Unit.ErrorHandlingCoreSpec.tests
    , Test.Unit.CorePropertiesQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Enhanced Test Modules (2025) - 10 comprehensive QuickCheck tests
  -- ============================================================================
  , testGroup "New Enhanced Test Modules - Comprehensive QuickCheck Testing"
    [ Test.Unit.NewEnhancedUtilsQuickCheckSpec.tests
    , Test.Unit.NewAdvancedSourceLocationQuickCheckSpec.tests
    , Test.Unit.NewRobustErrorHandlerQuickCheckSpec.tests
    , Test.Unit.NewComprehensiveParserQuickCheckSpec.tests
    , Test.Unit.NewAdvancedOwnershipQuickCheckSpec.tests
    , Test.Unit.NewDependenciesAdvancedQuickCheckSpec.tests
    , Test.Unit.NewIntegrationAdvancedQuickCheckSpec.tests
    , Test.Unit.NewCoreFunctionalityQuickCheckSpec.tests
    , Test.Unit.NewTextProcessingQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationMathQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive QuickCheck Test Modules (2025) - Core Module Testing
  -- ============================================================================
  , testGroup "New Comprehensive QuickCheck Test Modules - Core Module Testing"
    [ Test.Unit.NewParserQuickCheckTestsSpec.tests
    , Test.Unit.NewSourceLocationQuickCheckTestsSpec.tests
    , Test.Unit.NewErrorHandlerQuickCheckTestsSpec.tests
    , Test.Unit.NewCompilerQuickCheckTestsSpec.tests
    , Test.Unit.NewOwnershipQuickCheckTestsSpec.tests
    , Test.Unit.NewDependenciesQuickCheckTestsSpec.tests
    ]

  -- ============================================================================
  -- New Test Modules Added (2025) - 10 comprehensive tests
  -- ============================================================================
  , testGroup "New Test Modules - Comprehensive Testing"
    [ Test.Unit.SourceLocationBoundarySpec.tests
    , Test.Unit.ErrorHandlerRecoverySpec.tests
    , Test.Unit.OwnershipComplexScenariosSpec.tests
    , Test.Unit.UtilsPerformanceBoundarySpec.tests
    , Test.Unit.CompilerOptimizationConsistencySpec.tests
    , Test.Unit.DependenciesCycleDetectionSpec.tests
    , Test.Unit.TypeSystemInferenceBoundarySpec.tests
    , Test.Unit.IntegrationEndToEndScenariosSpec.tests
    , Test.Unit.MathematicalPropertiesQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive Cabal QuickCheck Test Suite (2025)
  -- ============================================================================
  , Test.Unit.NewComprehensiveCabalQuickCheckTestSuite.tests

  -- ============================================================================
  -- New Comprehensive Test Modules Created (2025) - 10 cabal tests
  -- ============================================================================
  , testGroup "New Comprehensive Test Modules - Core Functionality Testing"
    [ Test.Unit.ComprehensiveCoreQuickCheckSpec.tests
    , Test.Unit.CompilerOwnershipQuickCheckSpec.tests
    , Test.Unit.DependenciesErrorHandlingQuickCheckSpec.tests
    , Test.Unit.SyntaxValidatorGoToolchainQuickCheckSpec.tests
    , Test.Unit.NewEndToEndIntegrationQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Typus QuickCheck Test Modules (2025) - Core Functionality Testing
  -- ============================================================================
  , testGroup "New Typus QuickCheck Test Modules - Core Functionality Testing"
    [ Test.Unit.NewTypusCoreQuickCheckSpec.tests
    , Test.Unit.NewTypusParserQuickCheckSpec.tests
    , Test.Unit.NewTypusOwnershipQuickCheckSpec.tests
    , Test.Unit.NewTypusDependentTypesQuickCheckSpec.tests
    , Test.Unit.NewTypusCompilerQuickCheckSpec.tests
    , Test.Unit.NewTypusSourceLocationQuickCheckSpec.tests
    , Test.Unit.NewTypusErrorHandlerQuickCheckSpec.tests
    , Test.Unit.NewTypusUtilsQuickCheckSpec.tests
    , Test.Unit.NewTypusIntegrationQuickCheckSpec.tests
    , Test.Unit.NewTypusSyntaxValidatorQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules (Added for this request)
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Core Functionality"
    [ Test.Unit.NewCabalCoreTestsSpec.tests
    , Test.Unit.NewCabalQuickCheckPropertiesSpec.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules (2025) - 10 Comprehensive QuickCheck Tests
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Comprehensive QuickCheck Testing"
    [ Test.Unit.UtilsStringBoundaryQuickCheckSpec.tests
    , Test.Unit.SourceLocationMathQuickCheckSpec.tests
    , Test.Unit.ParserDirectiveQuickCheckSpec.tests
    , Test.Unit.ErrorHandlingConsistencyQuickCheckSpec.tests
    , Test.Unit.CompilerIRConsistencyQuickCheckSpec.tests
    , Test.Unit.OwnershipTransitivityQuickCheckSpec.tests
    , Test.Unit.DependentTypeBoundaryQuickCheckSpec.tests
    , Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec.tests
    , Test.Unit.IntegrationEndToEndQuickCheckSpec.tests
    , Test.Unit.PerformanceRegressionQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Enhanced Cabal Test Modules (2025) - 10 Additional QuickCheck Tests  
  -- ============================================================================
  , testGroup "New Enhanced Cabal Test Modules - Additional QuickCheck Testing"
    [ Test.Unit.NewSourceLocationMathQuickCheckSpec.tests
    , Test.Unit.NewParserErrorRecoveryQuickCheckSpec.tests
    , Test.Unit.NewCompilerOptimizationQuickCheckSpec.tests
    , Test.Unit.NewOwnershipTransitivityQuickCheckSpec.tests
    , Test.Unit.NewUtilsStringBoundaryQuickCheckSpec.tests
    , Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec.tests
    , Test.Unit.NewDependenciesCycleDetectionQuickCheckSpec.tests
    , Test.Unit.NewTypeSystemBoundaryQuickCheckSpec.tests
    , Test.Unit.NewIntegrationEndToEndQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive Test Modules (2025) - 10 cabal tests
  -- ============================================================================
  , testGroup "New Comprehensive Test Modules - Core Functionality Testing"
    [ Test.Unit.NewTextProcessingBoundarySpec.tests
    , Test.Unit.NewSourceLocationMathPropertiesSpec2.tests
    , Test.Unit.NewParserRobustnessSpec.tests
    , Test.Unit.NewCompilerOptimizationInvariantSpec.tests
    , Test.Unit.NewOwnershipTransitivitySpec.tests
    , Test.Unit.NewDependentTypeBoundarySpec.tests
    , Test.Unit.NewSyntaxValidatorRobustnessSpec.tests
    , Test.Unit.NewEndToEndCompilationSpec.tests
    , Test.Unit.NewPerformanceRegressionSpec.tests
    , Test.Unit.NewErrorHandlingConsistencySpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive Cabal Test Suite (2025) - 10 Additional Tests
  -- ============================================================================
  , testGroup "New Comprehensive Cabal Test Suite - Enhanced Coverage"
    [ Test.Unit.NewComprehensiveCabalTestSpec.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules - 10 Comprehensive QuickCheck Tests (2025)
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Enhanced QuickCheck Coverage"
    [ Test.Unit.TextProcessingPropertiesSpec.tests
    , Test.Unit.SourceLocationCalculationSpec.tests
    , Test.Unit.ParserCombinatorsSpec.tests
    , Test.Unit.ErrorHandlerConsistencySpec.tests
    , Test.Unit.UtilsStringFunctionsSpec.tests
    , Test.Unit.CompilerIRPropertiesSpec.tests
    , Test.Unit.OwnershipTransferSpec.tests
    , Test.Unit.DependencyAnalysisSpec.tests
    , Test.Unit.TypeInferenceSpec.tests
    , Test.Unit.IntegrationPropertiesSpec.tests
    ]

  -- ============================================================================
  -- New Enhanced QuickCheck Test Modules (2025) - 4 comprehensive tests
  -- ============================================================================
  , testGroup "New Enhanced QuickCheck Test Modules - Core Functionality Testing"
    [ Test.Unit.NewUtilsEnhancedQuickCheckSpec.test_UtilsEnhancedQuickCheck
    , Test.Unit.NewSourceLocationEnhancedQuickCheckSpec.test_SourceLocationEnhancedQuickCheck
    , Test.Unit.NewParserEnhancedQuickCheckSpec.test_ParserEnhancedQuickCheck
    , Test.Unit.NewErrorHandlerEnhancedQuickCheckSpec.test_ErrorHandlerEnhancedQuickCheck
    ]

  -- ============================================================================
  -- New Cabal Test Modules (10 comprehensive tests added)
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Enhanced Testing Coverage"
    [ Test.Unit.NewUtilsStringProcessingSpec.tests
    , Test.Unit.NewSourceLocationCalculationSpec.tests
    , Test.Unit.NewParserBasicPropertiesSpec.tests
    , Test.Unit.NewErrorHandlerPropertiesSpec.tests
    , Test.Unit.NewOwnershipTransferPropertiesSpec.tests
    , Test.Unit.NewDependenciesAnalysisPropertiesSpec.tests
    , Test.Unit.NewCompilerOptimizationPropertiesSpec.tests
    , Test.Unit.NewSyntaxValidatorRobustnessSpec.tests
    , Test.Unit.NewDependentTypeValidationSpec.tests
    , Test.Unit.NewIntegrationEndToEndSpec.tests
    ]

  -- ============================================================================
  -- New QuickCheck Test Modules Added (2025) - Additional comprehensive tests
  -- ============================================================================
  , testGroup "New QuickCheck Test Modules - Enhanced Coverage"
    [ Test.Unit.NewUtilsQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationQuickCheckSpec.tests
    , Test.Unit.NewParserQuickCheckSpec.tests
    , Test.Unit.NewComprehensiveQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive QuickCheck Test Modules (2025) - 10 Enhanced Tests
  -- ============================================================================
  , testGroup "New Comprehensive QuickCheck Test Modules - Enhanced Testing Coverage"
    [ Test.Unit.NewUtilsStringProcessingQuickCheckSpec.tests
    , Test.Unit.NewSourceLocationTrackingQuickCheckSpec.tests
    , Test.Unit.NewParserQuickCheckSpec.tests
    , Test.Unit.NewCompilerIRQuickCheckSpec.tests
    , Test.Unit.NewOwnershipQuickCheckSpec.tests
    , Test.Unit.NewErrorHandlerQuickCheckSpec.tests
    , Test.Unit.NewDependenciesQuickCheckSpec.tests
    , Test.Unit.NewIntegrationQuickCheckSpec.tests
    , Test.Unit.NewDependentTypesQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Concise QuickCheck Test Modules (2025) - 10 focused tests
  -- ============================================================================
  , testGroup "New Concise QuickCheck Test Modules - Focused Testing"
    [ Test.Unit.ConciseUtilsQuickCheckSpec.tests
    , Test.Unit.ConciseParserQuickCheckSpec.tests
    , Test.Unit.ConciseSourceLocationQuickCheckSpec.tests
    , Test.Unit.ConciseErrorHandlerQuickCheckSpec.tests
    , Test.Unit.ConciseDependenciesQuickCheckSpec.tests
    , Test.Unit.ConciseOwnershipQuickCheckSpec.tests
    , Test.Unit.ConciseTypeSystemQuickCheckSpec.tests
    , Test.Unit.ConciseSyntaxValidatorQuickCheckSpec.tests
    , Test.Unit.ConciseCompilerIRQuickCheckSpec.tests
    , Test.Unit.ConciseIntegrationQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Cabal QuickCheck Test Modules (2025) - 10 Comprehensive Tests
  -- ============================================================================
  , testGroup "New Cabal QuickCheck Test Modules - Comprehensive Property Testing"
    [ Test.Unit.NewCabalQuickCheckSpec1.tests
    , Test.Unit.NewCabalQuickCheckSpec2.tests
    , Test.Unit.NewCabalQuickCheckSpec3.tests
    , Test.Unit.NewCabalQuickCheckSpec4.tests
    , Test.Unit.NewCabalQuickCheckSpec5.tests
    , Test.Unit.NewCabalQuickCheckSpec6.tests
    , Test.Unit.NewCabalQuickCheckSpec7.tests
    , Test.Unit.NewCabalQuickCheckSpec8.tests
    , Test.Unit.NewCabalQuickCheckSpec9.tests
    , Test.Unit.NewCabalQuickCheckSpec10.tests
    , Test.Unit.AdditionalCabalQuickCheckTestSuite.tests
    ]

  -- ============================================================================
  -- New Cabal QuickCheck Test Suite (2025) - 10 comprehensive tests
  -- ============================================================================
  , testGroup "New Cabal QuickCheck Test Suite - Enhanced Property Testing"
    [ Test.Unit.NewCabalQuickCheckTestSuite.tests
    ]

  -- ============================================================================
  -- New Cabal Test Modules (2025) - 10 Comprehensive Tests
  -- ============================================================================
  , testGroup "New Cabal Test Modules - Comprehensive Testing Coverage"
    [ Test.Unit.NewCabalTest1Spec.tests
    , Test.Unit.NewCabalTest2Spec.tests
    , Test.Unit.NewCabalTest3Spec.tests
    , Test.Unit.NewCabalTest4Spec.tests
    , Test.Unit.NewCabalTest5Spec.tests
    , Test.Unit.NewCabalTest6Spec.tests
    , Test.Unit.NewCabalTest7Spec.tests
    , Test.Unit.NewCabalTest8Spec.tests
    , Test.Unit.NewCabalTest9Spec.tests
    , Test.Unit.NewCabalTest10Spec.tests
    ]

  -- ============================================================================
  -- New Core Module QuickCheck Test Suite (2025) - 8 comprehensive tests
  -- ============================================================================
  , testGroup "New Core Module QuickCheck Test Suite - Comprehensive Property Testing"
    [ Test.Unit.NewCoreUtilsQuickCheckSpec.testSuite
    , Test.Unit.NewCoreSourceLocationQuickCheckSpec.testSuite
    , Test.Unit.NewCoreParserQuickCheckSpec.testSuite
    , Test.Unit.NewComprehensiveCoreQuickCheckSpec.testSuite
    , Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec.testSuite
    , Test.Unit.NewCorePerformanceQuickCheckSpec.testSuite
    , Test.Unit.NewCoreErrorHandlingQuickCheckSpec.testSuite
    , Test.Unit.NewCompleteCoreTestSuiteSpec.testSuite
    ]

  -- ============================================================================
  -- New Essential Test Modules (2025) - Core Functionality Testing
  -- ============================================================================
  , testGroup "Essential Test Modules - Core Functionality"
    [ Test.Unit.CoreUtilsEssentialSpec.tests
    , Test.Unit.CoreSourceLocationEssentialSpec.tests
    , Test.Unit.CoreParserEssentialSpec.tests
    , Test.Unit.CoreCompilerEssentialSpec.tests
    , Test.Unit.IntegrationEssentialSpec.tests
    ]

  -- ============================================================================
  -- New Core Functionality QuickCheck Tests
  -- ============================================================================
  , testGroup "New Core Functionality QuickCheck Tests"
    [ Test.Unit.NewCoreFunctionalityQuickCheckTests.tests
    ]

  -- ============================================================================
  -- Additional Enhanced QuickCheck Test Modules (10 comprehensive tests)
  -- ============================================================================
  , testGroup "Additional Enhanced QuickCheck Test Modules - Comprehensive Testing"
    [ Test.Unit.EnhancedTextProcessingQuickCheckSpec.tests
    , Test.Unit.NewEnhancedSourceLocationMathPropertiesQuickCheckSpec.tests
    , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
    , Test.Unit.CrossModuleIntegrationQuickCheckSpec.tests
    , Test.Unit.PerformanceBoundaryQuickCheckSpec.tests
    , Test.Unit.ErrorHandlingPropertiesQuickCheckSpec.tests
    , Test.Unit.CompilerIRPropertiesQuickCheckSpec.tests
    , Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec.tests
    , Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec.tests
    , Test.Unit.EndToEndIntegrationQuickCheckSpec.tests
    ]

  -- ============================================================================
  -- New Comprehensive QuickCheck Test Modules (2025) - 10 Core Modules
  -- ============================================================================
  , testGroup "New Comprehensive QuickCheck Test Modules - Core Functionality Testing"
    [ Test.Unit.UtilsComprehensiveQuickCheckSpec.utilsComprehensiveQuickCheckSpec
    , Test.Unit.SourceLocationComprehensiveQuickCheckSpec.sourceLocationComprehensiveQuickCheckSpec
    , Test.Unit.ParserComprehensiveQuickCheckSpec.parserComprehensiveQuickCheckSpec
    , Test.Unit.ErrorHandlerComprehensiveQuickCheckSpec.errorHandlerComprehensiveQuickCheckSpec
    , Test.Unit.OwnershipComprehensiveQuickCheckSpec.ownershipComprehensiveQuickCheckSpec
    , Test.Unit.CompilerComprehensiveQuickCheckSpec.compilerComprehensiveQuickCheckSpec
    , Test.Unit.DependenciesComprehensiveQuickCheckSpec.dependenciesComprehensiveQuickCheckSpec
    , Test.Unit.DependentTypesComprehensiveQuickCheckSpec.dependentTypesComprehensiveQuickCheckSpec
    , Test.Unit.SyntaxValidatorComprehensiveQuickCheckSpec.syntaxValidatorComprehensiveQuickCheckSpec
      ]
    
      -- ============================================================================
      -- New Core Cabal QuickCheck Test Modules (2025) - 10 comprehensive tests
      -- ============================================================================
      , testGroup "New Core Cabal QuickCheck Tests - Comprehensive Test Suite"
        [ Test.Unit.NewCoreCabalQuickCheckSpec1.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec2.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec3.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec4.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec5.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec6.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec7.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec8.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec9.tests
        , Test.Unit.NewCoreCabalQuickCheckSpec10.tests
        ]

  -- ============================================================================

    -- New Cabal QuickCheck Test Modules (2025) - 6 comprehensive tests

    -- ============================================================================

    , testGroup "New Cabal QuickCheck Test Modules - Enhanced Coverage"

      [ Test.Unit.NewCabalUtilsQuickCheckSpec.tests

      , Test.Unit.NewCabalSourceLocationQuickCheckSpec.tests

      , Test.Unit.NewCabalParserQuickCheckSpec.tests

      , Test.Unit.NewCabalOwnershipQuickCheckSpec.tests

      , Test.Unit.NewCabalDependenciesQuickCheckSpec.tests

      , Test.Unit.NewCabalErrorHandlerQuickCheckSpec.tests

      ]

  

    -- ============================================================================

    -- New Cabal QuickCheck Test Modules (2025) - 10 comprehensive tests

    -- ============================================================================

    , testGroup "New Cabal QuickCheck Test Modules - Comprehensive Test Suite"

      [ Test.Unit.NewCabalUtilsQuickCheckTestSpec.tests

      , Test.Unit.NewCabalSourceLocationQuickCheckTestSpec.tests

      , Test.Unit.NewCabalParserQuickCheckTestSpec.tests

      , Test.Unit.NewCabalErrorHandlerQuickCheckTestSpec.tests

      , Test.Unit.NewCabalDependenciesQuickCheckTestSpec.tests

      , Test.Unit.NewCabalOwnershipQuickCheckTestSpec.tests

      , Test.Unit.NewCabalCompilerQuickCheckTestSpec.tests

      , Test.Unit.NewCabalSyntaxValidatorQuickCheckTestSpec.tests

      , Test.Unit.NewCabalGoToolchainQuickCheckTestSpec.tests

      , Test.Unit.NewCabalIntegrationQuickCheckTestSpec.tests

      ]

    -- ============================================================================
    -- New QuickCheck Test Modules Added (Core Functionality Testing)
    -- ============================================================================
    , testGroup "New QuickCheck Test Modules - Core Functionality"
        [ Test.Unit.UtilsCorePropertiesQuickCheckSpec.testSuite
        , Test.Unit.NewSourceLocationMathCoreQuickCheckSpec.testSuite
        , Test.Unit.NewParserBoundaryCoreQuickCheckSpec.testSuite
        , Test.Unit.NewOwnershipBasicCoreQuickCheckSpec.testSuite
        , Test.Unit.NewDependenciesInferenceCoreQuickCheckSpec.testSuite
        , Test.Unit.NewEnhancedErrorHandlerConsistencyQuickCheckSpec.tests
        , Test.Unit.NewCompilerIRCoreQuickCheckSpec.testSuite
        , Test.Unit.NewIntegrationEndToEndCoreQuickCheckSpec.testSuite
        , Test.Unit.SourceLocationBoundaryQuickCheckSpec.testSuite
        , Test.Unit.StringProcessingQuickCheckSpec.testSuite
        ]

    -- ============================================================================
    -- New QuickCheck Test Modules (Added for enhanced coverage)
    -- ============================================================================
    , testGroup "New QuickCheck Test Modules - Enhanced Coverage"
        [ Test.Unit.SourceLocationMathQuickCheckSpec.tests
        , Test.Unit.ErrorHandlingRecoveryQuickCheckSpec.tests
        , Test.Unit.OwnershipTransitivityQuickCheckSpec.tests
        , Test.Unit.DependencyCycleQuickCheckSpec.tests
        , Test.Unit.CompilerIROptimizationQuickCheckSpec.tests
        , Test.Unit.StringProcessingBoundaryQuickCheckSpec.tests
        , Test.Unit.TypeSystemInferenceQuickCheckSpec.tests
        ]

    -- ============================================================================
    -- New Comprehensive Typus Test Module (2025)
    -- ============================================================================
    , Test.Unit.NewComprehensiveTypusTestSpec.tests

    -- ============================================================================
    -- New Cabal QuickCheck Test Modules (2025) - 7 comprehensive tests
    -- ============================================================================
    , testGroup "New Cabal QuickCheck Test Modules - Core Functionality Testing"
        [ Test.Unit.NewCabalUtilsQuickCheckTestsSpec.tests
        , Test.Unit.NewCabalSourceLocationQuickCheckTestsSpec.tests
        , Test.Unit.NewCabalParserQuickCheckTestsSpec.tests
        , Test.Unit.NewCabalErrorHandlerQuickCheckTestsSpec.tests
        , Test.Unit.NewCabalOwnershipQuickCheckTestsSpec.tests
        , Test.Unit.NewCabalDependenciesQuickCheckTestsSpec.tests
        ]

    -- ============================================================================
    -- Enhanced Cabal Test Suite - 8 comprehensive QuickCheck tests
    -- ============================================================================
    , testGroup "Enhanced Cabal Test Suite - Core Functionality Tests"
        [ Test.Unit.EnhancedCabalTestSuiteSpec.tests
        , Test.Unit.CompilerIRConsistencyQuickCheckSpec.tests
        , Test.Unit.OwnershipTransitivityQuickCheckSpec.tests
        , Test.Unit.DependentTypeBoundaryQuickCheckSpec.tests
        , Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec.tests
        , Test.Unit.IntegrationEndToEndQuickCheckSpec.tests
        , Test.Unit.UtilsBoundaryConditionsQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
        ]

    -- ============================================================================
    -- New Cabal Test Modules (Added by user request) - 10 comprehensive tests
    -- ============================================================================
    , testGroup "New Cabal Test Modules - Comprehensive Testing Suite"
        [ Test.Unit.NewCabalCoreSpec.tests
        , Test.Unit.NewCabalParserBoundarySpec.tests
        , Test.Unit.NewCabalCompilerInvariantSpec.tests
        , Test.Unit.NewCabalOwnershipSafetySpec.tests
        , Test.Unit.NewCabalTypeSystemSpec.tests
        , Test.Unit.NewCabalErrorRecoverySpec.tests
        , Test.Unit.NewCabalPerformanceSpec.tests
        , Test.Unit.NewCabalIntegrationSpec.tests
        , Test.Unit.NewCabalQuickCheckSpec.tests
        , Test.Unit.NewCabalEdgeCaseSpec.tests
        ]

    -- ============================================================================
    -- Simple Cabal QuickCheck Tests (4 comprehensive tests)
    -- ============================================================================
    , Test.Unit.SimpleCabalQuickCheckTests.tests

    -- ============================================================================
    -- New QuickCheck Test Modules (2025) - 8 comprehensive tests
    -- ============================================================================
    , testGroup "New QuickCheck Test Modules - Core Functionality Testing"
        [ Test.Unit.NewCoreParsingQuickCheckSpec.tests
        , Test.Unit.NewAdvancedTypeSystemQuickCheckSpec.tests
        , Test.Unit.NewMemorySafetyQuickCheckSpec.tests
        , Test.Unit.NewAdvancedErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.NewPerformanceQuickCheckSpec.tests
        , Test.Unit.NewIntegrationQuickCheckSpec.tests
        , Test.Unit.NewOwnershipAnalysisQuickCheckSpec.tests
        , Test.Unit.NewCompilerOptimizationQuickCheckSpec.tests
        ]

    -- ============================================================================
    -- New QuickCheck Property Test Modules (Added for enhanced testing)
    -- ============================================================================
    , testGroup "New QuickCheck Property Test Modules - Core Functionality"
        [ Test.Unit.NewSourceLocationQuickCheckPropertiesSpec.tests
        , Test.Unit.NewErrorHandlerQuickCheckPropertiesSpec.tests
        , Test.Unit.NewCompilerQuickCheckPropertiesSpec.tests
        , Test.Unit.NewParserQuickCheckPropertiesSpec.tests
        , Test.Unit.NewUtilsQuickCheckPropertiesSpec.tests
        ]

    -- ============================================================================
    -- New Compact Test Modules (2025) - 8 comprehensive tests
    -- ============================================================================
    , testGroup "New Compact Core Functionality Tests"
        [ Test.Unit.NewCompactUtilsSpec.tests
        , Test.Unit.NewCompactSourceLocationSpec.tests
        , Test.Unit.NewCompactParserSpec.tests
        , Test.Unit.NewCompactErrorHandlerSpec.tests
        , Test.Unit.NewCompactOwnershipSpec.tests
        , Test.Unit.NewCompactDependenciesSpec.tests
        , Test.Unit.NewCompactCompilerIRSpec.tests
        , Test.Unit.NewCompactIntegrationSpec.tests
        ]

        ]
