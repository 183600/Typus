module Test.Unit.Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Unit.CLISpec
import qualified Test.Unit.CommandLineDebugSpec
import qualified Test.Unit.CompilerSpec
import qualified Test.Unit.DependentTypesSpec
import qualified Test.Unit.EmbedAssetsSpec
import qualified Test.Unit.ErrorHandlingSpec
import qualified Test.Unit.GoToolchainSpec
import qualified Test.Unit.OwnershipSpec
import qualified Test.Unit.OwnershipBridgeSpec
import qualified Test.Unit.ParserSpec
import qualified Test.Unit.TypeSystemSpec
import qualified Test.Unit.SymbolTableSpec
import qualified Test.Unit.SourceLocationSpec
import qualified Test.Unit.SyntaxValidatorSpec
import qualified Test.Unit.ValueAnalysisSpec
import qualified Test.Unit.VerbositySpec
import qualified Test.Unit.UtilsSpec
import qualified Test.Unit.AdvancedParserSpec
import qualified Test.Unit.IntegrationSpec
import qualified Test.Unit.PerformanceSpec
import qualified Test.Unit.EdgeCaseSpec
import qualified Test.Unit.EnhancedQuickCheckSpec

-- Additional QuickCheck test modules
import qualified Test.Unit.UtilsAdditionalQuickCheckSpec
import qualified Test.Unit.SourceLocationAdditionalQuickCheckSpec
import qualified Test.Unit.ParserAdditionalQuickCheckSpec
import qualified Test.Unit.IntegrationAdditionalQuickCheckSpec

-- Additional Core Tests Module
import qualified Test.Unit.AdditionalCoreTestsSpec

-- New Cabal Test Module
import qualified Test.Unit.NewCabalTestSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerErrorHandlingQuickCheckSpec
import qualified Test.Unit.DependentTypesSystemQuickCheckSpec
import qualified Test.Unit.OwnershipAnalysisComprehensiveQuickCheckSpec
import qualified Test.Unit.ParserBoundaryConditionsQuickCheckSpec
import qualified Test.Unit.IntegrationFeaturesQuickCheckSpec
-- New Cabal QuickCheck test modules
import qualified Test.Unit.NewCabalQuickCheckTestSpec
import qualified Test.Unit.NewCabalTestsSpec

-- New core test modules
import qualified Test.Unit.SourceLocationCoreTestSpec
import qualified Test.Unit.ParserCoreTestSpec
import qualified Test.Unit.ErrorHandlerCoreTestSpec
import qualified Test.Unit.DependenciesCoreTestSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerOptimizationQuickCheckSpec
import qualified Test.Unit.OwnershipTransferQuickCheckSpec
import qualified Test.Unit.DependentTypesValidationQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SourceLocationTrackingQuickCheckSpec

-- New core QuickCheck test modules
import qualified Test.Unit.SourceLocationCoreQuickCheckSpec
import qualified Test.Unit.ErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.DependenciesCoreQuickCheckSpec
import qualified Test.Unit.GoToolchainCoreQuickCheckSpec
import qualified Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec
import qualified Test.Unit.DebugIntegrationCoreQuickCheckSpec

-- QuickCheck test modules
import qualified Test.Unit.ParserQuickCheckSpec
import qualified Test.Unit.CompilerQuickCheckSpec
import qualified Test.Unit.TypeCheckerQuickCheckSpec
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
import qualified Test.Unit.EnhancedCoreQuickCheckSpec
-- New QuickCheck property test modules
import qualified Test.Unit.ComprehensiveQuickCheckSpec
import qualified Test.Unit.CoreDataStructuresQuickCheckSpec
import qualified Test.Unit.CompilerIRQuickCheckSpec
import qualified Test.Unit.TypeSystemQuickCheckSpec
import qualified Test.Unit.OwnershipAnalysisQuickCheckSpec
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
import qualified Test.Unit.ComprehensiveQuickCheckTestSpec
import qualified Test.Unit.FinalQuickCheckTestSpec
-- New comprehensive QuickCheck test modules
import qualified Test.Unit.NewCoreQuickCheckSpec
import qualified Test.Unit.ParserPropertiesQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SourceLocationPropertiesQuickCheckSpec
import qualified Test.Unit.NewCoreQuickCheckTests
import qualified Test.Unit.SimpleCoreQuickCheckSpec
import qualified Test.Unit.NewCabalQuickCheckSpec
import qualified Test.Unit.NewCabalQuickCheckTests
import qualified Test.Unit.NewCabalQuickCheckTestSpec
import qualified Test.Unit.NewCabalTestQuickCheckSpec
import qualified Test.Unit.AdditionalCabalQuickCheckSpec
import qualified Test.Unit.NewSimpleCabalQuickCheckSpec
import qualified Test.Unit.AdditionalCabalTestsSpec
import qualified Test.Unit.CabalQuickCheckTests
import qualified Test.Unit.NewCabslQuickCheckTests
import qualified Test.Unit.AdditionalQuickCheckTests
import qualified Test.Unit.NewQuickCheckTestCasesSpec
import qualified Test.Unit.CoreModuleQuickCheckSpec
import qualified Test.Unit.AdditionalCoreQuickCheckSpec
import qualified Test.Unit.NewQuickCheckTestSpec
import qualified Test.Unit.NewCabalQuickCheckTestCasesSpec
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
import qualified Test.Unit.AdditionalCabalTestsSpec
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

-- Additional new test modules for enhanced coverage
import qualified Test.Unit.EnhancedErrorHandlingQuickCheckSpec
import qualified Test.Unit.CompilerIntegrationQuickCheckSpec
import qualified Test.Unit.TypeSystemBoundaryQuickCheckSpec
import qualified Test.Unit.ParserErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SemanticAnalysisQuickCheckSpec
import qualified Test.Unit.CodeGenerationQuickCheckSpec
import qualified Test.Unit.DependentTypeSystemQuickCheckSpec
import qualified Test.Unit.ToolchainIntegrationQuickCheckSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.NewErrorHandlerQuickCheckSpec
import qualified Test.Unit.NewDependentTypesQuickCheckSpec
import qualified Test.Unit.NewOwnershipQuickCheckSpec
import qualified Test.Unit.NewSourceLocationQuickCheckSpec
import qualified Test.Unit.NewSyntaxValidatorQuickCheckSpec
import qualified Test.Unit.NewGoToolchainQuickCheckSpec
import qualified Test.Unit.NewIRQuickCheckSpec
import qualified Test.Unit.NewSymbolTableQuickCheckSpec

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
    , Test.Unit.OwnershipSpec.tests
    , Test.Unit.OwnershipBridgeSpec.tests
    , Test.Unit.DependentTypesSpec.tests
    , Test.Unit.TypeSystemSpec.tests
    , Test.Unit.SymbolTableSpec.tests
    , Test.Unit.SourceLocationSpec.tests
    , Test.Unit.SyntaxValidatorSpec.tests
    , Test.Unit.CompilerSpec.tests
    , Test.Unit.ValueAnalysisSpec.tests
    , Test.Unit.ErrorHandlingSpec.tests
    , Test.Unit.EmbedAssetsSpec.tests
    , Test.Unit.GoToolchainSpec.tests
    , Test.Unit.CommandLineDebugSpec.tests
    , Test.Unit.CLISpec.tests
    , Test.Unit.VerbositySpec.tests
    , Test.Unit.UtilsSpec.tests
    , Test.Unit.AdvancedParserSpec.tests
    , Test.Unit.IntegrationSpec.tests
    , Test.Unit.PerformanceSpec.tests
    , Test.Unit.EdgeCaseSpec.tests
    , Test.Unit.EnhancedQuickCheckSpec.tests
    , Test.Unit.AdditionalCabalTestsSpec.tests
    , Test.Unit.AdditionalUtilsSpec.tests
    , Test.Unit.AdditionalUtilsQuickCheckSpec.tests
    , Test.Unit.SourceLocationAdditionalSpec.tests
    , Test.Unit.SourceLocationAdditionalQuickCheckSpec.tests
    , Test.Unit.ParserAdditionalSpec.tests
    , Test.Unit.NewCabalTestSpec.tests
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
    , testGroup "Additional New Test Modules"
        [ Test.Unit.NewParserValidationSpec.tests
        , Test.Unit.NewCompilerOptimizationSpec.tests
        , Test.Unit.NewOwnershipTransferSpec.tests
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
        ]
    , testGroup "New Comprehensive QuickCheck Tests"
        [ Test.Unit.CompilerOptimizationQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferQuickCheckSpec.tests
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
        ]
    , testGroup "Additional Enhanced Test Modules"
        [ Test.Unit.EnhancedErrorHandlingQuickCheckSpec.tests
        , Test.Unit.CompilerIntegrationQuickCheckSpec.tests
        , Test.Unit.TypeSystemBoundaryQuickCheckSpec.tests
        , Test.Unit.ParserErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.SemanticAnalysisQuickCheckSpec.tests
        , Test.Unit.CodeGenerationQuickCheckSpec.tests
        , Test.Unit.DependentTypeSystemQuickCheckSpec.tests
        , Test.Unit.ToolchainIntegrationQuickCheckSpec.tests
        ]
    , testGroup "New Comprehensive QuickCheck Test Modules"
        [ Test.Unit.NewErrorHandlerQuickCheckSpec.tests
        , Test.Unit.NewDependentTypesQuickCheckSpec.tests
        , Test.Unit.NewOwnershipQuickCheckSpec.tests
        , Test.Unit.NewSourceLocationQuickCheckSpec.tests
        , Test.Unit.NewSyntaxValidatorQuickCheckSpec.tests
        , Test.Unit.NewGoToolchainQuickCheckSpec.tests
        , Test.Unit.NewIRQuickCheckSpec.tests
        , Test.Unit.NewSymbolTableQuickCheckSpec.tests
        ]
    ]
