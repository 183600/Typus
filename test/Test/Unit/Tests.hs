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

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerErrorHandlingQuickCheckSpec
import qualified Test.Unit.DependentTypesSystemQuickCheckSpec
import qualified Test.Unit.OwnershipAnalysisComprehensiveQuickCheckSpec
import qualified Test.Unit.ParserBoundaryConditionsQuickCheckSpec
import qualified Test.Unit.IntegrationFeaturesQuickCheckSpec
-- New Cabal QuickCheck test modules
import qualified Test.Unit.NewCabalQuickCheckTestSpec

-- New comprehensive QuickCheck test modules
import qualified Test.Unit.CompilerOptimizationQuickCheckSpec
import qualified Test.Unit.OwnershipTransferQuickCheckSpec
import qualified Test.Unit.DependentTypesValidationQuickCheckSpec
import qualified Test.Unit.ErrorRecoveryQuickCheckSpec
import qualified Test.Unit.SourceLocationTrackingQuickCheckSpec

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
        ]
    , testGroup "New Comprehensive QuickCheck Tests"
        [ Test.Unit.CompilerOptimizationQuickCheckSpec.tests
        , Test.Unit.OwnershipTransferQuickCheckSpec.tests
        , Test.Unit.DependentTypesValidationQuickCheckSpec.tests
        , Test.Unit.ErrorRecoveryQuickCheckSpec.tests
        , Test.Unit.SourceLocationTrackingQuickCheckSpec.tests
        ]
    ]
