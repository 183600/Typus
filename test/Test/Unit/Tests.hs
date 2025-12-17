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

-- | Aggregate all lightweight, fast-running tests that only depend on
-- in-process library calls. These can be executed under the "fast" Cabal flag.
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
        , testGroup "Extended QuickCheck Tests"
            [ Test.Unit.ExtendedParserQuickCheckSpec.tests
            , Test.Unit.ExtendedCompilerQuickCheckSpec.tests
            , Test.Unit.ExtendedOwnershipQuickCheckSpec.tests
            , Test.Unit.ExtendedTypeCheckerQuickCheckSpec.tests
            , Test.Unit.ExtendedAnalyzerQuickCheckSpec.tests
            , Test.Unit.ExtendedUtilsQuickCheckSpec.tests
            , testGroup "Additional QuickCheck Tests"
                [ Test.Unit.IRQuickCheckSpec.tests
                , Test.Unit.GoAstQuickCheckSpec.tests
                , Test.Unit.ErrorHandlerQuickCheckSpec.tests
                , Test.Unit.AnalyzerIntegrationQuickCheckSpec.tests
                , Test.Unit.CliQuickCheckSpec.tests
                , Test.Unit.GoToolchainQuickCheckSpec.tests
                ]
            , testGroup "Comprehensive QuickCheck Tests"
                [ Test.Unit.ComprehensiveParserQuickCheckSpec.tests
                , Test.Unit.ComprehensiveCompilerQuickCheckSpec.tests
                , Test.Unit.ComprehensiveOwnershipQuickCheckSpec.tests
                , Test.Unit.ComprehensiveDependenciesQuickCheckSpec.tests
                , Test.Unit.ComprehensiveUtilsQuickCheckSpec.tests
                , Test.Unit.ComprehensiveAnalyzerQuickCheckSpec.tests
                , testGroup "Additional Comprehensive QuickCheck Tests"
                [ Test.Unit.CoreQuickCheckSpec.tests
                , Test.Unit.ErrorHandlingComprehensiveQuickCheckSpec.tests
                , Test.Unit.DependentTypesComprehensiveQuickCheckSpec.tests
                , Test.Unit.OwnershipComprehensiveQuickCheckSpec.tests
                ]
            ]
        , testGroup "Simple QuickCheck Tests"
            [ Test.Unit.SimpleParserQuickCheckSpec.tests
            , Test.Unit.SimpleQuickCheckSpec.tests
            , Test.Unit.SimpleDataStructuresQuickCheckSpec.tests
            , Test.Unit.SimpleTypeCheckerQuickCheckSpec.tests
            , Test.Unit.NewQuickCheckSpec.tests
            , Test.Unit.FocusedQuickCheckSpec.tests
            , Test.Unit.BasicPropertiesQuickCheckSpec.tests
            , Test.Unit.StringUtilsQuickCheckSpec.tests
            , Test.Unit.NewCabalQuickCheckPropertiesSpec.tests
            , Test.Unit.WorkingQuickCheckSpec.tests
            , Test.Unit.AdditionalQuickCheckSpec.tests
            ]
        , testGroup "New Core QuickCheck Tests"
            [ Test.Unit.CoreDataStructuresQuickCheckSpec.tests
            , Test.Unit.CompilerIRQuickCheckSpec.tests
            , Test.Unit.TypeSystemQuickCheckSpec.tests
            , Test.Unit.OwnershipAnalysisQuickCheckSpec.tests
            , Test.Unit.ErrorHandlingQuickCheckSpec.tests
            , Test.Unit.NewCoreQuickCheckTests.tests
            , Test.Unit.SimpleCoreQuickCheckSpec.tests
            ]
        , testGroup "Additional New QuickCheck Tests"
            [ Test.Unit.SimpleQuickCheckTestSpec.tests
            , Test.Unit.WorkingQuickCheckSpec.tests
            , Test.Unit.ComprehensiveQuickCheckSpec.tests
            , Test.Unit.NewCabalQuickCheckSpec.tests
            , Test.Unit.CabalQuickCheckTestSpec.tests
            , Test.Unit.NewCabalQuickCheckTests.tests
            ]
        , testGroup "New Comprehensive QuickCheck Tests"
            [ Test.Unit.NewCoreQuickCheckSpec.tests
            , Test.Unit.ParserPropertiesQuickCheckSpec.tests
            , Test.Unit.ErrorRecoveryQuickCheckSpec.tests
            , Test.Unit.SourceLocationPropertiesQuickCheckSpec.tests
            , Test.Unit.OwnershipAnalysisQuickCheckSpec.tests
            ]
        , testGroup "New QuickCheck Test Modules"
            [ Test.Unit.NewCabalQuickCheckTestSpec.tests
            , Test.Unit.SimpleQuickCheckTestSpec.tests
            , Test.Unit.WorkingQuickCheckTestSpec.tests
            , Test.Unit.BasicQuickCheckTestSpec.tests
            , Test.Unit.PropertyQuickCheckTestSpec.tests
            , Test.Unit.CoreQuickCheckTestSpec.tests
            , Test.Unit.AdvancedQuickCheckTestSpec.tests
            , Test.Unit.ComprehensiveQuickCheckTestSpec.tests
            , Test.Unit.FinalQuickCheckTestSpec.tests
            ]
        , testGroup "New QuickCheck Tests"
            [ Test.Unit.NewQuickCheckTestsSpec.tests
            , Test.Unit.NewCabalQuickCheckTestSpec.tests
            , Test.Unit.NewCabalQuickCheckTestsSpec.tests
            , Test.Unit.NewCabalTestQuickCheckSpec.tests
            , Test.Unit.NewCabalQuickCheckTests.tests
            , Test.Unit.AdditionalCabalQuickCheckSpec.tests
            , Test.Unit.CabalQuickCheckTests.tests
            , Test.Unit.CabalEnhancedQuickCheckSpec.tests
            , Test.Unit.EnhancedCabalTestQuickCheckSpec.tests
            , Test.Unit.FreshCabalQuickCheckSpec.tests
            , Test.Unit.SimpleCabalQuickCheckSpec.tests
            , Test.Unit.MinimalCabalQuickCheckSpec.tests
            , Test.Unit.LightweightCabalQuickCheckSpec.tests
            , Test.Unit.FastCabalQuickCheckSpec.tests
            , Test.Unit.CompactCabalQuickCheckSpec.tests
            , Test.Unit.QuickCabalQuickCheckSpec.tests
            , Test.Unit.TinyCabalQuickCheckSpec.tests
            , Test.Unit.EfficientCabalQuickCheckSpec.tests
            , Test.Unit.ConciseCabalQuickCheckSpec.tests
            , Test.Unit.NewSimpleCabalQuickCheckSpec.tests
            , Test.Unit.AdditionalCabalTestsSpec.tests
            , Test.Unit.NewCabalTestSuiteQuickCheckSpec.tests
            , Test.Unit.NewCabalQuickCheckTestSuite2Spec.tests
            ]
        , Test.Unit.EnhancedCoreQuickCheckSpec.tests
            , Test.Unit.NewCabslQuickCheckTests.tests
            , Test.Unit.AdditionalQuickCheckTests.tests
            , Test.Unit.NewQuickCheckTestCasesSpec.tests
            , Test.Unit.NewCabalQuickCheckTestCasesSpec.tests
            -- New QuickCheck test modules
            , Test.Unit.ParserPropertiesQuickCheckSpec.tests
            , Test.Unit.ErrorHandlingQuickCheckSpec.tests
            , Test.Unit.TypeSystemQuickCheckSpec.tests
            , Test.Unit.OwnershipAnalysisQuickCheckSpec.tests
            , Test.Unit.SourceLocationPropertiesQuickCheckSpec.tests
            , Test.Unit.IRPropertiesQuickCheckSpec.tests
            , Test.Unit.SymbolTableQuickCheckSpec.tests
            , Test.Unit.CoreModuleQuickCheckSpec.tests
                        , Test.Unit.NewCabalQuickCheckTests.tests
                        , Test.Unit.NewCabalQuickCheckSpec.tests
                        , Test.Unit.NewCabalQuickCheckTests.tests
                        , Test.Unit.AdditionalCoreQuickCheckSpec.tests
                        , Test.Unit.NewIRQuickCheckSpec.tests
                        , Test.Unit.NewSymbolTableQuickCheckSpec.tests
                        , Test.Unit.NewTypeCheckerQuickCheckSpec.tests
                    ]
                    , Test.Unit.NewQuickCheckTestSpec.tests
                    , Test.Unit.NewCoreQuickCheckTests.tests        ]
    ]
