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
import qualified Test.Unit.CoreDataStructuresQuickCheckSpec
import qualified Test.Unit.ComprehensiveTypeCheckerQuickCheckSpec
-- Simple QuickCheck test modules
import qualified Test.Unit.SimpleParserQuickCheckSpec
import qualified Test.Unit.SimpleDataStructuresQuickCheckSpec
import qualified Test.Unit.SimpleTypeCheckerQuickCheckSpec

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
            , Test.Unit.SimpleDataStructuresQuickCheckSpec.tests
            , Test.Unit.SimpleTypeCheckerQuickCheckSpec.tests
            ]
            ]
        ]
    ]
