module Test.Unit.Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Unit.AnalyzerStateSpec
import qualified Test.Unit.CliRunnerSpec
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
import qualified Test.Unit.ValueAnalysisSpec
import qualified Test.Unit.VerbositySpec
import qualified Test.Unit.UtilsSpec

-- | Aggregate all lightweight, fast-running tests that only depend on
-- in-process library calls. These can be executed under the "fast" Cabal flag.
tests :: TestTree
tests =
  testGroup "Unit"
    [ Test.Unit.AnalyzerStateSpec.tests
    , Test.Unit.ParserSpec.tests
    , Test.Unit.OwnershipSpec.tests
    , Test.Unit.OwnershipBridgeSpec.tests
    , Test.Unit.DependentTypesSpec.tests
    , Test.Unit.TypeSystemSpec.tests
    , Test.Unit.SymbolTableSpec.tests
    , Test.Unit.CompilerSpec.tests
    , Test.Unit.ValueAnalysisSpec.tests
    , Test.Unit.ErrorHandlingSpec.tests
    , Test.Unit.EmbedAssetsSpec.tests
    , Test.Unit.GoToolchainSpec.tests
    , Test.Unit.CommandLineDebugSpec.tests
    , Test.Unit.CliRunnerSpec.tests
    , Test.Unit.CLISpec.tests
    , Test.Unit.VerbositySpec.tests
    , Test.Unit.UtilsSpec.tests
    ]
