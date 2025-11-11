module Test.Integration.Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Integration.PipelineSpec
import qualified Test.Integration.AnalyzerSpec
import qualified Test.Integration.IntegratedCompilerSpec
import qualified Test.Integration.FullProjectSpec

-- | Integration scenarios that exercise the full compiler pipeline and external
-- tooling (when available).
tests :: TestTree
tests =
  testGroup "Integration"
    [ Test.Integration.PipelineSpec.tests
    , Test.Integration.AnalyzerSpec.tests
    , Test.Integration.IntegratedCompilerSpec.tests
    , Test.Integration.FullProjectSpec.tests
    ]
