module Test.Integration (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Integration.PipelineSpec

-- | Integration scenarios that exercise the full compiler pipeline and external
-- tooling (when available).
tests :: TestTree
tests =
  testGroup "Integration"
    [ Test.Integration.PipelineSpec.tests
    ]
