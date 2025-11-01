module Test.Golden (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Golden.CompilerSpec

-- | Golden regression tests that compare generated artefacts against curated
-- fixtures.
tests :: TestTree
tests =
  testGroup "Golden"
    [ Test.Golden.CompilerSpec.tests
    ]
