module Test.Unit.Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Unit.CLISpec
import qualified Test.Unit.CompilerSpec
import qualified Test.Unit.DependentTypesSpec
import qualified Test.Unit.OwnershipSpec
import qualified Test.Unit.ParserSpec

-- | Aggregate all lightweight, fast-running tests that only depend on
-- in-process library calls. These can be executed under the "fast" Cabal flag.
tests :: TestTree
tests =
  testGroup "Unit"
    [ Test.Unit.ParserSpec.tests
    , Test.Unit.OwnershipSpec.tests
    , Test.Unit.DependentTypesSpec.tests
    , Test.Unit.CompilerSpec.tests
    , Test.Unit.CLISpec.tests
    ]
