module Test.Golden (tests) where

import Test.Tasty (TestTree)
import qualified Test.Golden.Tests as T

-- Re-export the consolidated golden tests under the expected module name
-- to match Cabal's module resolution.
tests :: TestTree
tests = T.tests
