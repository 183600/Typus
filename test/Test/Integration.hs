module Test.Integration (tests) where

import Test.Tasty (TestTree)
import qualified Test.Integration.Tests as T

-- Re-export the consolidated integration tests under the expected module name
tests :: TestTree
tests = T.tests
