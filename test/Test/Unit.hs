module Test.Unit (tests) where

import Test.Tasty (TestTree)
import qualified Test.Unit.Tests as T

-- Re-export the consolidated unit tests under the expected module name
tests :: TestTree
tests = T.tests
