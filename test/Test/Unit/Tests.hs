module Test.Unit.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- Only import essential existing test modules
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.Tests Tests"
  [ testProperty "basic property" prop_basic_property,
    ConciseTestSuite.tests,
    testListProperties
  ]