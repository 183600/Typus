module Test.Unit.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.Unit.NewCorePropertiesQuickCheckSpec as NewCoreProperties
import qualified Test.Unit.ComprehensiveCoreModulesQuickCheckSpec as ComprehensiveCoreModules
import qualified Test.Unit.NewQuickCheckTestSuiteSpec as NewQuickCheckTestSuite
import qualified Test.Unit.AdditionalQuickCheckTestSuiteSpec as AdditionalQuickCheckTestSuite

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.Tests Tests"
  [ testProperty "basic property" prop_basic_property,
    NewCoreProperties.tests,
    ComprehensiveCoreModules.tests,
    NewQuickCheckTestSuite.tests,
    AdditionalQuickCheckTestSuite.tests
  ]
