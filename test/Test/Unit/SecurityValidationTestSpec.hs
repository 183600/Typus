module Test.Unit.SecurityValidationTestSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.SecurityValidationTestSpec Tests"
  [ testProperty "basic property" prop_basic_property
  ]
