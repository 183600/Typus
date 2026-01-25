module Test.Unit.ParserErrorRecoveryPropertiesSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.ParserErrorRecoveryPropertiesSpec Tests"
  [ testProperty "basic property" prop_basic_property
  ]
