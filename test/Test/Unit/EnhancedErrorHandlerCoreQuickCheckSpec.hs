module Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec Tests"
  [ testProperty "basic property" prop_basic_property
  ]
