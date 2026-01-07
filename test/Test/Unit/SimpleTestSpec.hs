module Test.Unit.SimpleTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.List as L

-- Basic test properties
prop_reverse_reverse :: [Int] -> Bool
prop_reverse_reverse xs = L.reverse (L.reverse xs) == xs

tests :: TestTree
tests = testGroup "Simple Test"
  [ testProperty "reverse reverse" prop_reverse_reverse
  ]