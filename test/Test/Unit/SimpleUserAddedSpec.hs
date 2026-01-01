{-# LANGUAGE CPP #-}

module Test.Unit.SimpleUserAddedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "Simple User Added Properties"
  [ fastProperty "addition is commutative" prop_addition_commutative
  , fastProperty "addition is associative" prop_addition_associative
  , fastProperty "list L.reverse is involution" prop_reverse_involution
  ]

prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = x + y === y + x

prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = (x + y) + z === x + (y + z)

prop_reverse_involution :: [Int] -> Property
prop_reverse_involution xs = L.reverse (L.reverse xs) === xs