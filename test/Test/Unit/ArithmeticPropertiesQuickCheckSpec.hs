{-# LANGUAGE CPP #-}

module Test.Unit.ArithmeticPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y =
  x + y === y + x

prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z =
  (x + y) + z === x + (y + z)

prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y =
  x * y === y * x

prop_multiplication_associative :: Int -> Int -> Int -> Property
prop_multiplication_associative x y z =
  (x * y) * z === x * (y * z)

prop_distributive :: Int -> Int -> Int -> Property
prop_distributive x y z =
  x * (y + z) === x * y + x * z

prop_subtraction_inverse :: Int -> Int -> Property
prop_subtraction_inverse x y =
  (x + y) - y === x

tests :: TestTree
tests = testGroup "Arithmetic Properties QuickCheck"
  [ fastProperty "addition is commutative" prop_addition_commutative
  , fastProperty "addition is associative" prop_addition_associative
  , fastProperty "multiplication is commutative" prop_multiplication_commutative
  , fastProperty "multiplication is associative" prop_multiplication_associative
  , fastProperty "distributive law" prop_distributive
  , fastProperty "subtraction is inverse of addition" prop_subtraction_inverse
  ]
