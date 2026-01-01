{-# LANGUAGE CPP #-}

module Test.Unit.UserAddedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.QuickCheck

-- Simple standalone QuickCheck tests
tests :: TestTree
tests = testGroup "User Added QuickCheck Tests"
  [ testProperty "addition commutes" prop_addition_commutative
  , testProperty "addition associates" prop_addition_associative
  , testProperty "multiplication distributes" prop_multiplication_distributes
  , testProperty "L.reverse is involution" prop_reverse_involution
  , testProperty "sort is idempotent" prop_sort_idempotent
  , testProperty "L.length of L.reverse" prop_length_reverse
  , testProperty "L.sum of concatenation" prop_sum_concat
  , testProperty "map fusion" prop_map_fusion
  , testProperty "filter composition" prop_filter_composition
  , testProperty "foldr consistency" prop_foldr_consistency
  ]

prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative x y = x + y == y + x

prop_addition_associative :: Int -> Int -> Int -> Bool
prop_addition_associative x y z = (x + y) + z == x + (y + z)

prop_multiplication_distributes :: Int -> Int -> Int -> Bool
prop_multiplication_distributes x y z = x * (y + z) == x * y + x * z

prop_reverse_involution :: [Int] -> Bool
prop_reverse_involution xs = L.reverse (L.reverse xs) == xs

prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort xs == sort (sort xs)
  where
    sort = Data.List.sort

prop_length_reverse :: [Int] -> Bool
prop_length_reverse xs = L.length xs == L.length (L.reverse xs)

prop_sum_concat :: [Int] -> [Int] -> Bool
prop_sum_concat xs ys = L.sum xs + L.sum ys == L.sum (xs ++ ys)

prop_map_fusion :: [Int] -> Bool
prop_map_fusion xs = L.map (+1) (L.map (*2) xs) == L.map ((+1) . (*2)) xs

prop_filter_composition :: [Int] -> Bool
prop_filter_composition xs = filter even (L.filter (>0) xs) == L.filter (\x -> even x && x > 0) xs

prop_foldr_consistency :: [Int] -> Bool
prop_foldr_consistency xs = L.foldr (:) [] xs == xs