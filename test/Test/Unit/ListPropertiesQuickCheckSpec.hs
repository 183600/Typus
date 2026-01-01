{-# LANGUAGE CPP #-}

module Test.Unit.ListPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

prop_reverse_involutive :: [Int] -> Property
prop_reverse_involutive xs =
  L.reverse (L.reverse xs) === xs

prop_length_append :: [Int] -> [Int] -> Property
prop_length_append xs ys =
  L.length (xs ++ ys) === L.length xs + L.length ys

prop_map_composition :: Fun Int Int -> Fun Int Int -> [Int] -> Property
prop_map_composition (Fun _ f) (Fun _ g) xs =
  L.map (f . g) xs === map f (map g xs)

prop_filter_idempotent :: Fun Int Bool -> [Int] -> Property
prop_filter_idempotent (Fun _ p) xs =
  let filtered = filter p xs
  in filter p filtered === filtered

prop_concat_flatten :: [[Int]] -> Property
prop_concat_flatten xss =
  L.length (L.concat xss) === L.sum (map L.length xss)

prop_take_drop :: NonNegative Int -> [Int] -> Property
prop_take_drop (NonNegative n) xs =
  take n xs ++ drop n xs === xs

tests :: TestTree
tests = testGroup "List Properties QuickCheck"
  [ fastProperty "L.reverse is involutive" prop_reverse_involutive
  , fastProperty "L.length of append" prop_length_append
  , fastProperty "map composition" prop_map_composition
  , fastProperty "filter is idempotent" prop_filter_idempotent
  , fastProperty "L.concat flattens correctly" prop_concat_flatten
  , fastProperty "take L.and drop reconstruct" prop_take_drop
  ]
