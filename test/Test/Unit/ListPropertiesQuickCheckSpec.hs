{-# LANGUAGE CPP #-}

module Test.Unit.ListPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

prop_reverse_involutive :: [Int] -> Property
prop_reverse_involutive xs =
  reverse (reverse xs) === xs

prop_length_append :: [Int] -> [Int] -> Property
prop_length_append xs ys =
  length (xs ++ ys) === length xs + length ys

prop_map_composition :: Fun Int Int -> Fun Int Int -> [Int] -> Property
prop_map_composition (Fun _ f) (Fun _ g) xs =
  map (f . g) xs === map f (map g xs)

prop_filter_idempotent :: Fun Int Bool -> [Int] -> Property
prop_filter_idempotent (Fun _ p) xs =
  let filtered = filter p xs
  in filter p filtered === filtered

prop_concat_flatten :: [[Int]] -> Property
prop_concat_flatten xss =
  length (concat xss) === sum (map length xss)

prop_take_drop :: NonNegative Int -> [Int] -> Property
prop_take_drop (NonNegative n) xs =
  take n xs ++ drop n xs === xs

tests :: TestTree
tests = testGroup "List Properties QuickCheck"
  [ fastProperty "reverse is involutive" prop_reverse_involutive
  , fastProperty "length of append" prop_length_append
  , fastProperty "map composition" prop_map_composition
  , fastProperty "filter is idempotent" prop_filter_idempotent
  , fastProperty "concat flattens correctly" prop_concat_flatten
  , fastProperty "take and drop reconstruct" prop_take_drop
  ]
