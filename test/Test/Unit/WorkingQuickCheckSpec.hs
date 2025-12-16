{-# LANGUAGE CPP #-}

module Test.Unit.WorkingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (sort, nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

tests :: TestTree
tests = testGroup "Working QuickCheck Properties"
  [ basicListTests
  , basicMapTests
  , basicSetTests
  , basicStringTests
  ]

basicListTests :: TestTree
basicListTests = testGroup "Basic List Tests"
  [ fastProperty "sort is idempotent" prop_sort_idempotent
  , fastProperty "sort preserves length" prop_sort_preserves_length
  ]

basicMapTests :: TestTree
basicMapTests = testGroup "Basic Map Tests"
  [ fastProperty "Map lookup after insert" prop_map_insert_lookup
  , fastProperty "Map size increases for new keys" prop_map_insert_size
  ]

basicSetTests :: TestTree
basicSetTests = testGroup "Basic Set Tests"
  [ fastProperty "Set insert preserves element" prop_set_insert_preserves
  , fastProperty "Set size increases for new elements" prop_set_insert_size
  ]

basicStringTests :: TestTree
basicStringTests = testGroup "Basic String Tests"
  [ fastProperty "reverse is involutive" prop_reverse_involutory
  , fastProperty "length of reverse equals length of original" prop_reverse_preserves_length
  ]

-- List properties
prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  sort (sort xs) === sort xs

prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs =
  length (sort xs) === length xs

-- Map properties
prop_map_insert_lookup :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_lookup k v m =
  let m' = Map.insert k v m
  in Map.lookup k m' === Just v

prop_map_insert_size :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_size k v m =
  let m' = Map.insert k v m
      newSize = if Map.member k m then Map.size m else Map.size m + 1
  in Map.size m' === newSize

-- Set properties
prop_set_insert_preserves :: Int -> Set.Set Int -> Property
prop_set_insert_preserves x s =
  let s' = Set.insert x s
  in property $ Set.member x s'

prop_set_insert_size :: Int -> Set.Set Int -> Property
prop_set_insert_size x s =
  let s' = Set.insert x s
      newSize = if Set.member x s then Set.size s else Set.size s + 1
  in Set.size s' === newSize

-- String properties
prop_reverse_involutory :: String -> Property
prop_reverse_involutory s =
  reverse (reverse s) === s

prop_reverse_preserves_length :: String -> Property
prop_reverse_preserves_length s =
  length (reverse s) === length s