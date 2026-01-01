{-# LANGUAGE CPP #-}

module Test.Unit.MapPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map

prop_insert_lookup :: Int -> String -> Map.Map Int String -> Property
prop_insert_lookup k v m =
  Map.lookup k (Map.insert k v m) === Just v

prop_delete_lookup :: Int -> Map.Map Int String -> Property
prop_delete_lookup k m =
  Map.lookup k (Map.delete k m) === Nothing

prop_size_insert :: Int -> String -> Map.Map Int String -> Property
prop_size_insert k v m =
  let oldSize = Map.size m
      newSize = Map.size (Map.insert k v m)
  in if Map.member k m
     then newSize === oldSize
     else newSize === oldSize + 1

prop_size_delete :: Int -> Map.Map Int String -> Property
prop_size_delete k m =
  let oldSize = Map.size m
      newSize = Map.size (Map.delete k m)
  in if Map.member k m
     then newSize === oldSize - 1
     else newSize === oldSize

prop_union_size :: Map.Map Int String -> Map.Map Int String -> Property
prop_union_size m1 m2 =
  let unionSize = Map.size (Map.union m1 m2)
  in property $ unionSize <= Map.size m1 + Map.size m2

prop_keys_values_match :: Map.Map Int String -> Property
prop_keys_values_match m =
  L.length (Map.keys m) === L.length (Map.elems m)

tests :: TestTree
tests = testGroup "Map Properties QuickCheck"
  [ fastProperty "insert then lookup" prop_insert_lookup
  , fastProperty "delete then lookup" prop_delete_lookup
  , fastProperty "size after insert" prop_size_insert
  , fastProperty "size after delete" prop_size_delete
  , fastProperty "union size bound" prop_union_size
  , fastProperty "keys L.and values match" prop_keys_values_match
  ]
