{-# LANGUAGE CPP #-}

module Test.Unit.SetPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Set as Set

prop_insert_member :: Int -> Set.Set Int -> Property
prop_insert_member x s =
  Set.member x (Set.insert x s) === True

prop_delete_not_member :: Int -> Set.Set Int -> Property
prop_delete_not_member x s =
  Set.member x (Set.delete x s) === False

prop_union_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_union_commutative s1 s2 =
  Set.union s1 s2 === Set.union s2 s1

prop_intersection_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_intersection_commutative s1 s2 =
  Set.intersection s1 s2 === Set.intersection s2 s1

prop_difference_size :: Set.Set Int -> Set.Set Int -> Property
prop_difference_size s1 s2 =
  property $ Set.size (Set.difference s1 s2) <= Set.size s1

prop_subset_reflexive :: Set.Set Int -> Property
prop_subset_reflexive s =
  Set.isSubsetOf s s === True

tests :: TestTree
tests = testGroup "Set Properties QuickCheck"
  [ fastProperty "insert makes member" prop_insert_member
  , fastProperty "delete removes member" prop_delete_not_member
  , fastProperty "union is commutative" prop_union_commutative
  , fastProperty "intersection is commutative" prop_intersection_commutative
  , fastProperty "difference size bound" prop_difference_size
  , fastProperty "subset is reflexive" prop_subset_reflexive
  ]
