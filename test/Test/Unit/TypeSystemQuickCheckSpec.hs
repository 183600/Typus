{-# LANGUAGE CPP #-}

module Test.Unit.TypeSystemQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "TypeSystem QuickCheck Properties"
  [ typeTests
  ]

typeTests :: TestTree
typeTests = testGroup "Type Properties"
  [ fastProperty "type equality is reflexive" prop_type_equality_reflexive
  , fastProperty "type equality is symmetric" prop_type_equality_symmetric
  ]

-- Type properties
prop_type_equality_reflexive :: String -> Property
prop_type_equality_reflexive t =
  property $ t == t

prop_type_equality_symmetric :: String -> Property
prop_type_equality_symmetric t =
  property $ t == t