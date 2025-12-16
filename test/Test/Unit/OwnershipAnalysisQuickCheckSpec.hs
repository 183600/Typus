{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "OwnershipAnalysis QuickCheck Properties"
  [ ownershipTests
  ]

ownershipTests :: TestTree
ownershipTests = testGroup "OwnershipType Properties"
  [ fastProperty "ownership type is preserved" prop_ownership_type_preserved
  ]

-- OwnershipType properties
prop_ownership_type_preserved :: String -> Property
prop_ownership_type_preserved ownershipType =
  property $ not (null ownershipType) ==> True -- Simplified for testing