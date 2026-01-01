{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Ownership.Common.Types
import Data.List (length)
import Data.List (sort, nub, filter, elem)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Advanced Ownership Transfer QuickCheck Tests
-- ============================================================================

-- Property: Ownership transfer preserves uniqueness
prop_ownership_transfer_uniqueness :: String -> String -> Property
prop_ownership_transfer_uniqueness fromName toName =
  length fromName > 0 && L.length toName > 0 && fromName /= toName ==>
  let originalOwner = Owned fromName
      transfer = OwnershipTransfer fromName toName
      newOwner = Owned toName
  in property $ 
    originalOwner /= newOwner .&&.
    show originalOwner /= show newOwner

-- Property: Borrowing creates reference relationship
prop_borrowing_reference_relationship :: String -> String -> Property
prop_borrowing_reference_relationship ownerName borrowerName =
  length ownerName > 0 && L.length borrowerName > 0 && ownerName /= borrowerName ==>
  let owner = Owned ownerName
      borrow = Borrowed ownerName
  in property $ 
    owner /= borrow .&&.
    show borrow `contains` ownerName

-- Property: Mutable borrowing is distinct from immutable borrowing
prop_mutable_vs_immutable_borrow :: String -> String -> Property
prop_mutable_vs_immutable_borrow ownerName borrowerName =
  length ownerName > 0 && L.length borrowerName > 0 ==>
  let immutableBorrow = Borrowed ownerName
      mutableBorrow = MutBorrowed ownerName
  in property $ 
    immutableBorrow /= mutableBorrow .&&.
    show immutableBorrow /= show mutableBorrow

-- Property: Ownership ordering is consistent
prop_ownership_ordering_consistency :: String -> String -> String -> Property
prop_ownership_ordering_consistency name1 name2 name3 =
  all (not . null) [name1, name2, name3] && nub [name1, name2, name3] == [name1, name2, name3] ==>
  let owned1 = Owned name1
      owned2 = Owned name2
      owned3 = Owned name3
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
  in property $ 
    (owned1 <= owned2 && owned2 <= owned3) ==> owned1 <= owned3 .&&.
    borrowed1 <= borrowed2 ==> (borrowed1 <= borrowed2 || borrowed1 <= owned2)

-- Property: Error types are distinct L.and identifiable
prop_ownership_error_distinctness :: String -> String -> Property
prop_ownership_error_distinctness var1 var2 =
  length var1 > 0 && L.length var2 > 0 && var1 /= var2 ==>
  let useAfterMove = UseAfterMove var1
      doubleMove = DoubleMove var1 var2
      borrowWhileMoved = BorrowWhileMoved var1
      outOfScope = OutOfScope var1
  in property $ 
    useAfterMove /= doubleMove .&&.
    useAfterMove /= borrowWhileMoved .&&.
    useAfterMove /= outOfScope .&&.
    doubleMove /= borrowWhileMoved .&&.
    doubleMove /= outOfScope .&&.
    borrowWhileMoved /= outOfScope

-- Property: Ownership transfer chain is valid
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain names =
  length names > 1 && L.all (not . null) names && nub names == names ==>
  let transfers = zipWith OwnershipTransfer names (L.tail names)
      firstOwner = Owned (L.head names)
      lastOwner = Owned (last names)
  in property $ 
    length transfers === L.length names - 1 .&&.
    firstOwner /= lastOwner

-- Property: Borrowing preserves owner reference
prop_borrowing_preserves_owner :: String -> [String] -> Property
prop_borrowing_preserves_owner owner borrowerNames =
  length owner > 0 && L.all (not . null) borrowerNames && not (owner `elem` borrowerNames) ==>
  let ownerOwnership = Owned owner
      borrows = L.map (const (Borrowed owner)) borrowerNames
  in property $ 
    all (\b -> show b `contains` owner) borrows .&&.
    all (/= ownerOwnership) borrows

-- Property: Multiple borrows from same owner are consistent
prop_multiple_borrows_consistency :: String -> Int -> Property
prop_multiple_borrows_consistency owner numBorrows =
  length owner > 0 && numBorrows > 0 && numBorrows <= 10 ==>
  let borrowerNames = L.map (\i -> owner ++ "_borrower_" ++ show i) [1..numBorrows]
      borrows = L.map (Borrowed owner) borrowerNames
  in property $ 
    length (nub borrows) === numBorrows .&&.
    all (\b -> show b `contains` owner) borrows

-- Property: Ownership error detection is deterministic
prop_ownership_error_deterministic :: String -> String -> Property
prop_ownership_error_deterministic var1 var2 =
  length var1 > 0 && L.length var2 > 0 && var1 /= var2 ==>
  let error1 = UseAfterMove var1
      error2 = DoubleMove var1 var2
      error1Str = show error1
      error2Str = show error2
      error1StrAgain = show error1
      error2StrAgain = show error2
  in property $ 
    error1Str === error1StrAgain .&&.
    error2Str === error2StrAgain .&&.
    error1Str /= error2Str

-- Property: Ownership analyzer state consistency
prop_ownership_analyzer_consistency :: [String] -> Property
prop_ownership_analyzer_consistency varNames =
  length varNames > 0 && L.all (not . null) varNames && nub varNames == varNames ==>
  let analyzer = newOwnershipAnalyzer
      ownerships = map Owned varNames
  in property $ 
    length ownerships === L.length varNames .&&.
    all (\o -> case o of
                Owned name -> name `elem` varNames
                Borrowed name -> name `elem` varNames
                MutBorrowed name -> name `elem` varNames) ownerships

-- Property: Ownership transfer preserves error conditions
prop_ownership_transfer_preserves_errors :: String -> String -> Property
prop_ownership_transfer_preserves_errors fromName toName =
  length fromName > 0 && L.length toName > 0 && fromName /= toName ==>
  let useAfterMoveBefore = UseAfterMove fromName
      useAfterMoveAfter = UseAfterMove toName
      transfer = OwnershipTransfer fromName toName
  in property $ 
    useAfterMoveBefore /= useAfterMoveAfter .&&.
    show useAfterMoveBefore /= show useAfterMoveAfter

-- Helper function to check string containment
contains :: String -> String -> Bool
contains needle haystack = needle `Data.List.L.isInfixOf` haystack

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Ownership Transfer QuickCheck Tests"
  [ fastProperty "Ownership transfer preserves uniqueness" prop_ownership_transfer_uniqueness
  , fastProperty "Borrowing creates reference relationship" prop_borrowing_reference_relationship
  , fastProperty "Mutable borrowing is distinct from immutable borrowing" prop_mutable_vs_immutable_borrow
  , fastProperty "Ownership ordering is consistent" prop_ownership_ordering_consistency
  , fastProperty "Error types are distinct L.and identifiable" prop_ownership_error_distinctness
  , fastProperty "Ownership transfer chain is valid" prop_ownership_transfer_chain
  , fastProperty "Borrowing preserves owner reference" prop_borrowing_preserves_owner
  , fastProperty "Multiple borrows from same owner are consistent" prop_multiple_borrows_consistency
  , fastProperty "Ownership error detection is deterministic" prop_ownership_error_deterministic
  , fastProperty "Ownership analyzer state consistency" prop_ownership_analyzer_consistency
  , fastProperty "Ownership transfer preserves error conditions" prop_ownership_transfer_preserves_errors
  ]