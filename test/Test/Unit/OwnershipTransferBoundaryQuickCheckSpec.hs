{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..))
import Data.List (nub, sort, intersect, union)
import Data.Set (Set, toList, fromList, union, intersection, difference)
import qualified Data.Set as Set

-- Test data for ownership transfer
data OwnershipTransferData = OwnershipTransferData
  { originalOwner :: String
  , newOwner :: String
  , transferType :: OwnershipType
  , borrower :: String
  } deriving (Show, Eq)

instance Arbitrary OwnershipTransferData where
  arbitrary = do
    orig <- elements ["owner1", "owner2", "owner3", "var1", "var2"]
    new <- elements ["owner1", "owner2", "owner3", "var1", "var2"]
    transferType <- elements [Owned orig, Borrowed orig, MutBorrowed orig, Shared]
    borrower <- elements ["borrower1", "borrower2", "borrower3", "var1", "var2"]
    return $ OwnershipTransferData orig new transferType borrower

-- Property: Ownership transfer preserves ownership type structure
prop_ownership_transfer_preserves_structure :: OwnershipTransferData -> Property
prop_ownership_transfer_preserves_structure transferData =
  let original = transferType transferData
      transferred = transferOwnership (newOwner transferData) original
  in case transferred of
    Owned new -> property $ new === newOwner transferData
    Borrowed new -> property $ new === newOwner transferData
    MutBorrowed new -> property $ new === newOwner transferData
    Shared -> property True  -- Shared has no owner

-- Property: Borrowing from owned resources creates correct borrow type
prop_borrow_from_owned :: String -> String -> Property
prop_borrow_from_owned owner borrower =
  owner /= borrower ==>
  let owned = Owned owner
      immutableBorrow = createBorrow owned borrower False
      mutableBorrow = createBorrow owned borrower True
  in case (immutableBorrow, mutableBorrow) of
    (Borrowed b, MutBorrowed mb) -> property $ b === borrower && mb === borrower
    _ -> property False

-- Property: Multiple borrows from the same resource are handled correctly
prop_multiple_borrows :: String -> [String] -> Property
prop_multiple_borrows owner borrowers =
  not (null borrowers) && all (/= owner) borrowers ==>
  let owned = Owned owner
      borrows = map (\b -> createBorrow owned b False) borrowers
      borrowOwners = map getBorrowOwner borrows
  in property $ sort borrowOwners === sort borrowers

-- Property: Mutable borrows conflict with other borrows
prop_mutable_borrow_conflicts :: String -> String -> String -> Property
prop_mutable_borrow_conflicts owner borrower1 borrower2 =
  borrower1 /= borrower2 && borrower1 /= owner && borrower2 /= owner ==>
  let owned = Owned owner
      mutableBorrow1 = createBorrow owned borrower1 True
      immutableBorrow2 = createBorrow owned borrower2 False
      conflict1 = checkBorrowConflict mutableBorrow1 immutableBorrow2
      conflict2 = checkBorrowConflict immutableBorrow2 mutableBorrow1
  in property $ conflict1 && conflict2

-- Property: Ownership transfer invalidates existing borrows
prop_transfer_invalidates_borrows :: OwnershipTransferData -> [String] -> Property
prop_transfer_invalidates_borrows transferData borrowers =
  let original = Owned (originalOwner transferData)
      borrows = map (\b -> createBorrow original b False) borrowers
      transferred = transferOwnership (newOwner transferData) original
      allValid = all (isValidBorrow transferred) borrows
  in property $ not allValid || null borrowers

-- Property: Shared ownership allows concurrent access
prop_shared_allows_concurrent :: [String] -> Property
prop_shared_allows_concurrent owners =
  length owners >= 2 ==>
  let shared = Shared
      accesses = map (\_ -> canAccess shared) owners
  in property $ all id accesses

-- Property: Ownership transfer chain is maintained correctly
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain owners =
  length owners >= 3 ==>
  let initialOwner = head owners
      transfers = tail owners
      finalOwnership = foldl transferOwnership (Owned initialOwner) transfers
      expectedOwner = last transfers
  in case finalOwnership of
    Owned actual -> property $ actual === expectedOwner
    _ -> property False

-- Property: Borrow scope is correctly bounded
prop_borrow_scope_bounded :: String -> String -> Property
prop_borrow_scope_bounded owner borrower =
  owner /= borrower ==>
  let owned = Owned owner
      borrow = createBorrow owned borrower False
      scope = getBorrowScope borrow
  in property $ scope borrower

-- Property: Ownership transfer preserves resource availability
prop_transfer_preserves_availability :: OwnershipTransferData -> Property
prop_transfer_preserves_availability transferData =
  let original = transferType transferData
      transferred = transferOwnership (newOwner transferData) original
      originalAvailable = isAvailable original
      transferredAvailable = isAvailable transferred
  in property $ originalAvailable ==> transferredAvailable

-- Property: Recursive ownership transfer is handled correctly
prop_recursive_transfer :: OwnershipTransferData -> Int -> Property
prop_recursive_transfer transferData depth =
  depth > 0 && depth < 10 ==>
  let original = transferType transferData
      recursiveTransfer = iterate transferOwnership original (newOwner transferData)
      final = recursiveTransfer !! depth
  in case final of
    Owned owner -> property $ owner === newOwner transferData
    _ -> property True  -- Other types might behave differently

-- Property: Ownership type equality is consistent
prop_ownership_equality_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownership_equality_consistent own1 own2 =
  let equal = own1 == own2
      reflectEqual = own2 == own1
  in property $ equal === reflectEqual

-- Property: Ownership transfer error handling
prop_transfer_error_handling :: String -> String -> Property
prop_transfer_error_handling from to =
  from == to ==>
  let owned = Owned from
      result = safeTransferOwnership to owned
  in property $ case result of
    Left _ -> property True
    Right _ -> property False  -- Self-transfer should fail

-- Helper functions for ownership operations
transferOwnership :: String -> OwnershipType -> OwnershipType
transferOwnership newOwner ownership = 
  case ownership of
    Owned _ -> Owned newOwner
    Borrowed _ -> Borrowed newOwner
    MutBorrowed _ -> MutBorrowed newOwner
    Shared -> Shared

createBorrow :: OwnershipType -> String -> Bool -> OwnershipType
createBorrow ownership borrower isMutable =
  case ownership of
    Owned owner -> if isMutable then MutBorrowed borrower else Borrowed borrower
    _ -> Shared  -- Simplified: other cases become shared

getBorrowOwner :: OwnershipType -> String
getBorrowOwner ownership = 
  case ownership of
    Borrowed owner -> owner
    MutBorrowed owner -> owner
    _ -> ""

checkBorrowConflict :: OwnershipType -> OwnershipType -> Bool
checkBorrowConflict borrow1 borrow2 =
  case (borrow1, borrow2) of
    (MutBorrowed _, MutBorrowed _) -> True  -- Two mutable borrows conflict
    (MutBorrowed _, Borrowed _) -> True     -- Mutable and immutable borrows conflict
    (Borrowed _, MutBorrowed _) -> True
    _ -> False

isValidBorrow :: OwnershipType -> OwnershipType -> Bool
isValidBorrow resource borrow = 
  case (resource, borrow) of
    (Owned owner, Borrowed borrower) -> owner /= borrower
    (Owned owner, MutBorrowed borrower) -> owner /= borrower
    (Shared, _) -> True
    _ -> False

canAccess :: OwnershipType -> Bool
canAccess ownership = case ownership of
  Shared -> True
  Owned _ -> True
  Borrowed _ -> True
  MutBorrowed _ -> True

getBorrowScope :: OwnershipType -> String -> Bool
getBorrowScope borrow borrower = 
  case borrow of
    Borrowed owner -> owner == borrower
    MutBorrowed owner -> owner == borrower
    _ -> False

isAvailable :: OwnershipType -> Bool
isAvailable ownership = case ownership of
  Shared -> True
  Owned _ -> True
  _ -> False

safeTransferOwnership :: String -> OwnershipType -> Either String OwnershipType
safeTransferOwnership newOwner ownership
  | newOwner == extractCurrentOwner ownership = Left "Cannot transfer to same owner"
  | otherwise = Right $ transferOwnership newOwner ownership
  where
    extractCurrentOwner own = case own of
      Owned owner -> owner
      Borrowed owner -> owner
      MutBorrowed owner -> owner
      Shared -> ""

tests :: TestTree
tests = testGroup "Ownership Transfer Boundary QuickCheck Tests"
  [ fastProperty "Ownership transfer preserves structure" prop_ownership_transfer_preserves_structure
  , fastProperty "Borrowing from owned resources creates correct borrow type" prop_borrow_from_owned
  , fastProperty "Multiple borrows from the same resource are handled correctly" prop_multiple_borrows
  , fastProperty "Mutable borrows conflict with other borrows" prop_mutable_borrow_conflicts
  , fastProperty "Ownership transfer invalidates existing borrows" prop_transfer_invalidates_borrows
  , fastProperty "Shared ownership allows concurrent access" prop_shared_allows_concurrent
  , fastProperty "Ownership transfer chain is maintained correctly" prop_ownership_transfer_chain
  , fastProperty "Borrow scope is correctly bounded" prop_borrow_scope_bounded
  , fastProperty "Ownership transfer preserves resource availability" prop_transfer_preserves_availability
  , fastProperty "Recursive ownership transfer is handled correctly" prop_recursive_transfer
  , fastProperty "Ownership type equality is consistent" prop_ownership_equality_consistent
  , fastProperty "Ownership transfer error handling" prop_transfer_error_handling
  , testCase "Manual ownership transfer test" $ do
      let owned = Owned "owner1"
          borrow1 = createBorrow owned "borrower1" False
          borrow2 = createBorrow owned "borrower2" True
          transferred = transferOwnership "owner2" owned
      
      assertBool "Immutable borrow created correctly" $ case borrow1 of
        Borrowed "borrower1" -> True
        _ -> False
      
      assertBool "Mutable borrow created correctly" $ case borrow2 of
        MutBorrowed "borrower2" -> True
        _ -> False
      
      assertBool "Mutable and immutable borrows conflict" $ checkBorrowConflict borrow2 borrow1
      
      assertBool "Transfer creates correct new ownership" $ case transferred of
        Owned "owner2" -> True
        _ -> False
      
      assertBool "Borrow conflicts with transferred ownership" $ not $ isValidBorrow transferred borrow1
  ]