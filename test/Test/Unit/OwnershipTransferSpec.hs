{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipTransferSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Ownership (OwnershipInfo(..), TransferResult(..), transferOwnership, canTransfer, checkOwnership)
import Ownership.Common.Types (Resource, Owner, TransferType(..))

-- | Test suite for Ownership Transfer
tests :: TestTree
tests = testGroup "Ownership Transfer"
  [ testProperty "ownership transfer preserves resource" propOwnershipTransferPreservesResource
  , testProperty "ownership cannot be transferred twice" propOwnershipCannotBeTransferredTwice
  , testProperty "ownership transfer creates proper result" propOwnershipTransferCreatesProperResult
  , testProperty "ownership check works correctly" propOwnershipCheckWorksCorrectly
  , testProperty "borrowing doesn't transfer ownership" propBorrowingDoesntTransferOwnership
  , testCase "simple ownership transfer" testSimpleOwnershipTransfer
  , testCase "ownership transfer with borrowing" testOwnershipTransferWithBorrowing
  , testCase "ownership transfer failure" testOwnershipTransferFailure
  , testCase "ownership transfer chain" testOwnershipTransferChain
  , testCase "ownership transfer validation" testOwnershipTransferValidation
  ]

-- | Property: ownership transfer preserves resource
propOwnershipTransferPreservesResource :: Resource -> Owner -> Owner -> Property
propOwnershipTransferPreservesResource resource fromOwner toOwner =
  let ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      result = transferOwnership ownershipInfo toOwner Move
  in case result of
    Success newInfo -> property $ resource (resource newInfo) == resource
    Failure _ -> property $ False

-- | Property: ownership cannot be transferred twice
propOwnershipCannotBeTransferredTwice :: Resource -> Owner -> Owner -> Owner -> Property
propOwnershipCannotBeTransferredTwice resource owner1 owner2 owner3 =
  let ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = owner1
        , isBorrowed = False
        }
      result1 = transferOwnership ownershipInfo owner2 Move
  in case result1 of
    Success newInfo -> 
      let result2 = transferOwnership newInfo owner3 Move
      in property $ isFailure result2
    Failure _ -> property $ False

-- | Property: ownership transfer creates proper result
propOwnershipTransferCreatesProperResult :: Resource -> Owner -> Owner -> TransferType -> Property
propOwnershipTransferCreatesProperResult resource fromOwner toOwner transferType =
  let ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      result = transferOwnership ownershipInfo toOwner transferType
  in case result of
    Success newInfo -> property $ owner newInfo == toOwner &&
                                 (transferType == Borrow || isBorrowed newInfo == False)
    Failure _ -> property $ True

-- | Property: ownership check works correctly
propOwnershipCheckWorksCorrectly :: Resource -> Owner -> Owner -> Property
propOwnershipCheckWorksCorrectly resource owner1 owner2 =
  let ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = owner1
        , isBorrowed = False
        }
      canAccess = checkOwnership ownershipInfo owner2
  in property $ canAccess == (owner1 == owner2)

-- | Property: borrowing doesn't transfer ownership
propBorrowingDoesntTransferOwnership :: Resource -> Owner -> Owner -> Property
propBorrowingDoesntTransferOwnership resource fromOwner toOwner =
  let ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      result = transferOwnership ownershipInfo toOwner Borrow
  in case result of
    Success newInfo -> property $ owner newInfo == fromOwner && isBorrowed newInfo
    Failure _ -> property $ False

-- | Unit tests for simple ownership transfer
testSimpleOwnershipTransfer :: IO ()
testSimpleOwnershipTransfer = do
  let resource = "test_resource"
      fromOwner = "owner1"
      toOwner = "owner2"
      ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      result = transferOwnership ownershipInfo toOwner Move
  case result of
    Success newInfo -> do
      assertEqual "new owner" toOwner $ owner newInfo
      assertEqual "resource unchanged" resource $ resource newInfo
      assertBool "not borrowed" $ not $ isBorrowed newInfo
    Failure _ -> assertFailure "Expected successful transfer"

-- | Unit tests for ownership transfer with borrowing
testOwnershipTransferWithBorrowing :: IO ()
testOwnershipTransferWithBorrowing = do
  let resource = "test_resource"
      fromOwner = "owner1"
      borrower = "borrower"
      ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      result = transferOwnership ownershipInfo borrower Borrow
  case result of
    Success newInfo -> do
      assertEqual "owner unchanged" fromOwner $ owner newInfo
      assertEqual "resource unchanged" resource $ resource newInfo
      assertBool "is borrowed" $ isBorrowed newInfo
    Failure _ -> assertFailure "Expected successful borrow"

-- | Unit tests for ownership transfer failure
testOwnershipTransferFailure :: IO ()
testOwnershipTransferFailure = do
  let resource = "test_resource"
      fromOwner = "owner1"
      toOwner = "owner2"
      ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = True  -- Already borrowed
        }
      result = transferOwnership ownershipInfo toOwner Move
  case result of
    Success _ -> assertFailure "Expected transfer failure"
    Failure _ -> return ()

-- | Unit tests for ownership transfer chain
testOwnershipTransferChain :: IO ()
testOwnershipTransferChain = do
  let resource = "test_resource"
      owner1 = "owner1"
      owner2 = "owner2"
      owner3 = "owner3"
      ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = owner1
        , isBorrowed = False
        }
      result1 = transferOwnership ownershipInfo owner2 Move
  case result1 of
    Success newInfo1 -> do
      let result2 = transferOwnership newInfo1 owner3 Move
      case result2 of
        Success newInfo2 -> do
          assertEqual "final owner" owner3 $ owner newInfo2
          assertEqual "resource unchanged" resource $ resource newInfo2
        Failure _ -> assertFailure "Second transfer should succeed"
    Failure _ -> assertFailure "First transfer should succeed"

-- | Unit tests for ownership transfer validation
testOwnershipTransferValidation :: IO ()
testOwnershipTransferValidation = do
  let resource = "test_resource"
      fromOwner = "owner1"
      toOwner = "owner2"
      ownershipInfo = OwnershipInfo
        { resource = resource
        , owner = fromOwner
        , isBorrowed = False
        }
      canTransferBefore = canTransfer ownershipInfo fromOwner toOwner Move
      result = transferOwnership ownershipInfo toOwner Move
  case result of
    Success newInfo -> do
      assertBool "can transfer before" canTransferBefore
      assertBool "cannot transfer again" $ not $ canTransfer newInfo fromOwner toOwner Move
    Failure _ -> assertFailure "Transfer should succeed"

-- Helper functions and types
type Resource = String
type Owner = String

data TransferType = Move | Borrow deriving (Show, Eq)

data OwnershipInfo = OwnershipInfo
  { resource :: Resource
  , owner :: Owner
  , isBorrowed :: Bool
  } deriving (Show, Eq)

data TransferResult = Success OwnershipInfo | Failure String deriving (Show, Eq)

-- Mock functions
transferOwnership :: OwnershipInfo -> Owner -> TransferType -> TransferResult
transferOwnership info newOwner transferType
  | isBorrowed info && transferType == Move = Failure "Cannot move borrowed resource"
  | transferType == Borrow = Success info { isBorrowed = True }
  | otherwise = Success info { owner = newOwner, isBorrowed = False }

canTransfer :: OwnershipInfo -> Owner -> TransferType -> Bool
canTransfer info fromOwner transferType =
  not (isBorrowed info && transferType == Move) &&
  owner info == fromOwner

checkOwnership :: OwnershipInfo -> Owner -> Bool
checkOwnership info checkOwner = owner info == checkOwner

isFailure :: TransferResult -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False

-- Helper function for property testing
property :: Bool -> Property
property = id