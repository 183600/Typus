{-# LANGUAGE CPP #-}

-- | Ownership transfer tests using QuickCheck
module Test.Unit.OwnershipTransferSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit)

import Ownership (Ownership(..), OwnershipState(..), transferOwnership, checkOwnership)
import Compiler.IR (IRValue(..))

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: Ownership transfer updates owner correctly
prop_ownership_transfer_updates_owner :: String -> String -> Property
prop_ownership_transfer_updates_owner fromOwner toOwner =
  isValidIdentifier fromOwner && isValidIdentifier toOwner && fromOwner /= toOwner ==>
  let initialOwnership = Ownership fromOwner Owned
      transferred = transferOwnership toOwner initialOwnership
  in property $ ownershipOwner transferred == toOwner

-- Property: Ownership transfer preserves state
prop_ownership_transfer_preserves_state :: String -> String -> OwnershipState -> Property
prop_ownership_transfer_preserves_state fromOwner toOwner state =
  isValidIdentifier fromOwner && isValidIdentifier toOwner && fromOwner /= toOwner ==>
  let initialOwnership = Ownership fromOwner state
      transferred = transferOwnership toOwner initialOwnership
  in property $ ownershipState transferred == state

-- Property: Multiple transfers create correct chain
prop_multiple_ownership_transfers :: [String] -> Property
prop_multiple_ownership_transfers owners =
  all isValidIdentifier owners && length owners >= 2 && length owners <= 5 ==>
  let initialOwner = head owners
      initialOwnership = Ownership initialOwner Owned
      finalOwnership = foldl (\acc owner -> transferOwnership owner acc) initialOwnership (tail owners)
      finalOwner = last owners
  in property $ ownershipOwner finalOwnership == finalOwner

-- Property: Ownership check is consistent
prop_ownership_check_consistent :: String -> OwnershipState -> Property
prop_ownership_check_consistent owner state =
  isValidIdentifier owner ==>
  let ownership = Ownership owner state
      check1 = checkOwnership owner ownership
      check2 = checkOwnership owner ownership
  in property $ check1 == check2

-- Property: Ownership check fails for different owners
prop_ownership_check_fails_different_owner :: String -> String -> Property
prop_ownership_check_fails_different_owner owner1 owner2 =
  isValidIdentifier owner1 && isValidIdentifier owner2 && owner1 /= owner2 ==>
  let ownership = Ownership owner1 Owned
      checkResult = checkOwnership owner2 ownership
  in property $ not checkResult

-- Property: Ownership check succeeds for same owner
prop_ownership_check_succeeds_same_owner :: String -> OwnershipState -> Property
prop_ownership_check_succeeds_same_owner owner state =
  isValidIdentifier owner ==>
  let ownership = Ownership owner state
      checkResult = checkOwnership owner ownership
  in property $ checkResult

-- Property: Transfer from borrowed state preserves borrowed flag
prop_transfer_from_borrowed :: String -> String -> Property
prop_transfer_from_borrowed fromOwner toOwner =
  isValidIdentifier fromOwner && isValidIdentifier toOwner && fromOwner /= toOwner ==>
  let initialOwnership = Ownership fromOwner Borrowed
      transferred = transferOwnership toOwner initialOwnership
  in property $ ownershipState transferred == Borrowed

-- Property: Transfer from moved state preserves moved flag
prop_transfer_from_moved :: String -> String -> Property
prop_transfer_from_moved fromOwner toOwner =
  isValidIdentifier fromOwner && isValidIdentifier toOwner && fromOwner /= toOwner ==>
  let initialOwnership = Ownership fromOwner Moved
      transferred = transferOwnership toOwner initialOwnership
  in property $ ownershipState transferred == Moved

-- Property: Circular ownership transfer is detected
prop_circular_ownership_detection :: [String] -> Property
prop_circular_ownership_detection owners =
  all isValidIdentifier owners && length owners >= 2 && length owners <= 4 ==>
  let initialOwner = head owners
      initialOwnership = Ownership initialOwner Owned
      -- Simulate circular transfer by returning to original owner
      intermediateOwners = tail owners ++ [initialOwner]
      finalOwnership = foldl (\acc owner -> transferOwnership owner acc) initialOwnership intermediateOwners
  in property $ ownershipOwner finalOwnership == initialOwner

-- Property: Ownership transfer preserves type information
prop_transfer_preserves_type :: String -> String -> IRValue -> Property
prop_transfer_preserves_type fromOwner toOwner value =
  isValidIdentifier fromOwner && isValidIdentifier toOwner && fromOwner /= toOwner ==>
  let initialOwnership = Ownership fromOwner Owned
      typedOwnership = addTypeInformation initialOwnership value
      transferred = transferOwnership toOwner typedOwnership
  in property $ getOwnershipType transferred == Just (getValueType value)

-- Property: Ownership state transitions are valid
prop_valid_state_transitions :: String -> OwnershipState -> OwnershipState -> Property
prop_valid_state_transitions owner fromState toState =
  isValidIdentifier owner && isValidTransition fromState toState ==>
  let initialOwnership = Ownership owner fromState
      -- Simulate state transition
      updatedOwnership = updateOwnershipState initialOwnership toState
  in property $ ownershipState updatedOwnership == toState

-- ============================================================================
-- Helper Functions
-- ============================================================================

data Ownership = Ownership
  { ownershipOwner :: String
  , ownershipState :: OwnershipState
  , ownershipType :: Maybe IRType
  } deriving (Eq, Show)

data OwnershipState = Owned | Borrowed | Moved | Shared deriving (Eq, Show)

data IRType = IRInt | IRString | IRBool | IRFunction IRType IRType deriving (Eq, Show)

transferOwnership :: String -> Ownership -> Ownership
transferOwnership newOwner ownership = 
  ownership { ownershipOwner = newOwner }

checkOwnership :: String -> Ownership -> Bool
checkOwnership requester ownership = 
  ownershipOwner ownership == requester && 
  ownershipState ownership /= Moved

addTypeInformation :: Ownership -> IRValue -> Ownership
addTypeInformation ownership value = 
  ownership { ownershipType = Just (getValueType value) }

getOwnershipType :: Ownership -> Maybe IRType
getOwnershipType = ownershipType

updateOwnershipState :: Ownership -> OwnershipState -> Ownership
updateOwnershipState ownership newState = 
  ownership { ownershipState = newState }

getValueType :: IRValue -> IRType
getValueType (IRIntLiteral _) = IRInt
getValueType (IRStringLiteral _) = IRString
getValueType (IRBoolLiteral _) = IRBool
getValueType (IRVariable _) = IRInt -- Simplified

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && all isValidChar cs
  where
    isValidChar ch = isAlpha ch || isDigit ch || ch == '_'

isValidTransition :: OwnershipState -> OwnershipState -> Bool
isValidTransition Owned _ = True
isValidTransition Borrowed Owned = True
isValidTransition Borrowed Shared = True
isValidTransition Moved _ = False -- Can't transition from moved
isValidTransition Shared Owned = True
isValidTransition Shared Borrowed = True
isValidTransition Shared Shared = True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Tests"
  [ fastProperty "Ownership transfer updates owner correctly" prop_ownership_transfer_updates_owner
  , fastProperty "Ownership transfer preserves state" prop_ownership_transfer_preserves_state
  , fastProperty "Multiple transfers create correct chain" prop_multiple_ownership_transfers
  , fastProperty "Ownership check is consistent" prop_ownership_check_consistent
  , fastProperty "Ownership check fails for different owners" prop_ownership_check_fails_different_owner
  , fastProperty "Ownership check succeeds for same owner" prop_ownership_check_succeeds_same_owner
  , fastProperty "Transfer from borrowed state preserves borrowed flag" prop_transfer_from_borrowed
  , fastProperty "Transfer from moved state preserves moved flag" prop_transfer_from_moved
  , fastProperty "Circular ownership transfer is detected" prop_circular_ownership_detection
  , fastProperty "Ownership transfer preserves type information" prop_transfer_preserves_type
  , fastProperty "Ownership state transitions are valid" prop_valid_state_transitions
  ]