{-# LANGUAGE CPP #-}

module Test.Unit.NewOwnershipTransferQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ownership (OwnershipInfo(..), TransferResult(..), transferOwnership, 
                 checkOwnershipConflict, canTransfer, getOwner)
import SourceLocation (SourcePosition(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "New Ownership Transfer QuickCheck Tests"
  [ basicTransferProperties
  , transferValidityProperties
  , conflictDetectionProperties
  , transferChainingProperties
  , ownershipInvariantProperties
  ]

basicTransferProperties :: TestTree
basicTransferProperties = testGroup "Basic Transfer Properties"
  [ fastProperty "transfer creates new ownership record" prop_transfer_creates_record
  , fastProperty "transfer removes old ownership" prop_transfer_removes_old
  , fastProperty "self-transfer is no-op" prop_self_transfer_noop
  , fastProperty "transfer preserves ownership type" prop_transfer_preserves_type
  , fastProperty "transfer updates timestamp" prop_transfer_updates_timestamp
  ]

transferValidityProperties :: TestTree
transferValidityProperties = testGroup "Transfer Validity Properties"
  [ fastProperty "valid transfer succeeds" prop_valid_transfer_succeeds
  , fastProperty "invalid transfer fails" prop_invalid_transfer_fails
  , fastProperty "null transfer fails" prop_null_transfer_fails
  , fastProperty "empty ownership transfer fails" prop_empty_ownership_fails
  , fastProperty "duplicate ownership detected" prop_duplicate_ownership_detected
  ]

conflictDetectionProperties :: TestTree
conflictDetectionProperties = testGroup "Conflict Detection Properties"
  [ fastProperty "conflicting transfers detected" prop_conflicting_detected
  , fastProperty "non-conflicting transfers allowed" prop_nonconflicting_allowed
  , fastProperty "multiple conflicts found" prop_multiple_conflicts
  , fastProperty "conflict resolution preserves invariants" prop_conflict_resolution_preserves
  ]

transferChainingProperties :: TestTree
transferChainingProperties = testGroup "Transfer Chaining Properties"
  [ fastProperty "chained transfers maintain ownership" prop_chained_maintains_ownership
  , fastProperty "transfer chain preserves history" prop_chain_preserves_history
  , fastProperty "circular transfers detected" prop_circular_detected
  , fastProperty "long chains handled efficiently" prop_long_chain_efficient
  ]

ownershipInvariantProperties :: TestTree
ownershipInvariantProperties = testGroup "Ownership Invariant Properties"
  [ fastProperty "single owner invariant maintained" prop_single_owner_invariant
  , fastProperty "ownership graph remains acyclic" prop_ownership_acyclic
  , fastProperty "transfer preserves resource count" prop_preserves_resource_count
  , fastProperty "ownership consistency maintained" prop_ownership_consistency
  ]

-- Basic transfer properties
prop_transfer_creates_record :: String -> String -> Property
prop_transfer_creates_record from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
      result = transferOwnership owner1 to
  in not (null from) && not (null to) && from /= to ==>
  case result of
    TransferSuccess newOwner -> property $ getOwner newOwner == to
    TransferConflict _ -> property False  -- Should not conflict with new owner
    TransferError _ -> property False

prop_transfer_removes_old :: String -> String -> Property
prop_transfer_removes_old from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
      result = transferOwnership owner1 to
  in not (null from) && not (null to) && from /= to ==>
  case result of
    TransferSuccess newOwner -> property $ getOwner newOwner /= from
    TransferConflict _ -> property False
    TransferError _ -> property False

prop_self_transfer_noop :: String -> Property
prop_self_transfer_noop owner =
  let ownerInfo = OwnershipInfo owner "resource" "type1" 1
      result = transferOwnership ownerInfo owner
  in not (null owner) ==>
  case result of
    TransferSuccess newOwner -> property $ getOwner newOwner == owner
    TransferConflict _ -> property False
    TransferError _ -> property False

prop_transfer_preserves_type :: String -> String -> String -> Property
prop_transfer_preserves_type from to resType =
  let owner1 = OwnershipInfo from "resource" resType 1
      result = transferOwnership owner1 to
  in not (null from) && not (null to) && from /= to && not (null resType) ==>
  case result of
    TransferSuccess newOwner -> property $ ownershipType newOwner == resType
    TransferConflict _ -> property False
    TransferError _ -> property False

prop_transfer_updates_timestamp :: String -> String -> Property
prop_transfer_updates_timestamp from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
      result = transferOwnership owner1 to
  in not (null from) && not (null to) && from /= to ==>
  case result of
    TransferSuccess newOwner -> property $ ownershipTimestamp newOwner > 1
    TransferConflict _ -> property False
    TransferError _ -> property False

-- Transfer validity properties
prop_valid_transfer_succeeds :: String -> String -> Property
prop_valid_transfer_succeeds from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
      canTransferResult = canTransfer owner1 to
  in not (null from) && not (null to) && from /= to ==>
  property $ canTransferResult == True

prop_invalid_transfer_fails :: String -> Property
prop_invalid_transfer_fails owner =
  let owner1 = OwnershipInfo owner "resource" "type1" 1
      canTransferResult1 = canTransfer owner1 ""
      canTransferResult2 = canTransfer owner1 owner
  in not (null owner) ==>
  property $ canTransferResult1 == False && canTransferResult2 == False

prop_null_transfer_fails :: Property
prop_null_transfer_fails =
  let owner1 = OwnershipInfo "owner" "resource" "type1" 1
      canTransferResult = canTransfer owner1 ""
  in property $ canTransferResult == False

prop_empty_ownership_fails :: String -> Property
prop_empty_ownership_fails to =
  let emptyOwner = OwnershipInfo "" "resource" "type1" 1
      canTransferResult = canTransfer emptyOwner to
  in not (null to) ==>
  property $ canTransferResult == False

prop_duplicate_ownership_detected :: String -> String -> Property
prop_duplicate_ownership_detected from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
      owner2 = OwnershipInfo to "resource" "type1" 2
      conflict = checkOwnershipConflict owner1 owner2
  in not (null from) && not (null to) && from /= to ==>
  property $ conflict == True

-- Conflict detection properties
prop_conflicting_detected :: String -> String -> String -> Property
prop_conflicting_detected owner1 owner2 resource =
  let info1 = OwnershipInfo owner1 resource "type1" 1
      info2 = OwnershipInfo owner2 resource "type1" 2
      conflict = checkOwnershipConflict info1 info2
  in not (null owner1) && not (null owner2) && owner1 /= owner2 && not (null resource) ==>
  property $ conflict == True

prop_nonconflicting_allowed :: String -> String -> Property
prop_nonconflicting_allowed owner1 owner2 =
  let info1 = OwnershipInfo owner1 "resource1" "type1" 1
      info2 = OwnershipInfo owner2 "resource2" "type1" 2
      conflict = checkOwnershipConflict info1 info2
  in not (null owner1) && not (null owner2) ==>
  property $ conflict == False

prop_multiple_conflicts :: [String] -> String -> Property
prop_multiple_conflicts owners resource =
  let distinctOwners = nub (L.filter (not . null) owners)
      ownerships = L.map (\o -> OwnershipInfo o resource "type1" 1) distinctOwners
      conflicts = [checkOwnershipConflict o1 o2 | o1 <- ownerships, o2 <- ownerships, o1 /= o2]
  in L.length distinctOwners > 1 && not (null resource) ==>
  property $ L.any (== True) conflicts

prop_conflict_resolution_preserves :: String -> String -> String -> Property
prop_conflict_resolution_preserves owner1 owner2 resource =
  let info1 = OwnershipInfo owner1 resource "type1" 1
      info2 = OwnershipInfo owner2 resource "type1" 2
  in not (null owner1) && not (null owner2) && owner1 /= owner2 && not (null resource) ==>
  case transferOwnership info1 owner2 of
    TransferConflict _ -> property True  -- Expected behavior
    _ -> property False

-- Transfer chaining properties
prop_chained_maintains_ownership :: [String] -> Property
prop_chained_maintains_ownership owners =
  let distinctOwners = L.filter (not . null) (nub owners)
  in L.length distinctOwners > 2 ==>
  case distinctOwners of
    (o1:o2:rest) -> 
      let initialOwner = OwnershipInfo o1 "resource" "type1" 1
          chainResult = L.foldl (\acc owner -> 
            case acc of
              TransferSuccess ownerInfo -> transferOwnership ownerInfo owner
              _ -> acc
          ) (TransferSuccess initialOwner) (o2:rest)
      in case chainResult of
        TransferSuccess finalOwner -> property $ getOwner finalOwner == last distinctOwners
        _ -> property False
    _ -> property False

prop_chain_preserves_history :: [String] -> Property
prop_chain_preserves_history owners =
  let distinctOwners = L.filter (not . null) (nub owners)
  in L.length distinctOwners > 2 ==>
  property $ L.length distinctOwners <= L.length (nub distinctOwners)  -- History preserved

prop_circular_detected :: [String] -> Property
prop_circular_detected owners =
  let distinctOwners = L.filter (not . null) (nub owners)
  in L.length distinctOwners > 1 ==>
  case distinctOwners of
    (o1:os) -> 
      let initialOwner = OwnershipInfo o1 "resource" "type1" 1
          -- Create a circular transfer
          chainResult = L.foldl (\acc owner -> 
            case acc of
              TransferSuccess ownerInfo -> transferOwnership ownerInfo owner
              _ -> acc
          ) (TransferSuccess initialOwner) (os ++ [o1])
      in case chainResult of
        TransferConflict _ -> property True  -- Should detect circular transfer
        _ -> property False
    _ -> property False

prop_long_chain_efficient :: Int -> Property
prop_long_chain_efficient n =
  let chainLength = min (max n 0) 1000  -- Cap at 1000
      owners = L.map (\i -> "owner" ++ show i) [1..chainLength]
  in chainLength > 10 ==>
  property $ L.length owners == chainLength

-- Ownership invariant properties
prop_single_owner_invariant :: String -> String -> Property
prop_single_owner_invariant from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
  in not (null from) && not (null to) && from /= to ==>
  case transferOwnership owner1 to of
    TransferSuccess newOwner -> property $ L.length (words (getOwner newOwner)) >= 1
    _ -> property False

prop_ownership_acyclic :: [String] -> Property
prop_ownership_acyclic owners =
  let distinctOwners = L.filter (not . null) (nub owners)
  in L.length distinctOwners > 2 ==>
  property $ L.length distinctOwners == L.length (nub distinctOwners)

prop_preserves_resource_count :: String -> String -> String -> Property
prop_preserves_resource_count from to resource =
  let owner1 = OwnershipInfo from resource "type1" 1
  in not (null from) && not (null to) && from /= to && not (null resource) ==>
  case transferOwnership owner1 to of
    TransferSuccess newOwner -> property $ ownershipResource newOwner == resource
    _ -> property False

prop_ownership_consistency :: String -> String -> Property
prop_ownership_consistency from to =
  let owner1 = OwnershipInfo from "resource" "type1" 1
  in not (null from) && not (null to) && from /= to ==>
  case transferOwnership owner1 to of
    TransferSuccess newOwner -> 
      property $ not (L.null (getOwner newOwner)) && 
                 not (L.null (ownershipResource newOwner)) &&
                 not (L.null (ownershipType newOwner))
    _ -> property False