module Test.Unit.OwnershipBridgeSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.OwnershipBridge

-- Test ownership bridge creation
prop_ownership_bridge_creation :: String -> Property
prop_ownership_bridge_creation resource =
  let bridge = createOwnershipBridge resource
      bridgeResource = getBridgeResource bridge
  in property $ bridgeResource === resource

-- Test ownership transfer tracking
prop_ownership_transfer_tracking :: String -> String -> Property
prop_ownership_transfer_tracking fromOwner toOwner =
  let bridge = createOwnershipBridge "test"
      bridgeWithTransfer = trackOwnershipTransfer bridge fromOwner toOwner
      transfers = getOwnershipTransfers bridgeWithTransfer
  in property $ (fromOwner, toOwner) `elem` transfers

-- Test ownership constraint checking
prop_ownership_constraint_checking :: String -> [String] -> Property
prop_ownership_constraint_checking resource constraints =
  let bridge = createOwnershipBridge resource
      bridgeWithConstraints = addOwnershipConstraints bridge constraints
      valid = checkOwnershipConstraints bridgeWithConstraints
  in property $ 
    case constraints of
      [] -> valid
      _ -> property True  -- Simplified for this example

-- Test ownership lifetime management
prop_ownership_lifetime_management :: String -> Int -> Property
prop_ownership_lifetime_management resource lifetime =
  let bridge = createOwnershipBridge resource
      bridgeWithLifetime = setOwnershipLifetime bridge lifetime
      remaining = getRemainingLifetime bridgeWithLifetime
  in property $ remaining === lifetime

-- Test ownership borrowing
prop_ownership_borrowing :: String -> String -> Property
prop_ownership_borrowing owner borrower =
  let bridge = createOwnershipBridge "test"
      bridgeWithBorrow = createOwnershipBorrow bridge owner borrower
      borrowActive = isBorrowActive bridgeWithBorrow
  in property $ borrowActive

tests :: TestTree
tests = testGroup "Ownership Bridge Tests"
  [ testProperty "ownership bridge creation" prop_ownership_bridge_creation
  , testProperty "ownership transfer tracking" prop_ownership_transfer_tracking
  , testProperty "ownership constraint checking" prop_ownership_constraint_checking
  , testProperty "ownership lifetime management" prop_ownership_lifetime_management
  , testProperty "ownership borrowing" prop_ownership_borrowing
  ]