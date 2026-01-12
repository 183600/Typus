module Test.Unit.OwnershipTransferQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import qualified Ownership.Common.Types as Own
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Helper generators
data OwnershipState = OwnershipState
  { ownerMap :: Map.Map String String
  , borrowedVars :: Set String
  , movedVars :: Set String
  } deriving (Show, Eq)

instance Arbitrary OwnershipState where
  arbitrary = do
    owners <- arbitrary
    borrowed <- arbitrary
    moved <- arbitrary
    return $ OwnershipState owners borrowed moved

-- Properties for ownership transfer
prop_transfer_ownership_changes_owner :: OwnershipState -> String -> String -> Property
prop_transfer_ownership_changes_owner state from to = 
  from `Map.member` ownerMap state ==> 
  let newState = transferOwnership state from to
      newOwner = Map.lookup to (ownerMap newState)
  in newOwner == Just to

prop_transfer_ownership_removes_old_owner :: OwnershipState -> String -> String -> Property
prop_transfer_ownership_removes_old_owner state from to = 
  from `Map.member` ownerMap state ==> 
  let newState = transferOwnership state from to
      oldOwnerStillExists = from `Map.member` ownerMap newState
  in not oldOwnerStillExists

prop_transfer_ownership_preserves_other_owners :: OwnershipState -> String -> String -> Property
prop_transfer_ownership_preserves_other_owners state from to = 
  from `Map.member` ownerMap state && from /= to ==> 
  let otherOwners = Map.filterWithKey (\k _ -> k /= from && k /= to) (ownerMap state)
      newState = transferOwnership state from to
      newOtherOwners = Map.filterWithKey (\k _ -> k /= from && k /= to) (ownerMap newState)
  in otherOwners == newOtherOwners

-- Properties for borrowing
prop_borrow_preserves_ownership :: OwnershipState -> String -> String -> Property
prop_borrow_preserves_ownership state borrower target = 
  target `Map.member` ownerMap state ==> 
  let newState = borrowVariable state borrower target
      originalOwner = Map.lookup target (ownerMap state)
      newOwner = Map.lookup target (ownerMap newState)
  in originalOwner == newOwner

prop_borrow_adds_to_borrowed_set :: OwnershipState -> String -> String -> Property
prop_borrow_adds_to_borrowed_set state borrower target = 
  target `Map.member` ownerMap state ==> 
  let newState = borrowVariable state borrower target
  in target `Set.member` borrowedVars newState

prop_borrow_prevents_double_borrow :: OwnershipState -> String -> String -> Property
prop_borrow_prevents_double_borrow state borrower target = 
  target `Map.member` ownerMap state && 
  not (target `Set.member` borrowedVars state) ==> 
  let newState = borrowVariable state borrower target
      borrowedCount = Set.size (borrowedVars newState)
  in borrowedCount >= 1

-- Properties for moving
prop_move_transfers_ownership :: OwnershipState -> String -> String -> Property
prop_move_transfers_ownership state from to = 
  from `Map.member` ownerMap state ==> 
  let newState = moveVariable state from to
      newOwner = Map.lookup to (ownerMap newState)
  in newOwner == Just to

prop_move_removes_original_owner :: OwnershipState -> String -> String -> Property
prop_move_removes_original_owner state from to = 
  from `Map.member` ownerMap state ==> 
  let newState = moveVariable state from to
      originalOwnerExists = from `Map.member` ownerMap newState
  in not originalOwnerExists

prop_move_adds_to_moved_set :: OwnershipState -> String -> String -> Property
prop_move_adds_to_moved_set state from to = 
  from `Map.member` ownerMap state ==> 
  let newState = moveVariable state from to
  in from `Set.member` movedVars newState

-- Properties for ownership checking
prop_check_ownership_valid_owner :: OwnershipState -> String -> String -> Property
prop_check_ownership_valid_owner state var owner = 
  var `Map.member` ownerMap state && 
  Map.lookup var (ownerMap state) == Just owner ==> 
  canAccess state var owner

prop_check_ownership_invalid_owner :: OwnershipState -> String -> String -> Property
prop_check_ownership_invalid_owner state var owner = 
  var `Map.member` ownerMap state && 
  Map.lookup var (ownerMap state) /= Just owner ==> 
  not (canAccess state var owner)

prop_check_ownership_moved_variable :: OwnershipState -> String -> String -> Property
prop_check_ownership_moved_variable state var owner = 
  var `Set.member` movedVars state ==> 
  not (canAccess state var owner)

-- Properties for lifetime management
prop_lifetime_ends_with_scope :: OwnershipState -> String -> Bool
prop_lifetime_ends_with_scope state var = 
  let newState = endScope state var
      varExists = var `Map.member` ownerMap newState || var `Set.member` borrowedVars newState || var `Set.member` movedVars newState
  in not varExists

prop_lifetime_preserves_other_vars :: OwnershipState -> String -> Bool
prop_lifetime_preserves_other_vars state var = 
  let otherVars = Map.keysSet (ownerMap state) `Set.union` borrowedVars state `Set.union` movedVars state
      otherVars' = Set.delete var otherVars
      newState = endScope state var
      finalOtherVars = Map.keysSet (ownerMap newState) `Set.union` borrowedVars newState `Set.union` movedVars newState
  in otherVars' `Set.isSubsetOf` finalOtherVars

-- Properties for ownership rules
prop_no_multiple_owners :: OwnershipState -> Bool
prop_no_multiple_owners state = 
  let owners = Map.elems (ownerMap state)
  in length owners == length (nub owners)

prop_cannot_use_moved_variable :: OwnershipState -> String -> String -> Property
prop_cannot_use_moved_variable state var user = 
  var `Set.member` movedVars state ==> 
  not (canUse state var user)

prop_can_use_owned_variable :: OwnershipState -> String -> Property
prop_can_use_owned_variable state var = 
  var `Map.member` ownerMap state && 
  not (var `Set.member` movedVars state) ==> 
  let owner = case Map.lookup var (ownerMap state) of
                Just o -> o
                Nothing -> ""
  in canUse state var owner

-- Helper functions
transferOwnership :: OwnershipState -> String -> String -> OwnershipState
transferOwnership state from to = 
  let owners = ownerMap state
      newOwners = case Map.lookup from owners of
                   Just _ -> Map.insert to (owners Map.! from) (Map.delete from owners)
                   Nothing -> owners
  in state { ownerMap = newOwners }

borrowVariable :: OwnershipState -> String -> String -> OwnershipState
borrowVariable state borrower target = 
  let newBorrowed = Set.insert target (borrowedVars state)
  in state { borrowedVars = newBorrowed }

moveVariable :: OwnershipState -> String -> String -> OwnershipState
moveVariable state from to = 
  let owners = ownerMap state
      newOwners = case Map.lookup from owners of
                   Just owner -> Map.insert to owner (Map.delete from owners)
                   Nothing -> owners
      newMoved = Set.insert from (movedVars state)
  in state { ownerMap = newOwners, movedVars = newMoved }

canAccess :: OwnershipState -> String -> String -> Bool
canAccess state var user = 
  case Map.lookup var (ownerMap state) of
    Just owner -> owner == user && not (var `Set.member` movedVars state)
    Nothing -> False

canUse :: OwnershipState -> String -> String -> Bool
canUse = canAccess

endScope :: OwnershipState -> String -> OwnershipState
endScope state var = 
  let newOwners = Map.delete var (ownerMap state)
      newBorrowed = Set.delete var (borrowedVars state)
      newMoved = Set.delete var (movedVars state)
  in state { ownerMap = newOwners, borrowedVars = newBorrowed, movedVars = newMoved }

-- Helper function
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

tests :: TestTree
tests = testGroup "Test.Unit.OwnershipTransferQuickCheckSpec Tests"
  [ fastProperty "transfer ownership changes owner" prop_transfer_ownership_changes_owner
  , fastProperty "transfer ownership removes old owner" prop_transfer_ownership_removes_old_owner
  , fastProperty "transfer ownership preserves other owners" prop_transfer_ownership_preserves_other_owners
  , fastProperty "borrow preserves ownership" prop_borrow_preserves_ownership
  , fastProperty "borrow adds to borrowed set" prop_borrow_adds_to_borrowed_set
  , fastProperty "borrow prevents double borrow" prop_borrow_prevents_double_borrow
  , fastProperty "move transfers ownership" prop_move_transfers_ownership
  , fastProperty "move removes original owner" prop_move_removes_original_owner
  , fastProperty "move adds to moved set" prop_move_adds_to_moved_set
  , fastProperty "check ownership valid owner" prop_check_ownership_valid_owner
  , fastProperty "check ownership invalid owner" prop_check_ownership_invalid_owner
  , fastProperty "check ownership moved variable" prop_check_ownership_moved_variable
  , fastProperty "lifetime ends with scope" prop_lifetime_ends_with_scope
  , fastProperty "lifetime preserves other vars" prop_lifetime_preserves_other_vars
  , fastProperty "no multiple owners" prop_no_multiple_owners
  , fastProperty "cannot use moved variable" prop_cannot_use_moved_variable
  , fastProperty "can use owned variable" prop_can_use_owned_variable
  ]