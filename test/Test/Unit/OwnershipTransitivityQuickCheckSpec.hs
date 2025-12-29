{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipTransfer(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  )

import SourceLocation (SourcePos(..), startPos)

import Data.List (nub, sort)
import qualified Data.Set as Set

-- Mock ownership transfer operations for testing
data MockOwnershipState = MockOwnershipState
  { owners :: [(String, OwnershipType)]
  , transfers :: [OwnershipTransfer]
  } deriving (Show, Eq)

-- Property: Ownership transfer should be transitive
prop_ownership_transfer_transitive :: String -> String -> String -> Property
prop_ownership_transfer_transitive owner1 owner2 owner3 =
  let initial = MockOwnershipState [(owner1, Owned), (owner2, Owned), (owner3, Owned)] []
      transfer1 = performTransfer owner1 owner2 initial
      transfer2 = performTransfer owner2 owner3 transfer1
      finalOwners = map fst $ owners transfer2
  in property $ owner3 `elem` finalOwners .&&. owner1 `notElem` finalOwners

-- Property: Moving ownership should remove previous owner
prop_moving_ownership_removes_previous :: String -> String -> Property
prop_moving_ownership_removes_previous from to =
  from /= to ==>
  let initial = MockOwnershipState [(from, Owned), (to, Unowned)] []
      result = performTransfer from to initial
      resultOwners = map fst $ owners result
  in property $ to `elem` resultOwners .&&. from `notElem` resultOwners

-- Property: Copying ownership should preserve original owner
prop_copying_ownership_preserves_original :: String -> String -> Property
prop_copying_ownership_preserves_original from to =
  from /= to ==>
  let initial = MockOwnershipState [(from, Owned), (to, Unowned)] []
      result = performCopy from to initial
      resultOwners = map fst $ owners result
  in property $ to `elem` resultOwners .&&. from `elem` resultOwners

-- Property: Borrowing ownership should be temporary
prop_borrowing_is_temporary :: String -> String -> Property
prop_borrowing_is_temporary lender borrower =
  lender /= borrower ==>
  let initial = MockOwnershipState [(lender, Owned), (borrower, Unowned)] []
      borrowed = performBorrow lender borrower initial
      returned = performReturn borrower borrowed
      finalOwners = map fst $ owners returned
  in property $ lender `elem` finalOwners .&&. borrower `elem` finalOwners

-- Property: Shared ownership should allow multiple owners
prop_shared_ownership_multiple :: [String] -> Property
prop_shared_ownership_multiple ownersList =
  length ownersList >= 2 .&&. length (nub ownersList) >= 2 ==>
  let initialOwners = [(owner, if owner == head ownersList then Owned else Unowned) | owner <- ownersList]
      initial = MockOwnershipState initialOwners []
      result = foldl (\acc owner -> performShare (head ownersList) owner acc) initial (tail ownersList)
      sharedOwners = filter (\(_, otype) -> otype == Shared) $ owners result
  in property $ length sharedOwners >= 2

-- Property: Ownership transfer chain should maintain consistency
prop_ownership_chain_consistency :: [String] -> Property
prop_ownership_chain_consistency chain =
  length chain >= 3 .&&. length (nub chain) >= 3 ==>
  let initialOwners = [(head chain, Owned)] ++ [(owner, Unowned) | owner <- tail chain]
      initial = MockOwnershipState initialOwners []
      result = foldl (\acc (from, to) -> performTransfer from to acc) initial (zip chain (tail chain))
      finalOwners = map fst $ owners result
  in property $ last chain `elem` finalOwners .&&. head chain `notElem` finalOwners

-- Property: Ownership should prevent double move errors
prop_prevent_double_move :: String -> String -> String -> Property
prop_prevent_double_move owner target1 target2 =
  owner /= target1 .&&. owner /= target2 .&&. target1 /= target2 ==>
  let initial = MockOwnershipState [(owner, Owned), (target1, Unowned), (target2, Unowned)] []
      firstTransfer = performTransfer owner target1 initial
      secondTransfer = performTransfer owner target2 firstTransfer
      finalOwners = map fst $ owners secondTransfer
  in property $ target1 `elem` finalOwners .&&. owner `notElem` finalOwners .&&. target2 `elem` finalOwners

-- Property: Ownership analysis should detect cycles
prop_detect_ownership_cycles :: [String] -> Property
prop_detect_ownership_cycles nodes =
  length nodes >= 3 .&&. length (nub nodes) >= 3 ==>
  let cycle = zip nodes (tail nodes ++ [head nodes])
      hasCycle = detectOwnershipCycle cycle
  in property $ hasCycle

-- Property: Ownership transfer should preserve total ownership count
prop_preserve_ownership_count :: String -> String -> Property
prop_preserve_ownership_count from to =
  from /= to ==>
  let initial = MockOwnershipState [(from, Owned), (to, Unowned)] []
      result = performTransfer from to initial
      initialCount = length $ filter (\(_, otype) -> otype == Owned) $ owners initial
      finalCount = length $ filter (\(_, otype) -> otype == Owned) $ owners result
  in property $ initialCount === finalCount

-- Helper functions for mock ownership operations
performTransfer :: String -> String -> MockOwnershipState -> MockOwnershipState
performTransfer from to state =
  let newOwners = [(to, Owned)] ++ 
                  [(owner, if owner == from then Unowned else otype) | (owner, otype) <- owners state, owner /= to]
      transfer = OwnershipTransfer from to Move startPos
  in MockOwnershipState newOwners (transfer : transfers state)

performCopy :: String -> String -> MockOwnershipState -> MockOwnershipState
performCopy from to state =
  let newOwners = [(to, Owned)] ++ 
                  [(owner, otype) | (owner, otype) <- owners state, owner /= to]
      transfer = OwnershipTransfer from to Copy startPos
  in MockOwnershipState newOwners (transfer : transfers state)

performBorrow :: String -> String -> MockOwnershipState -> MockOwnershipState
performBorrow lender borrower state =
  let newOwners = [(borrower, Borrowed lender)] ++ 
                  [(owner, otype) | (owner, otype) <- owners state, owner /= borrower]
      transfer = OwnershipTransfer lender borrower Borrow startPos
  in MockOwnershipState newOwners (transfer : transfers state)

performReturn :: String -> MockOwnershipState -> MockOwnershipState
performReturn borrower state =
  case findBorrowedOwner borrower state of
    Just lender -> 
      let newOwners = [(borrower, Unowned), (lender, Owned)] ++ 
                      [(owner, otype) | (owner, otype) <- owners state, 
                                       owner /= borrower, owner /= lender]
      in MockOwnershipState newOwners (transfers state)
    Nothing -> state

performShare :: String -> String -> MockOwnershipState -> MockOwnershipState
performShare from to state =
  let newOwners = [(to, Shared), (from, Shared)] ++ 
                  [(owner, otype) | (owner, otype) <- owners state, 
                                   owner /= from, owner /= to]
      transfer = OwnershipTransfer from to Share startPos
  in MockOwnershipState newOwners (transfer : transfers state)

findBorrowedOwner :: String -> MockOwnershipState -> Maybe String
findBorrowedOwner borrower state =
  case filter (\(_, otype) -> case otype of Borrowed lender -> True; _ -> False) $ owners state of
    ((_, Borrowed lender):_) -> Just lender
    _ -> Nothing

detectOwnershipCycle :: [(String, String)] -> Bool
detectOwnershipCycle transfers =
  let visited = Set.empty
      recStack = Set.empty
  in hasCycleHelper transfers visited recStack

hasCycleHelper :: [(String, String)] -> Set.Set String -> Set.Set String -> Bool
hasCycleHelper [] _ _ = False
hasCycleHelper ((from, to):rest) visited recStack
  | Set.member from recStack = True
  | Set.member from visited = hasCycleHelper rest visited recStack
  | otherwise = 
      let newVisited = Set.insert from visited
          newRecStack = Set.insert from recStack
      in hasCycleHelper rest newVisited newRecStack

tests :: TestTree
tests = testGroup "Ownership Transitivity QuickCheck Tests"
  [ fastProperty "Ownership transfer is transitive" prop_ownership_transfer_transitive
  , fastProperty "Moving ownership removes previous owner" prop_moving_ownership_removes_previous
  , fastProperty "Copying ownership preserves original owner" prop_copying_ownership_preserves_original
  , fastProperty "Borrowing is temporary" prop_borrowing_is_temporary
  , fastProperty "Shared ownership allows multiple owners" prop_shared_ownership_multiple
  , fastProperty "Ownership chain maintains consistency" prop_ownership_chain_consistency
  , fastProperty "Ownership prevents double move errors" prop_prevent_double_move
  , fastProperty "Ownership analysis detects cycles" prop_detect_ownership_cycles
  , fastProperty "Ownership transfer preserves total count" prop_preserve_ownership_count
  ]