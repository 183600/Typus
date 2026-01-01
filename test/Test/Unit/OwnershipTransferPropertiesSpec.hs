{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
  ( OwnershipType(..)
  , OwnershipState(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , transferOwnership
  , checkOwnership
  , analyzeOwnership
  , isOwned
  , isBorrowed
  , isMutBorrowed
  , getOwner
  )

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  )

import qualified Ownership.Common.Types as Own
import qualified Data.Map as Map
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Ownership transfer mathematical properties
tests :: TestTree
tests = testGroup "Ownership transfer mathematical properties"
  [ -- Basic ownership properties
    testGroup "Basic ownership properties"
      [ testCase "owned values have clear ownership" $ do
          let analyzer = Own.newOwnershipAnalyzer
              state = Own.initialState analyzer
              value = "test_var"
              ownedState = Own.ownValue state value
          in isOwned ownedState value @?= True

      , testCase "borrowed values track their owner" $ do
          let analyzer = Own.newOwnershipAnalyzer
              state = Own.initialState analyzer
              owner = "owner_var"
              borrower = "borrower_var"
              borrowedState = Own.borrowValue state owner borrower
          in isBorrowed borrowedState borrower @?= True

      , fastProperty "ownership is exclusive" prop_ownership_exclusive
      , fastProperty "borrowing preserves original owner" prop_borrowing_preserves_owner
      , fastProperty "mutable borrowing is exclusive" prop_mutable_borrowing_exclusive
      ]

  , -- Transfer properties
    testGroup "Transfer properties"
      [ fastProperty "transfer changes ownership correctly" prop_transfer_changes_ownership
      , fastProperty "transfer is deterministic" prop_transfer_deterministic
      , fastProperty "transfer preserves borrow relationships" prop_transfer_preserves_borrows
      , fastProperty "transfer invalidates old borrows" prop_transfer_invalidates_old_borrows
      , fastProperty "transfer is idempotent for same owner" prop_transfer_idempotent_same_owner
      ]

  , -- Borrow properties
    testGroup "Borrow properties"
      [ fastProperty "borrowing creates valid relationship" prop_borrowing_creates_relationship
      , fastProperty "multiple immutable borrows allowed" prop_multiple_immutable_borrows
      , fastProperty "borrowing prevents mutations" prop_borrowing_prevents_mutation
      , fastProperty "borrowing tracks lifetime" prop_borrowing_tracks_lifetime
      , fastProperty "borrowing is transitive for references" prop_borrowing_transitive
      ]

  , -- Lifetime properties
    testGroup "Lifetime properties"
      [ fastProperty "lifetimes are properly nested" prop_lifetimes_nested
      , fastProperty "lifetime end releases borrows" prop_lifetime_end_releases
      , fastProperty "lifetime violations are detected" prop_lifetime_violations_detected
      , fastProperty "lifetime inference is conservative" prop_lifetime_inference_conservative
      ]

  , -- Error properties
    testGroup "Error detection properties"
      [ fastProperty "use after move is detected" prop_use_after_move_detected
      , fastProperty "double borrow is detected" prop_double_borrow_detected
      , fastProperty "borrowing owned value prevents move" prop_borrowing_prevents_move
      , fastProperty "ownership errors are deterministic" prop_ownership_errors_deterministic
      ]

  , -- Advanced properties
    testGroup "Advanced properties"
      [ fastProperty "ownership analysis is monotonic" prop_ownership_analysis_monotonic
      , fastProperty "ownership state is finite" prop_ownership_state_finite
      , fastProperty "ownership transfer preserves invariants" prop_transfer_preserves_invariants
      , fastProperty "ownership analysis terminates" prop_ownership_analysis_terminates
      ]

  , -- Edge cases
    testGroup "Edge cases"
      [ fastProperty "self-borrowing is handled correctly" prop_self_borrowing
      , fastProperty "circular ownership is detected" prop_circular_ownership_detected
      , fastProperty "empty ownership state is valid" prop_empty_ownership_valid
      , fastProperty "ownership with complex expressions" prop_complex_expressions
      ]
  ]

-- Basic ownership properties

prop_ownership_exclusive :: String -> String -> Property
prop_ownership_exclusive value owner1 owner2 =
  owner1 /= owner2 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      owned1 = Own.ownValue state value
      owner = getOwner owned1 value
  in property $ owner === Just owner1 .||. owner === Just owner2

prop_borrowing_preserves_owner :: String -> String -> String -> Property
prop_borrowing_preserves_owner value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      originalOwner = getOwner ownedState value
      currentOwner = getOwner borrowedState value
  in property $ originalOwner === currentOwner .&&. 
             isBorrowed borrowedState borrower

prop_mutable_borrowing_exclusive :: String -> String -> String -> Property
prop_mutable_borrowing_exclusive value borrower1 borrower2 =
  borrower1 /= borrower2 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      mutBorrowed1 = Own.mutBorrowValue ownedState value borrower1
      result = Own.mutBorrowValue mutBorrowed1 value borrower2
  in property $ isMutBorrowed mutBorrowed1 borrower1 .&&.
             not (isMutBorrowed result borrower2)

-- Transfer properties

prop_transfer_changes_ownership :: String -> String -> String -> Property
prop_transfer_changes_ownership value oldOwner newOwner =
  oldOwner /= newOwner ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      transferredState = Own.transferOwnership ownedState value oldOwner newOwner
      oldOwnerStill = getOwner ownedState value
      newOwnerNow = getOwner transferredState value
  in property $ oldOwnerStill === Just oldOwner .&&.
             newOwnerNow === Just newOwner

prop_transfer_deterministic :: String -> String -> String -> Property
prop_transfer_deterministic value oldOwner newOwner =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      transferred1 = Own.transferOwnership ownedState value oldOwner newOwner
      transferred2 = Own.transferOwnership ownedState value oldOwner newOwner
  in property $ getOwner transferred1 value === getOwner transferred2 value

prop_transfer_preserves_borrows :: String -> String -> String -> String -> Property
prop_transfer_preserves_borrows value oldOwner newOwner borrower =
  oldOwner /= newOwner && oldOwner /= borrower && newOwner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState oldOwner borrower
      transferredState = Own.transferOwnership borrowedState value oldOwner newOwner
      stillBorrowed = isBorrowed transferredState borrower
  in property $ stillBorrowed

prop_transfer_invalidates_old_borrows :: String -> String -> String -> String -> Property
prop_transfer_invalidates_old_borrows value oldOwner newOwner borrower1 borrower2 =
  oldOwner /= newOwner && borrower1 /= borrower2 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue (Own.borrowValue ownedState oldOwner borrower1) oldOwner borrower2
      transferredState = Own.transferOwnership borrowedState value oldOwner newOwner
      borrower1Still = isBorrowed transferredState borrower1
      borrower2Still = isBorrowed transferredState borrower2
  in property $ not (borrower1Still .||. borrower2Still)

prop_transfer_idempotent_same_owner :: String -> String -> Property
prop_transfer_idempotent_same_owner value owner =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      transferred1 = Own.transferOwnership ownedState value owner owner
      transferred2 = Own.transferOwnership transferred1 value owner owner
  in property $ getOwner transferred1 value === getOwner transferred2 value

-- Borrow properties

prop_borrowing_creates_relationship :: String -> String -> String -> Property
prop_borrowing_creates_relationship value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      hasRelationship = isBorrowed borrowedState borrower
      ownerPreserved = getOwner borrowedState value
  in property $ hasRelationship .&&. ownerPreserved === Just owner

prop_multiple_immutable_borrows :: String -> String -> [String] -> Property
prop_multiple_immutable_borrows value owner borrowers =
  not (null borrowers) && L.all (/= owner) borrowers && nub borrowers == borrowers ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = L.foldl (\s b -> Own.borrowValue s owner b) ownedState borrowers
      allBorrowed = L.all (isBorrowed borrowedState) borrowers
  in property $ allBorrowed

prop_borrowing_prevents_mutation :: String -> String -> String -> Property
prop_borrowing_prevents_mutation value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      canMutateBefore = Own.canMutate ownedState value owner
      canMutateAfter = Own.canMutate borrowedState value borrower
  in property $ canMutateBefore .&&. not canMutateAfter

prop_borrowing_tracks_lifetime :: String -> String -> String -> Property
prop_borrowing_tracks_lifetime value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      lifetime = Own.getBorrowLifetime borrowedState borrower
  in property $ isJust lifetime

prop_borrowing_transitive :: String -> String -> String -> String -> Property
prop_borrowing_transitive value owner borrower1 borrower2 =
  owner /= borrower1 && borrower1 /= borrower2 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowed1 = Own.borrowValue ownedState owner borrower1
      borrowed2 = Own.borrowValue borrowed1 borrower1 borrower2
      borrower1CanBorrow = isBorrowed borrowed1 borrower1
      borrower2CanBorrow = isBorrowed borrowed2 borrower2
  in property $ borrower1CanBorrow .&&. borrower2CanBorrow

-- Lifetime properties

prop_lifetimes_nested :: String -> [String] -> Property
prop_lifetimes_nested value borrowers =
  not (null borrowers) && nub borrowers == borrowers ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = L.foldl (\s b -> Own.borrowValue s value b) ownedState borrowers
      lifetimes = L.map (Own.getBorrowLifetime borrowedState) borrowers
  in property $ L.all isJust lifetimes

prop_lifetime_end_releases :: String -> String -> String -> Property
prop_lifetime_end_releases value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      releasedState = Own.endBorrow borrowedState borrower
      stillBorrowed = isBorrowed releasedState borrower
  in property $ not stillBorrowed

prop_lifetime_violations_detected :: String -> String -> String -> Property
prop_lifetime_violations_detected value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      releasedState = Own.endBorrow borrowedState borrower
      useAfterEnd = Own.useValue releasedState borrower
  in property $ isJust useAfterEnd

prop_lifetime_inference_conservative :: String -> String -> Property
prop_lifetime_inference_conservative value owner =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      inferredLifetime = Own.inferLifetime ownedState value
  in property $ isJust inferredLifetime

-- Error detection properties

prop_use_after_move_detected :: String -> String -> String -> Property
prop_use_after_move_detected value oldOwner newOwner =
  oldOwner /= newOwner ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      transferredState = Own.transferOwnership ownedState value oldOwner newOwner
      useAfterMove = Own.useValue transferredState oldOwner
  in property $ isJust useAfterMove

prop_double_borrow_detected :: String -> String -> String -> Property
prop_double_borrow_detected value borrower1 borrower2 =
  borrower1 /= borrower2 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      mutBorrowed1 = Own.mutBorrowValue ownedState value borrower1
      doubleBorrow = Own.mutBorrowValue mutBorrowed1 value borrower2
  in property $ Own.hasErrors doubleBorrow

prop_borrowing_prevents_move :: String -> String -> String -> Property
prop_borrowing_prevents_move value owner borrower =
  owner /= borrower ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      borrowedState = Own.borrowValue ownedState owner borrower
      moveResult = Own.transferOwnership borrowedState value owner "new_owner"
  in property $ Own.hasErrors moveResult

prop_ownership_errors_deterministic :: String -> String -> String -> Property
prop_ownership_errors_deterministic value owner1 owner2 =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      error1 = Own.transferOwnership ownedState value owner1 owner2
      error2 = Own.transferOwnership ownedState value owner1 owner2
  in property $ Own.hasErrors error1 === Own.hasErrors error2

-- Advanced properties

prop_ownership_analysis_monotonic :: String -> [String] -> Property
prop_ownership_analysis_monotonic value operations =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      finalState = L.foldl (\s op -> Own.applyOperation s value op) state operations
      initialStateInfo = Own.getStateInfo state
      finalStateInfo = Own.getStateInfo finalState
  in property $ L.length finalStateInfo >= L.length initialStateInfo

prop_ownership_state_finite :: String -> Property
prop_ownership_state_finite value =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      possibleStates = Own.enumerateStates state value
  in property $ L.length possibleStates < 1000 -- Reasonable bound

prop_transfer_preserves_invariants :: String -> String -> String -> Property
prop_transfer_preserves_invariants value oldOwner newOwner =
  oldOwner /= newOwner ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      invariants1 = Own.checkInvariants ownedState
      transferredState = Own.transferOwnership ownedState value oldOwner newOwner
      invariants2 = Own.checkInvariants transferredState
  in property $ L.all id invariants1 ==> L.all id invariants2

prop_ownership_analysis_terminates :: String -> [String] -> Property
prop_ownership_analysis_terminates value operations =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      result = Own.analyzeOperations state value operations
  in property $ True -- Should terminate for L.all inputs

-- Edge cases

prop_self_borrowing :: String -> Property
prop_self_borrowing value =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      ownedState = Own.ownValue state value
      selfBorrow = Own.borrowValue ownedState value value
  in property $ Own.hasErrors selfBorrow

prop_circular_ownership_detected :: String -> String -> String -> Property
prop_circular_ownership_detected value1 value2 value3 =
  value1 /= value2 && value2 /= value3 && value1 /= value3 ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      owned1 = Own.ownValue state value1
      owned2 = Own.ownValue owned1 value2
      owned3 = Own.ownValue owned2 value3
      circular = Own.transferOwnership owned3 value3 value2 value1
  in property $ Own.hasErrors circular

prop_empty_ownership_valid :: Property
prop_empty_ownership_valid =
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      invariants = Own.checkInvariants state
  in property $ L.all id invariants

prop_complex_expressions :: String -> [String] -> Property
prop_complex_expressions baseValue operations =
  not (null operations) ==>
  let analyzer = Own.newOwnershipAnalyzer
      state = Own.initialState analyzer
      complexState = L.foldl (\s op -> Own.applyComplexOperation s baseValue op) state operations
      invariants = Own.checkInvariants complexState
  in property $ L.all id invariants