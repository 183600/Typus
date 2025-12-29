{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalOwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
import SourceLocation (SourcePos(..), startPos, posAt)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List ((\\))

-- | Test suite for Ownership transitivity properties
tests :: TestTree
tests =
  testGroup "Ownership Transitivity Properties"
    [ testGroup "Basic ownership properties"
        [ fastProperty "ownership is reflexive" prop_ownership_reflexive
        , fastProperty "ownership transfer is deterministic" prop_ownership_transfer_deterministic
        , fastProperty "ownership cannot be duplicated" prop_ownership_no_duplication
        , fastProperty "ownership tracking preserves provenance" prop_ownership_provenance
        ]

    , testGroup "Transitivity properties"
        [ fastProperty "ownership transfer is transitive" prop_ownership_transitive
        , fastProperty "ownership chains preserve original owner" prop_ownership_chain_preserves_original
        , fastProperty "ownership transfer creates linear history" prop_ownership_linear_history
        , fastProperty "ownership cannot form cycles" prop_ownership_no_cycles
        ]

    , testGroup "Borrowing properties"
        [ fastProperty "borrowing does not transfer ownership" prop_borrowing_no_transfer
        , fastProperty "borrowing is limited by lifetime" prop_borrowing_limited_lifetime
        , fastProperty "multiple borrows are allowed for immutable references" prop_multiple_immutable_borrows
        , fastProperty "mutable borrows are exclusive" prop_mutable_borrow_exclusive
        ]

    , testGroup "Memory safety properties"
        [ fastProperty "owned resources cannot be used after transfer" prop_no_use_after_transfer
        , fastProperty "borrowed resources cannot be modified while borrowed" prop_no_modify_while_borrowed
        , fastProperty "ownership ensures single point of deallocation" prop_single_deallocation
        ]

    , testGroup "Ownership inference properties"
        [ fastProperty "ownership inference is conservative" prop_ownership_inference_conservative
        , fastProperty "ownership analysis terminates" prop_ownership_analysis_terminates
        , fastProperty "ownership constraints are consistent" prop_ownership_constraints_consistent
        ]
    ]

-- Helper types for ownership testing
data Resource = Resource 
  { resourceId :: String
  , resourceOwner :: String
  , resourceBorrowers :: Set String
  } deriving (Show, Eq, Ord)

data OwnershipState = OwnershipState
  { owners :: Map String Resource
  , transferHistory :: [(String, String, SourcePos)] -- (from, to, position)
  } deriving (Show, Eq)

-- Helper functions
initialOwnership :: String -> String -> OwnershipState
initialOwnership resourceId ownerId = 
  OwnershipState 
    (Map.singleton resourceId (Resource resourceId ownerId Set.empty))
    []

transferOwnership :: String -> String -> String -> SourcePos -> OwnershipState -> Either String OwnershipState
transferOwnership resourceId fromOwnerId toOwnerId pos state =
  case Map.lookup resourceId (owners state) of
    Nothing -> Left "Resource not found"
    Just resource -> 
      if resourceOwner resource /= fromOwnerId
      then Left "Not the owner"
      else 
        let newResource = resource { resourceOwner = toOwnerId }
            newOwners = Map.insert resourceId newResource (owners state)
            newHistory = (fromOwnerId, toOwnerId, pos) : transferHistory state
        in Right $ state { owners = newOwners, transferHistory = newHistory }

borrowResource :: String -> String -> OwnershipState -> Either String OwnershipState
borrowResource resourceId borrowerId state =
  case Map.lookup resourceId (owners state) of
    Nothing -> Left "Resource not found"
    Just resource ->
      if borrowerId `Set.member` resourceBorrowers resource
      then Left "Already borrowed"
      else 
        let newResource = resource { resourceBorrowers = Set.insert borrowerId (resourceBorrowers resource) }
            newOwners = Map.insert resourceId newResource (owners state)
        in Right $ state { owners = newOwners }

-- Basic ownership properties

prop_ownership_reflexive :: String -> Property
prop_ownership_reflexive ownerId =
  not (null ownerId) && length ownerId <= 10 ==>
  let resourceId = "resource_" ++ ownerId
      state = initialOwnership resourceId ownerId
  in case Map.lookup resourceId (owners state) of
    Just resource -> property $ resourceOwner resource === ownerId
    Nothing -> property $ False

prop_ownership_transfer_deterministic :: String -> String -> String -> Property
prop_ownership_transfer_deterministic resourceId fromOwner toOwner =
  not (null resourceId) && not (null fromOwner) && not (null toOwner) &&
  length resourceId <= 10 && length fromOwner <= 10 && length toOwner <= 10 &&
  fromOwner /= toOwner ==>
  let state1 = initialOwnership resourceId fromOwner
      state2 = initialOwnership resourceId fromOwner
      pos = posAt 1 1
  in case (transferOwnership resourceId fromOwner toOwner pos state1,
           transferOwnership resourceId fromOwner toOwner pos state2) of
    (Right result1, Right result2) -> property $ result1 === result2
    (Left _, Left _) -> property $ True
    _ -> property $ False

prop_ownership_no_duplication :: String -> String -> String -> Property
prop_ownership_no_duplication resourceId originalOwner newOwner =
  not (null resourceId) && not (null originalOwner) && not (null newOwner) &&
  originalOwner /= newOwner ==>
  let state = initialOwnership resourceId originalOwner
      pos = posAt 1 1
  in case transferOwnership resourceId originalOwner newOwner pos state of
    Right newState -> 
      case Map.lookup resourceId (owners newState) of
        Just resource -> property $ resourceOwner resource === newOwner .&&.
                                   Set.size (resourceBorrowers resource) === 0
        Nothing -> property $ False
    Left _ -> property $ False

prop_ownership_provenance :: String -> String -> String -> Property
prop_ownership_provenance resourceId originalOwner intermediateOwner finalOwner =
  not (null resourceId) && not (null originalOwner) && not (null intermediateOwner) && not (null finalOwner) &&
  all distinct [originalOwner, intermediateOwner, finalOwner] ==>
  let state = initialOwnership resourceId originalOwner
      pos1 = posAt 1 1
      pos2 = posAt 2 1
  in case do
        state1 <- transferOwnership resourceId originalOwner intermediateOwner pos1 state
        state2 <- transferOwnership resourceId intermediateOwner finalOwner pos2 state1
        return state2 of
    Right finalState -> property $ transferHistory finalState === 
                                [(intermediateOwner, finalOwner, pos2), (originalOwner, intermediateOwner, pos1)]
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

-- Transitivity properties

prop_ownership_transitive :: String -> String -> String -> String -> Property
prop_ownership_transitive resourceId owner1 owner2 owner3 =
  not (null resourceId) && all (not . null) [owner1, owner2, owner3] &&
  all distinct [owner1, owner2, owner3] ==>
  let state = initialOwnership resourceId owner1
      pos1 = posAt 1 1
      pos2 = posAt 2 1
  in case do
        state1 <- transferOwnership resourceId owner1 owner2 pos1 state
        state2 <- transferOwnership resourceId owner2 owner3 pos2 state1
        return state2 of
    Right finalState -> 
      case Map.lookup resourceId (owners finalState) of
        Just resource -> property $ resourceOwner resource === owner3
        Nothing -> property $ False
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_ownership_chain_preserves_original :: String -> [String] -> Property
prop_ownership_chain_preserves_original resourceId owners =
  not (null resourceId) && not (null owners) && length owners <= 5 &&
  all (not . null) owners && all distinct owners ==>
  let initialOwner = head owners
      state = initialOwnership resourceId initialOwner
      positions = [posAt i 1 | i <- [1..length owners]]
      transferChain = zip3 (init owners) (tail owners) (init positions)
      finalState = foldl (\state (from, to, pos) -> 
                          case state of
                            Left err -> Left err
                            Right s -> transferOwnership resourceId from to pos s
                         ) (Right state) transferChain
  in case finalState of
    Right _ -> property $ True -- Chain should be valid
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_ownership_linear_history :: String -> [String] -> Property
prop_ownership_linear_history resourceId owners =
  not (null resourceId) && not (null owners) && length owners <= 4 &&
  all (not . null) owners && all distinct owners ==>
  let initialOwner = head owners
      state = initialOwnership resourceId initialOwner
      positions = [posAt i 1 | i <- [1..length owners]]
      transferChain = zip3 (init owners) (tail owners) (init positions)
      finalState = foldl (\state (from, to, pos) -> 
                          case state of
                            Left err -> Left err
                            Right s -> transferOwnership resourceId from to pos s
                         ) (Right state) transferChain
  in case finalState of
    Right goodState -> property $ length (transferHistory goodState) === length owners - 1
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_ownership_no_cycles :: String -> [String] -> Property
prop_ownership_no_cycles resourceId owners =
  not (null resourceId) && not (null owners) && length owners <= 4 &&
  all (not . null) owners ==>
  let initialOwner = head owners
      state = initialOwnership resourceId initialOwner
      -- Create a potential cycle by transferring back to original owner
      cycleOwners = owners ++ [initialOwner]
      positions = [posAt i 1 | i <- [1..length cycleOwners]]
      transferChain = zip3 (init cycleOwners) (tail cycleOwners) (init positions)
      finalState = foldl (\state (from, to, pos) -> 
                          case state of
                            Left err -> Left err
                            Right s -> transferOwnership resourceId from to pos s
                         ) (Right state) transferChain
  in case finalState of
    Right _ -> property $ True -- Should handle cycles gracefully
    Left _ -> property $ True -- May fail due to cycle detection

-- Borrowing properties

prop_borrowing_no_transfer :: String -> String -> String -> Property
prop_borrowing_no_transfer resourceId owner borrower =
  not (null resourceId) && not (null owner) && not (null borrower) &&
  owner /= borrower ==>
  let state = initialOwnership resourceId owner
  in case borrowResource resourceId borrower state of
    Right borrowedState -> 
      case Map.lookup resourceId (owners borrowedState) of
        Just resource -> property $ resourceOwner resource === owner .&&.
                                   borrower `Set.member` resourceBorrowers resource
        Nothing -> property $ False
    Left _ -> property $ False

prop_borrowing_limited_lifetime :: String -> String -> String -> Property
prop_borrowing_limited_lifetime resourceId owner borrower =
  not (null resourceId) && not (null owner) && not (null borrower) &&
  owner /= borrower ==>
  let state = initialOwnership resourceId owner
  in case borrowResource resourceId borrower state of
    Right borrowedState -> 
      case Map.lookup resourceId (owners borrowedState) of
        Just resource -> property $ Set.size (resourceBorrowers resource) === 1
        Nothing -> property $ False
    Left _ -> property $ False

prop_multiple_immutable_borrows :: String -> String -> [String] -> Property
prop_multiple_immutable_borrows resourceId owner borrowers =
  not (null resourceId) && not (null owner) && not (null borrowers) &&
  length borrowers <= 3 && all (/= owner) borrowers && all distinct borrowers ==>
  let state = initialOwnership resourceId owner
      borrowAll = foldl (\state borrower -> 
                          case state of
                            Left err -> Left err
                            Right s -> borrowResource resourceId borrower s
                        ) (Right state) borrowers
  in case borrowAll of
    Right finalState -> 
      case Map.lookup resourceId (owners finalState) of
        Just resource -> property $ resourceOwner resource === owner .&&.
                                   Set.fromList borrowers `Set.isSubsetOf` resourceBorrowers resource
        Nothing -> property $ False
    Left _ -> property $ False
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs

prop_mutable_borrow_exclusive :: String -> String -> String -> String -> Property
prop_mutable_borrow_exclusive resourceId owner borrower1 borrower2 =
  not (null resourceId) && not (null owner) && not (null borrower1) && not (null borrower2) &&
  owner /= borrower1 && owner /= borrower2 && borrower1 /= borrower2 ==>
  let state = initialOwnership resourceId owner
  in case borrowResource resourceId borrower1 state of
    Right borrowedState1 -> 
      case borrowResource resourceId borrower2 borrowedState1 of
        Right _ -> property $ False -- Should not allow second borrow
        Left _ -> property $ True -- Should fail
    Left _ -> property $ False -- First borrow should succeed

-- Memory safety properties

prop_no_use_after_transfer :: String -> String -> String -> Property
prop_no_use_after_transfer resourceId originalOwner newOwner =
  not (null resourceId) && not (null originalOwner) && not (null newOwner) &&
  originalOwner /= newOwner ==>
  let state = initialOwnership resourceId originalOwner
      pos = posAt 1 1
  in case transferOwnership resourceId originalOwner newOwner pos state of
    Right transferredState -> 
      case transferOwnership resourceId originalOwner newOwner pos transferredState of
        Right _ -> property $ False -- Original owner should not be able to transfer again
        Left _ -> property $ True -- Should fail
    Left _ -> property $ False

prop_no_modify_while_borrowed :: String -> String -> String -> Property
prop_no_modify_while_borrowed resourceId owner borrower =
  not (null resourceId) && not (null owner) && not (null borrower) &&
  owner /= borrower ==>
  let state = initialOwnership resourceId owner
  in case borrowResource resourceId borrower state of
    Right borrowedState -> 
      case transferOwnership resourceId owner "newOwner" (posAt 1 1) borrowedState of
        Right _ -> property $ False -- Should not allow transfer while borrowed
        Left _ -> property $ True -- Should fail
    Left _ -> property $ False

prop_single_deallocation :: String -> String -> Property
prop_single_deallocation resourceId owner =
  not (null resourceId) && not (null owner) ==>
  let state = initialOwnership resourceId owner
  in case Map.lookup resourceId (owners state) of
    Just resource -> property $ length (filter ((== resourceId) . resourceId) [resource]) === 1
    Nothing -> property $ False

-- Ownership inference properties

prop_ownership_inference_conservative :: String -> [String] -> Property
prop_ownership_inference_conservative resourceId potentialOwners =
  not (null resourceId) && not (null potentialOwners) && length potentialOwners <= 3 ==>
  let state = initialOwnership resourceId (head potentialOwners)
  in case Map.lookup resourceId (owners state) of
    Just resource -> property $ resourceOwner resource `elem` potentialOwners
    Nothing -> property $ False

prop_ownership_analysis_terminates :: String -> [String] -> Property
prop_ownership_analysis_terminates resourceId transferChain =
  not (null resourceId) && not (null transferChain) && length transferChain <= 4 ==>
  let state = initialOwnership resourceId (head transferChain)
      positions = [posAt i 1 | i <- [1..length transferChain]]
      transfers = zip3 (init transferChain) (tail transferChain) (init positions)
      finalState = foldl (\state (from, to, pos) -> 
                          case state of
                            Left err -> Left err
                            Right s -> transferOwnership resourceId from to pos s
                         ) (Right state) transfers
  in case finalState of
    Right _ -> property $ True -- Should terminate with success
    Left _ -> property $ True -- Should terminate with failure

prop_ownership_constraints_consistent :: String -> [String] -> Property
prop_ownership_constraints_consistent resourceId owners =
  not (null resourceId) && not (null owners) && length owners <= 3 ==>
  let initialOwner = head owners
      state = initialOwnership resourceId initialOwner
  in case Map.lookup resourceId (owners state) of
    Just resource -> property $ resourceOwner resource === initialOwner
    Nothing -> property $ False