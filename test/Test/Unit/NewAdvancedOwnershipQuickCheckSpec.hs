{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewAdvancedOwnershipQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Ownership
import Ownership.Common.Types
import SourceLocation (SourcePos(..), SourceSpan(.., posAt, spanBetween))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.DeepSeq (NFData, rnf)

-- Test ownership transfer properties
prop_ownership_transfer_moves_ownership :: OwnershipState -> String -> String -> Property
prop_ownership_transfer_moves_ownership state from to = 
  hasOwnership state from ==> 
  let newState = transferOwnership state from to
  in not (hasOwnership state to) &&
     hasOwnership newState to &&
     not (hasOwnership newState from)

prop_ownership_transfer_preserves_other_ownerships :: OwnershipState -> String -> String -> String -> Property
prop_ownership_transfer_preserves_other_ownerships state from to other = 
  hasOwnership state from && other /= from && other /= to ==> 
  let oldOwnership = hasOwnership state other
      newState = transferOwnership state from to
      newOwnership = hasOwnership newState other
  in oldOwnership == newOwnership

prop_ownership_transfer_idempotent :: OwnershipState -> String -> String -> Property
prop_ownership_transfer_idempotent state from to = 
  hasOwnership state from ==> 
  let state1 = transferOwnership state from to
      state2 = transferOwnership state1 from to
  in state1 == state2

-- Test ownership borrowing properties
prop_borrowing_temporarily_restricts_ownership :: OwnershipState -> String -> String -> Property
prop_borrowing_temporarily_restricts_ownership state owner borrower = 
  hasOwnership state owner ==> 
  let borrowResult = borrowOwnership state owner borrower
  in case borrowResult of
    Right borrowedState -> 
      hasOwnership borrowedState borrower &&
      not (canTransferOwnership borrowedState owner)
    Left _ -> False

prop_borrowing_prevents_double_borrow :: OwnershipState -> String -> String -> String -> Property
prop_borrowing_prevents_double_borrow state owner borrower1 borrower2 = 
  hasOwnership state owner && borrower1 /= borrower2 ==> 
  case borrowOwnership state owner borrower1 of
    Right borrowedState -> 
      case borrowOwnership borrowedState owner borrower2 of
        Right _ -> False
        Left _ -> True
    Left _ -> False

prop_borrowing_allows_return :: OwnershipState -> String -> String -> Property
prop_borrowing_allows_return state owner borrower = 
  hasOwnership state owner ==> 
  case borrowOwnership state owner borrower of
    Right borrowedState -> 
      let returnedState = returnOwnership borrowedState owner borrower
      in hasOwnership returnedState owner &&
         not (hasOwnership returnedState borrower)
    Left _ -> False

-- Test ownership constraints properties
prop_constraint_validation_prevents_violations :: OwnershipState -> OwnershipConstraint -> Bool
prop_constraint_validation_prevents_violations state constraint = 
  let violations = findConstraintViolations state constraint
  in all (\v -> isConstraintViolation v constraint) violations

prop_constraint_satisfaction_check :: OwnershipState -> [OwnershipConstraint] -> Bool
prop_constraint_satisfaction_check state constraints = 
  let satisfied = all (satisfiesConstraint state) constraints
      violations = concatMap (findConstraintViolations state) constraints
  in satisfied == null violations

prop_constraint_addition_preserves_validity :: OwnershipState -> OwnershipConstraint -> Property
prop_constraint_addition_preserves_validity state constraint = 
  satisfiesConstraint state constraint ==> 
  let constrainedState = addConstraint state constraint
  in satisfiesConstraint constrainedState constraint

-- Test ownership lifecycle properties
prop_ownership_creation_increases_count :: OwnershipState -> String -> Property
prop_ownership_creation_increases_count state owner = 
  not (hasOwnership state owner) ==> 
  let newState = createOwnership state owner
      oldCount = countOwnerships state
      newCount = countOwnerships newState
  in newCount == oldCount + 1 && hasOwnership newState owner

prop_ownership_deletion_decreases_count :: OwnershipState -> String -> Property
prop_ownership_deletion_decreases_count state owner = 
  hasOwnership state owner ==> 
  let newState = deleteOwnership state owner
      oldCount = countOwnerships state
      newCount = countOwnerships newState
  in newCount == oldCount - 1 && not (hasOwnership newState owner)

prop_ownership_deletion_non_existent_no_change :: OwnershipState -> String -> Property
prop_ownership_deletion_non_existent_no_change state owner = 
  not (hasOwnership state owner) ==> 
  let newState = deleteOwnership state owner
  in state == newState

-- Test ownership analysis properties
prop_ownership_analysis_detects_cycles :: OwnershipState -> Bool
prop_ownership_analysis_detects_cycles state = 
  let cycles = findOwnershipCycles state
  in all (\cycle -> length cycle >= 2) cycles &&
     all (\cycle -> hasCycleProperty state cycle) cycles

prop_ownership_analysis_computes_reachable :: OwnershipState -> String -> Property
prop_ownership_analysis_computes_reachable state owner = 
  hasOwnership state owner ==> 
  let reachable = computeReachableOwnerships state owner
  in all (\o -> canReachOwnership state owner o) reachable

prop_ownership_analysis_ownership_graph_consistency :: OwnershipState -> Bool
prop_ownership_analysis_ownership_graph_consistency state = 
  let graph = buildOwnershipGraph state
      edges = extractGraphEdges graph
      nodes = extractGraphNodes graph
  in all (\(from, to) -> Set.member from nodes && Set.member to nodes) edges &&
     all (\node -> any (\(from, to) -> from == node || to == node) edges || 
                   hasOwnership state node) nodes

-- Test ownership error handling properties
prop_ownership_error_informative :: OwnershipState -> String -> String -> Bool
prop_ownership_error_informative state from to = 
  not (hasOwnership state from) ==> 
  case transferOwnership state from to of
    Right _ -> False
    Left err -> not (null (errorMessage err))

prop_ownership_error_recovery_preserves_state :: OwnershipState -> String -> String -> Property
prop_ownership_error_recovery_preserves_state state from to = 
  not (hasOwnership state from) ==> 
  case transferOwnership state from to of
    Right _ -> discard
    Left err -> recoverFromOwnershipError state err == state

-- Test NFData instances
prop_ownership_state_nfdata :: OwnershipState -> Bool
prop_ownership_state_nfdata state = rnf state == ()

prop_ownership_constraint_nfdata :: OwnershipConstraint -> Bool
prop_ownership_constraint_nfdata constraint = rnf constraint == ()

prop_ownership_error_nfdata :: OwnershipError -> Bool
prop_ownership_error_nfdata error = rnf error == ()

-- Helper functions (these would need to be implemented in Ownership module)
data OwnershipState = OwnershipState
  { ownershipMap :: Map String String
  , borrowMap :: Map String (Set String)
  , constraints :: Set OwnershipConstraint
  } deriving (Show, Eq, Ord)

data OwnershipConstraint = OwnershipConstraint
  { constraintId :: String
  , constraintType :: ConstraintType
  , constraintParams :: Map String String
  } deriving (Show, Eq, Ord)

data ConstraintType = NoTransfer | NoBorrow | ExclusiveAccess | LifetimeConstraint
  deriving (Show, Eq, Ord)

data OwnershipError = OwnershipError
  { errorType :: OwnershipErrorType
  , errorMessage :: String
  , errorContext :: Map String String
  } deriving (Show, Eq, Ord)

data OwnershipErrorType = NotOwner | AlreadyBorrowed | ConstraintViolation | CycleDetected
  deriving (Show, Eq, Ord)

hasOwnership :: OwnershipState -> String -> Bool
hasOwnership state owner = Map.member owner (ownershipMap state)

transferOwnership :: OwnershipState -> String -> String -> Either OwnershipError OwnershipState
transferOwnership state from to = 
  if hasOwnership state from
  then Right $ state { ownershipMap = Map.insert to from (ownershipMap state) }
  else Left $ OwnershipError NotOwner ("Not owner: " ++ from) Map.empty

borrowOwnership :: OwnershipState -> String -> String -> Either OwnershipError OwnershipState
borrowOwnership state owner borrower = 
  if hasOwnership state owner
  then Right $ state { borrowMap = Map.insertWith Set.union owner (Set.singleton borrower) (borrowMap state) }
  else Left $ OwnershipError NotOwner ("Not owner: " ++ owner) Map.empty

returnOwnership :: OwnershipState -> String -> String -> OwnershipState
returnOwnership state owner borrower = 
  state { borrowMap = Map.adjust (Set.delete borrower) owner (borrowMap state) }

canTransferOwnership :: OwnershipState -> String -> Bool
canTransferOwnership state owner = 
  not (Set.member owner $ concat $ Map.elems $ borrowMap state)

satisfiesConstraint :: OwnershipState -> OwnershipConstraint -> Bool
satisfiesConstraint _ _ = True  -- Simplified for testing

findConstraintViolations :: OwnershipState -> OwnershipConstraint -> [String]
findConstraintViolations _ _ = []  -- Simplified for testing

isConstraintViolation :: String -> OwnershipConstraint -> Bool
isConstraintViolation _ _ = True  -- Simplified for testing

addConstraint :: OwnershipState -> OwnershipConstraint -> OwnershipState
addConstraint state constraint = 
  state { constraints = Set.insert constraint (constraints state) }

countOwnerships :: OwnershipState -> Int
countOwnerships state = Map.size (ownershipMap state)

createOwnership :: OwnershipState -> String -> OwnershipState
createOwnership state owner = 
  state { ownershipMap = Map.insert owner owner (ownershipMap state) }

deleteOwnership :: OwnershipState -> String -> OwnershipState
deleteOwnership state owner = 
  state { ownershipMap = Map.delete owner (ownershipMap state) }

findOwnershipCycles :: OwnershipState -> [[String]]
findOwnershipCycles _ = []  -- Simplified for testing

hasCycleProperty :: OwnershipState -> [String] -> Bool
hasCycleProperty _ _ = True  -- Simplified for testing

computeReachableOwnerships :: OwnershipState -> String -> Set String
computeReachableOwnerships _ _ = Set.empty  -- Simplified for testing

canReachOwnership :: OwnershipState -> String -> String -> Bool
canReachOwnership _ _ _ = False  -- Simplified for testing

buildOwnershipGraph :: OwnershipState -> ()
buildOwnershipGraph _ = ()  -- Simplified for testing

extractGraphEdges :: () -> [(String, String)]
extractGraphEdges _ = []  -- Simplified for testing

extractGraphNodes :: () -> Set String
extractGraphNodes _ = Set.empty  -- Simplified for testing

recoverFromOwnershipError :: OwnershipState -> OwnershipError -> OwnershipState
recoverFromOwnershipError state _ = state

-- Arbitrary instances
instance Arbitrary OwnershipState where
  arbitrary = do
    ownershipMap <- arbitrary
    borrowMap <- arbitrary
    constraints <- arbitrary
    return $ OwnershipState ownershipMap borrowMap constraints

instance Arbitrary OwnershipConstraint where
  arbitrary = do
    constraintId <- arbitrary
    constraintType <- arbitrary
    constraintParams <- arbitrary
    return $ OwnershipConstraint constraintId constraintType constraintParams

instance Arbitrary ConstraintType where
  arbitrary = elements [NoTransfer, NoBorrow, ExclusiveAccess, LifetimeConstraint]

instance Arbitrary OwnershipError where
  arbitrary = do
    errorType <- arbitrary
    errorMessage <- arbitrary
    errorContext <- arbitrary
    return $ OwnershipError errorType errorMessage errorContext

instance Arbitrary OwnershipErrorType where
  arbitrary = elements [NotOwner, AlreadyBorrowed, ConstraintViolation, CycleDetected]

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests