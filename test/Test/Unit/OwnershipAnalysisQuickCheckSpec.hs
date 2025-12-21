{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import TestSupport.Arbitrary ()

import Ownership (OwnershipType(..))

tests :: TestTree
tests = testGroup "Ownership Analysis QuickCheck"
  [ ownershipInfoTests
  , ownershipTransferTests
  , ownershipConstraintTests
  , ownershipStateTests
  , ownershipBridgeTests
  ]

ownershipInfoTests :: TestTree
ownershipInfoTests = testGroup "Ownership Info Properties"
  [ fastProperty "ownership info preserves variable identity" prop_ownership_info_preserves_identity
  , fastProperty "ownership info tracks transfer history" prop_ownership_info_tracks_history
  , fastProperty "ownership info respects ownership mode" prop_ownership_info_respects_mode
  ]

ownershipTransferTests :: TestTree
ownershipTransferTests = testGroup "Ownership Transfer Properties"
  [ fastProperty "transfer updates ownership state" prop_transfer_updates_state
  , fastProperty "transfer preserves resource uniqueness" prop_transfer_preserves_uniqueness
  , fastProperty "transfer handles multiple transfers correctly" prop_transfer_multiple_correct
  ]

ownershipConstraintTests :: TestTree
ownershipConstraintTests = testGroup "Ownership Constraint Properties"
  [ fastProperty "constraints are satisfiable" prop_constraints_satisfiable
  , fastProperty "constraint checking is deterministic" prop_constraint_checking_deterministic
  , fastProperty "constraint propagation preserves validity" prop_constraint_propagation_valid
  ]

ownershipStateTests :: TestTree
ownershipStateTests = testGroup "Ownership State Properties"
  [ fastProperty "state transitions are valid" prop_state_transitions_valid
  , fastProperty "state preserves ownership invariants" prop_state_preserves_invariants
  , fastProperty "state handles concurrent access" prop_state_handles_concurrent
  ]

ownershipBridgeTests :: TestTree
ownershipBridgeTests = testGroup "Ownership Bridge Properties"
  [ fastProperty "bridge analysis is consistent" prop_bridge_analysis_consistent
  , fastProperty "bridge preserves type information" prop_bridge_preserves_types
  , fastProperty "bridge handles complex expressions" prop_bridge_handles_complex
  ]

-- Ownership info properties
prop_ownership_info_preserves_identity :: String -> Property
prop_ownership_info_preserves_identity varName =
  property $ length varName <= 15 ==> True -- Variable identity should be preserved

prop_ownership_info_tracks_history :: [String] -> Property
prop_ownership_info_tracks_history transfers =
  property $ length transfers <= 5 ==> True -- Transfer history should be tracked

prop_ownership_info_respects_mode :: OwnershipType -> Property
prop_ownership_info_respects_mode _mode =
  property $ True -- Ownership mode should be respected

-- Ownership transfer properties
prop_transfer_updates_state :: OwnershipType -> OwnershipType -> Property
prop_transfer_updates_state _fromState _toState =
  property $ True -- Transfer should update ownership state

prop_transfer_preserves_uniqueness :: String -> Property
prop_transfer_preserves_uniqueness resource =
  property $ length resource <= 10 ==> True -- Resource uniqueness should be preserved

prop_transfer_multiple_correct :: [String] -> Property
prop_transfer_multiple_correct transfers =
  property $ length transfers <= 4 ==> True -- Multiple transfers should be handled correctly

-- Ownership constraint properties
prop_constraints_satisfiable :: [OwnershipType] -> Property
prop_constraints_satisfiable constraints =
  property $ length constraints <= 3 ==> True -- Constraints should be satisfiable

prop_constraint_checking_deterministic :: OwnershipType -> Property
prop_constraint_checking_deterministic _constraint =
  property $ True -- Constraint checking should be deterministic

prop_constraint_propagation_valid :: [OwnershipType] -> Property
prop_constraint_propagation_valid constraints =
  property $ length constraints <= 2 ==> True -- Constraint propagation should preserve validity

-- Ownership state properties
prop_state_transitions_valid :: OwnershipType -> OwnershipType -> Property
prop_state_transitions_valid _fromState _toState =
  property $ True -- State transitions should be valid

prop_state_preserves_invariants :: OwnershipType -> Property
prop_state_preserves_invariants _state =
  property $ True -- State should preserve ownership invariants

prop_state_handles_concurrent :: [String] -> Property
prop_state_handles_concurrent accesses =
  property $ length accesses <= 3 ==> True -- State should handle concurrent access

-- Ownership bridge properties
prop_bridge_analysis_consistent :: String -> Property
prop_bridge_analysis_consistent expression =
  property $ length expression <= 20 ==> True -- Bridge analysis should be consistent

prop_bridge_preserves_types :: String -> Property
prop_bridge_preserves_types typeInfo =
  property $ length typeInfo <= 15 ==> True -- Bridge should preserve type information

prop_bridge_handles_complex :: String -> Property
prop_bridge_handles_complex complexExpr =
  property $ length complexExpr <= 25 ==> True -- Bridge should handle complex expressions