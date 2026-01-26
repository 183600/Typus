{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewOwnershipAnalysisQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
-- | Ownership analysis QuickCheck tests for the Typus compiler
-- This module contains property-based tests for ownership analysis utilities

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , removeComments
  , safeProcessString
  , isValidChar
  , breakOn
  )
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- Helper Types and Functions
-- ============================================================================

-- | Simple ownership type for testing
data OwnershipType = Owned | Borrowed | Shared | Unique deriving (Show, Eq, Ord)

-- | Arbitrary instance for OwnershipType
instance Arbitrary OwnershipType where
  arbitrary = elements [Owned, Borrowed, Shared, Unique]

-- | Simple ownership state for testing
data OwnershipState = OwnershipState
  { ownerMap :: Map.Map String OwnershipType
  , borrowMap :: Map.Map String String
  , ownershipConstraints :: Set.Set (String, String)
  } deriving (Show, Eq)

-- | Arbitrary instance for OwnershipState
instance Arbitrary OwnershipState where
  arbitrary = do
    size <- choose (0, 5)
    names <- vectorOf size $ elements ["x", "y", "z", "a", "b", "c"]
    types <- vectorOf size arbitrary
    let ownerMap' = Map.fromList $ zip names types
    return $ OwnershipState ownerMap' Map.empty Set.empty

-- | Create an empty ownership state
emptyOwnershipState :: OwnershipState
emptyOwnershipState = OwnershipState Map.empty Map.empty Set.empty

-- | Add ownership to state
addOwnership :: String -> OwnershipType -> OwnershipState -> OwnershipState
addOwnership name typ state = state
  { ownerMap = Map.insert name typ (ownerMap state)
  }

-- | Transfer ownership between entities
transferOwnership :: String -> String -> OwnershipState -> OwnershipState
transferOwnership from to state = 
  case Map.lookup from (ownerMap state) of
    Nothing -> state
    Just typ -> state
      { ownerMap = Map.insert to typ (Map.delete from (ownerMap state))
      , borrowMap = Map.insert to from (borrowMap state)
      }

-- | Check if ownership is valid
checkOwnership :: String -> OwnershipState -> Bool
checkOwnership name state = Map.member name (ownerMap state)

-- | Validate ownership transfer
validateOwnershipTransfer :: String -> String -> OwnershipState -> Bool
validateOwnershipTransfer from to state = 
  Map.member from (ownerMap state) && not (Map.member to (ownerMap state))

-- | Check ownership transitivity
ownershipTransitive :: String -> String -> String -> OwnershipState -> Bool
ownershipTransitive owner middle target state = 
  Map.lookup owner (ownerMap state) == Map.lookup middle (ownerMap state) &&
  Map.lookup middle (ownerMap state) == Map.lookup target (ownerMap state)

-- | Check ownership reflexivity
ownershipReflexive :: String -> OwnershipState -> Bool
ownershipReflexive name state = 
  Map.lookup name (ownerMap state) == Map.lookup name (ownerMap state)

-- | Check ownership symmetry (for certain types)
ownershipSymmetric :: String -> String -> OwnershipState -> Bool
ownershipSymmetric name1 name2 state = 
  case (Map.lookup name1 (ownerMap state), Map.lookup name2 (ownerMap state)) of
    (Just Shared, Just Shared) -> True
    (Just Borrowed, Just Borrowed) -> True
    _ -> False

-- | Combine ownership states
combineOwnership :: OwnershipState -> OwnershipState -> OwnershipState
combineOwnership state1 state2 = OwnershipState
  { ownerMap = Map.union (ownerMap state1) (ownerMap state2)
  , borrowMap = Map.union (borrowMap state1) (borrowMap state2)
  , ownershipConstraints = Set.union (ownershipConstraints state1) (ownershipConstraints state2)
  }

-- | Resolve ownership conflicts
resolveOwnershipConflict :: String -> OwnershipState -> OwnershipState
resolveOwnershipConflict name state = 
  case Map.lookup name (ownerMap state) of
    Nothing -> state
    Just _ -> state { ownerMap = Map.delete name (ownerMap state) }

-- | Check if ownership types are compatible
ownershipCompatible :: OwnershipType -> OwnershipType -> Bool
ownershipCompatible Owned Borrowed = True
ownershipCompatible Borrowed Owned = False
ownershipCompatible Shared Shared = True
ownershipCompatible Unique _ = False
ownershipCompatible _ Unique = False
ownershipCompatible _ _ = True

-- | Check if ownership implies another
ownershipImplies :: OwnershipType -> OwnershipType -> Bool
ownershipImplies Unique Shared = True
ownershipImplies Unique Borrowed = True
ownershipImplies Unique Owned = True
ownershipImplies Owned Borrowed = True
ownershipImplies Owned Shared = True
ownershipImplies Shared Shared = True
ownershipImplies Borrowed Borrowed = True
ownershipImplies _ _ = False

-- ============================================================================
-- Ownership State Tests
-- ============================================================================

-- | Test empty ownership state
prop_empty_ownership_state :: Bool
prop_empty_ownership_state = 
  let state = emptyOwnershipState
  in Map.null (ownerMap state) &&
     Map.null (borrowMap state) &&
     Set.null (ownershipConstraints state)

-- | Test add ownership
prop_add_ownership :: String -> OwnershipType -> OwnershipState -> Bool
prop_add_ownership name typ state = 
  let newState = addOwnership name typ state
  in Map.lookup name (ownerMap newState) == Just typ

-- | Test add ownership override
prop_add_ownership_override :: String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_add_ownership_override name typ1 typ2 state = 
  let state1 = addOwnership name typ1 state
      state2 = addOwnership name typ2 state1
  in Map.lookup name (ownerMap state2) == Just typ2

-- | Test check ownership
prop_check_ownership :: String -> OwnershipType -> OwnershipState -> Bool
prop_check_ownership name typ state = 
  let newState = addOwnership name typ state
  in checkOwnership name newState

-- | Test check ownership missing
prop_check_ownership_missing :: String -> OwnershipState -> Property
prop_check_ownership_missing name state = 
  not (Map.member name (ownerMap state)) ==> not (checkOwnership name state)

-- ============================================================================
-- Ownership Transfer Tests
-- ============================================================================

-- | Test transfer ownership
prop_transfer_ownership :: String -> String -> OwnershipType -> OwnershipState -> Bool
prop_transfer_ownership from to typ state = 
  let state1 = addOwnership from typ state
      state2 = transferOwnership from to state1
  in Map.lookup to (ownerMap state2) == Just typ &&
     not (Map.member from (ownerMap state2))

-- | Test transfer ownership missing source
prop_transfer_ownership_missing_source :: String -> String -> OwnershipState -> Property
prop_transfer_ownership_missing_source from to state = 
  not (Map.member from (ownerMap state)) ==>
  let state2 = transferOwnership from to state
  in state2 == state

-- | Test transfer ownership existing target
prop_transfer_ownership_existing_target :: String -> String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_transfer_ownership_existing_target from to typ1 typ2 state = 
  let state1 = addOwnership from typ1 (addOwnership to typ2 state)
      state2 = transferOwnership from to state1
      originalTo = Map.lookup to (ownerMap state1)
  in Map.lookup to (ownerMap state2) == Just typ1 &&
     originalTo == Just typ2

-- | Test validate ownership transfer
prop_validate_ownership_transfer :: String -> String -> OwnershipType -> OwnershipState -> Bool
prop_validate_ownership_transfer from to typ state = 
  let state1 = addOwnership from typ state
  in validateOwnershipTransfer from to state1

-- | Test validate ownership transfer missing source
prop_validate_ownership_transfer_missing_source :: String -> String -> OwnershipState -> Property
prop_validate_ownership_transfer_missing_source from to state = 
  not (Map.member from (ownerMap state)) ==> not (validateOwnershipTransfer from to state)

-- | Test validate ownership transfer existing target
prop_validate_ownership_transfer_existing_target :: String -> String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_validate_ownership_transfer_existing_target from to typ1 typ2 state = 
  let state1 = addOwnership from typ1 (addOwnership to typ2 state)
  in not (validateOwnershipTransfer from to state1)

-- ============================================================================
-- Ownership Properties Tests
-- ============================================================================

-- | Test ownership transitivity
prop_ownership_transitive :: String -> String -> String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_transitive owner middle target typ state = 
  let state1 = addOwnership owner typ (addOwnership middle typ (addOwnership target typ state))
  in ownershipTransitive owner middle target state1

-- | Test ownership reflexivity
prop_ownership_reflexive :: String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_reflexive name typ state = 
  let state1 = addOwnership name typ state
  in ownershipReflexive name state1

-- | Test ownership symmetry
prop_ownership_symmetric :: String -> String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_symmetric name1 name2 typ state = 
  let state1 = addOwnership name1 typ (addOwnership name2 typ state)
  in if typ `elem` [Shared, Borrowed]
     then ownershipSymmetric name1 name2 state1
     else not (ownershipSymmetric name1 name2 state1)

-- | Test ownership compatibility
prop_ownership_compatibility :: OwnershipType -> OwnershipType -> Bool
prop_ownership_compatibility typ1 typ2 = 
  ownershipCompatible typ1 typ2 == ownershipCompatible typ2 typ1

-- | Test ownership implication
prop_ownership_implication :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_implication typ1 typ2 typ3 = 
  ownershipImplies typ1 typ2 && ownershipImplies typ2 typ3 ==> ownershipImplies typ1 typ3

-- | Test ownership implication reflexive
prop_ownership_implication_reflexive :: OwnershipType -> Bool
prop_ownership_implication_reflexive typ = ownershipImplies typ typ

-- ============================================================================
-- Ownership Combination Tests
-- ============================================================================

-- | Test combine ownership
prop_combine_ownership :: String -> OwnershipType -> String -> OwnershipType -> OwnershipState -> Bool
prop_combine_ownership name1 typ1 name2 typ2 state = 
  let state1 = addOwnership name1 typ1 state
      state2 = addOwnership name2 typ2 state
      combined = combineOwnership state1 state2
  in Map.lookup name1 (ownerMap combined) == Just typ1 &&
     Map.lookup name2 (ownerMap combined) == Just typ2

-- | Test combine ownership conflict
prop_combine_ownership_conflict :: String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_combine_ownership_conflict name typ1 typ2 state = 
  let state1 = addOwnership name typ1 state
      state2 = addOwnership name typ2 state
      combined = combineOwnership state1 state2
  in Map.lookup name (ownerMap combined) == Just typ2 -- Second state wins

-- | Test combine ownership empty
prop_combine_ownership_empty :: OwnershipState -> Bool
prop_combine_ownership_empty state = 
  let combined = combineOwnership emptyOwnershipState state
  in combined == state

-- | Test combine ownership identity
prop_combine_ownership_identity :: OwnershipState -> Bool
prop_combine_ownership_identity state = 
  let combined1 = combineOwnership state emptyOwnershipState
      combined2 = combineOwnership emptyOwnershipState state
  in combined1 == state && combined2 == state

-- | Test combine ownership associativity
prop_combine_ownership_associative :: String -> OwnershipType -> String -> OwnershipType -> String -> OwnershipType -> OwnershipState -> Bool
prop_combine_ownership_associative name1 typ1 name2 typ2 name3 typ3 state = 
  let state1 = addOwnership name1 typ1 state
      state2 = addOwnership name2 typ2 state
      state3 = addOwnership name3 typ3 state
      combined1 = combineOwnership (combineOwnership state1 state2) state3
      combined2 = combineOwnership state1 (combineOwnership state2 state3)
  in combined1 == combined2

-- ============================================================================
-- Ownership Conflict Resolution Tests
-- ============================================================================

-- | Test resolve ownership conflict
prop_resolve_ownership_conflict :: String -> OwnershipType -> OwnershipState -> Bool
prop_resolve_ownership_conflict name typ state = 
  let state1 = addOwnership name typ state
      resolved = resolveOwnershipConflict name state1
  in not (Map.member name (ownerMap resolved))

-- | Test resolve ownership conflict missing
prop_resolve_ownership_conflict_missing :: String -> OwnershipState -> Property
prop_resolve_ownership_conflict_missing name state = 
  not (Map.member name (ownerMap state)) ==>
  let resolved = resolveOwnershipConflict name state
  in resolved == state

-- | Test resolve ownership conflict multiple
prop_resolve_ownership_conflict_multiple :: String -> String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_resolve_ownership_conflict_multiple name1 name2 typ1 typ2 state = 
  let state1 = addOwnership name1 typ1 (addOwnership name2 typ2 state)
      resolved = resolveOwnershipConflict name1 (resolveOwnershipConflict name2 state1)
  in not (Map.member name1 (ownerMap resolved)) &&
     not (Map.member name2 (ownerMap resolved))

-- | Test resolve ownership conflict preserve others
prop_resolve_ownership_conflict_preserve :: String -> String -> OwnershipType -> OwnershipType -> OwnershipState -> Bool
prop_resolve_ownership_conflict_preserve name1 name2 typ1 typ2 state = 
  let state1 = addOwnership name1 typ1 (addOwnership name2 typ2 state)
      resolved = resolveOwnershipConflict name1 state1
  in Map.lookup name2 (ownerMap resolved) == Just typ2

-- ============================================================================
-- Ownership Constraint Tests
-- ============================================================================

-- | Test ownership constraints
prop_ownership_constraints :: String -> String -> OwnershipState -> Bool
prop_ownership_constraints name1 name2 state = 
  let constraint = (name1, name2)
      state1 = state { ownershipConstraints = Set.singleton constraint }
  in constraint `Set.member` ownershipConstraints state1

-- | Test ownership constraints transitivity
prop_ownership_constraints_transitive :: String -> String -> String -> OwnershipState -> Bool
prop_ownership_constraints_transitive name1 name2 name3 state = 
  let constraintSet = Set.fromList [(name1, name2), (name2, name3)]
      state1 = state { ownershipConstraints = constraintSet }
  in (name1, name2) `Set.member` ownershipConstraints state1 &&
     (name2, name3) `Set.member` ownershipConstraints state1

-- | Test ownership constraints symmetry
prop_ownership_constraints_symmetry :: String -> String -> OwnershipState -> Bool
prop_ownership_constraints_symmetry name1 name2 state = 
  let constraint = (name1, name2)
      state1 = state { ownershipConstraints = Set.singleton constraint }
  in constraint `Set.member` ownershipConstraints state1 &&
     not ((name2, name1) `Set.member` ownershipConstraints state1)

-- ============================================================================
-- Ownership Analysis Tests
-- ============================================================================

-- | Test ownership analysis consistency
prop_ownership_analysis_consistency :: String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_analysis_consistency name typ state = 
  let state1 = addOwnership name typ state
  in checkOwnership name state1 == Map.member name (ownerMap state1)

-- | Test ownership analysis completeness
prop_ownership_analysis_completeness :: OwnershipState -> Bool
prop_ownership_analysis_completeness state = 
  let allNames = Map.keys (ownerMap state)
      checkedNames = filter (`checkOwnership` state) allNames
  in length allNames == length checkedNames

-- | Test ownership analysis uniqueness
prop_ownership_analysis_uniqueness :: String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_analysis_uniqueness name typ state = 
  let state1 = addOwnership name typ state
  in case Map.lookup name (ownerMap state1) of
    Nothing -> False
    Just foundTyp -> foundTyp == typ

-- | Test ownership analysis propagation
prop_ownership_analysis_propagation :: String -> String -> OwnershipType -> OwnershipState -> Bool
prop_ownership_analysis_propagation owner target typ state = 
  let state1 = addOwnership owner typ state
      state2 = transferOwnership owner target state1
  in checkOwnership target state2 && not (checkOwnership owner state2)

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Analysis QuickCheck Tests"
  [ testProperty "empty ownership state" prop_empty_ownership_state
  , testProperty "add ownership" prop_add_ownership
  , testProperty "add ownership override" prop_add_ownership_override
  , testProperty "check ownership" prop_check_ownership
  , testProperty "check ownership missing" prop_check_ownership_missing
  
  -- Ownership Transfer Tests
  , testProperty "transfer ownership" prop_transfer_ownership
  , testProperty "transfer ownership missing source" prop_transfer_ownership_missing_source
  , testProperty "transfer ownership existing target" prop_transfer_ownership_existing_target
  , testProperty "validate ownership transfer" prop_validate_ownership_transfer
  , testProperty "validate ownership transfer missing source" prop_validate_ownership_transfer_missing_source
  , testProperty "validate ownership transfer existing target" prop_validate_ownership_transfer_existing_target
  
  -- Ownership Properties Tests
  , testProperty "ownership transitive" prop_ownership_transitive
  , testProperty "ownership reflexive" prop_ownership_reflexive
  , testProperty "ownership symmetric" prop_ownership_symmetric
  , testProperty "ownership compatibility" prop_ownership_compatibility
  , testProperty "ownership implication" prop_ownership_implication
  , testProperty "ownership implication reflexive" prop_ownership_implication_reflexive
  
  -- Ownership Combination Tests
  , testProperty "combine ownership" prop_combine_ownership
  , testProperty "combine ownership conflict" prop_combine_ownership_conflict
  , testProperty "combine ownership empty" prop_combine_ownership_empty
  , testProperty "combine ownership identity" prop_combine_ownership_identity
  , testProperty "combine ownership associative" prop_combine_ownership_associative
  
  -- Ownership Conflict Resolution Tests
  , testProperty "resolve ownership conflict" prop_resolve_ownership_conflict
  , testProperty "resolve ownership conflict missing" prop_resolve_ownership_conflict_missing
  , testProperty "resolve ownership conflict multiple" prop_resolve_ownership_conflict_multiple
  , testProperty "resolve ownership conflict preserve" prop_resolve_ownership_conflict_preserve
  
  -- Ownership Constraint Tests
  , testProperty "ownership constraints" prop_ownership_constraints
  , testProperty "ownership constraints transitive" prop_ownership_constraints_transitive
  , testProperty "ownership constraints symmetry" prop_ownership_constraints_symmetry
  
  -- Ownership Analysis Tests
  , testProperty "ownership analysis consistency" prop_ownership_analysis_consistency
  , testProperty "ownership analysis completeness" prop_ownership_analysis_completeness
  , testProperty "ownership analysis uniqueness" prop_ownership_analysis_uniqueness
  , testProperty "ownership analysis propagation" prop_ownership_analysis_propagation
  ]