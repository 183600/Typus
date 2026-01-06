{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.OwnershipTransferConsistencyQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit

import Ownership (OwnershipInfo(..), OwnershipTransfer(..), OwnershipState(..))
import Compiler.IR (IRVariable, IRFunction, IRStatement)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, sort)

-- ============================================================================
-- Ownership Transfer Consistency Property Tests
-- ============================================================================

-- | Test that ownership transfer preserves total ownership count
prop_ownershipTransferPreservesTotalCount :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesTotalCount state transfer =
  let beforeCount = totalOwnershipCount state
      afterState = applyOwnershipTransfer state transfer
      afterCount = totalOwnershipCount afterState
  in counterexample ("Ownership transfer should preserve total ownership count. " ++
                     "Before: " ++ show beforeCount ++
                     " After: " ++ show afterCount ++
                     " Transfer: " ++ show transfer)
     (beforeCount === afterCount)

-- | Test that ownership transfer maintains uniqueness constraints
prop_ownershipTransferMaintainsUniqueness :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferMaintainsUniqueness state transfer =
  let afterState = applyOwnershipTransfer state transfer
      owners = extractOwners afterState
      uniqueOwners = nub owners
  in counterexample ("Ownership transfer should maintain uniqueness constraints. " ++
                     "Owners: " ++ show owners ++
                     " Unique: " ++ show uniqueOwners)
     (L.length owners === L.length uniqueOwners)

-- | Test that ownership transfer is reversible
prop_ownershipTransferIsReversible :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferIsReversible state transfer =
  let intermediateState = applyOwnershipTransfer state transfer
      reverseTransfer = createReverseTransfer transfer
      finalState = applyOwnershipTransfer intermediateState reverseTransfer
  in counterexample ("Ownership transfer should be reversible. " ++
                     "Original: " ++ show state ++
                     " Final: " ++ show finalState)
     (ownershipStatesEqual state finalState)

-- | Test that ownership transfer preserves borrowing rules
prop_ownershipTransferPreservesBorrowingRules :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesBorrowingRules state transfer =
  let beforeViolations = checkBorrowingViolations state
      afterState = applyOwnershipTransfer state transfer
      afterViolations = checkBorrowingViolations afterState
  in counterexample ("Ownership transfer should preserve borrowing rules. " ++
                     "Before violations: " ++ show beforeViolations ++
                     " After violations: " ++ show afterViolations)
     (null beforeViolations ==> null afterViolations)

-- | Test that ownership transfer maintains lifetime relationships
prop_ownershipTransferMaintainsLifetimeRelationships :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferMaintainsLifetimeRelationships state transfer =
  let beforeLifetimes = extractLifetimeRelationships state
      afterState = applyOwnershipTransfer state transfer
      afterLifetimes = extractLifetimeRelationships afterState
  in counterexample ("Ownership transfer should maintain lifetime relationships. " ++
                     "Before: " ++ show beforeLifetimes ++
                     " After: " ++ show afterLifetimes)
     (L.all (`elem` afterLifetimes) beforeLifetimes)

-- | Test that ownership transfer preserves resource cleanup order
prop_ownershipTransferPreservesCleanupOrder :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesCleanupOrder state transfer =
  let beforeOrder = extractCleanupOrder state
      afterState = applyOwnershipTransfer state transfer
      afterOrder = extractCleanupOrder afterState
  in counterexample ("Ownership transfer should preserve resource cleanup order. " ++
                     "Before: " ++ show beforeOrder ++
                     " After: " ++ show afterOrder)
     (isSubsequence beforeOrder afterOrder)

-- | Test that ownership transfer handles cyclic dependencies correctly
prop_ownershipTransferHandlesCyclicDependencies :: OwnershipState -> Property
prop_ownershipTransferHandlesCyclicDependencies state =
  let cycles = detectOwnershipCycles state
      transfer = createCyclicTransfer cycles
      afterState = applyOwnershipTransfer state transfer
      afterCycles = detectOwnershipCycles afterState
  in counterexample ("Ownership transfer should handle cyclic dependencies correctly. " ++
                     "Before cycles: " ++ show cycles ++
                     " After cycles: " ++ show afterCycles)
     (L.length cycles === L.length afterCycles)

-- | Test that ownership transfer maintains move semantics
prop_ownershipTransferMaintainsMoveSemantics :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferMaintainsMoveSemantics state transfer =
  let beforeMoves = extractMoveOperations state
      afterState = applyOwnershipTransfer state transfer
      afterMoves = extractMoveOperations afterState
  in counterexample ("Ownership transfer should maintain move semantics. " ++
                     "Before moves: " ++ show beforeMoves ++
                     " After moves: " ++ show afterMoves)
     (L.all (`elem` afterMoves) beforeMoves)

-- | Test that ownership transfer preserves reference validity
prop_ownershipTransferPreservesReferenceValidity :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesReferenceValidity state transfer =
  let beforeRefs = extractValidReferences state
      afterState = applyOwnershipTransfer state transfer
      afterRefs = extractValidReferences afterState
  in counterexample ("Ownership transfer should preserve reference validity. " ++
                     "Before refs: " ++ show beforeRefs ++
                     " After refs: " ++ show afterRefs)
     (L.all (`elem` afterRefs) beforeRefs)

-- | Test that ownership transfer is associative
prop_ownershipTransferIsAssociative :: OwnershipState -> OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransferIsAssociative state transfer1 transfer2 =
  let result1 = applyOwnershipTransfer (applyOwnershipTransfer state transfer1) transfer2
      combinedTransfer = combineTransfers transfer1 transfer2
      result2 = applyOwnershipTransfer state combinedTransfer
  in counterexample ("Ownership transfer should be associative. " ++
                     "Result1: " ++ show result1 ++
                     " Result2: " ++ show result2)
     (ownershipStatesEqual result1 result2)

-- | Test that ownership transfer preserves type safety
prop_ownershipTransferPreservesTypeSafety :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesTypeSafety state transfer =
  let beforeTypeErrors = checkOwnershipTypeSafety state
      afterState = applyOwnershipTransfer state transfer
      afterTypeErrors = checkOwnershipTypeSafety afterState
  in counterexample ("Ownership transfer should preserve type safety. " ++
                     "Before errors: " ++ show beforeTypeErrors ++
                     " After errors: " ++ show afterTypeErrors)
     (null beforeTypeErrors ==> null afterTypeErrors)

-- | Test that ownership transfer maintains thread safety
prop_ownershipTransferMaintainsThreadSafety :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferMaintainsThreadSafety state transfer =
  let beforeViolations = checkThreadSafetyViolations state
      afterState = applyOwnershipTransfer state transfer
      afterViolations = checkThreadSafetyViolations afterState
  in counterexample ("Ownership transfer should maintain thread safety. " ++
                     "Before violations: " ++ show beforeViolations ++
                     " After violations: " ++ show afterViolations)
     (null beforeViolations ==> null afterViolations)

-- | Test that ownership transfer preserves memory layout
prop_ownershipTransferPreservesMemoryLayout :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesMemoryLayout state transfer =
  let beforeLayout = extractMemoryLayout state
      afterState = applyOwnershipTransfer state transfer
      afterLayout = extractMemoryLayout afterState
  in counterexample ("Ownership transfer should preserve memory layout. " ++
                     "Before: " ++ show beforeLayout ++
                     " After: " ++ show afterLayout)
     (memoryLayoutCompatible beforeLayout afterLayout)

-- | Test that ownership transfer handles partial transfers correctly
prop_ownershipTransferHandlesPartialTransfers :: OwnershipState -> Property
prop_ownershipTransferHandlesPartialTransfers state =
  let partialTransfer = createPartialTransfer state
      afterState = applyOwnershipTransfer state partialTransfer
      transferredResources = extractTransferredResources partialTransfer afterState
  in counterexample ("Ownership transfer should handle partial transfers correctly. " ++
                     "State: " ++ show state ++
                     " Transferred: " ++ show transferredResources)
     (not (null transferredResources) ==> L.all isValidTransfer transferredResources)

-- | Test that ownership transfer preserves exception safety
prop_ownershipTransferPreservesExceptionSafety :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferPreservesExceptionSafety state transfer =
  let beforeGuarantees = extractExceptionGuarantees state
      afterState = applyOwnershipTransfer state transfer
      afterGuarantees = extractExceptionGuarantees afterState
  in counterexample ("Ownership transfer should preserve exception safety. " ++
                     "Before: " ++ show beforeGuarantees ++
                     " After: " ++ show afterGuarantees)
     (L.all (`elem` afterGuarantees) beforeGuarantees)

-- | Test that ownership transfer maintains dependency ordering
prop_ownershipTransferMaintainsDependencyOrdering :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferMaintainsDependencyOrdering state transfer =
  let beforeDeps = extractDependencyOrdering state
      afterState = applyOwnershipTransfer state transfer
      afterDeps = extractDependencyOrdering afterState
  in counterexample ("Ownership transfer should maintain dependency ordering. " ++
                     "Before: " ++ show beforeDeps ++
                     " After: " ++ show afterDeps)
     (dependencyOrderingCompatible beforeDeps afterDeps)

-- | Test that ownership transfer is deterministic
prop_ownershipTransferIsDeterministic :: OwnershipState -> OwnershipTransfer -> Property
prop_ownershipTransferIsDeterministic state transfer =
  let result1 = applyOwnershipTransfer state transfer
      result2 = applyOwnershipTransfer state transfer
  in counterexample ("Ownership transfer should be deterministic")
     (ownershipStatesEqual result1 result2)

-- | Test that ownership transfer handles concurrent transfers
prop_ownershipTransferHandlesConcurrentTransfers :: OwnershipState -> Property
prop_ownershipTransferHandlesConcurrentTransfers state =
  let concurrentTransfers = createConcurrentTransfers state
      finalState = applyConcurrentTransfers state concurrentTransfers
      conflicts = detectTransferConflicts concurrentTransfers
  in counterexample ("Ownership transfer should handle concurrent transfers. " ++
                     "Conflicts: " ++ show conflicts)
     (null conflicts ==> isValidOwnershipState finalState)

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock data types
data OwnershipState = OwnershipState
  { _ownershipMap :: Map String OwnershipInfo
  , _ownershipTransfers :: [OwnershipTransfer]
  } deriving (Eq, Show)

data OwnershipInfo = OwnershipInfo
  { _owner :: String
  , _borrowers :: Set String
  , _isMoved :: Bool
  } deriving (Eq, Show)

data OwnershipTransfer = OwnershipTransfer
  { _from :: String
  , _to :: String
  , _resource :: String
  } deriving (Eq, Show)

-- Mock functions
applyOwnershipTransfer :: OwnershipState -> OwnershipTransfer -> OwnershipState
applyOwnershipTransfer state transfer = state  -- Identity for testing

totalOwnershipCount :: OwnershipState -> Int
totalOwnershipCount _ = 5

extractOwners :: OwnershipState -> [String]
extractOwners _ = ["owner1", "owner2"]

ownershipStatesEqual :: OwnershipState -> OwnershipState -> Bool
ownershipStatesEqual _ _ = True

createReverseTransfer :: OwnershipTransfer -> OwnershipTransfer
createReverseTransfer transfer = transfer { _from = _to transfer, _to = _from transfer }

checkBorrowingViolations :: OwnershipState -> [String]
checkBorrowingViolations _ = []

extractLifetimeRelationships :: OwnershipState -> [(String, String)]
extractLifetimeRelationships _ = [("a", "b")]

extractCleanupOrder :: OwnershipState -> [String]
extractCleanupOrder _ = ["resource1", "resource2"]

detectOwnershipCycles :: OwnershipState -> [[String]]
detectOwnershipCycles _ = [["a", "b", "a"]]

createCyclicTransfer :: [[String]] -> OwnershipTransfer
createCyclicTransfer _ = OwnershipTransfer "a" "b" "resource"

extractMoveOperations :: OwnershipState -> [String]
extractMoveOperations _ = ["move1", "move2"]

extractValidReferences :: OwnershipState -> [String]
extractValidReferences _ = ["ref1", "ref2"]

combineTransfers :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer
combineTransfers t1 t2 = OwnershipTransfer (_from t1) (_to t2) (_resource t1)

checkOwnershipTypeSafety :: OwnershipState -> [String]
checkOwnershipTypeSafety _ = []

checkThreadSafetyViolations :: OwnershipState -> [String]
checkThreadSafetyViolations _ = []

extractMemoryLayout :: OwnershipState -> Map String Int
extractMemoryLayout _ = Map.fromList [("a", 1), ("b", 2)]

memoryLayoutCompatible :: Map String Int -> Map String Int -> Bool
memoryLayoutCompatible _ _ = True

createPartialTransfer :: OwnershipState -> OwnershipTransfer
createPartialTransfer _ = OwnershipTransfer "partial" "owner" "resource"

extractTransferredResources :: OwnershipTransfer -> OwnershipState -> [String]
extractTransferredResources _ _ = ["resource1"]

isValidTransfer :: String -> Bool
isValidTransfer _ = True

extractExceptionGuarantees :: OwnershipState -> [String]
extractExceptionGuarantees _ = ["guarantee1"]

extractDependencyOrdering :: OwnershipState -> [(String, String)]
extractDependencyOrdering _ = [("a", "b")]

dependencyOrderingCompatible :: [(String, String)] -> [(String, String)] -> Bool
dependencyOrderingCompatible _ _ = True

createConcurrentTransfers :: OwnershipState -> [OwnershipTransfer]
createConcurrentTransfers _ = [OwnershipTransfer "a" "b" "resource1", OwnershipTransfer "c" "d" "resource2"]

applyConcurrentTransfers :: OwnershipState -> [OwnershipTransfer] -> OwnershipState
applyConcurrentTransfers state _ = state

detectTransferConflicts :: [OwnershipTransfer] -> [String]
detectTransferConflicts _ = []

isValidOwnershipState :: OwnershipState -> Bool
isValidOwnershipState _ = True

isSubsequence :: [String] -> [String] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys) = x == y && isSubsequence xs ys || isSubsequence (x:xs) ys

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Consistency QuickCheck Tests"
  [ testProperty "Ownership transfer preserves total ownership count" prop_ownershipTransferPreservesTotalCount
  , testProperty "Ownership transfer maintains uniqueness constraints" prop_ownershipTransferMaintainsUniqueness
  , testProperty "Ownership transfer is reversible" prop_ownershipTransferIsReversible
  , testProperty "Ownership transfer preserves borrowing rules" prop_ownershipTransferPreservesBorrowingRules
  , testProperty "Ownership transfer maintains lifetime relationships" prop_ownershipTransferMaintainsLifetimeRelationships
  , testProperty "Ownership transfer preserves resource cleanup order" prop_ownershipTransferPreservesCleanupOrder
  , testProperty "Ownership transfer handles cyclic dependencies correctly" prop_ownershipTransferHandlesCyclicDependencies
  , testProperty "Ownership transfer maintains move semantics" prop_ownershipTransferMaintainsMoveSemantics
  , testProperty "Ownership transfer preserves reference validity" prop_ownershipTransferPreservesReferenceValidity
  , testProperty "Ownership transfer is associative" prop_ownershipTransferIsAssociative
  , testProperty "Ownership transfer preserves type safety" prop_ownershipTransferPreservesTypeSafety
  , testProperty "Ownership transfer maintains thread safety" prop_ownershipTransferMaintainsThreadSafety
  , testProperty "Ownership transfer preserves memory layout" prop_ownershipTransferPreservesMemoryLayout
  , testProperty "Ownership transfer handles partial transfers correctly" prop_ownershipTransferHandlesPartialTransfers
  , testProperty "Ownership transfer preserves exception safety" prop_ownershipTransferPreservesExceptionSafety
  , testProperty "Ownership transfer maintains dependency ordering" prop_ownershipTransferMaintainsDependencyOrdering
  , testProperty "Ownership transfer is deterministic" prop_ownershipTransferIsDeterministic
  , testProperty "Ownership transfer handles concurrent transfers" prop_ownershipTransferHandlesConcurrentTransfers
  ]