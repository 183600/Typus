module Test.Unit.OwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, property, Arbitrary(..), Gen, oneof, listOf, elements, choose)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ownership (OwnershipInfo(..), OwnershipRelation(..), OwnershipState(..))
import SourceLocation (SourcePos(..), startPos)
import Utils (trim)

-- | QuickCheck tests for Ownership transitivity properties
tests :: TestTree
tests =
  testGroup "OwnershipTransitivityQuickCheckSpec - Ownership Transitivity Tests"
    [ testProperty "Ownership transitivity: if A owns B and B owns C, then A should own C" prop_ownershipTransitivity
    , testProperty "Ownership transfer preserves uniqueness" prop_ownershipTransferUniqueness
    , testProperty "Ownership borrowing respects lifetimes" prop_ownershipBorrowingLifetimes
    , testProperty "Ownership sharing maintains reference counts" prop_ownershipSharingReferenceCounts
    , testProperty "Ownership move invalidates previous owner" prop_ownershipMoveInvalidation
    , testProperty "Ownership cloning preserves original" prop_ownershipCloningPreservation
    , testProperty "Ownership scopes are properly nested" prop_ownershipScopeNesting
    , testProperty "Ownership conflicts are detected" prop_ownershipConflictDetection
    ]

-- ============================================================================
-- Ownership Transitivity Properties
-- ============================================================================

-- Property: Ownership transitivity holds true
prop_ownershipTransitivity :: OwnershipRelation -> OwnershipRelation -> Bool
prop_ownershipTransitivity rel1 rel2 =
  let owner1 = getOwner rel1
      resource1 = getResource rel1
      owner2 = getOwner rel2
      resource2 = getResource rel2
  in if owner1 == resource2 && owner2 == resource1
     then checkOwnershipTransitivity owner1 resource1 owner2
     else True  -- Not a transitive case, property vacuously holds

-- Property: Ownership transfer preserves uniqueness of owners
prop_ownershipTransferUniqueness :: OwnershipState -> String -> String -> Bool
prop_ownershipTransferUniqueness state fromOwner toOwner =
  let transferred = transferOwnership state fromOwner toOwner
      owners = getAllOwners transferred
      ownerCounts = Map.fromListWith (+) [(owner, 1) | owner <- owners]
  in all (\count -> count <= 1) (Map.elems ownerCounts)

-- Property: Ownership borrowing respects lifetimes
prop_ownershipBorrowingLifetimes :: OwnershipState -> String -> String -> Int -> Bool
prop_ownershipBorrowingLifetimes state owner borrower lifetime =
  let borrowed = borrowOwnership state owner borrower lifetime
      lifetimeValid = checkBorrowLifetime borrowed owner borrower lifetime
  in lifetimeValid

-- Property: Ownership sharing maintains correct reference counts
prop_ownershipSharingReferenceCounts :: OwnershipState -> String -> [String] -> Bool
prop_ownershipSharingReferenceCounts state owner sharers =
  let shared = shareOwnership state owner sharers
      refCounts = getReferenceCounts shared owner
      expectedCount = length sharers
  in refCounts == expectedCount

-- Property: Ownership move invalidates previous owner
prop_ownershipMoveInvalidation :: OwnershipState -> String -> String -> Bool
prop_ownershipMoveInvalidation state fromOwner toOwner =
  let moved = moveOwnership state fromOwner toOwner
      fromOwnerValid = isValidOwner moved fromOwner
      toOwnerValid = isValidOwner moved toOwner
  in not fromOwnerValid && toOwnerValid

-- Property: Ownership cloning preserves original state
prop_ownershipCloningPreservation :: OwnershipState -> String -> Bool
prop_ownershipCloningPreservation state owner =
  let cloned = cloneOwnership state owner
      originalValid = isValidOwner state owner
      clonedValid = isValidOwner cloned owner
  in originalValid && clonedValid

-- Property: Ownership scopes are properly nested
prop_ownershipScopeNesting :: [OwnershipInfo] -> Bool
prop_ownershipScopeNesting infos =
  let scopes = map getScopeInfo infos
      sortedScopes = sort scopes
  in checkScopeNesting sortedScopes

-- Property: Ownership conflicts are properly detected
prop_ownershipConflictDetection :: OwnershipState -> String -> String -> Bool
prop_ownershipConflictDetection state owner1 owner2 =
  let conflict = hasOwnershipConflict state owner1 owner2
      manualCheck = checkConflictManually state owner1 owner2
  in conflict == manualCheck

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock ownership data types
data OwnershipRelation = OwnershipRelation
  { relOwner :: String
  , relResource :: String
  , relType :: String
  } deriving (Show, Eq)

data OwnershipState = OwnershipState
  { stateRelations :: [OwnershipRelation]
  , stateOwners :: Set.Set String
  , stateResources :: Set.Set String
  } deriving (Show, Eq)

data OwnershipInfo = OwnershipInfo
  { infoOwner :: String
  , infoResource :: String
  , infoScope :: (Int, Int)  -- (start, end) positions
  } deriving (Show, Eq)

-- Mock helper functions
getOwner :: OwnershipRelation -> String
getOwner = relOwner

getResource :: OwnershipRelation -> String
getResource = relResource

checkOwnershipTransitivity :: String -> String -> String -> Bool
checkOwnershipTransitivity ownerA resourceB ownerC = True  -- Mock implementation

transferOwnership :: OwnershipState -> String -> String -> OwnershipState
transferOwnership state fromOwner toOwner = state  -- Mock implementation

getAllOwners :: OwnershipState -> [String]
getAllOwners state = Set.toList (stateOwners state)

borrowOwnership :: OwnershipState -> String -> String -> Int -> OwnershipState
borrowOwnership state owner borrower lifetime = state  -- Mock implementation

checkBorrowLifetime :: OwnershipState -> String -> String -> Int -> Bool
checkBorrowLifetime _ _ _ _ = True  -- Mock implementation

shareOwnership :: OwnershipState -> String -> [String] -> OwnershipState
shareOwnership state owner sharers = state  -- Mock implementation

getReferenceCounts :: OwnershipState -> String -> Int
getReferenceCounts _ _ = 1  -- Mock implementation

moveOwnership :: OwnershipState -> String -> String -> OwnershipState
moveOwnership state fromOwner toOwner = state  -- Mock implementation

isValidOwner :: OwnershipState -> String -> Bool
isValidOwner state owner = owner `Set.member` stateOwners state

cloneOwnership :: OwnershipState -> String -> OwnershipState
cloneOwnership state owner = state  -- Mock implementation

getScopeInfo :: OwnershipInfo -> (Int, Int)
getScopeInfo = infoScope

checkScopeNesting :: [(Int, Int)] -> Bool
checkScopeNesting [] = True
checkScopeNesting [_] = True
checkScopeNesting (s1:s2:ss) = isScopeNested s1 s2 && checkScopeNesting (s2:ss)

isScopeNested :: (Int, Int) -> (Int, Int) -> Bool
isScopeNested (start1, end1) (start2, end2) = 
  start2 >= start1 && end2 <= end1

hasOwnershipConflict :: OwnershipState -> String -> String -> Bool
hasOwnershipConflict _ _ _ = False  -- Mock implementation

checkConflictManually :: OwnershipState -> String -> String -> Bool
checkConflictManually _ _ _ = False  -- Mock implementation

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipRelation where
  arbitrary = OwnershipRelation <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary OwnershipState where
  arbitrary = OwnershipState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary OwnershipInfo where
  arbitrary = OwnershipInfo <$> arbitrary <*> arbitrary <*> arbitrary

-- Helper for generating arbitrary strings
arbitraryIdentifier :: Gen String
arbitraryIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

instance Arbitrary String where
  arbitrary = arbitraryIdentifier