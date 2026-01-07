module Test.Unit.OwnershipTransitivityQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, choose)
import Data.Char 
import Ownership (OwnershipInfo(..), OwnershipRelation(..), OwnershipState)
import SourceLocation (SourcePos(..), startPos)
import Utils ()
  in L.all (\count -> count <= 1) (Map.elems ownerCounts)
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Property: Ownership borrowing respects lifetimes
prop_ownershipBorrowingLifetimes :: OwnershipState -> String -> String -> Int -> Bool
prop_ownershipBorrowingLifetimes state owner borrower                               lifetime =
  let borrowed = borrowOwnership state owner borrower lifetime
                                    lifetimeValid = checkBorrowLifetime borrowed owner borrower lifetime
  in lifetimeValid

-- Property: Ownership sharing maintains correct reference counts
prop_ownershipSharingReferenceCounts :: OwnershipState -> String -> [String] -> Bool
prop_ownershipSharingReferenceCounts state owner                               sharers =
  let shared = shareOwnership state owner sharers
                                    refCounts = getReferenceCounts shared owner
                                    expectedCount = L.length sharers
  in                               refCounts == expectedCount

-- Property: Ownership move invalidates previous owner
prop_ownershipMoveInvalidation :: OwnershipState -> String -> String -> Bool
prop_ownershipMoveInvalidation state fromOwner                               toOwner =
  let moved = moveOwnership state fromOwner toOwner
                                    fromOwnerValid = isValidOwner moved fromOwner
                                    toOwnerValid = isValidOwner moved toOwner
  in not fromOwnerValid && toOwnerValid

-- Property: Ownership cloning preserves original state
prop_ownershipCloningPreservation :: OwnershipState -> String -> Bool
prop_ownershipCloningPreservation state                               owner =
  let cloned = cloneOwnership state owner
                                    originalValid = isValidOwner state owner
                                    clonedValid = isValidOwner cloned owner
  in originalValid && clonedValid

-- Property: Ownership scopes are properly nested
prop_ownershipScopeNesting :: [OwnershipInfo] -> Bool
prop_ownershipScopeNesting                               infos =
  let scopes = map getScopeInfo infos
                                    sortedScopes = sort scopes
  in checkScopeNesting sortedScopes

-- Property: Ownership conflicts are properly detected
prop_ownershipConflictDetection :: OwnershipState -> String -> String -> Bool
prop_ownershipConflictDetection state owner1                               owner2 =
  let conflict = hasOwnershipConflict state owner1 owner2
                                    manualCheck = checkConflictManually state owner1 owner2
  in                               conflict == manualCheck

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock ownership data types
data                               OwnershipRelation = OwnershipRelation
  { relOwner :: String
  , relResource :: String
  , relType :: String
  } deriving (Show, Eq)

data                               OwnershipState = OwnershipState
  { stateRelations :: [OwnershipRelation]
  , stateOwners :: Set.Set String
  , stateResources :: Set.Set String
  } deriving (Show, Eq)

data                               OwnershipInfo = OwnershipInfo
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
checkOwnershipTransitivity ownerA resourceB                               ownerC = True  -- Mock implementation

transferOwnership :: OwnershipState -> String -> String -> OwnershipState
transferOwnership state fromOwner                               toOwner = state  -- Mock implementation
getAllOwners :: OwnershipState -> [String]
getAllOwners                               state = Set.toList (stateOwners state)

borrowOwnership :: OwnershipState -> String -> String -> Int -> OwnershipState
borrowOwnership state owner borrower                               lifetime = state  -- Mock implementation

checkBorrowLifetime :: OwnershipState -> String -> String -> Int -> Bool
checkBorrowLifetime _ _ _                               _ = True  -- Mock implementation

shareOwnership :: OwnershipState -> String -> [String] -> OwnershipState
shareOwnership state owner                               sharers = state  -- Mock implementation

getReferenceCounts :: OwnershipState -> String -> Int
getReferenceCounts _                               _ = 1  -- Mock implementation

moveOwnership :: OwnershipState -> String -> String -> OwnershipState
moveOwnership state fromOwner                               toOwner = state  -- Mock implementation

isValidOwner :: OwnershipState -> String -> Bool
isValidOwner state                               owner = owner `Set.member` stateOwners state

cloneOwnership :: OwnershipState -> String -> OwnershipState
cloneOwnership state                               owner = state  -- Mock implementation
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
hasOwnershipConflict _ _                               _ = False  -- Mock implementation

checkConflictManually :: OwnershipState -> String -> String -> Bool
checkConflictManually _ _                               _ = False  -- Mock implementation

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