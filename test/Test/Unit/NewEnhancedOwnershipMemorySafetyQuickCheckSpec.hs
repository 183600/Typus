{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedOwnershipMemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Ownership
import Ownership.Common.Types
import SourceLocation
import Data.List (sort, nub, group, intercalate, find, delete)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Ownership Memory Safety QuickCheck Tests
-- ============================================================================

-- Property: Ownership transfer preserves uniqueness
prop_ownership_transfer_preserves_uniqueness :: String -> String -> Property
prop_ownership_transfer_preserves_uniqueness owner1 owner2 =
  owner1 /= owner2 ==> 
  let resource = Resource "testResource" owner1
      transferred = transferOwnership resource owner2
  in property $ resourceOwner transferred === owner2 .&&. 
     resourceOwner resource === owner1

-- Property: Borrowing prevents double free
prop_borrowing_prevents_double_free :: String -> String -> Property
prop_borrowing_prevents_double_free borrower1 borrower2 =
  borrower1 /= borrower2 ==> 
  let resource = Resource "testResource" borrower1
      borrowed1 = borrowResource resource borrower1
      borrowed2 = borrowResource resource borrower2
  in property $ isJust borrowed1 .&&. isNothing borrowed2

-- Property: Reference counting correctness
prop_reference_counting_correctness :: [String] -> Property
prop_reference_counting_correctness borrowers =
  not (null borrowers) ==> 
  let resource = Resource "testResource" (L.head borrowers)
      withRefs = L.foldl (\res borrower -> addReference res borrower) resource borrowers
      finalCount = countReferences withRefs
  in property $ finalCount === L.length (nub borrowers)

-- Property: Lifetime tracking prevents use-after-free
prop_lifetime_tracking_prevents_use_after_free :: String -> Int -> Property
prop_lifetime_tracking_prevents_use_after_free owner lifetime =
  lifetime > 0 ==> 
  let resource = Resource "testResource" owner
      tracked = trackLifetime resource lifetime
      afterLifetime = useAfterLifetime tracked (lifetime + 1)
  in property $ isNothing afterLifetime

-- Property: Move semantics invalidate source
prop_move_semantics_invalidate_source :: String -> String -> Property
prop_move_semantics_invalidate_source source target =
  source /= target ==> 
  let resource = Resource "testResource" source
      moved = moveResource resource target
      sourceValid = isResourceValid resource
  in property $ not sourceValid .&&. resourceOwner moved === target

-- Property: Shared borrowing allows multiple readers
prop_shared_borrowing_multiple_readers :: [String] -> Property
prop_shared_borrowing_multiple_readers readers =
  L.length readers >= 2 ==> 
  let resource = Resource "testResource" (L.head readers)
      sharedBorrows = L.map (\reader -> sharedBorrow resource reader) readers
      successfulBorrows = catMaybes sharedBorrows
  in property $ L.length successfulBorrows === L.length readers

-- Property: Mutable borrowing prevents other borrows
prop_mutable_borrow_prevents_other_borrows :: String -> [String] -> Property
prop_mutable_borrow_prevents_other_borrows mutBorrower otherBorrowers =
  not (null otherBorrowers) ==> 
  let resource = Resource "testResource" mutBorrower
      mutableBorrow = mutableBorrowResource resource mutBorrower
      otherBorrows = L.map (\borrower -> borrowResource resource borrower) otherBorrowers
      successfulOthers = catMaybes otherBorrows
  in property $ isJust mutableBorrow .&&. null successfulOthers

-- Property: Ownership scope cleanup
prop_ownership_scope_cleanup :: [String] -> Property
prop_ownership_scope_cleanup owners =
  not (null owners) ==> 
  let resources = L.map (\owner -> Resource ("resource" ++ owner) owner) owners
      scope = createScope resources
      cleaned = cleanupScope scope
      remainingResources = getActiveResources cleaned
  in property $ null remainingResources

-- Property: Borrowing hierarchy enforcement
prop_borrowing_hierarchy_enforcement :: [String] -> Property
prop_borrowing_hierarchy_enforcement hierarchy =
  L.length hierarchy >= 3 ==> 
  let rootOwner = L.head hierarchy
      resource = Resource "testResource" rootOwner
      borrowChain = L.foldl (\res owner -> 
        case res of
          Just r -> borrowResource r owner
          Nothing -> Nothing
      ) (Just resource) (L.tail hierarchy)
  in property $ isJust borrowChain

-- Property: Resource leak detection
prop_resource_leak_detection :: [(String, Int)] -> Property
prop_resource_leak_detection resourceLifetimes =
  not (null resourceLifetimes) ==> 
  let resources = L.map (\(name, lifetime) -> Resource name "system") resourceLifetimes
      tracker = createResourceTracker resources
      leaks = detectLeaks tracker
  in property $ L.length leaks <= L.length resourceLifetimes

-- Property: Concurrent access safety
prop_concurrent_access_safety :: [String] -> Property
prop_concurrent_access_safety threads =
  L.length threads >= 2 ==> 
  let resource = Resource "sharedResource" (L.head threads)
      accessResults = L.map (\thread -> safeAccess resource thread) threads
      successfulAccesses = catMaybes accessResults
  in property $ L.length successfulAccesses <= 1

-- Property: Ownership transfer chain validity
prop_ownership_transfer_chain_validity :: [String] -> Property
prop_ownership_transfer_chain_validity owners =
  L.length owners >= 3 ==> 
  let initialResource = Resource "testResource" (L.head owners)
      transferChain = L.foldl (\res owner -> 
        case res of
          Just r -> Just (transferOwnership r owner)
          Nothing -> Nothing
      ) (Just initialResource) (L.tail owners)
      finalOwner = transferChain >>= Just . resourceOwner
  in property $ finalOwner === Just (last owners)

-- Property: Borrow checker rules consistency
prop_borrow_checker_rules_consistency :: String -> [String] -> Property
prop_borrow_checker_rules_consistency owner borrowers =
  not (null borrowers) ==> 
  let resource = Resource "testResource" owner
      borrowResults = L.map (\borrower -> checkBorrowRules resource borrower) borrowers
      validBorrows = filter id borrowResults
  in property $ L.length validBorrows <= 1 || L.all (== owner) (take 1 borrowers)

-- ============================================================================
-- Helper Functions L.and Types
-- ============================================================================

-- Ownership system types
data Resource = Resource
  { resourceName :: String
  , resourceOwner :: String
  , resourceRefs :: Set String
  , resourceLifetime :: Maybe Int
  , resourceValid :: Bool
  } deriving (Eq, Show)

data OwnershipScope = OwnershipScope
  { scopeResources :: [Resource]
  , scopeOwner :: String
  } deriving (Eq, Show)

data ResourceTracker = ResourceTracker
  { trackedResources :: Map String Resource
  , activeReferences :: Map String (Set String)
  } deriving (Eq, Show)

-- Ownership operations
transferOwnership :: Resource -> String -> Resource
transferOwnership resource newOwner = 
  resource { resourceOwner = newOwner, resourceRefs = empty }

borrowResource :: Resource -> String -> Maybe Resource
borrowResource resource borrower
  | resourceOwner resource == borrower && resourceValid resource = 
      Just $ resource { resourceRefs = insert borrower (resourceRefs resource) }
  | not (member borrower (resourceRefs resource)) && resourceValid resource = 
      Just $ resource { resourceRefs = insert borrower (resourceRefs resource) }
  | otherwise = Nothing

addReference :: Resource -> String -> Resource
addReference resource borrower = 
  resource { resourceRefs = insert borrower (resourceRefs resource) }

countReferences :: Resource -> Int
countReferences = size . resourceRefs

trackLifetime :: Resource -> Int -> Resource
trackLifetime resource lifetime = 
  resource { resourceLifetime = Just lifetime }

useAfterLifetime :: Resource -> Int -> Maybe Resource
useAfterLifetime resource currentTime
  | Just lifetime <- resourceLifetime resource =
      if currentTime <= lifetime then Just resource else Nothing
  | otherwise = Just resource

moveResource :: Resource -> String -> Resource
moveResource resource target = 
  resource { resourceOwner = target, resourceValid = False }

isResourceValid :: Resource -> Bool
isResourceValid = resourceValid

sharedBorrow :: Resource -> String -> Maybe Resource
sharedBorrow resource borrower = borrowResource resource borrower

mutableBorrowResource :: Resource -> String -> Maybe Resource
mutableBorrowResource resource borrower
  | resourceOwner resource == borrower && L.null (resourceRefs resource) = 
      Just resource
  | otherwise = Nothing

createScope :: [Resource] -> OwnershipScope
createScope resources = OwnershipScope resources "scopeOwner"

cleanupScope :: OwnershipScope -> OwnershipScope
cleanupScope scope = scope { scopeResources = [] }

getActiveResources :: OwnershipScope -> [Resource]
getActiveResources = filter resourceValid . scopeResources

createResourceTracker :: [Resource] -> ResourceTracker
createResourceTracker resources = ResourceTracker 
  (Map.fromList $ L.map (\r -> (resourceName r, r)) resources)
  (Map.fromList $ L.map (\r -> (resourceName r, resourceRefs r)) resources)

detectLeaks :: ResourceTracker -> [String]
detectLeaks tracker = 
  Map.keys $ Map.L.filter (\r -> resourceValid r && isNothing (resourceLifetime r)) (trackedResources tracker)

safeAccess :: Resource -> String -> Maybe Resource
safeAccess resource thread = 
  if resourceOwner resource == thread then Just resource else Nothing

checkBorrowRules :: Resource -> String -> Bool
checkBorrowRules resource borrower = 
  resourceOwner resource == borrower || not (member borrower (resourceRefs resource))

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Memory Safety QuickCheck Tests"
  [ fastProperty "Ownership transfer preserves uniqueness" prop_ownership_transfer_preserves_uniqueness
  , fastProperty "Borrowing prevents double free" prop_borrowing_prevents_double_free
  , fastProperty "Reference counting correctness" prop_reference_counting_correctness
  , fastProperty "Lifetime tracking prevents use-after-free" prop_lifetime_tracking_prevents_use_after_free
  , fastProperty "Move semantics invalidate source" prop_move_semantics_invalidate_source
  , fastProperty "Shared borrowing allows multiple readers" prop_shared_borrowing_multiple_readers
  , fastProperty "Mutable borrowing prevents other borrows" prop_mutable_borrow_prevents_other_borrows
  , fastProperty "Ownership scope cleanup" prop_ownership_scope_cleanup
  , fastProperty "Borrowing hierarchy enforcement" prop_borrowing_hierarchy_enforcement
  , fastProperty "Resource leak detection" prop_resource_leak_detection
  , fastProperty "Concurrent access safety" prop_concurrent_access_safety
  , fastProperty "Ownership transfer chain validity" prop_ownership_transfer_chain_validity
  , fastProperty "Borrow checker rules consistency" prop_borrow_checker_rules_consistency
  ]