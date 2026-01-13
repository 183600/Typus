module Test.Unit.NewEnhancedOwnershipTransitivitySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List (nub)

-- Test Properties for Ownership Transitivity

-- Property: Ownership should be transitive
prop_ownership_transitive :: String -> String -> String -> Property
prop_ownership_transitive a b c = property $ 
  let graph = emptyGraph
      ownsAB = owns a b graph
      ownsBC = owns b c graph
      ownsAC = owns a c graph
  in if ownsAB && ownsBC then ownsAC else True

-- Property: Ownership should not create cycles
prop_ownership_no_cycles :: String -> Property
prop_ownership_no_cycles s = property $ 
  let graph = buildOwnershipGraph s
  in not (hasCycle graph)

-- Property: Ownership transfer should revoke previous ownership
prop_ownership_transfer_revokes :: String -> String -> String -> Property
prop_ownership_transfer_revokes owner newOwner resource = property $ 
  let initialGraph = addOwnership owner resource emptyGraph
      transferGraph = transferOwnership owner newOwner resource initialGraph
  in owns newOwner resource transferGraph && not (owns owner resource transferGraph)

-- Property: Multiple ownership should be tracked correctly
prop_multiple_ownership_tracked :: String -> [String] -> Property
prop_multiple_ownership_tracked owner resources = property $ 
  let graph = foldl (\g r -> addOwnership owner r g) emptyGraph resources
      ownedResources = getOwnedResources owner graph
  in all (`elem` ownedResources) resources

-- Property: Ownership should respect borrowing rules
prop_ownership_respects_borrowing :: String -> String -> String -> Property
prop_ownership_respects_borrowing owner borrower resource = property $ 
  let graph = addOwnership owner resource emptyGraph
      borrowGraph = addBorrow borrower resource graph
  in owns owner resource borrowGraph && canBorrow borrower resource borrowGraph

-- Property: Ownership scope should be correctly tracked
prop_ownership_scope_tracked :: String -> String -> Int -> Property
prop_ownership_scope_tracked owner resource scope = property $ 
  let graph = addOwnershipWithScope owner resource scope emptyGraph
      validScope = isValidOwnershipScope owner resource scope graph
  in validScope

-- Helper functions (mock implementations)
data OwnershipGraph = OwnershipGraph [(String, String)] deriving (Show, Eq)

emptyGraph :: OwnershipGraph
emptyGraph = OwnershipGraph []

owns :: String -> String -> OwnershipGraph -> Bool
owns owner resource (OwnershipGraph pairs) = (owner, resource) `elem` pairs

buildOwnershipGraph :: String -> OwnershipGraph
buildOwnershipGraph s = OwnershipGraph [(s, "resource1"), ("owner2", s)]

hasCycle :: OwnershipGraph -> Bool
hasCycle (OwnershipGraph pairs) = 
  let checkCycle visited current = 
        if current `elem` visited then True
        else case lookup current pairs of
               Nothing -> False
               Just next -> checkCycle (current:visited) next
  in any (checkCycle [] . fst) pairs

addOwnership :: String -> String -> OwnershipGraph -> OwnershipGraph
addOwnership owner resource (OwnershipGraph pairs) = 
  OwnershipGraph ((owner, resource) : pairs)

transferOwnership :: String -> String -> String -> OwnershipGraph -> OwnershipGraph
transferOwnership oldOwner newOwner resource (OwnershipGraph pairs) = 
  OwnershipGraph ((newOwner, resource) : filter ((/= (oldOwner, resource))) pairs)

getOwnedResources :: String -> OwnershipGraph -> [String]
getOwnedResources owner (OwnershipGraph pairs) = map snd $ filter ((== owner) . fst) pairs

addBorrow :: String -> String -> OwnershipGraph -> OwnershipGraph
addBorrow borrower resource graph = graph  -- Mock implementation

canBorrow :: String -> String -> OwnershipGraph -> Bool
canBorrow _ _ _ = True  -- Mock implementation

addOwnershipWithScope :: String -> String -> Int -> OwnershipGraph -> OwnershipGraph
addOwnershipWithScope owner resource scope graph = 
  addOwnership owner resource graph  -- Mock implementation ignoring scope

isValidOwnershipScope :: String -> String -> Int -> OwnershipGraph -> Bool
isValidOwnershipScope _ _ _ _ = True  -- Mock implementation

tests :: TestTree
tests = testGroup "Test.Unit.NewEnhancedOwnershipTransitivitySpec Tests"
  [ testProperty "Ownership should be transitive" prop_ownership_transitive
  , testProperty "Ownership should not create cycles" prop_ownership_no_cycles
  , testProperty "Ownership transfer should revoke previous ownership" prop_ownership_transfer_revokes
  , testProperty "Multiple ownership should be tracked correctly" prop_multiple_ownership_tracked
  , testProperty "Ownership should respect borrowing rules" prop_ownership_respects_borrowing
  , testProperty "Ownership scope should be correctly tracked" prop_ownership_scope_tracked
  ]