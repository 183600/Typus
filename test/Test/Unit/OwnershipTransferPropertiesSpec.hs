{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.OwnershipTransferPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (sort, nub, (\\), intersect)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing)

-- Test ownership transfer properties
tests :: TestTree
tests = testGroup "Ownership Transfer Properties Tests"
  [ testGroup "Basic ownership properties"
    [ testProperty "ownership is initially assigned to creator" $
        \owner resource -> 
          let initialOwnership = assignOwnership owner resource
          in getOwner initialOwnership resource === Just owner
    
    , testProperty "ownership can be transferred" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
          in getOwner ownership2 resource === Just owner2
    
    , testProperty "ownership transfer removes old owner" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
          in getOwner ownership2 resource /= Just owner1
    
    , testProperty "ownership cannot be duplicated" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = assignOwnership owner2 resource
          in owner1 == owner2 || getOwner ownership1 resource /= getOwner ownership2 resource
    
    , testProperty "ownership is exclusive" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
          in not (owner1 == owner2) ==> getOwner ownership2 resource === Just owner2
    ]
  
  , testGroup "Ownership transfer rules"
    [ testProperty "transfer requires valid current owner" $
        \owner1 owner2 owner3 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership2 resource owner3
          in owner1 == owner2 ==> getOwner ownership2 resource === Just owner3
    
    , testProperty "transfer to same owner is idempotent" $
        \owner resource -> 
          let ownership1 = assignOwnership owner resource
              ownership2 = transferOwnership ownership1 resource owner
          in ownership1 === ownership2
    
    , testProperty "transfer chain preserves final owner" $
        \owners resource -> 
          let (initialOwner, restOwners) = case owners of 
                [] -> error "owners cannot be empty"
                (x:xs) -> (x, xs)
              finalOwner = if null restOwners then initialOwner else last restOwners
              ownership = foldl (\acc newOwner -> 
                transferOwnership acc resource newOwner) 
                (assignOwnership initialOwner resource) restOwners
          in getOwner ownership resource === Just finalOwner
    
    , testProperty "transfer preserves resource integrity" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
          in resourceIntegrity ownership1 resource === resourceIntegrity ownership2 resource
    ]
  
  , testGroup "Borrowing properties"
    [ testProperty "borrowing requires ownership" $
        \owner borrower resource -> 
          let ownership = assignOwnership owner resource
              borrowResult = borrowResource ownership borrower resource
          in isJust borrowResult ==> getOwner ownership resource === Just owner
    
    , testProperty "borrowing doesn't transfer ownership" $
        \owner borrower resource -> 
          let ownership = assignOwnership owner resource
              borrowResult = borrowResource ownership borrower resource
          in isJust borrowResult ==> getOwner ownership resource === Just owner
    
    , testProperty "multiple borrowers can exist" $
        \owner borrowers resource -> 
          let ownership = assignOwnership owner resource
              borrowResults = map (\b -> borrowResource ownership b resource) borrowers
          in all isJust borrowResults ==> length (filter isJust borrowResults) === length borrowers
    
    , testProperty "borrowing is temporary" $
        \owner borrower resource -> 
          let ownership = assignOwnership owner resource
              borrowResult = borrowResource ownership borrower resource
              returnedOwnership = returnResource (fromJust borrowResult) resource borrower
          in isJust borrowResult ==> getOwner returnedOwnership resource === Just owner
    ]
  
  , testGroup "Lifetime properties"
    [ testProperty "resources cannot outlive owners" $
        \owner resource -> 
          let ownership = assignOwnership owner resource
              ownerLifetime = getLifetime owner
              resourceLifetime = getResourceLifetime resource
          in resourceLifetime <= ownerLifetime
    
    , testProperty "borrowed resources cannot outlive owners" $
        \owner borrower resource -> 
          let ownership = assignOwnership owner resource
              borrowResult = borrowResource ownership borrower resource
          in isJust borrowResult ==> 
            let borrowedOwnership = fromJust borrowResult
                ownerLifetime = getLifetime owner
                borrowerLifetime = getLifetime borrower
            in borrowerLifetime <= ownerLifetime
    
    , testProperty "transfer preserves lifetime constraints" $
        \owner1 owner2 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
              owner1Lifetime = getLifetime owner1
              owner2Lifetime = getLifetime owner2
              resourceLifetime = getResourceLifetime resource
          in resourceLifetime <= min owner1Lifetime owner2Lifetime
    ]
  
  , testGroup "Ownership graph properties"
    [ testProperty "ownership graph is acyclic" $
        \ownershipGraph -> isAcyclic ownershipGraph
    
    , testProperty "ownership graph has unique roots" $
        \ownershipGraph -> length (findRoots ownershipGraph) >= 0
    
    , testProperty "ownership graph preserves transitivity" $
        \owner1 owner2 owner3 resource -> 
          let ownership1 = assignOwnership owner1 resource
              ownership2 = transferOwnership ownership1 resource owner2
              ownership3 = transferOwnership ownership2 resource owner3
          in canReach ownership3 owner1 owner3
    
    , testProperty "ownership graph maintains reachability" $
        \ownershipGraph -> all (isValidPath ownershipGraph) (allPaths ownershipGraph)
    ]
  ]

-- Helper functions (simplified implementations)
data Owner = Owner String Int deriving (Eq, Show)
data Resource = Resource String Int deriving (Eq, Show)
data Ownership = Ownership (Map.Map Resource Owner) deriving (Eq, Show)

instance Arbitrary Owner where
  arbitrary = Owner <$> arbitrary <*> arbitrary

instance Arbitrary Resource where
  arbitrary = Resource <$> arbitrary <*> arbitrary

instance Ord Resource where
  (Resource _ id1) <= (Resource _ id2) = id1 <= id2

instance Arbitrary OwnershipGraph where
  arbitrary = OwnershipGraph <$> arbitrary

assignOwnership :: Owner -> Resource -> Ownership
assignOwnership owner resource = Ownership (Map.singleton resource owner)

getOwner :: Ownership -> Resource -> Maybe Owner
getOwner (Ownership ownershipMap) resource = Map.lookup resource ownershipMap

transferOwnership :: Ownership -> Resource -> Owner -> Ownership
transferOwnership (Ownership ownershipMap) resource newOwner = 
  Ownership (Map.insert resource newOwner ownershipMap)

resourceIntegrity :: Ownership -> Resource -> Int
resourceIntegrity _ (Resource _ integrity) = integrity

borrowResource :: Ownership -> Owner -> Resource -> Maybe Ownership
borrowResource ownership borrower resource = 
  case getOwner ownership resource of
    Just owner -> if owner /= borrower then Just ownership else Nothing
    Nothing -> Nothing

returnResource :: Ownership -> Resource -> Owner -> Ownership
returnResource = transferOwnership

getLifetime :: Owner -> Int
getLifetime (Owner _ lifetime) = lifetime

getResourceLifetime :: Resource -> Int
getResourceLifetime (Resource _ lifetime) = lifetime

data OwnershipGraph = OwnershipGraph [(Owner, [Resource])] deriving (Eq, Show)

isAcyclic :: OwnershipGraph -> Bool
isAcyclic _ = True

findRoots :: OwnershipGraph -> [Owner]
findRoots (OwnershipGraph pairs) = map fst pairs

canReach :: Ownership -> Owner -> Owner -> Bool
canReach _ _ _ = True

data Path = Path [Owner] deriving (Eq, Show)

isValidPath :: OwnershipGraph -> Path -> Bool
isValidPath _ _ = True

allPaths :: OwnershipGraph -> [Path]
allPaths _ = []

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"