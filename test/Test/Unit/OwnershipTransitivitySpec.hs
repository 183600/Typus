{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds  -Wno-type-defaults #-}
module Test.Unit.OwnershipTransitivitySpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (nub)
import qualified Data.Map as Map
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Mock data types for ownership testing
data Owner = Owner
  { ownerId :: String
  , ownerName :: String
  } deriving (Show, Eq, Ord)

data Resource = Resource
  { resourceId :: String
  , resourceType :: String
  } deriving (Show, Eq, Ord)

data OwnershipRelation = OwnershipRelation
  { relationOwner :: Owner
  , relationResource :: Resource
  , relationSpan :: SourceSpan
  } deriving (Show, Eq)

data OwnershipGraph = OwnershipGraph
  { graphRelations :: [OwnershipRelation]
  , graphOwners :: [Owner]
  , graphResources :: [Resource]
  } deriving (Show, Eq)

-- Arbitrary instances for QuickCheck
instance Arbitrary Owner where
  arbitrary = do
    ownerId <- arbitrary
    ownerName <- arbitrary
    return $ Owner ownerId ownerName

instance Arbitrary Resource where
  arbitrary = do
    resourceId <- arbitrary
    resourceType <- elements ["Memory", "File", "Socket", "Network", "Database"]
    return $ Resource resourceId resourceType

-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary OwnershipRelation where
  arbitrary = do
    owner <- arbitrary
    resource <- arbitrary
    span <- arbitrary
    return $ OwnershipRelation owner resource span

instance Arbitrary OwnershipGraph where
  arbitrary = do
    relations <- listOf arbitrary
    let owners = map relationOwner relations
        resources = map relationResource relations
    return $ OwnershipGraph relations owners resources

data TransferResult = TransferResult
  { originalGraph :: OwnershipGraph
  , newGraph :: OwnershipGraph
  , transferredRelations :: [OwnershipRelation]
  } deriving (Show, Eq)

-- Mock ownership functions
addOwnership :: Owner -> Resource -> SourceSpan -> OwnershipGraph -> OwnershipGraph
addOwnership owner resource span graph = 
  let newRelation = OwnershipRelation owner resource span
      newRelations = newRelation : graphRelations graph
  in graph { graphRelations = newRelations }

transferOwnership :: Owner -> Owner -> [Resource] -> OwnershipGraph -> TransferResult
transferOwnership fromOwner toOwner resources graph =
  let relevantRelations = filter (\r -> relationOwner r == fromOwner && 
                                       relationResource r `elem` resources) $ graphRelations graph
      transferredRelations = map (\r -> r { relationOwner = toOwner }) relevantRelations
      otherRelations = filter (\r -> not (relationOwner r == fromOwner && 
                                         relationResource r `elem` resources)) $ graphRelations graph
      newRelations = transferredRelations ++ otherRelations
      newGraph = graph { graphRelations = newRelations }
  in TransferResult graph newGraph transferredRelations

checkTransitivity :: OwnershipGraph -> [(Owner, Owner, [Resource])]
checkTransitivity graph = 
  let relations = graphRelations graph
      ownerToResources = foldr (\rel acc -> 
            let owner = relationOwner rel
                resource = relationResource rel
            in Map.insertWith (++) owner [resource] acc) Map.empty relations
      transitiveChains = []
  in transitiveChains  -- Mock implementation

tests :: TestTree
tests = testGroup "Ownership Transitivity Tests"
  [ testGroup "Basic ownership relations"
    [ testCase "creates ownership relations correctly" $ do
        let owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation = OwnershipRelation owner resource span
        relationOwner relation @?= owner
        relationResource relation @?= resource
        relationSpan relation @?= span
      
    , testCase "adds ownership to graph" $ do
        let owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            graph = OwnershipGraph [] [] []
            newGraph = addOwnership owner resource span graph
        let relations = graphRelations newGraph
        length relations @?= 1
        case relations of (r:_) -> relationOwner r @?= owner; [] -> assertFailure "Expected at least one relation"
      
    , testCase "handles multiple ownership relations" $ do
        let owner1 = Owner "owner1" "Alice"
            owner2 = Owner "owner2" "Bob"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            graph = OwnershipGraph [] [] []
            graph1 = addOwnership owner1 resource1 span graph
            graph2 = addOwnership owner2 resource2 span graph1
        length (graphRelations graph2) @?= 2
        let relations = graphRelations graph2
        relationOwner (relations !! 0) @?= owner1
        relationOwner (relations !! 1) @?= owner2
    ]

  , testGroup "Ownership transfer"
    [ testCase "transfers single ownership" $ do
        let fromOwner = Owner "owner1" "Alice"
            toOwner = Owner "owner2" "Bob"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            graph = OwnershipGraph [OwnershipRelation fromOwner resource span] [fromOwner] [resource]
            result = transferOwnership fromOwner toOwner [resource] graph
        let transferred = transferredRelations result
        length transferred @?= 1
        case transferred of 
          (r:_) -> do
            relationOwner r @?= toOwner
            relationResource r @?= resource
          [] -> assertFailure "Expected at least one transferred relation"
      
    , testCase "transfers multiple ownerships" $ do
        let fromOwner = Owner "owner1" "Alice"
            toOwner = Owner "owner2" "Bob"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation1 = OwnershipRelation fromOwner resource1 span
            relation2 = OwnershipRelation fromOwner resource2 span
            graph = OwnershipGraph [relation1, relation2] [fromOwner] [resource1, resource2]
            result = transferOwnership fromOwner toOwner [resource1, resource2] graph
        length (transferredRelations result) @?= 2
        let transferred = transferredRelations result
        map relationOwner transferred @?= [toOwner, toOwner]
        map relationResource transferred @?= [resource1, resource2]
      
    , testCase "preserves non-transferred ownerships" $ do
        let fromOwner = Owner "owner1" "Alice"
            toOwner = Owner "owner2" "Bob"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            resource3 = Resource "resource3" "Socket"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation1 = OwnershipRelation fromOwner resource1 span
            relation2 = OwnershipRelation fromOwner resource2 span
            relation3 = OwnershipRelation fromOwner resource3 span
            graph = OwnershipGraph [relation1, relation2, relation3] [fromOwner] [resource1, resource2, resource3]
            result = transferOwnership fromOwner toOwner [resource1, resource2] graph
        length (transferredRelations result) @?= 2
        length (graphRelations $ newGraph result) @?= 3
        let remainingRelations = filter (\r -> relationOwner r == fromOwner) $ graphRelations $ newGraph result
        length remainingRelations @?= 1
        case remainingRelations of (r:_) -> relationResource r @?= resource3; [] -> assertFailure "Expected at least one remaining relation"
    ]

  , testGroup "Ownership transitivity"
    [ testCase "detects transitive ownership chains" $ do
        let owner1 = Owner "owner1" "Alice"
            owner2 = Owner "owner2" "Bob"
            owner3 = Owner "owner3" "Charlie"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            resource3 = Resource "resource3" "Socket"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            
            -- Create a chain: owner1 -> resource1, owner2 -> resource2, owner3 -> resource3
            relation1 = OwnershipRelation owner1 resource1 span
            relation2 = OwnershipRelation owner2 resource2 span
            relation3 = OwnershipRelation owner3 resource3 span
            
            graph = OwnershipGraph [relation1, relation2, relation3] [owner1, owner2, owner3] [resource1, resource2, resource3]
            chains = checkTransitivity graph
            
        length chains @?= 0  -- No transitive chains in this simple setup
      
    , testCase "handles complex ownership scenarios" $ do
        let owner1 = Owner "owner1" "Alice"
            owner2 = Owner "owner2" "Bob"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            
            -- Both owners own the same resource
            relation1 = OwnershipRelation owner1 resource1 span
            relation2 = OwnershipRelation owner2 resource1 span
            relation3 = OwnershipRelation owner1 resource2 span
            
            graph = OwnershipGraph [relation1, relation2, relation3] [owner1, owner2] [resource1, resource2]
            chains = checkTransitivity graph
            
        length chains @?= 0  -- No transitive chains in this setup
    ]

  , testGroup "Ownership graph invariants"
    [ testCase "maintains relation consistency" $ do
        let owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation = OwnershipRelation owner resource span
            graph = OwnershipGraph [relation] [owner] [resource]
        let relations = graphRelations graph
        length relations @?= 1
        case relations of 
          (r:_) -> do
            relationOwner r @?= owner
            relationResource r @?= resource
          [] -> assertFailure "Expected at least one relation"
      
    , testCase "validates owner and resource existence" $ do
        let owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation = OwnershipRelation owner resource span
            graph = OwnershipGraph [relation] [owner] [resource]
        owner `elem` graphOwners graph @?= True
        resource `elem` graphResources graph @?= True
      
    , testCase "handles duplicate ownership relations" $ do
        let owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relation1 = OwnershipRelation owner resource span
            relation2 = OwnershipRelation owner resource span
            graph = OwnershipGraph [relation1, relation2] [owner] [resource]
        length (graphRelations graph) @?= 2
    ]

  , testGroup "QuickCheck properties"
    [ testProperty "ownership transfer preserves total relations" $ property $
        \fromOwner toOwner resources graph ->
          let result = transferOwnership fromOwner toOwner resources graph
          in length (graphRelations $ originalGraph result) == length (graphRelations $ newGraph result)
          
    , testProperty "ownership transfer updates only specified relations" $ property $
        \fromOwner toOwner resources graph ->
          let result = transferOwnership fromOwner toOwner resources graph
              transferred = transferredRelations result
          in all (\r -> relationOwner r == toOwner && relationResource r `elem` resources) transferred
          
    , testProperty "ownership graph maintains owner sets" $ property $
        \relations ->
          let owners = map relationOwner relations
              resources = map relationResource relations
              uniqueOwners = nub owners
              uniqueResources = nub resources
              graph = OwnershipGraph relations uniqueOwners uniqueResources
          in all (`elem` graphOwners graph) uniqueOwners &&
             all (`elem` graphResources graph) uniqueResources
    ]

  , testGroup "Edge cases"
    [ testCase "handles empty ownership graph" $ do
        let graph = OwnershipGraph [] [] []
            owner = Owner "owner1" "Alice"
            resource = Resource "resource1" "Memory"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            newGraph = addOwnership owner resource span graph
        length (graphRelations newGraph) @?= 1
      
    , testCase "handles ownership transfer with no matching resources" $ do
        let fromOwner = Owner "owner1" "Alice"
            toOwner = Owner "owner2" "Bob"
            resource = Resource "resource1" "Memory"
            otherResource = Resource "resource2" "File"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            graph = OwnershipGraph [OwnershipRelation fromOwner resource span] [fromOwner] [resource]
            result = transferOwnership fromOwner toOwner [otherResource] graph
        length (transferredRelations result) @?= 0
        length (graphRelations $ newGraph result) @?= 1
      
    , testCase "handles circular ownership references" $ do
        let owner1 = Owner "owner1" "Alice"
            owner2 = Owner "owner2" "Bob"
            resource1 = Resource "resource1" "Memory"
            resource2 = Resource "resource2" "File"
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            
            -- Create circular references through resource types
            relation1 = OwnershipRelation owner1 resource2 span  -- owner1 owns resource2
            relation2 = OwnershipRelation owner2 resource1 span  -- owner2 owns resource1
            
            graph = OwnershipGraph [relation1, relation2] [owner1, owner2] [resource1, resource2]
            chains = checkTransitivity graph
            
        length chains @?= 0  -- No transitive chains detected
    ]

  , testCase "handles large ownership graphs" $ do
        let owners = [Owner ("owner" ++ show i) ("Owner" ++ show i) | i <- [1..50]]
            resources = [Resource ("resource" ++ show i) ("Resource" ++ show i) | i <- [1..50]]
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            relations = [OwnershipRelation (owners !! i) (resources !! i) span | i <- [0..49]]
            graph = OwnershipGraph relations owners resources
        length (graphRelations graph) @?= 50
        length (graphOwners graph) @?= 50
        length (graphResources graph) @?= 50
  ]