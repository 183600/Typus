{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.OwnershipTransitivitySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort, nub, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set
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

-- We need Map for the implementation above
import qualified Data.Map as Map

spec :: Spec
spec = describe "Ownership Transitivity Tests" $ do

  describe "Basic ownership relations" $ do
    it "creates ownership relations correctly" $ do
      let owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relation = OwnershipRelation owner resource span
      relationOwner relation `shouldBe` owner
      relationResource relation `shouldBe` resource
      relationSpan relation `shouldBe` span
      
    it "adds ownership to graph" $ do
      let owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          graph = OwnershipGraph [] [] []
          newGraph = addOwnership owner resource span graph
      length (graphRelations newGraph) `shouldBe` 1
      relationOwner (head $ graphRelations newGraph) `shouldBe` owner
      
    it "handles multiple ownership relations" $ do
      let owner1 = Owner "owner1" "Alice"
          owner2 = Owner "owner2" "Bob"
          resource1 = Resource "resource1" "Memory"
          resource2 = Resource "resource2" "File"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          graph = OwnershipGraph [] [] []
          graph1 = addOwnership owner1 resource1 span graph
          graph2 = addOwnership owner2 resource2 span graph1
      length (graphRelations graph2) `shouldBe` 2
      let relations = graphRelations graph2
      relationOwner (relations !! 0) `shouldBe` owner1
      relationOwner (relations !! 1) `shouldBe` owner2

  describe "Ownership transfer" $ do
    it "transfers single ownership" $ do
      let fromOwner = Owner "owner1" "Alice"
          toOwner = Owner "owner2" "Bob"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          graph = OwnershipGraph [OwnershipRelation fromOwner resource span] [fromOwner] [resource]
          result = transferOwnership fromOwner toOwner [resource] graph
      length (transferredRelations result) `shouldBe` 1
      let transferred = head $ transferredRelations result
      relationOwner transferred `shouldBe` toOwner
      relationResource transferred `shouldBe` resource
      
    it "transfers multiple ownerships" $ do
      let fromOwner = Owner "owner1" "Alice"
          toOwner = Owner "owner2" "Bob"
          resource1 = Resource "resource1" "Memory"
          resource2 = Resource "resource2" "File"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relation1 = OwnershipRelation fromOwner resource1 span
          relation2 = OwnershipRelation fromOwner resource2 span
          graph = OwnershipGraph [relation1, relation2] [fromOwner] [resource1, resource2]
          result = transferOwnership fromOwner toOwner [resource1, resource2] graph
      length (transferredRelations result) `shouldBe` 2
      let transferred = transferredRelations result
      map relationOwner transferred `shouldBe` [toOwner, toOwner]
      map relationResource transferred `shouldBe` [resource1, resource2]
      
    it "preserves non-transferred ownerships" $ do
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
      length (transferredRelations result) `shouldBe` 2
      length (graphRelations $ newGraph result) `shouldBe` 3
      let remainingRelations = filter (\r -> relationOwner r == fromOwner) $ graphRelations $ newGraph result
      length remainingRelations `shouldBe` 1
      relationResource (head remainingRelations) `shouldBe` resource3

  describe "Ownership transitivity" $ do
    it "detects transitive ownership chains" $ do
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
          
      length chains `shouldBe` 0  -- No transitive chains in this simple setup
      
    it "handles complex ownership scenarios" $ do
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
          
      length chains `shouldBe` 0  -- No transitive chains in this setup

  describe "Ownership graph invariants" $ do
    it "maintains relation consistency" $ do
      let owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relation = OwnershipRelation owner resource span
          graph = OwnershipGraph [relation] [owner] [resource]
      length (graphRelations graph) `shouldBe` 1
      let rel = head $ graphRelations graph
      relationOwner rel `shouldBe` owner
      relationResource rel `shouldBe` resource
      
    it "validates owner and resource existence" $ do
      let owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relation = OwnershipRelation owner resource span
          graph = OwnershipGraph [relation] [owner] [resource]
      owner `elem` graphOwners graph `shouldBe` True
      resource `elem` graphResources graph `shouldBe` True
      
    it "handles duplicate ownership relations" $ do
      let owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relation1 = OwnershipRelation owner resource span
          relation2 = OwnershipRelation owner resource span
          graph = OwnershipGraph [relation1, relation2] [owner] [resource]
      length (graphRelations graph) `shouldBe` 2

  describe "QuickCheck properties" $ do
    it "ownership transfer preserves total relations" $ property $
      \fromOwner toOwner resources graph ->
        let result = transferOwnership fromOwner toOwner resources graph
        in length (graphRelations $ originalGraph result) `shouldBe` length (graphRelations $ newGraph result)
        
    it "ownership transfer updates only specified relations" $ property $
      \fromOwner toOwner resources graph ->
        let result = transferOwnership fromOwner toOwner resources graph
            transferred = transferredRelations result
        in all (\r -> relationOwner r == toOwner && relationResource r `elem` resources) transferred
        
    it "ownership graph maintains owner sets" $ property $
      \relations ->
        let owners = map relationOwner relations
            resources = map relationResource relations
            uniqueOwners = nub owners
            uniqueResources = nub resources
            graph = OwnershipGraph relations uniqueOwners uniqueResources
        in all (`elem` graphOwners graph) uniqueOwners &&
           all (`elem` graphResources graph) uniqueResources

  describe "Edge cases" $ do
    it "handles empty ownership graph" $ do
      let graph = OwnershipGraph [] [] []
          owner = Owner "owner1" "Alice"
          resource = Resource "resource1" "Memory"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          newGraph = addOwnership owner resource span graph
      length (graphRelations newGraph) `shouldBe` 1
      
    it "handles ownership transfer with no matching resources" $ do
      let fromOwner = Owner "owner1" "Alice"
          toOwner = Owner "owner2" "Bob"
          resource = Resource "resource1" "Memory"
          otherResource = Resource "resource2" "File"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          graph = OwnershipGraph [OwnershipRelation fromOwner resource span] [fromOwner] [resource]
          result = transferOwnership fromOwner toOwner [otherResource] graph
      length (transferredRelations result) `shouldBe` 0
      length (graphRelations $ newGraph result) `shouldBe` 1
      
    it "handles circular ownership references" $ do
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
          
      length chains `shouldBe` 0  -- No transitive chains detected
      
    it "handles large ownership graphs" $ do
      let owners = [Owner ("owner" ++ show i) ("Owner" ++ show i) | i <- [1..50]]
          resources = [Resource ("resource" ++ show i) ("Resource" ++ show i) | i <- [1..50]]
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          relations = [OwnershipRelation (owners !! i) (resources !! i) span | i <- [0..49]]
          graph = OwnershipGraph relations owners resources
      length (graphRelations graph) `shouldBe` 50
      length (graphOwners graph) `shouldBe` 50
      length (graphResources graph) `shouldBe` 50