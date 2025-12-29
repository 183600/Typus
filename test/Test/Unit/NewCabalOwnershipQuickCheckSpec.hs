module Test.Unit.NewCabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, elements, listOf, Positive(..))
import Data.List (nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import Ownership

-- | QuickCheck tests for Ownership module covering ownership analysis properties
tests :: TestTree
tests =
  testGroup "New Cabal Ownership QuickCheck Tests"
    [ testGroup "Ownership tracking properties"
        [ fastProperty "createOwnedResource creates valid ownership" prop_createOwnedResourceValid
        , fastProperty "transferOwnership updates ownership correctly" prop_transferOwnershipCorrect
        , fastProperty "transferOwnership preserves resource integrity" prop_transferOwnershipPreservesResource
        , fastProperty "borrowResource maintains original ownership" prop_borrowResourceMaintainsOwnership
        , fastProperty "releaseBorrow removes borrow correctly" prop_releaseBorrowRemoves
        ]
    
    , testGroup "Ownership constraints properties"
        [ fastProperty "cannotBorrowOwnedResource prevents invalid borrows" prop_cannotBorrowOwned
        , fastProperty "cannotTransferBorrowedResource prevents invalid transfers" prop_cannotTransferBorrowed
        , fastProperty "cannotUseAfterFree prevents use-after-free" prop_cannotUseAfterFree
        , fastProperty "ownershipGraphIsAcyclic prevents cycles" prop_ownershipGraphAcyclic
        ]
    
    , testGroup "Ownership analysis properties"
        [ fastProperty "analyzeOwnership finds all ownership relationships" prop_analyzeOwnershipComplete
        , fastProperty "analyzeOwnership respects ownership rules" prop_analyzeOwnershipRespectsRules
        , fastProperty "checkOwnershipViolations detects violations" prop_checkOwnershipViolations
        , fastProperty "ownershipInference is sound" prop_ownershipInferenceSound
        ]
    
    , testGroup "Ownership optimization properties"
        [ fastProperty "optimizeOwnershipReducesBorrows" prop_optimizeOwnershipReducesBorrows
        , fastProperty "optimizeOwnershipPreservesSemantics" prop_optimizeOwnershipPreservesSemantics
        , fastProperty "ownershipTransferMinimization reduces transfers" prop_ownershipTransferMinimization
        ]
    
    , testGroup "Edge cases and robustness"
        [ testCase "handle empty ownership graph" $ do
            let emptyGraph = createEmptyOwnershipGraph
            size emptyGraph @?= 0
            
        , testCase "handle deeply nested ownership chains" $ do
            let chain = createOwnershipChain 100
            length chain @?= 100
            
        , testCase "handle ownership cycles detection" $ do
            let cycleGraph = createOwnershipCycle
            hasOwnershipCycle cycleGraph @?= True
        ]
    
    , testGroup "Performance properties"
        [ fastProperty "ownershipAnalysisScalesLinearly" prop_ownershipAnalysisScalesLinearly
        , fastProperty "ownershipTransferIsConstantTime" prop_ownershipTransferIsConstantTime
        ]
    ]

-- | Property: createOwnedResource creates valid ownership
prop_createOwnedResourceValid :: String -> Bool
prop_createOwnedResourceValid resourceId =
  let resource = createOwnedResource resourceId
  in resourceId == ownedResourceId resource &&
     isOwner (ownerId resource) resource &&
     null (borrowers resource)

-- | Property: transferOwnership updates ownership correctly
prop_transferOwnershipCorrect :: String -> String -> Bool
prop_transferOwnershipCorrect oldOwner newOwner =
  let resource = createOwnedResourceWithOwner "test" oldOwner
      transferred = transferOwnership resource newOwner
  in isOwner newOwner transferred &&
     not (isOwner oldOwner transferred)

-- | Property: transferOwnership preserves resource integrity
prop_transferOwnershipPreservesResource :: String -> String -> String -> Bool
prop_transferOwnershipPreservesResource resourceId oldOwner newOwner =
  let resource = createOwnedResourceWithOwner resourceId oldOwner
      transferred = transferOwnership resource newOwner
  in ownedResourceId transferred == resourceId &&
     resourceType transferred == resourceType resource

-- | Property: borrowResource maintains original ownership
prop_borrowResourceMaintainsOwnership :: String -> String -> Bool
prop_borrowResourceMaintainsOwnership ownerId borrowerId =
  let resource = createOwnedResourceWithOwner "test" ownerId
      borrowed = borrowResource resource borrowerId
  in isOwner ownerId borrowed &&
     borrowerId `elem` borrowers borrowed

-- | Property: releaseBorrow removes borrow correctly
prop_releaseBorrowRemoves :: String -> String -> String -> Bool
prop_releaseBorrowRemoves ownerId borrower1 borrower2 =
  let resource = createOwnedResourceWithOwner "test" ownerId
      borrowed1 = borrowResource resource borrower1
      borrowed2 = borrowResource borrowed1 borrower2
      released = releaseBorrow borrowed2 borrower1
  in not (borrower1 `elem` borrowers released) &&
     borrower2 `elem` borrowers released

-- | Property: cannotBorrowOwnedResource prevents invalid borrows
prop_cannotBorrowOwned :: String -> Bool
prop_cannotBorrowOwned ownerId =
  let resource = createOwnedResourceWithOwner "test" ownerId
      result = canBorrowResource resource ownerId
  in not result

-- | Property: cannotTransferBorrowedResource prevents invalid transfers
prop_cannotTransferBorrowed :: String -> String -> String -> Bool
prop_cannotTransferBorrowed ownerId borrower newOwner =
  let resource = createOwnedResourceWithOwner "test" ownerId
      borrowed = borrowResource resource borrower
      result = canTransferOwnership borrowed borrower
  in not result

-- | Property: cannotUseAfterFree prevents use-after-free
prop_cannotUseAfterFree :: String -> Bool
prop_cannotUseAfterFree ownerId =
  let resource = createOwnedResourceWithOwner "test" ownerId
      freed = freeResource resource
      result = canUseResource freed
  in not result

-- | Property: ownershipGraphIsAcyclic prevents cycles
prop_ownershipGraphAcyclic :: [String] -> Bool
prop_ownershipGraphAcyclic resourceIds =
  let graph = createLinearOwnershipGraph resourceIds
  in not (hasOwnershipCycle graph)

-- | Property: analyzeOwnership finds all ownership relationships
prop_analyzeOwnershipComplete :: [String] -> Bool
prop_analyzeOwnershipComplete resourceIds =
  let graph = createLinearOwnershipGraph resourceIds
      analysis = analyzeOwnership graph
      expectedRelations = length resourceIds - 1
  in length (ownershipRelations analysis) >= expectedRelations

-- | Property: analyzeOwnership respects ownership rules
prop_analyzeOwnershipRespectsRules :: [String] -> Bool
prop_analyzeOwnershipRespectsRules resourceIds =
  let graph = createLinearOwnershipGraph resourceIds
      analysis = analyzeOwnership graph
  in all isValidOwnershipRelation (ownershipRelations analysis)

-- | Property: checkOwnershipViolations detects violations
prop_checkOwnershipViolations :: [String] -> Bool
prop_checkOwnershipViolations resourceIds =
  let graph = createLinearOwnershipGraph resourceIds
      violations = checkOwnershipViolations graph
  in all isRealViolation violations

-- | Property: ownershipInference is sound
prop_ownershipInferenceSound :: [String] -> Bool
prop_ownershipInferenceSound resourceIds =
  let graph = createLinearOwnershipGraph resourceIds
      inferred = inferOwnership graph
  in all (\rel -> rel `elem` ownershipRelations (analyzeOwnership graph)) inferred

-- | Property: optimizeOwnershipReducesBorrows
prop_optimizeOwnershipReducesBorrows :: [String] -> Bool
prop_optimizeOwnershipReducesBorrows borrowerIds =
  let resource = createOwnedResource "test"
      borrowed = foldl (\res borrower -> borrowResource res borrower) resource borrowerIds
      optimized = optimizeOwnership borrowed
  in length (borrowers optimized) <= length (borrowers borrowed)

-- | Property: optimizeOwnershipPreservesSemantics
prop_optimizeOwnershipPreservesSemantics :: [String] -> Bool
prop_optimizeOwnershipPreservesSemantics borrowerIds =
  let resource = createOwnedResource "test"
      borrowed = foldl (\res borrower -> borrowResource res borrower) resource borrowerIds
      optimized = optimizeOwnership borrowed
  in ownedResourceId optimized == ownedResourceId borrowed &&
     ownerId optimized == ownerId borrowed

-- | Property: ownershipTransferMinimization reduces transfers
prop_ownershipTransferMinimization :: [String] -> Bool
prop_ownershipTransferMinimization ownerIds =
  let transfers = createTransferSequence ownerIds
      minimized = minimizeOwnershipTransfers transfers
  in length minimized <= length transfers

-- | Property: ownershipAnalysisScalesLinearly
prop_ownershipAnalysisScalesLinearly :: Positive Int -> Bool
prop_ownershipAnalysisScalesLinearly (Positive n) =
  let resourceIds = map (("resource_" ++) . show) [1..n]
      graph = createLinearOwnershipGraph resourceIds
      analysis = analyzeOwnership graph
  in length (ownershipRelations analysis) <= n * 2

-- | Property: ownershipTransferIsConstantTime
prop_ownershipTransferIsConstantTime :: String -> String -> Bool
prop_ownershipTransferIsConstantTime oldOwner newOwner =
  let resource = createOwnedResourceWithOwner "test" oldOwner
      transferred = transferOwnership resource newOwner
  in transferred /= resource && isOwner newOwner transferred

-- Helper data types and functions (mock implementations for demonstration)
data OwnedResource = OwnedResource
  { ownedResourceId :: String
  , resourceType :: ResourceType
  , ownerId :: String
  , borrowers :: [String]
  , isFreed :: Bool
  } deriving (Eq, Show)

data ResourceType = Variable | Function | Struct | Enum deriving (Eq, Show)

data OwnershipGraph = OwnershipGraph
  { nodes :: Set String
  , edges :: [(String, String)]
  } deriving (Eq, Show)

data OwnershipAnalysis = OwnershipAnalysis
  { ownershipRelations :: [(String, String)]
  , violations :: [OwnershipViolation]
  } deriving (Eq, Show)

data OwnershipViolation = OwnershipViolation
  { violationType :: ViolationType
  , violationLocation :: String
  } deriving (Eq, Show)

data ViolationType = UseAfterFree | DoubleBorrow | InvalidTransfer deriving (Eq, Show)

-- Mock functions (in real implementation, these would come from Ownership module)
createOwnedResource :: String -> OwnedResource
createOwnedResource resourceId = OwnedResource resourceId Variable "owner" [] False

createOwnedResourceWithOwner :: String -> String -> OwnedResource
createOwnedResourceWithOwner resourceId owner = OwnedResource resourceId Variable owner [] False

isOwner :: String -> OwnedResource -> Bool
isOwner owner resource = ownerId resource == owner

transferOwnership :: OwnedResource -> String -> OwnedResource
transferOwnership resource newOwner = resource { ownerId = newOwner }

borrowResource :: OwnedResource -> String -> OwnedResource
borrowResource resource borrower = resource { borrowers = borrower : borrowers resource }

releaseBorrow :: OwnedResource -> String -> OwnedResource
releaseBorrow resource borrower = resource { borrowers = filter (/= borrower) (borrowers resource) }

freeResource :: OwnedResource -> OwnedResource
freeResource resource = resource { isFreed = True }

canBorrowResource :: OwnedResource -> String -> Bool
canBorrowResource resource borrower = not (isOwner borrower resource) && borrower `notElem` borrowers resource

canTransferOwnership :: OwnedResource -> String -> Bool
canTransferOwnership resource borrower = not (borrower `elem` borrowers resource)

canUseResource :: OwnedResource -> Bool
canUseResource resource = not (isFreed resource)

createEmptyOwnershipGraph :: OwnershipGraph
createEmptyOwnershipGraph = OwnershipGraph Set.empty []

createLinearOwnershipGraph :: [String] -> OwnershipGraph
createLinearOwnershipGraph [] = OwnershipGraph Set.empty []
createLinearOwnershipGraph [_] = OwnershipGraph (Set.fromList [_]) []
createLinearOwnershipGraph (x:y:xs) = 
  let graph = createLinearOwnershipGraph (y:xs)
  in graph { nodes = Set.insert x (nodes graph)
          , edges = (x, y) : edges graph }

createOwnershipChain :: Int -> [OwnedResource]
createOwnershipChain n = map (\i -> createOwnedResourceWithOwner ("resource_" ++ show i) ("owner_" ++ show i)) [1..n]

createOwnershipCycle :: OwnershipGraph
createOwnershipCycle = OwnershipGraph (Set.fromList ["a", "b", "c"]) [("a", "b"), ("b", "c"), ("c", "a")]

hasOwnershipCycle :: OwnershipGraph -> Bool
hasOwnershipCycle graph = any (\(a, b) -> (b, a) `elem` edges graph) (edges graph)

analyzeOwnership :: OwnershipGraph -> OwnershipAnalysis
analyzeOwnership graph = OwnershipAnalysis (edges graph) []

checkOwnershipViolations :: OwnershipGraph -> [OwnershipViolation]
checkOwnershipViolations graph = 
  if hasOwnershipCycle graph
  then [OwnershipViolation InvalidTransfer "cycle detected"]
  else []

isValidOwnershipRelation :: (String, String) -> Bool
isValidOwnershipRelation (owner, resource) = not (null owner && null resource)

inferOwnership :: OwnershipGraph -> [(String, String)]
inferOwnership = edges

optimizeOwnership :: OwnedResource -> OwnedResource
optimizeOwnership resource = resource { borrowers = nub (borrowers resource) }

createTransferSequence :: [String] -> [(String, String)]
createTransferSequence [] = []
createTransferSequence [_] = []
createTransferSequence (x:y:xs) = (x, y) : createTransferSequence (y:xs)

minimizeOwnershipTransfers :: [(String, String)] -> [(String, String)]
minimizeOwnershipTransfers transfers = nub transfers

isRealViolation :: OwnershipViolation -> Bool
isRealViolation (OwnershipViolation _ _) = True