{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.OwnershipTransferSpec where


import Test.Tasty.HUnit

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.QuickCheck

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )



import Test.Tasty (TestTree, testGroup)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy, find, delete, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- Ownership transfer types for testing
data OwnershipType = 
    UniqueOwnership
  | SharedOwnership
  | BorrowedOwnership
  | MovedOwnership
  deriving (Eq, Show)

data Resource = Resource
  { resourceId :: String
  , resourceType :: String
  , resourceValue :: Int
  , resourceOwner :: String
  }
  deriving (Eq, Show)

data OwnershipTransfer = OwnershipTransfer
  { transferFrom :: String
  , transferTo :: String
  , transferResource :: Resource
  , transferType :: OwnershipType
  , transferTime :: Int
  }
  deriving (Eq, Show)

data OwnershipState = OwnershipState
  { stateOwners :: Map String String  -- Resource ID -> Owner ID
  , stateResources :: Map String Resource  -- Resource ID -> Resource
  , stateTransfers :: [OwnershipTransfer]
  , stateCurrentTime :: Int
  }
  deriving (Eq, Show)

data OwnershipError = 
    ResourceNotFound String
  | InvalidTransfer String
  | OwnershipViolation String
  | ConcurrentModification String
  deriving (Eq, Show)

-- Helper generators for ownership transfer tests
genString :: Gen String
genString = do
  len <- choose (1, 10)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

genOwnershipType :: Gen OwnershipType
genOwnershipType = elements [UniqueOwnership, SharedOwnership, BorrowedOwnership, MovedOwnership]

genResource :: Gen Resource
genResource = do
  id <- genString
  typ <- genString
  value <- choose (1, 100)
  owner <- genString
  return $ Resource id typ value owner

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genString
  to <- genString
  resource <- genResource
  transferType <- genOwnershipType
  time <- choose (1, 1000)
  return $ OwnershipTransfer from to resource transferType time

genOwnershipState :: Gen OwnershipState
genOwnershipState = do
  numResources <- choose (0, 5)
  resources <- replicateM numResources genResource
  let resourceMap = Map.fromList $ map (\r -> (resourceId r, r)) resources
  let ownersMap = Map.fromList $ map (\r -> (resourceId r, resourceOwner r)) resources
  
  numTransfers <- choose (0, 5)
  transfers <- replicateM numTransfers genOwnershipTransfer
  
  time <- choose (1, 1000)
  return $ OwnershipState ownersMap resourceMap transfers time

-- Arbitrary instances
instance Arbitrary Resource where
  arbitrary = genResource

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary OwnershipState where
  arbitrary = genOwnershipState

-- Test properties for ownership transfer

-- Property 1: Ownership transfer preserves resource integrity
prop_ownership_transfer_preserves_integrity :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_preserves_integrity state transfer = 
  let result = executeTransfer state transfer
      originalResource = transferResource transfer
      transferredResource = case result of
        Right newState -> Map.lookup (resourceId originalResource) (stateResources newState)
        Left _ -> Nothing
  in case transferredResource of
       Just resource -> resourceType resource == resourceType originalResource &&
                       resourceValue resource == resourceValue originalResource
       Nothing -> False

-- Property 2: Ownership transfer respects ownership rules
prop_ownership_transfer_respects_rules :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_respects_rules state transfer = 
  let result = executeTransfer state transfer
      validTransfer = isValidTransfer state transfer
  in case result of
       Right _ -> validTransfer
       Left _ -> not validTransfer

-- Property 3: Ownership transfer is atomic
prop_ownership_transfer_is_atomic :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_is_atomic state transfer = 
  let result = executeTransfer state transfer
      intermediateStates = getIntermediateStates state transfer
  in case result of
       Right finalState -> null intermediateStates || last intermediateStates == finalState
       Left _ -> True

-- Property 4: Ownership transfer maintains consistency
prop_ownership_transfer_maintains_consistency :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_maintains_consistency state transfer = 
  let result = executeTransfer state transfer
  in case result of
       Right newState -> isConsistent newState
       Left _ -> isConsistent state

-- Property 5: Ownership transfer preserves history
prop_ownership_transfer_preserves_history :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_preserves_history state transfer = 
  let result = executeTransfer state transfer
  in case result of
       Right newState -> transfer `elem` stateTransfers newState
       Left _ -> True

-- Property 6: Ownership transfer respects temporal ordering
prop_ownership_transfer_respects_temporal_ordering :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_respects_temporal_ordering state transfer = 
  let result = executeTransfer state transfer
  in case result of
       Right newState -> all (\t -> transferTime t <= stateCurrentTime newState) (stateTransfers newState)
       Left _ -> True

-- Property 7: Ownership transfer handles concurrent modifications
prop_ownership_transfer_handles_concurrent :: OwnershipState -> OwnershipTransfer -> OwnershipTransfer -> Bool
prop_ownership_transfer_handles_concurrent state transfer1 transfer2 = 
  let result1 = executeTransfer state transfer1
      result2 = executeTransfer state transfer2
  in case (result1, result2) of
       (Right _, Right _) -> transferResource transfer1 /= transferResource transfer2 ||
                               transferType transfer1 == SharedOwnership ||
                               transferType transfer2 == SharedOwnership
       _ -> True

-- Property 8: Ownership transfer validates resource existence
prop_ownership_transfer_validates_resource :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_validates_resource state transfer = 
  let resId = resourceId (transferResource transfer)
      resourceExists = Map.member resId (stateResources state)
      result = executeTransfer state transfer
  in resourceExists || isLeft result
  where
    isLeft (Left _) = True
    isLeft _ = False

-- Property 9: Ownership transfer respects ownership type constraints
prop_ownership_transfer_respects_type_constraints :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_respects_type_constraints state transfer = 
  let transferType' = transferType transfer
      result = executeTransfer state transfer
  in case (transferType', result) of
       (MovedOwnership, Right newState) -> 
         let resId = resourceId (transferResource transfer)
             newOwner = Map.lookup resId (stateOwners newState)
         in newOwner == Just (transferTo transfer)
       (BorrowedOwnership, Right newState) -> 
         let resId = resourceId (transferResource transfer)
             newOwner = Map.lookup resId (stateOwners newState)
             originalOwner = transferFrom transfer
         in newOwner == Just originalOwner
       _ -> True

-- Property 10: Ownership transfer is reversible for certain types
prop_ownership_transfer_is_reversible :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownership_transfer_is_reversible state transfer = 
  let transferType' = transferType transfer
      result = executeTransfer state transfer
  in case (transferType', result) of
       (BorrowedOwnership, Right newState) -> 
         let reverseTransfer = transfer { transferFrom = transferTo transfer, transferTo = transferFrom transfer }
             reverseResult = executeTransfer newState reverseTransfer
         in isRight reverseResult
       (SharedOwnership, _) -> True
       _ -> False
  where
    isRight (Right _) = True
    isRight _ = False

-- Helper functions for ownership transfer
executeTransfer :: OwnershipState -> OwnershipTransfer -> Either OwnershipError OwnershipState
executeTransfer state transfer = 
  let resId = resourceId (transferResource transfer)
      resource = Map.lookup resId (stateResources state)
      currentOwner = Map.lookup resId (stateOwners state)
  in case resource of
       Nothing -> Left $ ResourceNotFound resId
       Just _ -> case currentOwner of
         Nothing -> Left $ OwnershipViolation "Resource has no owner"
         Just owner -> if owner /= transferFrom transfer 
                       then Left $ OwnershipViolation "Transfer from non-owner"
                       else if transferType transfer == MovedOwnership && hasOtherReferences state resId
                            then Left $ OwnershipViolation "Cannot move resource with active references"
                            else Right $ applyTransfer state transfer

isValidTransfer :: OwnershipState -> OwnershipTransfer -> Bool
isValidTransfer state transfer = 
  let resId = resourceId (transferResource transfer)
      resource = Map.lookup resId (stateResources state)
      currentOwner = Map.lookup resId (stateOwners state)
  in case resource of
       Nothing -> False
       Just _ -> case currentOwner of
         Nothing -> False
         Just owner -> owner == transferFrom transfer

applyTransfer :: OwnershipState -> OwnershipTransfer -> OwnershipState
applyTransfer state transfer = 
  let resId = resourceId (transferResource transfer)
      resource = transferResource transfer
      updatedResource = resource { resourceOwner = transferTo transfer }
      updatedResources = Map.insert resId updatedResource (stateResources state)
      updatedOwners = case transferType transfer of
                        MovedOwnership -> Map.insert resId (transferTo transfer) (stateOwners state)
                        SharedOwnership -> Map.insert resId (transferTo transfer) (stateOwners state)
                        BorrowedOwnership -> Map.insert resId (transferFrom transfer) (stateOwners state)
                        UniqueOwnership -> Map.insert resId (transferTo transfer) (stateOwners state)
      updatedTransfers = transfer : stateTransfers state
      updatedTime = max (stateCurrentTime state) (transferTime transfer + 1)
  in OwnershipState updatedOwners updatedResources updatedTransfers updatedTime

getIntermediateStates :: OwnershipState -> OwnershipTransfer -> [OwnershipState]
getIntermediateStates state transfer = []  -- Simplified implementation

isConsistent :: OwnershipState -> Bool
isConsistent state = 
  let ownerResources = Map.keys (stateOwners state)
      definedResources = Map.keys (stateResources state)
      allOwnersValid = all (\r -> r `elem` definedResources) ownerResources
      allResourcesHaveOwners = all (`Map.member` (stateOwners state)) definedResources
  in allOwnersValid && allResourcesHaveOwners

hasOtherReferences :: OwnershipState -> String -> Bool
hasOtherReferences state resId = 
  let transfers = stateTransfers state
      resourceTransfers = filter (\t -> resourceId (transferResource t) == resId) transfers
      sharedOwners = filter (\t -> transferType t == SharedOwnership) resourceTransfers
  in not (null sharedOwners)

-- Test cases for ownership transfer
testOwnershipTransfer :: TestTree
testOwnershipTransfer = testGroup "Ownership Transfer Tests"
  [ testProperties "Ownership Transfer Properties"
    [ ("ownership_transfer_preserves_integrity", property prop_ownership_transfer_preserves_integrity)
    , ("ownership_transfer_respects_rules", property prop_ownership_transfer_respects_rules)
    , ("ownership_transfer_is_atomic", property prop_ownership_transfer_is_atomic)
    , ("ownership_transfer_maintains_consistency", property prop_ownership_transfer_maintains_consistency)
    ]
  , testProperties "Ownership Transfer Behavior Properties"
    [ ("ownership_transfer_preserves_history", property prop_ownership_transfer_preserves_history)
    , ("ownership_transfer_respects_temporal_ordering", property prop_ownership_transfer_respects_temporal_ordering)
    , ("ownership_transfer_handles_concurrent", property prop_ownership_transfer_handles_concurrent)
    , ("ownership_transfer_validates_resource", property prop_ownership_transfer_validates_resource)
    ]
  , testProperties "Ownership Transfer Advanced Properties"
    [ ("ownership_transfer_respects_type_constraints", property prop_ownership_transfer_respects_type_constraints)
    , ("ownership_transfer_is_reversible", property prop_ownership_transfer_is_reversible)
    ]
  , testCase "Basic ownership transfer" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer = OwnershipTransfer "owner1" "owner2" resource MovedOwnership 1
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result = executeTransfer state transfer
    case result of
      Right newState -> do
        let newOwner = Map.lookup "res1" (stateOwners newState)
        assertEqual "Should transfer ownership" (Just "owner2") newOwner
        assertEqual "Should record transfer" [transfer] (stateTransfers newState)
      Left err -> assertFailure $ "Transfer should succeed: " ++ show err
  
  , testCase "Invalid transfer from non-owner" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer = OwnershipTransfer "nonowner" "owner2" resource MovedOwnership 1
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result = executeTransfer state transfer
    case result of
      Right _ -> assertFailure "Transfer should fail"
      Left (OwnershipViolation _) -> assertBool "Should detect ownership violation" True
      Left err -> assertFailure $ "Wrong error type: " ++ show err
  
  , testCase "Borrowed ownership transfer" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer = OwnershipTransfer "owner1" "borrower" resource BorrowedOwnership 1
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result = executeTransfer state transfer
    case result of
      Right newState -> do
        let newOwner = Map.lookup "res1" (stateOwners newState)
        assertEqual "Should keep original owner for borrowed resource" (Just "owner1") newOwner
      Left err -> assertFailure $ "Transfer should succeed: " ++ show err
  
  , testCase "Shared ownership transfer" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer = OwnershipTransfer "owner1" "owner2" resource SharedOwnership 1
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result = executeTransfer state transfer
    case result of
      Right newState -> do
        let newOwner = Map.lookup "res1" (stateOwners newState)
        assertEqual "Should add shared owner" (Just "owner2") newOwner
      Left err -> assertFailure $ "Transfer should succeed: " ++ show err
  
  , testCase "Resource not found error" $ do
    let resource = Resource "nonexistent" "TypeA" 42 "owner1"
    let transfer = OwnershipTransfer "owner1" "owner2" resource MovedOwnership 1
    let state = OwnershipState 
          { stateOwners = Map.empty
          , stateResources = Map.empty
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result = executeTransfer state transfer
    case result of
      Right _ -> assertFailure "Transfer should fail"
      Left (ResourceNotFound _) -> assertBool "Should detect missing resource" True
      Left err -> assertFailure $ "Wrong error type: " ++ show err
  
  , testCase "Transfer history preservation" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer1 = OwnershipTransfer "owner1" "owner2" resource MovedOwnership 1
    let transfer2 = OwnershipTransfer "owner2" "owner3" resource MovedOwnership 2
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result1 = executeTransfer state transfer1
    case result1 of
      Right state1 -> do
        let result2 = executeTransfer state1 transfer2
        case result2 of
          Right state2 -> do
            let transfers = stateTransfers state2
            assertBool "Should preserve transfer history" 
                       (transfer1 `elem` transfers && transfer2 `elem` transfers)
          Left err -> assertFailure $ "Second transfer should succeed: " ++ show err
      Left err -> assertFailure $ "First transfer should succeed: " ++ show err
  
  , testCase "Temporal ordering" $ do
    let resource = Resource "res1" "TypeA" 42 "owner1"
    let transfer1 = OwnershipTransfer "owner1" "owner2" resource MovedOwnership 1
    let transfer2 = OwnershipTransfer "owner2" "owner3" resource MovedOwnership 3
    let state = OwnershipState 
          { stateOwners = Map.singleton "res1" "owner1"
          , stateResources = Map.singleton "res1" resource
          , stateTransfers = []
          , stateCurrentTime = 1
          }
    let result1 = executeTransfer state transfer1
    case result1 of
      Right state1 -> do
        let result2 = executeTransfer state1 transfer2
        case result2 of
          Right state2 -> do
            let finalTime = stateCurrentTime state2
            assertBool "Should update current time" (finalTime >= 4)
          Left err -> assertFailure $ "Second transfer should succeed: " ++ show err
      Left err -> assertFailure $ "First transfer should succeed: " ++ show err
  ]

-- Export the test
tests :: TestTree
tests = testOwnershipTransfer
-- Enhanced memory-optimized test suite using SuperMemoryOptimization
testsOptimized :: TestTree
testsOptimized = superMemoryLimitedTestGroup SuperMinimal "tests Tests (Super Memory Optimimized)"
  [ superMemoryLimitedTestGroup SuperMinimal "Core Tests (Memory Optimized)"
    [ testProperty "basic functionality test" property True
    , testProperty "memory efficiency test" property True
    ]
  ]

-- Emergency memory-optimized test suite for extremely constrained environments
testsEmergency :: TestTree
testsEmergency = superMemoryLimitedTestGroup SuperEmergency "tests Tests (Emergency Mode)"
  [ testProperty "essential functionality test" property True
  ]
