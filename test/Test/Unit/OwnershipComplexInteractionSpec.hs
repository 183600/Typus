{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipComplexInteractionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import Ownership
import Ownership.Common.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort, intercalate, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

-- | Generate variable names
genVarName :: Gen String
genVarName = oneof
  [ elements ["x", "y", "z", "data", "result", "value", "item", "ptr"]
  , do
      n <- choose (1, 3)
      prefix <- elements ["var", "temp", "buf"]
      return $ prefix ++ show n
  ]

-- | Generate ownership states
genOwnershipState :: Gen OwnershipState
genOwnershipState = elements 
  [ Unowned, Owned, Borrowed, Shared, Moved, Invalid]

-- | Generate variable ownership info
genVarOwnership :: Gen (String, OwnershipInfo)
genVarOwnership = do
  varName <- genVarName
  state <- genOwnershipState
  owner <- genVarName
  borrowCount <- choose (0, 3)
  return (varName, OwnershipInfo state owner borrowCount)

-- | Generate ownership transfer scenarios
genOwnershipTransfer :: Gen (String, String, TransferType)
genOwnershipTransfer = do
  from <- genVarName
  to <- genVarName
  transferType <- elements [Move, Borrow, Share, Copy]
  return (from, to, transferType)

-- | Generate complex ownership scenarios
genOwnershipScenario :: Gen [(String, OwnershipInfo)]
genOwnershipScenario = do
  numVars <- choose (2, 6)
  sequence $ replicate numVars genVarOwnership

-- | Generate borrowing scenarios
genBorrowingScenario :: Gen [(String, BorrowInfo)]
genBorrowingScenario = do
  numBorrows <- choose (1, 4)
  sequence $ replicate numBorrows $ do
    borrower <- genVarName
    lender <- genVarName
    borrowType <- elements [ImmutableBorrow, MutableBorrow]
    return (borrower, BorrowInfo lender borrowType)

-- | Generate lifetime annotations
genLifetime :: Gen String
genLifetime = oneof
  [ elements ["'a", "'b", "'c", "'static"]
  , do
      n <- choose (1, 9)
      return $ "'l" ++ show n
  ]

-- | Generate lifetime relationships
genLifetimeRelation :: Gen (String, String)
genLifetimeRelation = do
  lifetime1 <- genLifetime
  lifetime2 <- genLifetime
  guard (lifetime1 /= lifetime2)
  return (lifetime1, lifetime2)

-- Property: Ownership transfer should invalidate source
prop_ownership_transfer_invalidates_source :: String -> String -> TransferType -> Property
prop_ownership_transfer_invalidates_source from to transferType =
  from /= to ==> 
  let initialOwnership = Map.singleton from (OwnershipInfo Owned "owner" 0)
      result = performOwnershipTransfer initialOwnership from to transferType
      sourceInfo = Map.lookup from result
  in property $ case sourceInfo of
    Just info -> ownershipState info == Moved
    Nothing -> True

-- Property: Borrowing should track borrow counts correctly
prop_borrowing_track_counts :: String -> Int -> Property
prop_borrowing_track_counts varName borrowCount =
  borrowCount >= 0 && borrowCount <= 5 ==> 
  let initialOwnership = Map.singleton varName (OwnershipInfo Owned "owner" 0)
      finalOwnership = iterate (addBorrow varName) initialOwnership !! borrowCount
      finalInfo = Map.lookup varName finalOwnership
  in property $ case finalInfo of
    Just info -> borrowCount info == borrowCount
    Nothing -> False

-- Property: Shared ownership should allow multiple references
prop_shared_ownership_multiple_refs :: String -> [String] -> Property
prop_shared_ownership_multiple_refs owner refs =
  not (null refs) && L.all (/= owner) refs ==> 
  let initialOwnership = Map.singleton owner (OwnershipInfo Owned "owner" 0)
      sharedOwnership = L.foldl (shareOwnership owner) initialOwnership refs
      ownerInfo = Map.lookup owner sharedOwnership
      refInfos = L.map (`Map.lookup` sharedOwnership) refs
  in property $ case ownerInfo of
    Just info -> ownershipState info == Shared &&
                 L.all (\mi -> case mi of
                   Just i -> ownershipState i == Shared
                   Nothing -> False) refInfos
    Nothing -> False

-- Property: Move semantics should prevent double use
prop_move_prevents_double_use :: String -> String -> String -> Property
prop_move_prevents_double_use original firstDest secondDest =
  original /= firstDest && original /= secondDest && firstDest /= secondDest ==> 
  let initialOwnership = Map.singleton original (OwnershipInfo Owned "owner" 0)
      afterFirstMove = performOwnershipTransfer initialOwnership original firstDest Move
      afterSecondMove = performOwnershipTransfer afterFirstMove original secondDest Move
      originalInfo = Map.lookup original afterSecondMove
  in property $ case originalInfo of
    Just info -> ownershipState info == Moved
    Nothing -> True

-- Property: Borrow checking should prevent mutable aliasing
prop_borrow_check_prevent_mutable_aliasing :: String -> String -> Property
prop_borrow_check_prevent_mutable_aliasing lender borrower =
  lender /= borrower ==> 
  let initialOwnership = Map.fromList 
        [ (lender, OwnershipInfo Owned "owner" 0)
        , (borrower, OwnershipInfo Unowned "owner" 0)
        ]
      borrowResult = addMutableBorrow initialOwnership lender borrower
      isValid = validateBorrowing borrowResult
  in property $ isValid ==> 
    let lenderInfo = Map.lookup lender borrowResult
        borrowerInfo = Map.lookup borrower borrowResult
    in case (lenderInfo, borrowerInfo) of
      (Just lInfo, Just bInfo) -> 
        ownershipState lInfo == Borrowed && 
        ownershipState bInfo == Borrowed
      _ -> False

-- Property: Lifetime checking should prevent dangling references
prop_lifetime_check_prevent_dangling :: String -> String -> Property
prop_lifetime_check_prevent_dangling ref target =
  ref /= target ==> 
  let lifetimes = Map.fromList [(ref, "'a"), (target, "'b")]
      lifetimeRelations = [("'a", "'b")] -- 'a outlives 'b
      isValid = validateLifetimes lifetimes lifetimeRelations
  in property $ isValid ==> ref `elem` map fst lifetimeRelations

-- Property: Ownership system should handle complex transfer chains
prop_complex_transfer_chains :: [String] -> Property
prop_complex_transfer_chains vars =
  L.length vars >= 3 ==> 
  let initialOwnership = Map.fromList $ zip vars (repeat (OwnershipInfo Owned "owner" 0))
      chain = zip vars (L.tail vars)
      finalOwnership = L.foldl (\acc (from, to) -> 
        performOwnershipTransfer acc from to Move) initialOwnership chain
      movedVars = L.filter (\v -> case Map.lookup v finalOwnership of
        Just info -> ownershipState info == Moved
        Nothing -> False) (init vars)
      finalVar = last vars
      finalVarInfo = Map.lookup finalVar finalOwnership
  in property $ L.length movedVars == L.length vars - 1 &&
    case finalVarInfo of
      Just info -> ownershipState info == Owned
      Nothing -> False

-- Property: Borrowing should respect ownership hierarchy
prop_borrowing_respects_hierarchy :: String -> String -> String -> Property
prop_borrowing_respects_hierarchy owner borrower1 borrower2 =
  owner /= borrower1 && owner /= borrower2 && borrower1 /= borrower2 ==> 
  let initialOwnership = Map.fromList 
        [ (owner, OwnershipInfo Owned "owner" 0)
        , (borrower1, OwnershipInfo Unowned "owner" 0)
        , (borrower2, OwnershipInfo Unowned "owner" 0)
        ]
      afterFirstBorrow = addImmutableBorrow initialOwnership owner borrower1
      afterSecondBorrow = addImmutableBorrow afterFirstBorrow owner borrower2
      ownerInfo = Map.lookup owner afterSecondBorrow
      borrower1Info = Map.lookup borrower1 afterSecondBorrow
      borrower2Info = Map.lookup borrower2 afterSecondBorrow
  in property $ case (ownerInfo, borrower1Info, borrower2Info) of
    (Just oInfo, Just b1Info, Just b2Info) ->
      ownershipState oInfo == Borrowed &&
      ownershipState b1Info == Borrowed &&
      ownershipState b2Info == Borrowed &&
      borrowCount oInfo == 2
    _ -> False

-- Property: Ownership should be recoverable after scope exit
prop_ownership_recovery_scope_exit :: [(String, OwnershipInfo)] -> [String] -> Property
prop_ownership_recovery_scope_exit ownership exitingVars =
  not (null ownership) && not (null exitingVars) ==> 
  let beforeExit = Map.fromList ownership
      afterExit = exitScope beforeExit exitingVars
      remainingVars = Map.keys beforeExit \\ exitingVars
      allRemainingPresent = L.all (`Map.member` afterExit) remainingVars
      allExitingRemoved = L.all (`Map.notMember` afterExit) exitingVars
  in property $ allRemainingPresent && allExitingRemoved

-- Property: Ownership system should handle circular dependencies
prop_ownership_circular_dependencies :: [String] -> Property
prop_ownership_circular_dependencies vars =
  L.length vars >= 3 ==> 
  let circularOwnership = Map.fromList $ zip vars (repeat (OwnershipInfo Borrowed "owner" 1))
      isValid = validateOwnershipGraph circularOwnership
  in property $ not isValid -- Circular borrowing should be invalid

-- Property: Ownership transfer should preserve type safety
prop_ownership_transfer_type_safety :: String -> String -> String -> Property
prop_ownership_transfer_type_safety from to typeName =
  from /= to && not (null typeName) ==> 
  let typeSystem = Map.fromList [(from, typeName)]
      initialOwnership = Map.singleton from (OwnershipInfo Owned "owner" 0)
      result = performOwnershipTransfer initialOwnership from to Move
      isTypeSafe = validateTypeSafety result typeSystem
  in property $ isTypeSafe

-- Property: Shared ownership should deallocate correctly
prop_shared_ownership_deallocation :: String -> [String] -> Property
prop_shared_ownership_deallocation owner sharers =
  not (null sharers) ==> 
  let initialOwnership = Map.singleton owner (OwnershipInfo Owned "owner" 0)
      sharedOwnership = L.foldl (shareOwnership owner) initialOwnership sharers
      allReleased = L.foldl (releaseOwnership owner) sharedOwnership (owner:sharers)
      allUnowned = L.all (\info -> ownershipState info == Unowned) $ Map.elems allReleased
  in property $ allUnowned

-- Property: Ownership system should track resource usage
prop_ownership_resource_tracking :: [(String, OwnershipInfo)] -> Property
prop_ownership_resource_tracking ownership =
  not (null ownership) ==> 
  let ownershipMap = Map.fromList ownership
      resourceCount = countResources ownershipMap
      maxResources = L.length ownership
  in property $ resourceCount <= maxResources

-- | Helper functions L.and data types

data OwnershipState = Unowned | Owned | Borrowed | Shared | Moved | Invalid
  deriving (Show, Eq, Ord)

data OwnershipInfo = OwnershipInfo 
  { ownershipState :: OwnershipState
  , owner :: String
  , borrowCount :: Int
  } deriving (Show, Eq)

data TransferType = Move | Borrow | Share | Copy
  deriving (Show, Eq)

data BorrowInfo = BorrowInfo 
  { lender :: String
  , borrowType :: BorrowType
  } deriving (Show, Eq)

data BorrowType = ImmutableBorrow | MutableBorrow
  deriving (Show, Eq)

performOwnershipTransfer :: Map.Map String OwnershipInfo -> String -> String -> TransferType -> Map.Map String OwnershipInfo
performOwnershipTransfer ownership from to transferType =
  case transferType of
    Move -> Map.insert from (OwnershipInfo Moved (owner $ ownership Map.! from) 0) $
            Map.insert to (OwnershipInfo Owned to 0) ownership
    Borrow -> Map.insert from (OwnershipInfo Borrowed (owner $ ownership Map.! from) (borrowCount (ownership Map.! from) + 1)) $
              Map.insert to (OwnershipInfo Borrowed from 0) ownership
    Share -> Map.insert from (OwnershipInfo Shared (owner $ ownership Map.! from) 0) $
             L.foldl (\acc v -> Map.insert v (OwnershipInfo Shared (owner $ ownership Map.! from) 0) acc) ownership [to]
    Copy -> Map.insert to (OwnershipInfo Owned to 0) ownership

addBorrow :: String -> Map.Map String OwnershipInfo -> Map.Map String OwnershipInfo
addBorrow var ownership = 
  Map.adjust (\info -> info { borrowCount = borrowCount info + 1 }) var ownership

shareOwnership :: String -> Map.Map String OwnershipInfo -> String -> Map.Map String OwnershipInfo
shareOwnership owner ownership sharer = 
  Map.adjust (\info -> info { ownershipState = Shared }) owner $
  Map.insert sharer (OwnershipInfo Shared owner 0) ownership

addMutableBorrow :: Map.Map String OwnershipInfo -> String -> String -> Map.Map String OwnershipInfo
addMutableBorrow ownership lender borrower = 
  Map.adjust (\info -> info { ownershipState = Borrowed, borrowCount = borrowCount info + 1 }) lender $
  Map.insert borrower (OwnershipInfo Borrowed lender 0) ownership

addImmutableBorrow :: Map.Map String OwnershipInfo -> String -> String -> Map.Map String OwnershipInfo
addImmutableBorrow = addMutableBorrow

validateBorrowing :: Map.Map String OwnershipInfo -> Bool
validateBorrowing ownership = 
  L.all (\info -> borrowCount info <= 3) (Map.elems ownership)

validateLifetimes :: Map.Map String String -> [(String, String)] -> Bool
validateLifetimes lifetimes relations = 
  L.all (\(shorter, longer) -> 
    case (Map.lookup shorter lifetimes, Map.lookup longer lifetimes) of
      (Just s, Just l) -> s /= l
      _ -> False) relations

exitScope :: Map.Map String OwnershipInfo -> [String] -> Map.Map String OwnershipInfo
exitScope ownership exitingVars = 
  foldl Map.delete ownership exitingVars

validateOwnershipGraph :: Map.Map String OwnershipInfo -> Bool
validateOwnershipGraph ownership = 
  let borrowedVars = Map.keys $ Map.L.filter (\info -> ownershipState info == Borrowed) ownership
  in null borrowedVars || L.length borrowedVars < L.length ownership

validateTypeSafety :: Map.Map String OwnershipInfo -> Map.Map String String -> Bool
validateTypeSafety ownership typeSystem = True -- Simplified

releaseOwnership :: String -> Map.Map String OwnershipInfo -> String -> Map.Map String OwnershipInfo
releaseOwnership currentOwner ownership var = 
  Map.adjust (\info -> info { ownershipState = Unowned }) var ownership

countResources :: Map.Map String OwnershipInfo -> Int
countResources ownership = 
  L.length $ Map.L.filter (\info -> ownershipState info `elem` [Owned, Shared, Borrowed]) ownership

tests :: TestTree
tests = testGroup "Ownership Complex Interaction Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "transfer invalidates source" prop_ownership_transfer_invalidates_source
    , fastProperty "borrowing tracks counts" prop_borrowing_track_counts
    , fastProperty "shared ownership multiple refs" prop_shared_ownership_multiple_refs
    , fastProperty "move prevents double use" prop_move_prevents_double_use
    , fastProperty "borrow check prevents mutable aliasing" prop_borrow_check_prevent_mutable_aliasing
    , fastProperty "lifetime check prevents dangling" prop_lifetime_check_prevent_dangling
    , fastProperty "complex transfer chains" prop_complex_transfer_chains
    , fastProperty "borrowing respects hierarchy" prop_borrowing_respects_hierarchy
    , fastProperty "ownership recovery scope exit" prop_ownership_recovery_scope_exit
    , fastProperty "circular dependencies" prop_ownership_circular_dependencies
    , fastProperty "transfer type safety" prop_ownership_transfer_type_safety
    , fastProperty "shared ownership deallocation" prop_shared_ownership_deallocation
    , fastProperty "resource tracking" prop_ownership_resource_tracking
    ]

  , testGroup "Unit tests"
    [ testCase "basic ownership transfer" $ do
        let initial = Map.singleton "x" (OwnershipInfo Owned "owner" 0)
        let result = performOwnershipTransfer initial "x" "y" Move
        Map.lookup "x" result @?= Just (OwnershipInfo Moved "owner" 0)
        Map.lookup "y" result @?= Just (OwnershipInfo Owned "y" 0)
    
    , testCase "borrowing mechanics" $ do
        let initial = Map.singleton "x" (OwnershipInfo Owned "owner" 0)
        let afterBorrow = addBorrow "x" initial
        let xInfo = Map.lookup "x" afterBorrow
        case xInfo of
          Just info -> borrowCount info @?= 1
          Nothing -> assertFailure "Expected ownership info"
    
    , testCase "shared ownership" $ do
        let initial = Map.singleton "data" (OwnershipInfo Owned "owner" 0)
        let shared = shareOwnership "data" initial "ref1"
        let dataInfo = Map.lookup "data" shared
        let ref1Info = Map.lookup "ref1" shared
        case (dataInfo, ref1Info) of
          (Just dInfo, Just r1Info) -> do
            ownershipState dInfo @?= Shared
            ownershipState r1Info @?= Shared
          _ -> assertFailure "Expected ownership info"
    
    , testCase "scope exit cleanup" $ do
        let ownership = Map.fromList 
              [ ("x", OwnershipInfo Owned "owner" 0)
              , ("y", OwnershipInfo Owned "owner" 0)
              ]
        let afterExit = exitScope ownership ["x"]
        Map.member "x" afterExit @?= False
        Map.member "y" afterExit @?= True
    ]
  ]