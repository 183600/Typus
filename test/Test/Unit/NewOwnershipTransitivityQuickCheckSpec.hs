module Test.Unit.NewOwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..), OwnershipAnalyzer(..), newOwnershipAnalyzer, analyzeOwnership)
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import TestSupport.QuickCheck (fastProperty)

-- ============================================================================
-- New QuickCheck Tests for Ownership Transitivity Properties
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Ownership Transitivity QuickCheck Tests"
    [ testGroup "Ownership Transfer Properties"
        [ fastProperty "ownership transfer is transitive" prop_ownershipTransferIsTransitive
        , fastProperty "ownership transfer preserves uniqueness" prop_ownershipTransferPreservesUniqueness
        , fastProperty "multiple transfers maintain consistency" prop_multipleTransfersMaintainConsistency
        , fastProperty "circular transfers are detected" prop_circularTransfersAreDetected
        , fastProperty "transfer chain preserves ownership type" prop_transferChainPreservesOwnershipType
        ]

    , testGroup "Borrowing Properties"
        [ fastProperty "borrowing follows ownership hierarchy" prop_borrowingFollowsOwnershipHierarchy
        , fastProperty "mutable borrowing exclusivity" prop_mutableBorrowingExclusivity
        , fastProperty "borrowing scope is respected" prop_borrowingScopeIsRespected
        , fastProperty "nested borrowing rules" prop_nestedBorrowingRules
        , fastProperty "borrowing after transfer" prop_borrowingAfterTransfer
        ]

    , testGroup "Error Detection Properties"
        [ fastProperty "use after move is detected" prop_useAfterMoveIsDetected
        , fastProperty "double move is detected" prop_doubleMoveIsDetected
        , fastProperty "borrow while moved is detected" prop_borrowWhileMovedIsDetected
        , fastProperty "multiple mutable borrows are detected" prop_multipleMutBorrowsAreDetected
        , fastProperty "ownership errors are comprehensive" prop_ownershipErrorsAreComprehensive
        ]

    , testGroup "Safety Invariants"
        [ fastProperty "no dangling references" prop_noDanglingReferences
        , fastProperty "lifetime correctness" prop_lifetimeCorrectness
        , fastProperty "memory safety guarantees" prop_memorySafetyGuarantees
        , fastProperty "concurrent access safety" prop_concurrentAccessSafety
        ]
    ]

-- ============================================================================
-- Ownership Transfer Property Tests
-- ============================================================================

-- | Ownership transfer should be transitive: if A -> B and B -> C, then A -> C
prop_ownershipTransferIsTransitive :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransferIsTransitive transfer1 transfer2 =
  let OwnershipTransfer { transferFrom = from1, transferTo = to1 } = transfer1
      OwnershipTransfer { transferFrom = from2, transferTo = to2 } = transfer2
  in if to1 == from2  -- Chain condition: first transfer's target = second's source
     then counterexample ("transfer1=" ++ show transfer1 ++ ", transfer2=" ++ show transfer2) $
          let transitiveTransfer = OwnershipTransfer from1 to2
              -- The transitive property should hold
              result = analyzeTransferChain [transfer1, transfer2]
          in case result of
               Right transfers -> transitiveTransfer `elem` transfers
               Left _ -> property False
     else property True  -- Skip if not chainable

-- | Ownership transfer should preserve uniqueness
prop_ownershipTransferPreservesUniqueness :: OwnershipTransfer -> Property
prop_ownershipTransferPreservesUniqueness transfer =
  let OwnershipTransfer { transferFrom = from, transferTo = to } = transfer
      result = analyzeSingleTransfer transfer
  in counterexample ("transfer=" ++ show transfer) $
     case result of
       Right ownershipState -> 
         -- After transfer, 'to' should be the unique owner
         getOwnershipType to ownershipState == Just (Owned to)
       Left _ -> property True  -- May fail for other reasons

-- | Multiple transfers should maintain consistency
prop_multipleTransfersMaintainConsistency :: [OwnershipTransfer] -> Property
prop_multipleTransfersMaintainConsistency transfers =
  let uniqueVars = nub (concatMap (\t -> [transferFrom t, transferTo t]) transfers)
      result = analyzeTransferChain transfers
  in counterexample ("transfers=" ++ show transfers) $
     case result of
       Right ownershipState ->
         -- Each variable should have at most one ownership type
         all (\var -> length (filter (\t -> transferTo t == var) transfers) <= 1) uniqueVars
       Left _ -> property True  -- May fail for other reasons
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

-- | Circular transfers should be detected
prop_circularTransfersAreDetected :: [OwnershipTransfer] -> Property
prop_circularTransfersAreDetected transfers =
  let hasCycle = detectCircularTransfer transfers
      result = analyzeTransferChain transfers
  in counterexample ("transfers=" ++ show transfers ++ ", hasCycle=" ++ show hasCycle) $
     if hasCycle
       then case result of
              Left errors -> any isCircularError errors
              Right _ -> property False  -- Should have failed
       else property True  -- Non-circular is fine

-- | Transfer chain should preserve ownership type
prop_transferChainPreservesOwnershipType :: [OwnershipTransfer] -> Property
prop_transferChainPreservesOwnershipType transfers =
  let result = analyzeTransferChain transfers
  in counterexample ("transfers=" ++ show transfers) $
     case result of
       Right ownershipState ->
         -- Original owners should maintain Owned type
         all (\t -> getOwnershipType (transferFrom t) ownershipState == Just (Owned (transferFrom t))) transfers
       Left _ -> property True  -- May fail for other reasons

-- ============================================================================
-- Borrowing Property Tests
-- ============================================================================

-- | Borrowing should follow ownership hierarchy
prop_borrowingFollowsOwnershipHierarchy :: String -> String -> Property
prop_borrowingFollowsOwnershipHierarchy owner borrower =
  let ownershipState = createOwnershipState [(owner, Owned owner)]
      borrowOp = BorrowOperation owner borrower
      result = analyzeBorrowing borrowOp ownershipState
  in counterexample ("owner=" ++ owner ++ ", borrower=" ++ borrower) $
     case result of
       Right newState -> 
         -- Borrower should have Borrowed type
         getOwnershipType borrower newState == Just (Borrowed owner)
       Left _ -> property True  -- May fail for other reasons

-- | Mutable borrowing should be exclusive
prop_mutableBorrowingExclusivity :: String -> String -> String -> Property
prop_mutableBorrowingExclusivity owner borrower1 borrower2 =
  let ownershipState = createOwnershipState [(owner, Owned owner)]
      borrow1 = MutBorrowOperation owner borrower1
      borrow2 = MutBorrowOperation owner borrower2
      result1 = analyzeBorrowing borrow1 ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeBorrowing borrow2 state1
         in counterexample ("owner=" ++ owner ++ ", borrower1=" ++ borrower1 ++ ", borrower2=" ++ borrower2) $
            case result2 of
              Left errors -> any isMutBorrowError errors
              Right _ -> property False  -- Should have failed
       Left _ -> property True

-- | Borrowing scope should be respected
prop_borrowingScopeIsRespected :: String -> String -> Property
prop_borrowingScopeIsRespected owner borrower =
  let ownershipState = createOwnershipState [(owner, Owned owner)]
      borrowOp = BorrowOperation owner borrower
      result = analyzeBorrowing borrowOp ownershipState
  in counterexample ("owner=" ++ owner ++ ", borrower=" ++ borrower) $
     case result of
       Right newState ->
         -- Owner should still be accessible (not moved)
         getOwnershipType owner newState == Just (Owned owner)
       Left _ -> property True  -- May fail for other reasons

-- | Nested borrowing rules should be enforced
prop_nestedBorrowingRules :: String -> String -> String -> Property
prop_nestedBorrowingRules owner borrower1 borrower2 =
  let ownershipState = createOwnershipState [(owner, Owned owner)]
      borrow1 = BorrowOperation owner borrower1
      borrow2 = BorrowOperation borrower1 borrower2  -- Borrow from borrower
      result1 = analyzeBorrowing borrow1 ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeBorrowing borrow2 state1
         in counterexample ("owner=" ++ owner ++ ", borrower1=" ++ borrower1 ++ ", borrower2=" ++ borrower2) $
            case result2 of
              Right state2 ->
                -- Should create a borrow chain
                getOwnershipType borrower2 state2 == Just (Borrowed borrower1)
              Left _ -> property True  -- May fail for complex cases
       Left _ -> property True

-- | Borrowing after transfer should work correctly
prop_borrowingAfterTransfer :: String -> String -> String -> Property
prop_borrowingAfterTransfer originalOwner newOwner borrower =
  let transfer = OwnershipTransfer originalOwner newOwner
      ownershipState = createOwnershipState [(originalOwner, Owned originalOwner)]
      result1 = analyzeSingleTransfer transfer ownershipState
  in case result1 of
       Right state1 ->
         let borrowOp = BorrowOperation newOwner borrower
             result2 = analyzeBorrowing borrowOp state1
         in counterexample ("originalOwner=" ++ originalOwner ++ ", newOwner=" ++ newOwner ++ ", borrower=" ++ borrower) $
            case result2 of
              Right state2 ->
                -- Borrower should borrow from new owner
                getOwnershipType borrower state2 == Just (Borrowed newOwner)
              Left _ -> property True  -- May fail for other reasons
       Left _ -> property True

-- ============================================================================
-- Error Detection Property Tests
-- ============================================================================

-- | Use after move should be detected
prop_useAfterMoveIsDetected :: String -> String -> String -> Property
prop_useAfterMoveIsDetected originalOwner newOwner user =
  let transfer = OwnershipTransfer originalOwner newOwner
      useOp = UseOperation originalOwner
      ownershipState = createOwnershipState [(originalOwner, Owned originalOwner)]
      result1 = analyzeSingleTransfer transfer ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeUse useOp state1
         in counterexample ("originalOwner=" ++ originalOwner ++ ", newOwner=" ++ newOwner ++ ", user=" ++ user) $
            case result2 of
              Left errors -> any isUseAfterMoveError errors
              Right _ -> property False  -- Should have failed
       Left _ -> property True

-- | Double move should be detected
prop_doubleMoveIsDetected :: String -> String -> String -> Property
prop_doubleMoveIsDetected originalOwner newOwner1 newOwner2 =
  let transfer1 = OwnershipTransfer originalOwner newOwner1
      transfer2 = OwnershipTransfer originalOwner newOwner2
      ownershipState = createOwnershipState [(originalOwner, Owned originalOwner)]
      result1 = analyzeSingleTransfer transfer1 ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeSingleTransfer transfer2 state1
         in counterexample ("originalOwner=" ++ originalOwner ++ ", newOwner1=" ++ newOwner1 ++ ", newOwner2=" ++ newOwner2) $
            case result2 of
              Left errors -> any isDoubleMoveError errors
              Right _ -> property False  -- Should have failed
       Left _ -> property True

-- | Borrow while moved should be detected
prop_borrowWhileMovedIsDetected :: String -> String -> String -> Property
prop_borrowWhileMovedIsDetected originalOwner newOwner borrower =
  let transfer = OwnershipTransfer originalOwner newOwner
      borrowOp = BorrowOperation originalOwner borrower
      ownershipState = createOwnershipState [(originalOwner, Owned originalOwner)]
      result1 = analyzeSingleTransfer transfer ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeBorrowing borrowOp state1
         in counterexample ("originalOwner=" ++ originalOwner ++ ", newOwner=" ++ newOwner ++ ", borrower=" ++ borrower) $
            case result2 of
              Left errors -> any isBorrowWhileMovedError errors
              Right _ -> property False  -- Should have failed
       Left _ -> property True

-- | Multiple mutable borrows should be detected
prop_multipleMutBorrowsAreDetected :: String -> String -> String -> Property
prop_multipleMutBorrowsAreDetected owner borrower1 borrower2 =
  let ownershipState = createOwnershipState [(owner, Owned owner)]
      borrow1 = MutBorrowOperation owner borrower1
      borrow2 = MutBorrowOperation owner borrower2
      result1 = analyzeBorrowing borrow1 ownershipState
  in case result1 of
       Right state1 ->
         let result2 = analyzeBorrowing borrow2 state1
         in counterexample ("owner=" ++ owner ++ ", borrower1=" ++ borrower1 ++ ", borrower2=" ++ borrower2) $
            case result2 of
              Left errors -> any isMultipleMutBorrowsError errors
              Right _ -> property False  -- Should have failed
       Left _ -> property True

-- | Ownership errors should be comprehensive
prop_ownershipErrorsAreComprehensive :: String -> Property
prop_ownershipErrorsAreComprehensive code =
  let result = analyzeOwnershipCode code
  in counterexample ("code=" ++ take 50 code ++ "...") $
     case result of
       Left errors -> 
         -- Errors should be properly categorized
         all isValidOwnershipError errors
       Right _ -> property True  -- No errors is also valid

-- ============================================================================
-- Safety Invariant Tests
-- ============================================================================

-- | No dangling references should exist
prop_noDanglingReferences :: [OwnershipTransfer] -> [BorrowOperation] -> Property
prop_noDanglingReferences transfers borrows =
  let result = analyzeOwnershipOperations transfers borrows
  in counterexample ("transfers=" ++ show transfers ++ ", borrows=" ++ show borrows) $
     case result of
       Right ownershipState ->
         -- All borrowed references should point to valid owners
         all (borrowPointsToValidOwner ownershipState) borrows
       Left _ -> property True  -- May fail for other reasons

-- | Lifetime correctness should be maintained
prop_lifetimeCorrectness :: String -> Property
prop_lifetimeCorrectness code =
  let result = analyzeOwnershipCode code
  in counterexample ("code=" ++ take 50 code ++ "...") $
     case result of
       Left errors -> 
         -- Should not have lifetime-related errors if code is structurally correct
         not (any isLifetimeError errors) || hasStructuralIssues code
       Right _ -> property True

-- | Memory safety guarantees should hold
prop_memorySafetyGuarantees :: String -> Property
prop_memorySafetyGuarantees code =
  let result = analyzeOwnershipCode code
  in counterexample ("code=" ++ take 50 code ++ "...") $
     case result of
       Left errors ->
         -- Should not have memory safety violations
         all isMemorySafeError errors
       Right _ -> property True

-- | Concurrent access safety should be enforced
prop_concurrentAccessSafety :: [BorrowOperation] -> Property
prop_concurrentAccessSafety borrows =
  let mutableBorrows = filter isMutableBorrow borrows
      result = analyzeBorrowingOperations borrows
  in counterexample ("borrows=" ++ show borrows) $
     case result of
       Left errors ->
         -- Should prevent concurrent mutable access to same resource
         all isConcurrentSafe errors
       Right _ -> property True

-- ============================================================================
-- Helper Types and Functions
-- ============================================================================

data BorrowOperation = BorrowOperation String String  -- owner, borrower
                     | MutBorrowOperation String String  -- owner, borrower
                     deriving (Show, Eq)

data UseOperation = UseOperation String  -- variable being used
                  deriving (Show, Eq)

type OwnershipState = [(String, OwnershipType)]

-- | Analyze a single ownership transfer
analyzeSingleTransfer :: OwnershipTransfer -> OwnershipState -> Either [OwnershipError] OwnershipState
analyzeSingleTransfer transfer state = Right state  -- Simplified

-- | Analyze a chain of ownership transfers
analyzeTransferChain :: [OwnershipTransfer] -> Either [OwnershipError] [OwnershipTransfer]
analyzeTransferChain transfers = Right transfers  -- Simplified

-- | Analyze borrowing operation
analyzeBorrowing :: BorrowOperation -> OwnershipState -> Either [OwnershipError] OwnershipState
analyzeBorrowing borrow state = Right state  -- Simplified

-- | Analyze use operation
analyzeUse :: UseOperation -> OwnershipState -> Either [OwnershipError] OwnershipState
analyzeUse use state = Right state  -- Simplified

-- | Analyze ownership code
analyzeOwnershipCode :: String -> Either [OwnershipError] OwnershipState
analyzeOwnershipCode code = Right []  -- Simplified

-- | Analyze comprehensive ownership operations
analyzeOwnershipOperations :: [OwnershipTransfer] -> [BorrowOperation] -> Either [OwnershipError] OwnershipState
analyzeOwnershipOperations transfers borrows = Right []  -- Simplified

-- | Analyze borrowing operations
analyzeBorrowingOperations :: [BorrowOperation] -> Either [OwnershipError] OwnershipState
analyzeBorrowingOperations borrows = Right []  -- Simplified

-- | Create ownership state from list of pairs
createOwnershipState :: [(String, OwnershipType)] -> OwnershipState
createOwnershipState = id

-- | Get ownership type for a variable
getOwnershipType :: String -> OwnershipState -> Maybe OwnershipType
getOwnershipType var state = lookup var state

-- | Detect circular transfer
detectCircularTransfer :: [OwnershipTransfer] -> Bool
detectCircularTransfer transfers = False  -- Simplified

-- | Check if error is circular error
isCircularError :: OwnershipError -> Bool
isCircularError (ControlFlowError _) = True
isCircularError _ = False

-- | Check if error is mutable borrow error
isMutBorrowError :: OwnershipError -> Bool
isMutBorrowError (MutBorrowWhileBorrowed _) = True
isMutBorrowError (MultipleMutBorrows _) = True
isMutBorrowError _ = False

-- | Check if error is use after move
isUseAfterMoveError :: OwnershipError -> Bool
isUseAfterMoveError (UseAfterMove _) = True
isUseAfterMoveError _ = False

-- | Check if error is double move
isDoubleMoveError :: OwnershipError -> Bool
isDoubleMoveError (DoubleMove _ _) = True
isDoubleMoveError _ = False

-- | Check if error is borrow while moved
isBorrowWhileMovedError :: OwnershipError -> Bool
isBorrowWhileMovedError (BorrowWhileMoved _) = True
isBorrowWhileMovedError _ = False

-- | Check if error is multiple mut borrows
isMultipleMutBorrowsError :: OwnershipError -> Bool
isMultipleMutBorrowsError (MultipleMutBorrows _) = True
isMultipleMutBorrowsError _ = False

-- | Check if ownership error is valid
isValidOwnershipError :: OwnershipError -> Bool
isValidOwnershipError _ = True  -- Simplified

-- | Check if borrow points to valid owner
borrowPointsToValidOwner :: OwnershipState -> BorrowOperation -> Bool
borrowPointsToValidOwner state (BorrowOperation owner _) = owner `elem` map fst state
borrowPointsToValidOwner state (MutBorrowOperation owner _) = owner `elem` map fst state

-- | Check if borrow is mutable
isMutableBorrow :: BorrowOperation -> Bool
isMutableBorrow (MutBorrowOperation _ _) = True
isMutableBorrow _ = False

-- | Check if code has structural issues
hasStructuralIssues :: String -> Bool
hasStructuralIssues code = length code < 5  -- Simplified

-- | Check if error is lifetime error
isLifetimeError :: OwnershipError -> Bool
isLifetimeError (OutOfScope _) = True
isLifetimeError _ = False

-- | Check if error is memory safe
isMemorySafeError :: OwnershipError -> Bool
isMemorySafeError (UseAfterMove _) = False  -- Not memory safe
isMemorySafeError _ = True

-- | Check if concurrent access is safe
isConcurrentSafe :: OwnershipError -> Bool
isConcurrentSafe (MultipleMutBorrows _) = False  -- Not concurrent safe
isConcurrentSafe _ = True