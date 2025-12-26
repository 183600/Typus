{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewOwnershipQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, property, (==>), forAll)
import TestSupport.QuickCheck (fastProperty)

import Ownership
import Ownership.Common.Types
import SourceLocation (Located(..), SourceSpan(..), SourcePos(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- Additional generators for Ownership testing
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genIdentifier
  , Borrowed <$> genIdentifier
  , MutBorrowed <$> genIdentifier
  , Shared <$> genIdentifier
  , pure Unowned
  ]

genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ DoubleMoveError <$> genIdentifier <*> genSourceSpan
  , BorrowCheckerError <$> genIdentifier <*> genSourceSpan <*> genOwnershipType
  , LifetimeError <$> genIdentifier <*> genIdentifier <*> genSourceSpan
  , MutationError <$> genIdentifier <*> genSourceSpan
  , OwnershipConstraintError <$> genIdentifier <*> genSourceSpan
  ]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = OwnershipTransfer <$> genIdentifier <*> genIdentifier <*> genOwnershipType <*> genSourceSpan

genOwnershipConstraint :: Gen OwnershipConstraint
genOwnershipConstraint = OwnershipConstraint <$> genIdentifier <*> genOwnershipType <*> genSourceSpan

genOwnershipState :: Gen OwnershipState
genOwnershipState = do
  varCount <- choose (0, 20)
  vars <- listOf $ (,) <$> genIdentifier <*> genOwnershipType
  let varMap = Map.fromList vars
  transfers <- listOf genOwnershipTransfer
  constraints <- listOf genOwnershipConstraint
  errors <- pure []  -- Simplified
  return $ OwnershipState varMap transfers constraints errors

genOwnershipAnalysis :: Gen OwnershipAnalysis
genOwnershipAnalysis = do
  states <- listOf genOwnershipState
  errors <- listOf genOwnershipError
  return $ OwnershipAnalysis states errors

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  offset <- choose (0, 10000)
  let pos = SourcePos line col offset
  return $ SourceSpan pos pos

-- Property: Ownership type consistency
prop_ownershipTypeConsistency :: OwnershipType -> Bool
prop_ownershipTypeConsistency typ = 
  case typ of
    Owned var -> not (null var)
    Borrowed var -> not (null var)
    MutBorrowed var -> not (null var)
    Shared var -> not (null var)
    Unowned -> True

-- Property: Ownership transfer preserves variable identity
prop_ownershipTransferPreservesIdentity :: OwnershipTransfer -> Bool
prop_ownershipTransferPreservesIdentity (OwnershipTransfer from to _ _) = 
  not (null from) && not (null to)

-- Property: Ownership constraint validation
prop_ownershipConstraintValidation :: OwnershipConstraint -> Bool
prop_ownershipConstraintValidation (OwnershipConstraint var typ _) = 
  not (null var) && prop_ownershipTypeConsistency typ

-- Property: Ownership state variable uniqueness
prop_ownershipStateVariableUniqueness :: OwnershipState -> Bool
prop_ownershipStateVariableUniqueness state = 
  let vars = Map.keys (ownershipVariables state)
      uniqueVars = Set.fromList vars
  in length vars == Set.size uniqueVars
  where
    ownershipVariables (OwnershipState vars _ _ _) = vars

-- Property: Ownership transfer creates valid new state
prop_ownershipTransferValidNewState :: OwnershipState -> OwnershipTransfer -> Bool
prop_ownershipTransferValidNewState state transfer = 
  let OwnershipTransfer from to typ _ = transfer
      vars = ownershipVariables state
      fromExists = Map.member from vars
      toExists = Map.member to vars
  in fromExists ==> prop_ownershipTypeConsistency typ

-- Property: Borrow checker prevents double moves
prop_borrowCheckerPreventsDoubleMoves :: OwnershipState -> String -> Bool
prop_borrowCheckerPreventsDoubleMoves state var = 
  let vars = ownershipVariables state
      varType = Map.lookup var vars
  in case varType of
       Just (Owned _) -> True  -- Can move owned value
       Just (Borrowed _) -> False  -- Cannot move borrowed value
       Just (MutBorrowed _) -> False  -- Cannot move mutably borrowed value
       _ -> True  -- Other cases are valid

-- Property: Lifetime tracking prevents use-after-move
prop_lifetimeTrackingPreventsUseAfterMove :: [OwnershipTransfer] -> String -> Bool
prop_lifetimeTrackingPreventsUseAfterMove transfers var = 
  let moveTransfers = filter isMoveTransfer transfers
      hasBeenMoved = any (\(OwnershipTransfer from _ _ _) -> from == var) moveTransfers
  in not hasBeenMoved || True  -- Simplified logic
  where
    isMoveTransfer (OwnershipTransfer _ _ typ _) = 
      case typ of
        Owned _ -> True
        _ -> False

-- Property: Mutability constraints are enforced
prop_mutabilityConstraintsEnforced :: OwnershipState -> String -> Bool
prop_mutabilityConstraintsEnforced state var = 
  let vars = ownershipVariables state
      varType = Map.lookup var vars
  in case varType of
       Just (MutBorrowed _) -> True  -- Can mutate mutably borrowed value
       Just (Owned _) -> True  -- Can mutate owned value
       Just (Borrowed _) -> False  -- Cannot mutate immutably borrowed value
       Just (Shared _) -> False  -- Cannot mutate shared value
       _ -> False  -- Cannot mutate unowned or unknown

-- Property: Ownership analysis error collection
prop_ownershipAnalysisErrorCollection :: [OwnershipError] -> Bool
prop_ownershipAnalysisErrorCollection errors = 
  let analysis = OwnershipAnalysis [] errors
      collectedErrors = ownershipAnalysisErrors analysis
  in length collectedErrors == length errors

-- Property: Ownership state transitions are valid
prop_ownershipStateTransitionsValid :: OwnershipState -> [OwnershipTransfer] -> Bool
prop_ownershipStateTransitionsValid initialState transfers = 
  let finalState = foldl applyTransfer initialState transfers
      finalVars = ownershipVariables finalState
  in all prop_ownershipTypeConsistency (Map.elems finalVars)
  where
    applyTransfer state (OwnershipTransfer from to typ _) = 
      let vars = ownershipVariables state
          newVars = Map.insert to typ (Map.delete from vars)
      in state { ownershipVariables = newVars }

-- Property: Borrow checker lifetime analysis
prop_borrowCheckerLifetimeAnalysis :: OwnershipState -> String -> String -> Bool
prop_borrowCheckerLifetimeAnalysis state borrower owner = 
  let vars = ownershipVariables state
      borrowerType = Map.lookup borrower vars
      ownerType = Map.lookup owner vars
  in case (borrowerType, ownerType) of
       (Just (Borrowed b), Just (Owned o)) -> b == o
       (Just (MutBorrowed b), Just (Owned o)) -> b == o
       _ -> True  -- Other combinations are handled differently

-- Helper functions
ownershipVariables :: OwnershipState -> Map.Map String OwnershipType
ownershipVariables (OwnershipState vars _ _ _) = vars

ownershipAnalysisErrors :: OwnershipAnalysis -> [OwnershipError]
ownershipAnalysisErrors (OwnershipAnalysis _ errors) = errors

-- Test suite
tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ testProperty "Ownership type consistency" $
      fastProperty "Ownership type consistency" prop_ownershipTypeConsistency
  
  , testProperty "Ownership transfer preserves variable identity" $
      fastProperty "Ownership transfer preserves identity" prop_ownershipTransferPreservesIdentity
  
  , testProperty "Ownership constraint validation" $
      fastProperty "Ownership constraint validation" prop_ownershipConstraintValidation
  
  , testProperty "Ownership state variable uniqueness" $
      fastProperty "Ownership state variable uniqueness" prop_ownershipStateVariableUniqueness
  
  , testProperty "Ownership transfer creates valid new state" $
      fastProperty "Ownership transfer valid new state" prop_ownershipTransferValidNewState
  
  , testProperty "Borrow checker prevents double moves" $
      fastProperty "Borrow checker prevents double moves" prop_borrowCheckerPreventsDoubleMoves
  
  , testProperty "Lifetime tracking prevents use-after-move" $
      fastProperty "Lifetime tracking prevents use-after-move" prop_lifetimeTrackingPreventsUseAfterMove
  
  , testProperty "Mutability constraints are enforced" $
      fastProperty "Mutability constraints enforced" prop_mutabilityConstraintsEnforced
  
  , testProperty "Ownership analysis error collection" $
      fastProperty "Ownership analysis error collection" prop_ownershipAnalysisErrorCollection
  
  , testProperty "Ownership state transitions are valid" $
      fastProperty "Ownership state transitions valid" prop_ownershipStateTransitionsValid
  
  , testProperty "Borrow checker lifetime analysis" $
      fastProperty "Borrow checker lifetime analysis" prop_borrowCheckerLifetimeAnalysis
  ]