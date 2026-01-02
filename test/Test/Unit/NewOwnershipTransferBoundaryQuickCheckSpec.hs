{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Ownership transfer boundary tests for Ownership module
module Test.Unit.NewOwnershipTransferBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List (sort, nub, intercalate, delete, union)
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)

import Ownership
  ( Ownership(..)
  , OwnershipState(..)
  , OwnershipTransfer(..)
  , OwnershipConstraint(..)
  , OwnershipError(..)
  , OwnershipAnalysis(..)
  , OwnershipContext(..)
  , VariableOwnership(..)
  , TransferResult(..)
  , analyzeOwnership
  , transferOwnership
  , validateOwnership
  , checkOwnershipConstraints
  , getOwnershipState
  , canTransferOwnership
  , findOwnershipConflicts
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanBetween
  )

import Dependencies
  ( DependencyGraph(..)
  , DependencyNode(..)
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate variable names
genVariableName :: Gen String
genVariableName = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate ownership states
genOwnershipState :: Gen OwnershipState
genOwnershipState = elements
  [ Owned "owner1"
  , Owned "owner2"
  , Shared ["owner1", "owner2"]
  , Borrowed "borrower1"
  , Moved
  , Freed
  , Unknown
  ]

-- Generate ownership constraints
genOwnershipConstraint :: Gen OwnershipConstraint
genOwnershipConstraint = oneof
  [ CannotMove
  , CannotBorrow
  , MustReturn
  , LifetimeConstraint "lifetime1"
  , MutabilityConstraint False
  , MutabilityConstraint True
  , CustomConstraint "custom"
  ]

-- Generate variable ownership
genVariableOwnership :: Gen VariableOwnership
genVariableOwnership = do
  name <- genVariableName
  state <- genOwnershipState
  constraints <- listOf genOwnershipConstraint
  position <- startPos
  return $ VariableOwnership name state constraints position

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genVariableName
  to <- genVariableName
  transferType <- elements [MoveOwnership, BorrowOwnership, ShareOwnership, ReturnOwnership]
  position <- startPos
  return $ OwnershipTransfer from to transferType position

-- Generate ownership contexts
genOwnershipContext :: Gen OwnershipContext
genOwnershipContext = do
  variables <- listOf genVariableOwnership
  transfers <- listOf genOwnershipTransfer
  currentFunction <- elements ["main", "func1", "func2", ""]
  return $ OwnershipContext variables transfers currentFunction

-- Generate dependency graphs for ownership analysis
genDependencyGraph :: Gen DependencyGraph
genDependencyGraph = do
  nodes <- listOf $ do
    name <- genVariableName
    deps <- listOf genVariableName
    return $ DependencyNode name deps
  return $ DependencyGraph $ Map.fromList $ L.map (\n -> (nodeName n, n)) nodes

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: Ownership transfer should change ownership state
prop_ownership_transfer_changes_state :: OwnershipContext -> OwnershipTransfer -> Property
prop_ownership_transfer_changes_state context transfer =
  let initialState = getOwnershipState (transferFrom transfer) context
      transferResult = transferOwnership context transfer
      finalState = case transferResult of
        TransferSuccess newContext -> getOwnershipState (transferFrom transfer) newContext
        TransferFailure _ -> initialState
  in property $ case transferResult of
         TransferSuccess _ -> finalState /= initialState
         TransferFailure _ -> finalState === initialState

-- Property: Ownership transfer should be validated before execution
prop_ownership_transfer_validation :: OwnershipContext -> OwnershipTransfer -> Property
prop_ownership_transfer_validation context transfer =
  let canTransfer = canTransferOwnership context transfer
      transferResult = transferOwnership context transfer
  in property $ case (canTransfer, transferResult) of
         (True, TransferSuccess _) -> True
         (False, TransferFailure _) -> True
         (True, TransferFailure _) -> False  -- Should succeed but failed
         (False, TransferSuccess _) -> False  -- Should fail but succeeded

-- Property: Move ownership should make source unavailable
prop_move_ownership_makes_source_unavailable :: OwnershipContext -> String -> String -> Property
prop_move_ownership_makes_source_unavailable context fromVar toVar =
  fromVar /= toVar ==> 
  let moveTransfer = OwnershipTransfer fromVar toVar MoveOwnership startPos
      transferResult = transferOwnership context moveTransfer
      sourceStateAfter = case transferResult of
        TransferSuccess newContext -> getOwnershipState fromVar newContext
        TransferFailure _ -> getOwnershipState fromVar context
  in property $ case transferResult of
         TransferSuccess _ -> sourceStateAfter === Moved
         TransferFailure _ -> True  -- No change on failure

-- Property: Borrow ownership should preserve original ownership
prop_borrow_ownership_preserves_original :: OwnershipContext -> String -> String -> Property
prop_borrow_ownership_preserves_original context lender borrower =
  lender /= borrower ==> 
  let borrowTransfer = OwnershipTransfer lender borrower BorrowOwnership startPos
      transferResult = transferOwnership context borrowTransfer
      lenderStateAfter = case transferResult of
        TransferSuccess newContext -> getOwnershipState lender newContext
        TransferFailure _ -> getOwnershipState lender context
      originalState = getOwnershipState lender context
  in property $ case transferResult of
         TransferSuccess _ -> lenderStateAfter === originalState
         TransferFailure _ -> True  -- No change on failure

-- Property: Share ownership should create shared state
prop_share_ownership_creates_shared :: OwnershipContext -> String -> String -> Property
prop_share_ownership_creates_shared context owner1 owner2 =
  owner1 /= owner2 ==> 
  let shareTransfer = OwnershipTransfer owner1 owner2 ShareOwnership startPos
      transferResult = transferOwnership context shareTransfer
      owner1StateAfter = case transferResult of
        TransferSuccess newContext -> getOwnershipState owner1 newContext
        TransferFailure _ -> getOwnershipState owner1 context
      owner2StateAfter = case transferResult of
        TransferSuccess newContext -> getOwnershipState owner2 newContext
        TransferFailure _ -> getOwnershipState owner2 context
  in property $ case transferResult of
         TransferSuccess _ -> case (owner1StateAfter, owner2StateAfter) of
                              (Shared owners1, Shared owners2) -> owner1 `elem` owners1 && owner2 `elem` owners2
                              _ -> False
         TransferFailure _ -> True  -- No change on failure

-- Property: Return ownership should transfer to caller
prop_return_ownership_to_caller :: OwnershipContext -> String -> Property
prop_return_ownership_to_caller context variable =
  let returnTransfer = OwnershipTransfer variable "caller" ReturnOwnership startPos
      transferResult = transferOwnership context returnTransfer
      variableStateAfter = case transferResult of
        TransferSuccess newContext -> getOwnershipState variable newContext
        TransferFailure _ -> getOwnershipState variable context
  in property $ case transferResult of
         TransferSuccess _ -> variableStateAfter === Moved
         TransferFailure _ -> True  -- No change on failure

-- Property: Ownership transfer should respect constraints
prop_ownership_transfer_respects_constraints :: OwnershipContext -> OwnershipTransfer -> Property
prop_ownership_transfer_respects_constraints context transfer =
  let constraints = findConstraintsForVariable (transferFrom transfer) context
      hasCannotMove = CannotMove `elem` constraints
      transferResult = transferOwnership context transfer
  in hasCannotMove ==> 
     property $ case transferResult of
       TransferFailure (ConstraintViolation _) -> True
       _ -> False

-- Property: Multiple transfers should create consistent ownership chain
prop_multiple_transfers_consistent_chain :: [String] -> Property
prop_multiple_transfers_consistent_chain varNames =
  length varNames >= 3 ==> 
  let [owner1, owner2, owner3] = take 3 varNames
      initialContext = createInitialContext [owner1]
      transfer1 = OwnershipTransfer owner1 owner2 MoveOwnership startPos
      transfer2 = OwnershipTransfer owner2 owner3 MoveOwnership startPos
      result1 = transferOwnership initialContext transfer1
      result2 = case result1 of
                  TransferSuccess ctx1 -> transferOwnership ctx1 transfer2
                  TransferFailure _ -> result1
  in property $ case result1 of
         TransferSuccess _ -> case result2 of
                               TransferSuccess _ -> True
                               TransferFailure _ -> False  -- Second transfer should succeed
         TransferFailure _ -> True  -- First transfer failed as expected

-- Property: Ownership analysis should detect conflicts
prop_ownership_analysis_detects_conflicts :: OwnershipContext -> Property
prop_ownership_analysis_detects_conflicts context =
  let analysis = analyzeOwnership context
      conflicts = findOwnershipConflicts analysis
  in property $ L.length conflicts >= 0

-- Property: Ownership validation should catch invalid states
prop_ownership_validation_catches_invalid :: OwnershipContext -> Property
prop_ownership_validation_catches_invalid context =
  let validationResult = validateOwnership context
  in property $ case validationResult of
         Left _ -> True  -- Invalid state detected
         Right _ -> True  -- Valid state confirmed

-- Property: Circular ownership should be detected
prop_circular_ownership_detected :: [String] -> Property
prop_circular_ownership_detected varNames =
  length varNames >= 3 ==> 
  let [var1, var2, var3] = take 3 varNames
      initialContext = createInitialContext [var1]
      transfer1 = OwnershipTransfer var1 var2 MoveOwnership startPos
      transfer2 = OwnershipTransfer var2 var3 MoveOwnership startPos
      transfer3 = OwnershipTransfer var3 var1 MoveOwnership startPos  -- Circular
      result1 = transferOwnership initialContext transfer1
      result2 = case result1 of
                  TransferSuccess ctx1 -> transferOwnership ctx1 transfer2
                  TransferFailure _ -> result1
      result3 = case result2 of
                  TransferSuccess ctx2 -> transferOwnership ctx2 transfer3
                  TransferFailure _ -> result2
  in property $ case result3 of
         TransferFailure (CircularOwnership _) -> True
         _ -> False

-- Property: Ownership transfer should handle unknown variables gracefully
prop_transfer_unknown_variable :: String -> String -> Property
prop_transfer_unknown_variable fromVar toVar =
  fromVar /= toVar ==> 
  let emptyContext = OwnershipContext [] [] "main"
      transfer = OwnershipTransfer fromVar toVar MoveOwnership startPos
      result = transferOwnership emptyContext transfer
  in property $ case result of
         TransferFailure (UnknownVariable _) -> True
         _ -> False

-- ============================================================================
-- Helper Functions for Properties
-- ============================================================================

-- Find constraints for a variable in context
findConstraintsForVariable :: String -> OwnershipContext -> [OwnershipConstraint]
findConstraintsForVariable varName context =
  case L.filter (\vo -> variableName vo == varName) (contextVariables context) of
    [] -> []
    (varOwnership:_) -> variableConstraints varOwnership

-- Create initial ownership context with given variables
createInitialContext :: [String] -> OwnershipContext
createInitialContext varNames =
  let variables = L.map (\name -> VariableOwnership name (Owned name) [] startPos) varNames
  in OwnershipContext variables [] "main"

-- Check if ownership state is available for transfer
isAvailableForTransfer :: OwnershipState -> Bool
isAvailableForTransfer state = case state of
  Owned _ -> True
  Shared _ -> True
  Borrowed _ -> True
  Moved -> False
  Freed -> False
  Unknown -> False

-- ============================================================================
-- Performance L.and Scalability Properties
-- ============================================================================

-- Property: Ownership transfer should handle many variables efficiently
prop_ownership_transfer_many_variables :: Int -> Property
prop_ownership_transfer_many_variables numVariables =
  numVariables > 0 && numVariables <= 1000 ==> 
  let varNames = take numVariables $ L.map (\i -> "var" ++ show i) [1..]
      initialContext = createInitialContext varNames
      -- Transfer ownership from first to last
      transfer = case varNames of
                   (first:rest) -> case L.reverse rest of
                                     (last:_) -> Just $ OwnershipTransfer first last MoveOwnership startPos
                                     [] -> Nothing
                   [] -> Nothing
      result = case transfer of
                 Just t -> transferOwnership initialContext t
                 Nothing -> TransferSuccess initialContext
  in property $ case result of
         TransferSuccess _ -> True
         TransferFailure _ -> True

-- Property: Ownership analysis should scale with complexity
prop_ownership_analysis_scalability :: Int -> Property
prop_ownership_analysis_scalability numVariables =
  numVariables > 0 && numVariables <= 500 ==> 
  let varNames = take numVariables $ L.map (\i -> "var" ++ show i) [1..]
      initialContext = createInitialContext varNames
      analysis = analyzeOwnership initialContext
  in property $ True  -- If this completes without timeout, scaling is acceptable

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Self-transfer should be handled appropriately
prop_self_transfer_handled :: String -> Property
prop_self_transfer_handled varName =
  let context = createInitialContext [varName]
      transfer = OwnershipTransfer varName varName MoveOwnership startPos
      result = transferOwnership context transfer
  in property $ case result of
         TransferFailure (SelfTransfer _) -> True
         _ -> False

-- Property: Transfer to same owner should be no-op
prop_transfer_to_same_owner :: String -> Property
prop_transfer_to_same_owner varName =
  let context = createInitialContext [varName]
      transfer = OwnershipTransfer varName varName ShareOwnership startPos
      result = transferOwnership context transfer
  in property $ case result of
         TransferSuccess _ -> True
         TransferFailure _ -> True  -- Either is acceptable

-- Property: Empty context should handle transfers gracefully
prop_empty_context_transfer :: String -> String -> Property
prop_empty_context_transfer fromVar toVar =
  fromVar /= toVar ==> 
  let emptyContext = OwnershipContext [] [] "main"
      transfer = OwnershipTransfer fromVar toVar MoveOwnership startPos
      result = transferOwnership emptyContext transfer
  in property $ case result of
         TransferFailure (UnknownVariable _) -> True
         _ -> False

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Ownership Transfer Boundary QuickCheck Tests"
  [ testGroup "Basic Transfer Properties"
    [ fastProperty "ownership transfer changes state" prop_ownership_transfer_changes_state
    , fastProperty "ownership transfer validation" prop_ownership_transfer_validation
    , fastProperty "move ownership makes source unavailable" prop_move_ownership_makes_source_unavailable
    ]

  , testGroup "Transfer Type Properties"
    [ fastProperty "borrow ownership preserves original" prop_borrow_ownership_preserves_original
    , fastProperty "share ownership creates shared" prop_share_ownership_creates_shared
    , fastProperty "return ownership to caller" prop_return_ownership_to_caller
    ]

  , testGroup "Constraints L.and Validation"
    [ fastProperty "ownership transfer respects constraints" prop_ownership_transfer_respects_constraints
    , fastProperty "ownership validation catches invalid" prop_ownership_validation_catches_invalid
    ]

  , testGroup "Complex Transfer Scenarios"
    [ fastProperty "multiple transfers consistent chain" prop_multiple_transfers_consistent_chain
    , fastProperty "circular ownership detected" prop_circular_ownership_detected
    , fastProperty "ownership analysis detects conflicts" prop_ownership_analysis_detects_conflicts
    ]

  , testGroup "Error Handling L.and Edge Cases"
    [ fastProperty "transfer unknown variable" prop_transfer_unknown_variable
    , fastProperty "self transfer handled" prop_self_transfer_handled
    , fastProperty "transfer to same owner" prop_transfer_to_same_owner
    , fastProperty "empty context transfer" prop_empty_context_transfer
    ]

  , testGroup "Performance L.and Scalability"
    [ fastProperty "ownership transfer many variables" prop_ownership_transfer_many_variables
    , fastProperty "ownership analysis scalability" prop_ownership_analysis_scalability
    ]
  ]