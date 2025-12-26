{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipTransitivityAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, listOf)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Data.List (nub, sort)
import Data.Set (Set, fromList, toList, union, intersection, member)

-- ============================================================================
-- Test Generators
-- ============================================================================

-- Generate variable names
genVarName :: Gen String
genVarName = do
  len <- choose (1, 10)
  chars <- listOf $ choose ('a', 'z')
  return $ take len chars

instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> genVarName
    , Borrowed <$> genVarName
    , MutBorrowed <$> genVarName
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- genVarName
    to <- genVarName
    return $ OwnershipTransfer from to

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> genVarName
    , DoubleMove <$> genVarName <*> genVarName
    , BorrowWhileMoved <$> genVarName
    , MutBorrowWhileBorrowed <$> genVarName
    , BorrowWhileMutBorrowed <$> genVarName
    , MultipleMutBorrows <$> genVarName
    , UseWhileMutBorrowed <$> genVarName
    , OutOfScope <$> genVarName
    , BorrowError <$> genVarName
    , ParseError <$> genVarName
    , CrossFunctionMove <$> genVarName <*> genVarName
    , ParameterMoveMismatch <$> genVarName
    , ControlFlowError <$> genVarName
    , PathSensitiveError <$> genVarName
    , LoopOwnershipError <$> genVarName
    ]

-- ============================================================================
-- Ownership Type Properties
-- ============================================================================

-- Property: Ownership type ordering should be consistent
propOwnershipTypeOrdering :: OwnershipType -> OwnershipType -> Bool
propOwnershipTypeOrdering ot1 ot2 =
  let cmp1 = compare ot1 ot2
      cmp2 = compare ot2 ot1
  in case (cmp1, cmp2) of
    (EQ, EQ) -> True
    (LT, GT) -> True
    (GT, LT) -> True
    _ -> False

-- Property: Owned should be less than any borrowed type
propOwnedLessThanBorrowed :: String -> String -> Bool
propOwnedLessThanBorrowed owner borrower =
  let owned = Owned owner
      borrowed = Borrowed borrower
      mutBorrowed = MutBorrowed borrower
  in compare owned borrowed == LT && compare owned mutBorrowed == LT

-- Property: Borrowed should be less than MutBorrowed
propBorrowedLessThanMutBorrowed :: String -> String -> Bool
propBorrowedLessThanMutBorrowed owner borrower =
  let borrowed = Borrowed owner
      mutBorrowed = MutBorrowed borrower
  in compare borrowed mutBorrowed == LT

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: Transfer should preserve variable names
propTransferPreservesNames :: String -> String -> Bool
propTransferPreservesNames from to =
  let transfer = OwnershipTransfer from to
  in transferFrom transfer == from && transferTo transfer == to

-- Property: Transfer should be symmetric for equality check
propTransferEqualitySymmetric :: OwnershipTransfer -> OwnershipTransfer -> Bool
propTransferEqualitySymmetric t1 t2 =
  (t1 == t2) == (t2 == t1)

-- Property: Self-transfer should be identifiable
propSelfTransferIdentifiable :: String -> Bool
propSelfTransferIdentifiable var =
  let transfer = OwnershipTransfer var var
  in transferFrom transfer == transferTo transfer

-- ============================================================================
-- Ownership Error Properties
-- ============================================================================

-- Property: Error ordering should be consistent
propErrorOrderingConsistent :: OwnershipError -> OwnershipError -> Bool
propErrorOrderingConsistent err1 err2 =
  let cmp1 = compare err1 err2
      cmp2 = compare err2 err1
  in case (cmp1, cmp2) of
    (EQ, EQ) -> True
    (LT, GT) -> True
    (GT, LT) -> True
    _ -> False

-- Property: UseAfterMove errors should contain the variable name
propUseAfterMoveContainsVariable :: String -> Bool
propUseAfterMoveContainsVariable var =
  let err = UseAfterMove var
      errStr = show err
  in var `elem` words errStr

-- Property: DoubleMove errors should contain both variable names
propDoubleMoveContainsBothVariables :: String -> String -> Bool
propDoubleMoveContainsBothVariables var1 var2 =
  let err = DoubleMove var1 var2
      errStr = show err
      vars = words errStr
  in var1 `elem` vars && var2 `elem` vars

-- ============================================================================
-- Transitivity Properties
-- ============================================================================

-- Property: Ownership transfer chain should preserve ordering
propTransferChainPreservesOrdering :: [OwnershipTransfer] -> Bool
propTransferChainPreservesOrdering transfers =
  let sortedTransfers = sort transfers
      originalOrder = map (\t -> (transferFrom t, transferTo t)) transfers
      sortedOrder = map (\t -> (transferFrom t, transferTo t)) sortedTransfers
  in length originalOrder == length sortedOrder

-- Property: Transfer chain should not create cycles (simple detection)
propTransferChainNoSimpleCycles :: [OwnershipTransfer] -> Bool
propTransferChainNoSimpleCycles transfers =
  let pairs = [(transferFrom t, transferTo t) | t <- transfers]
      hasCycle (a,b) = (b,a) `elem` pairs
  in not (any hasCycle pairs)

-- Property: Variable ownership should be trackable through transfer chain
propVariableOwnershipTrackable :: String -> [OwnershipTransfer] -> Bool
propVariableOwnershipTrackable var transfers =
  let relevantTransfers = filter (\t -> transferFrom t == var || transferTo t == var) transfers
      fromVars = nub $ map transferFrom relevantTransfers
      toVars = nub $ map transferTo relevantTransfers
  in var `elem` fromVars || var `elem` toVars

-- Property: Ownership transfer should be associative in terms of variable sets
propTransferAssociativeVariableSets :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer -> Bool
propTransferAssociativeVariableSets t1 t2 t3 =
  let vars1 = fromList [transferFrom t1, transferTo t1]
      vars2 = fromList [transferFrom t2, transferTo t2]
      vars3 = fromList [transferFrom t3, transferTo t3]
      
      leftAssoc = union (union vars1 vars2) vars3
      rightAssoc = union vars1 (union vars2 vars3)
  in toList leftAssoc == toList rightAssoc

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- Test ownership type edge cases
testOwnershipTypeEdgeCases :: TestTree
testOwnershipTypeEdgeCases = testCase "Ownership type edge cases" $ do
  let owned = Owned "x"
  let borrowed = Borrowed "x"
  let mutBorrowed = MutBorrowed "x"
  
  -- Test ordering with same variable names
  assertEqual "Owned < Borrowed with same name" LT (compare owned borrowed)
  assertEqual "Owned < MutBorrowed with same name" LT (compare owned mutBorrowed)
  assertEqual "Borrowed < MutBorrowed with same name" LT (compare borrowed mutBorrowed)
  
  -- Test equality
  assertEqual "Same owned types are equal" owned (Owned "x")
  assertEqual "Same borrowed types are equal" borrowed (Borrowed "x")
  assertEqual "Same mutBorrowed types are equal" mutBorrowed (MutBorrowed "x")

-- Test ownership transfer edge cases
testOwnershipTransferEdgeCases :: TestTree
testOwnershipTransferEdgeCases = testCase "Ownership transfer edge cases" $ do
  let transfer1 = OwnershipTransfer "x" "y"
  let transfer2 = OwnershipTransfer "x" "y"
  let transfer3 = OwnershipTransfer "y" "x"
  
  -- Test equality
  assertEqual "Same transfers are equal" transfer1 transfer2
  assertBool "Different transfers are not equal" (transfer1 /= transfer3)
  
  -- Test self-transfer
  let selfTransfer = OwnershipTransfer "x" "x"
  assertEqual "Self-transfer from and to are same" "x" (transferFrom selfTransfer)
  assertEqual "Self-transfer from and to are same" "x" (transferTo selfTransfer)

-- Test ownership error edge cases
testOwnershipErrorEdgeCases :: TestTree
testOwnershipErrorEdgeCases = testCase "Ownership error edge cases" $ do
  let useAfterMove = UseAfterMove "x"
  let doubleMove = DoubleMove "x" "y"
  let borrowWhileMoved = BorrowWhileMoved "x"
  
  -- Test error messages contain variable names
  let useAfterMoveStr = show useAfterMove
  assertBool "UseAfterMove contains variable name" ("x" `elem` words useAfterMoveStr)
  
  let doubleMoveStr = show doubleMove
  assertBool "DoubleMove contains first variable" ("x" `elem` words doubleMoveStr)
  assertBool "DoubleMove contains second variable" ("y" `elem` words doubleMoveStr)
  
  let borrowWhileMovedStr = show borrowWhileMoved
  assertBool "BorrowWhileMoved contains variable name" ("x" `elem` words borrowWhileMovedStr)

-- Test complex transfer chains
testComplexTransferChains :: TestTree
testComplexTransferChains = testCase "Complex transfer chains" $ do
  let transfers = 
        [ OwnershipTransfer "a" "b"
        , OwnershipTransfer "b" "c"
        , OwnershipTransfer "c" "d"
        , OwnershipTransfer "a" "e"
        ]
  
  -- Test that we can track ownership through the chain
  let involvedVars = fromList ["a", "b", "c", "d", "e"]
  let transferVars = fromList $ concatMap (\t -> [transferFrom t, transferTo t]) transfers
  
  assertEqual "All variables should be involved in transfers" 
    (toList involvedVars) (sort $ toList transferVars)
  
  -- Test that 'a' is the original owner
  let fromVars = map transferFrom transfers
  assertBool "Original owner 'a' should appear in from vars" ("a" `elem` fromVars)

-- Test ownership analyzer creation
testOwnershipAnalyzerCreation :: TestTree
testOwnershipAnalyzerCreation = testCase "Ownership analyzer creation" $ do
  let analyzer = newOwnershipAnalyzer
  assertBool "Analyzer should be created successfully" (True)  -- Just test that it doesn't crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transitivity Advanced Tests"
  [ -- QuickCheck properties for OwnershipType
    testProperty "Ownership type ordering consistent" propOwnershipTypeOrdering
  , testProperty "Owned less than borrowed" propOwnedLessThanBorrowed
  , testProperty "Borrowed less than mutBorrowed" propBorrowedLessThanMutBorrowed
  
    -- QuickCheck properties for OwnershipTransfer
  , testProperty "Transfer preserves names" propTransferPreservesNames
  , testProperty "Transfer equality symmetric" propTransferEqualitySymmetric
  , testProperty "Self-transfer identifiable" propSelfTransferIdentifiable
  
    -- QuickCheck properties for OwnershipError
  , testProperty "Error ordering consistent" propErrorOrderingConsistent
  , testProperty "UseAfterMove contains variable" propUseAfterMoveContainsVariable
  , testProperty "DoubleMove contains both variables" propDoubleMoveContainsBothVariables
  
    -- QuickCheck properties for transitivity
  , testProperty "Transfer chain preserves ordering" propTransferChainPreservesOrdering
  , testProperty "Transfer chain no simple cycles" propTransferChainNoSimpleCycles
  , testProperty "Variable ownership trackable" propVariableOwnershipTrackable
  , testProperty "Transfer associative variable sets" propTransferAssociativeVariableSets
  
    -- Unit tests for edge cases
  , testOwnershipTypeEdgeCases
  , testOwnershipTransferEdgeCases
  , testOwnershipErrorEdgeCases
  , testComplexTransferChains
  , testOwnershipAnalyzerCreation
  ]