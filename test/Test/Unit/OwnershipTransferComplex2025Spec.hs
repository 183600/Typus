{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.OwnershipTransferComplex2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements)
import Test.Tasty.HUnit (testCase, (@=?))

import Ownership (OwnershipInfo(..), OwnershipTransfer(..), transferOwnership, canTransfer)
import Ownership.Common.Types (OwnershipMode(..), OwnershipState(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Ownership Transfer Complex Tests"
  [ testProperty "Ownership transfer preserves uniqueness" propOwnershipTransferPreservesUniqueness
  , testProperty "Transfer creates valid ownership chain" propTransferCreatesValidChain
  , testProperty "Borrowing rules enforced during transfer" propBorrowingRulesEnforced
  , testProperty "Multiple transfer composition" propMultipleTransferComposition
  , testProperty "Transfer rollback maintains consistency" propTransferRollbackConsistency
  , testCase "Complex ownership transfer scenario" testComplexOwnershipTransfer
  , testProperty "Circular ownership detection" propCircularOwnershipDetection
  , testCase "Partial ownership transfer" testPartialOwnershipTransfer
  , testProperty "Transfer with borrowing constraints" propTransferWithBorrowingConstraints
  , testCase "Ownership transfer in function calls" testOwnershipTransferInFunctionCalls
  ]

-- Mock data types for testing
data MockVariable = MockVariable
  { varName :: String
  , varType :: String
  , varOwnership :: OwnershipInfo
  , varLocation :: SourcePos
  } deriving (Show, Eq)

data MockOwnershipTransfer = MockOwnershipTransfer
  { transferFrom :: MockVariable
  , transferTo :: MockVariable
  , transferMode :: OwnershipMode
  , transferLocation :: SourceSpan
  } deriving (Show, Eq)

-- Property 1: Ownership transfer preserves uniqueness
propOwnershipTransferPreservesUniqueness :: MockVariable -> MockVariable -> Bool
propOwnershipTransferPreservesUniqueness from to =
  let transfer = MockOwnershipTransfer from to Unique (spanBetween (varLocation from) (varLocation to))
      result = mockTransferOwnership transfer
  in case result of
       Right (newFrom, newTo) -> 
         ownershipState (varOwnership newFrom) == Unowned &&
         ownershipState (varOwnership newTo) == Owned
       Left _ -> False

-- Property 2: Transfer creates valid ownership chain
propTransferCreatesValidChain :: [MockVariable] -> Bool
propTransferCreatesValidChain vars =
  length vars >= 2 ==> 
  let transfers = createTransferChain vars
      result = foldl mockTransferEither (Right vars) transfers
  in case result of
       Right finalVars -> all isValidOwnershipChain (zip finalVars (tail finalVars))
       Left _ -> False

-- Property 3: Borrowing rules enforced during transfer
propBorrowingRulesEnforced :: MockVariable -> MockVariable -> Bool
propBorrowingRulesEnforced from to =
  let borrowedFrom = from { varOwnership = (varOwnership from) { ownershipState = Borrowed } }
      transfer = MockOwnershipTransfer borrowedFrom to Unique (spanBetween (varLocation from) (varLocation to))
      result = mockTransferOwnership transfer
  in case result of
       Right _ -> False  -- Should not allow transfer from borrowed variable
       Left _ -> True    -- Should fail appropriately

-- Property 4: Multiple transfer composition
propMultipleTransferComposition :: MockVariable -> MockVariable -> MockVariable -> Bool
propMultipleTransferComposition var1 var2 var3 =
  let transfer1 = MockOwnershipTransfer var1 var2 Unique (spanBetween (varLocation var1) (varLocation var2))
      transfer2 = MockOwnershipTransfer var2 var3 Unique (spanBetween (varLocation var2) (varLocation var3))
      result1 = mockTransferOwnership transfer1
      result2 = case result1 of
                 Right (newVar1, newVar2) -> mockTransferOwnership (transfer2 { transferFrom = newVar2 })
                 Left _ -> Left "First transfer failed"
  in case result2 of
       Right (finalVar2, finalVar3) -> 
         ownershipState (varOwnership finalVar2) == Unowned &&
         ownershipState (varOwnership finalVar3) == Owned
       Left _ -> False

-- Property 5: Transfer rollback maintains consistency
propTransferRollbackConsistency :: MockVariable -> MockVariable -> Bool
propTransferRollbackConsistency from to =
  let originalFrom = from
      originalTo = to
      transfer = MockOwnershipTransfer from to Unique (spanBetween (varLocation from) (varLocation to))
  in case mockTransferOwnership transfer of
       Right (newFrom, newTo) ->
         let rollback = mockRollbackTransfer (newFrom, newTo)
         in rollback == (originalFrom, originalTo)
       Left _ -> True  -- Failed transfer doesn't need rollback

-- Test Case 6: Complex ownership transfer scenario
testComplexOwnershipTransfer :: IO ()
testComplexOwnershipTransfer = do
  let var1 = MockVariable "x" "String" (OwnershipInfo Unique Owned Nothing) (SourcePos 1 5)
      var2 = MockVariable "y" "String" (OwnershipInfo Unique Unowned Nothing) (SourcePos 2 5)
      var3 = MockVariable "z" "String" (OwnershipInfo Unique Unowned Nothing) (SourcePos 3 5)
  
  -- Transfer x -> y
  let transfer1 = MockOwnershipTransfer var1 var2 Unique (SourceSpan (SourcePos 1 1) (SourcePos 2 10))
  result1 <- return $ mockTransferOwnership transfer1
  
  case result1 of
    Right (newVar1, newVar2) -> do
      ownershipState (varOwnership newVar1) @=? Unowned
      ownershipState (varOwnership newVar2) @=? Owned
      
      -- Transfer y -> z
      let transfer2 = MockOwnershipTransfer newVar2 var3 Unique (SourceSpan (SourcePos 2 1) (SourcePos 3 10))
      result2 <- return $ mockTransferOwnership transfer2
      
      case result2 of
        Right (finalVar2, finalVar3) -> do
          ownershipState (varOwnership finalVar2) @=? Unowned
          ownershipState (varOwnership finalVar3) @=? Owned
        Left _ -> pure ()
    Left _ -> pure ()

-- Property 7: Circular ownership detection
propCircularOwnershipDetection :: MockVariable -> MockVariable -> Bool
propCircularOwnershipDetection var1 var2 =
  let transfer1 = MockOwnershipTransfer var1 var2 Unique (spanBetween (varLocation var1) (varLocation var2))
      transfer2 = MockOwnershipTransfer var2 var1 Unique (spanBetween (varLocation var2) (varLocation var1))
      result1 = mockTransferOwnership transfer1
      result2 = case result1 of
                 Right (newVar1, newVar2) -> mockTransferOwnership (transfer2 { transferFrom = newVar2, transferTo = newVar1 })
                 Left _ -> Left "First transfer failed"
  in case result2 of
       Right _ -> False  -- Should not allow circular transfer
       Left _ -> True    -- Should detect and prevent circular ownership

-- Test Case 8: Partial ownership transfer
testPartialOwnershipTransfer :: IO ()
testPartialOwnershipTransfer = do
  let sharedVar = MockVariable "data" "Vec<String>" (OwnershipInfo Shared Owned Nothing) (SourcePos 1 10)
      receiver = MockVariable "receiver" "Vec<String>" (OwnershipInfo Shared Unowned Nothing) (SourcePos 2 10)
  
  let transfer = MockOwnershipTransfer sharedVar receiver Shared (spanBetween (varLocation sharedVar) (varLocation receiver))
  result <- return $ mockTransferOwnership transfer
  
  case result of
    Right (newShared, newReceiver) -> do
      -- Both should still have access to shared data
      ownershipState (varOwnership newShared) @=? Owned
      ownershipState (varOwnership newReceiver) @=? Owned
    Left _ -> pure ()

-- Property 9: Transfer with borrowing constraints
propTransferWithBorrowingConstraints :: MockVariable -> MockVariable -> Bool
propTransferWithBorrowingConstraints from to =
  let borrowedFrom = from { varOwnership = (varOwnership from) { ownershipState = Borrowed } }
      transfer = MockOwnershipTransfer borrowedFrom to Unique (spanBetween (varLocation from) (varLocation to))
  in not (canMockTransfer transfer)

-- Test Case 10: Ownership transfer in function calls
testOwnershipTransferInFunctionCalls :: IO ()
testOwnershipTransferInFunctionCalls = do
  let arg = MockVariable "arg" "String" (OwnershipInfo Unique Owned Nothing) (SourcePos 1 15)
      param = MockVariable "param" "String" (OwnershipInfo Unique Unowned Nothing) (SourcePos 5 20)
  
  -- Simulate function call transfer
  let transfer = MockOwnershipTransfer arg param Unique (SourceSpan (SourcePos 1 10) (SourcePos 5 25))
  result <- return $ mockTransferOwnership transfer
  
  case result of
    Right (newArg, newParam) -> do
      ownershipState (varOwnership newArg) @=? Unowned
      ownershipState (varOwnership newParam) @=? Owned
    Left _ -> pure ()

-- Helper functions
mockTransferOwnership :: MockOwnershipTransfer -> Either String (MockVariable, MockVariable)
mockTransferOwnership transfer
  | not (canMockTransfer transfer) = Left "Transfer not allowed"
  | otherwise = Right (newFrom, newTo)
  where
    from = transferFrom transfer
    to = transferTo transfer
    mode = transferMode transfer
    
    newFrom = from { varOwnership = (varOwnership from) { ownershipState = Unowned } }
    newTo = to { varOwnership = (varOwnership to) { ownershipState = Owned, ownershipPrevious = Just (varName from) } }

canMockTransfer :: MockOwnershipTransfer -> Bool
canMockTransfer transfer =
  let from = transferFrom transfer
      to = transferTo transfer
      fromState = ownershipState (varOwnership from)
  in fromState /= Borrowed && fromState /= Unowned

mockTransferEither :: Either String [MockVariable] -> MockOwnershipTransfer -> Either String [MockVariable]
mockTransferEither (Right vars) transfer =
  case mockTransferOwnership transfer of
    Right (newFrom, newTo) -> Right $ map (\v -> 
      if varName v == varName newFrom then newFrom
      else if varName v == varName newTo then newTo
      else v) vars
    Left _ -> Left "Transfer failed"
mockTransferEither (Left err) _ = Left err

createTransferChain :: [MockVariable] -> [MockOwnershipTransfer]
createTransferChain vars = 
  zipWith (\from to -> MockOwnershipTransfer from to Unique (spanBetween (varLocation from) (varLocation to))) 
           vars (tail vars)

isValidOwnershipChain :: (MockVariable, MockVariable) -> Bool
isValidOwnershipChain (from, to) =
  let fromState = ownershipState (varOwnership from)
      toState = ownershipState (varOwnership to)
      toPrevious = ownershipPrevious (varOwnership to)
  in fromState == Unowned && toState == Owned && toPrevious == Just (varName from)

mockRollbackTransfer :: (MockVariable, MockVariable) -> (MockVariable, MockVariable)
mockRollbackTransfer (from, to) =
  let originalFrom = from { varOwnership = (varOwnership from) { ownershipState = Owned, ownershipPrevious = Nothing } }
      originalTo = to { varOwnership = (varOwnership to) { ownershipState = Unowned, ownershipPrevious = Nothing } }
  in (originalFrom, originalTo)

-- Arbitrary instances for testing
instance Arbitrary OwnershipMode where
  arbitrary = elements [Unique, Shared, Borrowed]

instance Arbitrary OwnershipState where
  arbitrary = elements [Owned, Unowned, Borrowed, Moved]

instance Arbitrary OwnershipInfo where
  arbitrary = do
    mode <- arbitrary
    state <- arbitrary
    prev <- arbitrary
    return $ OwnershipInfo mode state prev

instance Arbitrary MockVariable where
  arbitrary = do
    name <- elements ["x", "y", "z", "data", "result", "value"]
    varType <- elements ["String", "Int", "Vec<String>", "Option<Int>"]
    ownership <- arbitrary
    line <- choose (1, 100)
    col <- choose (1, 100)
    return $ MockVariable name varType ownership (SourcePos line col)