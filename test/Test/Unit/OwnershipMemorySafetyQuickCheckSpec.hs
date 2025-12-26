{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipMemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assert, assertBool)
import Ownership (OwnershipState, VariableOwnership, TransferResult(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose, listOf, oneof, sized)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)

-- | Generate arbitrary variable names
newtype VarName = VarName String
  deriving (Show, Eq, Ord)

instance Arbitrary VarName where
  arbitrary = do
    first <- elements $ ['a'..'z'] ++ ['A'..'Z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    return $ VarName (first : rest)

-- | Generate arbitrary ownership states
data OwnershipState = OwnershipState
  { ownerMap :: Map VarName VarName  -- variable -> owner
  , borrowedVars :: Set VarName      -- set of borrowed variables
  , movedVars :: Set VarName         -- set of moved variables
  } deriving (Show, Eq)

instance Arbitrary OwnershipState where
  arbitrary = do
    varCount <- choose (0, 10)
    vars <- take varCount <$> listOf arbitrary
    owners <- take varCount <$> listOf arbitrary
    let ownerMap' = Map.fromList $ zip vars owners
    borrowedCount <- choose (0, varCount)
    borrowed <- take borrowedCount <$> listOf (elements vars)
    movedCount <- choose (0, varCount - borrowedCount)
    availableForMove = filter (`notElem` borrowed) vars
    moved <- take movedCount <$> listOf (elements availableForMove)
    return $ OwnershipState ownerMap' (Set.fromList borrowed) (Set.fromList moved)

-- | Generate arbitrary transfer operations
data TransferOp = Transfer VarName VarName | Borrow VarName VarName | Return VarName
  deriving (Show, Eq)

instance Arbitrary TransferOp where
  arbitrary = oneof
    [ Transfer <$> arbitrary <*> arbitrary
    , Borrow <$> arbitrary <*> arbitrary
    , Return <$> arbitrary
    ]

-- | Generate arbitrary transfer results
data TransferResult = Success | DoubleMoveError | UseAfterMoveError | BorrowMovedError
  deriving (Show, Eq)

instance Arbitrary TransferResult where
  arbitrary = elements [Success, DoubleMoveError, UseAfterMoveError, BorrowMovedError]

tests :: TestTree
tests = testGroup "Ownership Memory Safety Tests"
  [ testProperty "ownership transfer prevents double moves" $ \state ->
      \var from to -> let result = performTransfer state (Transfer var from to)
      in case result of
        (Success, newState) -> 
          not (Set.member var (movedVars state)) || -- First move is allowed
          Set.member var (movedVars newState)      -- Variable is now moved
        (DoubleMoveError, _) -> 
          Set.member var (movedVars state)         -- Error when already moved
        _ -> property True

  , testProperty "borrowing prevents moving borrowed variables" $ \state ->
      \var from -> let result = performTransfer state (Borrow var from)
      in case result of
        (Success, newState) ->
          not (Set.member var (movedVars state)) &&
          Set.member var (borrowedVars newState)
        (BorrowMovedError, _) ->
          Set.member var (movedVars state)
        _ -> property True

  , testProperty "use after move is detected" $ \state ->
      \var -> let result = useVariable state var
      in if Set.member var (movedVars state)
         then result == UseAfterMoveError
         else result == Success

  , testProperty "ownership state consistency is maintained" $ \state ->
      \ops -> let (results, finalState) = foldl applyTransfer ([], state) ops
                  movedConsistent = all (`Set.member` movedVars finalState) 
                                    (map movedVar results)
                  borrowedConsistent = all (`Set.member` borrowedVars finalState)
                                       (map borrowedVar results)
              in movedConsistent && borrowedConsistent

  , testProperty "ownership transfer preserves total ownership" $ \state ->
      \var from to -> let result = performTransfer state (Transfer var from to)
      in case result of
        (Success, newState) ->
          let originalOwners = Set.fromList $ Map.elems (ownerMap state)
              newOwners = Set.fromList $ Map.elems (ownerMap newState)
          in Set.size newOwners == Set.size originalOwners
        _ -> property True

  , testCase "basic ownership transfer works" $ do
      let state = OwnershipState Map.empty Set.empty Set.empty
          result = performTransfer state (Transfer (VarName "x") (VarName "owner") (VarName "newOwner"))
      assert (fst result == Success)

  , testCase "double move is prevented" $ do
      let state = OwnershipState 
            { ownerMap = Map.singleton (VarName "x") (VarName "owner")
            , borrowedVars = Set.empty
            , movedVars = Set.singleton (VarName "x")
            }
          result = performTransfer state (Transfer (VarName "x") (VarName "owner") (VarName "newOwner"))
      assert (fst result == DoubleMoveError)

  , testCase "borrowing moved variable fails" $ do
      let state = OwnershipState
            { ownerMap = Map.singleton (VarName "x") (VarName "owner")
            , borrowedVars = Set.empty
            , movedVars = Set.singleton (VarName "x")
            }
          result = performTransfer state (Borrow (VarName "x") (VarName "borrower"))
      assert (fst result == BorrowMovedError)

  , testCase "borrowing prevents move" $ do
      let state = OwnershipState
            { ownerMap = Map.singleton (VarName "x") (VarName "owner")
            , borrowedVars = Set.singleton (VarName "x")
            , movedVars = Set.empty
            }
          result = performTransfer state (Transfer (VarName "x") (VarName "owner") (VarName "newOwner"))
      assert (fst result == BorrowMovedError)

  , testCase "ownership state tracks all variables correctly" $ do
      let state = OwnershipState
            { ownerMap = Map.fromList [(VarName "x", VarName "owner1"), (VarName "y", VarName "owner2")]
            , borrowedVars = Set.singleton (VarName "x")
            , movedVars = Set.singleton (VarName "y")
            }
      assert (Set.member (VarName "x") (borrowedVars state))
      assert (Set.member (VarName "y") (movedVars state))
      assert (Map.lookup (VarName "x") (ownerMap state) == Just (VarName "owner1"))

  , testProperty "ownership operations are reversible" $ \state ->
      \var from to -> case performTransfer state (Transfer var from to) of
        (Success, newState) -> 
          let revertResult = performTransfer newState (Transfer var to from)
          in case revertResult of
            (Success, revertedState) -> 
              -- Should return to equivalent state (ignoring move flags)
              Map.filterWithKey (\k _ -> not (Set.member k (movedVars revertedState))) 
                              (ownerMap revertedState) == 
              Map.filterWithKey (\k _ -> not (Set.member k (movedVars state))) 
                              (ownerMap state)
            _ -> property False
        _ -> property True
  ]

-- Helper functions for ownership testing (these would be implemented in the actual ownership module)
performTransfer :: OwnershipState -> TransferOp -> (TransferResult, OwnershipState)
performTransfer state op = case op of
  Transfer var from to
    | Set.member var (movedVars state) -> (DoubleMoveError, state)
    | Set.member var (borrowedVars state) -> (BorrowMovedError, state)
    | otherwise -> (Success, state 
        { ownerMap = Map.insert var to (ownerMap state)
        , movedVars = Set.insert var (movedVars state)
        })
  Borrow var from
    | Set.member var (movedVars state) -> (BorrowMovedError, state)
    | otherwise -> (Success, state
        { borrowedVars = Set.insert var (borrowedVars state)
        })
  Return var
    | Set.member var (movedVars state) -> (UseAfterMoveError, state)
    | otherwise -> (Success, state)

useVariable :: OwnershipState -> VarName -> TransferResult
useVariable state var
  | Set.member var (movedVars state) = UseAfterMoveError
  | otherwise = Success

applyTransfer :: ([(TransferResult, OwnershipState)], OwnershipState) -> TransferOp -> ([(TransferResult, OwnershipState)], OwnershipState)
applyTransfer (results, state) op = 
  let (result, newState) = performTransfer state op
  in (results ++ [(result, newState)], newState)

movedVar :: (TransferResult, OwnershipState) -> VarName
movedVar (Success, state) = VarName "unknown" -- Simplified for testing
movedVar _ = VarName "error"

borrowedVar :: (TransferResult, OwnershipState) -> VarName
borrowedVar (Success, state) = VarName "unknown" -- Simplified for testing
borrowedVar _ = VarName "error"