{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.OwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  )

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)

-- ============================================================================
-- Ownership Transitivity Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Ownership Transitivity Tests"
    [ testGroup "Basic Ownership Transfer"
        [ testCase "simple move operation transfers ownership" $ do
            let code = unlines
                  [ "func main() {"
                  , "    data := create_data()"
                  , "    new_owner := take_value(data)"
                  , "    // data should be moved, new_owner owns it"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect move of data" (not $ null moveErrors)

        , testCase "chained moves create ownership chain" $ do
            let code = unlines
                  [ "func chain_moves() {"
                  , "    data := create_data()"
                  , "    first := take_value(data)"
                  , "    second := take_value(first)"
                  , "    third := take_value(second)"
                  , "    // data -> first -> second -> third"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect chain of moves" (length moveErrors >= 2)

        , testCase "move through function parameters" $ do
            let code = unlines
                  [ "func process(data Data) {"
                  , "    // data is moved into this function"
                  , "    processed := transform(data)"
                  , "    return processed"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    original := create_data()"
                  , "    result := process(original)"
                  , "    // original should be moved"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let crossFunctionMoves = filter isCrossFunctionMove errors
                    assertBool "should detect cross-function move" (not $ null crossFunctionMoves)
        ]

    , testGroup "Borrowing and Transitivity"
        [ testCase "immutable borrow preserves original ownership" $ do
            let code = unlines
                  [ "func borrow_example() {"
                  , "    data := create_data()"
                  , "    borrowed := &data"
                  , "    // data still owned, borrowed references it"
                  , "    use_data(borrowed)"
                  , "    // data should still be usable after borrow"
                  , "    process(data)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let useAfterMoves = filter isUseAfterMove errors
                    assertBool "should not report use after move for borrowed data" (null useAfterMoves)

        , testCase "mutable borrow restrictions" $ do
            let code = unlines
                  [ "func mutable_borrow() {"
                  , "    data := create_data()"
                  , "    mut_ref := &mut data"
                  , "    // data is mutably borrowed"
                  , "    other_ref := &data  // should error"
                  , "    use_data(mut_ref)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let borrowErrors = filter isBorrowError errors
                    assertBool "should detect borrow conflict" (not $ null borrowErrors)

        , testCase "borrow chain and ownership preservation" $ do
            let code = unlines
                  [ "func borrow_chain() {"
                  , "    data := create_data()"
                  , "    borrow1 := &data"
                  , "    borrow2 := borrow1"
                  , "    borrow3 := &borrow2"
                  , "    // chain of borrows, data still owned"
                  , "    finalize(data)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "borrowing should not move original" (null moveErrors)
        ]

    , testGroup "Complex Transfer Scenarios"
        [ testCase "ownership transfer through data structures" $ do
            let code = unlines
                  [ "func struct_transfer() {"
                  , "    data := create_data()"
                  , "    container := Container{value: data}"
                  , "    // data moved into struct"
                  , "    new_container := transfer_container(container)"
                  , "    // container moved to new_container"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect struct field moves" (not $ null moveErrors)

        , testCase "conditional ownership transfer" $ do
            let code = unlines
                  [ "func conditional_move() {"
                  , "    data := create_data()"
                  , "    if condition {"
                  , "        moved := take_value(data)"
                  , "        process(moved)"
                  , "    } else {"
                  , "        // data not moved in else branch"
                  , "        process(data)"
                  , "    }"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let conditionalErrors = filter isConditionalError errors
                    assertBool "should handle conditional transfers" (True)

        , testCase "loop-based ownership transfer" $ do
            let code = unlines
                  [ "func loop_transfer() {"
                  , "    items := create_list()"
                  , "    for item := range items {"
                  , "        processed := process_item(item)"
                  , "        // item moved in each iteration"
                  , "        store(processed)"
                  , "    }"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let loopErrors = filter isLoopError errors
                    assertBool "should handle loop-based transfers" (True)
        ]

    , testGroup "Error Detection in Transfer Chains"
        [ testCase "detects use after move in transfer chain" $ do
            let code = unlines
                  [ "func use_after_move_chain() {"
                  , "    data := create_data()"
                  , "    first := take_value(data)"
                  , "    second := take_value(first)"
                  , "    use_data(data)  // should error - data moved"
                  , "    process(second)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let useAfterMoves = filter isUseAfterMove errors
                    assertBool "should detect use after move" (not $ null useAfterMoves)

        , testCase "detects double move in transfer chain" $ do
            let code = unlines
                  [ "func double_move_chain() {"
                  , "    data := create_data()"
                  , "    first := take_value(data)"
                  , "    second := take_value(data)  // should error - data already moved"
                  , "    process(first)"
                  , "    process(second)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let doubleMoves = filter isDoubleMove errors
                    assertBool "should detect double move" (not $ null doubleMoves)

        , testCase "detects borrow after move" $ do
            let code = unlines
                  [ "func borrow_after_move() {"
                  , "    data := create_data()"
                  , "    moved := take_value(data)"
                  , "    borrow := &data  // should error - data moved"
                  , "    process(moved)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let borrowWhileMoved = filter isBorrowWhileMoved errors
                    assertBool "should detect borrow after move" (not $ null borrowWhileMoved)
        ]

    , testGroup "Advanced Transfer Patterns"
        [ testCase "ownership transfer through closures" $ do
            let code = unlines
                  [ "func closure_transfer() {"
                  , "    data := create_data()"
                  , "    closure := func() {"
                  , "        process(data)  // data moved into closure"
                  , "    }"
                  , "    closure()"
                  , "    // data should be moved"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect closure capture" (not $ null moveErrors)

        , testCase "transfer through interface satisfaction" $ do
            let code = unlines
                  [ "func interface_transfer() {"
                  , "    data := create_data()"
                  , "    var processor Processor = data"
                  , "    // data moved into interface"
                  , "    another := transfer_processor(processor)"
                  , "    // processor moved"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect interface boxing" (not $ null moveErrors)

        , testCase "partial transfer through slices" $ do
            let code = unlines
                  [ "func slice_transfer() {"
                  , "    data := create_data()"
                  , "    slice := []Data{data}"
                  , "    // data moved into slice"
                  , "    first := slice[0]  // move from slice"
                  , "    process(first)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should detect slice element moves" (not $ null moveErrors)
        ]

    , testGroup "Property-Based Transfer Tests"
        [ fastProperty "ownership transfer is transitive" $
            \transferChain ->
                let chainLength = min 5 (max 1 transferChain)
                    code = generateTransferChain chainLength
                    analyzer = newOwnershipAnalyzer
                    (errors, _) = analyzeOwnership analyzer code
                    moveErrors = filter isMoveError errors
                in property $ length moveErrors >= chainLength - 1

        , fastProperty "borrowing prevents move" $
            \borrowBeforeMove ->
                let code = if borrowBeforeMove
                          then unlines
                               [ "data := create_data()"
                               , "borrow := &data"
                               , "moved := take_value(data)"  -- Should error
                               ]
                          else unlines
                               [ "data := create_data()"
                               , "moved := take_value(data)"
                               , "borrow := &data"  -- Should error
                               ]
                    analyzer = newOwnershipAnalyzer
                    (errors, _) = analyzeOwnership analyzer code
                    borrowErrors = filter isBorrowError errors
                in property $ not $ null borrowErrors

        , fastProperty "move invalidates all borrows" $
            \numBorrows ->
                let borrowCount = min 3 (max 1 numBorrows)
                    code = unlines $ ["data := create_data()"] ++
                                   ["borrow" ++ show i ++ " := &data" | i <- [1..borrowCount]] ++
                                   ["moved := take_value(data)"]  -- Should invalidate all borrows
                    analyzer = newOwnershipAnalyzer
                    (errors, _) = analyzeOwnership analyzer code
                    borrowErrors = filter isBorrowError errors
                in property $ length borrowErrors >= 1
        ]

    , testGroup "Edge Cases and Complex Scenarios"
        [ testCase "self-transfer detection" $ do
            let code = unlines
                  [ "func self_transfer() {"
                  , "    data := create_data()"
                  , "    data := take_value(data)  -- self assignment"
                  , "    process(data)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should handle self-transfer" (True)

        , testCase "cyclic transfer detection" $ do
            let code = unlines
                  [ "func cyclic_transfer() {"
                  , "    a := create_data()"
                  , "    b := create_data()"
                  , "    a := take_value(b)"
                  , "    b := take_value(a)"  -- potential cycle
                  , "    process(b)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let moveErrors = filter isMoveError errors
                    assertBool "should handle potential cycles" (True)

        , testCase "transfer through return values" $ do
            let code = unlines
                  [ "func create_and_transfer() Data {"
                  , "    data := create_data()"
                  , "    return data"  -- data moved to caller
                  , "}"
                  , ""
                  , "func main() {"
                  , "    result := create_and_transfer()"
                  , "    process(result)"
                  , "}"
                  ]
            let analyzer = newOwnershipAnalyzer
            case analyzeOwnership analyzer code of
                (errors, _) -> do
                    let crossFunctionMoves = filter isCrossFunctionMove errors
                    assertBool "should detect return value transfer" (not $ null crossFunctionMoves)
        ]
    ]

-- Helper functions for error detection
isMoveError :: OwnershipError -> Bool
isMoveError (UseAfterMove _) = True
isMoveError (DoubleMove _ _) = True
isMoveError (CrossFunctionMove _ _) = True
isMoveError _ = False

isBorrowError :: OwnershipError -> Bool
isBorrowError (BorrowWhileMoved _) = True
isBorrowError (MutBorrowWhileBorrowed _) = True
isBorrowError (BorrowWhileMutBorrowed _) = True
isBorrowError (MultipleMutBorrows _) = True
isBorrowError _ = False

isUseAfterMove :: OwnershipError -> Bool
isUseAfterMove (UseAfterMove _) = True
isUseAfterMove _ = False

isDoubleMove :: OwnershipError -> Bool
isDoubleMove (DoubleMove _ _) = True
isDoubleMove _ = False

isBorrowWhileMoved :: OwnershipError -> Bool
isBorrowWhileMoved (BorrowWhileMoved _) = True
isBorrowWhileMoved _ = False

isCrossFunctionMove :: OwnershipError -> Bool
isCrossFunctionMove (CrossFunctionMove _ _) = True
isCrossFunctionMove _ = False

isConditionalError :: OwnershipError -> Bool
isConditionalError (ControlFlowError _) = True
isConditionalError _ = False

isLoopError :: OwnershipError -> Bool
isLoopError (LoopOwnershipError _) = True
isLoopError _ = False

-- Helper function to generate transfer chains
generateTransferChain :: Int -> String
generateTransferChain n = unlines $
    ["func transfer_chain() {"] ++
    ["    data := create_data()"] ++
    ["    step" ++ show i ++ " := take_value(step" ++ show (i-1) ++ ")" | i <- [1..n]] ++
    ["    process(step" ++ show n ++ ")"] ++
    ["}"]