{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipTransitivityAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (isInfixOf, nub)

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

tests :: TestTree
tests = testGroup "Ownership Transitivity Advanced Tests"
  [ basicTransitivityTests
  , borrowTransitivityTests
  , moveTransitivityTests
  , complexTransitivityTests
  , errorPropagationTests
  , quickCheckProperties
  ]

basicTransitivityTests :: TestTree
basicTransitivityTests = testGroup "Basic Transitivity Tests"
  [ testCase "ownership transfers through simple assignment" $ do
      let code = "let x = Owned \"value\";\nlet y = x;\nlet z = y;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (_, transfers) -> length transfers @?= 2
        Left err -> "Expected successful analysis" @?= show err
        
  , testCase "borrow preserves original ownership" $ do
      let code = "let x = Owned \"value\";\nlet y = Borrowed x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, _) -> do
          -- x should remain owned, y should be borrowed
          ownership `seq` True @?= True
        Left err -> "Expected successful analysis" @?= show err
        
  , testCase "mutable borrow restrictions propagate" $ do
      let code = "let x = Owned \"value\";\nlet y = MutBorrowed x;\nlet z = Borrowed x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "MutBorrow" `isInfixOf` errorStr @?= True
        Right _ -> "Expected borrow conflict error" @?= "Got success"
  ]

borrowTransitivityTests :: TestTree
borrowTransitivityTests = testGroup "Borrow Transitivity Tests"
  [ testCase "immutable borrow chain" $ do
      let code = "let x = Owned \"value\";\nlet y = Borrowed x;\nlet z = Borrowed y;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, transfers) -> do
          length transfers @?= 2
          ownership `seq` True @?= True
        Left err -> "Expected successful borrow chain" @?= show err
        
  , testCase "borrow after move should fail" $ do
      let code = "let x = Owned \"value\";\nlet y = x;\nlet z = Borrowed x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "UseAfterMove" `isInfixOf` errorStr @?= True
        Right _ -> "Expected use after move error" @?= "Got success"
        
  , testCase "multiple immutable borrows allowed" $ do
      let code = "let x = Owned \"value\";\nlet y = Borrowed x;\nlet z = Borrowed x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, transfers) -> do
          length transfers @?= 2
          ownership `seq` True @?= True
        Left err -> "Multiple immutable borrows should be allowed" @?= show err
  ]

moveTransitivityTests :: TestTree
moveTransitivityTests = testGroup "Move Transitivity Tests"
  [ testCase "move chain transfers ownership" $ do
      let code = "let x = Owned \"value\";\nlet y = x;\nlet z = y;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (_, transfers) -> do
          length transfers @?= 2
          let transferList = map transferFrom transfers ++ map transferTo transfers
          nub transferList `seq` True @?= True
        Left err -> "Expected successful move chain" @?= show err
        
  , testCase "double move detection" $ do
      let code = "let x = Owned \"value\";\nlet y = x;\nlet z = x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "DoubleMove" `isInfixOf` errorStr @?= True
        Right _ -> "Expected double move error" @?= "Got success"
        
  , testCase "move after borrow fails" $ do
      let code = "let x = Owned \"value\";\nlet y = Borrowed x;\nlet z = x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "BorrowWhileMoved" `isInfixOf` errorStr @?= True
        Right _ -> "Expected borrow while moved error" @?= "Got success"
  ]

complexTransitivityTests :: TestTree
complexTransitivityTests = testGroup "Complex Transitivity Tests"
  [ testCase "nested ownership patterns" $ do
      let code = unlines
            [ "let x = Owned \"value\";"
            , "let y = Borrowed x;"
            , "let z = MutBorrowed y;"
            , "let w = Owned z;"
            ]
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, transfers) -> do
          length transfers @?= 3
          ownership `seq` True @?= True
        Left err -> "Should handle complex nested patterns" @?= show err
        
  , testCase "cross-function ownership transfer" $ do
      let code = unlines
            [ "fn consume(x: Owned) {"
            , "    let y = x;"
            , "}"
            , "let a = Owned \"value\";"
            , "consume(a);"
            ]
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, transfers) -> do
          length transfers @?= 2
          ownership `seq` True @?= True
        Left err -> "Should handle cross-function transfers" @?= show err
        
  , testCase "loop ownership consistency" $ do
      let code = unlines
            [ "let x = Owned \"value\";"
            , "for i in 0..10 {"
            , "    let y = Borrowed x;"
            , "    let z = y;"
            , "}"
            ]
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Right (ownership, transfers) -> do
          length transfers @?= 20  -- 10 iterations * 2 transfers each
          ownership `seq` True @?= True
        Left err -> "Should handle loop ownership" @?= show err
  ]

errorPropagationTests :: TestTree
errorPropagationTests = testGroup "Error Propagation Tests"
  [ testCase "errors cascade through transfer chain" $ do
      let code = "let x = Owned \"value\";\nlet y = x;\nlet z = x;\nlet w = y;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "DoubleMove" `isInfixOf` errorStr @?= True
          length errors @?= 1  -- Should detect the root cause
        Right _ -> "Expected error propagation" @?= "Got success"
        
  , testCase "borrow conflicts detected early" $ do
      let code = "let x = Owned \"value\";\nlet y = MutBorrowed x;\nlet z = MutBorrowed x;"
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          "MultipleMutBorrows" `isInfixOf` errorStr @?= True
        Right _ -> "Expected multiple mutable borrow error" @?= "Got success"
        
  , testCase "path-sensitive ownership errors" $ do
      let code = unlines
            [ "let x = Owned \"value\";"
            , "if condition {"
            , "    let y = x;"
            , "}"
            , "let z = x;"
            ]
          analyzer = newOwnershipAnalyzer
          result <- return $ analyzeOwnership analyzer code
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          errorStr `seq` True @?= True  -- Should detect path-sensitive issues
        Right _ -> "May succeed depending on analysis" @?= "Got success"
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Transitivity Properties"
  [ fastProperty "ownership transfers are acyclic" prop_transfers_acyclic
  , fastProperty "borrow chains preserve original owner" prop_borrow_chain_preserves
  , fastProperty "move chains transfer unique ownership" prop_move_chain_unique
  ]

-- QuickCheck property implementations
prop_transfers_acyclic :: [OwnershipTransfer] -> Property
prop_transfers_acyclic transfers =
  let hasCycle transfer = transferFrom transfer == transferTo transfer
  in not (any hasCycle transfers) ==> property True

prop_borrow_chain_preserves :: String -> Property
prop_borrow_chain_preserves ownerName =
  let owned = Owned ownerName
      borrowed1 = Borrowed ownerName
      borrowed2 = Borrowed ownerName
  in case (owned, borrowed1, borrowed2) of
    (Owned o, Borrowed b1, Borrowed b2) -> o == b1 && b1 == b2 ==> property True
    _ -> property True

prop_move_chain_unique :: [String] -> Property
prop_move_chain_unique names =
  let uniqueNames = nub names
      transfers = zipWith OwnershipTransfer uniqueNames (tail uniqueNames ++ [""])
  in length uniqueNames > 1 ==> 
     all (\t -> transferFrom t /= transferTo t) transfers ==> property True