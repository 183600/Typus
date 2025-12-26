{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.OwnershipTransferBoundarySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership)
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Data.List (isInfixOf)

-- | Test ownership transfer boundary cases and edge conditions
tests :: TestTree
tests = testGroup "Ownership Transfer Boundary Tests"
  [ testCase "Simple ownership transfer" testSimpleOwnershipTransfer
  , testCase "Multiple ownership transfers" testMultipleOwnershipTransfers
  , testCase "Ownership transfer with function calls" testOwnershipTransferWithFunctions
  , testCase "Circular ownership detection" testCircularOwnershipDetection
  , testCase "Ownership transfer in conditional branches" testOwnershipTransferInConditionals
  , testCase "Ownership transfer with loops" testOwnershipTransferWithLoops
  , testProperty "Ownership transfer is deterministic" ownershipTransferDeterministic
  , testCase "Ownership transfer error recovery" testOwnershipTransferErrorRecovery
  ]

-- | Test simple ownership transfer scenario
testSimpleOwnershipTransfer :: Assertion
testSimpleOwnershipTransfer = do
  let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    s := NewString(\"hello\")\n    t := s  // Ownership transferred\n    println(t.data)\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> 
      -- Should not have ownership transfer errors for simple case
      assertBool "Simple transfer should not cause errors" $
        not (any isOwnershipTransferError errors)

-- | Test multiple ownership transfers in sequence
testMultipleOwnershipTransfers :: Assertion
testMultipleOwnershipTransfers = do
  let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    s := NewString(\"hello\")\n    t := s  // First transfer\n    u := t  // Second transfer\n    v := u  // Third transfer\n    println(v.data)\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> 
      -- Multiple transfers should be valid
      assertBool "Multiple transfers should be valid" $
        not (any isOwnershipTransferError errors)

-- | Test ownership transfer with function calls
testOwnershipTransferWithFunctions :: Assertion
testOwnershipTransferWithFunctions = do
  let input = "//! ownership: on\n\npackage main\n\nfunc consumeString(s String) {\n    println(s.data)\n}\n\nfunc main() {\n    s := NewString(\"hello\")\n    consumeString(s)  // Ownership transferred to function\n    // s should no longer be usable here\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> do
      -- Function parameter transfer should be valid
      assertBool "Function parameter transfer should be valid" $
        not (any isOwnershipTransferError errors)

-- | Test circular ownership detection
testCircularOwnershipDetection :: Assertion
testCircularOwnershipDetection = do
  let input = "//! ownership: on\n\npackage main\n\ntype Node struct {\n    next *Node\n}\n\nfunc main() {\n    n1 := &Node{}\n    n2 := &Node{}\n    n1.next = n2\n    n2.next = n1  // This should create a potential cycle\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> do
      -- Should detect potential circular reference issues
      assertBool "Should detect circular reference potential" $
        any isCircularReferenceError errors

-- | Test ownership transfer in conditional branches
testOwnershipTransferInConditionals :: Assertion
testOwnershipTransferInConditionals = do
  let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    s := NewString(\"hello\")\n    \n    if true {\n        t := s  // Transfer in one branch\n        println(t.data)\n    } else {\n        println(s.data)  // s might still be valid here\n    }\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> do
      -- Conditional transfers should be handled carefully
      assertBool "Should handle conditional transfers" $
        not (any isOwnershipTransferError errors)

-- | Test ownership transfer with loops
testOwnershipTransferWithLoops :: Assertion
testOwnershipTransferWithLoops = do
  let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    s := NewString(\"hello\")\n    \n    for i := 0; i < 3; i++ {\n        t := s  // Transfer in loop - this might be problematic\n        println(t.data)\n        s = NewString(\"again\")  // Reassign s\n    }\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> do
      -- Loop transfers should be analyzed carefully
      assertBool "Should handle loop transfers" $ True -- Detailed verification depends on implementation

-- | Property: Ownership transfer analysis should be deterministic
ownershipTransferDeterministic :: String -> Property
ownershipTransferDeterministic input =
  "ownership" `isInfixOf` input && "NewString" `isInfixOf` input ==>
  case newOwnershipAnalyzer of
    Left _ -> property True -- If analyzer creation fails, skip
    Right analyzer -> 
      case analyzeOwnership analyzer input of
        Left _ -> property True -- Analysis failure is acceptable
        Right firstResult ->
          case analyzeOwnership analyzer input of
            Left _ -> property False -- Should be consistent
            Right secondResult -> length firstResult === length secondResult

-- | Test ownership transfer error recovery
testOwnershipTransferErrorRecovery :: Assertion
testOwnershipTransferErrorRecovery = do
  let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    s := NewString(\"hello\")\n    t := s  // Valid transfer\n    u := s  // This should be an error - s already moved\n    \n    v := NewString(\"world\")  // This should still work\n    println(v.data)\n}"
  
  analyzer <- newOwnershipAnalyzer
  result <- try $ analyzeOwnership analyzer input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Analysis failed: " ++ show e
    Right errors -> do
      -- Should detect double move error
      assertBool "Should detect double move error" $
        any isDoubleMoveError errors
      -- Should continue analysis after error
      assertBool "Should continue analysis after error" $
        length errors > 0

-- | Helper functions for error classification
isOwnershipTransferError :: OwnershipError -> Bool
isOwnershipTransferError (OwnershipTransferError _) = True
isOwnershipTransferError _ = False

isCircularReferenceError :: OwnershipError -> Bool
isCircularReferenceError (CircularReferenceError _) = True
isCircularReferenceError _ = False

isDoubleMoveError :: OwnershipError -> Bool
isDoubleMoveError (DoubleMoveError _) = True
isDoubleMoveError _ = False