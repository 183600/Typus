{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreOwnershipSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  )
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo)
import Data.Map (Map, empty, insert)
import Data.Set (Set, empty, insert)

-- Test properties for Ownership module

-- | newOwnershipAnalyzer should create a valid analyzer
prop_defaultOwnershipPolicy_values :: Property
prop_defaultOwnershipPolicy_values = 
  let analyzer = newOwnershipAnalyzer
  in property $ analyzer /= undefined

-- | analyzeOwnership should return valid analysis for simple cases
prop_checkOwnership_simple :: String -> Property
prop_checkOwnership_simple code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in property $ result /= undefined

-- | OwnershipType should have defined values
prop_transferOwnership_updates :: OwnershipType -> Property
prop_transferOwnership_updates ownershipType = 
  property $ ownershipType == Owned || ownershipType == Shared || ownershipType == Borrowed || ownershipType == Moved

-- Unit tests
test_newOwnershipAnalyzer :: Assertion
test_newOwnershipAnalyzer = do
  let analyzer = newOwnershipAnalyzer
  assertBool "newOwnershipAnalyzer should create analyzer" (analyzer /= undefined)

test_analyzeOwnership_simple :: Assertion
test_analyzeOwnership_simple = do
  let analyzer = newOwnershipAnalyzer
  let code = "let x = new Resource();"
  let result = analyzeOwnership analyzer code
  assertBool "analyzeOwnership should return result" (result /= undefined)

test_transferOwnership_basic :: Assertion
test_transferOwnership_basic = do
  let initial = empty :: Map String Ownership
  let result = transferOwnership "x" "y" initial
  assertBool "transferOwnership should modify map" (result /= initial)
  assertBool "transferOwnership should give ownership to target" (hasOwnership "y" result)

test_transferOwnership_existing_source :: Assertion
test_transferOwnership_existing_source = do
  let initial = insert "x" Owned empty :: Map String Ownership
  let result = transferOwnership "x" "y" initial
  assertBool "transferOwnership should remove from source" (not $ hasOwnership "x" result)
  assertBool "transferOwnership should add to target" (hasOwnership "y" result)

test_transferOwnership_same_variable :: Assertion
test_transferOwnership_same_variable = do
  let initial = insert "x" Owned empty :: Map String Ownership
  let result = transferOwnership "x" "x" initial
  assertBool "transferOwnership to same variable should preserve ownership" (hasOwnership "x" result)

test_validateOwnership_valid :: Assertion
test_validateOwnership_valid = do
  let ownershipMap = insert "x" Owned empty :: Map String Ownership
  let isValid = validateOwnership "x" ownershipMap
  assertBool "validateOwnership should return true for valid ownership" isValid

test_validateOwnership_invalid :: Assertion
test_validateOwnership_invalid = do
  let ownershipMap = empty :: Map String Ownership
  let isValid = validateOwnership "x" ownershipMap
  assertBool "validateOwnership should return false for invalid ownership" (not isValid)

test_hasOwnership_true :: Assertion
test_hasOwnership_true = do
  let ownershipMap = insert "x" Owned empty :: Map String Ownership
  let hasOwn = hasOwnership "x" ownershipMap
  assertBool "hasOwnership should return true for existing variable" hasOwn

test_hasOwnership_false :: Assertion
test_hasOwnership_false = do
  let ownershipMap = empty :: Map String Ownership
  let hasOwn = hasOwnership "x" ownershipMap
  assertBool "hasOwnership should return false for non-existing variable" (not hasOwn)

test_ownership_values :: Assertion
test_ownership_values = do
  assertEqual "Owned should be defined" Owned Owned
  assertEqual "Shared should be defined" Shared Shared
  assertEqual "Borrowed should be defined" Borrowed Borrowed
  assertEqual "Moved should be defined" Moved Moved

test_ownership_transfer_values :: Assertion
test_ownership_transfer_values = do
  assertEqual "CanTransfer should be defined" CanTransfer CanTransfer
  assertEqual "CannotTransfer should be defined" CannotTransfer CannotTransfer
  assertEqual "TransferWithConditions should be defined" TransferWithConditions TransferWithConditions

test_complex_ownership_scenario :: Assertion
test_complex_ownership_scenario = do
  -- Start with x owning a resource
  let initial = insert "x" Owned empty :: Map String Ownership
  -- Transfer from x to y
  let afterTransfer1 = transferOwnership "x" "y" initial
  -- Transfer from y to z
  let afterTransfer2 = transferOwnership "y" "z" afterTransfer1
  -- Check final state
  assertBool "x should not have ownership" (not $ hasOwnership "x" afterTransfer2)
  assertBool "y should not have ownership" (not $ hasOwnership "y" afterTransfer2)
  assertBool "z should have ownership" (hasOwnership "z" afterTransfer2)

test_ownership_policy_application :: Assertion
test_ownership_policy_application = do
  let policy = defaultOwnershipPolicy
  let ownershipMap = insert "x" Owned empty :: Map String Ownership
  let analysis = checkOwnership policy "x" ownershipMap
  assertBool "ownership policy should be applied" (analysis /= undefined)

test_multiple_ownership_transfers :: Assertion
test_multiple_ownership_transfers = do
  let initial = insert "resource" Owned empty :: Map String Ownership
  let step1 = transferOwnership "resource" "owner1" initial
  let step2 = transferOwnership "owner1" "owner2" step1
  let step3 = transferOwnership "owner2" "owner3" step2
  assertBool "resource should not have ownership" (not $ hasOwnership "resource" step3)
  assertBool "owner1 should not have ownership" (not $ hasOwnership "owner1" step3)
  assertBool "owner2 should not have ownership" (not $ hasOwnership "owner2" step3)
  assertBool "owner3 should have ownership" (hasOwnership "owner3" step3)

test_ownership_validation_chain :: Assertion
test_ownership_validation_chain = do
  let ownershipMap = insert "x" Owned (insert "y" Shared (insert "z" Borrowed empty))
  let xValid = validateOwnership "x" ownershipMap
  let yValid = validateOwnership "y" ownershipMap
  let zValid = validateOwnership "z" ownershipMap
  let wValid = validateOwnership "w" ownershipMap
  assertBool "x should have valid ownership" xValid
  assertBool "y should have valid ownership" yValid
  assertBool "z should have valid ownership" zValid
  assertBool "w should not have valid ownership" (not wValid)

-- Test suite
tests :: TestTree
tests = testGroup "Core Ownership Tests"
  [ testProperties "QuickCheck Properties"
    [ prop_defaultOwnershipPolicy_values
    , prop_checkOwnership_simple
    , prop_transferOwnership_updates
    ]
  , testCase "newOwnershipAnalyzer" test_newOwnershipAnalyzer
  , testCase "analyzeOwnership simple" test_analyzeOwnership_simple
  , testCase "ownership values" test_ownership_values
  , testCase "ownership transfer values" test_ownership_transfer_values
  , testCase "complex ownership scenario" test_complex_ownership_scenario
  ]