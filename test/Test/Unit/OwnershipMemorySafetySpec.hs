{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipMemorySafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , builtInFunctions
  )

import Compiler.OwnershipChecker (checkOwnership, checkOwnershipWithValueInfo)

import Parser (TypusFile(..), parseTypus)
import Compiler (compile)

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import qualified Data.Map as Map

-- Test: Ownership system prevents use-after-move
test_use_after_move_prevention :: TestTree
test_use_after_move_prevention = testCase "Ownership system prevents use-after-move" $ do
  let useAfterMoveCode = "//! ownership: true\n\npackage main\n\nfunc main() {\n  data := make([]int, 100)\n  moved := data\n  // Should not allow using data after move\n  data[0] = 42\n}"
      result = compile useAfterMoveCode
  case result of
    Right _ -> assertFailure "Expected ownership error for use-after-move"
    Left errs -> do
      let hasOwnershipError = L.any (\err -> "ownership" `L.isInfixOf` show err || "move" `L.isInfixOf` show err) errs
      if hasOwnershipError
        then return ()  -- Expected error
        else assertFailure $ "Expected ownership error, got: " ++ unlines (map show errs)

-- Property: Ownership transfer follows correct rules
prop_ownership_transfer_rules :: String -> String -> Property
prop_ownership_transfer_rules var1 var2 =
  not (null var1) && not (null var2) && 
  L.all isLetter var1 && L.all isLetter var2 && var1 /= var2 ==>
  let code = "//! ownership: true\n\npackage main\n\nfunc main() {\n  " ++ var1 ++ " := make([]int, 10)\n  " ++ var2 ++ " := " ++ var1 ++ "\n  // " ++ var1 ++ " should no longer be accessible\n}"
      result = compile code
  in case result of
    Right _ -> property False  -- Should detect ownership violation
    Left errs -> property $ L.any (\err -> "ownership" `L.isInfixOf` show err) errs

-- Test: Ownership system allows borrowing without transfer
test_borrowing_without_transfer :: TestTree
test_borrowing_without_transfer = testCase "Ownership system allows borrowing" $ do
  let borrowingCode = "//! ownership: true\n\npackage main\n\nfunc process(data []int) int {\n  return len(data)\n}\n\nfunc main() {\n  data := make([]int, 100)\n  result := process(data)  // Borrowing, not moving\n  // Should still be able to use data\n  data[0] = 42\n}"
      result = compile borrowingCode
  case result of
    Left errs -> assertFailure $ "Borrowing should be allowed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - borrowing worked correctly

-- Property: Ownership checker tracks function parameter ownership correctly
prop_function_parameter_ownership :: String -> Property
prop_function_parameter_ownership paramName =
  not (null paramName) && L.all isLetter paramName ==>
  let code = "//! ownership: true\n\npackage main\n\nfunc consume(" ++ paramName ++ " []int) int {\n  return len(" ++ paramName ++ ")\n}\n\nfunc main() {\n  data := make([]int, 10)\n  result := consume(data)\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Should succeed - parameter ownership handled correctly
    Left errs -> property $ not (L.any (\err -> "ownership" `L.isInfixOf` show err && "error" `L.isInfixOf` show err) errs)

-- Test: Ownership system handles shared ownership correctly
test_shared_ownership :: TestTree
test_shared_ownership = testCase "Ownership system handles shared ownership" $ do
  let sharedCode = "//! ownership: true\n\npackage main\n\nfunc main() {\n  data := make([]int, 100)\n  ref1 := &data\n  ref2 := &data\n  // Both references should be valid\n  ref1[0] = 42\n  ref2[1] = 84\n}"
      result = compile sharedCode
  case result of
    Left errs -> assertFailure $ "Shared ownership should be allowed: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - shared ownership worked correctly

-- Property: Ownership system prevents double free scenarios
prop_double_free_prevention :: String -> Property
prop_double_free_prevention varName =
  not (null varName) && L.all isLetter varName ==>
  let code = "//! ownership: true\n\npackage main\n\nfunc main() {\n  " ++ varName ++ " := make([]int, 10)\n  // First release\n  " ++ varName ++ " = nil\n  // Second access should be prevented\n  " ++ varName ++ "[0] = 42\n}"
      result = compile code
  in case result of
    Right _ -> property False  -- Should prevent double access
    Left errs -> property $ L.any (\err -> "ownership" `L.isInfixOf` show err || "null" `L.isInfixOf` show err) errs

-- Test: Ownership system handles lifetime annotations correctly
test_lifetime_annotations :: TestTree
test_lifetime_annotations = testCase "Ownership system handles lifetime annotations" $ do
  let lifetimeCode = "//! ownership: true\n\npackage main\n\nfunc main() {\n  data := make([]int, 100)\n  {\n    // Inner scope\n    ref := &data\n    ref[0] = 42\n  }\n  // data should still be valid in outer scope\n  data[1] = 84\n}"
      result = compile lifetimeCode
  case result of
    Left errs -> assertFailure $ "Lifetime handling should work: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - lifetime annotations handled correctly

-- Property: Ownership analyzer correctly identifies ownership transfer patterns
prop_ownership_transfer_identification :: [String] -> Property
prop_ownership_transfer_identification operations =
  L.length operations >= 2 && L.length operations <= 5 ==>
  let validOps = L.filter (L.all isLetter) operations
      code = "//! ownership: true\n\npackage main\n\nfunc main() {\n  data := make([]int, 10)\n" ++ unlines (L.map (\op -> "  " ++ op ++ " := data") validOps) ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property False  -- Should detect ownership issues with multiple transfers
    Left errs -> property $ L.any (\err -> "ownership" `L.isInfixOf` show err) errs

-- Test: Ownership system respects move semantics in function returns
test_move_semantics_in_returns :: TestTree
test_move_semantics_in_returns = testCase "Ownership system respects move semantics in returns" $ do
  let returnCode = "//! ownership: true\n\npackage main\n\nfunc createData() []int {\n  return make([]int, 100)\n}\n\nfunc main() {\n  data := createData()\n  data[0] = 42  // Should be able to use returned data\n}"
      result = compile returnCode
  case result of
    Left errs -> assertFailure $ "Move semantics in returns should work: " ++ unlines (map show errs)
    Right _ -> return ()  -- Success - move semantics in returns worked correctly

-- Property: Ownership system handles complex data structures correctly
prop_complex_data_structure_ownership :: String -> Property
prop_complex_data_structure_ownership structName =
  not (null structName) && L.all isLetter structName ==>
  let code = "//! ownership: true\n\npackage main\n\ntype " ++ structName ++ " struct {\n  data []int\n  next *" ++ structName ++ "\n}\n\nfunc main() {\n  node := &" ++ structName ++ "{data: make([]int, 10)}\n  node.data[0] = 42\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Should succeed - complex structures handled correctly
    Left errs -> property $ not (L.any (\err -> "ownership" `L.isInfixOf` show err && "error" `L.isInfixOf` show err) errs)

tests :: TestTree
tests = testGroup "Ownership Memory Safety Tests"
  [ test_use_after_move_prevention
  , test_borrowing_without_transfer
  , test_shared_ownership
  , test_lifetime_annotations
  , test_move_semantics_in_returns
  , fastProperty "Ownership transfer rules" prop_ownership_transfer_rules
  , fastProperty "Function parameter ownership" prop_function_parameter_ownership
  , fastProperty "Double free prevention" prop_double_free_prevention
  , fastProperty "Ownership transfer identification" prop_ownership_transfer_identification
  , fastProperty "Complex data structure ownership" prop_complex_data_structure_ownership
  ]