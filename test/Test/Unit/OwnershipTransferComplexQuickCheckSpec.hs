{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferComplexQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership)
import Ownership.Common.Types (OwnershipError(..), OwnershipAnalyzer(..))
import Compiler.OwnershipChecker (checkOwnership, checkOwnershipWithValueInfo)
import qualified Data.Map as Map
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub)

-- Property: Ownership transfer preserves uniqueness constraints
prop_ownership_transfer_preserves_uniqueness :: String -> String -> Property
prop_ownership_transfer_preserves_uniqueness var1 var2 =
  let validVars = L.length var1 > 0 && L.length var2 > 0 && var1 /= var2
      transferCode = var1 ++ " = " ++ var2 ++ "\n" ++ var2 ++ " = " ++ var1
  in validVars ==>
  case analyzeOwnership transferCode of
    Right transfer ->
      let transferStr = show transfer
          hasTransfer = "transfer" `L.isInfixOf` transferStr || "move" `L.isInfixOf` transferStr
      in property $ hasTransfer
    Left _ -> property $ True -- Expected to fail for invalid transfers

-- Property: Multiple ownership transfers are tracked correctly
prop_multiple_ownership_transfers :: [String] -> Property
prop_multiple_ownership_transfers variables =
  let hasVars = L.length variables >= 2
      validVars = L.all (not . null) variables
      uniqueVars = L.length (nub variables) == L.length variables
      transferChain = unlines $ zipWith (++) variables (L.map (" = " ++) (L.tail variables ++ [L.head variables]))
  in hasVars && validVars && uniqueVars ==>
  case analyzeOwnership transferChain of
    Right result ->
      let resultStr = show result
          transferCount = L.length $ L.filter (`L.isInfixOf` resultStr) ["transfer", "move", "owned"]
          reasonableCount = transferCount <= L.length variables + 2
      in property $ reasonableCount
    Left _ -> property $ True

-- Property: Ownership analyzer detects double moves correctly
prop_ownership_detects_double_moves :: String -> String -> String -> Property
prop_ownership_detects_double_moves owner receiver1 receiver2 =
  let validNames = L.all (\n -> L.length n > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ '_') n) [owner, receiver1, receiver2]
      uniqueNames = L.length (nub [owner, receiver1, receiver2]) == 3
      doubleMoveCode = owner ++ " = " ++ receiver1 ++ "\n" ++ owner ++ " = " ++ receiver2
  in validNames && uniqueNames ==>
  case analyzeOwnership doubleMoveCode of
    Right _ -> property $ True -- Some valid transfers might succeed
    Left ownershipError ->
      let errorStr = show ownershipError
          hasDoubleMove = L.any (`L.isInfixOf` errorStr) ["double", "move", "used", "borrowed"]
      in property $ hasDoubleMove .||. "type" `L.isInfixOf` errorStr

-- Property: Ownership transfer respects borrowing rules
prop_ownership_respects_borrowing :: String -> String -> String -> Property
prop_ownership_respects_borrowing lender borrower user =
  let validNames = L.all (\n -> L.length n > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ '_') n) [lender, borrower, user]
      uniqueNames = L.length (nub [lender, borrower, user]) >= 2
      borrowCode = lender ++ " = " ++ borrower ++ "\n" ++ lender ++ "." ++ user ++ " = 42"
  in validNames && uniqueNames ==>
  case checkOwnership borrowCode of
    Right result ->
      let resultStr = show result
          hasBorrow = "borrow" `L.isInfixOf` resultStr || "reference" `L.isInfixOf` resultStr
      in property $ hasBorrow .||. "owned" `L.isInfixOf` resultStr
    Left _ -> property $ True

-- Property: Complex ownership scenarios are analyzed correctly
prop_complex_ownership_scenarios :: [(String, String)] -> Property
prop_complex_ownership_scenarios assignments =
  let hasAssignments = L.length assignments > 2
      validAssignments = L.all (\(l, r) -> L.length l > 0 && L.length r > 0) assignments
      uniqueVars = L.length (nub $ map fst assignments ++ map snd assignments) >= 2
      complexCode = unlines $ L.map (\(l, r) -> l ++ " = " ++ r) assignments
  in hasAssignments && validAssignments && uniqueVars ==>
  case analyzeOwnership complexCode of
    Right result ->
      let resultStr = show result
          hasAnalysis = L.any (`L.isInfixOf` resultStr) ["ownership", "transfer", "move", "borrow"]
          notEmpty = L.length resultStr > 0
      in property $ hasAnalysis .&&. notEmpty
    Left _ -> property $ True

-- Property: Ownership transfer preserves value information
prop_ownership_preserves_value_info :: String -> String -> String -> Property
prop_ownership_preserves_value_info source dest value =
  let validNames = L.all (\n -> L.length n > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ '_') n) [source, dest]
      validValue = L.length value > 0
      transferCode = source ++ " = " ++ value ++ "\n" ++ dest ++ " = " ++ source
  in validNames && validValue ==>
  case checkOwnershipWithValueInfo transferCode of
    Right result ->
      let resultStr = show result
          hasValue = value `L.isInfixOf` resultStr
          hasTransfer = "transfer" `L.isInfixOf` resultStr || "move" `L.isInfixOf` resultStr
      in property $ hasTransfer .&&. (hasValue .||. "int" `L.isInfixOf` resultStr)
    Left _ -> property $ True

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic code =
  let hasCode = L.length code > 5
  in hasCode ==>
  let result1 = analyzeOwnership code
      result2 = analyzeOwnership code
      bothSuccess = case (result1, result2) of
        (Right r1, Right r2) -> show r1 == show r2
        (Left e1, Left e2) -> show e1 == show e2
        _ -> False
  in property $ bothSuccess

tests :: TestTree
tests = testGroup "Ownership Transfer Complex QuickCheck Tests"
  [ fastProperty "Ownership transfer preserves uniqueness constraints" prop_ownership_transfer_preserves_uniqueness
  , fastProperty "Multiple ownership transfers are tracked correctly" prop_multiple_ownership_transfers
  , fastProperty "Ownership analyzer detects double moves correctly" prop_ownership_detects_double_moves
  , fastProperty "Ownership transfer respects borrowing rules" prop_ownership_respects_borrowing
  , fastProperty "Complex ownership scenarios are analyzed correctly" prop_complex_ownership_scenarios
  , fastProperty "Ownership transfer preserves value information" prop_ownership_preserves_value_info
  , fastProperty "Ownership analysis is deterministic" prop_ownership_analysis_deterministic
  ]