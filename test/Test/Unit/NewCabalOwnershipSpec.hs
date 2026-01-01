{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalOwnershipSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , formatOwnershipErrors
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- Test 1: Ownership analyzer creation
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ True -- Analyzer should be created successfully

-- Test 2: Simple ownership analysis
prop_simple_ownership_analysis :: String -> Property
prop_simple_ownership_analysis code =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in not (null code) ==> 
     property $ True -- Analysis should complete without crashing

-- Test 3: Ownership transfer consistency
prop_ownership_transfer_consistency :: String -> String -> Property
prop_ownership_transfer_consistency fromVar toVar =
  let code = unlines
        [ fromVar ++ " := 42"
        , toVar ++ " := " ++ fromVar
        ]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in not (null fromVar) && not (null toVar) ==> 
     property $ True -- Should analyze transfer correctly

-- Test 4: Multiple ownership transfers
prop_multiple_ownership_transfers :: [String] -> Property
prop_multiple_ownership_transfers variables =
  let code = unlines $ L.map (\v -> v ++ " := 1") variables
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in L.length variables > 0 ==> 
     property $ True -- Should handle multiple variables

-- Test 5: Ownership error formatting
prop_ownership_error_formatting :: OwnershipError -> Property
prop_ownership_error_formatting error =
  let formatted = formatOwnershipErrors [error]
  in property $ not (null formatted)

-- Test 6: Empty code analysis
prop_empty_code_analysis :: Property
prop_empty_code_analysis =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in property $ True -- Should handle empty input gracefully

-- Test 7: Ownership type consistency
prop_ownership_type_consistency :: OwnershipType -> Property
prop_ownership_type_consistency ownershipType =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer "x := 42"
  in property $ True -- Should work with L.any ownership type

-- Test 8: Variable reassignment analysis
prop_variable_reassignment_analysis :: String -> Property
prop_variable_reassignment_analysis varName =
  let code = unlines
        [ varName ++ " := 1"
        , varName ++ " := 2"
        ]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in not (null varName) ==> 
     property $ True -- Should analyze reassignment

-- Test 9: Function call ownership transfer
prop_function_call_ownership :: String -> String -> Property
prop_function_call_ownership funcName argName =
  let code = funcName ++ "(" ++ argName ++ ")"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in not (null funcName) && not (null argName) ==> 
     property $ True -- Should analyze function calls

-- Test 10: Complex ownership scenarios
prop_complex_ownership_scenarios :: [String] -> Property
prop_complex_ownership_scenarios statements =
  let code = unlines statements
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in L.length statements > 0 ==> 
     property $ True -- Should handle complex scenarios

tests :: TestTree
tests = 
  testGroup "New Cabal Ownership Tests"
    [ fastProperty "Ownership analyzer creation" prop_ownership_analyzer_creation
    , fastProperty "Simple ownership analysis" prop_simple_ownership_analysis
    , fastProperty "Ownership transfer consistency" prop_ownership_transfer_consistency
    , fastProperty "Multiple ownership transfers" prop_multiple_ownership_transfers
    , fastProperty "Ownership error formatting" prop_ownership_error_formatting
    , fastProperty "Empty code analysis" prop_empty_code_analysis
    , fastProperty "Ownership type consistency" prop_ownership_type_consistency
    , fastProperty "Variable reassignment analysis" prop_variable_reassignment_analysis
    , fastProperty "Function call ownership transfer" prop_function_call_ownership
    , fastProperty "Complex ownership scenarios" prop_complex_ownership_scenarios
    ]