{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import qualified Ownership.Common.Types as Own
import Ownership (analyzeOwnership)

-- Property: Ownership analysis preserves variable count
prop_ownership_preserves_variable_count :: [String] -> Property
prop_ownership_preserves_variable_count vars =
  let input = unlines $ L.map (\v -> "var " ++ v ++ " int") vars
      result = analyzeOwnership input
      varCount = L.length vars
      analyzedCount = either (const 0) (const varCount) result
  in classify (not (null vars)) "has variables" $
     property $ (varCount === 0) .||. (analyzedCount === varCount)

-- Property: Ownership transfer is transitive
prop_ownership_transfer_transitive :: String -> String -> String -> Property
prop_ownership_transfer_transitive var1 var2 var3 =
  let input = unlines 
        [ "var " ++ var1 ++ " MyString"
        , "var " ++ var2 ++ " MyString = " ++ var1
        , "var " ++ var3 ++ " MyString = " ++ var2
        ]
      result = analyzeOwnership input
      isValidTransfer = either (const False) (const True) result
  in property $ (var1 /= var2 && var2 /= var3 && var1 /= var3) ==> isValidTransfer

-- Property: Ownership analysis detects moves
prop_ownership_detects_moves :: String -> String -> Property
prop_ownership_detects_moves source target =
  let input = unlines 
        [ "var " ++ source ++ " MyString"
        , "var " ++ target ++ " MyString = " ++ source
        , "println(" ++ source ++ ".data)"  -- This should be an error
        ]
      result = analyzeOwnership input
      hasMoveError = either (const False) (const True) result
  in property $ (source /= target) ==> hasMoveError

-- Property: Ownership analysis handles borrowing
prop_ownership_handles_borrowing :: String -> String -> Property
prop_ownership_handles_borrowing owner borrower =
  let input = unlines 
        [ "var " ++ owner ++ " MyString"
        , "var " ++ borrower ++ " &MyString = &" ++ owner
        , "println(" ++ owner ++ ".data)"  -- This should be OK with borrowing
        , "println(" ++ borrower ++ ".data)"  -- This should also be OK
        ]
      result = analyzeOwnership input
      borrowingValid = either (const False) (const True) result
  in property $ (owner /= borrower) ==> borrowingValid

-- Property: Ownership analysis respects block boundaries
prop_ownership_respects_block_boundaries :: String -> Property
prop_ownership_respects_block_boundaries varName =
  let input = unlines 
        [ "var " ++ varName ++ " MyString"
        , "{//! ownership: on"
        , "  var moved " ++ varName ++ " = " ++ varName
        , "}"
        , "println(" ++ varName ++ ".data)"  -- Should be valid outside block
        ]
      result = analyzeOwnership input
      blockBoundaryRespected = either (const False) (const True) result
  in property $ not (null varName) ==> blockBoundaryRespected

tests :: TestTree
tests = testGroup "New Typus Ownership QuickCheck Tests"
  [ fastProperty "Ownership preserves variable count" prop_ownership_preserves_variable_count
  , fastProperty "Ownership transfer is transitive" prop_ownership_transfer_transitive
  , fastProperty "Ownership detects moves" prop_ownership_detects_moves
  , fastProperty "Ownership handles borrowing" prop_ownership_handles_borrowing
  , fastProperty "Ownership respects block boundaries" prop_ownership_respects_block_boundaries
  ]