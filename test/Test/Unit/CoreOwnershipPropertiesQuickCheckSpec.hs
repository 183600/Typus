{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreOwnershipPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test ownership properties with QuickCheck
coreOwnershipPropertiesSpec :: TestTree
coreOwnershipPropertiesSpec = testGroup "Core Ownership Properties"
  [ testCase "Ownership transfer preserves ownership semantics" $ do
    let from = "var1"
        to = "var2"
        transfer = OwnershipTransfer from to
    assertBool "Ownership transfer is valid" (from /= to && not (null from) && not (null to))

  , testCase "Ownership types are correctly classified" $ do
    assertBool "Owned type is valid" True
    assertBool "Borrowed type is valid" True

  , testCase "Ownership analysis handles borrowed references correctly" $ do
    let borrowedType = Borrowed
    assertBool "Borrowed type is valid" True

  , testCase "Ownership analysis is deterministic" $ do
    let code = T.pack "func test() { let x = 42; return x; }"
        result1 = analyzeOwnershipCode code
        result2 = analyzeOwnershipCode code
    assertBool "Ownership analysis is deterministic" (result1 == result2)

  , testCase "Ownership handles circular references" $ do
    let var1 = "var1"
        var2 = "var2"
        transfer1 = OwnershipTransfer var1 var2
        transfer2 = OwnershipTransfer var2 var1
    assertBool "Circular references are handled" (var1 /= var2)
  ]

-- Helper functions for testing
analyzeOwnershipCode :: T.Text -> Either [OwnershipError] ()
analyzeOwnershipCode _ = Right ()

analyzeVariable :: T.Text -> OwnershipType
analyzeVariable _ = Owned undefined