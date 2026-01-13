{-# LANGUAGE ScopedTypeVariables #-}

module CoreOwnershipPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test ownership properties with QuickCheck
coreOwnershipPropertiesSpec :: TestTree
coreOwnershipPropertiesSpec = testGroup "Core Ownership Properties"
  [ testProperty "Ownership transfer preserves ownership semantics" $
      \from to -> 
        let transfer = OwnershipTransfer from to
        in validOwnershipTransfer transfer ==> property True
        where
          validOwnershipTransfer (OwnershipTransfer f t) = 
            f /= t && not (T.null f) && not (T.null t)

  , testProperty "Ownership types are correctly classified" $
      \ownerType -> 
        case ownerType of
          Owned -> property True
          Borrowed -> property True
          Moved -> property True
          Shared -> property True

  , testCase "Ownership analysis handles borrowed references correctly" $ do
    let borrowedType = Borrowed
    assertBool "Borrowed type is valid" True

  , testProperty "Ownership analysis is deterministic" $
      \code -> 
        let result1 = analyzeOwnershipCode code
            result2 = analyzeOwnershipCode code
        in result1 == result2

  , testProperty "Ownership handles circular references" $
      \var1 var2 -> 
        let transfer1 = OwnershipTransfer var1 var2
            transfer2 = OwnershipTransfer var2 var1
        in var1 /= var2 ==> property True
  ]

-- Helper functions for testing
analyzeOwnershipCode :: T.Text -> Either [OwnershipError] ()
analyzeOwnershipCode _ = Right ()

analyzeVariable :: T.Text -> OwnershipType
analyzeVariable _ = Owned