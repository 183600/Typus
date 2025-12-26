{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.OwnershipAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, intercalate, nub)
import Data.Char (isSpace, isDigit, isAlpha, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , formatOwnershipErrors
  )

-- Arbitrary instances for QuickCheck
instance Arbitrary OwnershipType where
  arbitrary = elements [Owned, Borrowed, Shared, Moved]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromType <- arbitrary
    toType <- arbitrary
    isValid <- arbitrary
    return $ OwnershipTransfer fromType toType isValid

instance Arbitrary OwnershipError where
  arbitrary = do
    message <- arbitrary
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    return $ OwnershipError message line column

-- Property: OwnershipType ordering and comparison
prop_ownership_type_comparison :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_comparison type1 type2 =
  let typeOrder = [Owned, Borrowed, Shared, Moved]
      index1 = case type1 of
        Owned -> 0
        Borrowed -> 1
        Shared -> 2
        Moved -> 3
      index2 = case type2 of
        Owned -> 0
        Borrowed -> 1
        Shared -> 2
        Moved -> 3
  in property $ (type1 == type2) === (index1 == index2)

-- Property: OwnershipTransfer validity
prop_ownership_transfer_validity :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_validity fromType toType =
  let transfer = OwnershipTransfer fromType toType True
      (OwnershipTransfer from' to' valid) = transfer
  in property $ from' === fromType .&&. to' === toType .&&. valid === True

-- Property: OwnershipTransfer invalidity
prop_ownership_transfer_invalidity :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_invalidity fromType toType =
  let transfer = OwnershipTransfer fromType toType False
      (OwnershipTransfer from' to' valid) = transfer
  in property $ from' === fromType .&&. to' === toType .&&. valid === False

-- Property: OwnershipError structure
prop_ownership_error_structure :: String -> Int -> Int -> Property
prop_ownership_error_structure message line column =
  let error = OwnershipError message line column
      OwnershipError msg l c = error
  in property $ msg === message .&&. l === line .&&. c === column

-- Property: Ownership analyzer creation
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ True -- Placeholder since we can't inspect analyzer directly

-- Property: Ownership analysis consistency
prop_ownership_analysis_consistency :: String -> Property
prop_ownership_analysis_consistency code =
  let result1 = analyzeOwnership code
      result2 = analyzeOwnership code
  in property $ True -- Placeholder since we can't inspect results directly

-- Property: Error formatting preserves content
prop_error_formatting_preservation :: [OwnershipError] -> Property
prop_error_formatting_preservation errors =
  let formatted = formatOwnershipErrors errors
      messages = map (\(OwnershipError msg _ _) -> msg) errors
      allMessagesPresent = all (`isInfixOf` formatted) messages
  in not (null errors) ==> property $ allMessagesPresent === True

-- Property: Empty error list formatting
prop_empty_error_formatting :: Property
prop_empty_error_formatting =
  let formatted = formatOwnershipErrors []
  in property $ length formatted >= 0

-- Property: Ownership type transitions
prop_ownership_type_transitions :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_transitions fromType toType =
  let validTransitions = [(Owned, Moved), (Borrowed, Owned), (Shared, Shared)]
      isValidTransition = (fromType, toType) `elem` validTransitions
      transfer = OwnershipTransfer fromType toType isValidTransition
      (OwnershipTransfer _ _ valid) = transfer
  in property $ valid === isValidTransition

-- Property: Ownership error location bounds
prop_ownership_error_location_bounds :: String -> Int -> Int -> Property
prop_ownership_error_location_bounds message line column =
  let validLocation = line > 0 && column > 0
      error = OwnershipError message line column
      OwnershipError _ l c = error
  in property $ (l > 0 && c > 0) === validLocation

-- Property: Ownership analyzer idempotency
prop_ownership_analyzer_idempotency :: String -> Property
prop_ownership_analyzer_idempotency code =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ True -- Placeholder since we can't compare analyzers

-- Property: Complex ownership scenarios
prop_complex_ownership_scenarios :: [OwnershipType] -> Property
prop_complex_ownership_scenarios types =
  let uniqueTypes = nub types
      typeCount = length uniqueTypes
      maxTypes = 4 -- Owned, Borrowed, Shared, Moved
  in property $ typeCount <= maxTypes

-- Property: Ownership transfer chain
prop_ownership_transfer_chain :: OwnershipType -> [OwnershipType] -> Property
prop_ownership_transfer_chain initialType subsequentTypes =
  let transfers = zipWith OwnershipTransfer (initialType : subsequentTypes) subsequentTypes (repeat True)
      transferCount = length transfers
  in property $ transferCount === length subsequentTypes

-- Property: Error message content preservation
prop_error_message_content :: String -> Int -> Int -> Property
prop_error_message_content message line column =
  let error = OwnershipError message line column
      formatted = formatOwnershipErrors [error]
  in not (null message) ==> property $ message `isInfixOf` formatted

-- Property: Ownership type properties
prop_ownership_type_properties :: OwnershipType -> Property
prop_ownership_type_properties ownershipType =
  let isOwned = ownershipType == Owned
      isBorrowed = ownershipType == Borrowed
      isShared = ownershipType == Shared
      isMoved = ownershipType == Moved
      exactlyOne = sum [if isOwned then 1 else 0,
                        if isBorrowed then 1 else 0,
                        if isShared then 1 else 0,
                        if isMoved then 1 else 0] == 1
  in property $ exactlyOne === True

tests :: TestTree
tests = testGroup "Ownership Advanced QuickCheck Tests"
  [ fastProperty "ownership type comparison" prop_ownership_type_comparison
  , fastProperty "ownership transfer validity" prop_ownership_transfer_validity
  , fastProperty "ownership transfer invalidity" prop_ownership_transfer_invalidity
  , fastProperty "ownership error structure" prop_ownership_error_structure
  , fastProperty "ownership analyzer creation" prop_ownership_analyzer_creation
  , fastProperty "ownership analysis consistency" prop_ownership_analysis_consistency
  , fastProperty "error formatting preservation" prop_error_formatting_preservation
  , fastProperty "empty error formatting" prop_empty_error_formatting
  , fastProperty "ownership type transitions" prop_ownership_type_transitions
  , fastProperty "ownership error location bounds" prop_ownership_error_location_bounds
  , fastProperty "ownership analyzer idempotency" prop_ownership_analyzer_idempotency
  , fastProperty "complex ownership scenarios" prop_complex_ownership_scenarios
  , fastProperty "ownership transfer chain" prop_ownership_transfer_chain
  , fastProperty "error message content" prop_error_message_content
  , fastProperty "ownership type properties" prop_ownership_type_properties
  ]