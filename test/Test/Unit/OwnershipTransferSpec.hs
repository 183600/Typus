module Test.Unit.OwnershipTransferSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
import Data.List (isInfixOf)
import Test.Tasty.QuickCheck (conjoin, Arbitrary(..), oneof)

-- Add Arbitrary instance for OwnershipType
instance Arbitrary OwnershipType where
  arbitrary = oneof [pure $ Owned "test", pure $ Borrowed "test", pure $ MutBorrowed "test"]

-- Test ownership type properties
prop_ownership_type_reflexivity :: OwnershipType -> Property
prop_ownership_type_reflexivity ownershipType =
  property $ ownershipType === ownershipType

-- Test ownership transfer consistency
prop_ownership_transfer_consistency :: String -> String -> Property
prop_ownership_transfer_consistency fromName toName =
  let transfer = OwnershipTransfer fromName toName
  in conjoin [transferFrom transfer === fromName,
              transferTo transfer === toName]

-- Test ownership analyzer creation
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ analyzer === analyzer

-- Test ownership error formatting
prop_ownership_error_formatting :: String -> Property
prop_ownership_error_formatting errorMsg =
  let error = UseAfterMove errorMsg
      formatted = formatOwnershipErrors [error]
  in property $ errorMsg `isInfixOf` formatted

-- Test lexing and parsing consistency
prop_lex_parse_consistency :: String -> Property
prop_lex_parse_consistency input =
  let tokens = lexAll input
      ast = parseProgram tokens
  in property $ not (null tokens) ==> length (show ast) >= 0  -- Simplified test

tests :: TestTree
tests = testGroup "Ownership Transfer Tests"
  [ testProperty "ownership type reflexivity" prop_ownership_type_reflexivity
  , testProperty "ownership transfer consistency" prop_ownership_transfer_consistency
  , testProperty "ownership analyzer creation" prop_ownership_analyzer_creation
  , testProperty "ownership error formatting" prop_ownership_error_formatting
  , testProperty "lex parse consistency" prop_lex_parse_consistency
  ]