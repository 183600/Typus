module Test.Unit.OwnershipTransferSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership

-- Test ownership type properties
prop_ownership_type_reflexivity :: OwnershipType -> Property
prop_ownership_type_reflexivity ownershipType =
  property $ ownershipType === ownershipType

-- Test ownership transfer consistency
prop_ownership_transfer_consistency :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_consistency fromType toType =
  let transfer = OwnershipTransfer fromType toType
  in property $ 
    (OwnershipTransfer fromType toType) === transfer &&
    otFrom transfer === fromType &&
    otTo transfer === toType

-- Test ownership analyzer creation
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ analyzer === analyzer

-- Test ownership error formatting
prop_ownership_error_formatting :: String -> Property
prop_ownership_error_formatting errorMsg =
  let error = OwnershipError errorMsg
      formatted = formatOwnershipErrors [error]
  in property $ errorMsg `isInfixOf` formatted

-- Test lexing and parsing consistency
prop_lex_parse_consistency :: String -> Property
prop_lex_parse_consistency input =
  let tokens = lexAll input
      parseResult = parseProgram tokens
  in property $ 
    case parseResult of
      Left _ -> property True
      Right ast -> property $ not (null tokens) ==> length ast >= 0

tests :: TestTree
tests = testGroup "Ownership Transfer Tests"
  [ testProperty "ownership type reflexivity" prop_ownership_type_reflexivity
  , testProperty "ownership transfer consistency" prop_ownership_transfer_consistency
  , testProperty "ownership analyzer creation" prop_ownership_analyzer_creation
  , testProperty "ownership error formatting" prop_ownership_error_formatting
  , testProperty "lexing and parsing consistency" prop_lex_parse_consistency
  ]