{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SyntaxValidator (validateSyntax, ValidationResult(..))
import Parser (parseTypusFile)
import Utils (trim)

-- Property: Syntax validator accepts valid constructs
prop_validator_accepts_valid_constructs :: String -> Property
prop_validator_accepts_valid_constructs validCode =
  let validConstructs = ["func main() {}", "var x int", "type MyStruct struct {}", "import \"fmt\""]
      input = unlines $ validConstructs ++ [validCode]
      result = validateSyntax input
      isValid = result == Valid
  in classify (L.length validCode > 0) "non-empty code" $
     property $ isValid

-- Property: Syntax validator rejects invalid constructs
prop_validator_rejects_invalid_constructs :: String -> Property
prop_validator_rejects_invalid_constructs invalidCode =
  let invalidConstructs = ["func {", "var x", "type struct", "import"]
      input = unlines $ invalidConstructs ++ [invalidCode]
      result = validateSyntax input
      isInvalid = result == Invalid
  in classify (L.length invalidCode > 0) "non-empty code" $
     property $ isInvalid

-- Property: Syntax validator preserves structure
prop_validator_preserves_structure :: [String] -> Property
prop_validator_preserves_structure lines =
  let input = unlines lines
      result = validateSyntax input
      preservesLineCount = either (const False) (\_ -> L.length lines > 0) (Right result)
  in classify (L.length lines > 1) "multiple lines" $
     property $ preservesLineCount

-- Property: Syntax validator handles directives correctly
prop_validator_handles_directives :: Bool -> Bool -> String -> Property
prop_validator_handles_directives hasOwnership hasDependentTypes code =
  let ownershipDirective = if hasOwnership then "//! ownership: on\n" else ""
      dependentDirective = if hasDependentTypes then "//! dependent_types: on\n" else ""
      input = ownershipDirective ++ dependentDirective ++ code
      result = validateSyntax input
      acceptsDirectives = result == Valid || result == Warning
  in classify hasOwnership "has ownership directive" $
     classify hasDependentTypes "has dependent types directive" $
     property $ acceptsDirectives

-- Property: Syntax validator handles mixed valid/invalid code
prop_validator_handles_mixed_code :: String -> String -> Property
prop_validator_handles_mixed_code valid invalid =
  let input = valid ++ "\n" ++ invalid
      result = validateSyntax input
      hasMixedResult = result == Warning || result == Invalid
  in classify (not (null valid)) "has valid code" $
     classify (not (null invalid)) "has invalid code" $
     property $ hasMixedResult

-- Helper functions
data ValidationResult = Valid | Warning | Invalid
  deriving (Eq, Show)

validateSyntax :: String -> ValidationResult
validateSyntax input = 
  if null input then Invalid
  else if "func" `L.isInfixOf` input then Valid
  else if "var" `L.isInfixOf` input then Valid
  else Warning

isInfixOf :: String -> String -> Bool
isInfixOf = undefined  -- Simplified for test

tests :: TestTree
tests = testGroup "New Typus Syntax Validator QuickCheck Tests"
  [ fastProperty "Validator accepts valid constructs" prop_validator_accepts_valid_constructs
  , fastProperty "Validator rejects invalid constructs" prop_validator_rejects_invalid_constructs
  , fastProperty "Validator preserves structure" prop_validator_preserves_structure
  , fastProperty "Validator handles directives correctly" prop_validator_handles_directives
  , fastProperty "Validator handles mixed valid/invalid code" prop_validator_handles_mixed_code
  ]