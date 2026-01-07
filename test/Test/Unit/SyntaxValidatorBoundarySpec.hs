module Test.Unit.SyntaxValidatorBoundarySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SyntaxValidator

-- Test validation boundary conditions
prop_validation_empty_input :: Property
prop_validation_empty_input =
  let result = validateSyntax ""
  in property $ not $ isValidValidation result

-- Test validation with whitespace only
prop_validation_whitespace_only :: String -> Property
prop_validation_whitespace_only s =
  let whitespaceOnly = all isSpace s
      result = validateSyntax s
  in property $ whitespaceOnly ==> not (isValidValidation result)

-- Test validation with valid syntax
prop_validation_valid_syntax_preserved :: String -> Property
prop_validation_valid_syntax_preserved validCode =
  let result = validateSyntax validCode
  in property $ isValidValidation result ==> getValidatedCode result === validCode

-- Test validation error reporting
prop_validation_error_reporting :: String -> Property
prop_validation_error_reporting invalidCode =
  let result = validateSyntax invalidCode
  in property $ not (isValidValidation result) ==> 
    not (null (getValidationErrors result))

-- Test validation idempotency
prop_validation_idempotent :: String -> Property
prop_validation_idempotent code =
  let result1 = validateSyntax code
      result2 = validateSyntax code
  in property $ result1 === result2

-- Helper functions
isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r\f\v"

tests :: TestTree
tests = testGroup "SyntaxValidator Boundary Tests"
  [ testProperty "validation empty input" prop_validation_empty_input
  , testProperty "validation whitespace only" prop_validation_whitespace_only
  , testProperty "validation valid syntax preserved" prop_validation_valid_syntax_preserved
  , testProperty "validation error reporting" prop_validation_error_reporting
  , testProperty "validation idempotent" prop_validation_idempotent
  ]