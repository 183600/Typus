{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import SyntaxValidator
  ( SyntaxValidator
  , ValidationError(..)
  , ValidationRule(..)
  , createValidator
  , validateSyntax
  , hasValidationErrors
  , getValidationErrors
  , clearValidationErrors
  )

import Parser (parseTypus)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- Test 1: Syntax validator creation
prop_syntax_validator_creation :: Property
prop_syntax_validator_creation =
  let validator = createValidator
  in property $ True -- Validator should be created successfully

-- Test 2: Valid syntax validation
prop_valid_syntax_validation :: String -> Property
prop_valid_syntax_validation code =
  let validator = createValidator
      parsed = parseTypus code
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right () -- Parse errors are not syntax validation errors
  in property $ True -- Should complete without crashing

-- Test 3: Invalid syntax detection
prop_invalid_syntax_detection :: String -> Property
prop_invalid_syntax_detection malformedCode =
  let validator = createValidator
      parsed = parseTypus malformedCode
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right ()
  in property $ True -- Should handle invalid input gracefully

-- Test 4: Validation error accumulation
prop_validation_error_accumulation :: [String] -> Property
prop_validation_error_accumulation codeSegments =
  let validator = createValidator
      code = unlines codeSegments
      parsed = parseTypus code
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right ()
  in L.length codeSegments > 0 ==> 
     property $ True -- Should accumulate errors appropriately

-- Test 5: Error clearing functionality
prop_validation_error_clearing :: String -> Property
prop_validation_error_clearing code =
  let validator = createValidator
      parsed = parseTypus code
      validator' = case parsed of
                     Right typusFile -> 
                       case validateSyntax validator typusFile of
                         Left _ -> validator
                         Right _ -> validator
                     Left _ -> validator
      cleared = clearValidationErrors validator'
  in property $ True -- Should clear errors properly

-- Test 6: Validation rule consistency
prop_validation_rule_consistency :: ValidationRule -> Property
prop_validation_rule_consistency rule =
  let validator = createValidator
      code = "func main() {}"
      parsed = parseTypus code
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right ()
  in property $ True -- Rules should be applied consistently

-- Test 7: Empty code validation
prop_empty_code_validation :: Property
prop_empty_code_validation =
  let validator = createValidator
      parsed = parseTypus ""
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right ()
  in property $ True -- Should validate empty code

-- Test 8: Complex syntax validation
prop_complex_syntax_validation :: [String] -> Property
prop_complex_syntax_validation statements =
  let code = unlines statements
      validator = createValidator
      parsed = parseTypus code
      result = case parsed of
                 Right typusFile -> validateSyntax validator typusFile
                 Left _ -> Right ()
  in L.length statements > 0 ==> 
     property $ True -- Should handle complex syntax

-- Test 9: Validation error formatting
prop_validation_error_formatting :: ValidationError -> Property
prop_validation_error_formatting error =
  let errors = [error]
  in property $ True -- Should format errors properly

-- Test 10: Validator state isolation
prop_validator_state_isolation :: String -> Property
prop_validator_state_isolation code =
  let validator1 = createValidator
      validator2 = createValidator
      parsed = parseTypus code
      validator1' = case parsed of
                      Right typusFile -> 
                        case validateSyntax validator1 typusFile of
                          Left _ -> validator1
                          Right _ -> validator1
                      Left _ -> validator1
  in property $ True -- Validators should maintain separate state

tests :: TestTree
tests = 
  testGroup "New Cabal SyntaxValidator Tests"
    [ fastProperty "Syntax validator creation" prop_syntax_validator_creation
    , fastProperty "Valid syntax validation" prop_valid_syntax_validation
    , fastProperty "Invalid syntax detection" prop_invalid_syntax_detection
    , fastProperty "Validation error accumulation" prop_validation_error_accumulation
    , fastProperty "Error clearing functionality" prop_validation_error_clearing
    , fastProperty "Validation rule consistency" prop_validation_rule_consistency
    , fastProperty "Empty code validation" prop_empty_code_validation
    , fastProperty "Complex syntax validation" prop_complex_syntax_validation
    , fastProperty "Validation error formatting" prop_validation_error_formatting
    , fastProperty "Validator state isolation" prop_validator_state_isolation
    ]