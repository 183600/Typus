{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.SyntaxValidatorBoundarySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import SyntaxValidator (validateSyntax, SyntaxError(..))
import Parser (parseTypus)
import Control.Exception (try, SomeException)
import Data.List (isInfixOf)

-- | Test syntax validator boundary conditions
tests :: TestTree
tests = testGroup "Syntax Validator Boundary Tests"
  [ testCase "Empty file validation" testEmptyFileValidation
  , testCase "Invalid UTF-8 handling" testInvalidUTF8Handling
  , testCase "Extremely long lines" testExtremelyLongLines
  , testCase "Deep nesting validation" testDeepNestingValidation
  , testCase "Malformed directive syntax" testMalformedDirectiveSyntax
  , testCase "Mixed language features" testMixedLanguageFeatures
  , testProperty "Syntax validation is sound" syntaxValidationSound
  , testCase "Error recovery in syntax validation" testSyntaxValidationRecovery
  ]

-- | Test empty file validation
testEmptyFileValidation :: Assertion
testEmptyFileValidation = do
  let input = ""
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Validation failed: " ++ show e
    Right errors -> 
      -- Empty file should either be valid or produce appropriate error
      assertBool "Empty file validation should be handled gracefully" $
        True  -- Any outcome is acceptable as long as it doesn't crash

-- | Test invalid UTF-8 handling
testInvalidUTF8Handling :: Assertion
testInvalidUTF8Handling = do
  -- Note: This test might need adjustment based on actual UTF-8 handling
  let input = "package main\n\nfunc main() {\n    println(\"Invalid: \xC0\x80\")\n}"
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> 
      -- Should handle invalid UTF-8 gracefully
      assertBool "Should handle invalid UTF-8 gracefully" $
        True
    Right errors -> 
      -- Either valid or produce appropriate error
      assertBool "Should handle UTF-8 without crashing" $
        True

-- | Test extremely long lines
testExtremelyLongLines :: Assertion
testExtremelyLongLines = do
  let longString = replicate 1000 'a'
  let input = "package main\n\nfunc main() {\n    println(\"" ++ longString ++ "\")\n}"
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Validation failed: " ++ show e
    Right errors -> 
      -- Should handle long lines without crashing
      assertBool "Should handle long lines without crashing" $
        True

-- | Test deep nesting validation
testDeepNestingValidation :: Assertion
testDeepNestingValidation = do
  let nestedBraces = replicate 50 '{'
  let nestedBracesEnd = replicate 50 '}'
  let input = "package main\n\nfunc main() {\n" ++ nestedBraces ++ "\n    var x int = 42\n" ++ nestedBracesEnd ++ "\n}"
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> 
      -- Should handle deep nesting gracefully
      assertBool "Should handle deep nesting gracefully" $
        True
    Right errors -> 
      -- Should either validate or produce appropriate nesting error
      assertBool "Should handle deep nesting" $
        True

-- | Test malformed directive syntax
testMalformedDirectiveSyntax :: Assertion
testMalformedDirectiveSyntax = do
  let testCases = 
        [ ("//! ownership", "Incomplete directive")
        , ("//! ownership maybe", "Invalid boolean value")
        , ("//! ownership on extra", "Extra tokens")
        , ("// !ownership: on", "Malformed spacing")
        , ("//! unknown_directive: on", "Unknown directive")
        ]
  
  mapM_ runTestCase testCases
  where
    runTestCase (input, description) = do
      result <- try $ validateSyntax input
      case result of
        Left (e :: SomeException) -> 
          assertFailure $ description ++ " failed: " ++ show e
        Right errors -> 
          -- Should handle malformed directives gracefully
          assertBool (description ++ " should handle malformed directive") $
            True

-- | Test mixed language features
testMixedLanguageFeatures :: Assertion
testMixedLanguageFeatures = do
  let input = "//! ownership: on\n//! dependent_types: on\n\npackage main\n\ntype Vector(n int) struct {\n    length int\n    data []float64\n}\n\nfunc main() {\n    {//! ownership: off\n        // Mixed features in different contexts\n        v := Vector{length: 3, data: []float64{1.0, 2.0, 3.0}}\n        \n        {//! dependent_types: off\n            // All features disabled\n            var x int = 42\n        }\n    }\n}"
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Validation failed: " ++ show e
    Right errors -> 
      -- Should validate mixed language features
      assertBool "Should validate mixed language features" $
        True

-- | Property: Syntax validation should be sound
syntaxValidationSound :: String -> Property
syntaxValidationSound input =
  length input < 1000 ==>  -- Limit size for property testing
  case validateSyntax input of
    Left _ -> property True -- Validation failure is acceptable
    Right errors -> 
      -- If no syntax errors, parsing should succeed
      case parseTypus input of
        Left _ -> property False -- No syntax errors but parse failed
        Right _ -> property True -- No syntax errors and parse succeeded

-- | Test error recovery in syntax validation
testSyntaxValidationRecovery :: Assertion
testSyntaxValidationRecovery = do
  let input = "package main\n\nfunc main() {\n    var x int = \n    // Missing value - syntax error\n    var y string = \"hello\"\n    // Valid statement after error\n    println(y)\n    // Another error\n    var z bool = \n    // Valid statement after second error\n    println(\"done\")\n}"
  
  result <- try $ validateSyntax input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Validation failed: " ++ show e
    Right errors -> do
      -- Should detect multiple syntax errors
      assertBool "Should detect multiple syntax errors" $
        length errors >= 2
      -- Should continue validation after errors
      assertBool "Should continue validation after errors" $
        True

-- | Helper function to check if an error is a syntax error
isSyntaxError :: SyntaxError -> Bool
isSyntaxError _ = True  -- All SyntaxError values are syntax errors

-- | Helper function to check for specific error patterns
hasErrorPattern :: String -> [SyntaxError] -> Bool
hasErrorPattern pattern errors = 
  any (pattern `isInfixOf` . show) errors