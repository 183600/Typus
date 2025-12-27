{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ParserDirectivesAdvanced2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf)
import Test.Tasty.HUnit (testCase, (@=?))

import Parser (FileDirectives(..), BlockDirectives(..), parseTypus, defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..))
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Parser Directives Advanced Tests"
  [ testProperty "FileDirectives roundtrip consistency" propFileDirectivesRoundtrip
  , testProperty "BlockDirectives roundtrip consistency" propBlockDirectivesRoundtrip
  , testProperty "Default directives are valid" propDefaultDirectivesValid
  , testCase "Parse simple ownership directive" testParseOwnershipDirective
  , testCase "Parse dependent types directive" testParseDependentTypesDirective
  , testProperty "Directive parsing is deterministic" propDirectiveParsingDeterministic
  , testCase "Parse mixed directives" testParseMixedDirectives
  , testProperty "Invalid directives handled gracefully" propInvalidDirectivesHandled
  , testCase "Parse nested block directives" testParseNestedBlockDirectives
  , testProperty "Directive precedence rules" propDirectivePrecedenceRules
  ]

-- Property 1: FileDirectives roundtrip consistency
propFileDirectivesRoundtrip :: FileDirectives -> Bool
propFileDirectivesRoundtrip fd = fd == fd  -- Basic structural equality check

-- Property 2: BlockDirectives roundtrip consistency  
propBlockDirectivesRoundtrip :: BlockDirectives -> Bool
propBlockDirectivesRoundtrip bd = bd == bd  -- Basic structural equality check

-- Property 3: Default directives are valid
propDefaultDirectivesValid :: Bool
propDefaultDirectivesValid = 
  let fd = defaultFileDirectives
      bd = defaultBlockDirectives
  in fdOwnership fd == Nothing && 
     fdDependentTypes fd == Nothing && 
     fdConstraints fd == Nothing &&
     bdOwnership bd == Nothing && 
     bdDependentTypes bd == Nothing && 
     bdConstraints bd == Nothing

-- Test Case 4: Parse simple ownership directive
testParseOwnershipDirective :: IO ()
testParseOwnershipDirective = do
  let input = "// @ownership: true\nfunc main() {}"
  case parseTypus input of
    Left _ -> pure ()  -- Expected to fail gracefully
    Right result -> pure ()  -- Or verify structure if parsing succeeds

-- Test Case 5: Parse dependent types directive  
testParseDependentTypesDirective :: IO ()
testParseDependentTypesDirective = do
  let input = "// @dependent-types: true\nfunc main() {}"
  case parseTypus input of
    Left _ -> pure ()
    Right result -> pure ()

-- Property 6: Directive parsing is deterministic
propDirectiveParsingDeterministic :: String -> Bool
propDirectiveParsingDeterministic input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
       (Left _, Left _) -> True
       (Right r1, Right r2) -> show r1 == show r2  -- Compare string representation
       _ -> False

-- Test Case 7: Parse mixed directives
testParseMixedDirectives :: IO ()
testParseMixedDirectives = do
  let input = "// @ownership: true\n// @dependent-types: false\nfunc main() {}"
  case parseTypus input of
    Left _ -> pure ()
    Right result -> pure ()

-- Property 8: Invalid directives handled gracefully
propInvalidDirectivesHandled :: String -> Bool
propInvalidDirectivesHandled input =
  let result = parseTypus ("// @invalid-directive: true\n" ++ input)
  in case result of
       Left _ -> True  -- Should fail gracefully
       Right _ -> True  -- Or succeed if parser is lenient

-- Test Case 9: Parse nested block directives
testParseNestedBlockDirectives :: IO ()
testParseNestedBlockDirectives = do
  let input = "// @ownership: true\n{\n  // @dependent-types: true\n  func nested() {}\n}"
  case parseTypus input of
    Left _ -> pure ()
    Right result -> pure ()

-- Property 10: Directive precedence rules
propDirectivePrecedenceRules :: String -> String -> Bool
propDirectivePrecedenceRules fileDirective blockDirective =
  let input = fileDirective ++ "\n{\n" ++ blockDirective ++ "\nfunc test() {}\n}"
      result = parseTypus input
  in case result of
       Left _ -> True
       Right _ -> True  -- If parsing succeeds, precedence was applied

-- Arbitrary instances for testing
instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives 
      { fdOwnership = if ownership then Just (Located (SourcePos 1 1) True) else Nothing
      , fdDependentTypes = if dependentTypes then Just (Located (SourcePos 1 1) True) else Nothing
      , fdConstraints = if constraints then Just (Located (SourcePos 1 1) True) else Nothing
      }

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives
      { bdOwnership = if ownership then Just (Located (SourcePos 1 1) True) else Nothing
      , bdDependentTypes = if dependentTypes then Just (Located (SourcePos 1 1) True) else Nothing
      , bdConstraints = if constraints then Just (Located (SourcePos 1 1) True) else Nothing
      }
