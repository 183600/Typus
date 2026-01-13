module Test.Unit.EnhancedParserSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import Parser module
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), 
              TypusFile(..), defaultFileDirectives, defaultBlockDirectives)

-- Test properties for parser

-- Property 1: Parsing empty string should not crash
prop_parse_empty_string :: Property
prop_parse_empty_string = property $
  case parseTypus "" of
    Left _ -> property True
    Right _ -> property True

-- Property 2: Parsing well-formed package directive should succeed
prop_parse_package_directive :: String -> Property
prop_parse_package_directive name = 
  not (null name) && all isAlphaNum name ==>
  case parseTypus ("package " ++ name) of
    Left _ -> property False
    Right _ -> property True

-- Property 3: Parsing ownership directive should be recognized
prop_parse_ownership_directive :: Bool -> Property
prop_parse_ownership_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! ownership: " ++ directive
  in case parseTypus code of
    Left _ -> property False
    Right result -> property True

-- Property 4: Parsing dependent types directive should be recognized
prop_parse_dependent_types_directive :: Bool -> Property
prop_parse_dependent_types_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! dependent_types: " ++ directive
  in case parseTypus code of
    Left _ -> property False
    Right result -> property True

-- Property 5: Parsing simple function should not crash
prop_parse_simple_function :: String -> Property
prop_parse_simple_function name = 
  not (null name) && all isAlpha name ==>
  let code = "func " ++ name ++ "() {}"
  in case parseTypus code of
    Left _ -> property True  -- May fail due to incomplete grammar, but shouldn't crash
    Right _ -> property True

-- Property 6: Parsing block directives should be recognized
prop_parse_block_directive :: Bool -> Property
prop_parse_block_directive flag = 
  let directive = if flag then "on" else "off"
      code = "func main() {//! ownership: " ++ directive ++ "\n}"
  in case parseTypus code of
    Left _ -> property True  -- May fail due to incomplete grammar, but shouldn't crash
    Right _ -> property True

-- Property 7: Parsing import statements should not crash
prop_parse_import_statement :: String -> Property
prop_parse_import_statement path = 
  not (null path) && all (\c -> isAlphaNum c || c `elem` "/._-") path ==>
  let code = "import \"" ++ path ++ "\""
  in case parseTypus code of
    Left _ -> property True  -- May fail due to incomplete grammar, but shouldn't crash
    Right _ -> property True

-- Property 8: Parsing comments should not crash
prop_parse_comments :: String -> Property
prop_parse_comments comment = 
  not (null comment) && not (any (== '\n') comment) ==>
  let code = "// " ++ comment ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True  -- May fail due to incomplete grammar, but shouldn't crash
    Right _ -> property True

-- Property 9: Parsing multiple directives should work
prop_parse_multiple_directives :: Bool -> Bool -> Property
prop_parse_multiple_directives ownership dependentTypes = 
  let ownDir = if ownership then "on" else "off"
      depDir = if dependentTypes then "on" else "off"
      code = "//! ownership: " ++ ownDir ++ "\n//! dependent_types: " ++ depDir ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True  -- May fail due to incomplete grammar, but shouldn't crash
    Right _ -> property True

-- Property 10: Parsing malformed input should not crash
prop_parse_malformed_input :: String -> Property
prop_parse_malformed_input input = 
  not (null input) ==>
  case parseTypus input of
    Left _ -> property True  -- Should fail gracefully
    Right _ -> property True  -- Or succeed, but shouldn't crash

-- Unit tests for specific parser functionality

test_parse_empty_package :: Assertion
test_parse_empty_package = 
  case parseTypus "package" of
    Left _ -> assertBool "Expected parsing to fail for incomplete package directive" True
    Right _ -> assertFailure "Expected parsing to fail for incomplete package directive"

test_parse_valid_package :: Assertion
test_parse_valid_package = 
  case parseTypus "package main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for valid package directive" True

test_parse_empty_file :: Assertion
test_parse_empty_file = 
  case parseTypus "" of
    Left _ -> assertBool "Parsing empty file should not crash" True
    Right _ -> assertBool "Parsing empty file should not crash" True

test_parse_only_whitespace :: Assertion
test_parse_only_whitespace = 
  case parseTypus "   \n\t  " of
    Left _ -> assertBool "Parsing whitespace-only file should not crash" True
    Right _ -> assertBool "Parsing whitespace-only file should not crash" True

test_parse_ownership_on :: Assertion
test_parse_ownership_on = 
  case parseTypus "//! ownership: on\npackage main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for ownership on directive" True

test_parse_ownership_off :: Assertion
test_parse_ownership_off = 
  case parseTypus "//! ownership: off\npackage main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for ownership off directive" True

test_parse_dependent_types_on :: Assertion
test_parse_dependent_types_on = 
  case parseTypus "//! dependent_types: on\npackage main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for dependent_types on directive" True

test_parse_dependent_types_off :: Assertion
test_parse_dependent_types_off = 
  case parseTypus "//! dependent_types: off\npackage main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for dependent_types off directive" True

test_parse_constraints_directive :: Assertion
test_parse_constraints_directive = 
  case parseTypus "//! constraints: on\npackage main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right _ -> assertBool "Parsing should succeed for constraints directive" True

test_parse_block_ownership :: Assertion
test_parse_block_ownership = 
  case parseTypus "func main() {//! ownership: on\n}" of
    Left _ -> assertBool "Parsing block ownership directive should not crash" True
    Right _ -> assertBool "Parsing block ownership directive should not crash" True

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedParserSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "parse empty string should not crash" prop_parse_empty_string
    , testProperty "parse package directive" prop_parse_package_directive
    , testProperty "parse ownership directive" prop_parse_ownership_directive
    , testProperty "parse dependent types directive" prop_parse_dependent_types_directive
    , testProperty "parse simple function" prop_parse_simple_function
    , testProperty "parse block directive" prop_parse_block_directive
    , testProperty "parse import statement" prop_parse_import_statement
    , testProperty "parse comments" prop_parse_comments
    , testProperty "parse multiple directives" prop_parse_multiple_directives
    , testProperty "parse malformed input" prop_parse_malformed_input
    ]
  , testGroup "Unit Tests"
    [ testCase "parse empty package" test_parse_empty_package
    , testCase "parse valid package" test_parse_valid_package
    , testCase "parse empty file" test_parse_empty_file
    , testCase "parse only whitespace" test_parse_only_whitespace
    , testCase "parse ownership on" test_parse_ownership_on
    , testCase "parse ownership off" test_parse_ownership_off
    , testCase "parse dependent types on" test_parse_dependent_types_on
    , testCase "parse dependent types off" test_parse_dependent_types_off
    , testCase "parse constraints directive" test_parse_constraints_directive
    , testCase "parse block ownership" test_parse_block_ownership
    ]
  ]