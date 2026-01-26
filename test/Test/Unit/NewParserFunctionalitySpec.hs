{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.NewParserFunctionalitySpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (void)

-- Import Parser module
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), 
              TypusFile(..), defaultFileDirectives, defaultBlockDirectives)

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 1: Parsing empty string should not crash
prop_parse_empty_string :: Property
prop_parse_empty_string = property $
  case parseTypus "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 2: Parsing simple package directive should not crash
prop_parse_simple_package :: String -> Property
prop_parse_simple_package name = 
  not (null name) && all isAlphaNum name ==>
  case parseTypus ("package " ++ name) of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 3: Parsing with ownership directive should not crash
prop_parse_ownership_directive :: Bool -> Property
prop_parse_ownership_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! ownership: " ++ directive ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 4: Parsing with dependent types directive should not crash
prop_parse_dependent_types_directive :: Bool -> Property
prop_parse_dependent_types_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! dependent_types: " ++ directive ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 5: Parsing simple function should not crash
prop_parse_simple_function :: String -> Property
prop_parse_simple_function name = 
  not (null name) && all isAlpha name ==>
  let code = "package main\n\nfunc " ++ name ++ "() {}\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 6: Parsing with imports should not crash
prop_parse_imports :: String -> Property
prop_parse_imports path = 
  not (null path) && all (\c -> isAlphaNum c || c `elem` "/._-") path ==>
  let code = "package main\n\nimport \"" ++ path ++ "\"\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 7: Parsing with multiple directives should not crash
prop_parse_multiple_directives :: Bool -> Bool -> Property
prop_parse_multiple_directives ownership dependentTypes = 
  let ownDir = if ownership then "on" else "off"
      depDir = if dependentTypes then "on" else "off"
      code = "//! ownership: " ++ ownDir ++ "\n//! dependent_types: " ++ depDir ++ "\npackage main\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 8: Parsing with comments should not crash
prop_parse_with_comments :: String -> Property
prop_parse_with_comments comment = 
  not (null comment) && not (any (== '\n') comment) ==> -- Ensure single line comment
  let code = "package main\n// " ++ comment ++ "\nfunc main() {}\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 9: Parsing with block comments should not crash
prop_parse_with_block_comments :: String -> Property
prop_parse_with_block_comments comment = 
  not (null comment) && not (any (== '/') comment) ==> -- Avoid comment end markers
  let code = "package main\n/* " ++ comment ++ " */\nfunc main() {}\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property 10: Parsing with variables should not crash
prop_parse_with_variables :: String -> String -> Property
prop_parse_with_variables varName varType = 
  not (null varName) && all isAlpha varName &&
  not (null varType) && all isAlphaNum varType ==>
  let code = "package main\n\nvar " ++ varName ++ " " ++ varType ++ "\n"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_parse_empty_file :: Assertion
test_parse_empty_file = 
  case parseTypus "" of
    Left _ -> assertBool "Parsing empty file should not crash" True
    Right _ -> assertBool "Parsing empty file should not crash" True

test_parse_simple_package :: Assertion
test_parse_simple_package = 
  case parseTypus "package main" of
    Left err -> assertFailure $ "Parsing simple package failed: " ++ show err
    Right _ -> assertBool "Parsing simple package should succeed" True

test_parse_with_ownership_on :: Assertion
test_parse_with_ownership_on = 
  let code = "//! ownership: on\npackage main\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with ownership on failed: " ++ show err
    Right _ -> assertBool "Parsing with ownership on should succeed" True

test_parse_with_ownership_off :: Assertion
test_parse_with_ownership_off = 
  let code = "//! ownership: off\npackage main\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with ownership off failed: " ++ show err
    Right _ -> assertBool "Parsing with ownership off should succeed" True

test_parse_with_dependent_types_on :: Assertion
test_parse_with_dependent_types_on = 
  let code = "//! dependent_types: on\npackage main\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with dependent types on failed: " ++ show err
    Right _ -> assertBool "Parsing with dependent types on should succeed" True

test_parse_with_dependent_types_off :: Assertion
test_parse_with_dependent_types_off = 
  let code = "//! dependent_types: off\npackage main\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with dependent types off failed: " ++ show err
    Right _ -> assertBool "Parsing with dependent types off should succeed" True

test_parse_simple_function :: Assertion
test_parse_simple_function = 
  let code = "package main\n\nfunc hello() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing simple function failed: " ++ show err
    Right _ -> assertBool "Parsing simple function should succeed" True

test_parse_function_with_params :: Assertion
test_parse_function_with_params = 
  let code = "package main\n\nfunc add(x int, y int) int {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing function with params failed: " ++ show err
    Right _ -> assertBool "Parsing function with params should succeed" True

test_parse_with_import :: Assertion
test_parse_with_import = 
  let code = "package main\n\nimport \"fmt\"\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with import failed: " ++ show err
    Right _ -> assertBool "Parsing with import should succeed" True

test_parse_with_multiple_imports :: Assertion
test_parse_with_multiple_imports = 
  let code = "package main\n\nimport \"fmt\"\nimport \"os\"\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with multiple imports failed: " ++ show err
    Right _ -> assertBool "Parsing with multiple imports should succeed" True

test_parse_with_variable :: Assertion
test_parse_with_variable = 
  let code = "package main\n\nvar x int\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with variable failed: " ++ show err
    Right _ -> assertBool "Parsing with variable should succeed" True

test_parse_with_line_comment :: Assertion
test_parse_with_line_comment = 
  let code = "package main\n// This is a comment\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing with line comment failed: " ++ show err
    Right _ -> assertBool "Parsing with line comment should succeed" True

tests :: TestTree
tests = testGroup "Test.Unit.NewParserFunctionalitySpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "parse empty string should not crash" prop_parse_empty_string
    , testProperty "parse simple package" prop_parse_simple_package
    , testProperty "parse ownership directive" prop_parse_ownership_directive
    , testProperty "parse dependent types directive" prop_parse_dependent_types_directive
    , testProperty "parse simple function" prop_parse_simple_function
    , testProperty "parse imports" prop_parse_imports
    , testProperty "parse multiple directives" prop_parse_multiple_directives
    , testProperty "parse with comments" prop_parse_with_comments
    , testProperty "parse with block comments" prop_parse_with_block_comments
    , testProperty "parse with variables" prop_parse_with_variables
    ]
  , testGroup "Unit Tests"
    [ testCase "parse empty file" test_parse_empty_file
    , testCase "parse simple package" test_parse_simple_package
    , testCase "parse with ownership on" test_parse_with_ownership_on
    , testCase "parse with ownership off" test_parse_with_ownership_off
    , testCase "parse with dependent types on" test_parse_with_dependent_types_on
    , testCase "parse with dependent types off" test_parse_with_dependent_types_off
    , testCase "parse simple function" test_parse_simple_function
    , testCase "parse function with params" test_parse_function_with_params
    , testCase "parse with import" test_parse_with_import
    , testCase "parse with multiple imports" test_parse_with_multiple_imports
    , testCase "parse with variable" test_parse_with_variable
    , testCase "parse with line comment" test_parse_with_line_comment
    ]
  ]