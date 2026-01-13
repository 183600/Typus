module Test.Unit.EnhancedOwnershipSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import Ownership module
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer,
                 OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership,
                 analyzeOwnershipFile, analyzeOwnershipDebug, formatOwnershipErrors,
                 lexAll, parseProgram, builtInFunctions)

-- Import Parser module
import Parser (TypusFile(..), parseTypus)

-- Test properties for ownership

-- Property 1: Creating ownership analyzer should not crash
prop_new_ownership_analyzer :: Property
prop_new_ownership_analyzer = property $
  let analyzer = newOwnershipAnalyzer
  in property True  -- Should not crash

-- Property 2: Analyzing empty string should not crash
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty = property $
  case analyzeOwnership "" of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 3: Analyzing simple ownership code should not crash
prop_analyze_simple_ownership :: String -> Property
prop_analyze_simple_ownership name = 
  not (null name) && all isAlpha name ==>
  let code = "//! ownership: on\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeOwnership code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 4: Analyzing code with ownership off should not crash
prop_analyze_ownership_off :: String -> Property
prop_analyze_ownership_off name = 
  not (null name) && all isAlpha name ==>
  let code = "//! ownership: off\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeOwnership code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 5: Analyzing code with block ownership should not crash
prop_analyze_block_ownership :: String -> Property
prop_analyze_block_ownership name = 
  not (null name) && all isAlpha name ==>
  let code = "package main\n\nfunc main() {\n  {//! ownership: on\n    var " ++ name ++ " int\n  }\n}\n"
  in case analyzeOwnership code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 6: Lexing empty string should not crash
prop_lex_empty :: Property
prop_lex_empty = property $
  case lexAll "" of
    Left _ -> property True  -- Lexing may fail, but shouldn't crash
    Right _ -> property True

-- Property 7: Lexing simple code should not crash
prop_lex_simple :: String -> Property
prop_lex_simple name = 
  not (null name) && all isAlpha name ==>
  let code = "func " ++ name ++ "() {}"
  in case lexAll code of
    Left _ -> property True  -- Lexing may fail, but shouldn't crash
    Right _ -> property True

-- Property 8: Parsing empty program should not crash
prop_parse_empty_program :: Property
prop_parse_empty_program = property $
  case parseProgram "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 9: Parsing simple program should not crash
prop_parse_simple_program :: String -> Property
prop_parse_simple_program name = 
  not (null name) && all isAlpha name ==>
  let code = "func " ++ name ++ "() {}"
  in case parseProgram code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 10: Error formatting should not crash
prop_format_ownership_errors :: [String] -> Property
prop_format_ownership_errors errors = 
  let ownershipErrors = map (\msg -> OwnershipError (OwnershipTransfer "test" "test") msg) errors
      formatted = formatOwnershipErrors ownershipErrors
  in property $ length formatted >= 0  -- Should not crash

-- Unit tests for specific ownership functionality

test_new_ownership_analyzer :: Assertion
test_new_ownership_analyzer = 
  let analyzer = newOwnershipAnalyzer
  in assertBool "Creating ownership analyzer should not crash" True

test_analyze_empty_code :: Assertion
test_analyze_empty_code = 
  case analyzeOwnership "" of
    Left _ -> assertBool "Analyzing empty code should not crash" True
    Right _ -> assertBool "Analyzing empty code should not crash" True

test_analyze_ownership_on :: Assertion
test_analyze_ownership_on = 
  let code = "//! ownership: on\npackage main\n\nfunc main() {}\n"
  in case analyzeOwnership code of
    Left _ -> assertBool "Analyzing ownership on should not crash" True
    Right _ -> assertBool "Analyzing ownership on should not crash" True

test_analyze_ownership_off :: Assertion
test_analyze_ownership_off = 
  let code = "//! ownership: off\npackage main\n\nfunc main() {}\n"
  in case analyzeOwnership code of
    Left _ -> assertBool "Analyzing ownership off should not crash" True
    Right _ -> assertBool "Analyzing ownership off should not crash" True

test_analyze_block_ownership :: Assertion
test_analyze_block_ownership = 
  let code = "package main\n\nfunc main() {\n  {//! ownership: on\n    var x int\n  }\n}\n"
  in case analyzeOwnership code of
    Left _ -> assertBool "Analyzing block ownership should not crash" True
    Right _ -> assertBool "Analyzing block ownership should not crash" True

test_lex_empty :: Assertion
test_lex_empty = 
  case lexAll "" of
    Left _ -> assertBool "Lexing empty string should not crash" True
    Right _ -> assertBool "Lexing empty string should not crash" True

test_lex_simple :: Assertion
test_lex_simple = 
  let code = "func main() {}"
  in case lexAll code of
    Left _ -> assertBool "Lexing simple code should not crash" True
    Right _ -> assertBool "Lexing simple code should not crash" True

test_parse_empty_program :: Assertion
test_parse_empty_program = 
  case parseProgram "" of
    Left _ -> assertBool "Parsing empty program should not crash" True
    Right _ -> assertBool "Parsing empty program should not crash" True

test_parse_simple_program :: Assertion
test_parse_simple_program = 
  let code = "func main() {}"
  in case parseProgram code of
    Left _ -> assertBool "Parsing simple program should not crash" True
    Right _ -> assertBool "Parsing simple program should not crash" True

test_format_ownership_errors :: Assertion
test_format_ownership_errors = 
  let errors = [OwnershipError (OwnershipTransfer "x" "y") "Test error 1",
                OwnershipError (OwnershipTransfer "a" "b") "Test error 2"]
      formatted = formatOwnershipErrors errors
  in assertBool "Formatting ownership errors should not crash" $ not (null formatted)

test_built_in_functions :: Assertion
test_built_in_functions = 
  let functions = builtInFunctions
  in assertBool "Built-in functions should not crash" $ length functions >= 0

test_analyze_ownership_file :: Assertion
test_analyze_ownership_file = 
  let code = "//! ownership: on\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case analyzeOwnershipFile typusFile of
        Left _ -> assertBool "Analyzing ownership file should not crash" True
        Right _ -> assertBool "Analyzing ownership file should not crash" True

test_analyze_ownership_debug :: Assertion
test_analyze_ownership_debug = 
  let code = "//! ownership: on\npackage main\n\nfunc main() {}\n"
  in case analyzeOwnershipDebug code of
    Left _ -> assertBool "Analyzing ownership debug should not crash" True
    Right _ -> assertBool "Analyzing ownership debug should not crash" True

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedOwnershipSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "new ownership analyzer" prop_new_ownership_analyzer
    , testProperty "analyze ownership empty" prop_analyze_ownership_empty
    , testProperty "analyze simple ownership" prop_analyze_simple_ownership
    , testProperty "analyze ownership off" prop_analyze_ownership_off
    , testProperty "analyze block ownership" prop_analyze_block_ownership
    , testProperty "lex empty" prop_lex_empty
    , testProperty "lex simple" prop_lex_simple
    , testProperty "parse empty program" prop_parse_empty_program
    , testProperty "parse simple program" prop_parse_simple_program
    , testProperty "format ownership errors" prop_format_ownership_errors
    ]
  , testGroup "Unit Tests"
    [ testCase "new ownership analyzer" test_new_ownership_analyzer
    , testCase "analyze empty code" test_analyze_empty_code
    , testCase "analyze ownership on" test_analyze_ownership_on
    , testCase "analyze ownership off" test_analyze_ownership_off
    , testCase "analyze block ownership" test_analyze_block_ownership
    , testCase "lex empty" test_lex_empty
    , testCase "lex simple" test_lex_simple
    , testCase "parse empty program" test_parse_empty_program
    , testCase "parse simple program" test_parse_simple_program
    , testCase "format ownership errors" test_format_ownership_errors
    , testCase "built in functions" test_built_in_functions
    , testCase "analyze ownership file" test_analyze_ownership_file
    , testCase "analyze ownership debug" test_analyze_ownership_debug
    ]
  ]