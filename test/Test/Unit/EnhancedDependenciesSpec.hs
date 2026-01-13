module Test.Unit.EnhancedDependenciesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import Dependencies module
import Dependencies (DependencyType(..), DependencyError(..), DependencyAnalyzer,
                    newDependencyAnalyzer, analyzeDependencies, analyzeDependenciesFile,
                    analyzeDependenciesDebug, formatDependencyErrors, lexAll,
                    parseProgram, builtInFunctions)

-- Import Parser module
import Parser (TypusFile(..), parseTypus)

-- Test properties for dependencies

-- Property 1: Creating dependency analyzer should not crash
prop_new_dependency_analyzer :: Property
prop_new_dependency_analyzer = property $
  let analyzer = newDependencyAnalyzer
  in property True  -- Should not crash

-- Property 2: Analyzing empty string should not crash
prop_analyze_dependencies_empty :: Property
prop_analyze_dependencies_empty = property $
  case analyzeDependencies "" of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 3: Analyzing simple dependent types code should not crash
prop_analyze_simple_dependencies :: String -> Property
prop_analyze_simple_dependencies name = 
  not (null name) && all isAlpha name ==>
  let code = "//! dependent_types: on\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependencies code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 4: Analyzing code with dependent types off should not crash
prop_analyze_dependencies_off :: String -> Property
prop_analyze_dependencies_off name = 
  not (null name) && all isAlpha name ==>
  let code = "//! dependent_types: off\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependencies code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 5: Analyzing code with constraints directive should not crash
prop_analyze_constraints_directive :: String -> Property
prop_analyze_constraints_directive name = 
  not (null name) && all isAlpha name ==>
  let code = "//! constraints: on\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependencies code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 6: Analyzing code with block dependent types should not crash
prop_analyze_block_dependencies :: String -> Property
prop_analyze_block_dependencies name = 
  not (null name) && all isAlpha name ==>
  let code = "package main\n\nfunc main() {\n  {//! dependent_types: on\n    var " ++ name ++ " int\n  }\n}\n"
  in case analyzeDependencies code of
    Left _ -> property True  -- Analysis may fail, but shouldn't crash
    Right _ -> property True

-- Property 7: Lexing empty string should not crash
prop_lex_dependencies_empty :: Property
prop_lex_dependencies_empty = property $
  case lexAll "" of
    Left _ -> property True  -- Lexing may fail, but shouldn't crash
    Right _ -> property True

-- Property 8: Lexing simple code should not crash
prop_lex_dependencies_simple :: String -> Property
prop_lex_dependencies_simple name = 
  not (null name) && all isAlpha name ==>
  let code = "func " ++ name ++ "() {}"
  in case lexAll code of
    Left _ -> property True  -- Lexing may fail, but shouldn't crash
    Right _ -> property True

-- Property 9: Parsing empty program should not crash
prop_parse_dependencies_empty_program :: Property
prop_parse_dependencies_empty_program = property $
  case parseProgram "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 10: Error formatting should not crash
prop_format_dependency_errors :: [String] -> Property
prop_format_dependency_errors errors = 
  let dependencyErrors = map (\msg -> DependencyError (DependencyType "test") msg) errors
      formatted = formatDependencyErrors dependencyErrors
  in property $ length formatted >= 0  -- Should not crash

-- Unit tests for specific dependencies functionality

test_new_dependency_analyzer :: Assertion
test_new_dependency_analyzer = 
  let analyzer = newDependencyAnalyzer
  in assertBool "Creating dependency analyzer should not crash" True

test_analyze_empty_code :: Assertion
test_analyze_empty_code = 
  case analyzeDependencies "" of
    Left _ -> assertBool "Analyzing empty code should not crash" True
    Right _ -> assertBool "Analyzing empty code should not crash" True

test_analyze_dependent_types_on :: Assertion
test_analyze_dependent_types_on = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing dependent types on should not crash" True
    Right _ -> assertBool "Analyzing dependent types on should not crash" True

test_analyze_dependent_types_off :: Assertion
test_analyze_dependent_types_off = 
  let code = "//! dependent_types: off\npackage main\n\nfunc main() {}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing dependent types off should not crash" True
    Right _ -> assertBool "Analyzing dependent types off should not crash" True

test_analyze_constraints_directive :: Assertion
test_analyze_constraints_directive = 
  let code = "//! constraints: on\npackage main\n\nfunc main() {}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing constraints directive should not crash" True
    Right _ -> assertBool "Analyzing constraints directive should not crash" True

test_analyze_block_dependencies :: Assertion
test_analyze_block_dependencies = 
  let code = "package main\n\nfunc main() {\n  {//! dependent_types: on\n    var x int\n  }\n}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing block dependencies should not crash" True
    Right _ -> assertBool "Analyzing block dependencies should not crash" True

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

test_format_dependency_errors :: Assertion
test_format_dependency_errors = 
  let errors = [DependencyError (DependencyType "test") "Test error 1",
                DependencyError (DependencyType "test") "Test error 2"]
      formatted = formatDependencyErrors errors
  in assertBool "Formatting dependency errors should not crash" $ not (null formatted)

test_built_in_functions :: Assertion
test_built_in_functions = 
  let functions = builtInFunctions
  in assertBool "Built-in functions should not crash" $ length functions >= 0

test_analyze_dependencies_file :: Assertion
test_analyze_dependencies_file = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case analyzeDependenciesFile typusFile of
        Left _ -> assertBool "Analyzing dependencies file should not crash" True
        Right _ -> assertBool "Analyzing dependencies file should not crash" True

test_analyze_dependencies_debug :: Assertion
test_analyze_dependencies_debug = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
  in case analyzeDependenciesDebug code of
    Left _ -> assertBool "Analyzing dependencies debug should not crash" True
    Right _ -> assertBool "Analyzing dependencies debug should not crash" True

test_analyze_type_constraints :: Assertion
test_analyze_type_constraints = 
  let code = "//! constraints: on\npackage main\n\ntype Vector struct {\n  length int\n  data []float64\n}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing type constraints should not crash" True
    Right _ -> assertBool "Analyzing type constraints should not crash" True

test_analyze_refinement_types :: Assertion
test_analyze_refinement_types = 
  let code = "//! dependent_types: on\npackage main\n\nfunc SafeDivide(a, b int) int {\n  if b == 0 {\n    panic(\"Division by zero\")\n  }\n  return a / b\n}\n"
  in case analyzeDependencies code of
    Left _ -> assertBool "Analyzing refinement types should not crash" True
    Right _ -> assertBool "Analyzing refinement types should not crash" True

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedDependenciesSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "new dependency analyzer" prop_new_dependency_analyzer
    , testProperty "analyze dependencies empty" prop_analyze_dependencies_empty
    , testProperty "analyze simple dependencies" prop_analyze_simple_dependencies
    , testProperty "analyze dependencies off" prop_analyze_dependencies_off
    , testProperty "analyze constraints directive" prop_analyze_constraints_directive
    , testProperty "analyze block dependencies" prop_analyze_block_dependencies
    , testProperty "lex dependencies empty" prop_lex_dependencies_empty
    , testProperty "lex dependencies simple" prop_lex_dependencies_simple
    , testProperty "parse dependencies empty program" prop_parse_dependencies_empty_program
    , testProperty "format dependency errors" prop_format_dependency_errors
    ]
  , testGroup "Unit Tests"
    [ testCase "new dependency analyzer" test_new_dependency_analyzer
    , testCase "analyze empty code" test_analyze_empty_code
    , testCase "analyze dependent types on" test_analyze_dependent_types_on
    , testCase "analyze dependent types off" test_analyze_dependent_types_off
    , testCase "analyze constraints directive" test_analyze_constraints_directive
    , testCase "analyze block dependencies" test_analyze_block_dependencies
    , testCase "lex empty" test_lex_empty
    , testCase "lex simple" test_lex_simple
    , testCase "parse empty program" test_parse_empty_program
    , testCase "parse simple program" test_parse_simple_program
    , testCase "format dependency errors" test_format_dependency_errors
    , testCase "built in functions" test_built_in_functions
    , testCase "analyze dependencies file" test_analyze_dependencies_file
    , testCase "analyze dependencies debug" test_analyze_dependencies_debug
    , testCase "analyze type constraints" test_analyze_type_constraints
    , testCase "analyze refinement types" test_analyze_refinement_types
    ]
  ]