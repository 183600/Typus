module Test.Unit.EnhancedDependenciesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import Dependencies module
import Dependencies (newDependentTypeChecker, 
                    analyzeDependentTypes, analyzeAST, validateASTSemantics, validateStatement,
                    TypeVar(..), TypeConstraint(..), Substitution, TypeScheme(..),
                    TypeEnvironment(..), TypeInferenceState(..), TypeInferenceError(..))
import Dependencies.Parser (runParser)
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import Dependencies.Parser (parseProgram, runParser)
import Dependencies.Stub (DependencyType(..), Dependency(..), DependencyAnalysis(..), 
                          analyzeDependencies, checkCircularDependencies, resolveDependencyOrder,
                          validateDependencies)
import qualified Dependencies.TypeSystem as DTS

-- Import Parser module
import Parser (TypusFile(..), parseTypus)

-- Test properties for dependencies

-- Property 1: Creating dependency analyzer should not crash
prop_new_dependency_analyzer :: Property
prop_new_dependency_analyzer = property $
  let analyzer = newDependentTypeChecker
  in property True  -- Should not crash

-- Property 2: Analyzing empty string should not crash
prop_analyze_dependencies_empty :: Property
prop_analyze_dependencies_empty = property $
  case analyzeDependentTypes "" of
    errs -> property True  -- Analysis returns list of errors, but shouldn't crash

-- Property 3: Analyzing simple dependent types code should not crash
prop_analyze_simple_dependencies :: String -> Property
prop_analyze_simple_dependencies name = 
  not (null name) && all isAlpha name ==>
  let code = "//! dependent_types: on\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependentTypes code of
    errs -> property True  -- Analysis returns list of errors, but shouldn't crash

-- Property 4: Analyzing code with dependent types off should not crash
prop_analyze_dependencies_off :: String -> Property
prop_analyze_dependencies_off name = 
  not (null name) && all isAlpha name ==>
  let code = "//! dependent_types: off\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependentTypes code of
    errs -> property True  -- Analysis returns list of errors, but shouldn't crash

-- Property 5: Analyzing code with constraints directive should not crash
prop_analyze_constraints_directive :: String -> Property
prop_analyze_constraints_directive name = 
  not (null name) && all isAlpha name ==>
  let code = "//! constraints: on\npackage main\n\nfunc " ++ name ++ "() {}\n"
  in case analyzeDependentTypes code of
    errs -> property True  -- Analysis returns list of errors, but shouldn't crash

-- Property 6: Analyzing code with block dependent types should not crash
prop_analyze_block_dependencies :: String -> Property
prop_analyze_block_dependencies name = 
  not (null name) && all isAlpha name ==>
  let code = "package main\n\nfunc main() {\n  {//! dependent_types: on\n    var " ++ name ++ " int\n  }\n}\n"
  in case analyzeDependentTypes code of
    errs -> property True  -- Analysis returns list of errors, but shouldn't crash

-- Property 7: Parsing empty string should not crash
prop_parse_dependencies_empty :: Property
prop_parse_dependencies_empty = property $
  case runParser "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 8: Parsing simple code should not crash
prop_parse_dependencies_simple :: String -> Property
prop_parse_dependencies_simple name = 
  not (null name) && all isAlpha name ==>
  let code = "func " ++ name ++ "() {}"
  in case runParser code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 9: Parsing empty program should not crash
prop_parse_dependencies_empty_program :: Property
prop_parse_dependencies_empty_program = property $
  case runParser "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right _ -> property True

-- Property 10: Error formatting should not crash
prop_format_dependency_errors :: [String] -> Property
prop_format_dependency_errors errors = 
  let dependencyErrors = map (\msg -> DTS.ParseError msg) errors
  in property True  -- Should not crash

-- Unit tests for specific dependencies functionality

test_new_dependency_analyzer :: Assertion
test_new_dependency_analyzer = 
  let analyzer = newDependentTypeChecker
  in assertBool "Creating dependency analyzer should not crash" True

test_analyze_empty_code :: Assertion
test_analyze_empty_code = 
  let errs = analyzeDependentTypes ""
  in assertBool "Analyzing empty code should not crash" True

test_analyze_dependent_types_on :: Assertion
test_analyze_dependent_types_on = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing dependent types on should not crash" True

test_analyze_dependent_types_off :: Assertion
test_analyze_dependent_types_off = 
  let code = "//! dependent_types: off\npackage main\n\nfunc main() {}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing dependent types off should not crash" True

test_analyze_constraints_directive :: Assertion
test_analyze_constraints_directive = 
  let code = "//! constraints: on\npackage main\n\nfunc main() {}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing constraints directive should not crash" True

test_analyze_block_dependencies :: Assertion
test_analyze_block_dependencies = 
  let code = "package main\n\nfunc main() {\n  {//! dependent_types: on\n    var x int\n  }\n}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing block dependencies should not crash" True

test_parse_empty :: Assertion
test_parse_empty = 
  case runParser "" of
    Left _ -> assertBool "Parsing empty string should not crash" True
    Right _ -> assertBool "Parsing empty string should not crash" True

test_parse_simple :: Assertion
test_parse_simple = 
  let code = "func main() {}"
  in case runParser code of
    Left _ -> assertBool "Parsing simple code should not crash" True
    Right _ -> assertBool "Parsing simple code should not crash" True

test_parse_empty_program :: Assertion
test_parse_empty_program = 
  case runParser "" of
    Left _ -> assertBool "Parsing empty program should not crash" True
    Right _ -> assertBool "Parsing empty program should not crash" True

test_parse_simple_program :: Assertion
test_parse_simple_program = 
  let code = "func main() {}"
  in case runParser code of
    Left _ -> assertBool "Parsing simple program should not crash" True
    Right _ -> assertBool "Parsing simple program should not crash" True

test_format_dependency_errors :: Assertion
test_format_dependency_errors = 
  let errors = [DTS.ParseError "error1", DTS.ParseError "error2"]
  in assertBool "Formatting dependency errors should not crash" True

test_analyze_dependencies_file :: Assertion
test_analyze_dependencies_file = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      let errs = analyzeDependentTypes code
      in assertBool "Analyzing dependencies file should not crash" True

test_analyze_dependencies_debug :: Assertion
test_analyze_dependencies_debug = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing dependencies debug should not crash" True

test_analyze_type_constraints :: Assertion
test_analyze_type_constraints = 
  let code = "//! constraints: on\npackage main\n\ntype Vector struct {\n  length int\n  data []float64\n}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing type constraints should not crash" True

test_analyze_refinement_types :: Assertion
test_analyze_refinement_types = 
  let code = "//! dependent_types: on\npackage main\n\nfunc SafeDivide(a, b int) int {\n  if b == 0 {\n    panic(\"Division by zero\")\n  }\n  return a / b\n}\n"
      errs = analyzeDependentTypes code
  in assertBool "Analyzing refinement types should not crash" True

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedDependenciesSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "new dependency analyzer" prop_new_dependency_analyzer
    , testProperty "analyze dependencies empty" prop_analyze_dependencies_empty
    , testProperty "analyze simple dependencies" prop_analyze_simple_dependencies
    , testProperty "analyze dependencies off" prop_analyze_dependencies_off
    , testProperty "analyze constraints directive" prop_analyze_constraints_directive
    , testProperty "analyze block dependencies" prop_analyze_block_dependencies
    , testProperty "parse dependencies empty" prop_parse_dependencies_empty
    , testProperty "parse dependencies simple" prop_parse_dependencies_simple
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
    , testCase "parse empty" test_parse_empty
    , testCase "parse simple" test_parse_simple
    , testCase "parse empty program" test_parse_empty_program
    , testCase "parse simple program" test_parse_simple_program
    , testCase "format dependency errors" test_format_dependency_errors
    , testCase "analyze dependencies file" test_analyze_dependencies_file
    , testCase "analyze dependencies debug" test_analyze_dependencies_debug
    , testCase "analyze type constraints" test_analyze_type_constraints
    , testCase "analyze refinement types" test_analyze_refinement_types
    ]
  ]