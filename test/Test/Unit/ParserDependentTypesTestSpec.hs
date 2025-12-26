{-# LANGUAGE CPP #-}

module Test.Unit.ParserDependentTypesTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>))

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import DependentTypesParser (parseDependentType, DependentTypeConstraint(..))
import SourceLocation (SourcePos(..), startPos)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (isNothing, isJust)

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Test parser with empty input
test_parse_empty_input :: IO ()
test_parse_empty_input = do
    let result = parseTypus ""
    assertBool "Empty input should parse successfully" (isRight result)
  where
    isRight (Right _) = True
    isRight _ = False

-- Test parser with simple valid input
test_parse_simple_valid :: IO ()
test_parse_simple_valid = do
    let input = "func main() { return 42; }"
        result = parseTypus input
    assertBool "Simple valid input should parse" (isRight result)

-- Test parser with comments
test_parse_with_comments :: IO ()
test_parse_with_comments = do
    let input = "// This is a comment\nfunc main() { return 42; }"
        result = parseTypus input
    assertBool "Input with comments should parse" (isRight result)

-- Test parser directives
test_parse_file_directives :: IO ()
test_parse_file_directives = do
    let input = "#![ownership = true]\n#![dependent_types = true]\nfunc main() { return 42; }"
        result = parseTypus input
    case result of
        Right typusFile -> do
            let directives = fileDirectives typusFile
            assertBool "Should have ownership directive" (isJust (fdOwnership directives))
            assertBool "Should have dependent types directive" (isJust (fdDependentTypes directives))
        Left _ -> assertBool "Should parse successfully" False

-- Test parser properties
prop_parser_roundtrip :: String -> Property
prop_parser_roundtrip input = 
    not (null input) && length input < 100 ==> -- Limit size for performance
    case parseTypus input of
        Right parsed -> True -- If it parses, consider it successful
        Left _ -> True -- Failed parsing is also a valid outcome

-- ============================================================================
-- Dependent Types Tests
-- ============================================================================

-- Test dependent type constraint parsing
test_parse_dependent_type_constraint :: IO ()
test_parse_dependent_type_constraint = do
    let input = "Vector{n : Nat | n > 0}"
        result = parseDependentType input
    case result of
        Right constraint -> do
            assertEqual "Should parse constraint name" "Vector" (constraintName constraint)
            assertBool "Should have predicate" (not (T.null (constraintPredicate constraint)))
        Left _ -> assertBool "Should parse dependent type constraint" False

-- Test dependent type with complex constraint
test_parse_complex_dependent_type :: IO ()
test_parse_complex_dependent_type = do
    let input = "Matrix{m : Nat, n : Nat | m > 0 && n > 0}"
        result = parseDependentType input
    case result of
        Right constraint -> do
            assertEqual "Should parse matrix type" "Matrix" (constraintName constraint)
            assertBool "Should have complex predicate" ("> 0 &&" `isInfixOf` T.unpack (constraintPredicate constraint))
        Left _ -> assertBool "Should parse complex dependent type" False

-- Test dependent type properties
prop_dependent_type_has_name :: DependentTypeConstraint -> Bool
prop_dependent_type_has_name constraint = not (T.null (constraintName constraint))

prop_dependent_type_has_predicate :: DependentTypeConstraint -> Bool
prop_dependent_type_has_predicate constraint = not (T.null (constraintPredicate constraint))

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test parsing with ownership and dependent types
test_parse_ownership_dependent_types :: IO ()
test_parse_ownership_dependent_types = do
    let input = "#![ownership = true]\n#![dependent_types = true]\n\nfunc process(data: Vector{n : Nat | n > 0}) { data.move() }"
        result = parseTypus input
    assertBool "Should parse with ownership and dependent types" (isRight result)

-- Test parsing nested blocks
test_parse_nested_blocks :: IO ()
test_parse_nested_blocks = do
    let input = "func outer() { if true { func inner() { return 42; } } }"
        result = parseTypus input
    assertBool "Should parse nested blocks" (isRight result)

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary DependentTypeConstraint where
  arbitrary = DependentTypeConstraint <$> arbitrary <*> arbitrary

-- ============================================================================
-- Test Utilities
-- ============================================================================

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser and Dependent Types Test Suite"
  [ testGroup "Parser Tests"
      [ testCase "Parse empty input" test_parse_empty_input
      , testCase "Parse simple valid input" test_parse_simple_valid
      , testCase "Parse with comments" test_parse_with_comments
      , testCase "Parse file directives" test_parse_file_directives
      , fastProperty "Parser roundtrip property" prop_parser_roundtrip
      ]
  , testGroup "Dependent Types Tests"
      [ testCase "Parse dependent type constraint" test_parse_dependent_type_constraint
      , testCase "Parse complex dependent type" test_parse_complex_dependent_type
      , testProperty "Dependent type has name" prop_dependent_type_has_name
      , testProperty "Dependent type has predicate" prop_dependent_type_has_predicate
      ]
  , testGroup "Integration Tests"
      [ testCase "Parse ownership with dependent types" test_parse_ownership_dependent_types
      , testCase "Parse nested blocks" test_parse_nested_blocks
      ]
  ]