{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Unit.CoreParserSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo, posAt)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)

-- Test properties for Parser module

-- | defaultFileDirectives should have all fields as Nothing
prop_defaultFileDirectives_nothing :: Property
prop_defaultFileDirectives_nothing = 
  let FileDirectives{..} = defaultFileDirectives
  in property $ fdOwnership == Nothing && 
                fdDependentTypes == Nothing && 
                fdConstraints == Nothing

-- | defaultBlockDirectives should have all fields as Nothing
prop_defaultBlockDirectives_nothing :: Property
prop_defaultBlockDirectives_nothing = 
  let BlockDirectives{..} = defaultBlockDirectives
  in property $ bdOwnership == Nothing && 
                bdDependentTypes == Nothing && 
                bdConstraints == Nothing

-- | parseTypus should handle empty input
prop_parseTypus_empty :: Property
prop_parseTypus_empty = 
  let result = parseTypus ""
  in property $ isRight result

-- | parseTypus should handle simple valid input
prop_parseTypus_simple_valid :: String -> Property
prop_parseTypus_simple_valid s = 
  let simpleInput = "function test() {\n  return 42;\n}"
      result = parseTypus simpleInput
  in property $ isRight result

-- | parseTypus should reject input with unmatched braces
prop_parseTypus_unmatched_braces :: Int -> Property
prop_parseTypus_unmatched_braces n = 
  let invalidInput = "function test() {\n  return 42;\n"  -- Missing closing brace
      result = parseTypus invalidInput
  in property $ isLeft result

-- | parseTypus should handle directives correctly
prop_parseTypus_directives :: Property
prop_parseTypus_directives = 
  let inputWithDirectives = "// @ownership: true\n// @dependent-types: false\nfunction test() {\n  return 42;\n}"
      result = parseTypus inputWithDirectives
  in property $ isRight result

-- | parseTypus should preserve line numbers in error messages
prop_parseTypus_error_positions :: Positive Int -> Property
prop_parseTypus_error_positions (Positive n) = 
  let inputWithLines = unlines $ replicate n "valid line" ++ ["invalid line {"]
      result = parseTypus inputWithLines
  in property $ case result of
    Left err -> show n `isInfixOf` show err
    Right _ -> False  -- Should have failed

-- Unit tests
test_defaultFileDirectives :: Assertion
test_defaultFileDirectives = do
  let FileDirectives{..} = defaultFileDirectives
  assertEqual "fdOwnership" Nothing fdOwnership
  assertEqual "fdDependentTypes" Nothing fdDependentTypes
  assertEqual "fdConstraints" Nothing fdConstraints

test_defaultBlockDirectives :: Assertion
test_defaultBlockDirectives = do
  let BlockDirectives{..} = defaultBlockDirectives
  assertEqual "bdOwnership" Nothing bdOwnership
  assertEqual "bdDependentTypes" Nothing bdDependentTypes
  assertEqual "bdConstraints" Nothing bdConstraints

test_parseTypus_empty :: Assertion
test_parseTypus_empty = do
  let result = parseTypus ""
  assertBool "parseTypus empty should succeed" (isRight result)

test_parseTypus_simple_function :: Assertion
test_parseTypus_simple_function = do
  let input = "function test() {\n  return 42;\n}"
  let result = parseTypus input
  assertBool "parseTypus simple function should succeed" (isRight result)

test_parseTypus_with_directives :: Assertion
test_parseTypus_with_directives = do
  let input = "// @ownership: true\n// @dependent-types: false\nfunction test() {\n  return 42;\n}"
  let result = parseTypus input
  assertBool "parseTypus with directives should succeed" (isRight result)

test_parseTypus_multiple_functions :: Assertion
test_parseTypus_multiple_functions = do
  let input = "function test1() {\n  return 1;\n}\n\nfunction test2() {\n  return 2;\n}"
  let result = parseTypus input
  assertBool "parseTypus multiple functions should succeed" (isRight result)

test_parseTypus_with_comments :: Assertion
test_parseTypus_with_comments = do
  let input = "// This is a comment\nfunction test() {\n  // Another comment\n  return 42;\n}\n/* Block comment */"
  let result = parseTypus input
  assertBool "parseTypus with comments should succeed" (isRight result)

test_parseTypus_unmatched_brace :: Assertion
test_parseTypus_unmatched_brace = do
  let input = "function test() {\n  return 42;\n"  -- Missing closing brace
  let result = parseTypus input
  assertBool "parseTypus unmatched brace should fail" (isLeft result)

test_parseTypus_invalid_syntax :: Assertion
test_parseTypus_invalid_syntax = do
  let input = "function test() {\n  return 42\n}"  -- Missing semicolon
  let result = parseTypus input
  assertBool "parseTypus invalid syntax should fail" (isLeft result)

test_parseTypus_nested_functions :: Assertion
test_parseTypus_nested_functions = do
  let input = "function outer() {\n  function inner() {\n    return 42;\n  }\n  return inner();\n}"
  let result = parseTypus input
  assertBool "parseTypus nested functions should succeed" (isRight result)

test_parseTypus_with_strings :: Assertion
test_parseTypus_with_strings = do
  let input = "function test() {\n  return \"Hello, world!\";\n}"
  let result = parseTypus input
  assertBool "parseTypus with strings should succeed" (isRight result)

test_parseTypus_with_numbers :: Assertion
test_parseTypus_with_numbers = do
  let input = "function test() {\n  return 42.5;\n}"
  let result = parseTypus input
  assertBool "parseTypus with numbers should succeed" (isRight result)

test_parseTypus_with_variables :: Assertion
test_parseTypus_with_variables = do
  let input = "function test() {\n  let x = 10;\n  let y = 20;\n  return x + y;\n}"
  let result = parseTypus input
  assertBool "parseTypus with variables should succeed" (isRight result)

-- Test suite
tests :: TestTree
tests = testGroup "Core Parser Tests"
  [ testProperties "QuickCheck Properties"
    [ ("parseTypus_empty", prop_parseTypus_empty)
    , ("parseTypus_simple_valid", property $ prop_parseTypus_simple_valid "function test() { return 42; }")
    , ("parseTypus_unmatched_braces", property $ prop_parseTypus_unmatched_braces 5)
    ]
  , testCase "parseTypus empty" test_parseTypus_empty
  , testCase "parseTypus simple function" test_parseTypus_simple_function
  , testCase "parseTypus with directives" test_parseTypus_with_directives
  , testCase "parseTypus unmatched brace" test_parseTypus_unmatched_brace
  , testCase "parseTypus invalid syntax" test_parseTypus_invalid_syntax
  ]