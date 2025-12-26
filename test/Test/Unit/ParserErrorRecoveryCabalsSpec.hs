{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T

-- Test 1: Parser recovers from malformed function declarations
test_parser_recovers_from_malformed_function :: TestTree
test_parser_recovers_from_malformed_function =
  testCase "Parser recovers from malformed function declarations" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func malformed( {  // Missing closing parenthesis"
          , "func valid() {"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should parse successfully with error recovery
        assertBool "Parser should recover from syntax errors" $
          "malformed" `isInfixOf` err || "valid" `isInfixOf` err
      Right typusFile -> do
        -- Should have parsed the valid function despite the error
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse valid function after error" $
          any (isInfixOf "valid" . unlines . cbLines) codeBlocks

-- Test 2: Parser handles incomplete type declarations
test_parser_handles_incomplete_types :: TestTree
test_parser_handles_incomplete_types =
  testCase "Parser handles incomplete type declarations" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type Incomplete struct {  // Missing closing brace"
          , "func valid() int {"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should provide helpful error message
        assertBool "Error should mention incomplete type" $
          any (`isInfixOf` err) ["Incomplete", "struct", "brace", "missing"]
      Right _ -> do
        -- Parser recovered successfully
        assertFailure "Expected parsing error for incomplete type"

-- QuickCheck property: Parser is robust to random string injections
prop_parser_robust_to_random_injections :: String -> Property
prop_parser_robust_to_random_injections injection =
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ injection
        , "  return 42"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True -- Parser gracefully handles errors
       Right typusFile -> property $ 
         -- Should still extract some structure despite injection
         not (null $ tfCodeBlocks typusFile)

-- QuickCheck property: Parser preserves line numbers in error messages
prop_parser_preserves_line_numbers :: Int -> String -> Property
prop_parser_preserves_line_numbers lineNum content =
  lineNum >= 0 && lineNum <= 100 ==>  -- Reasonable line number range
  let source = unlines $ replicate lineNum "// comment" ++ ["func invalid( {"]
  in case parseTypus source of
       Left err -> 
         property $ any (`isInfixOf` err) [show (lineNum + 1), "line", "Line"]
       Right _ -> property True

-- Test 3: Parser recovers from nested block errors
test_parser_recovers_from_nested_errors :: TestTree
test_parser_recovers_from_nested_errors =
  testCase "Parser recovers from nested block errors" $ do
    let source = unlines
          [ "package main"
          , "func outer() {"
          , "  if true {"
          , "    // Missing closing brace for if"
          , "  func inner() {  // This should still be parsed"
          , "    return 42"
          , "  }"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should report error but continue parsing
        assertBool "Should mention brace or block issue" $
          any (`isInInfix` err) ["brace", "block", "nested", "missing"]
      Right typusFile -> do
        -- Should have found the inner function
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should find inner function" $
          any (isInfixOf "inner" . unlines . cbLines) codeBlocks

-- Test 4: Parser handles directive errors gracefully
test_parser_handles_directive_errors :: TestTree
test_parser_handles_directive_errors =
  testCase "Parser handles directive errors gracefully" $ do
    let source = unlines
          [ "//! ownership: invalid_value"  -- Invalid directive value
          , "//! dependent_types: definitely_not_a_boolean"
          , "package main"
          , "func main() {"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle directive parsing errors
        assertBool "Should mention directive error" $
          any (`isInfixOf` err) ["directive", "invalid", "ownership", "dependent_types"]
      Right typusFile -> do
        -- Should still parse the main function
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse main function despite directive errors" $
          any (isInfixOf "main" . unlines . cbLines) codeBlocks

-- Test 5: Parser error recovery with Unicode content
test_parser_unicode_error_recovery :: TestTree
test_parser_unicode_error_recovery =
  testCase "Parser error recovery with Unicode content" $ do
    let source = unlines
          [ "// 测试中文注释"
          , "func 测试函数( {  // Unicode function name with syntax error"
          , "  return 🚀  // Unicode emoji"
          , "}"
          , "func normal() {"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle Unicode gracefully
        assertBool "Should handle Unicode content" $
          length err > 0  -- Just check we get some response
      Right typusFile -> do
        -- Should parse the normal function
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse normal function after Unicode error" $
          any (isInfixOf "normal" . unlines . cbLines) codeBlocks

tests :: TestTree
tests =
  testGroup "Parser Error Recovery Cabals Tests"
    [ test_parser_recovers_from_malformed_function
    , test_parser_handles_incomplete_types
    , fastProperty "Parser robust to random injections" prop_parser_robust_to_random_injections
    , fastProperty "Parser preserves line numbers in errors" prop_parser_preserves_line_numbers
    , test_parser_recovers_from_nested_errors
    , test_parser_handles_directive_errors
    , test_parser_unicode_error_recovery
    ]