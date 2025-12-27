{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  )

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort)

-- Test parser error recovery with malformed syntax
test_parser_error_recovery :: TestTree
test_parser_error_recovery = testCase "Parser recovers from syntax errors" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 5"
        , "    // Missing closing brace"
        , "    y := x + 1"
        , "}"
        ]
  case parseTypus source of
    Left err -> do
      -- Should fail with a meaningful error message
      assertBool "Error message should contain line info" $ isInfixOf "line" err
      assertBool "Error message should be descriptive" $ isInfixOf "syntax" err
    Right _ -> assertFailure "Expected parse failure due to missing closing brace"

-- Test parser handles empty input gracefully
test_parser_empty_input :: TestTree
test_parser_empty_input = testCase "Parser handles empty input" $ do
  case parseTypus "" of
    Left err -> assertFailure $ "parseTypus failed on empty input: " <> err
    Right typusFile -> do
      -- Should parse empty file successfully
      tfCodeBlocks typusFile @?= []

-- Test parser handles whitespace-only input
test_parser_whitespace_only :: TestTree
test_parser_whitespace_only = testCase "Parser handles whitespace-only input" $ do
  let source = unlines ["", "   ", "\t", "  \t  ", ""]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed on whitespace-only input: " <> err
    Right typusFile -> do
      -- Should parse whitespace-only file successfully
      tfCodeBlocks typusFile @?= []

-- Test parser handles very long lines
test_parser_long_lines :: TestTree
test_parser_long_lines = testCase "Parser handles very long lines" $ do
  let longString = replicate 1000 'a'
      source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := \"" ++ longString ++ "\""
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed on long lines: " <> err
    Right _ -> pure () -- Should parse successfully

-- Test parser handles Unicode characters
test_parser_unicode :: TestTree
test_parser_unicode = testCase "Parser handles Unicode characters" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    // 测试中文注释"
        , "    x := \"测试Unicode字符串\""
        , "    y := \"🚀 Rocket emoji\""
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed on Unicode: " <> err
    Right _ -> pure () -- Should parse successfully

-- Test parser handles nested structures
test_parser_nested_structures :: TestTree
test_parser_nested_structures = testCase "Parser handles nested structures" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    if true {"
        , "        for i := 0; i < 10; i++ {"
        , "            if i % 2 == 0 {"
        , "                x := i * 2"
        , "            } else {"
        , "                y := i + 1"
        , "            }"
        , "        }"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed on nested structures: " <> err
    Right _ -> pure () -- Should parse successfully

-- Property: Parser should handle arbitrary strings without crashing
prop_parser_no_crash :: String -> Property
prop_parser_no_crash input = 
  let result = parseTypus input
  in property $ case result of
    Left _ -> True -- Failing is OK, just shouldn't crash
    Right _ -> True -- Success is also OK

-- Property: Parser should preserve line numbers in error messages
prop_parser_line_numbers :: String -> Property
prop_parser_line_numbers input = 
  let linesCount = length (lines input)
      result = parseTypus input
  in classify (linesCount > 1) "multi-line input" $
     case result of
       Left err -> property $ isInfixOf "line" err
       Right _ -> property True

-- Property: Parser should handle comments gracefully
prop_parser_comments :: String -> Property
prop_parser_comments baseInput = 
  let withComments = unlines 
        [ "// Single line comment"
        , "/* Multi-line"
        , "   comment */"
        , baseInput
        , "// Another comment"
        ]
      result = parseTypus withComments
  in property $ case result of
    Left _ -> True -- Failing is OK, just shouldn't crash due to comments
    Right _ -> True -- Success is also OK

-- Property: Parser should handle indentation variations
prop_parser_indentation :: String -> Property
prop_parser_indentation baseInput = 
  let indentedInputs = 
        [ baseInput -- Original
        , unlines $ map ("    " ++) (lines baseInput) -- 4-space indent
        , unlines $ map ("\t" ++) (lines baseInput) -- Tab indent
        , unlines $ map ("  " ++) (lines baseInput) -- 2-space indent
        ]
      results = map parseTypus indentedInputs
  in property $ all (\r -> case r of Left _ -> True; Right _ -> True) results

tests :: TestTree
tests = testGroup "New Parser Error Recovery Tests"
  [ test_parser_error_recovery
  , test_parser_empty_input
  , test_parser_whitespace_only
  , test_parser_long_lines
  , test_parser_unicode
  , test_parser_nested_structures
  , fastProperty "Parser doesn't crash on arbitrary input" prop_parser_no_crash
  , fastProperty "Parser reports line numbers in errors" prop_parser_line_numbers
  , fastProperty "Parser handles comments gracefully" prop_parser_comments
  , fastProperty "Parser handles indentation variations" prop_parser_indentation
  ]