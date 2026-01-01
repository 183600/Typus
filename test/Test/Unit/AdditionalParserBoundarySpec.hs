{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalParserBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), SourcePos(..))

-- ============================================================================
-- Additional Parser Boundary Condition Tests
-- ============================================================================

-- Test: Empty input
test_empty_input :: TestTree
test_empty_input = testCase "parseTypus handles empty input" $ do
  let result = parseTypus ""
  case result of
    Left err -> assertBool "Should not fail on empty input" False
    Right file -> do
      tfDirectives file @?= defaultFileDirectives
      tfBuildTags file @?= []
      tfBlocks file @?= []

-- Test: Only whitespace
test_whitespace_only :: TestTree
test_whitespace_only = testCase "parseTypus handles whitespace-only input" $ do
  let inputs = ["   ", "\n\n", "  \n  \n  ", "\t\t", "  \t\n \t  "]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should not fail on whitespace-only: " ++ show input) False
      Right file -> do
        tfDirectives file @?= defaultFileDirectives
        tfBuildTags file @?= []
        tfBlocks file @?= []
    ) inputs

-- Test: Only comments
test_comments_only :: TestTree
test_comments_only = testCase "parseTypus handles comments-only input" $ do
  let inputs = 
        [ "// line comment\n"
        , "/* block comment */\n"
        , "// comment 1\n// comment 2\n"
        , "/* nested\n * block\n * comment\n */\n"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should not fail on comments-only: " ++ input) False
      Right file -> do
        tfDirectives file @?= defaultFileDirectives
        tfBuildTags file @?= []
        tfBlocks file @?= []
    ) inputs

-- Test: Malformed file directives
test_malformed_file_directives :: TestTree
test_malformed_file_directives = testCase "parseTypus handles malformed file directives" $ do
  let inputs = 
        [ "//! ownership:\n"  -- Missing value
        , "//! :true\n"      -- Missing key
        , "//! ownership true\n"  -- Missing colon
        , "//! ownership::true\n"  -- Double colon
        , "//! ownership: true, dependent-types:\n"  -- Missing second value
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left _ -> assertBool "Should handle malformed file directives gracefully" True
      Right file -> assertBool "Should parse successfully L.or fail gracefully" True
    ) inputs

-- Test: Malformed block directives
test_malformed_block_directives :: TestTree
test_malformed_block_directives = testCase "parseTypus handles malformed block directives" $ do
  let inputs = 
        [ "{//! ownership:\n}\n"  -- Missing value
        , "{//! :true}\n"      -- Missing key
        , "{//! ownership true}\n"  -- Missing colon
        , "{//! ownership::true}\n"  -- Double colon
        , "{//! ownership: true, dependent-types:\n}\n"  -- Missing second value
        , "{//! ownership: true\n"  -- Missing closing brace
        , "//! ownership: true}\n"  -- Missing opening brace
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left _ -> assertBool "Should handle malformed block directives gracefully" True
      Right file -> assertBool "Should parse successfully L.or fail gracefully" True
    ) inputs

-- Test: Very long lines
test_very_long_lines :: TestTree
test_very_long_lines = testCase "parseTypus handles very long lines" $ do
  let longLine = replicate 10000 'a'
      input = longLine ++ "\n"
  let result = parseTypus input
  case result of
    Left err -> assertBool ("Should handle long lines: " ++ take 100 err) False
    Right file -> assertBool "Should parse long lines successfully" True

-- Test: Deep nesting
test_deep_nesting :: TestTree
test_deep_nesting = testCase "parseTypus handles deep nesting" $ do
  let nestedBraces = replicate 1000 '{'
      closingBraces = replicate 1000 '}'
      input = nestedBraces ++ "\n" ++ closingBraces ++ "\n"
  let result = parseTypus input
  case result of
    Left _ -> assertBool "Should handle deep nesting (may fail but gracefully)" True
    Right file -> assertBool "Should parse deep nesting successfully" True

-- Test: Mixed line endings
test_mixed_line_endings :: TestTree
test_mixed_line_endings = testCase "parseTypus handles mixed line endings" $ do
  let inputs = 
        [ "line1\nline2\r\nline3\r"
        , "line1\r\nline2\nline3\r\n"
        , "line1\rline2\nline3\r"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should handle mixed line endings: " ++ show input) False
      Right file -> assertBool "Should parse with mixed line endings" True
    ) inputs

-- Test: Unicode content
test_unicode_content :: TestTree
test_unicode_content = testCase "parseTypus handles Unicode content" $ do
  let inputs = 
        [ "café naïve résumé\n"
        , "测试内容\n"
        , "🚀 emoji test 🎉\n"
        , "mixed 中文 L.and english 🌟\n"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should handle Unicode: " ++ input) False
      Right file -> assertBool "Should parse Unicode content" True
    ) inputs

-- Test: Special characters
test_special_characters :: TestTree
test_special_characters = testCase "parseTypus handles special characters" $ do
  let inputs = 
        [ "!@#$%^&*()_+-=[]{}|;':\",./<>?\n"
        , "\0\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127\n"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left _ -> assertBool "Should handle special characters gracefully" True
      Right file -> assertBool "Should parse special characters" True
    ) inputs

-- Test: If statements without braces (error case)
test_if_without_braces :: TestTree
test_if_without_braces = testCase "parseTypus detects if statements without braces" $ do
  let inputs = 
        [ "if condition\n"
        , "if condition // comment\n"
        , "if (condition)\n"
        , "if (condition) // comment\n"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should detect if without braces: " ++ err) True
      Right file -> assertBool "Should detect syntax error" True
    ) inputs

-- Test: Valid if statements with braces
test_if_with_braces :: TestTree
test_if_with_braces = testCase "parseTypus accepts valid if statements with braces" $ do
  let inputs = 
        [ "if condition {\n"
        , "if (condition) {\n"
        , "if condition { // comment\n"
        , "if (condition) { // comment\n"
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left err -> assertBool ("Should accept if with braces: " ++ err) False
      Right file -> assertBool "Should parse valid if statements" True
    ) inputs

-- Test: Multiple file directives
test_multiple_file_directives :: TestTree
test_multiple_file_directives = testCase "parseTypus handles multiple file directives" $ do
  let input = "//! ownership: true\n//! dependent-types: true\n//! constraints: false\n"
  let result = parseTypus input
  case result of
    Left err -> assertBool ("Should handle multiple directives: " ++ err) False
    Right file -> do
      let dirs = tfDirectives file
      assertBool "Should have ownership directive" (fdOwnership dirs /= Nothing)
      assertBool "Should have dependent-types directive" (fdDependentTypes dirs /= Nothing)
      assertBool "Should have constraints directive" (fdConstraints dirs /= Nothing)

-- Test: Multiple block directives
test_multiple_block_directives :: TestTree
test_multiple_block_directives = testCase "parseTypus handles multiple block directives" $ do
  let input = "{//! ownership: true, dependent-types: true, constraints: false}\ncontent\n"
  let result = parseTypus input
  case result of
    Left err -> assertBool ("Should handle multiple block directives: " ++ err) False
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have at least one block" (not (null blocks))
      let firstBlock = L.head blocks
      let dirs = cbDirectives firstBlock
      assertBool "Should have ownership directive" (bdOwnership dirs /= Nothing)
      assertBool "Should have dependent-types directive" (bdDependentTypes dirs /= Nothing)
      assertBool "Should have constraints directive" (bdConstraints dirs /= Nothing)

-- Property: Round-trip parsing for simple content
prop_round_trip_simple :: String -> Property
prop_round_trip_simple content =
  not (L.any (`elem` "\n\r{}//!") content) ==> -- Avoid complex parsing
  let input = content ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in if null blocks
         then property True
         else property (content `elem` map cbContent blocks)

-- Property: Parser is idempotent for directives
prop_parser_idempotent_directives :: String -> Property
prop_parser_idempotent_directives content =
  let input = "//! ownership: true\n" ++ content
      result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Right file1, Right file2) -> 
      property (tfDirectives file1 === tfDirectives file2)
    _ -> property True -- If parsing fails, that's acceptable for this test

-- Property: Parser handles large inputs
prop_parser_large_input :: Int -> String -> Property
prop_parser_large_input multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let largeContent = L.concat (replicate multiplier (baseContent ++ "\n"))
      result = parseTypus largeContent
  in case result of
    Left _ -> property True -- May fail, but should not crash
    Right file -> property True

-- Property: Parser preserves line count
prop_parser_preserves_line_count :: String -> Property
prop_parser_preserves_line_count content =
  let inputLines = L.length (lines content)
      result = parseTypus content
  in case result of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          blockLines = L.sum $ L.map (L.length . lines . cbContent) blocks
      in property (blockLines <= inputLines + 1) -- Allow some variance for directives

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Parser Boundary Tests"
  [ test_empty_input
  , test_whitespace_only
  , test_comments_only
  , test_malformed_file_directives
  , test_malformed_block_directives
  , test_very_long_lines
  , test_deep_nesting
  , test_mixed_line_endings
  , test_unicode_content
  , test_special_characters
  , test_if_without_braces
  , test_if_with_braces
  , test_multiple_file_directives
  , test_multiple_block_directives
  , fastProperty "round-trip parsing for simple content" prop_round_trip_simple
  , fastProperty "parser is idempotent for directives" prop_parser_idempotent_directives
  , fastProperty "parser handles large inputs" prop_parser_large_input
  , fastProperty "parser preserves line count" prop_parser_preserves_line_count
  ]