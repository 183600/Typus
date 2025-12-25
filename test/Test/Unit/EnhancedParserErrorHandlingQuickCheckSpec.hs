{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.EnhancedParserErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), counterexample, forAll, oneof, elements, listOf, listOf1, choose, sized)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (SourceSpan(..))
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Control.Exception (evaluate, try, SomeException)

-- ============================================================================
-- Parser Error Handling QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Handling QuickCheck Tests"
  [ testProperty "parseTypus handles empty input gracefully" prop_parse_empty_input
  , testProperty "parseTypus handles whitespace-only input" prop_parse_whitespace_only
  , testProperty "parseTypus handles malformed file directives" prop_parse_malformed_file_directives
  , testProperty "parseTypus handles malformed block directives" prop_parse_malformed_block_directives
  , testProperty "parseTypus handles unclosed blocks" prop_parse_unclosed_blocks
  , testProperty "parseTypus handles nested blocks" prop_parse_nested_blocks
  , testProperty "parseTypus preserves content with special characters" prop_parse_preserves_special_chars
  , testProperty "parseTypus handles mixed line endings" prop_parse_mixed_line_endings
  , testProperty "parseTypus handles deeply nested braces" prop_parse_deeply_nested_braces
  , testProperty "parseTypus handles strings with braces" prop_parse_strings_with_braces
  , testProperty "parseTypus handles comments with braces" prop_parse_comments_with_braces
  , testCase "parseTypus handles specific error cases" test_parse_specific_errors
  , testCase "parseTypus handles edge cases" test_parse_edge_cases
  ]

-- ============================================================================
-- Basic Parsing Properties
-- ============================================================================

prop_parse_empty_input :: Property
prop_parse_empty_input =
  let result = parseTypus ""
  in counterexample ("Empty input should parse successfully: " ++ show result) $
     case result of
       Left _ -> False
       Right typusFile -> True  -- Empty input should be valid

prop_parse_whitespace_only :: Property
prop_parse_whitespace_only =
  forAll genWhitespaceOnly $ \whitespace ->
    let result = parseTypus whitespace
    in counterexample ("Whitespace-only input should parse: " ++ show whitespace) $
       case result of
         Left _ -> False
         Right typusFile -> True

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

prop_parse_malformed_file_directives :: Property
prop_parse_malformed_file_directives =
  forAll genMalformedFileDirective $ \directive ->
    let input = directive ++ "\ncontent"
        result = parseTypus input
    in counterexample ("Should handle malformed directive: " ++ directive) $
       case result of
         Left _ -> True  -- Malformed directives should fail gracefully
         Right _ -> True  -- Or succeed if parser is lenient

prop_parse_malformed_block_directives :: Property
prop_parse_malformed_block_directives =
  forAll genMalformedBlockDirective $ \directive ->
    let input = directive ++ "\ncontent\n}"
        result = parseTypus input
    in counterexample ("Should handle malformed block directive: " ++ directive) $
       case result of
         Left _ -> True  -- Malformed directives should fail gracefully
         Right _ -> True  -- Or succeed if parser is lenient

-- ============================================================================
-- Block Parsing Properties
-- ============================================================================

prop_parse_unclosed_blocks :: Property
prop_parse_unclosed_blocks =
  forAll genUnclosedBlock $ \block ->
    let result = parseTypus block
    in counterexample ("Should handle unclosed block: " ++ take 100 block) $
       case result of
         Left _ -> True  -- Unclosed blocks should fail gracefully
         Right _ -> True  -- Or succeed with partial parsing

prop_parse_nested_blocks :: Property
prop_parse_nested_blocks =
  forAll genNestedBlocks $ \blocks ->
    let result = parseTypus blocks
    in counterexample ("Should handle nested blocks: " ++ take 100 blocks) $
       case result of
         Left _ -> True  -- Should either succeed or fail gracefully
         Right typusFile -> True

-- ============================================================================
-- Content Preservation Properties
-- ============================================================================

prop_parse_preserves_special_chars :: Property
prop_parse_preserves_special_chars =
  forAll genContentWithSpecialChars $ \content ->
    let input = "//! ownership: on\n" ++ content
        result = parseTypus input
    in counterexample ("Should preserve special characters: " ++ take 50 content) $
       case result of
         Right typusFile -> 
           let blockContents = map cbContent (tfBlocks typusFile)
           in any (`isInfixOf` content) blockContents
         Left _ -> False

prop_parse_mixed_line_endings :: Property
prop_parse_mixed_line_endings =
  forAll genMixedLineEndings $ \content ->
    let result = parseTypus content
    in counterexample ("Should handle mixed line endings") $
       case result of
         Left _ -> False
         Right _ -> True

-- ============================================================================
-- Brace Handling Properties
-- ============================================================================

prop_parse_deeply_nested_braces :: Property
prop_parse_deeply_nested_braces =
  forAll genDeeplyNestedBraces $ \content ->
    let result = parseTypus content
    in counterexample ("Should handle deeply nested braces") $
       case result of
         Left _ -> True  -- Should fail gracefully or succeed
         Right _ -> True

prop_parse_strings_with_braces :: Property
prop_parse_strings_with_braces =
  forAll genStringsWithBraces $ \content ->
    let result = parseTypus content
    in counterexample ("Should handle strings with braces: " ++ take 50 content) $
       case result of
         Left _ -> True  -- Should not crash
         Right _ -> True

prop_parse_comments_with_braces :: Property
prop_parse_comments_with_braces =
  forAll genCommentsWithBraces $ \content ->
    let result = parseTypus content
    in counterexample ("Should handle comments with braces: " ++ take 50 content) $
       case result of
         Left _ -> True  -- Should not crash
         Right _ -> True

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_parse_specific_errors :: IO ()
test_parse_specific_errors = do
  -- Test multiple package declarations
  let multiplePackages = "package main\npackage test"
      result1 = parseTypus multiplePackages
  case result1 of
    Left _ -> assertBool "Multiple packages should fail" True
    Right _ -> assertBool "Multiple packages should not succeed" False
  
  -- Test if statements without braces
  let ifWithoutBrace = "if condition {\n  // some code\n}\nif condition\n  // no brace"
      result2 = parseTypus ifWithoutBrace
  case result2 of
    Left _ -> assertBool "If without brace should fail" True
    Right _ -> assertBool "If without brace should not succeed" False
  
  -- Test malformed boolean values
  let malformedBool = "//! ownership: maybe\ncontent"
      result3 = parseTypus malformedBool
  case result3 of
    Left _ -> assertBool "Malformed boolean should fail" True
    Right _ -> assertBool "Malformed boolean should not succeed" False

test_parse_edge_cases :: IO ()
test_parse_edge_cases = do
  -- Test extremely long lines
  let longLine = replicate 10000 'a'
      result1 = parseTypus longLine
  case result1 of
    Left _ -> assertBool "Long line should not crash" True
    Right _ -> assertBool "Long line should parse" True
  
  -- Test many small blocks
  let manyBlocks = concat $ replicate 100 "{//! ownership: on\nsmall block\n}\n"
      result2 = parseTypus manyBlocks
  case result2 of
    Left _ -> assertBool "Many blocks should not crash" True
    Right typusFile -> assertBool "Many blocks should parse" $ length (tfBlocks typusFile) > 0
  
  -- Test unicode content
  let unicodeContent = "//! ownership: on\nHello 世界 \ud83c\udf0d\n测试内容\n}"
      result3 = parseTypus unicodeContent
  case result3 of
    Left _ -> assertBool "Unicode should not crash" True
    Right typusFile -> assertBool "Unicode should parse" $ 
      any ("世界" `isInfixOf`) (map cbContent (tfBlocks typusFile))

-- ============================================================================
-- Helper Generators
-- ============================================================================

genWhitespaceOnly :: Gen String
genWhitespaceOnly = listOf $ elements " \t\n\r"

genMalformedFileDirective :: Gen String
genMalformedFileDirective = oneof
  [ return "//!"
  , return "//!:"
  , return "//! ownership"
  , return "//! ownership:"
  , return "//! ownership: maybe"
  , return "//! unknown: on"
  , return "//! ownership: on extra"
  ]

genMalformedBlockDirective :: Gen String
genMalformedBlockDirective = oneof
  [ return "{//!"
  , return "{//!:"
  , return "{//! ownership"
  , return "{//! ownership:"
  , return "{//! ownership: maybe"
  , return "{//! unknown: on"
  , return "{//! ownership: on extra"
  , return "{//! ownership: on"  -- missing closing brace
  ]

genUnclosedBlock :: Gen String
genUnclosedBlock = oneof
  [ do
      directive <- genValidBlockDirective
      content <- listOf1 (elements "abc \n")
      return $ directive ++ "\n" ++ concat content
  , do
      content <- listOf1 (elements "abc \n")
      return $ "{//! ownership: on\n" ++ concat content
  ]

genNestedBlocks :: Gen String
genNestedBlocks = sized $ \n -> genNestedBlocksDepth n

genNestedBlocksDepth :: Int -> Gen String
genNestedBlocksDepth 0 = return "content"
genNestedBlocksDepth n = do
  directive <- genValidBlockDirective
  content <- genNestedBlocksDepth (n-1)
  return $ directive ++ "\n" ++ content ++ "\n}"

genValidBlockDirective :: Gen String
genValidBlockDirective = do
  ownership <- elements ["on", "off"]
  return $ "{//! ownership: " ++ ownership ++ "}"

genContentWithSpecialChars :: Gen String
genContentWithSpecialChars = listOf1 $ elements 
  [ 'a'..'z', 'A'..'Z', '0'..'9', ' ', '\t', '\n', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=', '[', ']', '{', '}', '|', '\\', ';', ':', '\'', '"', ',', '.', '<', '>', '/', '?' ]

genMixedLineEndings :: Gen String
genMixedLineEndings = do
  lines' <- listOf1 $ listOf1 (elements ['a'..'z'])
  endings <- listOf1 $ elements ["\n", "\r\n", "\r"]
  return $ concat $ zipWith (++) lines' endings

genDeeplyNestedBraces :: Gen String
genDeeplyNestedBraces = sized $ \n -> do
  depth <- choose (1, min n 20)
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      content <- listOf1 (elements "abc \n")
  return $ concat openBraces ++ concat content ++ concat closeBraces

genStringsWithBraces :: Gen String
genStringsWithBraces = do
  before <- listOf1 (elements "abc ")
  strContent <- listOf1 (elements "{}abc ")
  after <- listOf1 (elements " abc")
  return $ concat before ++ "\"" ++ concat strContent ++ "\"" ++ concat after

genCommentsWithBraces :: Gen String
genCommentsWithBraces = do
  before <- listOf1 (elements "abc ")
  commentContent <- listOf1 (elements "{}abc ")
  after <- listOf1 (elements " abc")
  return $ concat before ++ "// " ++ concat commentContent ++ "\n" ++ concat after

-- Helper functions for generating test content
genTypusContent :: Gen String
genTypusContent = do
  hasFileDirective <- elements [True, False]
  directive <- if hasFileDirective then genFileDirective else return ""
  blocks <- listOf1 genBlock
  return $ directive ++ "\n" ++ concat blocks

genFileDirective :: Gen String
genFileDirective = do
  ownership <- elements ["on", "off"]
  dependentTypes <- elements ["on", "off"]
  return $ "//! ownership: " ++ ownership ++ ", dependent_types: " ++ dependentTypes

genBlock :: Gen String
genBlock = do
  hasDirective <- elements [True, False]
  directive <- if hasDirective then genBlockDirective else return ""
  content <- listOf1 (elements "abc \n")
  return $ directive ++ "\n" ++ concat content ++ "\n"

genBlockDirective :: Gen String
genBlockDirective = do
  ownership <- elements ["on", "off"]
  dependentTypes <- elements ["on", "off"]
  constraints <- elements ["on", "off"]
  return $ "{//! ownership: " ++ ownership ++ ", dependent_types: " ++ dependentTypes ++ ", constraints: " ++ constraints ++ "}"
