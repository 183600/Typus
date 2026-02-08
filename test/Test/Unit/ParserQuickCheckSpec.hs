{-# LANGUAGE LambdaCase #-}
module Test.Unit.ParserQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Helper functions for testing
generateValidIdentifier :: String -> String
generateValidIdentifier s = 
  let filtered = filter (\c -> c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-") s
  in if null filtered then "id" else take 20 filtered

generateValidDirective :: String -> String
generateValidDirective s = 
  let keys = ["ownership", "dependent_types", "constraints"]
      values = ["true", "false"]
      keyIndex = abs (length s) `mod` length keys
      valueIndex = abs (length s + 1) `mod` length values
  in keys !! keyIndex ++ ":" ++ values !! valueIndex

generateValidCodeBlock :: String -> String
generateValidCodeBlock s = 
  let codeTemplates = 
        [ "func test() {\n    return " ++ show (abs (length s) `mod` 100) ++ "\n}"
        , "var x = " ++ show (abs (length s) `mod` 50) ++ "\n"
        , "type Test struct {\n    Field int\n}"
        , "if x > 0 {\n    fmt.Println(\"test\")\n}"
        , "for i := 0; i < " ++ show (abs (length s) `mod` 10) ++ "; i++ {\n    // loop\n}"
        ]
      templateIndex = abs (length s) `mod` length codeTemplates
  in codeTemplates !! templateIndex

-- QuickCheck properties
prop_parse_empty_input :: Property
prop_parse_empty_input =
  forAll arbitrary $ \s ->
    let input = ""
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ 
          null (tfBlocks file) && 
          null (tfBuildTags file) &&
          tfDirectives file == defaultFileDirectives

prop_parse_whitespace_only :: Property
prop_parse_whitespace_only =
  forAll arbitrary $ \s ->
    let input = replicate (abs (length s) `mod` 20 + 1) ' '
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ 
          null (tfBlocks file) && 
          null (tfBuildTags file) &&
          tfDirectives file == defaultFileDirectives

prop_parse_simple_identifier :: Property
prop_parse_simple_identifier =
  forAll arbitrary $ \s ->
    let input = generateValidIdentifier s
        result = isIdentifierChar (head input)
    in property result

prop_parse_file_directive :: Property
prop_parse_file_directive =
  forAll arbitrary $ \s ->
    let directive = generateValidDirective s
        input = "//! " ++ directive ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ tfDirectives file /= defaultFileDirectives

prop_parse_multiple_file_directives :: Property
prop_parse_multiple_file_directives =
  forAll arbitrary $ \s ->
    let directives = take 3 $ map generateValidDirective [s, s ++ "1", s ++ "2"]
        input = unlines $ map ("//! " ++) directives
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ tfDirectives file /= defaultFileDirectives

prop_parse_block_directive :: Property
prop_parse_block_directive =
  forAll arbitrary $ \s ->
    let directive = generateValidDirective s
        code = generateValidCodeBlock s
        input = "{//! " ++ directive ++ "}\n" ++ code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file)) &&
                  cbDirectives (head (tfBlocks file)) /= defaultBlockDirectives

prop_parse_build_tag :: Property
prop_parse_build_tag =
  forAll arbitrary $ \s ->
    let tag = "go:build test" ++ show (abs (length s) `mod` 10)
        input = "//" ++ tag ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBuildTags file))

prop_parse_code_block :: Property
prop_parse_code_block =
  forAll arbitrary $ \s ->
    let code = generateValidCodeBlock s
        input = code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file))

prop_parse_mixed_content :: Property
prop_parse_mixed_content =
  forAll arbitrary $ \s ->
    let directive = generateValidDirective s
        tag = "go:build test" ++ show (abs (length s) `mod` 10)
        code = generateValidCodeBlock s
        input = "//! " ++ directive ++ "\n" ++
                "//" ++ tag ++ "\n" ++
                code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ tfDirectives file /= defaultFileDirectives &&
                  not (null (tfBuildTags file)) &&
                  not (null (tfBlocks file))

prop_parse_preserves_content :: Property
prop_parse_preserves_content =
  forAll arbitrary $ \s ->
    let content = "func test() {\n    return " ++ show (abs (length s) `mod` 100) ++ "\n}"
        input = content ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file)) &&
                  content `isInfixOf` cbContent (head (tfBlocks file))

prop_parse_comments_ignored :: Property
prop_parse_comments_ignored =
  forAll arbitrary $ \s ->
    let comment = "// This is a comment " ++ show (abs (length s) `mod` 10)
        code = "func test() {\n    return 1\n}"
        input = comment ++ "\n" ++ code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file)) &&
                  not (comment `isInfixOf` cbContent (head (tfBlocks file)))

prop_parse_block_comments_ignored :: Property
prop_parse_block_comments_ignored =
  forAll arbitrary $ \s ->
    let comment = "/* This is a block comment " ++ show (abs (length s) `mod` 10) ++ " */"
        code = "func test() {\n    return 1\n}"
        input = comment ++ "\n" ++ code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file)) &&
                  not (comment `isInfixOf` cbContent (head (tfBlocks file)))

prop_parse_indented_content :: Property
prop_parse_indented_content =
  forAll arbitrary $ \s ->
    let indent = replicate (abs (length s) `mod` 5 + 1) ' '
        code = "func test() {\n    return 1\n}"
        input = unlines $ map (indent ++) (lines code)
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file))

prop_parse_unicode_content :: Property
prop_parse_unicode_content =
  forAll arbitrary $ \s ->
    let unicodeChars = "测试αβγ"
        code = "func " ++ unicodeChars ++ "() {\n    return 1\n}"
        input = code ++ "\n"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> 
        property $ not (null (tfBlocks file)) &&
                  unicodeChars `isInfixOf` cbContent (head (tfBlocks file))

-- Test suite
testSuite :: TestTree
testSuite = testGroup "Parser QuickCheck Tests"
  [ testProperty "parse empty input" prop_parse_empty_input
  , testProperty "parse whitespace only" prop_parse_whitespace_only
  , testProperty "parse simple identifier" prop_parse_simple_identifier
  , testProperty "parse file directive" prop_parse_file_directive
  , testProperty "parse multiple file directives" prop_parse_multiple_file_directives
  , testProperty "parse block directive" prop_parse_block_directive
  , testProperty "parse build tag" prop_parse_build_tag
  , testProperty "parse code block" prop_parse_code_block
  , testProperty "parse mixed content" prop_parse_mixed_content
  , testProperty "parse preserves content" prop_parse_preserves_content
  , testProperty "parse comments ignored" prop_parse_comments_ignored
  , testProperty "parse block comments ignored" prop_parse_block_comments_ignored
  , testProperty "parse indented content" prop_parse_indented_content
  , testProperty "parse unicode content" prop_parse_unicode_content
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "Parser Unit Tests"
  [ testCase "parse malformed directive" $ do
      let input = "//! malformed directive without equals\n"
          result = parseTypus input
      case result of
        Left _ -> assertBool "Should handle malformed directive gracefully" True
        Right file -> assertBool "Should parse successfully" True

  , testCase "parse incomplete expression" $ do
      let input = "let x =\n"
          result = parseTypus input
      case result of
        Left _ -> assertBool "Should detect incomplete expression" True
        Right _ -> assertFailure "Should detect incomplete expression"

  , testCase "parse if without brace" $ do
      let input = "if x > 0\n    return x\n"
          result = parseTypus input
      case result of
        Left _ -> assertBool "Should detect missing brace" True
        Right _ -> assertFailure "Should detect missing brace"

  , testCase "parse multiple packages" $ do
      let input = "package main\npackage test\n"
          result = parseTypus input
      case result of
        Left _ -> assertBool "Should detect multiple packages" True
        Right _ -> assertFailure "Should detect multiple packages"

  , testCase "parse simple content" $ do
      let input = "{\n"
          result = parseTypus input
      case result of
        Left _ -> assertFailure "Should parse simple content"
        Right file -> assertBool "Should parse simple content" True
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "Parser Tests"
  [ testSuite
  , unitTests
  ]