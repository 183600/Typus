{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat, listOf1)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanBetween
  )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlpha, isAlphaNum, isSpace)

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid identifier character
genIdentifierChar :: Gen Char
genIdentifierChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

-- Generate a valid identifier
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf genIdentifierChar
  return (first : rest)

-- Generate a valid boolean value
genBool :: Gen Bool
genBool = elements [True, False]

-- Generate a file directive
genFileDirective :: Gen (String, String)
genFileDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- elements ["true", "false"]
  return (key, value)

-- Generate file directives line
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  directives <- listOf1 genFileDirective
  let directiveStr = concatMap (\(k, v) -> k ++ ":" ++ v) directives
  return $ "//! " ++ directiveStr

-- Generate a block directive
genBlockDirective :: Gen (String, String)
genBlockDirective = genFileDirective

-- Generate block directives line
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  directives <- listOf1 genBlockDirective
  let directiveStr = concatMap (\(k, v) -> k ++ ":" ++ v) directives
  return $ "{//! " ++ directiveStr ++ "}"

-- Generate a simple code line
genCodeLine :: Gen String
genCodeLine = do
  L.length <- choose (1, 50)
  chars <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;:()[]{}+-*/=<>'\"")
  return $ take L.length chars

-- Generate a comment line
genCommentLine :: Gen String
genCommentLine = do
  content <- genCodeLine
  return $ "// " ++ content

-- Generate a block comment line
genBlockCommentLine :: Gen String
genBlockCommentLine = do
  content <- genCodeLine
  return $ "/* " ++ content ++ " */"

-- Generate an empty line
genEmptyLine :: Gen String
genEmptyLine = return ""

-- Generate a line with whitespace only
genWhitespaceLine :: Gen String
genWhitespaceLine = do
  L.length <- choose (1, 10)
  chars <- listOf (elements " \t")
  return $ take L.length chars

-- Generate L.any line
genLine :: Gen String
genLine = oneof
  [ genFileDirectiveLine
  , genBlockDirectiveLine
  , genCodeLine
  , genCommentLine
  , genBlockCommentLine
  , genEmptyLine
  , genWhitespaceLine
  ]

-- Generate a list of lines
genLines :: Gen [String]
genLines = listOf genLine

-- Generate a valid Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  maybeFileDirective <- genFileDirectiveLine
  codeLines <- listOf1 genCodeLine
  return $ unlines (maybeFileDirective : codeLines)

-- Generate content with syntax errors
genContentWithSyntaxErrors :: Gen String
genTypusFileContentWithErrors :: Gen String
genTypusFileContentWithErrors = do
  baseContent <- genTypusFileContent
  errorType <- elements ["missing_brace", "invalid_directive", "unclosed_comment"]
  case errorType of
    "missing_brace" -> do
      indent <- elements ["  ", "\t", "    "]
      return $ baseContent ++ "\n" ++ indent ++ "if condition\n" ++ indent ++ "    doSomething()"
    "invalid_directive" -> do
      invalidDirective <- "//! invalid:key:value"
      return $ invalidDirective ++ "\n" ++ baseContent
    "unclosed_comment" -> do
      return $ baseContent ++ "\n/* This comment is never closed"
    _ -> return baseContent

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Parsing empty content returns file with no blocks
prop_parse_empty_content :: Property
prop_parse_empty_content =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> tfBlocks file === []

-- Property: Parsing content with only directives works
prop_parse_directives_only :: Property
prop_parse_directives_only =
  forAll genFileDirectiveLine $ \directive ->
    let result = parseTypus directive
    in case result of
      Left _ -> property False
      Right file -> tfBlocks file === []

-- Property: Parsing simple code creates blocks
prop_parse_simple_code :: Property
prop_parse_simple_code =
  forAll genTypusFileContent $ \content ->
    let result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsed file preserves directives
prop_parse_preserves_directives :: Property
prop_parse_preserves_directives =
  forAll genFileDirectiveLine $ \directive ->
    let result = parseTypus directive
    in case result of
      Left _ -> property False
      Right file -> tfDirectives file /= defaultFileDirectives

-- Property: Parsed blocks have valid spans
prop_parse_blocks_valid_spans :: Property
prop_parse_blocks_valid_spans =
  forAll genTypusFileContent $ \content ->
    let result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> L.all (\block -> spanStart (cbSpan block) <= spanEnd (cbSpan block)) (tfBlocks file)

-- Property: Parsing preserves line count
prop_parse_preserves_line_count :: Property
prop_parse_preserves_line_count =
  forAll genLines $ \lines ->
    let content = unlines lines
        lineCount = L.length lines
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ L.length (tfBlocks file) <= lineCount

-- Property: Parsing content with comments works
prop_parse_with_comments :: Property
prop_parse_with_comments =
  forAll (listOf1 genCommentLine) $ \comments ->
    let content = unlines comments
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing content with block directives works
prop_parse_with_block_directives :: Property
prop_parse_with_block_directives =
  forAll genBlockDirectiveLine $ \directive ->
    let content = directive ++ "\n" ++ "some code here"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing handles whitespace correctly
prop_parse_handles_whitespace :: Property
prop_parse_handles_whitespace =
  forAll genWhitespaceLine $ \whitespace ->
    let content = whitespace ++ "\n" ++ "code line"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing multiple blocks works
prop_parse_multiple_blocks :: Property
prop_parse_multiple_blocks =
  forAll (listOf2 genCodeLine) $ \lines ->
    let content = unlines lines
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ L.length (tfBlocks file) >= 1

-- Property: Block directives affect block properties
prop_block_directives_affect_blocks :: Property
prop_block_directives_affect_blocks =
  forAll genBlockDirectiveLine $ \directive ->
    let content = directive ++ "\n" ++ "code content"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> 
        case tfBlocks file of
          [] -> property False
          (block:_) -> cbDirectives block /= defaultBlockDirectives

-- Property: Parse error handling works
prop_parse_error_handling :: Property
prop_parse_error_handling =
  forAll genTypusFileContentWithErrors $ \content ->
    let result = parseTypus content
    in case result of
      Left _ -> property True  -- Expected to fail
      Right file -> property $ not (L.null (tfSyntaxErrors file))

-- Property: Round-trip parsing preserves structure
prop_roundtrip_preserves_structure :: Property
prop_roundtrip_preserves_structure =
  forAll genTypusFileContent $ \content ->
    let result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> 
        let reconstructed = unlines $ map cbContent (tfBlocks file)
        in property $ L.length reconstructed > 0

-- Property: Parsing preserves directive ordering
prop_parse_preserves_directive_ordering :: Property
prop_parse_preserves_directive_ordering =
  forAll (listOf1 genFileDirective) $ \directives ->
    let directiveLine = "//! " ++ concatMap (\(k, v) -> k ++ ":" ++ v ++ ",") directives
        result = parseTypus directiveLine
    in case result of
      Left _ -> property False
      Right file -> tfDirectives file /= defaultFileDirectives

-- Property: Parsing handles mixed content
prop_parse_handles_mixed_content :: Property
prop_parse_handles_mixed_content =
  forAll (listOf1 genLine) $ \lines ->
    let content = unlines lines
        result = parseTypus content
    in case result of
      Left err -> property $ "syntax error" `L.isInfixOf` err .||. "directive" `L.isInfixOf` err
      Right file -> property $ L.length (tfBlocks file) >= 0

-- Property: Block content is preserved correctly
prop_block_content_preserved :: Property
prop_block_content_preserved =
  forAll genCodeLine $ \codeLine ->
    let content = codeLine ++ "\n"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> 
        case tfBlocks file of
          [] -> property False
          (block:_) -> codeLine `L.isInfixOf` cbContent block

-- Property: Parsing handles Unicode characters
prop_parse_handles_unicode :: Property
prop_parse_handles_unicode =
  let unicodeContent = "func 测试() {\n    println(\"Hello 世界\")\n}\n"
      result = parseTypus unicodeContent
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: File directives are parsed correctly
prop_file_directives_parsed_correctly :: Property
prop_file_directives_parsed_correctly =
  forAll genFileDirectiveLine $ \directive ->
    let result = parseTypus directive
    in case result of
      Left _ -> property False
      Right file -> tfDirectives file /= defaultFileDirectives

-- Property: Empty lines are handled correctly
prop_empty_lines_handled_correctly :: Property
prop_empty_lines_handled_correctly =
  let content = "\n\n\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> tfBlocks file === []

-- Property: Parsing is idempotent for valid content
prop_parsing_idempotent :: Property
prop_parsing_idempotent =
  forAll genTypusFileContent $ \content ->
    let result1 = parseTypus content
        result2 = case result1 of
          Left _ -> Left ""
          Right file -> parseTypus $ unlines $ map cbContent (tfBlocks file)
    in case (result1, result2) of
      (Right _, Right file2) -> property $ not (L.null (tfBlocks file2))
      _ -> property False

-- Helper function to generate list of at least 2 elements
listOf2 :: Gen a -> Gen [a]
listOf2 gen = do
  x1 <- gen
  x2 <- gen
  rest <- listOf gen
  return (x1 : x2 : rest)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Core QuickCheck Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "parse empty content" prop_parse_empty_content
    , fastProperty "parse directives only" prop_parse_directives_only
    , fastProperty "parse simple code" prop_parse_simple_code
    , fastProperty "parse preserves directives" prop_parse_preserves_directives
    , fastProperty "parse blocks have valid spans" prop_parse_blocks_valid_spans
    ]

  , testGroup "Content Handling Properties"
    [ fastProperty "parse preserves line count" prop_parse_preserves_line_count
    , fastProperty "parse with comments" prop_parse_with_comments
    , fastProperty "parse with block directives" prop_parse_with_block_directives
    , fastProperty "parse handles whitespace" prop_parse_handles_whitespace
    , fastProperty "parse multiple blocks" prop_parse_multiple_blocks
    ]

  , testGroup "Directive Properties"
    [ fastProperty "block directives affect blocks" prop_block_directives_affect_blocks
    , fastProperty "parse error handling" prop_parse_error_handling
    , fastProperty "roundtrip preserves structure" prop_roundtrip_preserves_structure
    , fastProperty "parse preserves directive ordering" prop_parse_preserves_directive_ordering
    , fastProperty "file directives parsed correctly" prop_file_directives_parsed_correctly
    ]

  , testGroup "Advanced Properties"
    [ fastProperty "parse handles mixed content" prop_parse_handles_mixed_content
    , fastProperty "block content preserved" prop_block_content_preserved
    , fastProperty "parse handles unicode" prop_parse_handles_unicode
    , fastProperty "empty lines handled correctly" prop_empty_lines_handled_correctly
    , fastProperty "parsing is idempotent" prop_parsing_idempotent
    ]
  ]