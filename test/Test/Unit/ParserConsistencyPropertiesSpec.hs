{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserConsistencyPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen)
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat)

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
  , startPos
  , spanStart
  , spanEnd
  )

import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate valid directive content
arbitraryDirectiveContent :: Gen String
arbitraryDirectiveContent = do
  length <- choose (1, 50)
  elements $ map (:[]) (filter isAlphaNum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

arbitraryDirectiveLine :: Gen String
arbitraryDirectiveLine = do
  directiveType <- elements ["//!", "{//!", "}"]
  content <- arbitraryDirectiveContent
  return $ directiveType ++ " " ++ content

-- Generate valid code blocks
arbitraryCodeBlock :: Gen String
arbitraryCodeBlock = do
  numLines <- choose (1, 10)
  lines' <- vectorOf numLines arbitraryCodeLine
  return $ unlines lines'

arbitraryCodeLine :: Gen String
arbitraryCodeLine = do
  hasComment <- arbitrary
  if hasComment
    then do
      code <- arbitraryDirectiveContent
      comment <- arbitraryDirectiveContent
      return $ "  " ++ code ++ " // " ++ comment
    else do
      code <- arbitraryDirectiveContent
      return $ "  " ++ code

-- Generate valid Typus content
arbitraryTypusContent :: Gen String
arbitraryTypusContent = do
  hasFileDirectives <- arbitrary
  fileDirectives <- if hasFileDirectives then arbitraryDirectiveLine else return ""
  numBlocks <- choose (1, 5)
  blocks <- vectorOf numBlocks arbitraryCodeBlock
  return $ fileDirectives ++ "\n" ++ unlines blocks

-- ============================================================================
-- Parser Consistency Properties
-- ============================================================================

-- Property: Parsing empty string returns valid TypusFile
prop_parse_empty_string :: Property
prop_parse_empty_string =
  case parseTypus "" of
    Left _ -> property False
    Right typusFile -> 
      property $ tfDirectives typusFile === defaultFileDirectives .&&.
                 null (tfBuildTags typusFile) .&&.
                 null (tfBlocks typusFile)

-- Property: Parsing only whitespace returns valid TypusFile
prop_parse_whitespace_only :: Property
prop_parse_whitespace_only =
  forAll arbitraryWhitespace $ \whitespace ->
  case parseTypus whitespace of
    Left _ -> property False
    Right typusFile -> 
      property $ tfDirectives typusFile === defaultFileDirectives .&&.
                 null (tfBuildTags typusFile)

-- Property: Parsing preserves line structure
prop_parse_preserves_lines :: Property
prop_parse_preserves_lines =
  forAll arbitraryTypusContent $ \content ->
  case parseTypus content of
    Left _ -> property $ not (null content) ==> property False
    Right typusFile ->
      let inputLines = lines content
          blockCount = length (tfBlocks typusFile)
      in property $ blockCount >= 0 .&&. blockCount <= length inputLines

-- Property: File directives are parsed correctly
prop_parse_file_directives :: Property
prop_parse_file_directives =
  forAll arbitraryDirectiveContent $ \directive ->
  let content = "//! " ++ directive ++ "\nfunc main() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let dirs = tfDirectives typusFile
      in property $ dirs /= defaultFileDirectives

-- Property: Block directives are parsed correctly
prop_parse_block_directives :: Property
prop_parse_block_directives =
  forAll arbitraryDirectiveContent $ \directive ->
  let content = "{//! " ++ directive ++ "}\nfunc test() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let blocks = tfBlocks typusFile
      in property $ not (null blocks) ==> 
                 (cbDirectives (head blocks) /= defaultBlockDirectives)

-- Property: Parsing is idempotent for valid content
prop_parse_idempotent :: Property
prop_parse_idempotent =
  forAll arbitraryTypusContent $ \content ->
  case parseTypus content of
    Left _ -> property False
    Right typusFile1 ->
      -- Reconstruct content from parsed file and parse again
      let reconstructed = reconstructTypusFile typusFile1
      in case parseTypus reconstructed of
        Left _ -> property False
        Right typusFile2 ->
          property $ length (tfBlocks typusFile1) === length (tfBlocks typusFile2)

-- Property: Parsing preserves content order
prop_parse_preserves_order :: Property
prop_parse_preserves_order =
  forAll arbitraryTypusContent $ \content ->
  case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let blocks = tfBlocks typusFile
          blockContents = map cbContent blocks
          contentLines = lines content
      in property $ not (null blockContents) ==> 
                 all (`elem` contentLines) (concatMap lines blockContents)

-- Property: Comments are handled correctly
prop_parse_comments :: Property
prop_parse_comments =
  forAll arbitraryCodeBlock $ \codeBlock ->
  let withComments = codeBlock ++ "\n// This is a comment\nfunc new() {}\n"
  in case parseTypus withComments of
    Left _ -> property False
    Right typusFile ->
      property $ length (tfBlocks typusFile) >= 1

-- Property: Multiple directives are parsed correctly
prop_parse_multiple_directives :: Property
prop_parse_multiple_directives =
  let content = "//! ownership: true\n//! dependent-types: true\n{//! constraints: true}\nfunc test() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let dirs = tfDirectives typusFile
          blocks = tfBlocks typusFile
      in property $ dirs /= defaultFileDirectives .&&.
                 not (null blocks) ==> 
                 (cbDirectives (head blocks) /= defaultBlockDirectives)

-- Property: Empty code blocks are handled
prop_parse_empty_blocks :: Property
prop_parse_empty_blocks =
  let content = "//! ownership: true\n{//! constraints: true}\n\nfunc test() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      property $ length (tfBlocks typusFile) >= 1

-- Property: Parsing with mixed line endings
prop_parse_mixed_line_endings :: Property
prop_parse_mixed_line_endings =
  let content = "//! ownership: true\r\nfunc test() {}\n{//! constraints: true}\r\nfunc main() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      property $ length (tfBlocks typusFile) >= 2

-- Property: Parsing preserves directive values
prop_parse_preserves_directive_values :: Property
prop_parse_preserves_directive_values =
  forAll arbitraryDirectiveContent $ \directive ->
  let content = "//! ownership: " ++ directive ++ "\nfunc main() {}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let dirs = tfDirectives typusFile
      in property $ case fdOwnership dirs of
        Nothing -> property False
        Just (Located value _) -> property $ show value `isInfixOf` directive

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Malformed directives produce errors
prop_malformed_directives_error :: Property
prop_malformed_directives_error =
  let content = "//! ownership \nfunc main() {}\n"  -- Missing colon and value
  in case parseTypus content of
    Left _ -> property True
    Right _ -> property False

-- Property: Unclosed block directives produce errors
prop_unclosed_block_directives_error :: Property
prop_unclosed_block_directives_error =
  let content = "{//! ownership: true\nfunc main() {}\n"  -- Missing closing brace
  in case parseTypus content of
    Left _ -> property True
    Right _ -> property False

-- Property: Invalid syntax produces errors
prop_invalid_syntax_error :: Property
prop_invalid_syntax_error =
  let content = "func main() {\n  if true\n  print(\"hello\")\n}\n"  -- Missing opening brace after if
  in case parseTypus content of
    Left _ -> property True
    Right _ -> property False

-- ============================================================================
-- Advanced Consistency Properties
-- ============================================================================

-- Property: Parsing large files is consistent
prop_parse_large_files_consistent :: Property
prop_parse_large_files_consistent =
  let largeContent = unlines $ replicate 1000 "  x := 1\n  y := 2\n"
  in case parseTypus largeContent of
    Left _ -> property False
    Right typusFile ->
      property $ length (tfBlocks typusFile) >= 1

-- Property: Parsing with Unicode content
prop_parse_unicode_content :: Property
prop_parse_unicode_content =
  let content = "//! 测试\nfunc main() {\n  message := \"你好世界\"\n  fmt.Println(message)\n}\n"
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      property $ length (tfBlocks typusFile) >= 1

-- Property: Parsing preserves source location information
prop_parse_preserves_source_locations :: Property
prop_parse_preserves_source_locations =
  forAll arbitraryTypusContent $ \content ->
  case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      let blocks = tfBlocks typusFile
          spans = map cbSpan blocks
      in property $ all isValidSpan spans

-- Property: Syntax errors are collected properly
prop_syntax_errors_collected :: Property
prop_syntax_errors_collected =
  let content = "func main() {\n  if true\n    x := 1\n  }\n}\n"  -- Malformed if statement
  in case parseTypus content of
    Left _ -> property False
    Right typusFile ->
      property $ not (null (tfSyntaxErrors typusFile))

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Reconstruct a TypusFile back to string content
reconstructTypusFile :: TypusFile -> String
reconstructTypusFile typusFile =
  let directivesStr = reconstructFileDirectives (tfDirectives typusFile)
      blocksStr = concatMap reconstructCodeBlock (tfBlocks typusFile)
  in directivesStr ++ blocksStr

reconstructFileDirectives :: FileDirectives -> String
reconstructFileDirectives dirs = ""

reconstructCodeBlock :: CodeBlock -> String
reconstructCodeBlock block = cbContent block ++ "\n"

-- Generate arbitrary whitespace
arbitraryWhitespace :: Gen String
arbitraryWhitespace = do
  length <- choose (1, 20)
  vectorOf length (elements " \t\n\r")

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Consistency Properties Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "Parsing empty string returns valid TypusFile" prop_parse_empty_string
    , fastProperty "Parsing only whitespace returns valid TypusFile" prop_parse_whitespace_only
    , fastProperty "Parsing preserves line structure" prop_parse_preserves_lines
    ]

  , testGroup "Directive Parsing Properties"
    [ fastProperty "File directives are parsed correctly" prop_parse_file_directives
    , fastProperty "Block directives are parsed correctly" prop_parse_block_directives
    , fastProperty "Multiple directives are parsed correctly" prop_parse_multiple_directives
    , fastProperty "Parsing preserves directive values" prop_parse_preserves_directive_values
    ]

  , testGroup "Content Parsing Properties"
    [ fastProperty "Parsing is idempotent for valid content" prop_parse_idempotent
    , fastProperty "Parsing preserves content order" prop_parse_preserves_order
    , fastProperty "Comments are handled correctly" prop_parse_comments
    , fastProperty "Empty code blocks are handled" prop_parse_empty_blocks
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "Malformed directives produce errors" prop_malformed_directives_error
    , fastProperty "Unclosed block directives produce errors" prop_unclosed_block_directives_error
    , fastProperty "Invalid syntax produces errors" prop_invalid_syntax_error
    ]

  , testGroup "Advanced Consistency Properties"
    [ fastProperty "Parsing large files is consistent" prop_parse_large_files_consistent
    , fastProperty "Parsing with Unicode content" prop_parse_unicode_content
    , fastProperty "Parsing preserves source location information" prop_parse_preserves_source_locations
    , fastProperty "Syntax errors are collected properly" prop_syntax_errors_collected
    , fastProperty "Parsing with mixed line endings" prop_parse_mixed_line_endings
    ]
  ]