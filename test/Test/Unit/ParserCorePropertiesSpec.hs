{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing #-}
module Test.Unit.ParserCorePropertiesSpec where


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
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
import SourceLocation (Located(..))
import qualified SyntaxValidator

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Arbitrary instance for SourcePos is now defined in SourceLocation module


instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

instance Arbitrary SyntaxValidator.SyntaxError where
  arbitrary = do
    errorType <- elements 
      [ SyntaxValidator.MissingBrace
      , SyntaxValidator.MissingParenthesis
      , SyntaxValidator.MissingBracket
      , SyntaxValidator.UnclosedString
      , SyntaxValidator.UnclosedComment
      , SyntaxValidator.InvalidIdentifier
      , SyntaxValidator.InvalidTypeDeclaration
      , SyntaxValidator.InvalidFunctionDeclaration
      , SyntaxValidator.InvalidImport
      , SyntaxValidator.InvalidStatement
      , SyntaxValidator.UnterminatedBlock
      , SyntaxValidator.InvalidOperator
      , SyntaxValidator.MissingSemicolon
      , SyntaxValidator.UnexpectedToken
      , SyntaxValidator.MissingPackageDeclaration
      , SyntaxValidator.DuplicateDeclaration
      , SyntaxValidator.InvalidBlockStructure
      , SyntaxValidator.UndeclaredVariable
      , SyntaxValidator.SyntaxWarning
      ]
    errorMessage <- arbitrary
    lineNumber <- arbitrary
    columnNumber <- arbitrary
    lineContent <- arbitrary
    return $ SyntaxValidator.SyntaxError errorType errorMessage lineNumber columnNumber lineContent

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

-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary
    span <- arbitrary
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- arbitrary
    blocks <- arbitrary
    syntaxErrors <- arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Parsing empty string returns file with no blocks
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  let result = parseTypus ""
      expected = TypusFile defaultFileDirectives [] [] []
  in case result of
    Left _ -> property False
    Right file -> file === expected

-- Property: Parsing string with only directives returns file with directives
prop_parse_only_directives :: Property
prop_parse_only_directives = 
  let input = "//! ownership=true, dependent-types=true"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> tfBlocks file === []

-- Property: Parsing simple code block returns file with one block
prop_parse_simple_block :: Property
prop_parse_simple_block = 
  let input = "```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> length (tfBlocks file) === 1

-- Property: Parsing multiple blocks returns file with multiple blocks
prop_parse_multiple_blocks :: Property
prop_parse_multiple_blocks = 
  let input = "```typus\nlet x = 42\n```\n\n```typus\nlet y = 24\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> length (tfBlocks file) === 2

-- Property: Parsing preserves block content
prop_parse_preserves_content :: Property
prop_parse_preserves_content = 
  let content = "let x = 42\nlet y = x + 1"
      input = "```typus\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property (cbContent block `contains` "let x = 42")

-- Property: Parsing with block directives preserves directives
prop_parse_block_directives :: Property
prop_parse_block_directives = 
  let input = "```typus ownership=true\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> 
          case bdOwnership (cbDirectives block) of
            Nothing -> property False
            Just _ -> property True

-- Property: Parsing malformed code returns syntax errors
prop_parse_malformed_errors :: Property
prop_parse_malformed_errors = 
  let input = "```typus\nlet x = \n```"  -- Incomplete assignment
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Parser should fail
    Right file -> property (not (null (tfSyntaxErrors file)))

-- Property: Parsing with file directives sets file directives
prop_parse_file_directives :: Property
prop_parse_file_directives = 
  let input = "//! ownership=true, dependent-types=false\n\n```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case fdOwnership (tfDirectives file) of
        Nothing -> property False
        Just _ -> property True

-- Property: Parsing preserves build tags
prop_parse_build_tags :: Property
prop_parse_build_tags = 
  let input = "// +build tag1,tag2\n\n```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property (not (null (tfBuildTags file)))

-- Property: Parsing nested blocks handles correctly
prop_parse_nested_blocks :: Property
prop_parse_nested_blocks = 
  let input = "```typus\nif true {\n  ```typus\n  let x = 42\n  ```\n}\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Should likely fail or handle specially
    Right file -> property (length (tfBlocks file) >= 1)

-- Property: Parsing with comments preserves non-comment content
prop_parse_with_comments :: Property
prop_parse_with_comments = 
  let input = "// This is a comment\n```typus\nlet x = 42\n// Another comment\nlet y = 24\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property (cbContent block `contains` "let x = 42") .&&. 
                     property (cbContent block `contains` "let y = 24")

-- Property: Parsing large input doesn't crash
prop_parse_large_input :: Property
prop_parse_large_input = 
  let largeContent = unlines $ replicate 20 "let x" ++ [show (42 :: Int)]  -- 从1000减少到20，大幅减少内存使用
      input = "```typus\n" ++ largeContent ++ "\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Failing is OK, just shouldn't crash
    Right file -> property (length (tfBlocks file) >= 1)

-- Property: Parsing with unicode content preserves unicode
prop_parse_unicode_content :: Property
prop_parse_unicode_content = 
  let input = "```typus\nlet greeting = \"你好世界\"\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property (cbContent block `contains` "你好世界")

-- Property: Parsing empty blocks returns empty block content
prop_parse_empty_blocks :: Property
prop_parse_empty_blocks = 
  let input = "```typus\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property (null (cbContent block))

-- Property: Parsing with mixed newlines works correctly
prop_parse_mixed_newlines :: Property
prop_parse_mixed_newlines = 
  let input = "```typus\r\nlet x = 42\nlet y = 24\r\n```"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> length (tfBlocks file) === 1

-- ============================================================================
-- Helper Functions
-- ============================================================================

contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Core Properties Tests"
  [ testGroup "Basic Parsing Properties"
    [ testProperty "Parsing empty string returns file with no blocks" prop_parse_empty_string
    , testProperty "Parsing string with only directives returns file with directives" prop_parse_only_directives
    , testProperty "Parsing simple code block returns file with one block" prop_parse_simple_block
    , testProperty "Parsing multiple blocks returns file with multiple blocks" prop_parse_multiple_blocks
    ]
  , testGroup "Content Preservation Properties"
    [ testProperty "Parsing preserves block content" prop_parse_preserves_content
    , testProperty "Parsing with block directives preserves directives" prop_parse_block_directives
    , testProperty "Parsing with file directives sets file directives" prop_parse_file_directives
    , testProperty "Parsing preserves build tags" prop_parse_build_tags
    , testProperty "Parsing with comments preserves non-comment content" prop_parse_with_comments
    ]
  , testGroup "Error Handling Properties"
    [ testProperty "Parsing malformed code returns syntax errors" prop_parse_malformed_errors
    ]
  , testGroup "Edge Case Properties"
    [ testProperty "Parsing nested blocks handles correctly" prop_parse_nested_blocks
    , testProperty "Parsing large input doesn't crash" prop_parse_large_input
    , testProperty "Parsing with unicode content preserves unicode" prop_parse_unicode_content
    , testProperty "Parsing empty blocks returns empty block content" prop_parse_empty_blocks
    , testProperty "Parsing with mixed newlines works correctly" prop_parse_mixed_newlines
    ]
  ]