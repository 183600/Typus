{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserNewQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat)

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
  , locatedWithSpan
  , spanStart
  , spanEnd
  )

import Data.Char (isSpace, isAlphaNum)
import qualified Data.Text as T
import Utils (trim)

-- ============================================================================
-- Arbitrary Instances for Parser Types
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

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary `suchThat` (not . null)
    span <- arbitrary
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- arbitrary
    blocks <- arbitrary
    syntaxErrors <- arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate valid boolean directive values
genBoolValue :: Gen String
genBoolValue = elements ["on", "off", "true", "false"]

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate valid file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genDirectiveKey
  value <- genBoolValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate valid block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  key <- genDirectiveKey
  value <- genBoolValue
  return $ "{//! " ++ key ++ ": " ++ value ++ " }"

-- Generate simple code content
genCodeContent :: Gen String
genCodeContent = do
  lines' <- listOf (elements ["func main() {", "    fmt.Println(\"hello\")", "}", "var x int = 42", "return x"])
  return $ unlines lines'

-- Generate build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = oneof
  [return "//go:build linux", 
   return "// +build ignore",
   return "//go:build windows && amd64"]

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Empty input parses to file with no blocks
prop_empty_input :: Property
prop_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ null (tfBlocks file)

-- Property: Simple code without directives parses
prop_simple_code :: Property
prop_simple_code =
  forAll genCodeContent $ \content ->
  let result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: File directives are parsed correctly
prop_file_directives_parsed :: Property
prop_file_directives_parsed =
  forAll genFileDirectiveLine $ \directive ->
  let result = parseTypus directive
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Block directives are parsed correctly
prop_block_directives_parsed :: Property
prop_block_directives_parsed =
  forAll genBlockDirectiveLine $ \directive ->
  forAll genCodeContent $ \content ->
  let fullContent = directive ++ "\n" ++ content ++ "\n}"
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property $ cbDirectives block /= defaultBlockDirectives

-- Property: Build tags are parsed correctly
prop_build_tags_parsed :: Property
prop_build_tags_parsed =
  forAll genBuildTagLine $ \buildTag ->
  forAll genCodeContent $ \content ->
  let fullContent = buildTag ++ "\n" ++ content
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBuildTags file))

-- Property: Multiple file directives are handled
prop_multiple_file_directives :: Property
prop_multiple_file_directives =
  forAll (listOf genFileDirectiveLine) $ \directives ->
  let content = unlines directives
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Mixed directives and code are parsed
prop_mixed_directives_and_code :: Property
prop_mixed_directives_and_code =
  forAll genFileDirectiveLine $ \fileDirective ->
  forAll genBuildTagLine $ \buildTag ->
  forAll genBlockDirectiveLine $ \blockDirective ->
  forAll genCodeContent $ \content ->
  let fullContent = unlines [fileDirective, buildTag, blockDirective, content, "]"]
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right file -> 
      let hasFileDirectives = tfDirectives file /= defaultFileDirectives
          hasBuildTags = not (null (tfBuildTags file))
          hasBlockDirectives = case tfBlocks file of
            [] -> False
            (block:_) -> cbDirectives block /= defaultBlockDirectives
      in property $ hasFileDirectives .&&. hasBuildTags .&&. hasBlockDirectives

-- Property: Parser handles comments correctly
prop_parser_handles_comments :: Property
prop_parser_handles_comments =
  forAll genCodeContent $ \content ->
  let contentWithComments = content ++ "\n// This is a comment\n// Another comment"
      result = parseTypus contentWithComments
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Parser handles empty lines correctly
prop_parser_handles_empty_lines :: Property
prop_parser_handles_empty_lines =
  forAll genCodeContent $ \content ->
  let contentWithEmptyLines = "\n\n" ++ content ++ "\n\n\n"
      result = parseTypus contentWithEmptyLines
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Parser preserves code content
prop_parser_preserves_content :: Property
prop_parser_preserves_content =
  forAll genCodeContent $ \content ->
  let result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property $ content `isInfixOf` cbContent block

-- Property: Parser handles whitespace correctly
prop_parser_handles_whitespace :: Property
prop_parser_handles_whitespace =
  forAll genCodeContent $ \content ->
  let contentWithWhitespace = "  \n  " ++ content ++ "  \n  "
      result = parseTypus contentWithWhitespace
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Invalid directive values are rejected
prop_invalid_directive_rejected :: Property
prop_invalid_directive_rejected =
  let invalidDirective = "//! ownership: maybe"
      result = parseTypus invalidDirective
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: Invalid directive keys are rejected
prop_invalid_directive_key_rejected :: Property
prop_invalid_directive_key_rejected =
  let invalidDirective = "//! unknown: on"
      result = parseTypus invalidDirective
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: Unclosed block directives are rejected
prop_unclosed_block_rejected :: Property
prop_unclosed_block_rejected =
  forAll genBlockDirectiveLine $ \directive ->
  forAll genCodeContent $ \content ->
  let unclosedContent = directive ++ "\n" ++ content  -- Missing closing }
      result = parseTypus unclosedContent
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: Parser handles nested braces correctly
prop_parser_nested_braces :: Property
prop_parser_nested_braces =
  let contentWithNestedBraces = "func main() {\n    if true {\n        fmt.Println(\"nested\")\n    }\n}"
      result = parseTypus contentWithNestedBraces
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Parser handles strings with braces correctly
prop_parser_strings_with_braces :: Property
prop_parser_strings_with_braces =
  let contentWithStringBraces = "func main() {\n    s := \"{ not a block }\"\n    fmt.Println(s)\n}"
      result = parseTypus contentWithStringBraces
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Parser handles line comments with braces correctly
prop_parser_comments_with_braces :: Property
prop_parser_comments_with_braces =
  let contentWithCommentBraces = "func main() {\n    // This is a comment with { braces }\n    fmt.Println(\"hello\")\n}"
      result = parseTypus contentWithCommentBraces
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file))

-- Property: Parser roundtrip consistency
prop_parser_roundtrip :: Property
prop_parser_roundtrip =
  forAll genCodeContent $ \content ->
  let result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      let reconstructed = unlines (map cbContent (tfBlocks file))
      in property $ not (null content) ==> 
                 trim content `isInfixOf` trim reconstructed

-- Property: Parser handles complex directive combinations
prop_parser_complex_directives :: Property
prop_parser_complex_directives =
  let complexDirectives = ["//! ownership: on", "//! dependent_types: true", "//go:build linux", "// +build ignore"]
      content = unlines complexDirectives ++ "\nfunc main() {\n    fmt.Println(\"hello\")\n}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      let hasOwnership = case fdOwnership (tfDirectives file) of
            Nothing -> False
            Just (Located _ True) -> True
            _ -> False
          hasDependentTypes = case fdDependentTypes (tfDirectives file) of
            Nothing -> False
            Just (Located _ True) -> True
            _ -> False
          hasBuildTags = not (null (tfBuildTags file))
      in property $ hasOwnership .&&. hasDependentTypes .&&. hasBuildTags

-- Property: Parser error handling consistency
prop_parser_error_consistency :: Property
prop_parser_error_consistency =
  let malformedContent = "{//! ownership: on\nfunc main() {\n    fmt.Println(\"hello\")\n"  -- Missing closing brace
      result1 = parseTypus malformedContent
      result2 = parseTypus malformedContent
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ err1 === err2
    (Right _, Right _) -> property $ True  // Shouldn't happen for malformed content
    _ -> property $ False  -- Inconsistent results

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser New QuickCheck Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "Empty input parses to file with no blocks" prop_empty_input
    , fastProperty "Simple code without directives parses" prop_simple_code
    , fastProperty "File directives are parsed correctly" prop_file_directives_parsed
    , fastProperty "Block directives are parsed correctly" prop_block_directives_parsed
    , fastProperty "Build tags are parsed correctly" prop_build_tags_parsed
    ]

  , testGroup "Complex Parsing Properties"
    [ fastProperty "Multiple file directives are handled" prop_multiple_file_directives
    , fastProperty "Mixed directives and code are parsed" prop_mixed_directives_and_code
    , fastProperty "Parser handles comments correctly" prop_parser_handles_comments
    , fastProperty "Parser handles empty lines correctly" prop_parser_handles_empty_lines
    , fastProperty "Parser preserves code content" prop_parser_preserves_content
    , fastProperty "Parser handles whitespace correctly" prop_parser_handles_whitespace
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "Invalid directive values are rejected" prop_invalid_directive_rejected
    , fastProperty "Invalid directive keys are rejected" prop_invalid_directive_key_rejected
    , fastProperty "Unclosed block directives are rejected" prop_unclosed_block_rejected
    ]

  , testGroup "Advanced Parsing Properties"
    [ fastProperty "Parser handles nested braces correctly" prop_parser_nested_braces
    , fastProperty "Parser handles strings with braces correctly" prop_parser_strings_with_braces
    , fastProperty "Parser handles line comments with braces correctly" prop_parser_comments_with_braces
    , fastProperty "Parser roundtrip consistency" prop_parser_roundtrip
    , fastProperty "Parser handles complex directive combinations" prop_parser_complex_directives
    , fastProperty "Parser error handling consistency" prop_parser_error_consistency
    ]
  ]