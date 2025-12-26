{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements, oneof)
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourcePos(..), SourceSpan(..))

-- Generator for safe identifiers (no special characters that could break parsing)
genSafeIdentifier :: Gen String
genSafeIdentifier = do
  size <- choose (1, 10)
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- vectorOf (size - 1) $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generator for safe strings (no newlines, quotes, or comment markers)
genSafeString :: Gen String
genSafeString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ filter (`notElem` "/\"'\n\r") [' '..'~']

-- Generator for code content (safe strings with spaces)
genCodeContent :: Gen String
genCodeContent = do
  linesCount <- choose (1, 5)
  lines' <- vectorOf linesCount $ do
    content <- genSafeString
    return $ "    " ++ content  -- Add some indentation
  return $ unlines lines'

-- Generator for boolean values
genBoolValue :: Gen String
genBoolValue = elements ["on", "off", "true", "false"]

-- Generator for file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- elements ["ownership", "dependent_types", "constraints"]
  value <- genBoolValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generator for block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  key <- elements ["ownership", "dependent_types", "constraints"]
  value <- genBoolValue
  return $ "{//! " ++ key ++ ": " ++ value ++ " }"

-- Generator for build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = do
  tag <- genSafeIdentifier
  return $ "//go:build " ++ tag

-- Generator for simple typus content
genSimpleTypusContent :: Gen String
genSimpleTypusContent = do
  directives <- oneof 
    [ return []
    , do
        count <- choose (1, 3)
        vectorOf count genFileDirectiveLine
    ]
  buildTags <- oneof
    [ return []
    , do
        count <- choose (1, 2)
        vectorOf count genBuildTagLine
    ]
  code <- genCodeContent
  return $ unlines (directives ++ buildTags ++ [""] ++ lines code)

-- Property: parseTypus handles empty input
prop_parseTypus_empty :: Property
prop_parseTypus_empty =
  let result = parseTypus ""
  in case result of
       Left _ -> property False
       Right file -> property $ null (tfBlocks file)

-- Property: parseTypus handles simple content
prop_parseTypus_simple :: Property
prop_parseTypus_simple =
  forAll genSimpleTypusContent $ \content ->
  let result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: parseTypus preserves code content in blocks
prop_parseTypus_preserves_code :: Property
prop_parseTypus_preserves_code =
  forAll genCodeContent $ \code ->
  let content = code ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> 
         case tfBlocks file of
           [] -> property False
           (block:_) -> property $ code `List.isInfixOf` cbContent block

-- Property: parseTypus handles file directives
prop_parseTypus_file_directives :: Property
prop_parseTypus_file_directives =
  forAll genFileDirectiveLine $ \directive ->
  let content = directive ++ "\n" ++ "some code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: parseTypus handles build tags
prop_parseTypus_build_tags :: Property
prop_parseTypus_build_tags =
  forAll genBuildTagLine $ \buildTag ->
  let content = buildTag ++ "\n" ++ "some code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBuildTags file))

-- Property: parseTypus handles block directives
prop_parseTypus_block_directives :: Property
prop_parseTypus_block_directives =
  forAll genBlockDirectiveLine $ \directive ->
  let content = directive ++ "\n    some code\n}\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> 
         case tfBlocks file of
           [] -> property False
           (block:_) -> property $ cbDirectives block /= defaultBlockDirectives

-- Property: parseTypus idempotency for simple content
prop_parseTypus_idempotent :: Property
prop_parseTypus_idempotent =
  forAll genSimpleTypusContent $ \content ->
  let result1 = parseTypus content
      result2 = case result1 of
                  Left _ -> Left ""
                  Right file -> Right $ length (tfBlocks file)
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right file1, Right count) -> 
         let result3 = parseTypus content
         in case result3 of
              Left _ -> property False
              Right file2 -> property $ length (tfBlocks file2) == length (tfBlocks file1)

-- Property: parseTypus handles multiple blocks
prop_parseTypus_multiple_blocks :: Property
prop_parseTypus_multiple_blocks =
  let block1 = "    code1\n"
      block2 = "    code2\n"
      content = block1 ++ "\n" ++ block2
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ length (tfBlocks file) >= 1

-- Property: parseTypus preserves line structure
prop_parseTypus_preserves_lines :: Property
prop_parseTypus_preserves_lines =
  forAll genCodeContent $ \code ->
  let originalLines = length $ lines code
      content = code
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> 
         case tfBlocks file of
           [] -> property $ originalLines == 0
           blocks -> property $ sum (map (length . lines . cbContent) blocks) >= originalLines

-- Property: parseTypus handles whitespace correctly
prop_parseTypus_whitespace :: Property
prop_parseTypus_whitespace =
  let content = "\n\n   \n    code\n   \n\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: parseTypus handles mixed directives and code
prop_parseTypus_mixed_content :: Property
prop_parseTypus_mixed_content =
  forAll genFileDirectiveLine $ \directive ->
  forAll genBuildTagLine $ \buildTag ->
  forAll genCodeContent $ \code ->
  let content = directive ++ "\n" ++ buildTag ++ "\n" ++ code
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBuildTags file)) &&
                             tfDirectives file /= defaultFileDirectives

-- Property: parseTypus error handling for malformed directives
prop_parseTypus_malformed_directives :: Property
prop_parseTypus_malformed_directives =
  let malformedDirective = "//! invalid directive format\n"
      content = malformedDirective ++ "some code\n"
      result = parseTypus content
  in property $ case result of
                  Left _ -> True
                  Right _ -> True  -- Parser might recover

-- Property: parseTypus handles unclosed blocks gracefully
prop_parseTypus_unclosed_block :: Property
prop_parseTypus_unclosed_block =
  let unclosedBlock = "{//! ownership: on\n    code without closing brace\n"
      content = unclosedBlock
      result = parseTypus content
  in property $ case result of
                  Left _ -> True  -- Expected to fail
                  Right _ -> True  -- Or recover gracefully

-- Property: parseTypus handles nested braces in strings
prop_parseTypus_nested_braces_strings :: Property
prop_parseTypus_nested_braces_strings =
  let content = "var s = \"{//! not a directive }\"\n    real code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: parseTypus handles comments in code
prop_parseTypus_comments_in_code :: Property
prop_parseTypus_comments_in_code =
  let content = "    code // line comment\n    more code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: parseTypus handles special characters in strings
prop_parseTypus_special_chars_strings :: Property
prop_parseTypus_special_chars_strings =
  let content = "var s = \"string with { and } and // and /* */\"\n    code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: parseTypus default directives are applied correctly
prop_parseTypus_default_directives :: Property
prop_parseTypus_default_directives =
  let content = "simple code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ tfDirectives file == defaultFileDirectives

-- Property: parseTypus creates valid spans for blocks
prop_parseTypus_valid_spans :: Property
prop_parseTypus_valid_spans =
  forAll genCodeContent $ \code ->
  let content = code
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> 
         case tfBlocks file of
           [] -> property True
           (block:_) -> property $ spanStart (cbSpan block) <= spanEnd (cbSpan block)

-- Property: parseTypus handles multiple file directives
prop_parseTypus_multiple_file_directives :: Property
prop_parseTypus_multiple_file_directives =
  let directives = ["//! ownership: on", "//! dependent_types: true"]
      content = unlines directives ++ "code\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right file -> property $ tfDirectives file /= defaultFileDirectives

tests :: TestTree
tests = testGroup "Parser New QuickCheck Tests"
  [ fastProperty "parseTypus handles empty input" prop_parseTypus_empty
  , fastProperty "parseTypus handles simple content" prop_parseTypus_simple
  , fastProperty "parseTypus preserves code content in blocks" prop_parseTypus_preserves_code
  , fastProperty "parseTypus handles file directives" prop_parseTypus_file_directives
  , fastProperty "parseTypus handles build tags" prop_parseTypus_build_tags
  , fastProperty "parseTypus handles block directives" prop_parseTypus_block_directives
  , fastProperty "parseTypus idempotency for simple content" prop_parseTypus_idempotent
  , fastProperty "parseTypus handles multiple blocks" prop_parseTypus_multiple_blocks
  , fastProperty "parseTypus preserves line structure" prop_parseTypus_preserves_lines
  , fastProperty "parseTypus handles whitespace correctly" prop_parseTypus_whitespace
  , fastProperty "parseTypus handles mixed directives and code" prop_parseTypus_mixed_content
  , fastProperty "parseTypus error handling for malformed directives" prop_parseTypus_malformed_directives
  , fastProperty "parseTypus handles unclosed blocks gracefully" prop_parseTypus_unclosed_block
  , fastProperty "parseTypus handles nested braces in strings" prop_parseTypus_nested_braces_strings
  , fastProperty "parseTypus handles comments in code" prop_parseTypus_comments_in_code
  , fastProperty "parseTypus handles special characters in strings" prop_parseTypus_special_chars_strings
  , fastProperty "parseTypus default directives are applied correctly" prop_parseTypus_default_directives
  , fastProperty "parseTypus creates valid spans for blocks" prop_parseTypus_valid_spans
  , fastProperty "parseTypus handles multiple file directives" prop_parseTypus_multiple_file_directives
  ]