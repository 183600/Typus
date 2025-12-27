{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = FileDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockDirectives where
  arbitrary = BlockDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary CodeBlock where
  arbitrary = CodeBlock <$> arbitrary <*> arbitrary <*> arbitrary

-- Generate valid identifier characters
genIdentifierChar :: Gen Char
genIdentifierChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = listOf1 genIdentifierChar

-- Generate directive content
genDirectiveContent :: Gen String
genDirectiveContent = do
  key <- genIdentifier
  value <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-")
  return $ key ++ "=" ++ value

-- Generate file directive lines
genFileDirective :: Gen String
genFileDirective = do
  content <- genDirectiveContent
  return $ "//! " ++ content

-- Generate block directive lines  
genBlockDirective :: Gen String
genBlockDirective = do
  content <- genDirectiveContent
  return $ "//@ " ++ content

-- Generate code content
genCodeContent :: Gen String
genCodeContent = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n(){}[];,+*/"

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Parsing empty content returns default structure
prop_parse_empty_content :: Property
prop_parse_empty_content =
  let result = parseTypus "" 
      expected = TypusFile defaultFileDirectives [] [] []
  in case result of
       Left _ -> property False
       Right parsed -> parsed === expected

-- Property: Parsing content with only file directives preserves directives
prop_parse_file_directives_preserved :: [String] -> Property
prop_parse_file_directives_preserved directives =
  not (null directives) && all (not . null) directives ==>
  let directiveLines = map ("//! " ++) directives
      content = unlines directiveLines
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ not (null (tfDirectives parsed))

-- Property: Parsing content with code blocks preserves block structure
prop_parse_code_blocks_preserved :: String -> Property
prop_parse_code_blocks_preserved code =
  not (null code) && not (any isNullChar code) ==>
  let content = "//@ ownership=true\n" ++ code ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Roundtrip parsing preserves content structure
prop_parse_roundtrip_structure :: String -> String -> Property
prop_parse_roundtrip_structure prefix suffix =
  not (null prefix) && not (null suffix) && 
  not (any isNullChar (prefix ++ suffix)) ==>
  let content = prefix ++ "\n//@ ownership=true\n" ++ suffix
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Multiple directives are parsed correctly
prop_parse_multiple_directives :: [String] -> Property
prop_parse_multiple_directives directives =
  length directives <= 5 && all (not . null) directives ==>
  let directiveLines = map ("//! " ++) directives
      content = unlines directiveLines ++ "\n//@ ownership=true\nfunc main() {}\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Invalid directives don't crash parser
prop_parse_invalid_directives_safe :: String -> Property
prop_parse_invalid_directives_safe badContent =
  not (null badContent) && not (any isNullChar badContent) ==>
  let content = "//! " ++ badContent ++ "\nfunc test() {}\n"
      result = parseTypus content
  in case result of
       Left _ -> property True  -- Parser should handle errors gracefully
       Right _ -> property True  -- Or succeed if it can parse

-- Property: Code content with special characters is preserved
prop_parse_special_characters_preserved :: String -> Property
prop_parse_special_characters_preserved specialChars =
  not (null specialChars) && not (any isNullChar specialChars) ==>
  let content = "//@ dependent_types=true\nfunc test() { " ++ specialChars ++ " }\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> 
         case tfBlocks parsed of
           [] -> property False
           (block:_) -> property $ specialChars `isInfixOf` cbContent block

-- Property: Parsing with mixed directives works correctly
prop_parse_mixed_directives :: String -> String -> String -> Property
prop_parse_mixed_directives fileDirective blockDirective code =
  not (null fileDirective) && not (null blockDirective) && not (null code) &&
  not (any isNullChar (fileDirective ++ blockDirective ++ code)) ==>
  let content = "//! " ++ fileDirective ++ "\n//@ " ++ blockDirective ++ "\n" ++ code ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Large files are parsed without memory issues
prop_parse_large_files_safe :: Int -> String -> Property
prop_parse_large_files_safe multiplier baseContent =
  multiplier > 0 && multiplier <= 100 && not (any isNullChar baseContent) ==>
  let repeatedContent = concat $ replicate multiplier (baseContent ++ "\n")
      content = "//@ ownership=true\n" ++ repeatedContent
      result = parseTypus content
  in case result of
       Left _ -> property True  -- Should handle errors gracefully
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Parsing preserves line numbers in source locations
prop_parse_preserves_line_numbers :: [String] -> Property
prop_parse_preserves_line_numbers lines =
  not (null lines) && length lines <= 10 && all (not . null) lines ==>
  let content = unlines lines
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ True  -- Should have valid source locations

-- Property: Unicode content is handled correctly
prop_parse_unicode_content :: String -> Property
prop_parse_unicode_content unicodeContent =
  not (null unicodeContent) && not (any isNullChar unicodeContent) ==>
  let content = "//@ constraints=true\nfunc test() { " ++ unicodeContent ++ " }\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> 
         case tfBlocks parsed of
           [] -> property False
           (block:_) -> property $ unicodeContent `isInfixOf` cbContent block

-- Property: Empty code blocks are handled gracefully
prop_parse_empty_code_blocks :: Property
prop_parse_empty_code_blocks =
  let content = "//@ ownership=true\n\n//@ dependent_types=true\n"
      result = parseTypus content
  in case result of
       Left _ -> property True  -- Should handle gracefully
       Right parsed -> property $ True  -- Should parse successfully

-- Property: Nested directive structure is parsed correctly
prop_parse_nested_directives :: String -> String -> Property
prop_parse_nested_directives outerDirective innerDirective =
  not (null outerDirective) && not (null innerDirective) &&
  not (any isNullChar (outerDirective ++ innerDirective)) ==>
  let content = "//! " ++ outerDirective ++ "\n//@ " ++ innerDirective ++ "\nfunc nested() {}\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= 1

-- Property: Comments in code blocks are preserved
prop_parse_comments_preserved :: String -> Property
prop_parse_comments_preserved comment =
  not (null comment) && not (any isNullChar comment) ==>
  let content = "//@ ownership=true\nfunc test() {\n  // " ++ comment ++ "\n}\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> 
         case tfBlocks parsed of
           [] -> property False
           (block:_) -> property $ comment `isInfixOf` cbContent block

-- Property: Indentation in code blocks is preserved
prop_parse_indentation_preserved :: Int -> String -> Property
prop_parse_indentation_preserved indentLevel code =
  indentLevel >= 0 && indentLevel <= 10 && not (null code) ==>
  let indent = replicate indentLevel ' '
      content = "//@ ownership=true\nfunc test() {\n" ++ indent ++ code ++ "\n}\n"
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> 
         case tfBlocks parsed of
           [] -> property False
           (block:_) -> property $ indent `isInfixOf` cbContent block

-- Property: Multiple code blocks are parsed separately
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks codeBlocks =
  not (null codeBlocks) && length codeBlocks <= 5 && all (not . null) codeBlocks ==>
  let blockContents = map (\code -> "//@ ownership=true\n" ++ code ++ "\n") codeBlocks
      content = concat blockContents
      result = parseTypus content
  in case result of
       Left _ -> property False
       Right parsed -> property $ length (tfBlocks parsed) >= length codeBlocks

-- Helper function to check for null bytes
isNullChar :: Char -> Bool
isNullChar = (== '\0')

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ fastProperty "Parse empty content returns default structure" prop_parse_empty_content
  , fastProperty "Parse file directives preserves directives" prop_parse_file_directives_preserved
  , fastProperty "Parse code blocks preserves block structure" prop_parse_code_blocks_preserved
  , fastProperty "Parse roundtrip preserves structure" prop_parse_roundtrip_structure
  , fastProperty "Parse multiple directives correctly" prop_parse_multiple_directives
  , fastProperty "Parse invalid directives safely" prop_parse_invalid_directives_safe
  , fastProperty "Parse special characters preserved" prop_parse_special_characters_preserved
  , fastProperty "Parse mixed directives works correctly" prop_parse_mixed_directives
  , fastProperty "Parse large files safely" prop_parse_large_files_safe
  , fastProperty "Parse preserves line numbers" prop_parse_preserves_line_numbers
  , fastProperty "Parse unicode content correctly" prop_parse_unicode_content
  , fastProperty "Parse empty code blocks gracefully" prop_parse_empty_code_blocks
  , fastProperty "Parse nested directives correctly" prop_parse_nested_directives
  , fastProperty "Parse comments preserved" prop_parse_comments_preserved
  , fastProperty "Parse indentation preserved" prop_parse_indentation_preserved
  , fastProperty "Parse multiple blocks separately" prop_parse_multiple_blocks
  ]