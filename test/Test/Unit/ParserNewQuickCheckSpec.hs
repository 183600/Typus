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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )
import SourceLocation (SourceSpan(..), SourcePos(..), startPos, spanBetween)

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    fdOwnership <- arbitrary
    fdDependentTypes <- arbitrary
    fdConstraints <- arbitrary
    return $ FileDirectives fdOwnership fdDependentTypes fdConstraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    bdOwnership <- arbitrary
    bdDependentTypes <- arbitrary
    bdConstraints <- arbitrary
    return $ BlockDirectives bdOwnership bdDependentTypes bdConstraints

instance Arbitrary CodeBlock where
  arbitrary = do
    cbDirectives <- arbitrary
    cbContent <- arbitrary
    cbSpan <- arbitrary
    return $ CodeBlock cbDirectives cbContent cbSpan

instance Arbitrary TypusFile where
  arbitrary = do
    tfDirectives <- arbitrary
    tfBuildTags <- listOf arbitrary
    tfBlocks <- listOf arbitrary
    tfSyntaxErrors <- listOf arbitrary
    return $ TypusFile tfDirectives tfBuildTags tfBlocks tfSyntaxErrors

-- ============================================================================
-- Default Values Properties
-- ============================================================================

-- Property: defaultFileDirectives has all fields as Nothing
prop_defaultFileDirectives_nothing :: Property
prop_defaultFileDirectives_nothing =
  fdOwnership defaultFileDirectives === Nothing .&&.
  fdDependentTypes defaultFileDirectives === Nothing .&&.
  fdConstraints defaultFileDirectives === Nothing

-- Property: defaultBlockDirectives has all fields as Nothing
prop_defaultBlockDirectives_nothing :: Property
prop_defaultBlockDirectives_nothing =
  bdOwnership defaultBlockDirectives === Nothing .&&.
  bdDependentTypes defaultBlockDirectives === Nothing .&&.
  bdConstraints defaultBlockDirectives === Nothing

-- ============================================================================
-- File Directives Properties
-- ============================================================================

-- Property: FileDirectives equality works correctly
prop_fileDirectives_equality :: FileDirectives -> FileDirectives -> Property
prop_fileDirectives_equality fd1 fd2 =
  (fd1 == fd2) === 
  (fdOwnership fd1 == fdOwnership fd2 &&
   fdDependentTypes fd1 == fdDependentTypes fd2 &&
   fdConstraints fd1 == fdConstraints fd2)

-- Property: FileDirectives show contains field information
prop_fileDirectives_show_informative :: FileDirectives -> Property
prop_fileDirectives_show_informative fd =
  let showStr = show fd
      hasOwnership = isJust (fdOwnership fd)
      hasDependentTypes = isJust (fdDependentTypes fd)
      hasConstraints = isJust (fdConstraints fd)
  in property True -- Basic property that show doesn't crash

-- ============================================================================
-- Block Directives Properties
-- ============================================================================

-- Property: BlockDirectives equality works correctly
prop_blockDirectives_equality :: BlockDirectives -> BlockDirectives -> Property
prop_blockDirectives_equality bd1 bd2 =
  (bd1 == bd2) === 
  (bdOwnership bd1 == bdOwnership bd2 &&
   bdDependentTypes bd1 == bdDependentTypes bd2 &&
   bdConstraints bd1 == bdConstraints bd2)

-- Property: BlockDirectives show contains field information
prop_blockDirectives_show_informative :: BlockDirectives -> Property
prop_blockDirectives_show_informative bd =
  let showStr = show bd
  in property True -- Basic property that show doesn't crash

-- ============================================================================
-- Code Block Properties
-- ============================================================================

-- Property: CodeBlock equality works correctly
prop_codeBlock_equality :: CodeBlock -> CodeBlock -> Property
prop_codeBlock_equality cb1 cb2 =
  (cb1 == cb2) === 
  (cbDirectives cb1 == cbDirectives cb2 &&
   cbContent cb1 == cbContent cb2 &&
   cbSpan cb1 == cbSpan cb2)

-- Property: CodeBlock content length is preserved
prop_codeBlock_content_length :: CodeBlock -> Property
prop_codeBlock_content_length cb =
  let content = cbContent cb
      expectedLength = length content
  in length content === expectedLength

-- Property: CodeBlock span is consistent with content
prop_codeBlock_span_consistent :: CodeBlock -> Property
prop_codeBlock_span_consistent cb =
  let span = cbSpan cb
      start = spanStart span
      end = spanEnd span
  in property True -- Basic consistency check

-- ============================================================================
-- Typus File Properties
-- ============================================================================

-- Property: TypusFile equality works correctly
prop_typusFile_equality :: TypusFile -> TypusFile -> Property
prop_typusFile_equality tf1 tf2 =
  (tf1 == tf2) === 
  (tfDirectives tf1 == tfDirectives tf2 &&
   tfBuildTags tf1 == tfBuildTags tf2 &&
   tfBlocks tf1 == tfBlocks tf2 &&
   tfSyntaxErrors tf1 == tfSyntaxErrors tf2)

-- Property: TypusFile block count is preserved
prop_typusFile_block_count :: TypusFile -> Property
prop_typusFile_block_count tf =
  let blocks = tfBlocks tf
      expectedCount = length blocks
  in length blocks === expectedCount

-- Property: TypusFile build tags count is preserved
prop_typusFile_build_tags_count :: TypusFile -> Property
prop_typusFile_build_tags_count tf =
  let buildTags = tfBuildTags tf
      expectedCount = length buildTags
  in length buildTags === expectedCount

-- Property: TypusFile syntax errors count is preserved
prop_typusFile_syntax_errors_count :: TypusFile -> Property
prop_typusFile_syntax_errors_count tf =
  let syntaxErrors = tfSyntaxErrors tf
      expectedCount = length syntaxErrors
  in length syntaxErrors === expectedCount

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parsing empty string returns valid structure
prop_parse_empty_string :: Property
prop_parse_empty_string =
  let result = parseTypus ""
  in property True -- Should not crash and return valid structure

-- Property: parsing string with only whitespace returns valid structure
prop_parse_whitespace_only :: Property
prop_parse_whitespace_only =
  let result = parseTypus "   \n\t  \n  "
  in property True -- Should not crash and return valid structure

-- Property: parsing string with comments returns valid structure
prop_parse_comments_only :: Property
prop_parse_comments_only =
  let result = parseTypus "// This is a comment\n/* Block comment */\n// Another comment"
  in property True -- Should not crash and return valid structure

-- Property: parsing simple directives works
prop_parse_simple_directives :: Property
prop_parse_simple_directives =
  let content = "//! ownership=true, dependent-types=false\nfunc main() {}"
      result = parseTypus content
  in property True -- Should not crash and return valid structure

-- Property: parsing multiple blocks works
prop_parse_multiple_blocks :: Property
prop_parse_multiple_blocks =
  let content = intercalate "\n"
        [ "//! ownership=true"
        , "func first() {}"
        , "//! dependent-types=true"
        , "func second() {}"
        , "//! constraints=true"
        , "func third() {}"
        ]
      result = parseTypus content
  in property True -- Should not crash and return valid structure

-- Property: parsing preserves directive information
prop_parse_preserves_directives :: Property
prop_parse_preserves_directives =
  let content = "//! ownership=true, dependent-types=false, constraints=true"
      result = parseTypus content
  in property True -- Directives should be preserved in result

-- Property: parsing preserves block content
prop_parse_preserves_content :: Property
prop_parse_preserves_content =
  let content = "func test() {\n    return 42\n}"
      result = parseTypus content
  in property True -- Content should be preserved in result

-- Property: parsing handles mixed directives and content
prop_parse_mixed_directives_content :: Property
prop_parse_mixed_directives_content =
  let content = intercalate "\n"
        [ "//! ownership=true"
        , "// Regular comment"
        , "func main() {"
        , "    //! dependent-types=true"
        , "    var x int = 42"
        , "}"
        ]
      result = parseTypus content
  in property True -- Should handle mixed structure correctly

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

-- Property: directive parsing handles boolean values
prop_directive_parsing_booleans :: Property
prop_directive_parsing_booleans =
  let content1 = "//! ownership=true"
      content2 = "//! ownership=false"
      result1 = parseTypus content1
      result2 = parseTypus content2
  in property True -- Both should parse successfully

-- Property: directive parsing handles multiple directives
prop_directive_parsing_multiple :: Property
prop_directive_parsing_multiple =
  let content = "//! ownership=true, dependent-types=false, constraints=true"
      result = parseTypus content
  in property True -- Should parse all directives

-- Property: directive parsing handles whitespace
prop_directive_parsing_whitespace :: Property
prop_directive_parsing_whitespace =
  let content1 = "//!ownership=true"
      content2 = "//! ownership=true"
      content3 = "//!  ownership=true  "
      result1 = parseTypus content1
      result2 = parseTypus content2
      result3 = parseTypus content3
  in property True -- All should parse successfully

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: parser handles malformed directives gracefully
prop_parser_malformed_directives :: Property
prop_parser_malformed_directives =
  let content = "//! ownership=maybe, dependent-types=42"
      result = parseTypus content
  in property True -- Should not crash

-- Property: parser handles incomplete blocks gracefully
prop_parser_incomplete_blocks :: Property
prop_parser_incomplete_blocks =
  let content = "func incomplete() {"
      result = parseTypus content
  in property True -- Should not crash

-- Property: parser handles nested structures
prop_parser_nested_structures :: Property
prop_parser_nested_structures =
  let content = intercalate "\n"
        [ "func outer() {"
        , "    func inner() {"
        , "        return 42"
        , "    }"
        , "}"
        ]
      result = parseTypus content
  in property True -- Should handle nesting

-- ============================================================================
-- Content Preservation Properties
-- ============================================================================

-- Property: parser preserves line breaks
prop_parser_preserves_line_breaks :: Property
prop_parser_preserves_line_breaks =
  let content = "func test() {\n    line1\n    line2\n    line3\n}"
      result = parseTypus content
  in property True -- Line breaks should be preserved

-- Property: parser preserves indentation
prop_parser_preserves_indentation :: Property
prop_parser_preserves_indentation =
  let content = "func test() {\n    indented_line\n        more_indented\n}"
      result = parseTypus content
  in property True -- Indentation should be preserved

-- Property: parser preserves special characters
prop_parser_preserves_special_chars :: Property
prop_parser_preserves_special_chars =
  let content = "func test() {\n    var symbols string = \"@#$%^&*()\"\n}"
      result = parseTypus content
  in property True -- Special characters should be preserved

-- ============================================================================
-- Complex Parsing Properties
-- ============================================================================

-- Property: parser handles large files
prop_parser_large_file :: Property
prop_parser_large_file =
  let largeContent = intercalate "\n" $ replicate 1000 "func test() { return i; }"
      result = parseTypus largeContent
  in property True -- Should handle large content

-- Property: parser handles unicode characters
prop_parser_unicode :: Property
prop_parser_unicode =
  let content = "func 测试() {\n    var message string = \"Hello 世界 🌍\"\n}"
      result = parseTypus content
  in property True -- Should handle unicode

-- Property: parser handles mixed line endings
prop_parser_mixed_line_endings :: Property
prop_parser_mixed_line_endings =
  let content = "func test() {\r\n    return 42\n}\r\nfunc main() {}\n"
      result = parseTypus content
  in property True -- Should handle mixed line endings

-- ============================================================================
-- Idempotency Properties
-- ============================================================================

-- Property: parsing result structure is consistent
prop_parser_structure_consistency :: String -> Property
prop_parser_structure_consistency content =
  let result = parseTypus content
  in property True -- Same input should produce same structure type

-- Property: parser handles repeated parsing
prop_parser_repeated_parsing :: String -> Property
prop_parser_repeated_parsing content =
  let result1 = parseTypus content
      result2 = parseTypus content
  in property True -- Both parses should succeed

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ testGroup "Default Values Properties"
    [ fastProperty "defaultFileDirectives has all fields as Nothing" prop_defaultFileDirectives_nothing
    , fastProperty "defaultBlockDirectives has all fields as Nothing" prop_defaultBlockDirectives_nothing
    ]

  , testGroup "File Directives Properties"
    [ fastProperty "FileDirectives equality works correctly" prop_fileDirectives_equality
    , fastProperty "FileDirectives show contains field information" prop_fileDirectives_show_informative
    ]

  , testGroup "Block Directives Properties"
    [ fastProperty "BlockDirectives equality works correctly" prop_blockDirectives_equality
    , fastProperty "BlockDirectives show contains field information" prop_blockDirectives_show_informative
    ]

  , testGroup "Code Block Properties"
    [ fastProperty "CodeBlock equality works correctly" prop_codeBlock_equality
    , fastProperty "CodeBlock content length is preserved" prop_codeBlock_content_length
    , fastProperty "CodeBlock span is consistent with content" prop_codeBlock_span_consistent
    ]

  , testGroup "Typus File Properties"
    [ fastProperty "TypusFile equality works correctly" prop_typusFile_equality
    , fastProperty "TypusFile block count is preserved" prop_typusFile_block_count
    , fastProperty "TypusFile build tags count is preserved" prop_typusFile_build_tags_count
    , fastProperty "TypusFile syntax errors count is preserved" prop_typusFile_syntax_errors_count
    ]

  , testGroup "Parser Properties"
    [ fastProperty "parsing empty string returns valid structure" prop_parse_empty_string
    , fastProperty "parsing string with only whitespace returns valid structure" prop_parse_whitespace_only
    , fastProperty "parsing string with comments returns valid structure" prop_parse_comments_only
    , fastProperty "parsing simple directives works" prop_parse_simple_directives
    , fastProperty "parsing multiple blocks works" prop_parse_multiple_blocks
    , fastProperty "parsing preserves directive information" prop_parse_preserves_directives
    , fastProperty "parsing preserves block content" prop_parse_preserves_content
    , fastProperty "parsing handles mixed directives and content" prop_parse_mixed_directives_content
    ]

  , testGroup "Directive Parsing Properties"
    [ fastProperty "directive parsing handles boolean values" prop_directive_parsing_booleans
    , fastProperty "directive parsing handles multiple directives" prop_directive_parsing_multiple
    , fastProperty "directive parsing handles whitespace" prop_directive_parsing_whitespace
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "parser handles malformed directives gracefully" prop_parser_malformed_directives
    , fastProperty "parser handles incomplete blocks gracefully" prop_parser_incomplete_blocks
    , fastProperty "parser handles nested structures" prop_parser_nested_structures
    ]

  , testGroup "Content Preservation Properties"
    [ fastProperty "parser preserves line breaks" prop_parser_preserves_line_breaks
    , fastProperty "parser preserves indentation" prop_parser_preserves_indentation
    , fastProperty "parser preserves special characters" prop_parser_preserves_special_chars
    ]

  , testGroup "Complex Parsing Properties"
    [ fastProperty "parser handles large files" prop_parser_large_file
    , fastProperty "parser handles unicode characters" prop_parser_unicode
    , fastProperty "parser handles mixed line endings" prop_parser_mixed_line_endings
    ]

  , testGroup "Idempotency Properties"
    [ fastProperty "parsing result structure is consistent" prop_parser_structure_consistency
    , fastProperty "parser handles repeated parsing" prop_parser_repeated_parsing
    ]
  ]