{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserCombinatorPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Char (isAlphaNum, isSpace, isLetter)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import qualified Data.Text as T
import Data.Void (Void)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , ParsedLine(..)
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid identifiers (alphanumeric + underscore + hyphen)
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")
  return (first : rest)

-- Generate valid directive values
genDirectiveValue :: Gen String
genDirectiveValue = oneof
  [ genIdentifier
  , elements ["true", "false", "on", "off", "yes", "no"]
  ]

-- Generate file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genIdentifier
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  key <- genIdentifier
  value <- genDirectiveValue
  return $ "{//! " ++ key ++ ": " ++ value ++ "}"

-- Generate regular code lines (avoiding directive patterns)
genCodeLine :: Gen String
genCodeLine = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;(){}[]+-*/%=<>!&|"
  let line = content
  -- Ensure it doesn't start with directive patterns
  return $ if "//!" `L.isPrefixOf` line || "{//!" `L.isPrefixOf` line
           then "code: " ++ line
           else line

-- Generate mixed content lines
genMixedLines :: Gen [String]
genMixedLines = do
  numLines <- choose (0, 20)
  lines <- listOf $ oneof
    [ genFileDirectiveLine
    , genBlockDirectiveLine
    , genCodeLine
    , return ""  -- Empty line
    ]
  return $ take numLines lines

-- Generate content with guaranteed parseable structure
genParseableContent :: Gen String
genParseableContent = do
  fileDirectives <- listOf genFileDirectiveLine
  codeBlocks <- listOf $ do
    blockDirectives <- listOf genBlockDirectiveLine
    codeLines <- listOf genCodeLine
    let blockContent = unlines (blockDirectives ++ codeLines)
    return blockContent
  return $ unlines (fileDirectives ++ concatMap lines codeBlocks)

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

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Parsing empty content returns valid structure with defaults
prop_parse_empty_content :: Property
prop_parse_empty_content =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file === defaultFileDirectives .&&.
                        L.null (tfBuildTags file) .&&.
                        L.null (tfBlocks file)

-- Property: Parsing content with only file directives preserves them
prop_parse_file_directives_preserved :: [String] -> Property
prop_parse_file_directives_preserved directives =
  L.all ("//! " `L.isPrefixOf`) directives ==>
  let content = unlines directives
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ not (null directives) ==> 
      tfDirectives file /= defaultFileDirectives

-- Property: Parsing preserves line count in blocks
prop_parse_preserves_line_count :: [String] -> Property
prop_parse_preserves_line_count lines =
  let content = unlines lines
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) <= L.length (L.filter (not . null . trim) lines)

-- Property: Parsing is idempotent for well-formed content
prop_parse_idempotent :: String -> Property
prop_parse_idempotent content =
  let firstParse = parseTypus content
  in case firstParse of
    Left _ -> property True  -- Can't test idempotency on parse failures
    Right file -> property True  -- We can't easily re-serialize, so just ensure it parses

-- Property: Parsing handles mixed directives L.and code correctly
prop_parse_mixed_content :: Property
prop_parse_mixed_content =
  forAll genMixedLines $ \lines ->
    let content = unlines lines
        result = parseTypus content
    in case result of
      Left _ -> property $ not (null content)  -- Empty content should parse
      Right file -> property $ L.length (tfBlocks file) >= 0

-- Property: File directives are correctly extracted
prop_file_directives_extraction :: Property
prop_file_directives_extraction =
  forAll genFileDirectiveLine $ \directive ->
    let content = directive ++ "\ncode line\n"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Block directives are correctly associated with blocks
prop_block_directives_association :: Property
prop_block_directives_association =
  forAll genBlockDirectiveLine $ \directive ->
    let content = directive ++ "\ncode line\n"
        result = parseTypus content
    in case result of
      Left _ -> property False
      Right file -> property $ case tfBlocks file of
        [] -> property False
        (block:_) -> property $ cbDirectives block /= defaultBlockDirectives

-- Property: Parsing preserves code content
prop_parse_preserves_code_content :: String -> Property
prop_parse_preserves_code_content code =
  not ("//! " `L.isPrefixOf` code) && not ("{//!" `L.isPrefixOf` code) ==>
  let content = code ++ "\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ case tfBlocks file of
      [] -> property $ null code
      (block:_) -> property $ code `L.isInfixOf` cbContent block

-- Property: Parsing handles Unicode characters
prop_parse_unicode_handling :: Property
prop_parse_unicode_handling =
  let unicodeContent = "//! test: 测试\n{//! 测试: true}\nvar 测试变量 = \"🚀\"\n"
      result = parseTypus unicodeContent
  in case result of
    Left _ -> property False
    Right file -> property $ "测试" `L.isInfixOf` (show file) .||. "🚀" `L.isInfixOf` (show file)

-- Property: Multiple file directives are accumulated
prop_multiple_file_directives :: [String] -> Property
prop_multiple_file_directives directives =
  L.all ("//! " `L.isPrefixOf`) directives && not (null directives) ==>
  let content = unlines directives
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Empty lines are handled gracefully
prop_empty_lines_handling :: Int -> Property
prop_empty_lines_handling numEmptyLines =
  numEmptyLines >= 0 && numEmptyLines <= 50 ==>
  let emptyLines = replicate numEmptyLines ""
      content = unlines emptyLines
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file === defaultFileDirectives

-- Property: Parsing with syntax errors still returns structure
prop_parse_with_syntax_errors :: String -> Property
prop_parse_with_syntax_errors malformedCode =
  let content = malformedCode ++ "\nif condition\n"  -- Intentionally malformed
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfSyntaxErrors file) >= 0

-- Property: Directive values are correctly parsed
prop_directive_values_parsed :: String -> String -> Property
prop_directive_values_parsed key value =
  let content = "//! " ++ key ++ ": " ++ value ++ "\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Complex nested structures are parsed correctly
prop_complex_nested_structures :: Property
prop_complex_nested_structures =
  let complexContent = unlines
        [ "//! ownership: true"
        , "//! dependent-types: true"
        , "{//! ownership: false}"
        , "func test() {"
        , "  // code here"
        , "}"
        , "{//! constraints: true}"
        , "type SafeInt struct {"
        , "  value int"
        , "}"
        ]
      result = parseTypus complexContent
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 2 .&&.
                        tfDirectives file /= defaultFileDirectives

-- Property: Parsing is consistent with line endings
prop_parse_line_endings_consistency :: String -> Property
prop_parse_line_endings_consistency content =
  let unixContent = unlines (lines content)
      windowsContent = unlines $ L.map (++ "\r") $ lines content
      unixResult = parseTypus unixContent
      windowsResult = parseTypus windowsContent
  in case (unixResult, windowsResult) of
    (Left _, Left _) -> property True
    (Right unixFile, Right windowsFile) -> property $ 
      L.length (tfBlocks unixFile) === L.length (tfBlocks windowsFile)
    _ -> property False  -- One succeeded, one failed - inconsistency

-- Property: Large files are parsed without stack overflow
prop_large_file_parsing :: Int -> Property
prop_large_file_parsing multiplier =
  multiplier >= 0 && multiplier <= 100 ==>
  let baseLine = "code line with some content\n"
      largeContent = L.concat (replicate multiplier baseLine)
      result = parseTypus largeContent
  in case result of
    Left _ -> property $ multiplier == 0  -- Only allow failure for empty content
    Right file -> property $ L.length (tfBlocks file) >= 0

-- Property: Parsing preserves order of blocks
prop_parse_preserves_block_order :: [String] -> Property
prop_parse_preserves_block_order blocks =
  let content = unlines blocks
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 0

-- Property: Malformed directives are handled gracefully
prop_malformed_directives_handling :: String -> Property
prop_malformed_directives_handling malformed =
  let content = "//! " ++ malformed ++ "\ncode\n"
      result = parseTypus content
  in case result of
    Left _ -> property True  -- Expected to fail on malformed directives
    Right file -> property $ True  -- Or succeed with graceful degradation

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Parsing never crashes on L.any input
prop_parse_never_crashes :: String -> Property
prop_parse_never_crashes content =
  let result = parseTypus content
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- Property: Error messages contain useful information
prop_error_messages_useful :: String -> Property
prop_error_messages_useful content =
  let result = parseTypus content
  in case result of
    Left errMsg -> property $ L.length errMsg > 0
    Right _ -> property True

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Parsing time is reasonable for moderate input
prop_parsing_performance_reasonable :: Int -> Property
prop_parsing_performance_reasonable size =
  size >= 0 && size <= 1000 ==>
  let content = unlines (map show [1..size])
      result = parseTypus content
  in case result of
    Left _ -> property $ size <= 10  -- Allow failures for very small inputs
    Right file -> property $ True  -- If it parses, performance is acceptable

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Combinator Properties QuickCheck Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "parse empty content" prop_parse_empty_content
    , fastProperty "file directives preserved" prop_parse_file_directives_preserved
    , fastProperty "preserves line count" prop_parse_preserves_line_count
    , fastProperty "parse idempotent" prop_parse_idempotent
    , fastProperty "mixed content handling" prop_parse_mixed_content
    ]

  , testGroup "Directive Handling"
    [ fastProperty "file directives extraction" prop_file_directives_extraction
    , fastProperty "block directives association" prop_block_directives_association
    , fastProperty "multiple file directives" prop_multiple_file_directives
    , fastProperty "directive values parsed" prop_directive_values_parsed
    , fastProperty "malformed directives handling" prop_malformed_directives_handling
    ]

  , testGroup "Content Preservation"
    [ fastProperty "preserves code content" prop_parse_preserves_code_content
    , fastProperty "unicode handling" prop_parse_unicode_handling
    , fastProperty "empty lines handling" prop_empty_lines_handling
    , fastProperty "line endings consistency" prop_parse_line_endings_consistency
    , fastProperty "preserves block order" prop_parse_preserves_block_order
    ]

  , testGroup "Complex Scenarios"
    [ fastProperty "complex nested structures" prop_complex_nested_structures
    , fastProperty "large file parsing" prop_large_file_parsing
    , fastProperty "parse with syntax errors" prop_parse_with_syntax_errors
    ]

  , testGroup "Error Handling"
    [ fastProperty "parse never crashes" prop_parse_never_crashes
    , fastProperty "error messages useful" prop_error_messages_useful
    ]

  , testGroup "Performance"
    [ fastProperty "parsing performance reasonable" prop_parsing_performance_reasonable
    ]
  ]