{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalParserQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose)
import Parser
  ( FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives, parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, spanStart, spanEnd)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)
import qualified Data.Text as T

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate a simple source span for testing
genSimpleSpan :: Gen SourceSpan
genSimpleSpan = do
  line <- choose (1, 100)
  startCol <- choose (1, 50)
  endCol <- choose (startCol, startCol + 100)
  let start = SourcePos line startCol 0
      end = SourcePos line endCol (endCol - startCol)
  return $ SourceSpan start end

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n,.;(){}[]"
    span <- genSimpleSpan
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- listOf $ do
      span <- genSimpleSpan
      tag <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
      return $ locatedWithSpan span tag
    blocks <- listOf arbitrary
    syntaxErrors <- return []  -- Simplified for testing
    return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate valid directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  ownership <- oneof ["", "ownership: on", "ownership: off", "ownership: true", "ownership: false"]
  dependentTypes <- oneof ["", "dependent_types: on", "dependent_types: off"]
  constraints <- oneof ["", "constraints: on", "constraints: off"]
  let directives = L.filter (not . null) [ownership, dependentTypes, constraints]
  case directives of
    [] -> return "//!"
    _ -> return $ "//! " ++ unwords directives

genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  ownership <- oneof ["", "ownership: on", "ownership: off"]
  dependentTypes <- oneof ["", "dependent_types: on", "dependent_types: off"]
  constraints <- oneof ["", "constraints: on", "constraints: off"]
  let directives = L.filter (not . null) [ownership, dependentTypes, constraints]
  case directives of
    [] -> return "{//!}"
    _ -> return $ "{//! " ++ unwords directives ++ " }"

genBuildTagLine :: Gen String
genBuildTagLine = do
  tag <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  oneof [return $ "//go:build " ++ tag, return $ "// +build " ++ tag]

genSimpleCodeContent :: Gen String
genSimpleCodeContent = do
  lines' <- listOf $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,.;(){}[]"
    return $ indent ++ content
  return $ unlines lines'

-- ============================================================================
-- Parser QuickCheck Tests
-- ============================================================================

-- Test default values
prop_default_file_directives :: Property
prop_default_file_directives =
  fdOwnership defaultFileDirectives === Nothing .&&.
  fdDependentTypes defaultFileDirectives === Nothing .&&.
  fdConstraints defaultFileDirectives === Nothing

prop_default_block_directives :: Property
prop_default_block_directives =
  bdOwnership defaultBlockDirectives === Nothing .&&.
  bdDependentTypes defaultBlockDirectives === Nothing .&&.
  bdConstraints defaultBlockDirectives === Nothing

-- Test parsing empty input
prop_parse_empty_input :: Property
prop_parse_empty_input = 
  case parseTypus "" of
    Left _ -> property False
    Right typusFile -> 
      tfDirectives typusFile === defaultFileDirectives .&&.
      L.null (tfBuildTags typusFile) .&&.
      L.null (tfBlocks typusFile)

-- Test parsing simple file directives
prop_parse_file_directives :: Property
prop_parse_file_directives = forAll genFileDirectiveLine $ \directiveLine ->
  case parseTypus directiveLine of
    Left _ -> property False
    Right typusFile -> 
      let directives = tfDirectives typusFile
      in directives /= defaultFileDirectives || directiveLine == "//!"

-- Test parsing build tags
prop_parse_build_tags :: Property
prop_parse_build_tags = forAll genBuildTagLine $ \buildTag ->
  case parseTypus buildTag of
    Left _ -> property False
    Right typusFile -> 
      not (L.null (tfBuildTags typusFile))

-- Test parsing simple code blocks
prop_parse_simple_code :: Property
prop_parse_simple_code = forAll genSimpleCodeContent $ \code ->
  case parseTypus code of
    Left _ -> property False
    Right typusFile -> 
      not (L.null (tfBlocks typusFile)) || L.all L.null (lines code)

-- Test parsing block directives
prop_parse_block_directives :: Property
prop_parse_block_directives = forAll genBlockDirectiveLine $ \directiveLine ->
  let input = directiveLine ++ "\n  some code\n"
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> 
         not (L.null (tfBlocks typusFile))

-- Test parsing multiple directives
prop_parse_multiple_file_directives :: Property
prop_parse_multiple_file_directives = 
  forAll (listOf genFileDirectiveLine) $ \directives ->
  let input = unlines directives
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> 
         tfDirectives typusFile /= defaultFileDirectives || null directives

-- Test parsing mixed content
prop_parse_mixed_content :: Property
prop_parse_mixed_content = do
  directives <- listOf genFileDirectiveLine
  buildTags <- listOf genBuildTagLine
  code <- genSimpleCodeContent
  blockDirectives <- listOf genBlockDirectiveLine
  let blocks = L.map (\d -> d ++ "\n  block code\n") blockDirectives
      input = unlines $ directives ++ buildTags ++ [code] ++ blocks
  case parseTypus input of
    Left _ -> property False
    Right typusFile -> 
      let hasDirectives = tfDirectives typusFile /= defaultFileDirectives
          hasBuildTags = not (L.null (tfBuildTags typusFile))
          hasBlocks = not (L.null (tfBlocks typusFile))
      in hasDirectives || hasBuildTags || hasBlocks || null input

-- Test error handling for invalid directives
prop_parse_invalid_directive_fails :: Property
prop_parse_invalid_directive_fails = 
  let invalidDirective = "//! invalid_key: value"
  in case parseTypus invalidDirective of
       Left _ -> property True
       Right _ -> property False

-- Test parsing preserves content structure
prop_parse_preserves_line_structure :: Property
prop_parse_preserves_line_structure = 
  forAll (listOf $ listOf $ elements $ ['a'..'z'] ++ ' ') $ \lines' ->
  let input = unlines lines'
      expectedLines = L.length $ L.filter (not . L.all isSpace) lines'
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             totalLines = L.sum $ L.length . lines . cbContent <$> blocks
         in totalLines >= expectedLines || null lines'

-- Test parsing with whitespace variations
prop_parse_whitespace_variations :: Property
prop_parse_whitespace_variations = 
  forAll genFileDirectiveLine $ \baseDirective ->
  let variations = 
        [ baseDirective
        , "  " ++ baseDirective
        , baseDirective ++ "  "
        , "  " ++ baseDirective ++ "  "
        , baseDirective ++ "\n"
        ]
  in L.all (\variant -> case parseTypus variant of
                       Left _ -> False
                       Right _ -> True) variations

-- Test block directive parsing with nested braces
prop_parse_nested_braces :: Property
prop_parse_nested_braces = 
  let input = "{//! ownership: on }\n{\n  if true {\n    // code\n  }\n}\n"
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> 
         not (L.null (tfBlocks typusFile))

-- Test parsing comments L.and directives interaction
prop_parse_comments_with_directives :: Property
prop_parse_comments_with_directives = 
  let input = "// This is a comment\n//! ownership: on\n// Another comment\nfunc main() {}\n"
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> 
         tfDirectives typusFile /= defaultFileDirectives

-- Test parsing preserves directive values
prop_parse_preserves_directive_values :: Property
prop_parse_preserves_directive_values = do
  ownershipValue <- elements [True, False]
  dependentTypesValue <- elements [True, False]
  let input = "//! ownership: " ++ show ownershipValue ++ "\n" ++
              "//! dependent_types: " ++ show dependentTypesValue ++ "\n"
  case parseTypus input of
    Left _ -> property False
    Right typusFile -> 
      let directives = tfDirectives typusFile
          parsedOwnership = fmap locValue (fdOwnership directives)
          parsedDependentTypes = fmap locValue (fdDependentTypes directives)
      in parsedOwnership === Just ownershipValue .&&.
         parsedDependentTypes === Just dependentTypesValue

-- Test parsing error for unclosed blocks
prop_parse_unclosed_block_fails :: Property
prop_parse_unclosed_block_fails = 
  let input = "{//! ownership: on\n  some code\n// missing closing brace"
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False

-- Test parsing with multiple package declarations should fail
prop_parse_multiple_packages_fails :: Property
prop_parse_multiple_packages_fails = 
  let input = "package main\npackage utils\n"
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False

-- Test parsing if statements without braces should fail
prop_parse_if_without_brace_fails :: Property
prop_parse_if_without_brace_fails = 
  let input = "if condition\n  doSomething()\n"
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False

-- Test parsing if statements with braces should succeed
prop_parse_if_with_brace_succeeds :: Property
prop_parse_if_with_brace_succeeds = 
  let input = "if condition {\n  doSomething()\n}\n"
  in case parseTypus input of
    Left _ -> property False
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Cabal Parser QuickCheck Tests"
  [ testGroup "Default values tests"
      [ testProperty "default file directives" prop_default_file_directives
      , testProperty "default block directives" prop_default_block_directives
      ]
  , testGroup "Basic parsing tests"
      [ testProperty "parse empty input" prop_parse_empty_input
      , testProperty "parse file directives" prop_parse_file_directives
      , testProperty "parse build tags" prop_parse_build_tags
      , testProperty "parse simple code" prop_parse_simple_code
      , testProperty "parse block directives" prop_parse_block_directives
      ]
  , testGroup "Complex parsing tests"
      [ testProperty "parse multiple file directives" prop_parse_multiple_file_directives
      , testProperty "parse mixed content" prop_parse_mixed_content
      , testProperty "parse preserves line structure" prop_parse_preserves_line_structure
      , testProperty "parse whitespace variations" prop_parse_whitespace_variations
      , testProperty "parse nested braces" prop_parse_nested_braces
      ]
  , testGroup "Error handling tests"
      [ testProperty "parse invalid directive fails" prop_parse_invalid_directive_fails
      , testProperty "parse unclosed block fails" prop_parse_unclosed_block_fails
      , testProperty "parse multiple packages fails" prop_parse_multiple_packages_fails
      , testProperty "parse if without brace fails" prop_parse_if_without_brace_fails
      ]
  , testGroup "Correctness tests"
      [ testProperty "parse preserves directive values" prop_parse_preserves_directive_values
      , testProperty "parse comments with directives" prop_parse_comments_with_directives
      , testProperty "parse if with brace succeeds" prop_parse_if_with_brace_succeeds
      ]
  ]