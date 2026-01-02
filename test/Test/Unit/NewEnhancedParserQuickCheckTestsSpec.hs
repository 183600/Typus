{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  )

import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import Utils (trim)

-- ============================================================================
-- Custom Generators
-- ============================================================================

genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  return (first : rest)

genBoolString :: Gen String
genBoolString = elements ["true", "false"]

genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

genFileDirective :: Gen String
genFileDirective = do
  key <- genDirectiveKey
  value <- genBoolString
  return $ "//! " ++ key ++ ": " ++ value

genBuildTag :: Gen String
genBuildTag = oneof 
  [ return "//go:build linux"
  , return "// +build ignore"
  , do
      tag <- genValidIdentifier
      return $ "//go:build " ++ tag
  ]

genBlockDirective :: Gen String
genBlockDirective = do
  key <- genDirectiveKey
  value <- genBoolString
  return $ "{//! " ++ key ++ ": " ++ value ++ "}"

genGoCode :: Gen String
genGoCode = do
  lines' <- listOf $ oneof
    [ return "package main"
    , return "import \"fmt\""
    , return "func main() {"
    , return "    fmt.Println(\"Hello, World!\")"
    , return "}"
    , do
        var <- genValidIdentifier
        return $ "var " ++ var ++ " int = 42"
    , do
        func <- genValidIdentifier
        return $ "func " ++ func ++ "() {"
    , return "    if true {"
    , return "        return 42"
    , return "    }"
    ]
  return $ unlines lines'

genTypusContent :: Gen String
genTypusContent = do
  fileDirectives <- listOf genFileDirective
  buildTags <- listOf genBuildTag
  codeBlocks <- listOf genCodeBlock
  return $ unlines (fileDirectives ++ buildTags ++ codeBlocks)

genCodeBlock :: Gen String
genCodeBlock = do
  directive <- genBlockDirective
  code <- genGoCode
  return $ directive ++ "\n" ++ code

genSimpleTypusFile :: Gen String
genSimpleTypusFile = do
  hasDirectives <- elements [True, False]
  hasCode <- elements [True, False]
  directives <- if hasDirectives 
                then listOf genFileDirective 
                else return []
  code <- if hasCode 
          then genGoCode 
          else return ""
  return $ unlines (directives ++ [code])

genInvalidDirective :: Gen String
genInvalidDirective = oneof
  [ do
      key <- genValidIdentifier
      value <- genValidIdentifier
      return $ "//! " ++ key ++ ": " ++ value  -- invalid key
  , do
      key <- genDirectiveKey
      value <- genValidIdentifier
      return $ "//! " ++ key ++ ": " ++ value  -- invalid value
  , return "//! malformed directive without colon"
  ]

-- ============================================================================
-- FileDirectives Properties
-- ============================================================================

-- Property: defaultFileDirectives should have L.all fields as Nothing
prop_defaultFileDirectives_nothing :: Property
prop_defaultFileDirectives_nothing =
  property $ fdOwnership defaultFileDirectives === Nothing .&&.
             fdDependentTypes defaultFileDirectives === Nothing .&&.
             fdConstraints defaultFileDirectives === Nothing

-- Property: parsing empty file should return default directives
prop_parse_empty_file_default_directives :: Property
prop_parse_empty_file_default_directives =
  let result = parseTypus ""
  in case result of
       Left _ -> property $ False  -- Should not fail on empty input
       Right typusFile -> property $ tfDirectives typusFile === defaultFileDirectives

-- Property: parsing file with only whitespace should return default directives
prop_parse_whitespace_file_default_directives :: Property
prop_parse_whitespace_file_default_directives =
  forAll (listOf (elements " \t\n\r")) $ \whitespace ->
  let result = parseTypus whitespace
  in case result of
       Left _ -> property $ False
       Right typusFile -> property $ tfDirectives typusFile === defaultFileDirectives

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

-- Property: valid file directives should be parsed successfully
prop_parse_valid_file_directive :: Property
prop_parse_valid_file_directive =
  forAll genFileDirective $ \directive ->
  let result = parseTypus directive
  in case result of
       Left _ -> property $ False
       Right typusFile -> property $ tfDirectives typusFile /= defaultFileDirectives

-- Property: multiple valid directives should L.all be parsed
prop_parse_multiple_file_directives :: Property
prop_parse_multiple_file_directives =
  forAll (listOf genFileDirective `suchThat` (not . null)) $ \directives ->
  let content = unlines directives
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         let dirs = tfDirectives typusFile
         in property $ dirs /= defaultFileDirectives

-- Property: invalid directives should cause parse failure
prop_parse_invalid_directive_fails :: Property
prop_parse_invalid_directive_fails =
  forAll genInvalidDirective $ \directive ->
  let result = parseTypus directive
  in case result of
       Left _ -> property $ True
       Right _ -> property $ False

-- ============================================================================
-- Block Parsing Properties
-- ============================================================================

-- Property: defaultBlockDirectives should have L.all fields as Nothing
prop_defaultBlockDirectives_nothing :: Property
prop_defaultBlockDirectives_nothing =
  property $ bdOwnership defaultBlockDirectives === Nothing .&&.
             bdDependentTypes defaultBlockDirectives === Nothing .&&.
             bdConstraints defaultBlockDirectives === Nothing

-- Property: parsing simple code block should create at least one block
prop_parse_simple_code_block :: Property
prop_parse_simple_code_block =
  let content = "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}"
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> property $ not (L.null (tfBlocks typusFile))

-- Property: code with block directives should create blocks with directives
prop_parse_block_directives :: Property
prop_parse_block_directives =
  forAll genCodeBlock $ \block ->
  let result = parseTypus block
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         case tfBlocks typusFile of
           [] -> property $ False
           (firstBlock:_) -> property $ cbDirectives firstBlock /= defaultBlockDirectives

-- ============================================================================
-- Build Tag Properties
-- ============================================================================

-- Property: files with build tags should have them in the result
prop_parse_build_tags :: Property
prop_parse_build_tags =
  forAll genBuildTag $ \tag ->
  let result = parseTypus tag
  in case result of
       Left _ -> property $ False
       Right typusFile -> property $ not (L.null (tfBuildTags typusFile))

-- Property: multiple build tags should L.all be parsed
prop_parse_multiple_build_tags :: Property
prop_parse_multiple_build_tags =
  forAll (listOf genBuildTag `suchThat` (not . null)) $ \tags ->
  let content = unlines tags
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         property $ L.length (tfBuildTags typusFile) == L.length tags

-- ============================================================================
-- Syntax Error Properties
-- ============================================================================

-- Property: files with if statements without braces should have syntax errors
prop_if_without_brace_syntax_error :: Property
prop_if_without_brace_syntax_error =
  let content = "if true {\n    fmt.Println(\"test\")\nif false {\n    fmt.Println(\"test\")"
      result = parseTypus content
  in case result of
       Left _ -> property $ True  -- Should fail due to syntax error
       Right typusFile -> property $ not (L.null (tfSyntaxErrors typusFile))

-- Property: well-formed Go code should have no syntax errors
prop_well_formed_no_syntax_errors :: Property
prop_well_formed_no_syntax_errors =
  let content = unlines 
        [ "package main"
        , "import \"fmt\""
        , "func main() {"
        , "    if true {"
        , "        fmt.Println(\"Hello\")"
        , "    }"
        , "}"
        ]
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> property $ L.null (tfSyntaxErrors typusFile)

-- ============================================================================
-- Content Preservation Properties
-- ============================================================================

-- Property: parsing L.and extracting content should preserve meaningful parts
prop_parse_preserves_meaningful_content :: Property
prop_parse_preserves_meaningful_content =
  forAll genSimpleTypusFile $ \content ->
  let result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         let hasContent = not (L.null (tfBlocks typusFile)) || 
                        not (L.null (tfBuildTags typusFile)) ||
                        tfDirectives typusFile /= defaultFileDirectives
         in property $ hasContent || L.all isSpace (trim content)

-- Property: empty lines should be ignored in parsing
prop_empty_lines_ignored :: Property
prop_empty_lines_ignored =
  forAll (listOf (return "")) $ \emptyLines ->
  let content = unlines emptyLines
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         property $ tfDirectives typusFile === defaultFileDirectives .&&.
                    L.null (tfBuildTags typusFile) .&&.
                    L.null (tfBlocks typusFile)

-- ============================================================================
-- Directive Value Properties
-- ============================================================================

-- Property: constraints directive should also set dependent_types
prop_constraints_sets_dependent_types :: Property
prop_constraints_sets_dependent_types =
  let content = "//! constraints: true"
      result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         let dirs = tfDirectives typusFile
         in case (fdConstraints dirs, fdDependentTypes dirs) of
              (Just (Located _ True), Just (Located _ True)) -> property $ True
              _ -> property $ False

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: malformed directives should produce error messages
prop_malformed_directive_error_message :: Property
prop_malformed_directive_error_message =
  let content = "//! malformed directive"
      result = parseTypus content
  in case result of
       Left errMsg -> property $ "Invalid file directive format" `L.isInfixOf` errMsg
       Right _ -> property $ False

-- Property: unclosed block directives should fail
prop_unclosed_block_directive_fails :: Property
prop_unclosed_block_directive_fails =
  let content = "{//! ownership: true\npackage main\nfunc main() {}"
      result = parseTypus content
  in case result of
       Left _ -> property $ True
       Right _ -> property $ False

-- ============================================================================
-- Position Tracking Properties
-- ============================================================================

-- Property: parsed blocks should have valid source spans
prop_parsed_blocks_valid_spans :: Property
prop_parsed_blocks_valid_spans =
  forAll genTypusContent $ \content ->
  let result = parseTypus content
  in case result of
       Left _ -> property $ False
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             spansValid = L.all (\block -> 
               let span = cbSpan block
                   start = spanStart span
                   end = spanEnd span
               in posLine start <= posLine end && 
                  (posLine start < posLine end || posColumn start <= posColumn end)
             ) blocks
         in property $ spansValid

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ testGroup "FileDirectives Properties"
    [ fastProperty "defaultFileDirectives nothing" prop_defaultFileDirectives_nothing
    , fastProperty "parse empty file default directives" prop_parse_empty_file_default_directives
    , fastProperty "parse whitespace file default directives" prop_parse_whitespace_file_default_directives
    ]
  , testGroup "Directive Parsing Properties"
    [ fastProperty "parse valid file directive" prop_parse_valid_file_directive
    , fastProperty "parse multiple file directives" prop_parse_multiple_file_directives
    , fastProperty "parse invalid directive fails" prop_parse_invalid_directive_fails
    ]
  , testGroup "Block Parsing Properties"
    [ fastProperty "defaultBlockDirectives nothing" prop_defaultBlockDirectives_nothing
    , fastProperty "parse simple code block" prop_parse_simple_code_block
    , fastProperty "parse block directives" prop_parse_block_directives
    ]
  , testGroup "Build Tag Properties"
    [ fastProperty "parse build tags" prop_parse_build_tags
    , fastProperty "parse multiple build tags" prop_parse_multiple_build_tags
    ]
  , testGroup "Syntax Error Properties"
    [ fastProperty "if without brace syntax error" prop_if_without_brace_syntax_error
    , fastProperty "well formed no syntax errors" prop_well_formed_no_syntax_errors
    ]
  , testGroup "Content Preservation Properties"
    [ fastProperty "parse preserves meaningful content" prop_parse_preserves_meaningful_content
    , fastProperty "empty lines ignored" prop_empty_lines_ignored
    ]
  , testGroup "Directive Value Properties"
    [ fastProperty "constraints sets dependent_types" prop_constraints_sets_dependent_types
    ]
  , testGroup "Error Handling Properties"
    [ fastProperty "malformed directive error message" prop_malformed_directive_error_message
    , fastProperty "unclosed block directive fails" prop_unclosed_block_directive_fails
    ]
  , testGroup "Position Tracking Properties"
    [ fastProperty "parsed blocks valid spans" prop_parsed_blocks_valid_spans
    ]
  ]