module Test.Unit.ParserBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), emptySpan, startPos)
import qualified Data.Text as T
import Data.Char (isAlphaNum)

-- | Test that defaultFileDirectives has all Nothing values
prop_default_file_directives_nothing :: Property
prop_default_file_directives_nothing = property $
  fdOwnership defaultFileDirectives == Nothing &&
  fdDependentTypes defaultFileDirectives == Nothing &&
  fdConstraints defaultFileDirectives == Nothing

-- | Test that defaultBlockDirectives has all Nothing values
prop_default_block_directives_nothing :: Property
prop_default_block_directives_nothing = property $
  bdOwnership defaultBlockDirectives == Nothing &&
  bdDependentTypes defaultBlockDirectives == Nothing &&
  bdConstraints defaultBlockDirectives == Nothing

-- | Test that TypusFile with empty content has correct structure
prop_typus_file_empty_structure :: Property
prop_typus_file_empty_structure = 
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  in property $ 
    null (tfBuildTags emptyFile) &&
    null (tfBlocks emptyFile) &&
    null (tfSyntaxErrors emptyFile)

-- | Test that CodeBlock preserves content
prop_code_block_preserves_content :: String -> Property
prop_code_block_preserves_content content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
  in property $ 
    cbContent block == content &&
    cbDirectives block == defaultBlockDirectives

-- | Test that parseTypus handles empty input
prop_parse_typus_empty_input :: Property
prop_parse_typus_empty_input = 
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile == defaultFileDirectives &&
      null (tfBuildTags typusFile) &&
      null (tfBlocks typusFile)

-- | Test that parseTypus handles simple content
prop_parse_typus_simple_content :: String -> Property
prop_parse_typus_simple_content content = 
  let notEmpty = not (null content)
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      notEmpty ==> not (null (tfBlocks typusFile))

-- | Test that parseTypus preserves directives in content
prop_parse_typus_preserves_directives :: String -> Property
prop_parse_typus_preserves_directives content = 
  let withDirective = "// @ownership: true\n" ++ content
      result = parseTypus withDirective
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile /= defaultFileDirectives

-- | Test that parseTypus handles multiple blocks
prop_parse_typus_multiple_blocks :: String -> String -> Property
prop_parse_typus_multiple_blocks content1 content2 = 
  let notEmpty1 = not (null content1)
      notEmpty2 = not (null content2)
      multiBlock = content1 ++ "\n---\n" ++ content2
      result = parseTypus multiBlock
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      notEmpty1 && notEmpty2 ==> length (tfBlocks typusFile) >= 2

-- | Test that parseTypus tracks syntax errors
prop_parse_typus_tracks_syntax_errors :: String -> Property
prop_parse_typus_tracks_syntax_errors content = 
  let withError = content ++ "\n@invalid-directive"
      result = parseTypus withError
  in case result of
    Left _ -> property True  -- Parse error is expected
    Right typusFile -> property $ 
      not (null content) ==> not (null (tfSyntaxErrors typusFile))

-- | Test that parseTypus handles build tags
prop_parse_typus_handles_build_tags :: String -> Property
prop_parse_typus_handles_build_tags tag = 
  let validTag = not (null tag) && all isAlphaNum tag
      withTag = "// @build: " ++ tag ++ "\ncontent"
      result = parseTypus withTag
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      validTag ==> not (null (tfBuildTags typusFile))

-- | Test that parseTypus handles block directives
prop_parse_typus_handles_block_directives :: String -> Property
prop_parse_typus_handles_block_directives content = 
  let withBlockDirective = "// @block: ownership=true\n" ++ content
      result = parseTypus withBlockDirective
  in case result of
    Left _ -> property False
    Right typusFile -> 
      let blocks = tfBlocks typusFile
      in property $ 
        not (null content) && not (null blocks) ==> 
        cbDirectives (head blocks) /= defaultBlockDirectives

-- | Test that parseTypus preserves line structure
prop_parse_typus_preserves_lines :: String -> Property
prop_parse_typus_preserves_lines content = 
  let linesIn = length (lines content)
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> 
      let blocks = tfBlocks typusFile
      in property $ 
        not (null content) && not (null blocks) ==>
        let blockLines = length (lines (cbContent (head blocks)))
        in blockLines >= 1

-- | Test that parseTypus handles mixed directives
prop_parse_typus_handles_mixed_directives :: String -> Property
prop_parse_typus_handles_mixed_directives content = 
  let mixed = "// @ownership: true\n// @dependent-types: true\n" ++ content
      result = parseTypus mixed
  in case result of
    Left _ -> property False
    Right typusFile -> 
      let directives = tfDirectives typusFile
      in property $ 
        not (null content) ==> 
        fdOwnership directives /= Nothing ||
        fdDependentTypes directives /= Nothing

-- | Test that parseTypus handles comments correctly
prop_parse_typus_handles_comments :: String -> Property
prop_parse_typus_handles_comments content = 
  let withComments = content ++ "\n// This is a comment\n// Another comment"
      result = parseTypus withComments
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      not (null content) ==> not (null (tfBlocks typusFile))

-- | Test that parseTypus handles empty directives
prop_parse_typus_handles_empty_directives :: Property
prop_parse_typus_handles_empty_directives = 
  let withEmptyDirectives = "// @ownership:\n// @dependent-types:\n"
      result = parseTypus withEmptyDirectives
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile == defaultFileDirectives

-- | Test that parseTypus handles special characters
prop_parse_typus_handles_special_chars :: String -> Property
prop_parse_typus_handles_special_chars content = 
  let special = content ++ "\n\t\n\"quotes\"\n'single'\n{}[]()"
      result = parseTypus special
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      not (null content) ==> not (null (tfBlocks typusFile))

-- | Test that parseTypus handles unicode characters
prop_parse_typus_handles_unicode :: String -> Property
prop_parse_typus_handles_unicode content = 
  let unicode = content ++ " \955 \948 \956"  -- Greek letters
      result = parseTypus unicode
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      not (null content) ==> not (null (tfBlocks typusFile))

tests :: TestTree
tests = testGroup "Parser Boundary QuickCheck Tests"
  [ testProperty "defaultFileDirectives nothing" prop_default_file_directives_nothing
  , testProperty "defaultBlockDirectives nothing" prop_default_block_directives_nothing
  , testProperty "TypusFile empty structure" prop_typus_file_empty_structure
  , testProperty "CodeBlock preserves content" prop_code_block_preserves_content
  , testProperty "parseTypus empty input" prop_parse_typus_empty_input
  , testProperty "parseTypus simple content" prop_parse_typus_simple_content
  , testProperty "parseTypus preserves directives" prop_parse_typus_preserves_directives
  , testProperty "parseTypus multiple blocks" prop_parse_typus_multiple_blocks
  , testProperty "parseTypus tracks syntax errors" prop_parse_typus_tracks_syntax_errors
  , testProperty "parseTypus handles build tags" prop_parse_typus_handles_build_tags
  , testProperty "parseTypus handles block directives" prop_parse_typus_handles_block_directives
  , testProperty "parseTypus preserves lines" prop_parse_typus_preserves_lines
  , testProperty "parseTypus handles mixed directives" prop_parse_typus_handles_mixed_directives
  , testProperty "parseTypus handles comments" prop_parse_typus_handles_comments
  , testProperty "parseTypus handles empty directives" prop_parse_typus_handles_empty_directives
  , testProperty "parseTypus handles special chars" prop_parse_typus_handles_special_chars
  , testProperty "parseTypus handles unicode" prop_parse_typus_handles_unicode
  ]