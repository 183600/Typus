{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewParserBoundaryQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Parser
  ( parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives
  )
import SourceLocation (SourceSpan(..), SourcePos(..))
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)

-- | Test empty input handling
prop_parse_empty_input :: Bool
prop_parse_empty_input = 
    case parseTypus "" of
      Left _ -> False
      Right tf -> null (tfBlocks tf) && tfDirectives tf == defaultFileDirectives

prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only ws =
    all isSpace ws ==>
    case parseTypus ws of
      Left _ -> False
      Right tf -> null (tfBlocks tf) && tfDirectives tf == defaultFileDirectives

-- | Test file directive parsing
prop_parse_file_directive_valid :: String -> Bool
prop_parse_file_directive_valid directive =
    let validDirectives = ["ownership", "dependent_types", "constraints"]
        validValues = ["on", "off", "true", "false"]
        testDirective dir val = "//!" ++ dir ++ ": " ++ val
    in any (\d -> testDirective d "on" `isInfixOf` directive) validDirectives ==>
       case parseTypus directive of
         Left _ -> False
         Right tf -> tfDirectives tf /= defaultFileDirectives

prop_parse_file_directive_invalid :: String -> Property
prop_parse_file_directive_invalid invalid =
    not ("//!" `isPrefixOf` invalid) && length invalid > 0 ==>
    case parseTypus invalid of
      Left _ -> False  -- Should not fail on invalid directive, just ignore
      Right tf -> tfDirectives tf == defaultFileDirectives

prop_parse_multiple_file_directives :: String -> String -> String -> Property
prop_parse_multiple_file_directives d1 d2 d3 =
    all ("//!" `isPrefixOf`) [d1, d2, d3] &&
    all (":" `isInfixOf`) [d1, d2, d3] ==>
    let input = unlines [d1, d2, d3]
    in case parseTypus input of
         Left _ -> False
         Right tf -> tfDirectives tf /= defaultFileDirectives

-- | Test block directive parsing
prop_parse_block_directive_basic :: String -> Bool
prop_parse_block_directive_basic content =
    let directive = "{//! ownership: on}"
        input = directive ++ "\n" ++ content ++ "\n}"
    in case parseTypus input of
         Left _ -> False
         Right tf -> length (tfBlocks tf) == 1 && 
                    cbDirectives (head (tfBlocks tf)) /= defaultBlockDirectives

prop_parse_block_directive_nested :: String -> String -> Property
prop_parse_block_directive_nested outerContent innerContent =
    length outerContent > 0 && length innerContent > 0 ==>
    let input = "{//! ownership: on}\n" ++ outerContent ++ "\n" ++
                "{//! dependent_types: off}\n" ++ innerContent ++ "\n}\n}"
    in case parseTypus input of
         Left _ -> False
         Right tf -> length (tfBlocks tf) >= 1

prop_parse_block_directive_unclosed :: String -> Property
prop_parse_block_directive_unclosed content =
    not ("}" `isInfixOf` content) ==>
    let input = "{//! ownership: on}\n" ++ content
    in case parseTypus input of
         Left _ -> True  -- Should fail on unclosed block
         Right _ -> False

-- | Test code block parsing
prop_parse_code_block_without_directives :: String -> Property
prop_parse_code_block_without_directives code =
    length code > 0 && not ("{//!" `isInfixOf` code) ==>
    case parseTypus code of
      Left _ -> False
      Right tf -> length (tfBlocks tf) >= 1

prop_parse_multiple_code_blocks :: String -> String -> Property
prop_parse_multiple_code_blocks block1 block2 =
    length block1 > 0 && length block2 > 0 &&
    not ("{//!" `isInfixOf` block1) && not ("{//!" `isInfixOf` block2) ==>
    let input = block1 ++ "\n\n" ++ block2
    in case parseTypus input of
         Left _ -> False
         Right tf -> length (tfBlocks tf) >= 2

-- | Test build tag parsing
prop_parse_build_tag_go :: String -> Property
prop_parse_build_tag_go tag =
    length tag > 0 && not (isSpace (head tag)) ==>
    let input = "//go:build " ++ tag ++ "\nsome code"
    in case parseTypus input of
         Left _ -> False
         Right tf -> not (null (tfBuildTags tf))

prop_parse_build_tag_plus :: String -> Property
prop_parse_build_tag_plus tag =
    length tag > 0 && not (isSpace (head tag)) ==>
    let input = "// +build " ++ tag ++ "\nsome code"
    in case parseTypus input of
         Left _ -> False
         Right tf -> not (null (tfBuildTags tf))

-- | Test error handling
prop_parse_maintains_line_structure :: String -> Property
prop_parse_maintains_line_structure input =
    let lineCount = length (lines input)
    in lineCount > 0 ==>
    case parseTypus input of
      Left _ -> False
      Right tf -> sum (map (length . lines . cbContent) (tfBlocks tf)) <= lineCount

prop_parse_preserves_content_order :: String -> String -> Property
prop_parse_preserves_content_order first second =
    length first > 0 && length second > 0 ==>
    let input = first ++ "\n" ++ second
    in case parseTypus input of
         Left _ -> False
         Right tf -> 
           case tfBlocks tf of
             [] -> False
             (b:_) -> first `isInfixOf` cbContent b

-- | Test edge cases
prop_parse_unicode_content :: String -> Property
prop_parse_unicode_content unicode =
    length unicode > 0 && any (> 127) (map fromEnum unicode) ==>
    case parseTypus unicode of
      Left _ -> False
      Right tf -> not (null (tfBlocks tf)) || 
                  tfDirectives tf /= defaultFileDirectives

prop_parse_very_long_line :: String -> Property
prop_parse_very_long_line base =
    length base > 0 ==>
    let longLine = concat (replicate 1000 base) ++ "\n"
    in case parseTypus longLine of
         Left _ -> False
         Right tf -> not (null (tfBlocks tf))

prop_parse_mixed_indentation :: String -> Property
prop_parse_mixed_indentation content =
    length content > 0 ==>
    let mixedIndent = unlines 
          [ "  " ++ content  -- spaces
          , "\t" ++ content  -- tabs
          , " \t " ++ content  -- mixed
          ]
    in case parseTypus mixedIndent of
         Left _ -> False
         Right tf -> not (null (tfBlocks tf))

-- | Test directive value parsing
prop_parse_boolean_values :: String -> Property
prop_parse_boolean_values value =
    value `elem` ["on", "off", "true", "false"] ==>
    let input = "//! ownership: " ++ value
    in case parseTypus input of
         Left _ -> False
         Right tf -> tfDirectives tf /= defaultFileDirectives

prop_parse_invalid_boolean_values :: String -> Property
prop_parse_invalid_boolean_values value =
    not (value `elem` ["on", "off", "true", "false"]) && length value > 0 ==>
    let input = "//! ownership: " ++ value
    in case parseTypus input of
         Left _ -> True  -- Should fail on invalid boolean values
         Right _ -> False

-- | Test nested structures
prop_parse_deeply_nested_blocks :: String -> Property
prop_parse_deeply_nested_blocks content =
    length content > 0 ==>
    let nested = concat (replicate 10 ("{//! ownership: on}\n" ++ content ++ "\n"))
        closing = concat (replicate 10 "}")
        input = nested ++ closing
    in case parseTypus input of
         Left _ -> False
         Right tf -> length (tfBlocks tf) >= 1

-- | Test comment handling
prop_parse_line_comments :: String -> Property
prop_parse_line_comments code =
    length code > 0 && not ("//" `isInfixOf` code) ==>
    let withComment = code ++ "\n// This is a comment\nmore code"
    in case parseTypus withComment of
         Left _ -> False
         Right tf -> not (null (tfBlocks tf))

prop_parse_comments_in_directives :: String -> Property
prop_parse_comments_in_directives directive =
    "//!" `isPrefixOf` directive && ":" `isInfixOf` directive ==>
    let withComment = directive ++ " // comment"
    in case parseTypus withComment of
         Left _ -> False
         Right tf -> tfDirectives tf /= defaultFileDirectives

tests :: TestTree
tests = testGroup "Parser Boundary Conditions QuickCheck Tests"
  [ testProperty "parse empty input" prop_parse_empty_input
  , testProperty "parse whitespace only" prop_parse_whitespace_only
  , testProperty "parse file directive valid" prop_parse_file_directive_valid
  , testProperty "parse file directive invalid" prop_parse_file_directive_invalid
  , testProperty "parse multiple file directives" prop_parse_multiple_file_directives
  , testProperty "parse block directive basic" prop_parse_block_directive_basic
  , testProperty "parse block directive nested" prop_parse_block_directive_nested
  , testProperty "parse block directive unclosed" prop_parse_block_directive_unclosed
  , testProperty "parse code block without directives" prop_parse_code_block_without_directives
  , testProperty "parse multiple code blocks" prop_parse_multiple_code_blocks
  , testProperty "parse build tag go" prop_parse_build_tag_go
  , testProperty "parse build tag plus" prop_parse_build_tag_plus
  , testProperty "parse maintains line structure" prop_parse_maintains_line_structure
  , testProperty "parse preserves content order" prop_parse_preserves_content_order
  , testProperty "parse unicode content" prop_parse_unicode_content
  , testProperty "parse very long line" prop_parse_very_long_line
  , testProperty "parse mixed indentation" prop_parse_mixed_indentation
  , testProperty "parse boolean values" prop_parse_boolean_values
  , testProperty "parse invalid boolean values" prop_parse_invalid_boolean_values
  , testProperty "parse deeply nested blocks" prop_parse_deeply_nested_blocks
  , testProperty "parse line comments" prop_parse_line_comments
  , testProperty "parse comments in directives" prop_parse_comments_in_directives
  ]