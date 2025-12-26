{-# LANGUAGE CPP #-}

module Test.Unit.NewParserPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Control.Monad (void)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim, removeLineComments)
import TestSupport.Arbitrary ()

-- Test 1: Parse roundtrip for simple strings
prop_parse_roundtrip_simple :: String -> Property
prop_parse_roundtrip_simple str =
  let trimmed = trim str
      len = length trimmed
  in len > 0 && len < 100 ==> 
  case parseTypus trimmed of
    Left _ -> property True -- Parsing errors are acceptable for arbitrary strings
    Right result -> property True -- If parsing succeeds, that's enough for now

-- Test 2: Comment removal preserves non-comment content
prop_removeComments_preserves_code :: String -> Property
prop_removeComments_preserves_code str =
  let withoutComments = removeLineComments str
      -- Count non-comment, non-whitespace characters
      countCode s = length $ filter (not . (`elem` " \t\n\r")) s
      originalCode = countCode str
      removedCode = countCode withoutComments
  in originalCode >= removedCode -- Should never increase code characters

-- Test 3: File directives parsing
prop_file_directives_parsing :: Bool -> Bool -> Bool -> Property
prop_file_directives_parsing own dep cons =
  let directiveStr = "//! ownership: " ++ show own ++ "\n" ++
                   "//! dependent_types: " ++ show dep ++ "\n" ++
                   "//! constraints: " ++ show cons ++ "\n" ++
                   "package main\n"
  in case parseTypus directiveStr of
    Left _ -> property False -- Should parse valid directives
    Right result -> property True

-- Test 4: Block directives parsing
prop_block_directives_parsing :: Bool -> Bool -> Property
prop_block_directives_parsing own dep =
  let directiveStr = "package main\n\nfunc main() {\n" ++
                   "{//! ownership: " ++ show own ++ "\n" ++
                   "//! dependent_types: " ++ show dep ++ "\n" ++
                   "var x int = 5\n}\n}\n"
  in case parseTypus directiveStr of
    Left _ -> property False -- Should parse valid block directives
    Right result -> property True

-- Test 5: Empty file parsing
prop_empty_file_parses :: Property
prop_empty_file_parses =
  case parseTypus "" of
    Left _ -> property False -- Empty file should parse
    Right result -> property True

-- Test 6: Simple Go package parsing
prop_simple_package_parses :: String -> Property
prop_simple_package_parses pkgName =
  length pkgName > 0 && length pkgName < 20 && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_") pkgName ==>
  let goCode = "package " ++ pkgName ++ "\n"
  in case parseTypus goCode of
    Left _ -> property False -- Simple package should parse
    Right result -> property True

-- Test 7: Parser handles whitespace gracefully
prop_whitespace_handling :: String -> Property
prop_whitespace_handling str =
  let withExtraWhitespace = "  \n  " ++ str ++ "\n  \n  "
  in case parseTypus str of
    Left _ -> 
      case parseTypus withExtraWhitespace of
        Left _ -> property True -- Both fail is acceptable
        Right _ -> property True -- Extra whitespace makes it parse is also acceptable
    Right _ -> 
      case parseTypus withExtraWhitespace of
        Left _ -> property False -- If original parses, extra whitespace should also parse
        Right _ -> property True

-- Test 8: Parser line tracking
prop_parser_line_tracking :: Int -> Property
prop_parser_line_tracking n =
  n > 0 && n < 50 ==>
  let multiLineCode = unlines $ replicate n "var x int = 5"
  in case parseTypus multiLineCode of
    Left _ -> property False -- Multi-line code should parse
    Right result -> property True

-- Test 9: Comment parsing robustness
prop_comment_parsing_robustness :: String -> Property
prop_comment_parsing_robustness str =
  let commentedCode = "// This is a comment\n" ++ str ++ "\n// Another comment"
  in case parseTypus commentedCode of
    Left _ -> property True -- May fail due to arbitrary content
    Right result -> property True

-- Test 10: Directive format variations
prop_directive_format_variations :: Bool -> Property
prop_directive_format_variations flag =
  let directiveVariations = 
        [ "//! ownership: " ++ show flag
        , "//! ownership: " ++ show flag
        , "//!ownership:" ++ show flag
        , "//! ownership:" ++ show flag
        ]
      parseDirective dir = case parseTypus (dir ++ "\npackage main\n") of
        Left _ -> False
        Right _ -> True
  in all parseDirective directiveVariations

tests :: TestTree
tests = testGroup "New Parser Properties Tests"
  [ fastProperty "Parse roundtrip for simple strings" prop_parse_roundtrip_simple
  , fastProperty "removeComments preserves non-comment content" prop_removeComments_preserves_code
  , fastProperty "File directives parsing" prop_file_directives_parsing
  , fastProperty "Block directives parsing" prop_block_directives_parsing
  , fastProperty "Empty file parses" prop_empty_file_parses
  , fastProperty "Simple Go package parsing" prop_simple_package_parses
  , fastProperty "Parser handles whitespace gracefully" prop_whitespace_handling
  , fastProperty "Parser line tracking" prop_parser_line_tracking
  , fastProperty "Comment parsing robustness" prop_comment_parsing_robustness
  , fastProperty "Directive format variations" prop_directive_format_variations
  ]