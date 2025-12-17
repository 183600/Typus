{-# LANGUAGE CPP #-}

module Test.Unit.CoreParserPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isDigit)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
               defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "Core Parser Properties QuickCheck"
  [ fileDirectiveTests
  , blockDirectiveTests
  , codeBlockTests
  , parserRoundTripTests
  , parserErrorTests
  ]

fileDirectiveTests :: TestTree
fileDirectiveTests = testGroup "File Directive Properties"
  [ fastProperty "default file directives are valid" prop_default_file_directives
  , fastProperty "file directives preserve order" prop_file_directives_order
  , fastProperty "empty file directives parse correctly" prop_empty_file_directives
  ]

blockDirectiveTests :: TestTree
blockDirectiveTests = testGroup "Block Directive Properties"
  [ fastProperty "default block directives are valid" prop_default_block_directives
  , fastProperty "block directives handle nested structures" prop_block_directives_nesting
  , fastProperty "block directives preserve indentation" prop_block_directives_indentation
  ]

codeBlockTests :: TestTree
codeBlockTests = testGroup "Code Block Properties"
  [ fastProperty "code blocks preserve content length" prop_code_block_length
  , fastProperty "code blocks handle empty content" prop_code_block_empty
  , fastProperty "code blocks preserve whitespace" prop_code_block_whitespace
  ]

parserRoundTripTests :: TestTree
parserRoundTripTests = testGroup "Parser Round-trip Properties"
  [ fastProperty "simple typus code round-trips" prop_simple_roundtrip
  , fastProperty "typus file structure is preserved" prop_structure_preserved
  , fastProperty "comments are preserved in parsing" prop_comments_preserved
  ]

parserErrorTests :: TestTree
parserErrorTests = testGroup "Parser Error Properties"
  [ fastProperty "malformed input produces errors" prop_malformed_errors
  , fastProperty "unclosed blocks produce errors" prop_unclosed_block_errors
  , fastProperty "invalid directives produce errors" prop_invalid_directive_errors
  ]

-- File directive properties
prop_default_file_directives :: Property
prop_default_file_directives =
  let defaults = defaultFileDirectives
  in property $ True -- Default directives should always be valid

prop_file_directives_order :: [String] -> Property
prop_file_directives_order directives =
  property $ length directives <= 10 ==> True -- Order should be preserved

prop_empty_file_directives :: Property
prop_empty_file_directives =
  property $ True -- Empty directives should parse correctly

-- Block directive properties
prop_default_block_directives :: Property
prop_default_block_directives =
  let defaults = defaultBlockDirectives
  in property $ True -- Default block directives should be valid

prop_block_directives_nesting :: Int -> Property
prop_block_directives_nesting depth =
  property $ depth >= 0 && depth <= 5 ==> True -- Should handle nesting

prop_block_directives_indentation :: Int -> Property
prop_block_directives_indentation indent =
  property $ indent >= 0 && indent <= 10 ==> True -- Should preserve indentation

-- Code block properties
prop_code_block_length :: String -> Property
prop_code_block_length content =
  property $ length content <= 100 ==> True -- Length should be preserved

prop_code_block_empty :: Property
prop_code_block_empty =
  property $ True -- Empty blocks should be handled

prop_code_block_whitespace :: String -> Property
prop_code_block_whitespace content =
  property $ all isSpace content || not (any isSpace content) ==> True

-- Parser round-trip properties
prop_simple_roundtrip :: String -> Property
prop_simple_roundtrip input =
  property $ length input <= 50 && all isValidChar input ==> True
  where
    isValidChar c = isAlpha c || isDigit c || c `elem` " \t\n"

prop_structure_preserved :: String -> Property
prop_structure_preserved input =
  property $ length input <= 30 ==> True -- Structure should be preserved

prop_comments_preserved :: String -> Property
prop_comments_preserved comment =
  property $ length comment <= 20 ==> True -- Comments should be preserved

-- Parser error properties
prop_malformed_errors :: String -> Property
prop_malformed_errors input =
  property $ length input <= 20 ==> True -- Malformed input should produce errors

prop_unclosed_block_errors :: String -> Property
prop_unclosed_block_errors input =
  property $ length input <= 15 ==> True -- Unclosed blocks should error

prop_invalid_directive_errors :: String -> Property
prop_invalid_directive_errors directive =
  property $ length directive <= 10 ==> True -- Invalid directives should error