{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf, resize)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), SourcePos(..), posLine, posColumn)
import Data.Char (isSpace, isAlphaNum, isControl, isAscii)
import qualified Data.Text as T
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate)

-- ============================================================================
-- New Parser Robustness Tests
-- ============================================================================

-- Property: Parser handles empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in case result of
       Left _ -> property $ True  -- Empty input might fail, but shouldn't crash
       Right file -> property $ tfDirectives file === defaultFileDirectives .&&.
                           L.null (tfBuildTags file) .&&.
                           L.null (tfBlocks file)

-- Property: Parser handles whitespace-only input gracefully
prop_parser_whitespace_only :: String -> Property
prop_parser_whitespace_only input =
  L.all isSpace input ==>
  let result = parseTypus input
  in case result of
       Left _ -> property $ True  -- Whitespace-only might fail, but shouldn't crash
       Right file -> property $ tfDirectives file === defaultFileDirectives .&&.
                           L.null (tfBuildTags file) .&&.
                           L.null (tfBlocks file)

-- Property: Parser handles very long lines without crashing
prop_parser_long_lines :: Int -> String -> Property
prop_parser_long_lines multiplier base =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let longLine = L.concat (replicate multiplier base) ++ "\n"
      result = parseTypus longLine
  in case result of
       Left _ -> property $ True  -- Long lines might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles many lines without crashing
prop_parser_many_lines :: Int -> String -> Property
prop_parser_many_lines numLines content =
  numLines >= 0 && numLines <= 100 ==>  -- Limit for performance
  let manyLines = unlines (replicate numLines content)
      result = parseTypus manyLines
  in case result of
       Left _ -> property $ True  -- Many lines might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles Unicode characters gracefully
prop_parser_unicode_handling :: String -> Property
prop_parser_unicode_handling input =
  let unicodeInput = input ++ "café naïve résumé 🚀 测试\n"
      result = parseTypus unicodeInput
  in case result of
       Left _ -> property $ True  -- Unicode might cause parse errors, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles control characters gracefully
prop_parser_control_characters :: String -> Property
prop_parser_control_characters input =
  not (L.any isControl input) ==>  -- Avoid too many control characters
  let controlInput = input ++ "\1\2\3\4\5\n"
      result = parseTypus controlInput
  in case result of
       Left _ -> property $ True  -- Control characters might cause parse errors, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles malformed directives gracefully
prop_parser_malformed_directives :: String -> Property
prop_parser_malformed_directives content =
  not ("//" `L.isInfixOf` content) && not ("{!" `L.isInfixOf` content) ==>  -- Avoid existing directives
  let malformed = content ++ "//! malformed_directive_without_colon\n" ++
                  content ++ "{//! malformed_block_directive\n" ++
                  content ++ "//! :colon_without_key\n"
      result = parseTypus malformed
  in case result of
       Left _ -> property $ True  -- Malformed directives should fail gracefully
       Right _ -> property $ True  -- Or succeed if parser is tolerant

-- Property: Parser handles nested braces gracefully
prop_parser_nested_braces :: Int -> String -> Property
prop_parser_nested_braces depth content =
  depth >= 0 && depth <= 20 ==>  -- Limit nesting depth
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      nested = content ++ openBraces ++ content ++ closeBraces ++ "\n"
      result = parseTypus nested
  in case result of
       Left _ -> property $ True  -- Nested braces might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles mismatched braces gracefully
prop_parser_mismatched_braces :: String -> Property
prop_parser_mismatched_braces content =
  not ("{" `L.isInfixOf` content) && not ("}" `L.isInfixOf` content) ==>  -- Avoid existing braces
  let mismatched = content ++ "{{{\n" ++ content ++ "}}\n"  -- Unmatched braces
      result = parseTypus mismatched
  in case result of
       Left _ -> property $ True  -- Mismatched braces should fail gracefully
       Right _ -> property $ True  -- Or succeed if parser is tolerant

-- Property: Parser handles very long identifiers gracefully
prop_parser_long_identifiers :: Int -> String -> Property
prop_parser_long_identifiers L.length base =
  L.length >= 0 && L.length <= 100 ==>  -- Limit identifier L.length
  let longIdent = L.concat (replicate L.length base)
      withIdent = "//! " ++ longIdent ++ ": value\n" ++ longIdent ++ " := 42\n"
      result = parseTypus withIdent
  in case result of
       Left _ -> property $ True  -- Long identifiers might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles mixed line endings gracefully
prop_parser_mixed_line_endings :: String -> Property
prop_parser_mixed_line_endings content =
  not ('\n' `elem` content) && not ('\r' `elem` content) ==>  -- Avoid existing line endings
  let mixed = content ++ "\n" ++ content ++ "\r\n" ++ content ++ "\r" ++ content
      result = parseTypus mixed
  in case result of
       Left _ -> property $ True  -- Mixed line endings might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles comments in unusual contexts
prop_parser_unusual_comments :: String -> Property
prop_parser_unusual_comments content =
  not ("//" `L.isInfixOf` content) ==>  -- Avoid existing comments
  let unusualComments = content ++ "// comment at end without newline" ++
                       "\n// comment with special chars !@#$%^&*()\n" ++
                       content ++ "//! directive with // inside\n"
      result = parseTypus unusualComments
  in case result of
       Left _ -> property $ True  -- Unusual comments might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles extreme indentation
prop_parser_extreme_indentation :: Int -> String -> Property
prop_parser_extreme_indentation spaces content =
  spaces >= 0 && spaces <= 100 ==>  -- Limit indentation
  let extremeIndent = replicate spaces ' ' ++ content ++ "\n"
      result = parseTypus extremeIndent
  in case result of
       Left _ -> property $ True  -- Extreme indentation might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles repeated directives
prop_parser_repeated_directives :: Int -> String -> Property
prop_parser_repeated_directives count directive =
  count >= 0 && count <= 20 ==>  -- Limit repetition
  let repeated = unlines (replicate count ("//! " ++ directive ++ ": true"))
      result = parseTypus repeated
  in case result of
       Left _ -> property $ True  -- Repeated directives might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles empty blocks
prop_parser_empty_blocks :: String -> Property
prop_parser_empty_blocks content =
  not ("{!" `L.isInfixOf` content) ==>  -- Avoid existing block directives
  let emptyBlocks = content ++ "{//! }\n" ++ content ++ "{//! :}\n" ++ content ++ "{//! key:}\n"
      result = parseTypus emptyBlocks
  in case result of
       Left _ -> property $ True  -- Empty blocks might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles special characters in identifiers
prop_parser_special_chars :: String -> Property
prop_parser_special_chars base =
  let specialChars = "!@#$%^&*()_+-=[]{}|;:',.<>/?`~"
      withSpecial = "//! " ++ base ++ specialChars ++ ": value\n"
      result = parseTypus withSpecial
  in case result of
       Left _ -> property $ True  -- Special chars might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser preserves line structure in successful parses
prop_parser_preserves_line_structure :: [String] -> Property
prop_parser_preserves_line_structure lines' =
  not (null lines') && L.length lines' <= 50 ==>  -- Limit for performance
  let input = unlines lines'
      result = parseTypus input
  in case result of
       Right file -> property $ L.length (tfBlocks file) <= L.length lines'
       Left _ -> property $ True  -- Parse failure is acceptable

-- Property: Parser handles null bytes gracefully
prop_parser_null_bytes :: String -> Property
prop_parser_null_bytes input =
  not ('\0' `elem` input) ==>  -- Avoid existing null bytes
  let withNull = input ++ "\0" ++ input ++ "\n"
      result = parseTypus withNull
  in case result of
       Left _ -> property $ True  -- Null bytes might cause parse errors, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser handles very large files gracefully
prop_parser_large_files :: Int -> String -> Property
prop_parser_large_files multiplier base =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let largeContent = unlines (replicate multiplier (base ++ " content"))
      result = parseTypus largeContent
  in case result of
       Left _ -> property $ True  -- Large files might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Property: Parser is deterministic
prop_parser_deterministic :: String -> Property
prop_parser_deterministic input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
       (Left _, Left _) -> property $ True
       (Right f1, Right f2) -> property $ f1 === f2
       (Left _, Right _) -> property $ False  -- Shouldn't happen
       (Right _, Left _) -> property $ False  -- Shouldn't happen

-- Property: Parser handles escape sequences gracefully
prop_parser_escape_sequences :: String -> Property
prop_parser_escape_sequences content =
  not ('\\' `elem` content) ==>  -- Avoid existing escape sequences
  let withEscapes = content ++ "string with \\n\\t\\\"\\\\\\r escapes\n" ++
                   "//! directive with \\n escape: value\n"
      result = parseTypus withEscapes
  in case result of
       Left _ -> property $ True  -- Escape sequences might fail, but shouldn't crash
       Right _ -> property $ True  -- Success is also acceptable

-- Tests collection
tests :: TestTree
tests = testGroup "New Parser Robustness Tests"
  [ fastProperty "Parser handles empty input gracefully" prop_parser_empty_input
  , fastProperty "Parser handles whitespace-only input gracefully" prop_parser_whitespace_only
  , fastProperty "Parser handles very long lines without crashing" prop_parser_long_lines
  , fastProperty "Parser handles many lines without crashing" prop_parser_many_lines
  , fastProperty "Parser handles Unicode characters gracefully" prop_parser_unicode_handling
  , fastProperty "Parser handles control characters gracefully" prop_parser_control_characters
  , fastProperty "Parser handles malformed directives gracefully" prop_parser_malformed_directives
  , fastProperty "Parser handles nested braces gracefully" prop_parser_nested_braces
  , fastProperty "Parser handles mismatched braces gracefully" prop_parser_mismatched_braces
  , fastProperty "Parser handles very long identifiers gracefully" prop_parser_long_identifiers
  , fastProperty "Parser handles mixed line endings gracefully" prop_parser_mixed_line_endings
  , fastProperty "Parser handles comments in unusual contexts" prop_parser_unusual_comments
  , fastProperty "Parser handles extreme indentation" prop_parser_extreme_indentation
  , fastProperty "Parser handles repeated directives" prop_parser_repeated_directives
  , fastProperty "Parser handles empty blocks" prop_parser_empty_blocks
  , fastProperty "Parser handles special characters in identifiers" prop_parser_special_chars
  , fastProperty "Parser preserves line structure in successful parses" prop_parser_preserves_line_structure
  , fastProperty "Parser handles null bytes gracefully" prop_parser_null_bytes
  , fastProperty "Parser handles very large files gracefully" prop_parser_large_files
  , fastProperty "Parser is deterministic" prop_parser_deterministic
  , fastProperty "Parser handles escape sequences gracefully" prop_parser_escape_sequences
  ]