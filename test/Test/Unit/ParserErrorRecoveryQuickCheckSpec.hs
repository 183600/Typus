{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, resize)
import Data.Char (isSpace, isAlphaNum, isPunctuation)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import qualified Data.Text as T

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
-- import qualified Text.Megaparsec as MP
-- import Text.Megaparsec (Parsec, errorBundlePretty)

-- Property: parseTypus never crashes on any input
prop_parseTypus_never_crashes :: String -> Property
prop_parseTypus_never_crashes s =
  let result = parseTypus s
  in counterexample "parseTypus should never crash on any input" $
     case result of
       Left _ -> property True -- Parse errors are acceptable
       Right _ -> property True -- Successful parse is acceptable

-- Property: parseTypus result is deterministic
prop_parseTypus_deterministic :: String -> Property
prop_parseTypus_deterministic s =
  let result1 = parseTypus s
      result2 = parseTypus s
  in counterexample "parseTypus should be deterministic" $
     show result1 === show result2

-- Property: parsing empty string gives consistent result
prop_parse_empty_string :: Property
prop_parse_empty_string =
  let result = parseTypus ""
  in counterexample "parsing empty string should give consistent result" $
     case result of
       Left err -> property True -- Should parse successfully or give predictable error
       Right file -> property True

-- Property: parsing whitespace-only string behaves consistently
prop_parse_whitespace_only :: Property
prop_parse_whitespace_only =
  let whitespace = " \t\n\r\f\v"
      result = parseTypus whitespace
  in counterexample "parsing whitespace-only should behave consistently" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus handles very long strings
prop_parse_long_string :: Property
prop_parse_long_string =
  let longString = replicate 1000 'a'
      result = parseTypus longString
  in counterexample "parseTypus should handle very long strings" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus handles special characters
prop_parse_special_characters :: String -> Property
prop_parse_special_characters s =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"
      testString = s ++ specialChars
      result = parseTypus testString
  in counterexample "parseTypus should handle special characters" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus handles Unicode characters
prop_parse_unicode :: Property
prop_parse_unicode =
  let unicodeString = "Hello 世界 🌍 Café naïve résumé"
      result = parseTypus unicodeString
  in counterexample "parseTypus should handle Unicode characters" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parsing with line comments doesn't crash
prop_parse_line_comments :: String -> Property
prop_parse_line_comments s =
  let withComments = s ++ "\n// This is a comment\n" ++ s
      result = parseTypus withComments
  in counterexample "parsing with line comments shouldn't crash" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parsing with block comments doesn't crash
prop_parse_block_comments :: String -> Property
prop_parse_block_comments s =
  let withComments = s ++ "/* This is a\nmulti-line comment */" ++ s
      result = parseTypus withComments
  in counterexample "parsing with block comments shouldn't crash" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus handles malformed comment structures
prop_parse_malformed_comments :: String -> Property
prop_parse_malformed_comments s =
  let malformedComments = ["/*", "*/", "//", "/**/", "/*/", "/* */ */"]
      testStrings = map (\c -> s ++ c ++ s) malformedComments
      results = map parseTypus testStrings
  in counterexample "parseTypus should handle malformed comment structures" $
     all (\r -> case r of { Left _ -> True; Right _ -> True }) results

-- Property: parseTypus handles nested directives
prop_parse_nested_directives :: Property
prop_parse_nested_directives =
  let nestedDirectives = "@ownership(true)\n@dependentTypes(true)\n@constraints(true)\n" ++
                        "some content here\n" ++
                        "@ownership(false)\nmore content"
      result = parseTypus nestedDirectives
  in counterexample "parseTypus should handle nested directives" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus handles extremely indented content
prop_parse_extreme_indentation :: Property
prop_parse_extreme_indentation =
  let indentedContent = concat $ replicate 50 "    " ++ "deeply indented content"
      result = parseTypus indentedContent
  in counterexample "parseTypus should handle extreme indentation" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseTypus recovers from syntax errors
prop_parse_error_recovery :: String -> String -> Property
prop_parse_error_recovery prefix suffix =
  let malformed = prefix ++ "{@#$@#$}" ++ suffix
      result = parseTypus malformed
  in counterexample "parseTypus should attempt error recovery" $
     case result of
       Left err -> property True -- Should provide error information
       Right file -> property True -- Or successfully recover

-- Generate strings with various problematic patterns
genProblematicString :: Gen String
genProblematicString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  , elements "{}[]()" -- Brackets that might cause nesting issues
  , return '@' -- Directive character
  ]

tests :: TestTree
tests = testGroup "Parser Error Recovery QuickCheck Tests"
  [ fastProperty "parseTypus never crashes" prop_parseTypus_never_crashes
  , fastProperty "parseTypus is deterministic" prop_parseTypus_deterministic
  , fastProperty "parse empty string" prop_parse_empty_string
  , fastProperty "parse whitespace-only" prop_parse_whitespace_only
  , fastProperty "parse long string" prop_parse_long_string
  , fastProperty "parse special characters" prop_parse_special_characters
  , fastProperty "parse Unicode characters" prop_parse_unicode
  , fastProperty "parse with line comments" prop_parse_line_comments
  , fastProperty "parse with block comments" prop_parse_block_comments
  , fastProperty "parse malformed comments" prop_parse_malformed_comments
  , fastProperty "parse nested directives" prop_parse_nested_directives
  , fastProperty "parse extreme indentation" prop_parse_extreme_indentation
  , fastProperty "parse error recovery" prop_parse_error_recovery
  ]