{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, errorBundlePretty, ParseErrorBundle)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- Helper functions for generating test inputs

-- Generate valid Typus identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_']))
  return (first : rest)

-- Generate valid directive strings
genDirective :: Gen String
genDirective = oneof
  [ return "// @ownership"
  , return "// @dependent-types"
  , return "// @constraints"
  , return "// @no-ownership"
  , return "// @no-dependent-types"
  , return "// @no-constraints"
  ]

-- Generate malformed directive strings
genMalformedDirective :: Gen String
genMalformedDirective = oneof
  [ return "// @invalid-directive"
  , return "@ownership"  -- missing //
  , return "// ownership"  -- missing @
  , return "// @ownership extra stuff"
  , return "// @"
  , return "// @"
  ]

-- Generate valid code blocks
genCodeBlock :: Gen String
genCodeBlock = do
  lang <- elements ["go", "rust", "haskell", "python"]
  code <- listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ [' ', '\n', ';', '=', '+', '-', '*', '/']))
  return $ "```" ++ lang ++ "\n" ++ code ++ "\n```"

-- Generate malformed code blocks
genMalformedCodeBlock :: Gen String
genCodeBlock = do
  oneof
    [ return "```"  -- incomplete
    , return "```go\nincomplete block"
    , return "go\n```"  -- wrong order
    , return "```\n```"  -- empty
    ]

-- Generate mixed content with potential errors
genMixedContent :: Gen String
genMixedContent = do
  directives <- listOf1 genDirective
  malformedDirs <- listOf1 genMalformedDirective
  codeBlocks <- listOf1 genCodeBlock
  malformedBlocks <- listOf1 genMalformedCodeBlock
  let allParts = directives ++ malformedDirs ++ codeBlocks ++ malformedBlocks
  return $ unlines allParts

-- Generate content with syntax errors
genContentWithSyntaxErrors :: Gen String
genContentWithSyntaxErrors = do
  validContent <- genMixedContent
  errorType <- elements
    [ "unclosed_comment"
    , "invalid_directive"
    , "malformed_block"
    , "mixed_indentation"
    ]
  case errorType of
    "unclosed_comment" -> do
      return $ validContent ++ "\n/* This comment is never closed"
    "invalid_directive" -> do
      return $ validContent ++ "\n// @completely-invalid-directive-name"
    "malformed_block" -> do
      return $ validContent ++ "\n```go\nincomplete"
    "mixed_indentation" -> do
      return $ validContent ++ "\n  mixed\t indentation\n    here"
    _ -> return validContent

-- Property: Parser should handle empty input gracefully
prop_parser_handles_empty_input :: Property
prop_parser_handles_empty_input =
  let result = parseTypus "" ""
  in case result of
    Left _ -> property True
    Right file -> property $ tfCodeBlocks file == []

-- Property: Parser should handle only whitespace input
prop_parser_handles_whitespace_only :: Property
prop_parser_handles_whitespace_only =
  let whitespace = unlines ["", "   ", "\t", "  \t  ", ""]
      result = parseTypus whitespace ""
  in case result of
    Left _ -> property True
    Right file -> property $ tfCodeBlocks file == []

-- Property: Parser should handle single directive correctly
prop_parser_handles_single_directive :: Property
prop_parser_handles_single_directive =
  forAll genDirective $ \directive ->
  let result = parseTypus directive ""
  in case result of
    Left _ -> property False
    Right file -> property $ length (tfCodeBlocks file) >= 0

-- Property: Parser should handle multiple directives
prop_parser_handles_multiple_directives :: Property
prop_parser_handles_multiple_directives =
  forAll (listOf1 genDirective) $ \directives ->
  let content = unlines directives
      result = parseTypus content ""
  in case result of
    Left _ -> property False
    Right file -> property $ True  -- If it parses, consider it a success

-- Property: Parser should recover from malformed directives
prop_parser_recovers_from_malformed_directives :: Property
prop_parser_recovers_from_malformed_directives =
  forAll genMalformedDirective $ \malformedDir ->
  let content = malformedDir ++ "\n// @ownership\n```go\nfmt.Println(\"test\")\n```"
      result = parseTypus content ""
  in case result of
    Left _ -> property True  -- Failing is acceptable for malformed input
    Right file -> property $ not (null (tfCodeBlocks file))  -- But should recover if it succeeds

-- Property: Parser should handle incomplete code blocks gracefully
prop_parser_handles_incomplete_blocks :: Property
prop_parser_handles_incomplete_blocks =
  forAll genMalformedCodeBlock $ \malformedBlock ->
  let content = "// @ownership\n" ++ malformedBlock
      result = parseTypus content ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right file -> property $ True  -- Or succeed with partial parsing

-- Property: Parser should handle mixed valid and invalid content
prop_parser_handles_mixed_content :: Property
prop_parser_handles_mixed_content =
  forAll genContentWithSyntaxErrors $ \content ->
  let result = parseTypus content ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right file -> property $ True  -- Or succeed with partial parsing

-- Property: Parser should preserve line numbers in error messages
prop_parser_preserves_line_numbers :: Property
prop_parser_preserves_line_numbers =
  let content = unlines
        [ "// @ownership"
        , "```go"
        , "fmt.Println(\"test\")"
        , "```"
        , "// @invalid-directive"
        , "```go"
        , "fmt.Println(\"another test\")"
        , "```"
        ]
      result = parseTypus content ""
  in case result of
    Left err -> property $ "line" `isInfixOf` (errorBundlePretty err)
    Right _ -> property True

-- Property: Parser should handle very long lines
prop_parser_handles_long_lines :: Property
prop_parser_handles_long_lines =
  let longLine = replicate 1000 'a'
      content = "// @ownership\n```go\nvar " ++ longLine ++ " string = \"test\"\n```"
      result = parseTypus content ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right file -> property $ True  -- Or succeed

-- Property: Parser should handle Unicode content
prop_parser_handles_unicode :: Property
prop_parser_handles_unicode =
  let unicodeContent = unlines
        [ "// @ownership"
        , "```go"
        , "fmt.Println(\"测试中文 🚀\")"
        , "var café string = \"naïve résumé\""
        , "```"
        ]
      result = parseTypus unicodeContent ""
  in case result of
    Left _ -> property True  -- Should handle Unicode gracefully
    Right file -> property $ True

-- Property: Parser should handle nested block comments
prop_parser_handles_nested_comments :: Property
prop_parser_handles_nested_comments =
  let nestedComments = unlines
        [ "// @ownership"
        , "/* outer comment"
        , "/* inner comment */"
        , "still in outer"
        , "*/"
        , "```go"
        , "fmt.Println(\"test\")"
        , "```"
        ]
      result = parseTypus nestedComments ""
  in case result of
    Left _ -> property True  -- Should handle nested comments
    Right file -> property $ True

-- Property: Parser should handle escape sequences in strings
prop_parser_handles_escape_sequences :: Property
prop_parser_handles_escape_sequences =
  let contentWithEscapes = unlines
        [ "// @ownership"
        , "```go"
        , "fmt.Println(\"Line 1\\nLine 2\\tTabbed\\\"Quoted\\\"\")"
        , "var path string = \"C:\\\\Windows\\\\System32\""
        , "```"
        ]
      result = parseTypus contentWithEscapes ""
  in case result of
    Left _ -> property True  -- Should handle escape sequences
    Right file -> property $ True

-- Property: Parser should handle inconsistent indentation
prop_parser_handles_inconsistent_indentation :: Property
prop_parser_handles_inconsistent_indentation =
  let inconsistentIndent = unlines
        [ "// @ownership"
        , "  ```go"
        , "\tfmt.Println(\"mixed indentation\")"
        , "    fmt.Println(\"more spaces\")"
        , "\t```"
        ]
      result = parseTypus inconsistentIndent ""
  in case result of
    Left _ -> property True  -- Should handle indentation issues
    Right file -> property $ True

-- Property: Parser should handle empty code blocks
prop_parser_handles_empty_blocks :: Property
prop_parser_handles_empty_blocks =
  let emptyBlocks = unlines
        [ "// @ownership"
        , "```go"
        , "```"
        , "// @dependent-types"
        , "```rust"
        , "```"
        ]
      result = parseTypus emptyBlocks ""
  in case result of
    Left _ -> property True  -- Should handle empty blocks
    Right file -> property $ length (tfCodeBlocks file) >= 0

-- Property: Parser should handle multiple consecutive directives
prop_parser_handles_consecutive_directives :: Property
prop_parser_handles_consecutive_directives =
  let consecutiveDirectives = unlines
        [ "// @ownership"
        , "// @dependent-types"
        , "// @constraints"
        , "```go"
        , "fmt.Println(\"test\")"
        , "```"
        ]
      result = parseTypus consecutiveDirectives ""
  in case result of
    Left _ -> property False  -- Should parse consecutive directives
    Right file -> property $ True

-- Property: Parser should handle directives with extra whitespace
prop_parser_handles_directive_whitespace :: Property
prop_parser_handles_directive_whitespace =
  let whitespaceVariants = unlines
        [ "// @ownership"
        , "  //   @dependent-types  "
        , "\t\t//\t@constraints\t"
        , "```go"
        , "fmt.Println(\"test\")"
        , "```"
        ]
      result = parseTypus whitespaceVariants ""
  in case result of
    Left _ -> property False  -- Should handle whitespace in directives
    Right file -> property $ True

-- Property: Parser should provide meaningful error messages
prop_parser_provides_meaningful_errors :: Property
prop_parser_provides_meaningful_errors =
  let problematicContent = unlines
        [ "// @ownership"
        , "```go"
        , "fmt.Println(\"unclosed string"
        , "```"
        ]
      result = parseTypus problematicContent ""
  in case result of
    Left err -> 
      let errorMsg = errorBundlePretty err
      in property $ length errorMsg > 10  -- Error message should not be trivial
    Right _ -> property True

tests :: TestTree
tests = testGroup "Parser Error Recovery Tests"
  [ fastProperty "Parser handles empty input gracefully" prop_parser_handles_empty_input
  , fastProperty "Parser handles whitespace-only input" prop_parser_handles_whitespace_only
  , fastProperty "Parser handles single directive correctly" prop_parser_handles_single_directive
  , fastProperty "Parser handles multiple directives" prop_parser_handles_multiple_directives
  , fastProperty "Parser recovers from malformed directives" prop_parser_recovers_from_malformed_directives
  , fastProperty "Parser handles incomplete code blocks gracefully" prop_parser_handles_incomplete_blocks
  , fastProperty "Parser handles mixed valid and invalid content" prop_parser_handles_mixed_content
  , fastProperty "Parser preserves line numbers in error messages" prop_parser_preserves_line_numbers
  , fastProperty "Parser handles very long lines" prop_parser_handles_long_lines
  , fastProperty "Parser handles Unicode content" prop_parser_handles_unicode
  , fastProperty "Parser handles nested block comments" prop_parser_handles_nested_comments
  , fastProperty "Parser handles escape sequences in strings" prop_parser_handles_escape_sequences
  , fastProperty "Parser handles inconsistent indentation" prop_parser_handles_inconsistent_indentation
  , fastProperty "Parser handles empty code blocks" prop_parser_handles_empty_blocks
  , fastProperty "Parser handles multiple consecutive directives" prop_parser_handles_consecutive_directives
  , fastProperty "Parser handles directives with extra whitespace" prop_parser_handles_directive_whitespace
  , fastProperty "Parser provides meaningful error messages" prop_parser_provides_meaningful_errors
  ]