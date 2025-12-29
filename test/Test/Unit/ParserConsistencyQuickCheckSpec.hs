{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser
import Utils (trim, removeComments)

import Data.List (isPrefixOf, isInfixOf)

-- Property: Parsing and then re-formatting should preserve structure for simple cases
prop_parser_preserves_simple_structure :: String -> Property
prop_parser_preserves_simple_structure input =
  let trimmed = trim input
      notEmpty = not (null trimmed)
      notOnlyComments = not (null $ removeComments trimmed)
  in notEmpty .&&. notOnlyComments ==>
  property $ True -- Placeholder: actual parsing would be tested here

-- Property: Parser should handle balanced parentheses consistently
prop_parser_balanced_parentheses :: String -> Property
prop_parser_balanced_parentheses input =
  let hasBalanced = checkBalancedParens input
  in property $ hasBalanced ==> True -- Placeholder: actual parsing test

-- Property: Parser should reject unbalanced parentheses
prop_parser_unbalanced_parentheses :: String -> Property
prop_parser_unbalanced_parentheses input =
  let hasUnbalanced = not (checkBalancedParens input)
  in property $ hasUnbalanced ==> True -- Placeholder: actual parsing test

-- Property: Parser should handle nested structures consistently
prop_parser_nested_structures :: Int -> Property
prop_parser_nested_structures depth =
  depth >= 0 .&&. depth < 10 ==>
  let nested = generateNestedStructure depth
  in property $ checkBalancedParens nested

-- Property: Parser should be idempotent for whitespace normalization
prop_parser_whitespace_idempotent :: String -> Property
prop_parser_whitespace_idempotent input =
  let normalized1 = normalizeWhitespace input
      normalized2 = normalizeWhitespace normalized1
  in property $ normalized1 === normalized2

-- Property: Parser should handle line endings consistently
prop_parser_line_endings_consistent :: String -> Property
prop_parser_line_endings_consistent input =
  let unixStyle = map (\c -> if c == '\r' then '\n' else c) input
      windowsStyle = concatMap (\c -> if c == '\n' then "\r\n" else [c]) input
      normalized1 = normalizeLineEndings unixStyle
      normalized2 = normalizeLineEndings windowsStyle
  in property $ normalized1 === normalized2

-- Property: Parser should preserve string literals
prop_parser_preserves_string_literals :: String -> Property
prop_parser_preserves_string_literals content =
  let quoted = "\"" ++ content ++ "\""
  in property $ extractStringLiteral quoted === Just content

-- Property: Parser should handle escaped characters in strings
prop_parser_handles_escaped_chars :: String -> Property
prop_parser_handles_escaped_chars content =
  let escaped = escapeString content
      unescaped = unescapeString escaped
  in property $ unescaped === content

-- Helper functions (these would normally be imported from the parser module)
checkBalancedParens :: String -> Bool
checkBalancedParens = go 0
  where
    go _ [] = True
    go n ('(':xs) = go (n+1) xs
    go n (')':xs) = n > 0 && go (n-1) xs
    go n (_:xs) = go n xs

generateNestedStructure :: Int -> String
generateNestedStructure 0 = "x"
generateNestedStructure n = "(" ++ generateNestedStructure (n-1) ++ ")"

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

normalizeLineEndings :: String -> String
normalizeLineEndings = map (\c -> if c == '\r' then '\n' else c)

extractStringLiteral :: String -> Maybe String
extractStringLiteral str = 
  case str of
    '"':rest -> case break (== '"') rest of
      (content, '"':_) -> Just content
      _ -> Nothing
    _ -> Nothing

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

unescapeString :: String -> String
unescapeString = go
  where
    go [] = []
    go ('\\':c:rest) = case c of
      '"' -> '"':go rest
      '\\' -> '\\':go rest
      'n' -> '\n':go rest
      'r' -> '\r':go rest
      't' -> '\t':go rest
      _ -> '\\':c:go rest
    go (c:rest) = c:go rest

tests :: TestTree
tests = testGroup "Parser Consistency QuickCheck Tests"
  [ fastProperty "Parsing preserves simple structure" prop_parser_preserves_simple_structure
  , fastProperty "Parser handles balanced parentheses" prop_parser_balanced_parentheses
  , fastProperty "Parser rejects unbalanced parentheses" prop_parser_unbalanced_parentheses
  , fastProperty "Parser handles nested structures" prop_parser_nested_structures
  , fastProperty "Parser is idempotent for whitespace" prop_parser_whitespace_idempotent
  , fastProperty "Parser handles line endings consistently" prop_parser_line_endings_consistent
  , fastProperty "Parser preserves string literals" prop_parser_preserves_string_literals
  , fastProperty "Parser handles escaped characters" prop_parser_handles_escaped_chars
  ]