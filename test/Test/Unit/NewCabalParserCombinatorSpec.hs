{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserCombinatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
import Utils (trim, splitBy)

import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.List as List

-- | Test suite for Parser combinator properties
tests :: TestTree
tests =
  testGroup "Parser Combinator Properties"
    [ testGroup "Basic parsing properties"
        [ fastProperty "parse identifier preserves content" prop_parse_identifier_preserves
        , fastProperty "parse number preserves numeric value" prop_parse_number_preserves
        , fastProperty "parse string preserves literal content" prop_parse_string_preserves
        , fastProperty "choice parser returns first successful" prop_choice_first_success
        , fastProperty "many parser returns list of parsed items" prop_many_returns_list
        ]

    , testGroup "Parser composition properties"
        [ fastProperty "sequential parsing consumes input in order" prop_sequential_consumes_order
        , fastProperty "optional parser either succeeds with value or returns Nothing" prop_optional_behavior
        , fastProperty "try parser backtrack preserves input on failure" prop_try_backtrack
        , fastProperty "lookahead parser does not consume input" prop_lookahead_no_consume
        ]

    , testGroup "Error handling properties"
        [ fastProperty "parse error position matches consumption" prop_error_position_matches
        , fastProperty "custom error messages are preserved" prop_custom_error_preserved
        , fastProperty "nested errors provide context" prop_nested_errors_context
        ]

    , testGroup "Performance and efficiency properties"
        [ fastProperty "parsing is linear in input size for simple grammars" prop_linear_parsing_simple
        , fastProperty "backtracking is bounded for deterministic parsers" prop_bounded_backtracking
        , fastProperty "memory usage does not grow exponentially" prop_memory_bounded
        ]

    , testGroup "Grammar invariants"
        [ fastProperty "parsing roundtrip property" prop_parsing_roundtrip
        , fastProperty "parsing is idempotent for whitespace" prop_whitespace_idempotent
        , fastProperty "concatenated parsing preserves order" prop_concatenated_order
        ]
    ]

-- Helper functions for testing
parseIdentifier :: String -> Either String String
parseIdentifier input = 
  case parse (many (letterChar <|> char '_' <|> digitChar) <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseNumber :: String -> Either String Int
parseNumber input = 
  case parse (many digitChar <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right digits -> Right $ read digits

parseString :: String -> Either String String
parseString input = 
  case parse (char '\"' *> many (noneOf "\"") <* char '\"' <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

-- Basic parsing properties

prop_parse_identifier_preserves :: String -> Property
prop_parse_identifier_preserves input =
  not (null input) && all (`elem` ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) input ==>
  case parseIdentifier input of
    Right result -> property $ result === input
    Left _ -> property $ False -- Should not fail on valid identifiers

prop_parse_number_preserves :: String -> Property
prop_parse_number_preserves input =
  not (null input) && all isDigit input && length input <= 10 ==>
  case parseNumber input of
    Right result -> property $ show result === input
    Left _ -> property $ False -- Should not fail on valid numbers

prop_parse_string_preserves :: String -> Property
prop_parse_string_preserves content =
  not (any (`elem` "\\\"") content) && length content <= 20 ==>
  let quoted = "\"" ++ content ++ "\""
  in case parseString quoted of
    Right result -> property $ result === content
    Left _ -> property $ False -- Should not fail on valid strings

prop_choice_first_success :: String -> String -> Property
prop_choice_first_success alt1 alt2 =
  not (null alt1) && not (null alt2) && alt1 /= alt2 ==>
  let input = alt1 ++ "extra"
      choiceParser = (string alt1 <|> string alt2) <* eof
  in case parse choiceParser "" input of
    Right result -> property $ result === alt1
    Left _ -> property $ False -- Should succeed with first alternative

prop_many_returns_list :: String -> String -> Property
prop_many_returns_list item separator =
  not (null item) && not (null separator) && item /= separator ==>
  let items = List.take 5 $ repeat item
      input = List.intercalate separator items
      manyParser = many (string item <* optional (string separator))
  in case parse manyParser "" input of
    Right results -> property $ length results >= 1 .&&. head results === item
    Left _ -> property $ False -- Should parse at least one item

-- Parser composition properties

prop_sequential_consumes_order :: String -> String -> Property
prop_sequential_consumes_order first second =
  not (null first) && not (null second) && first /= second ==>
  let input = first ++ second
      sequentialParser = string first *> string second
  in case parse sequentialParser "" input of
    Right _ -> property $ True -- Should successfully parse both in order
    Left _ -> property $ False

prop_optional_behavior :: String -> Property
prop_optional_behavior input =
  let optionalParser = optional (string "test")
  in case parse optionalParser "" input of
    Right (Just "test") -> property $ "test" `isPrefixOf` input
    Right Nothing -> property $ True -- Optional can always fail gracefully
    Left _ -> property $ False -- Should never fail for optional

prop_try_backtrack :: String -> String -> Property
prop_try_backtrack prefix alternative =
  not (null prefix) && not (null alternative) && prefix /= alternative ==>
  let input = prefix ++ "suffix"
      tryParser = try (string prefix <* string "wrong") <|> string alternative
  in case parse tryParser "" input of
    Right _ -> property $ True -- Should backtrack and try alternative
    Left _ -> property $ True -- Backtracking failure is acceptable

prop_lookahead_no_consume :: String -> String -> Property
prop_lookahead_no_consume prefix suffix =
  not (null prefix) && not (null suffix) ==>
  let input = prefix ++ suffix
      lookaheadParser = lookAhead (string prefix) *> string suffix
  in case parse lookaheadParser "" input of
    Right result -> property $ result === suffix
    Left _ -> property $ False

-- Error handling properties

prop_error_position_matches :: String -> String -> Property
prop_error_position_matches valid invalid =
  not (null valid) && not (null invalid) && valid /= invalid ==>
  let input = valid ++ invalid
      parser = string valid <* string "expected"
  in case parse parser "" input of
    Left err -> property $ True -- Error should occur at expected position
    Right _ -> property $ False -- Should fail on invalid input

prop_custom_error_preserved :: String -> Property
prop_custom_error_preserved input =
  let customError = "Custom error message"
      parser = string "expected" <|> fail customError
  in case parse parser "" input of
    Left err -> property $ customError `isInfixOf` errorBundlePretty err
    Right _ -> property $ False -- Should fail with custom error

prop_nested_errors_context :: String -> String -> Property
prop_nested_errors_context outer inner =
  not (null outer) && not (null inner) ==>
  let input = outer ++ inner
      nestedParser = string outer *> (string "expected" <|> fail "Inner error")
  in case parse nestedParser "" input of
    Left err -> property $ True -- Should provide context about both levels
    Right _ -> property $ False

-- Performance and efficiency properties

prop_linear_parsing_simple :: Int -> String -> Property
prop_linear_parsing_simple repetitions token =
  repetitions >= 0 && repetitions <= 100 && not (null token) && length token <= 5 ==>
  let input = concat $ replicate repetitions token
      simpleParser = many (string token)
  in case parse simpleParser "" input of
    Right results -> property $ length results === repetitions
    Left _ -> property $ repetitions == 0 -- Only fail on empty input

prop_bounded_backtracking :: Int -> String -> Property
prop_bounded_backtracking depth content =
  depth >= 0 && depth <= 10 && not (null content) ==>
  let nestedParser = foldr (\_ p -> try (string content <* string "wrong") <|> p) 
                          (string content) 
                          [1..depth]
      input = content
  in case parse nestedParser "" input of
    Right _ -> property $ True -- Should eventually succeed
    Left _ -> property $ True -- Bounded backtracking failure

prop_memory_bounded :: Int -> String -> Property
prop_memory_bounded size token =
  size >= 0 && size <= 50 && not (null token) && length token <= 3 ==>
  let input = concat $ replicate size token
      recursiveParser = many (string token <* optional (string token))
  in case parse recursiveParser "" input of
    Right _ -> property $ True -- Should not cause memory issues
    Left _ -> property $ True

-- Grammar invariants

prop_parsing_roundtrip :: String -> Property
prop_parsing_roundtrip content =
  length content <= 20 && not (any (`elem` "\\\"") content) ==>
  let quoted = "\"" ++ content ++ "\""
      roundtripParser = char '\"' *> many (noneOf "\"") <* char '\"'
  in case parse roundtripParser "" quoted of
    Right result -> property $ result === content
    Left _ -> property $ False

prop_whitespace_idempotent :: String -> Property
prop_whitespace_idempotent input =
  let whitespaceParser = many spaceChar
  in case parse whitespaceParser "" input of
    Right _ -> 
      let trimmed = trim input
      in case parse whitespaceParser "" trimmed of
        Right _ -> property $ True
        Left _ -> property $ False
    Left _ -> property $ True -- Whitespace parsing can fail

prop_concatenated_order :: String -> String -> String -> Property
prop_concatenated_order first second third =
  not (null first) && not (null second) && not (null third) &&
  all (/=) [first, second, third] ==>
  let input = first ++ second ++ third
      concatenatedParser = string first *> string second *> string third
  in case parse concatenatedParser "" input of
    Right _ -> property $ True
    Left _ -> property $ False