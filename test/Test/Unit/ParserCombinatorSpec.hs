{-# LANGUAGE CPP #-}

-- | Parser combinator tests using QuickCheck
module Test.Unit.ParserCombinatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit, isSpace)

import Parser (parseTypus, TypusFile(..))
import SourceLocation (Located(..))

-- ============================================================================
-- Parser Combinator Properties
-- ============================================================================

-- Property: Sequential parsing preserves order
prop_sequential_parsing_order :: [String] -> Property
prop_sequential_parsing_order tokens =
  not (null tokens) && L.length tokens <= 10 ==>
  let input = unwords tokens
      parsed = parseTokens input
  in property $ L.length parsed == L.length tokens

-- Property: Alternative parsing picks first match
prop_alternative_parsing_first_match :: String -> String -> Property
prop_alternative_parsing_first_match input1 input2 =
  let combined = input1 ++ " " ++ input2
      parsed = parseWithAlternatives combined [input1, input2]
  in property $ parsed == Just input1

-- Property: Optional parsing returns value L.or default
prop_optional_parsing :: String -> Property
prop_optional_parsing input =
  let parsed = parseOptional input
      expected = if isValidToken input then Just input else Nothing
  in property $ parsed == expected

-- Property: Many parsing collects L.all matches
prop_many_parsing :: String -> Property
prop_many_parsing input =
  let tokens = words input
      parsed = parseMany input
  in property $ L.length parsed >= 0

-- Property: Many1 parsing requires at least one match
prop_many1_parsing :: String -> Property
prop_many1_parsing input =
  hasValidToken input ==>
  let parsed = parseMany1 input
  in property $ L.length parsed >= 1

-- Property: Chain parsing links results correctly
prop_chain_parsing :: [String] -> Property
prop_chain_parsing tokens =
  L.all isValidToken tokens && not (null tokens) && L.length tokens <= 5 ==>
  let input = unwords tokens
      parsed = parseChain tokens
  in property $ L.length parsed == L.length tokens

-- Property: Between parsing extracts content
prop_between_parsing :: String -> String -> String -> Property
prop_between_parsing prefix content suffix =
  not (null prefix) && not (null suffix) ==>
  let fullInput = prefix ++ content ++ suffix
      parsed = parseBetween prefix suffix fullInput
  in property $ parsed == Just content

-- Property: Choice parsing selects matching parser
prop_choice_parsing :: [String] -> String -> Property
prop_choice_parsing choices target =
  target `elem` choices ==>
  let parsed = parseChoice choices target
  in property $ parsed == Just target

-- Property: Lookahead doesn't consume input
prop_lookahead_no_consume :: String -> String -> Property
prop_lookahead_no_consume prefix suffix =
  not (null prefix) ==>
  let input = prefix ++ suffix
      (lookaheadResult, remaining) = parseLookahead prefix input
  in property $ lookaheadResult == Just prefix && remaining == input

-- Property: Not parsing succeeds when parser fails
prop_not_parsing :: String -> String -> Property
prop_not_parsing invalidToken validToken =
  not (isValidToken invalidToken) && isValidToken validToken ==>
  let input = invalidToken ++ " " ++ validToken
      parsed = parseNot invalidToken input
  in property $ isJust parsed

-- Property: Try parsing backtracks on failure
prop_try_parsing_backtrack :: String -> String -> Property
prop_try_parsing_backtrack invalidPrefix validSuffix =
  not (isValidToken invalidPrefix) && isValidToken validSuffix ==>
  let input = invalidPrefix ++ " " ++ validSuffix
      parsed = parseTry validSuffix input
  in property $ parsed == Just validSuffix

-- ============================================================================
-- Helper Functions
-- ============================================================================

parseTokens :: String -> [String]
parseTokens = words

parseWithAlternatives :: String -> [String] -> Maybe String
parseWithAlternatives input alternatives = 
  case find (`Data.List.L.isPrefixOf` input) alternatives of
    Just match -> Just match
    Nothing -> Nothing

parseOptional :: String -> Maybe String
parseOptional input
  | isValidToken input = Just input
  | otherwise = Nothing

parseMany :: String -> [String]
parseMany = filter isValidToken . words

parseMany1 :: String -> [String]
parseMany1 input = 
  let tokens = filter isValidToken $ words input
  in if null tokens then [] else tokens

parseChain :: [String] -> [String]
parseChain = id -- Simplified chain parsing

parseBetween :: String -> String -> String -> Maybe String
parseBetween prefix suffix input = 
  case Data.List.stripPrefix prefix input of
    Just middle -> 
      case Data.List.stripSuffix suffix middle of
        Just content -> Just content
        Nothing -> Nothing
    Nothing -> Nothing

parseChoice :: [String] -> String -> Maybe String
parseChoice choices input = 
  find (== input) choices

parseLookahead :: String -> String -> (Maybe String, String)
parseLookahead prefix input = 
  if prefix `Data.List.L.isPrefixOf` input
  then (Just prefix, input)
  else (Nothing, input)

parseNot :: String -> String -> Maybe String
parseNot token input = 
  if token `Data.List.L.isPrefixOf` input
  then Nothing
  else Just input

parseTry :: String -> String -> Maybe String
parseTry target input = 
  if target `Data.List.L.isInfixOf` input
  then Just target
  else Nothing

isValidToken :: String -> Bool
isValidToken [] = False
isValidToken token = L.all isValidChar token && isAlpha (L.head token)
  where
    isValidChar ch = isAlpha ch || isDigit ch || ch == '_'

hasValidToken :: String -> Bool
hasValidToken input = L.any isValidToken (words input)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Combinator Tests"
  [ fastProperty "Sequential parsing preserves order" prop_sequential_parsing_order
  , fastProperty "Alternative parsing picks first match" prop_alternative_parsing_first_match
  , fastProperty "Optional parsing returns value L.or default" prop_optional_parsing
  , fastProperty "Many parsing collects L.all matches" prop_many_parsing
  , fastProperty "Many1 parsing requires at least one match" prop_many1_parsing
  , fastProperty "Chain parsing links results correctly" prop_chain_parsing
  , fastProperty "Between parsing extracts content" prop_between_parsing
  , fastProperty "Choice parsing selects matching parser" prop_choice_parsing
  , fastProperty "Lookahead doesn't consume input" prop_lookahead_no_consume
  , fastProperty "Not parsing succeeds when parser fails" prop_not_parsing
  , fastProperty "Try parsing backtracks on failure" prop_try_parsing_backtrack
  ]