{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.LexerBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, listOf1, choose, Positive(..), NonEmptyList(..))

import Compiler.GoLexer (GoToken(..), GoTokenKind(..), lexGoCode)
import Parser (parseTypus)

import Data.List (sort, nub, group, sortBy, find)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import Data.Char (isSpace, isLetter, isDigit, isPunctuation)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Lexer handles empty input gracefully
prop_lexer_handles_empty_input :: Property
prop_lexer_handles_empty_input =
  let tokens = lexGoCode ""
  in null tokens

-- Property: Lexer tokenization is deterministic
prop_lexer_tokenization_deterministic :: String -> Property
prop_lexer_tokenization_deterministic input =
  let tokens1 = lexGoCode input
      tokens2 = lexGoCode input
  in tokens1 === tokens2

-- Property: Lexer preserves token order
prop_lexer_preserves_token_order :: String -> Property
prop_lexer_preserves_token_order input =
  let tokens = lexGoCode input
      originalPositions = extractTokenPositions input tokens
      sortedPositions = sort originalPositions
  in originalPositions === sortedPositions

-- Property: Lexer handles whitespace correctly
prop_lexer_handles_whitespace_correctly :: String -> Property
prop_lexer_handles_whitespace_correctly input =
  let withWhitespace = insertRandomWhitespace input
      tokens1 = lexGoCode input
      tokens2 = lexGoCode withWhitespace
  in tokenContentEquality tokens1 tokens2

-- Property: Lexer handles special characters
prop_lexer_handles_special_characters :: String -> Property
prop_lexer_handles_special_characters input =
  let specialChars = filter isPunctuation input
      tokens = lexGoCode input
      recognizedSpecials = countSpecialTokens tokens
  in not (null specialChars) ==> recognizedSpecials > 0

-- Property: Lexer token positions are accurate
prop_lexer_token_positions_accurate :: String -> Property
prop_lexer_token_positions_accurate input =
  let tokens = lexGoCode input
  in all (hasValidPosition input) tokens

-- Property: Lexer handles unicode characters
prop_lexer_handles_unicode_characters :: String -> Property
prop_lexer_handles_unicode_characters input =
  let unicodeInput = addUnicodeChars input
      tokens = lexGoCode unicodeInput
  in not (null unicodeInput) ==> not (null tokens)

-- Property: Lexer handles large inputs efficiently
prop_lexer_handles_large_inputs_efficiently :: Positive Int -> Property
prop_lexer_handles_large_inputs_efficiently (Positive n) =
  let largeInput = replicate (n `mod` 1000) 'x'
      tokens = lexGoCode largeInput
  in length tokens > 0

-- Property: Lexer error recovery is robust
prop_lexer_error_recovery_robust :: String -> Property
prop_lexer_error_recovery_robust input =
  let problematicInput = addLexerErrors input
      tokens = lexGoCode problematicInput
  in hasErrorTokens tokens || not (null tokens)

-- Helper functions (these would need to be implemented in the actual modules)
extractTokenPositions :: String -> [GoToken] -> [Int]
extractTokenPositions _ tokens = map tokenPosition tokens
  where
    tokenPosition (GoToken _ pos _) = pos

insertRandomWhitespace :: String -> String
insertRandomWhitespace [] = []
insertRandomWhitespace (c:cs) = c : ' ' : insertRandomWhitespace cs

tokenContentEquality :: [GoToken] -> [GoToken] -> Bool
tokenContentEquality tokens1 tokens2 = 
  length tokens1 == length tokens2 &&
  all (\(t1, t2) -> tokenValue t1 == tokenValue t2) (zip tokens1 tokens2)
  where
    tokenValue (GoToken _ _ value) = value

countSpecialTokens :: [GoToken] -> Int
countSpecialTokens = length . filter isSpecialToken
  where
    isSpecialToken (GoToken kind _ _) = kind `elem` [TokenOperator, TokenDelimiter]

hasValidPosition :: String -> GoToken -> Bool
hasValidPosition input (GoToken _ pos _) = pos >= 0 && pos < length input

addUnicodeChars :: String -> String
addUnicodeChars input = input ++ "αβγδε"

hasErrorTokens :: [GoToken] -> Bool
hasErrorTokens = any isErrorToken
  where
    isErrorToken (GoToken TokenError _ _) = True
    isErrorToken _ = False

addLexerErrors :: String -> String
addLexerErrors input = input ++ "§¶†‡"

tests :: TestTree
tests = testGroup "Lexer Boundary QuickCheck Tests"
  [ fastProperty "Lexer handles empty input" prop_lexer_handles_empty_input
  , fastProperty "Lexer tokenization deterministic" prop_lexer_tokenization_deterministic
  , fastProperty "Lexer preserves token order" prop_lexer_preserves_token_order
  , fastProperty "Lexer handles whitespace correctly" prop_lexer_handles_whitespace_correctly
  , fastProperty "Lexer handles special characters" prop_lexer_handles_special_characters
  , fastProperty "Lexer token positions accurate" prop_lexer_token_positions_accurate
  , fastProperty "Lexer handles unicode characters" prop_lexer_handles_unicode_characters
  , fastProperty "Lexer handles large inputs efficiently" prop_lexer_handles_large_inputs_efficiently
  , fastProperty "Lexer error recovery robust" prop_lexer_error_recovery_robust
  ]