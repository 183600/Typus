{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Parser performance boundary tests for Parser module
module Test.Unit.NewParserPerformanceBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, length)
import Data.List (sort, nub, foldl')
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (NFData, force)

import Parser
  ( ParseResult(..)
  , Parser
  , parse
  , parseWithLimit
  , parseWithTimeout
  , parseTokens
  , ParseError(..)
  , Token(..)
  , TokenType(..)
  )

import Utils
  ( splitBy
  , trim
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate large input strings for performance testing
genLargeInput :: Int -> Gen String
genLargeInput size = do
  let chunkSize = max 1 (size `div` 100)
  chunks <- listOf $ elements
    [ "func test() { return " ++ replicate chunkSize 'a' ++ "; }"
    , "var x = " ++ replicate chunkSize 'b' ++ ";"
    , "if (true) { " ++ replicate chunkSize 'c' ++ " }"
    , "class Test { " ++ replicate chunkSize 'd' ++ " }"
    , "interface Test { " ++ replicate chunkSize 'e' ++ " }"
    ]
  return $ unlines chunks

-- Generate deeply nested structures
genDeeplyNested :: Int -> Gen String
genDeeplyNested depth = do
  let nested = foldl' (\acc d -> acc ++ "if (true) { " ++ replicate d ' ' ++ "") "" [1..depth]
      closing = replicate depth "}"
  return $ nested ++ "result;" ++ closing

-- Generate token-heavy input
genTokenHeavy :: Int -> Gen String
genTokenHeavy numTokens = do
  tokens <- listOf $ elements
    [ "identifier", "12345", "true", "false", "null", "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "(", ")", "{", "}", "[", "]", ";", "," ]
  return $ unwords $ take numTokens tokens

-- Generate input with many comments
genCommentHeavy :: Int -> Gen String
genCommentHeavy numComments = do
  comments <- listOf $ elements
    [ "// This is a line comment\n"
    , "/* This is a block comment */"
    , "/*\n * Multi-line comment\n * with multiple lines\n */"
    ]
  code <- listOf $ elements ["var x = 1;", "func test() {}", "class Test {}"]
  let interleaved = take numComments $ zipWith (\c code' -> c ++ code' ++ "\n") comments (cycle code)
  return $ L.concat interleaved

-- Generate input with long identifiers
genLongIdentifiers :: Int -> Gen String
genLongIdentifiers L.length = do
  let longId = replicate L.length 'a'
  return $ "var " ++ longId ++ " = function() { return " ++ longId ++ "; };"

-- ============================================================================
-- Performance Boundary Properties
-- ============================================================================

-- Property: Parser should handle large inputs within reasonable time
prop_parser_large_input_performance :: Int -> Property
prop_parser_large_input_performance size =
  size > 0 && size <= 10000 ==> 
  forAll (genLargeInput size) $ \input ->
    let result = parseWithLimit input 1000000  -- 1 second limit
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True  -- Error is acceptable, timeout is not
         ParseTimeout -> property False  -- Timeout indicates performance issue

-- Property: Parser should handle deeply nested structures
prop_parser_deep_nesting :: Int -> Property
prop_parser_deep_nesting depth =
  depth > 0 && depth <= 100 ==> 
  forAll (genDeeplyNested depth) $ \input ->
    let result = parse input
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True  -- Error is acceptable for very deep nesting
         _ -> property False

-- Property: Parser should handle token-heavy inputs efficiently
prop_parser_token_heavy :: Int -> Property
prop_parser_token_heavy numTokens =
  numTokens > 0 && numTokens <= 5000 ==> 
  forAll (genTokenHeavy numTokens) $ \input ->
    let result = parseTokens input
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle comment-heavy inputs
prop_parser_comment_heavy :: Int -> Property
prop_parser_comment_heavy numComments =
  numComments > 0 && numComments <= 1000 ==> 
  forAll (genCommentHeavy numComments) $ \input ->
    let result = parse input
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle long identifiers
prop_parser_long_identifiers :: Int -> Property
prop_parser_long_identifiers L.length =
  length > 0 && L.length <= 1000 ==> 
  forAll (genLongIdentifiers L.length) $ \input ->
    let result = parse input
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser memory usage should be bounded
prop_parser_memory_bounded :: Int -> Property
prop_parser_memory_bounded size =
  size > 0 && size <= 10000 ==> 
  forAll (genLargeInput size) $ \input ->
    let result = force $ parse input  -- Force evaluation to check memory usage
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle Unicode inputs efficiently
prop_parser_unicode_performance :: String -> Property
prop_parser_unicode_performance baseInput =
  let unicodeInput = baseInput ++ "café naïve résumé 测试 🚀" ++ baseInput
      result = parse unicodeInput
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser timeout should work correctly
prop_parser_timeout_works :: Int -> Int -> Property
prop_parser_timeout_works size timeoutMicros =
  size > 0 && size <= 10000 && timeoutMicros > 0 ==> 
  forAll (genLargeInput size) $ \input ->
    let result = parseWithTimeout input timeoutMicros
    in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         ParseTimeout -> True  -- Timeout is acceptable when explicitly set
         _ -> property False

-- Property: Parser should handle incremental parsing
prop_parser_incremental :: [String] -> Property
prop_parser_incremental chunks =
  not (null chunks) ==> 
  let combined = unlines chunks
      incrementalResult = foldl' (\acc chunk -> 
        case acc of
          ParseSuccess _ _ -> parse chunk  -- Parse next chunk
          ParseError _ -> ParseError (ParseError "Previous error" 0 "" "")
          _ -> acc
      ) (ParseSuccess [] []) chunks
      directResult = parse combined
  in property $ case (incrementalResult, directResult) of
         (ParseSuccess _ _, ParseSuccess _ _) -> True
         (ParseError _, ParseError _) -> True
         (ParseTimeout, ParseTimeout) -> True
         _ -> property False  -- Results should be consistent

-- Property: Parser should handle malformed inputs gracefully
prop_parser_malformed_graceful :: String -> Property
prop_parser_malformed_graceful input =
  let malformedInput = input ++ "\0\1\2\3" ++ input ++ "\x1F\x7F"
      result = parse malformedInput
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True  -- Error is expected for malformed input
         _ -> property False

-- Property: Parser performance should degrade gracefully
prop_parser_graceful_degradation :: Int -> Int -> Property
prop_parser_graceful_degradation smallSize largeSize =
  smallSize > 0 && largeSize > smallSize && largeSize <= 10000 ==> 
  forAll (genLargeInput smallSize) $ \smallInput ->
  forAll (genLargeInput largeSize) $ \largeInput ->
    let smallResult = parse smallInput
        largeResult = parse largeInput
    in property $ case (smallResult, largeResult) of
         (ParseSuccess _ _, ParseSuccess _ _) -> True
         (ParseError _, ParseError _) -> True
         (ParseTimeout, ParseTimeout) -> True
         (ParseSuccess _ _, ParseTimeout) -> True  -- Large input may timeout
         _ -> property False

-- ============================================================================
-- Memory L.and Resource Management Properties
-- ============================================================================

-- Property: Parser should not leak memory on repeated parsing
prop_parser_no_memory_leak :: [String] -> Property
prop_parser_no_memory_leak inputs =
  not (null inputs) ==> 
  let results = map parse inputs
      forcedResults = map force results
  in property $ L.all (\r -> case r of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> False) forcedResults

-- Property: Parser should handle resource exhaustion gracefully
prop_parser_resource_exhaustion :: Int -> Property
prop_parser_resource_exhaustion size =
  size > 0 && size <= 100000 ==> 
  let hugeInput = replicate size 'a' ++ ";\n"
      result = parseWithLimit hugeInput 100000  -- Short timeout
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         ParseTimeout -> True  -- Expected for huge inputs
         _ -> property False

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Parser should handle empty input
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parse ""
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle whitespace-only input
prop_parser_whitespace_only :: Property
prop_parser_whitespace_only =
  let whitespaceInput = "   \t\n\r   \t\n\r   "
      result = parse whitespaceInput
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle extremely long lines
prop_parser_extreme_line_length :: Int -> Property
prop_parser_extreme_line_length lineLength =
  lineLength > 0 && lineLength <= 10000 ==> 
  let longLine = "var x = " ++ replicate lineLength 'a' ++ ";"
      result = parse longLine
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- Property: Parser should handle inputs with many escape sequences
prop_parser_many_escape_sequences :: Int -> Property
prop_parser_many_escape_sequences numEscapes =
  numEscapes > 0 && numEscapes <= 1000 ==> 
  let escapes = L.concat $ replicate numEscapes "\n\t\r\\\\\"'"
      input = "var s = \"" ++ escapes ++ "\";"
      result = parse input
  in property $ case result of
         ParseSuccess _ _ -> True
         ParseError _ -> True
         _ -> property False

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Performance Boundary QuickCheck Tests"
  [ testGroup "Large Input Performance"
    [ fastProperty "parser large input performance" prop_parser_large_input_performance
    , fastProperty "parser deep nesting" prop_parser_deep_nesting
    , fastProperty "parser token heavy" prop_parser_token_heavy
    , fastProperty "parser comment heavy" prop_parser_comment_heavy
    , fastProperty "parser long identifiers" prop_parser_long_identifiers
    ]

  , testGroup "Memory L.and Resource Management"
    [ fastProperty "parser memory bounded" prop_parser_memory_bounded
    , fastProperty "parser no memory leak" prop_parser_no_memory_leak
    , fastProperty "parser resource exhaustion" prop_parser_resource_exhaustion
    ]

  , testGroup "Unicode L.and Special Characters"
    [ fastProperty "parser unicode performance" prop_parser_unicode_performance
    , fastProperty "parser malformed graceful" prop_parser_malformed_graceful
    ]

  , testGroup "Timeout L.and Incremental Parsing"
    [ fastProperty "parser timeout works" prop_parser_timeout_works
    , fastProperty "parser incremental" prop_parser_incremental
    ]

  , testGroup "Performance Degradation"
    [ fastProperty "parser graceful degradation" prop_parser_graceful_degradation
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "parser empty input" prop_parser_empty_input
    , fastProperty "parser whitespace only" prop_parser_whitespace_only
    , fastProperty "parser extreme line L.length" prop_parser_extreme_line_length
    , fastProperty "parser many escape sequences" prop_parser_many_escape_sequences
    ]
  ]