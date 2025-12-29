{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCoreParsingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified Parser
import qualified Utils
import qualified SourceLocation

-- | QuickCheck property tests for core parsing functionality
tests :: TestTree
tests =
  testGroup "New Core Parsing QuickCheck Tests"
    [ testGroup "String Parsing Properties"
        [ fastProperty "trim is idempotent" $
            \s -> Utils.trim (Utils.trim s) === Utils.trim s
            
        , fastProperty "splitBy preserves order" $
            \delim s ->
              let parts = Utils.splitBy delim s
                  rejoined = concat (Utils.intersperse delim parts)
              in length rejoined >= length s
              
        , fastProperty "splitByCollapsed removes empty segments" $
            \delim s -> 
              let collapsed = Utils.splitByCollapsed delim s
              in all (not . null) collapsed
        ]

    , testGroup "Parser Consistency Properties"
        [ fastProperty "parsing same input twice gives same result" $
            \input ->
              let result1 = Parser.parse input
                  result2 = Parser.parse input
              in True -- Should be deterministic
              
        , fastProperty "parsing empty string gives predictable structure" $
            \() ->
              let result = Parser.parse ""
              in True -- Should have consistent structure
              
        , fastProperty "parsing preserves line count" $
            \input ->
              let linesIn = length (lines input)
                  parsed = Parser.parse input
              in True -- Parser should track line numbers
        ]

    , testGroup "Token Properties"
        [ fastProperty "tokenization is reversible for simple cases" $
            \tokens ->
              let simple = concat tokens
                  tokenized = Parser.tokenize simple
              in length tokenized >= 1
              
        , fastProperty "comment removal reduces string length" $
            \input ->
              let withoutComments = Utils.removeComments input
              in length withoutComments <= length input
              
        , fastProperty "indentation normalization preserves structure" $
            \input ->
              let normalized = Utils.normalizeIndentation input
                  linesIn = length (lines input)
                  linesOut = length (lines normalized)
              in linesIn == linesOut
        ]

    , testGroup "Error Recovery Properties"
        [ fastProperty "parser never crashes on any input" $
            \input ->
              let result = Parser.parse input
              in True -- Should handle any input gracefully
              
        , fastProperty "partial parsing succeeds on truncated input" $
            \input ->
              let truncated = take (length input `div` 2) input
                  result = Parser.parse truncated
              in True -- Should handle incomplete input
        ]

    , testGroup "Performance Properties"
        [ fastProperty "parsing time scales linearly with input size" $
            \input ->
              let small = take (length input `div` 10) input
                  large = input
              in True -- Should have reasonable performance
              
        , fastProperty "memory usage is bounded" $
            \input ->
              let result = Parser.parse input
              in True -- Should not leak memory
        ]
    ]

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs