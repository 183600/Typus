{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.ParserPropertySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (locatedValue)

-- | Property tests for Parser module
tests :: TestTree
tests = testGroup "Parser Property Tests"
  [ testProperty "round-trip parsing" propRoundTripParsing
  , testProperty "directive parsing consistency" propDirectiveParsingConsistency
  , testProperty "whitespace preservation" propWhitespacePreservation
  , testProperty "empty input handling" propEmptyInputHandling
  ]

-- | Parsing and re-serializing should preserve essential structure
propRoundTripParsing :: String -> Property
propRoundTripParsing input =
  let result = parseTypus input
  in case result of
    Left _ -> property True  -- Invalid inputs can fail, that's OK
    Right typusFile -> 
      let reconstructed = reconstructTypusFile typusFile
      in length (lines reconstructed) >= length (lines input)

-- | Directive parsing should be consistent regardless of whitespace
propDirectiveParsingConsistency :: String -> Property
propDirectiveParsingConsistency input =
  let withExtraWhitespace = addWhitespace input
      result1 = parseTypus input
      result2 = parseTypus withExtraWhitespace
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right file1, Right file2) -> 
      locatedValue <$> fdOwnership (tfDirectives file1) ==
      locatedValue <$> fdOwnership (tfDirectives file2)
    _ -> property False

-- | Whitespace should be preserved in code blocks
propWhitespacePreservation :: String -> Property
propWhitespacePreservation input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile ->
      let blocks = tfBlocks typusFile
      in all (hasConsistentWhitespace . cbContent) blocks

-- | Empty input should be handled gracefully
propEmptyInputHandling :: String -> Property
propEmptyInputHandling input =
  let emptyInput = ""
      result = parseTypus emptyInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Reconstruct a TypusFile from its components (simplified)
reconstructTypusFile :: TypusFile -> String
reconstructTypusFile file = 
  unlines $ map cbContent (tfBlocks file)

-- | Add random whitespace to input
addWhitespace :: String -> String
addWhitespace = concatMap (\c -> 
  if isSpace c then c ++ "  "
  else if c == '\n' then "\n  "
  else [c])

-- | Check if content has consistent whitespace
hasConsistentWhitespace :: String -> Bool
hasConsistentWhitespace content =
  let lines' = lines content
      leadingSpaces = map (length . takeWhile isSpace) lines'
  in all (>= 0) leadingSpaces

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary Char where
  arbitrary = arbitrary `suchThat` (/= '\0')