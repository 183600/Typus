{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, vector)
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub)
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
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Arbitrary Instances for Parser Types
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate valid directive values (on/off)
genDirectiveValue :: Gen String
genDirectiveValue = elements ["on", "off", "true", "false"]

-- Generate file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  keys <- listOf genDirectiveKey
  values <- listOf genDirectiveValue
  let pairs = zipWith (\k v -> k ++ ": " ++ v) keys values
      joined = Data.List.intercalate ", " pairs
  return $ "{//! " ++ joined ++ "}"

-- Generate simple code content without directives
genCodeContent :: Gen String
genCodeContent = do
  lines' <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t(){}[];,+"
  return $ unlines lines'

-- Generate build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = do
  tag <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ "//go:build " ++ tag

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Empty input parses to file with no blocks
prop_empty_input_parses :: Property
prop_empty_input_parses =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ null (tfBlocks file) .&&.
                        tfDirectives file === defaultFileDirectives .&&.
                        null (tfBuildTags file)

-- Property: Simple code without directives parses
prop_simple_code_parses :: String -> Property
prop_simple_code_parses content =
  not ("{//!" `isInfixOf` content) && not ("//!" `isPrefixOf` content) ==>
  let result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file)) ==> 
                              all (\block -> cbDirectives block === defaultBlockDirectives) (tfBlocks file)

-- Property: File directives are parsed correctly
prop_file_directives_parsed :: String -> String -> String -> Property
prop_file_directives_parsed ownershipVal dependentTypesVal constraintsVal =
  ownershipVal `elem` ["on", "off"] && dependentTypesVal `elem` ["on", "off"] && constraintsVal `elem` ["on", "off"] ==>
  let input = "//! ownership: " ++ ownershipVal ++ "\n" ++
              "//! dependent_types: " ++ dependentTypesVal ++ "\n" ++
              "//! constraints: " ++ constraintsVal ++ "\n" ++
              "some code here"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ case (fdOwnership (tfDirectives file), 
                                   fdDependentTypes (tfDirectives file), 
                                   fdConstraints (tfDirectives file)) of
                              (Just ownership, Just dependentTypes, Just constraints) ->
                                locatedValue ownership === (ownershipVal == "on" || ownershipVal == "true") .&&.
                                locatedValue dependentTypes === (dependentTypesVal == "on" || dependentTypesVal == "true") .&&.
                                locatedValue constraints === (constraintsVal == "on" || constraintsVal == "true")
                              _ -> property False

-- Property: Block directives are parsed correctly
prop_block_directives_parsed :: String -> String -> String -> Property
prop_block_directives_parsed ownershipVal dependentTypesVal constraintsVal =
  ownershipVal `elem` ["on", "off"] && dependentTypesVal `elem` ["on", "off"] && constraintsVal `elem` ["on", "off"] ==>
  let input = "{//! ownership: " ++ ownershipVal ++ ", dependent_types: " ++ dependentTypesVal ++ ", constraints: " ++ constraintsVal ++ "}\n" ++
              "code block content"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ case tfBlocks file of
                              [block] -> case (bdOwnership (cbDirectives block),
                                              bdDependentTypes (cbDirectives block),
                                              bdConstraints (cbDirectives block)) of
                                (Just ownership, Just dependentTypes, Just constraints) ->
                                  locatedValue ownership === (ownershipVal == "on" || ownershipVal == "true") .&&.
                                  locatedValue dependentTypes === (dependentTypesVal == "on" || dependentTypesVal == "true") .&&.
                                  locatedValue constraints === (constraintsVal == "on" || constraintsVal == "true")
                                _ -> property False
                              _ -> property False

-- Property: Mixed file and block directives
prop_mixed_directives_parsed :: String -> String -> Property
prop_mixed_directives_parsed fileOwnership blockOwnership =
  fileOwnership `elem` ["on", "off"] && blockOwnership `elem` ["on", "off"] ==>
  let input = "//! ownership: " ++ fileOwnership ++ "\n" ++
              "regular code\n" ++
              "{//! ownership: " ++ blockOwnership ++ "}\n" ++
              "block code"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ case (fdOwnership (tfDirectives file), tfBlocks file) of
                              (Just fileDir, [block]) -> case bdOwnership (cbDirectives block) of
                                Just blockDir -> property $ locatedValue fileDir === (fileOwnership == "on" || fileOwnership == "true") .&&.
                                                              locatedValue blockDir === (blockOwnership == "on" || blockOwnership == "true")
                                _ -> property False
                              _ -> property False

-- Property: Build tags are preserved
prop_build_tags_preserved :: [String] -> Property
prop_build_tags_preserved tags =
  all (not . null) tags && all (not . any isSpace) tags ==>
  let tagLines = map (\tag -> "//go:build " ++ tag) tags
      input = unlines tagLines ++ "\nsome code"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ length (tfBuildTags file) === length tags .&&.
                        all (`elem` map locatedValue (tfBuildTags file)) tags

-- Property: Multiple code blocks are parsed
prop_multiple_blocks_parsed :: [String] -> Property
prop_multiple_blocks_parsed contents =
  all (not . null) contents && all (not . any (`elem` "{}/") contents ==>
  let blockTemplate content = "{//! ownership: on}\n" ++ content
      blocks = map blockTemplate contents
      input = unlines $ intersperse "" blocks
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ length (tfBlocks file) === length contents .&&.
                        all (\block -> cbDirectives block /= defaultBlockDirectives) (tfBlocks file)
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)

-- Property: Parsing is idempotent for parsed content
prop_parsing_idempotent :: String -> Property
prop_parsing_idempotent content =
  not ("{//!" `isInfixOf` content) && not ("//!" `isPrefixOf` content) ==>
  case parseTypus content of
    Left _ -> property False
    Right file -> 
      let reconstructed = unlines $ map cbContent (tfBlocks file)
          result2 = parseTypus reconstructed
      in case result2 of
        Left _ -> property False
        Right file2 -> property $ length (tfBlocks file) === length (tfBlocks file2)

-- Property: Invalid directives are handled gracefully
prop_invalid_directives_handled :: String -> Property
prop_invalid_directives_handled invalidKey =
  not (null invalidKey) && not (any isSpace invalidKey) && not (invalidKey `elem` ["ownership", "dependent_types", "constraints"]) ==>
  let input = "//! " ++ invalidKey ++ ": on\nsome code"
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Expected to fail
    Right _ -> property False -- Should not succeed with invalid directives

-- Property: Comments in code are preserved (except directive comments)
prop_comments_preserved :: String -> Property
prop_comments_preserved content =
  not ("//!" `isInfixOf` content) && not ("{//!" `isInfixOf` content) ==>
  let withComments = content ++ "\n// regular comment\n/* block comment */\nmore code"
      result = parseTypus withComments
  in case result of
    Left _ -> property False
    Right file -> property $ any (isInfixOf "// regular comment") (map cbContent (tfBlocks file)) .&&.
                        any (isInfixOf "/* block comment */") (map cbContent (tfBlocks file))

-- Property: Whitespace handling in directives
prop_whitespace_directives :: String -> String -> Property
prop_whitespace_directives key value =
  key `elem` ["ownership", "dependent_types", "constraints"] && value `elem` ["on", "off"] ==>
  let inputs = [ "//! " ++ key ++ ":" ++ value
               , "//! " ++ key ++ " : " ++ value
               , "//!\t" ++ key ++ ":\t" ++ value
               , "//!  " ++ key ++ "  :  " ++ value
               ]
      results = map parseTypus inputs
      successes = [file | Right file <- results]
  in property $ length successes === length inputs -- All should parse successfully

-- Property: Complex nested structures
prop_complex_nested_structures :: String -> String -> String -> Property
prop_complex_nested_structures prefix middle suffix =
  not (any (`elem` "{}/") prefix) && not (any (`elem` "{}/") middle) && not (any (`elem` "{}/") suffix) ==>
  let input = prefix ++ "\n" ++
              "{//! ownership: on, dependent_types: true}\n" ++
              middle ++ "\n" ++
              "{//! constraints: off}\n" ++
              suffix
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ length (tfBlocks file) === 2 .&&.
                        case cbDirectives (head (tfBlocks file)) of
                          BlockDirectives (Just ownership) (Just dependentTypes) Nothing ->
                            locatedValue ownership .&&. locatedValue dependentTypes
                          _ -> property False

-- Property: Empty blocks are handled
prop_empty_blocks_handled :: Property
prop_empty_blocks_handled =
  let input = "{//! ownership: on}\n{//! dependent_types: true}\nregular code"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ length (tfBlocks file) >= 1 -- At least the non-empty block

-- Property: Line endings are preserved
prop_line_endings_preserved :: String -> Property
prop_line_endings_preserved content =
  not ("{//!" `isInfixOf` content) && not ("//!" `isPrefixOf` content) ==>
  let withLineEndings = content ++ "\n\n"  -- Ensure line endings
      result = parseTypus withLineEndings
  in case result of
    Left _ -> property False
    Right file -> property $ any (isInfixOf "\n") (map cbContent (tfBlocks file))

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Malformed directives are rejected
prop_malformed_directives_rejected :: String -> Property
prop_malformed_directives_rejected malformed =
  not (null malformed) && not ("//! ownership: on" `isPrefixOf` malformed) ==>
  let input = malformed ++ "\nsome code"
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Expected to fail
    Right _ -> property False -- Should not succeed with malformed directives

-- Property: Unclosed block directives are handled
prop_unclosed_blocks_handled :: String -> Property
prop_unclosed_blocks_handled content =
  not ("}" `isInfixOf` content) ==>
  let input = "{//! ownership: on}\n" ++ content
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Expected to fail
    Right _ -> property False -- Should not succeed with unclosed blocks

-- ============================================================================
-- Performance and Edge Cases
-- ============================================================================

-- Property: Large file parsing performance (bounded)
prop_large_file_parsing :: Int -> String -> Property
prop_large_file_parsing multiplier base =
  multiplier > 0 && multiplier <= 100 ==> -- Bounded for performance
  let largeContent = concat $ replicate multiplier (base ++ "\n")
      result = parseTypus largeContent
  in case result of
    Left _ -> property False
    Right file -> property $ not (null (tfBlocks file)) || null (trim base)

-- Property: Unicode content in code blocks
prop_unicode_content :: String -> Property
prop_unicode_content content =
  let unicodeContent = content ++ " café naïve 🚀 测试"
      input = "{//! ownership: on}\n" ++ unicodeContent
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ case tfBlocks file of
                              [block] -> "café naïve 🚀 测试" `isInfixOf` cbContent block
                              _ -> property False

-- ============================================================================
-- Helper Functions
-- ============================================================================

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "empty input parses to file with no blocks" prop_empty_input_parses
    , fastProperty "simple code without directives parses" prop_simple_code_parses
    , fastProperty "file directives are parsed correctly" prop_file_directives_parsed
    , fastProperty "block directives are parsed correctly" prop_block_directives_parsed
    , fastProperty "mixed file and block directives" prop_mixed_directives_parsed
    , fastProperty "build tags are preserved" prop_build_tags_preserved
    ]
  , testGroup "Complex Parsing Properties"
    [ fastProperty "multiple code blocks are parsed" prop_multiple_blocks_parsed
    , fastProperty "parsing is idempotent for parsed content" prop_parsing_idempotent
    , fastProperty "comments in code are preserved" prop_comments_preserved
    , fastProperty "whitespace handling in directives" prop_whitespace_directives
    , fastProperty "complex nested structures" prop_complex_nested_structures
    , fastProperty "empty blocks are handled" prop_empty_blocks_handled
    , fastProperty "line endings are preserved" prop_line_endings_preserved
    ]
  , testGroup "Error Handling Properties"
    [ fastProperty "invalid directives are handled gracefully" prop_invalid_directives_handled
    , fastProperty "malformed directives are rejected" prop_malformed_directives_rejected
    , fastProperty "unclosed block directives are handled" prop_unclosed_blocks_handled
    ]
  , testGroup "Performance and Edge Cases"
    [ fastProperty "large file parsing performance" prop_large_file_parsing
    , fastProperty "unicode content in code blocks" prop_unicode_content
    ]
  ]