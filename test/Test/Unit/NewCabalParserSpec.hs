module Test.Unit.NewCabalParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((===), Property, counterexample)

import TestSupport.QuickCheck (fastProperty)
import Parser
  ( parseTypus
  , TypusFile(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T

-- | Additional comprehensive tests for Parser module
tests :: TestTree
tests =
  testGroup "NewCabal Parser Tests"
    [ testGroup "File directive parsing"
        [ testCase "parses simple ownership directive" $ do
            let input = "//! ownership: on\n"
                result = parseTypus input
            case result of
              Right file -> assertBool "ownership should be enabled" $
                case fdOwnership (tfDirectives file) of
                  Just (Located value _) -> value
                  Nothing -> False
              Left _ -> assertBool "should parse successfully" False

        , testCase "parses multiple file directives" $ do
            let input = "//! ownership: on, dependent_types: true\n"
                result = parseTypus input
            case result of
              Right file -> do
                assertBool "ownership should be enabled" $
                  case fdOwnership (tfDirectives file) of
                    Just (Located value _) -> value
                    Nothing -> False
                assertBool "dependent_types should be enabled" $
                  case fdDependentTypes (tfDirectives file) of
                    Just (Located value _) -> value
                    Nothing -> False
              Left _ -> assertBool "should parse successfully" False

        , testCase "handles invalid file directive gracefully" $ do
            let input = "//! invalid_directive: on\n"
                result = parseTypus input
            case result of
              Left _ -> assertBool "should fail on invalid directive" True
              Right _ -> assertBool "should not succeed" False
        ]

    , testGroup "Block directive parsing"
        [ testCase "parses block with ownership directive" $ do
            let input = "{//! ownership: on}\nfunc test() {}\n"
                result = parseTypus input
            case result of
              Right file -> do
                let blocks = tfBlocks file
                assertBool "should have one block" $ length blocks == 1
                case blocks of
                  (block:_) -> assertBool "block should have ownership enabled" $
                    case bdOwnership (cbDirectives block) of
                      Just (Located value _) -> value
                      Nothing -> False
                  [] -> assertBool "should have blocks" False
              Left _ -> assertBool "should parse successfully" False

        , testCase "handles nested braces in block content" $ do
            let input = "{//! ownership: on}\nfunc test() {\n  if true {\n    return\n  }\n}\n"
                result = parseTypus input
            case result of
              Right file -> do
                let blocks = tfBlocks file
                assertBool "should have one block" $ length blocks == 1
                case blocks of
                  (block:_) -> assertBool "block content should contain nested braces" $
                    "{\n  if true {\n    return\n  }\n}" `isInfixOf` cbContent block
                  [] -> assertBool "should have blocks" False
              Left _ -> assertBool "should parse successfully" False
        ]

    , testGroup "Build tag parsing"
        [ testCase "parses go build tags" $ do
            let input = "//go:build linux\n// +build amd64\n\npackage main\n"
                result = parseTypus input
            case result of
              Right file -> do
                let buildTags = tfBuildTags file
                assertBool "should have two build tags" $ length buildTags == 2
                assertBool "first tag should be go:build" $
                  "//go:build linux" `isInfixOf` locatedValue (head buildTags)
                assertBool "second tag should be +build" $
                  "// +build amd64" `isInfixOf` locatedValue (buildTags !! 1)
              Left _ -> assertBool "should parse successfully" False
        ]

    , testGroup "Error handling"
        [ testCase "detects unclosed directive blocks" $ do
            let input = "{//! ownership: on\nfunc test() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool "should report unclosed block" $
                "Unclosed directive block" `isInfixOf` err
              Right _ -> assertBool "should not succeed" False

        , testCase "handles malformed directives" $ do
            let input = "{//! ownership}\nfunc test() {}\n"
                result = parseTypus input
            case result of
              Left _ -> assertBool "should fail on malformed directive" True
              Right _ -> assertBool "should not succeed" False
        ]

    , testGroup "QuickCheck property tests"
        [ fastProperty "parseTypus handles empty input" prop_parseEmptyInput
        , fastProperty "parseTypus preserves content structure" prop_preserveContentStructure
        , fastProperty "default directives are used when none specified" prop_defaultDirectives
        ]
    ]

-- Helper function to check substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

-- Property: parseTypus should handle empty input gracefully
prop_parseEmptyInput :: Property
prop_parseEmptyInput =
  let result = parseTypus ""
  in counterexample ("result: " ++ show result) $
     case result of
       Right file -> tfDirectives file === defaultFileDirectives &&
                    null (tfBlocks file) &&
                    null (tfBuildTags file)
       Left _ -> property False  -- Should not fail on empty input

-- Property: parseTypus should preserve basic content structure
prop_preserveContentStructure :: String -> Property
prop_preserveContentStructure input =
  let result = parseTypus input
  in counterexample ("input: " ++ show input ++ ", result: " ++ show result) $
     case result of
       Right file -> length (tfBlocks file) >= 0  -- Should have non-negative number of blocks
       Left _ -> property False  -- For this test, expect parsing to succeed

-- Property: default directives should be used when none specified
prop_defaultDirectives :: Property
prop_defaultDirectives =
  let input = "func test() {}\n"
      result = parseTypus input
  in counterexample ("result: " ++ show result) $
     case result of
       Right file -> tfDirectives file === defaultFileDirectives
       Left _ -> property False