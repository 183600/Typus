{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewParserBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Text as T
import qualified Data.Char as Char

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.QuickCheck (fastProperty)

-- | Test parser boundary conditions and error recovery
tests :: TestTree
tests =
  testGroup "New Parser Boundary Tests"
    [ testGroup "Directive parsing"
        [ testCase "parses empty file directives" $ do
            let input = ""
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should parse empty file" False
              Right (TypusFile directives blocks) -> do
                directives @?= defaultFileDirectives
                blocks @?= []

        , testCase "parses file-level ownership directive" $ do
            let input = "// @ownership: true\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should parse ownership directive" False
              Right (TypusFile directives blocks) -> do
                case fdOwnership directives of
                  Nothing -> assertBool "Should have ownership directive" False
                  Just (Located _ val) -> val @?= True

        , testCase "parses block-level dependent types directive" $ do
            let input = "// @dependent-types: false\nfunc test() {}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should parse dependent types directive" False
              Right (TypusFile directives blocks) -> do
                length blocks @?= 1
                let block = head blocks
                case bdDependentTypes (blockDirectives block) of
                  Nothing -> assertBool "Should have dependent types directive" False
                  Just (Located _ val) -> val @?= False
        ]

    , testGroup "Error recovery and malformed input"
        [ testCase "recovers from malformed directive" $ do
            let input = "// @ownership: maybe\nfunc test() {}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should attempt recovery" False
              Right (TypusFile _ blocks) -> do
                length blocks @?= 1

        , testCase "handles unterminated strings gracefully" $ do
            let input = "func test() {\n  s := \"unterminated string\n}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should report error but not crash" True
              Right _ -> assertBool "Should not succeed with malformed input" False

        , testCase "handles deeply nested structures" $ do
            let nested = concat $ replicate 100 "  if true { "
                input = nested ++ "func test() {}" ++ concat (replicate 100 " }")
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle deep nesting" False
              Right (TypusFile _ blocks) -> do
                length blocks @?= 1

        , testCase "recovers from missing closing brace" $ do
            let input = "func test() {\n  if true {\n    // missing closing braces\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should report error gracefully" True
              Right _ -> assertBool "Should not succeed with incomplete input" False
        ]

    , testGroup "Unicode and special characters"
        [ testCase "handles Unicode identifiers" $ do
            let input = "func 测试函数() {\n  变量 := \"值\"\n}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle Unicode" False
              Right (TypusFile _ blocks) -> do
                length blocks @?= 1

        , testCase "handles special characters in strings" $ do
            let input = "func test() {\n  s := \"特殊字符: \\n\\t\\\"\\\\\"\n}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle escaped characters" False
              Right (TypusFile _ blocks) -> do
                length blocks @?= 1

        , testCase "handles mixed line endings" $ do
            let input = "func test() {\r\n  x := 1\n}\r\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle mixed line endings" False
              Right (TypusFile _ blocks) -> do
                length blocks @?= 1
        ]

    , testGroup "Performance and large inputs"
        [ fastProperty "handles large files efficiently" prop_largeFileParsing
        , fastProperty "handles long lines without stack overflow" prop_longLineParsing
        ]

    , testGroup "Edge cases"
        [ testCase "handles only whitespace input" $ do
            let input = "   \n\t  \n   \t\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle whitespace-only input" False
              Right (TypusFile directives blocks) -> do
                directives @?= defaultFileDirectives
                blocks @?= []

        , testCase "handles only comments" $ do
            let input = "// This is a comment\n/* This is a block comment */\n// Another comment\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle comment-only input" False
              Right (TypusFile directives blocks) -> do
                blocks @?= []

        , testCase "handles malformed block comments" $ do
            let input = "/* Unterminated block comment\nfunc test() {}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should handle malformed block comments" True
              Right _ -> assertBool "Should not succeed with malformed comments" False
        ]

    , testGroup "Directive precedence and inheritance"
        [ testCase "block directives override file directives" $ do
            let input = "// @ownership: true\n// @ownership: false\nfunc test() {}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should parse conflicting directives" False
              Right (TypusFile directives blocks) -> do
                length blocks @?= 1
                let block = head blocks
                -- Block directive should override file directive
                case bdOwnership (blockDirectives block) of
                  Nothing -> assertBool "Should have block ownership directive" False
                  Just (Located _ val) -> val @?= False

        , testCase "directives without proper format are ignored" $ do
            let input = "// @invalid-directive: true\nfunc test() {}\n"
                result = parseTypus "test" input
            case result of
              Left _ -> assertBool "Should ignore invalid directives" False
              Right (TypusFile directives blocks) -> do
                directives @?= defaultFileDirectives
                length blocks @?= 1
        ]
    ]

-- Property: Parser should handle large files efficiently
prop_largeFileParsing :: Positive Int -> Property
prop_largeFileParsing (Positive numBlocks) =
  let blocks = replicate numBlocks "func test() { return 42 }\n"
      input = concat blocks
      result = parseTypus "test" input
  in case result of
       Left _ -> property False
       Right (TypusFile _ parsedBlocks) -> length parsedBlocks == numBlocks

-- Property: Parser should handle long lines
prop_longLineParsing :: Positive Int -> Property
prop_longLineParsing (Positive lineLength) =
  let longLine = replicate lineLength 'x'
      input = "func test() {\n  s := \"" ++ longLine ++ "\"\n}\n"
      result = parseTypus "test" input
  in case result of
       Left _ -> property False
       Right (TypusFile _ blocks) -> length blocks == 1

-- Helper wrapper for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> choose (1, 50)  -- Keep it reasonable for testing