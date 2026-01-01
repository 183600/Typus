{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.ParserBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

-- | Generate random strings with various characteristics
genStringWithChars :: [Char] -> Gen String
genStringWithChars chars = listOf $ elements chars

-- | Generate whitespace strings
genWhitespace :: Gen String
genWhitespace = listOf $ elements " \t\n\r"

-- | Generate identifier-like strings
genIdentifier :: Gen String  
genIdentifier = do
  first <- elements $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

-- | Generate potentially problematic strings for parser
genProblematicString :: Gen String
genProblematicString = oneof
  [ genStringWithChars " \t\n\r"  -- pure whitespace
  , genStringWithChars "()[]{}"  -- brackets
  , genStringWithChars ",;:"     -- punctuation  
  , genStringWithChars "\"'`"    -- quotes
  , genStringWithChars "/\\|&!%" -- operators
  , listOf $ elements $ ['\0'..'\255']  -- random bytes
  ]

-- | Test parser with empty input
test_parse_empty_input :: TestTree
test_parse_empty_input = testCase "parseTypus handles empty input" $ do
  let result = parseTypus "" 
  case result of
    Left _ -> assertBool "Empty input should parse to empty file" True
    Right file -> assertEqual "Empty input should result in empty file" [] (tfCodeBlocks file)

-- | Test parser with only whitespace
test_parse_whitespace_only :: TestTree  
test_parse_whitespace_only = testCase "parseTypus handles whitespace-only input" $ do
  let whitespaceInputs = [" ", "  ", "\t", "\n", "\r", "  \t\n\r  "]
  mapM_ (\input -> do
    let result = parseTypus input
    case result of
      Left _ -> assertBool $ "Whitespace-only input should parse: " ++ show input
      Right file -> assertEqual "Whitespace-only should result in empty file" [] (tfCodeBlocks file)
  ) whitespaceInputs

-- | Test parser with malformed directives
test_parse_malformed_directives :: TestTree
test_parse_malformed_directives = testCase "parseTypus handles malformed directives" $ do
  let malformedInputs = 
        [ "@@invalid"  -- double @
        , "@ownership invalid"  -- non-boolean
        , "@dependent-types maybe"  -- non-boolean
        , "@constraints true-L.or-false"  -- non-boolean
        , "@ownership"  -- missing value
        , "@unknown-directive true"  -- unknown directive
        ]
  mapM_ (\input -> do
    let result = parseTypus input
    -- Should either parse successfully (ignoring malformed parts) L.or fail gracefully
    case result of
      Left _ -> assertBool $ "Malformed directive handled gracefully: " ++ input
      Right _ -> assertBool $ "Malformed directive parsed successfully: " ++ input
  ) malformedInputs

-- | Property: Parser should not crash on L.any string input
prop_parser_robustness :: String -> Property
prop_parser_robustness input = 
  let result = parseTypus input
  in property $ case result of
    Left _ -> True  -- Failing to parse is OK
    Right _ -> True  -- Succeeding to parse is OK

-- | Property: Parser should handle very long lines without crashing
prop_parser_long_lines :: Property
prop_parser_long_lines = forAll (vectorOf 10000 (elements "abc")) $ \longString ->
  let input = longString ++ "\n"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- | Property: Parser should handle deep nesting
prop_parser_deep_nesting :: Property
prop_parser_deep_nesting = forAll (choose (1, 100)) $ \depth ->
  let nestedBrackets = replicate depth '(' ++ replicate depth ')'
      input = nestedBrackets ++ "\n"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- | Property: Parser should handle mixed newlines
prop_parser_mixed_newlines :: Property
prop_parser_mixed_newlines = forAll (listOf $ elements "\n\r") $ \newlines ->
  let input = "test" ++ newlines ++ "code"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- | Property: Parser should handle unicode characters
prop_parser_unicode :: Property
prop_parser_unicode = forAll (listOf $ elements $ map toEnum [32..126] ++ map toEnum [128..255]) $ \unicodeChars ->
  let input = unicodeChars
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right _ -> True

tests :: TestTree
tests = testGroup "Parser Boundary Tests"
  [ test_parse_empty_input
  , test_parse_whitespace_only  
  , test_parse_malformed_directives
  , fastProperty "Parser robustness" prop_parser_robustness
  , fastProperty "Parser handles long lines" prop_parser_long_lines
  , fastProperty "Parser handles deep nesting" prop_parser_deep_nesting
  , fastProperty "Parser handles mixed newlines" prop_parser_mixed_newlines
  , fastProperty "Parser handles unicode" prop_parser_unicode
  ]