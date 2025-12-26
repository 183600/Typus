{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, choose)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- ============================================================================
-- Test Generators
-- ============================================================================

-- Generate arbitrary source positions
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- Generate arbitrary source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- Generate arbitrary located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value span

-- Generate file directives
instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ FileDirectives ownership dependentTypes constraints

-- Generate block directives  
instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate code blocks
instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- listOf $ choose (' ', '~')
    return $ CodeBlock directives content

-- ============================================================================
-- Boundary Condition Tests
-- ============================================================================

-- Test parsing empty input
testParseEmptyInput :: TestTree
testParseEmptyInput = testCase "Parse empty input" $ do
  let result = parseTypus ""
  case result of
    Left _ -> assertBool "Empty input should parse successfully" False
    Right file -> assertEqual "Empty file should have no blocks" 0 (length (tfCodeBlocks file))

-- Test parsing only whitespace
testParseOnlyWhitespace :: TestTree
testParseOnlyWhitespace = testCase "Parse only whitespace" $ do
  let whitespace = "   \n  \t   \n   "
  let result = parseTypus whitespace
  case result of
    Left _ -> assertBool "Whitespace-only input should parse successfully" False
    Right file -> assertEqual "Whitespace-only file should have no blocks" 0 (length (tfCodeBlocks file))

-- Test parsing extremely long lines
testParseExtremelyLongLines :: TestTree
testParseExtremelyLongLines = testCase "Parse extremely long lines" $ do
  let longLine = replicate 10000 'a' ++ "\n"
  let result = parseTypus longLine
  case result of
    Left _ -> assertBool "Very long lines should be handled" False
    Right _ -> return ()

-- Test parsing deeply nested indentation
testParseDeeplyNestedIndentation :: TestTree
testParseDeeplyNestedIndentation = testCase "Parse deeply nested indentation" $ do
  let nested = concat $ replicate 100 "    " ++ "content\n"
  let result = parseTypus nested
  case result of
    Left _ -> assertBool "Deeply nested indentation should be handled" False
    Right _ -> return ()

-- Test parsing with mixed line endings
testParseMixedLineEndings :: TestTree
testParseMixedLineEndings = testCase "Parse mixed line endings" $ do
  let mixed = "line1\nline2\r\nline3\n"
  let result = parseTypus mixed
  case result of
    Left _ -> assertBool "Mixed line endings should be handled" False
    Right _ -> return ()

-- Test parsing with Unicode characters
testParseUnicodeCharacters :: TestTree
testParseUnicodeCharacters = testCase "Parse Unicode characters" $ do
  let unicode = "测试中文 🚀 TypeScript ñáéíóú\n"
  let result = parseTypus unicode
  case result of
    Left _ -> assertBool "Unicode characters should be handled" False
    Right _ -> return ()

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parsing twice should give same result
propParsingIdempotent :: String -> Bool
propParsingIdempotent input = 
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Left _, Left _) -> True
    (Right f1, Right f2) -> length (tfCodeBlocks f1) == length (tfCodeBlocks f2)
    _ -> False

-- Property: Adding whitespace at start/end shouldn't change block count
propWhitespaceInvariant :: String -> Bool
propWhitespaceInvariant input =
  let result1 = parseTypus input
      result2 = parseTypus ("  \n  " ++ input ++ "  \n  ")
  in case (result1, result2) of
    (Left _, Left _) -> True
    (Right f1, Right f2) -> length (tfCodeBlocks f1) == length (tfCodeBlocks f2)
    _ -> False

-- Property: Parsing should never crash on any string
propParsingNeverCrashes :: String -> Bool
propParsingNeverCrashes input = 
  case parseTypus input of
    Left _ -> True
    Right _ -> True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Boundary Conditions Tests"
  [ testParseEmptyInput
  , testParseOnlyWhitespace
  , testParseExtremelyLongLines
  , testParseDeeplyNestedIndentation
  , testParseMixedLineEndings
  , testParseUnicodeCharacters
  , testProperty "Parsing is idempotent" propParsingIdempotent
  , testProperty "Whitespace invariant" propWhitespaceInvariant
  , testProperty "Parsing never crashes" propParsingNeverCrashes
  ]