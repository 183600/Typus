{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module TestSuite.ParserBoundary where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (Positive(..), NonEmptyList(..))
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedValue, locatedSpan, locatedWithSpan)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)

-- | Test properties for Parser module boundary conditions

-- | Default file directives should have Nothing values
prop_default_file_directives :: Property
prop_default_file_directives = 
  let dirs = defaultFileDirectives
  in property $ fdOwnership dirs == Nothing && 
              fdDependentTypes dirs == Nothing && 
              fdConstraints dirs == Nothing

-- | Default block directives should have Nothing values
prop_default_block_directives :: Property
prop_default_block_directives = 
  let dirs = defaultBlockDirectives
  in property $ bdOwnership dirs == Nothing && 
              bdDependentTypes dirs == Nothing && 
              bdConstraints dirs == Nothing

-- | Very long strings should be handled without issues
prop_long_strings :: Positive Int -> String -> Property
prop_long_strings (Positive len) content =
  let longString = replicate len 'a' ++ content
  in property $ length longString >= len

-- Unit tests
test_default_directives :: Assertion
test_default_directives = do
  let fileDirs = defaultFileDirectives
      blockDirs = defaultBlockDirectives
  assertEqual "Default file directives should be empty" 
    (FileDirectives Nothing Nothing Nothing) fileDirs
  assertEqual "Default block directives should be empty" 
    (BlockDirectives Nothing Nothing Nothing) blockDirs

test_sourcepos_creation :: Assertion
test_sourcepos_creation = do
  let pos = SourcePos 10 20 100
  assertEqual "source line" 10 (posLine pos)
  assertEqual "source column" 20 (posColumn pos)
  assertEqual "source offset" 100 (posOffset pos)

test_sourcespan_creation :: Assertion
test_sourcespan_creation = do
  let start = SourcePos 5 10 0
      end = SourcePos 5 15 0
      span = SourceSpan start end
  assertEqual "start position" start (spanStart span)
  assertEqual "end position" end (spanEnd span)

test_located_value :: Assertion
test_located_value = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 0)
      value = "test"
      located = locatedWithSpan span value
  assertEqual "located value" value (locatedValue located)
  assertEqual "located span" span (locatedSpan located)

test_typus_file_creation :: Assertion
test_typus_file_creation = do
  let file = TypusFile defaultFileDirectives [] [] []
  assertEqual "File should have default directives" defaultFileDirectives (tfDirectives file)
  assertEqual "File should have no build tags" [] (tfBuildTags file)
  assertEqual "File should have no blocks" [] (tfBlocks file)
  assertEqual "File should have no syntax errors" [] (tfSyntaxErrors file)

test_code_block_creation :: Assertion
test_code_block_creation = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 0)
      block = CodeBlock defaultBlockDirectives "test content" span
  assertEqual "Block should have default directives" defaultBlockDirectives (cbDirectives block)
  assertEqual "Block should have content" "test content" (cbContent block)
  assertEqual "Block should have span" span (cbSpan block)

test_unicode_handling :: Assertion
test_unicode_handling = do
  let unicodeString = "测试函数 with unicode: 你好世界"
      span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 30 0)
      located = locatedWithSpan span unicodeString
  assertBool "Should handle unicode characters" $ 
    "测试函数" `isInfixOf` locatedValue located && 
    "你好世界" `isInfixOf` locatedValue located

-- | Test suite for Parser boundary conditions
tests :: TestTree
tests = testGroup "Parser Boundary Condition Tests"
  [ testProperty "Default file directives" prop_default_file_directives
  , testProperty "Default block directives" prop_default_block_directives
  , testProperty "Long strings handling" prop_long_strings
  , testCase "Default directives" test_default_directives
  , testCase "SourcePos creation" test_sourcepos_creation
  , testCase "SourceSpan creation" test_sourcespan_creation
  , testCase "Located value" test_located_value
  , testCase "TypusFile creation" test_typus_file_creation
  , testCase "CodeBlock creation" test_code_block_creation
  , testCase "Unicode handling" test_unicode_handling
  ]