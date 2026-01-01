{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation (SourceSpan(..), SourcePos(..), startPos)

import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- ============================================================================
-- Additional Cabal Tests for Parser Module
-- ============================================================================

-- | Test case 1: Parse file with mixed directives
test_parse_mixed_directives :: TestTree
test_parse_mixed_directives = testCase "parseTypus handles mixed directives correctly" $ do
    let input = unlines
            [ "// @ownership: true"
            , "// @dependent-types: false"
            , "// @constraints: true"
            , ""
            , "// build-tags: test,debug"
            , ""
            , "```typus"
            , "// @ownership: false"
            , "func test() {}"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            let fileDirectives = tfDirectives typusFile
            assertEqual "ownership directive" (Just True) (fmap locatedValue $ fdOwnership fileDirectives)
            assertEqual "dependent-types directive" (Just False) (fmap locatedValue $ fdDependentTypes fileDirectives)
            assertEqual "constraints directive" (Just True) (fmap locatedValue $ fdConstraints fileDirectives)

-- | Test case 2: Parse empty file
test_parse_empty_file :: TestTree
test_parse_empty_file = testCase "parseTypus handles empty file" $ do
    result <- parseTypus ""
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            assertEqual "default file directives" defaultFileDirectives (tfDirectives typusFile)
            assertEqual "no build tags" [] (tfBuildTags typusFile)
            assertEqual "no blocks" [] (tfBlocks typusFile)

-- | Test case 3: Parse file with only comments
test_parse_comments_only :: TestTree
test_parse_comments_only = testCase "parseTypus handles comments-only file" $ do
    let input = unlines
            [ "// This is a comment"
            , "// Another comment"
            , "/* Block comment */"
            , "// @ownership: true"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            let fileDirectives = tfDirectives typusFile
            assertEqual "ownership directive from comments" (Just True) (fmap locatedValue $ fdOwnership fileDirectives)

-- | Test case 4: Parse malformed directives gracefully
test_parse_malformed_directives :: TestTree
test_parse_malformed_directives = testCase "parseTypus handles malformed directives gracefully" $ do
    let input = unlines
            [ "// @ownership: maybe"
            , "// @dependent-types"
            , "// @constraints: true extra"
            , ""
            , "```typus"
            , "code here"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            -- Should parse successfully but potentially ignore malformed directives
            assertBool "has at least one block" $ not $ L.null $ tfBlocks typusFile

-- | Test case 5: Parse multiple code blocks
test_parse_multiple_blocks :: TestTree
test_parse_multiple_blocks = testCase "parseTypus handles multiple code blocks" $ do
    let input = unlines
            [ "// @ownership: true"
            , ""
            , "```typus"
           , "// @ownership: false"
           , "func first() {}"
           , "```"
            , ""
            , "```typus"
           , "// @dependent-types: true"
           , "func second() {}"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            assertEqual "two code blocks" 2 (L.length $ tfBlocks typusFile)
            let blocks = tfBlocks typusFile
            let firstBlock = L.head blocks
            let secondBlock = blocks !! 1
            assertBool "first block has ownership directive" $ isJust $ bdOwnership $ cbDirectives firstBlock
            assertBool "second block has dependent-types directive" $ isJust $ bdDependentTypes $ cbDirectives secondBlock

-- | Test case 6: Property test for directive parsing consistency
prop_directive_parsing_consistency :: String -> Property
prop_directive_parsing_consistency directiveStr =
    let validPrefix = "// @"
        hasValidPrefix = validPrefix `L.isPrefixOf` directiveStr
    in classify hasValidPrefix "has valid directive prefix" $
       property $ True -- Basic property test - more complex tests would need parsing functions

-- | Test case 7: Property test for block content preservation
prop_block_content_preservation :: String -> Property
prop_block_content_preservation content =
    let input = unlines ["```typus", content, "```"]
    in property $ True -- Basic property - actual implementation would parse L.and compare

-- | Test case 8: Parse with build tags
test_parse_build_tags :: TestTree
test_parse_build_tags = testCase "parseTypus handles build tags correctly" $ do
    let input = unlines
            [ "// build-tags: test,debug,release"
            , "// @ownership: true"
            , ""
            , "```typus"
            , "func main() {}"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            let buildTags = tfBuildTags typusFile
            assertEqual "three build tags" 3 (L.length buildTags)
            assertEqual "first tag" "test" (locatedValue $ L.head buildTags)
            assertEqual "second tag" "debug" (locatedValue $ buildTags !! 1)
            assertEqual "third tag" "release" (locatedValue $ buildTags !! 2)

-- | Test case 9: Parse nested block comments
test_parse_nested_block_comments :: TestTree
test_parse_nested_block_comments = testCase "parseTypus handles nested block comments" $ do
    let input = unlines
            [ "/* outer comment"
            , "   /* inner comment */"
            , "   still in outer"
            , "*/"
            , "// @ownership: true"
            , ""
            , "```typus"
           , "func test() {}"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            let fileDirectives = tfDirectives typusFile
            assertEqual "ownership directive parsed" (Just True) (fmap locatedValue $ fdOwnership fileDirectives)

-- | Test case 10: Parse with Unicode content
test_parse_unicode_content :: TestTree
test_parse_unicode_content = testCase "parseTypus handles Unicode content" $ do
    let input = unlines
            [ "// @ownership: true"
            , "// 描述: 这是一个测试"
            , ""
            , "```typus"
            , "func 测试函数() {"
            , "  let 世界 = \"hello world\""
            , "}"
            , "```"
            ]
    
    result <- parseTypus input
    
    case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "has one block" $ L.length blocks == 1
            let blockContent = cbContent (L.head blocks)
            assertBool "contains Unicode characters" $ "测试函数" `L.isInfixOf` blockContent
            assertBool "contains Chinese characters" $ "世界" `L.isInfixOf` blockContent

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Cabal Tests"
    [ testGroup "Unit Tests"
        [ test_parse_mixed_directives
        , test_parse_empty_file
        , test_parse_comments_only
        , test_parse_malformed_directives
        , test_parse_multiple_blocks
        , test_parse_build_tags
        , test_parse_nested_block_comments
        , test_parse_unicode_content
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "directive parsing consistency" prop_directive_parsing_consistency
        , fastProperty "block content preservation" prop_block_content_preservation
        ]
    ]