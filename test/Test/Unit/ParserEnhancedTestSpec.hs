module Test.Unit.ParserEnhancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), spanBetween, posAt)

-- | Enhanced unit tests for Parser module
tests :: TestTree
tests =
  testGroup "Parser Enhanced Tests"
    [ testGroup "File directive parsing"
        [ testCase "parse simple ownership directive" $ do
            let content = "//! ownership=true\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case fdOwnership directives of
                  Just (Located _ True) -> return ()
                  _ -> assertBool "Should have ownership=true" False

        , testCase "parse multiple file directives" $ do
            let content = "//! ownership=true, dependent-types=true\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case (fdOwnership directives, fdDependentTypes directives) of
                  (Just (Located _ True), Just (Located _ True)) -> return ()
                  _ -> assertBool "Should have both directives true" False

        , testCase "parse directive with false value" $ do
            let content = "//! ownership=false\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case fdOwnership directives of
                  Just (Located _ False) -> return ()
                  _ -> assertBool "Should have ownership=false" False

        , testCase "parse without directives" $ do
            let content = "some content without directives"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                directives @?= defaultFileDirectives
        ]

    , testGroup "Block directive parsing"
        [ testCase "parse block with ownership directive" $ do
            let content = "/* ownership=true */\nsome code\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should have one block" (length blocks == 1)
                let block = head blocks
                let directives = cbDirectives block
                case bdOwnership directives of
                  Just (Located _ True) -> return ()
                  _ -> assertBool "Should have block ownership=true" False

        , testCase "parse block with multiple directives" $ do
            let content = "/* ownership=true, dependent-types=false */\ncode\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                let block = head blocks
                let directives = cbDirectives block
                case (bdOwnership directives, bdDependentTypes directives) of
                  (Just (Located _ True), Just (Located _ False)) -> return ()
                  _ -> assertBool "Should parse both directives correctly" False

        , testCase "parse block without directives" $ do
            let content = "/* regular comment */\nsome code\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                let block = head blocks
                let directives = cbDirectives block
                directives @?= defaultBlockDirectives
        ]

    , testGroup "Build tags parsing"
        [ testCase "parse single build tag" $ do
            let content = "// +build linux\nsome code\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let buildTags = tfBuildTags typusFile
                assertBool "Should have one build tag" (length buildTags == 1)
                let tag = head buildTags
                locatedValue tag @?= "linux"

        , testCase "parse multiple build tags" $ do
            let content = "// +build linux,amd64\n// +build !windows\nsome code\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let buildTags = tfBuildTags typusFile
                assertBool "Should have two build tags" (length buildTags == 2)
                let tag1 = buildTags !! 0
                let tag2 = buildTags !! 1
                locatedValue tag1 @?= "linux,amd64"
                locatedValue tag2 @?= "!windows"

        , testCase "parse without build tags" $ do
            let content = "just code without build tags"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let buildTags = tfBuildTags typusFile
                assertBool "Should have no build tags" (null buildTags)
        ]

    , testGroup "Code block parsing"
        [ testCase "parse single code block" $ do
            let content = "some code content"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should have one block" (length blocks == 1)
                let block = head blocks
                cbContent block @?= content
                isValidSpan (cbSpan block) @?= True

        , testCase "parse multiple code blocks" $ do
            let content = "first block\n\nsecond block\n"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should have two blocks" (length blocks == 2)
                let block1 = blocks !! 0
                let block2 = blocks !! 1
                cbContent block1 @?= "first block"
                cbContent block2 @?= "second block"

        , testCase "preserve empty lines in blocks" $ do
            let content = "line1\n\nline3"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                let block = head blocks
                cbContent block @?= content
        ]

    , testGroup "Error handling and edge cases"
        [ testCase "handle empty input" $ do
            let content = ""
            case parseTypus content of
              Left err -> assertBool ("Should parse empty input: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Empty input should have no blocks" (null blocks)

        , testCase "handle only whitespace" $ do
            let content = "   \n\t  \n   "
            case parseTypus content of
              Left err -> assertBool ("Should parse whitespace: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Whitespace should have no blocks" (null blocks)

        , testCase "handle malformed directives gracefully" $ do
            let content = "//! invalid-directive\nsome code"
            case parseTypus content of
              Left err -> assertBool ("Should handle malformed directive: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should still parse code block" (length blocks >= 1)

        , testCase "handle unclosed block comments" $ do
            let content = "/* ownership=true\nsome code"
            case parseTypus content of
              Left err -> assertBool ("Should handle unclosed comment: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should parse despite unclosed comment" (length blocks >= 1)
        ]
    ]