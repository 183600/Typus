{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreParserEssentialSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import qualified Data.Text as T

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)

tests :: TestTree
tests = testGroup "Core Parser Essential Tests"
  [ testGroup "Directive Parsing"
    [ testCase "defaultFileDirectives has no directives" $
        assertEqual "default should have no directives" 
          (FileDirectives Nothing Nothing Nothing) 
          defaultFileDirectives
    
    , testCase "defaultBlockDirectives has no directives" $
        assertEqual "default should have no directives"
          (BlockDirectives Nothing Nothing Nothing)
          defaultBlockDirectives
    
    , testCase "parseTypus handles empty input" $
        case parseTypus "" of
          Left err -> assertBool "should not error on empty input" False
          Right result -> assertEqual "should create empty file" 
            (TypusFile defaultFileDirectives []) result
    
    , testCase "parseTypus handles simple code block" $
        let input = "func main() {\n  return 0\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse simple block" False
          Right result -> assertBool "should have one code block" 
            (L.length (tfCodeBlocks result) == 1)
    ]
  
  , testGroup "Code Block Recognition"
    [ testCase "parseTypus recognizes function blocks" $
        let input = "func test() {\n  // function body\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse function" False
          Right result -> do
            assertBool "should have code block" (not $ L.null $ tfCodeBlocks result)
            let block = L.head $ tfCodeBlocks result
            assertBool "should contain function keyword" 
              ("func" `L.isInfixOf` cbContent block)
    
    , testCase "parseTypus handles multiple blocks" $
        let input = "func one() {}\nfunc two() {}\nfunc three() {}"
        in case parseTypus input of
          Left err -> assertBool "should parse multiple blocks" False
          Right result -> 
            assertEqual "should have three blocks" 3 (L.length $ tfCodeBlocks result)
    
    , testCase "parseTypus preserves block content" $
        let input = "func test() {\n  let x = 42\n  return x\n}"
        in case parseTypus input of
          Left err -> assertBool "should preserve content" False
          Right result -> do
            let blocks = tfCodeBlocks result
            assertBool "should have block" (not $ null blocks)
            let content = cbContent $ L.head blocks
            assertBool "should contain let statement" ("let x = 42" `L.isInfixOf` content)
    ]
  
  , testGroup "File Structure Parsing"
    [ testCase "parseTypus handles file with directives" $
        let input = "// @ownership: true\nfunc test() {}"
        in case parseTypus input of
          Left err -> assertBool "should parse with directives" False
          Right result -> do
            let directives = tfDirectives result
            assertBool "should have ownership directive" 
              (isJust $ fdOwnership directives)
    
    , testCase "parseTypus handles mixed content" $
        let input = "// @dependent-types: true\n\nfunc main() {}\n// comment\n"
        in case parseTypus input of
          Left err -> assertBool "should handle mixed content" False
          Right result -> do
            assertBool "should have dependent-types directive" 
              (isJust $ fdDependentTypes $ tfDirectives result)
            assertBool "should have code block" 
              (not $ L.null $ tfCodeBlocks result)
    ]
  
  , testGroup "Error Handling"
    [ testCase "parseTypus provides meaningful errors" $
        let input = "func incomplete {\n  missing closing"
        in case parseTypus input of
          Left _ -> assertBool "should detect syntax error" True
          Right _ -> assertBool "should not succeed on invalid input" False
    
    , testCase "parseTypus handles unicode content" $
        let input = "func 测试() {\n  return \"你好\"\n}"
        in case parseTypus input of
          Left err -> assertBool "should handle unicode" False
          Right result -> 
            assertBool "should parse unicode correctly" 
              (not $ L.null $ tfCodeBlocks result)
    ]
  
  , testGroup "Block Directive Parsing"
    [ testCase "parseTypus recognizes block directives" $
        let input = "// @ownership: false\nfunc test() {}"
        in case parseTypus input of
          Left err -> assertBool "should parse block directives" False
          Right result -> do
            let blocks = tfCodeBlocks result
            assertBool "should have block" (not $ null blocks)
            let block = L.head blocks
            let directives = cbDirectives block
            assertBool "should have block ownership directive" 
              (isJust $ bdOwnership directives)
    ]
  ]
  where
    isJust Nothing = False
    isJust (Just _) = True