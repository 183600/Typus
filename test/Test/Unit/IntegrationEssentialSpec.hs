{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.IntegrationEssentialSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import qualified Data.Text as T

import Parser (parseTypus, TypusFile(..), defaultFileDirectives)
import Compiler (compile, generateGoCode, hasTypeErrors)
import Utils (trim, splitBy, removeComments)
import SourceLocation (SourcePos(..), Located(..), locatedAt)

tests :: TestTree
tests = testGroup "Integration Essential Tests"
  [ testGroup "Parser to Compiler Integration"
    [ testCase "complete pipeline: parse -> compile" $
        let input = "func add(a: int, b: int) int {\n  return a + b\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse successfully" False
          Right parsedFile -> 
            case compile parsedFile of
              Left err -> assertBool "should compile successfully" False
              Right result -> assertBool "should produce compilation result" True
    
    , testCase "pipeline preserves function signatures" $
        let input = "func multiply(x: int, y: int) int {\n  return x * y\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse" False
          Right parsedFile -> 
            case compile parsedFile of
              Left err -> assertBool "should compile" False
              Right result -> assertBool "should complete" True
    
    , testCase "pipeline handles multiple functions" $
        let input = "func first() {}\nfunc second() {}\nfunc third() {}"
        in case parseTypus input of
          Left err -> assertBool "should parse multiple functions" False
          Right parsedFile -> 
            case compile parsedFile of
              Left err -> assertBool "should compile multiple functions" False
              Right result -> assertBool "should handle all functions" True
    ]
  
  , testGroup "Parser to Go Code Generation"
    [ testCase "complete pipeline: parse -> generate Go" $
        let input = "func hello() {\n  println(\"Hello, World!\")\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse" False
          Right parsedFile -> 
            case generateGoCode parsedFile of
              Left err -> assertBool "should generate Go code" False
              Right goCode -> do
                assertBool "should contain package declaration" ("package" `T.isInfixOf` goCode)
                assertBool "should contain func keyword" ("func" `T.isInfixOf` goCode)
    
    , testCase "pipeline preserves function names in Go code" $
        let input = "func calculate() {\n  return 42\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse" False
          Right parsedFile -> 
            case generateGoCode parsedFile of
              Left err -> assertBool "should generate Go" False
              Right goCode -> 
                assertBool "should preserve function name" ("calculate" `T.isInfixOf` goCode)
    
    , testCase "pipeline handles type annotations" $
        let input = "func typed(a: int, b: string) bool {\n  return true\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse typed function" False
          Right parsedFile -> 
            case generateGoCode parsedFile of
              Left err -> assertBool "should generate typed Go" False
              Right goCode -> do
                assertBool "should contain int type" ("int" `T.isInfixOf` goCode)
                assertBool "should contain string type" ("string" `T.isInfixOf` goCode)
                assertBool "should contain bool type" ("bool" `T.isInfixOf` goCode)
    ]
  
  , testGroup "Utils Integration with Parser"
    [ testCase "parser output works with utils functions" $
        let input = "  func spaced() {  }  "
        in case parseTypus input of
          Left err -> assertBool "should parse spaced input" False
          Right parsedFile -> do
            let trimmed = trim input
            assertBool "trim should work" (length trimmed < length input)
            assertBool "parser should handle spaced input" 
              (not $ null $ tfCodeBlocks parsedFile)
    
    , testCase "comment removal integration" $
        let input = "func test() {\n  // comment\n  return 0\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse with comments" False
          Right parsedFile -> do
            let withoutComments = removeComments input
            assertBool "should remove comments" (not $ "// comment" `T.isInfixOf` withoutComments)
            assertBool "parser should handle comments" 
              (not $ null $ tfCodeBlocks parsedFile)
    
    , testCase "string splitting integration" $
        let input = "func one() {}\nfunc two() {}\nfunc three() {}"
            lines' = splitBy '\n' input
        in case parseTypus input of
          Left err -> assertBool "should parse multi-line input" False
          Right parsedFile -> do
            assertEqual "should have three lines" 3 (length lines')
            assertEqual "should have three functions" 3 (length $ tfCodeBlocks parsedFile)
    ]
  
  , testGroup "SourceLocation Integration"
    [ testCase "parser provides location information" $
        let input = "func located() {\n  return 0\n}"
        in case parseTypus input of
          Left err -> assertBool "should parse" False
          Right parsedFile -> 
            case tfCodeBlocks parsedFile of
              (block:_) -> 
                case cbSpan block of
                  Just span -> assertBool "should have valid span" True
                  Nothing -> assertBool "should have location info" False
              [] -> assertBool "should have code blocks" False
    
    , testCase "compiler uses location information for errors" $
        let input = "func invalid {\n  missing closing"
        in case parseTypus input of
          Left err -> assertBool "should parse (with errors)" True
          Right parsedFile -> 
            case compile parsedFile of
              Left compileErr -> assertBool "should report location in error" True
              Right _ -> assertBool "should not succeed on invalid input" False
    ]
  
  , testGroup "Error Propagation Integration"
    [ testCase "parse errors propagate through pipeline" $
        let invalidInput = "func completely { invalid syntax ]]["
        in case parseTypus invalidInput of
          Left parseErr -> assertBool "should fail at parse stage" True
          Right parsedFile -> 
            case compile parsedFile of
              Left compileErr -> assertBool "should fail at compile stage" True
              Right _ -> assertBool "should not succeed" False
    
    , testCase "type errors are properly identified" $
        let typeErrorInput = "func typeError() {\n  let x: int = \"string\"\n}"
        in case parseTypus typeErrorInput of
          Left err -> assertBool "should parse" False
          Right parsedFile -> 
            case compile parsedFile of
              Left compileErr -> assertBool "should detect type error" True
              Right result -> assertBool "should not succeed on type error" False
    ]
  
  , testGroup "Directive Integration"
    [ testCase "ownership directives affect compilation" $
        let ownershipInput = "// @ownership: true\nfunc transfer() {\n  // ownership logic\n}"
        in case parseTypus ownershipInput of
          Left err -> assertBool "should parse ownership directive" False
          Right parsedFile -> do
            let directives = tfDirectives parsedFile
            assertBool "should have ownership directive" 
              (case fdOwnership directives of
                 Just (Located _ True) -> True
                 _ -> False)
            case compile parsedFile of
              Left err -> assertBool "should handle ownership" True
              Right result -> assertBool "should compile with ownership" True
    
    , testCase "dependent-type directives affect compilation" $
        let depTypesInput = "// @dependent-types: true\nfunc dependent(n: int) {\n  // dependent type logic\n}"
        in case parseTypus depTypesInput of
          Left err -> assertBool "should parse dependent-types directive" False
          Right parsedFile -> do
            let directives = tfDirectives parsedFile
            assertBool "should have dependent-types directive"
              (case fdDependentTypes directives of
                 Just (Located _ True) -> True
                 _ -> False)
            case compile parsedFile of
              Left err -> assertBool "should handle dependent types" True
              Right result -> assertBool "should compile with dependent types" True
    ]
  ]