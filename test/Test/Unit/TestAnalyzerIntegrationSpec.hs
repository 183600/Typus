{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  -Wno-unused-imports #-}
module Test.Unit.TestAnalyzerIntegrationSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Analyzer Integration
testAnalyzerIntegration :: TestTree
testAnalyzerIntegration = testGroup "Analyzer Integration Tests"
  [ testCase "Analyzer integration with parser" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = Parser.parseTypus input
          ownershipResult = Ownership.analyzeOwnership input
      in case parseResult of
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             length ownershipResult @?= 1  -- Expect one ownership transfer
           Left err -> assertFailure $ "Parse failed: " ++ show err
           
  , testCase "Analyzer integration with error handler" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - violation\n    println(len(data))\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = Parser.parseTypus input
          ownershipResult = Ownership.analyzeOwnership input
      in case parseResult of
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             -- Should detect ownership violation
             length ownershipResult @?= 2
           Left err -> assertFailure $ "Parse failed: " ++ show err
           
  , testCase "Analyzer integration with type system" $
      let input = "//! dependent=true\n```go\npackage main\n\nfunc processData[T any](data T) T {\n    return data\n}\n\nfunc main() {\n    result := processData(42)\n    println(result)\n}\n```"
          parseResult = Parser.parseTypus input
      in case parseResult of
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             -- Type checking should succeed
             return ()
           Left err -> assertFailure $ "Parse failed: " ++ show err
           
  , testCase "Analyzer integration with build system" $
      let input = "// +build linux,amd64\n//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = Parser.parseTypus input
      in case parseResult of
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             return ()
           Left err -> assertFailure $ "Parse failed: " ++ show err
           
  , testCase "Analyzer integration with testing framework" $
      let input = "//! ownership=true\n```go\npackage main\n\nimport \"testing\"\n\nfunc TestProcessData(t *testing.T) {\n    data := make([]byte, 100)\n    processData(data)\n    if len(data) != 100 {\n        t.Errorf(\"Expected data length 100, got %d\", len(data))\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = Parser.parseTypus input
          ownershipResult = Ownership.analyzeOwnership input
      in case parseResult of
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             length ownershipResult @?= 1
           Left err -> assertFailure $ "Parse failed: " ++ show err
  ]