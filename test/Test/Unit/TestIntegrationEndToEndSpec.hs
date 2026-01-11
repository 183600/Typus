{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestIntegrationEndToEndSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for End-to-End Integration
testIntegrationEndToEnd :: TestTree
testIntegrationEndToEnd = testGroup "End-to-End Integration Tests"
  [ testCase "Parse simple Go code with ownership" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1
           
  , testCase "Parse Go code with dependent types" $
      let input = "//! dependent=true\n```go\npackage main\n\nfunc processData[T any](data T) T {\n    return data\n}\n\nfunc main() {\n    result := processData(42)\n    println(result)\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1
           
  , testCase "Parse Go code with constraints" $
      let input = "//! constraints=true\n```go\npackage main\n\ntype Addable interface {\n    Add(other Addable) Addable\n}\n\ntype Number struct {\n    value int\n}\n\nfunc (n Number) Add(other Number) Number {\n    return Number{n.value + other.value}\n}\n\nfunc main() {\n    a := Number{10}\n    b := Number{20}\n    c := a.Add(b)\n    println(c.value)\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1
           
  , testCase "Parse Go code with multiple blocks" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc processData1(data []byte) {\n    // Process data\n}\n```\n\n```go\nfunc processData2(data []byte) {\n    // Process data\n}\n```\n\n```go\nfunc main() {\n    data := make([]byte, 100)\n    processData1(data)\n    processData2(data)\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 3
           
  , testCase "Parse Go code with error handling" $
      let input = "//! ownership=true\n```go\npackage main\n\nimport \"errors\"\n\nfunc processData(data []byte) error {\n    if len(data) == 0 {\n        return errors.New(\"empty data\")\n    }\n    // Process data\n    return nil\n}\n\nfunc main() {\n    data := make([]byte, 100)\n    err := processData(data)\n    if err != nil {\n        println(\"Error:\", err)\n    }\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1
           
  , testCase "Parse Go code with ownership violations" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc processData(data []byte) {\n    // Process data\n}\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - violation\n    println(len(data))\n}\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             length (Parser.tfBlocks typusFile) @?= 1
             -- Ownership analyzer should detect violation
             return ()
  ]

-- Helper types and functions for testing
data TestTypeError = TestTypeError 
  { testErrorMessage :: String
  , testErrorLocation :: TestErrorLocation
  , testErrorSuggestions :: [String]
  }

data TestErrorLocation = TestErrorLocation 
  { testLine :: Int
  , testColumn :: Int
  }

posAt :: Int -> Int -> SourceLocation.SourcePos
posAt line column = SourceLocation.SourcePos line column 0

errorAt :: SourceLocation.SourcePos -> String -> TestTypeError
errorAt pos message = TestTypeError message (TestErrorLocation (SourceLocation.posLine pos) (SourceLocation.posColumn pos)) []

formatError :: TestTypeError -> String
formatError err = testErrorMessage err ++ 
                  (if null (testErrorSuggestions err) 
                     then "" 
                     else "\nSuggestions: " ++ unwords (testErrorSuggestions err))

withSuggestions :: [String] -> TestTypeError -> TestTypeError
withSuggestions suggestions err = err { testErrorSuggestions = suggestions }