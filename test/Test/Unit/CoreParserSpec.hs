{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Unit.CoreParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, spanFrom)
import qualified Text.Megaparsec as MP

-- | Test basic parser functionality
tests :: TestTree
tests = testGroup "Core Parser Tests"
  [ testFileDirectives
  , testBlockDirectives  
  , testCodeBlockParsing
  , testTypusFileStructure
  , testDirectiveParsing
  , testErrorHandling
  ]

-- | Test file directive parsing
testFileDirectives :: TestTree
testFileDirectives = testCase "File Directives Parsing" $ do
  let input = "// @ownership: true\n// @dependent-types: false\n"
      result = parseTypus input
  case result of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
      fmap locatedValue (fdOwnership directives) @?= Just True
      fmap locatedValue (fdDependentTypes directives) @?= Just False

-- | Test block directive parsing
testBlockDirectives :: TestTree
testBlockDirectives = testCase "Block Directives Parsing" $ do
  let input = "// @block-ownership: true\n// @block-dependent-types: true\n"
      result = parseTypus input
  case result of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right typusFile -> do
      let firstBlock = head $ tfBlocks typusFile
          directives = cbDirectives firstBlock
      fmap locatedValue (bdOwnership directives) @?= Just True
      fmap locatedValue (bdDependentTypes directives) @?= Just True

-- | Test code block parsing
testCodeBlockParsing :: TestTree
testCodeBlockParsing = testCase "Code Block Parsing" $ do
  let input = "func test() {\n  return 42\n}\n"
      result = parseTypus input
  case result of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      length blocks @?= 1
      let firstBlock = head blocks
      cbContent firstBlock @?= input

-- | Test complete Typus file structure
testTypusFileStructure :: TestTree
testTypusFileStructure = testCase "Complete Typus File Structure" $ do
  let input = "// @ownership: true\n\nfunc main() {\n  return 0\n}\n"
      result = parseTypus input
  case result of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right typusFile -> do
      length (tfBlocks typusFile) @?= 1

-- | Test directive parsing edge cases
testDirectiveParsing :: TestTree
testDirectiveParsing = testCase "Directive Parsing Edge Cases" $ do
  let malformedInput = "// @ownership: maybe\n"
      result = parseTypus malformedInput
  case result of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Expected parse failure for malformed directive"

-- | Test parser error handling
testErrorHandling :: TestTree
testErrorHandling = testCase "Parser Error Handling" $ do
  let emptyInput = ""
      result = parseTypus emptyInput
  case result of
    Left _ -> return () -- Empty input should fail gracefully
    Right typusFile -> do
      -- Should create empty file structure for empty input
      null (tfBlocks typusFile) @?= True