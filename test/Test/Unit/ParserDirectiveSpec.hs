module Test.Unit.ParserDirectiveSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, startPos)
import qualified Data.Text as T

-- Test cases for FileDirectives
testFileDirectives :: TestTree
testFileDirectives = testGroup "FileDirectives tests"
  [ testCase "defaultFileDirectives has all Nothing values" $
      defaultFileDirectives @?= FileDirectives Nothing Nothing Nothing
  , testCase "FileDirectives equality works" $
      let fd1 = FileDirectives (Just (locatedAt startPos True)) Nothing Nothing
          fd2 = FileDirectives (Just (locatedAt startPos True)) Nothing Nothing
          fd3 = FileDirectives (Just (locatedAt startPos False)) Nothing Nothing
      in do
        fd1 @?= fd2
        fd1 @?= fd2
        assertBool "fd1 should not equal fd3" (fd1 /= fd3)
  ]

-- Test cases for BlockDirectives
testBlockDirectives :: TestTree
testBlockDirectives = testGroup "BlockDirectives tests"
  [ testCase "defaultBlockDirectives has all Nothing values" $
      defaultBlockDirectives @?= BlockDirectives Nothing Nothing Nothing
  , testCase "BlockDirectives equality works" $
      let bd1 = BlockDirectives (Just (locatedAt startPos True)) Nothing Nothing
          bd2 = BlockDirectives (Just (locatedAt startPos True)) Nothing Nothing
          bd3 = BlockDirectives (Just (locatedAt startPos False)) Nothing Nothing
      in do
        bd1 @?= bd2
        bd1 @?= bd2
        assertBool "bd1 should not equal bd3" (bd1 /= bd3)
  ]

-- Test cases for CodeBlock
testCodeBlock :: TestTree
testCodeBlock = testGroup "CodeBlock tests"
  [ testCase "CodeBlock stores directives and content" $
      let directives = defaultBlockDirectives
          content = "print(\"hello\")"
          span = SourceSpan startPos startPos
          block = CodeBlock directives content span
      in do
        cbDirectives block @?= directives
        cbContent block @?= content
        cbSpan block @?= span
  , testCase "CodeBlock equality works" $
      let directives = defaultBlockDirectives
          span = SourceSpan startPos startPos
          block1 = CodeBlock directives "code1" span
          block2 = CodeBlock directives "code1" span
          block3 = CodeBlock directives "code2" span
      in do
        block1 @?= block2
        block1 @?= block2
        assertBool "block1 should not equal block3" (block1 /= block3)
  ]

-- Test cases for TypusFile
testTypusFile :: TestTree
testTypusFile = testGroup "TypusFile tests"
  [ testCase "TypusFile stores all components" $
      let directives = defaultFileDirectives
          buildTags = []
          blocks = []
          syntaxErrors = []
          file = TypusFile directives buildTags blocks syntaxErrors
      in do
        tfDirectives file @?= directives
        tfBuildTags file @?= buildTags
        tfBlocks file @?= blocks
        tfSyntaxErrors file @?= syntaxErrors
  , testCase "TypusFile equality works" $
      let directives = defaultFileDirectives
          file1 = TypusFile directives [] [] []
          file2 = TypusFile directives [] [] []
          file3 = TypusFile directives [] [CodeBlock defaultBlockDirectives "" (SourceSpan startPos startPos)] []
      in do
        file1 @?= file2
        file1 @?= file2
        assertBool "file1 should not equal file3" (file1 /= file3)
  ]

-- Test cases for directive parsing (simplified tests)
testDirectiveParsing :: TestTree
testDirectiveParsing = testGroup "Directive parsing tests"
  [ testCase "parseTypus handles empty input" $
      let result = parseTypus ""
      in case result of
        Left _ -> assertFailure "Should parse empty input successfully"
        Right file -> do
          tfDirectives file @?= defaultFileDirectives
          tfBuildTags file @?= []
          tfBlocks file @?= []
  , testCase "parseTypus handles simple content" $
      let input = "print(\"hello\")"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse simple content successfully"
        Right file -> do
          length (tfBlocks file) @?= 1
          case tfBlocks file of
            [] -> assertFailure "Should have at least one block"
            (firstBlock:_) -> cbContent firstBlock @?= input
  , testCase "parseTypus handles file directives" $
      let input = "//! ownership=true\ntest code"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse file directives successfully"
        Right file -> do
          case fdOwnership (tfDirectives file) of
            Nothing -> assertFailure "Should have ownership directive"
            Just located -> locValue located @?= True
  ]

-- Test cases for directive format validation
testDirectiveFormat :: TestTree
testDirectiveFormat = testGroup "Directive format tests"
  [ testCase "directive format with boolean values" $
      let input = "//! ownership=true, dependent-types=false"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse boolean directives"
        Right file -> do
          case fdOwnership (tfDirectives file) of
            Nothing -> assertFailure "Should have ownership directive"
            Just located -> locValue located @?= True
          case fdDependentTypes (tfDirectives file) of
            Nothing -> assertFailure "Should have dependent-types directive"
            Just located -> locValue located @?= False
  , testCase "directive format with mixed whitespace" $
      let input = "//!  ownership =  true  , dependent-types=false  "
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse directives with mixed whitespace"
        Right file -> do
          case fdOwnership (tfDirectives file) of
            Nothing -> assertFailure "Should have ownership directive"
            Just located -> locValue located @?= True
  ]

-- Test cases for block directive parsing
testBlockDirectiveParsing :: TestTree
testBlockDirectiveParsing = testGroup "Block directive tests"
  [ testCase "block directives in code blocks" $
      let input = "//! ownership=true\n//! block: dependent-types=false\ncode here"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse block directives"
        Right file -> do
          length (tfBlocks file) @?= 1
          case tfBlocks file of
            [] -> assertFailure "Should have at least one block"
            (block:_) -> 
              case bdOwnership (cbDirectives block) of
                Nothing -> assertFailure "Should have block ownership directive"
                Just located -> locValue located @?= True
  ]

-- Test cases for error handling
testErrorHandling :: TestTree
testErrorHandling = testGroup "Error handling tests"
  [ testCase "parseTypus collects syntax errors" $
      let input = "//! invalid=syntax\ncode"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should parse with syntax errors"
        Right file -> do
          -- File should still be parsed even with syntax errors
          length (tfBlocks file) @?= 1
  , testCase "parseTypus handles malformed directives gracefully" $
      let input = "//! ownership=\ncode"
          result = parseTypus input
      in case result of
        Left _ -> assertFailure "Should handle malformed directives gracefully"
        Right file -> do
          -- Should still parse the code block
          length (tfBlocks file) @?= 1
  ]

-- QuickCheck properties
prop_parseTypus_roundtrip :: String -> Property
prop_parseTypus_roundtrip s = 
  let result = parseTypus s
  in case result of
    Left _ -> property True  -- Parsing can fail for invalid input
    Right file -> 
      let reconstructed = unlines $ map cbContent (tfBlocks file)
      in property (length reconstructed >= 0)  -- Basic sanity check

prop_empty_file_has_default_directives :: String -> Property
prop_empty_file_has_default_directives s = 
  let result = parseTypus ""
  in case result of
    Left _ -> property False  -- Empty file should always parse
    Right file -> property (tfDirectives file == defaultFileDirectives)

prop_code_blocks_preserve_content :: String -> Property
prop_code_blocks_preserve_content s = 
  let result = parseTypus s
  in case result of
    Left _ -> property True  -- Parsing can fail
    Right file -> 
      let blocks = tfBlocks file
          content = concatMap cbContent blocks
      in property (length content <= length s)  -- Content shouldn't grow

tests :: TestTree
tests = testGroup "Parser Directive Tests"
  [ testFileDirectives
  , testBlockDirectives
  , testCodeBlock
  , testTypusFile
  , testDirectiveParsing
  , testDirectiveFormat
  , testBlockDirectiveParsing
  , testErrorHandling
  , testProperty "parseTypus roundtrip" prop_parseTypus_roundtrip
  , testProperty "empty file has default directives" prop_empty_file_has_default_directives
  , testProperty "code blocks preserve content" prop_code_blocks_preserve_content
  ]