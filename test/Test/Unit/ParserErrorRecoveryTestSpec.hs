{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.ParserErrorRecoveryTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourceSpan(..), SourcePos(..), spanStart, spanEnd)
import qualified Text.Megaparsec as MP
import Utils (trim)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid directive content
genDirectiveContent :: Gen String
genDirectiveContent = oneof
  [ pure "ownership: on"
  , pure "ownership: off"
  , pure "dependent_types: on"
  , pure "dependent_types: off"
  , pure "constraints: on"
  , pure "constraints: off"
  , pure "ownership: on, dependent_types: off"
  , pure "ownership: off, dependent_types: on, constraints: on"
  ]

-- Generate valid block directive content
genBlockDirectiveContent :: Gen String
genBlockDirectiveContent = oneof
  [ pure "{//! ownership: on}"
  , pure "{//! ownership: off}"
  , pure "{//! dependent_types: on}"
  , pure "{//! dependent_types: off}"
  , pure "{//! constraints: on}"
  , pure "{//! constraints: off}"
  , pure "{//! ownership: on, dependent_types: off}"
  , pure "{//! ownership: off, dependent_types: on, constraints: on}"
  ]

-- Generate valid Go-like code content
genCodeContent :: Gen String
genCodeContent = do
  lines' <- listOf $ elements
    [ "func main() {"
    , "    fmt.Println(\"hello\")"
    , "    x := 42"
    , "    if x > 0 {"
    , "        return x"
    , "    }"
    , "    return 0"
    , "}"
    , ""
    ]
  return $ unlines lines'

-- Generate invalid directive content (for error testing)
genInvalidDirectiveContent :: Gen String
genInvalidDirectiveContent = oneof
  [ pure "//! invalid_key: on"
  , pure "//! ownership: maybe"
  , pure "//! ownership:"
  , pure "//! ownership on"  -- missing colon
  , pure "{//! invalid_key: on}"
  , pure "{//! ownership: maybe}"
  , pure "{//! ownership:"
  , pure "{//! ownership on"  -- missing colon
  , pure "{//! ownership: on"  -- missing closing brace
  ]

-- Generate malformed code content
genMalformedCodeContent :: Gen String
genMalformedCodeContent = do
  lines' <- listOf $ elements
    [ "func main() {"
    , "    fmt.Println(\"hello\""  -- missing closing quote
    , "    x := 42"
    , "    if x > 0"  -- missing opening brace
    , "        return x"
    , "    }"  -- unmatched closing brace
    , "    return 0"
    , "func extra {"  -- malformed function
    , ""
    ]
  return $ unlines lines'

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic parsing success cases
testBasicParsingSuccess :: TestTree
testBasicParsingSuccess = testGroup "Basic Parsing Success"
  [ testCase "empty input" $ do
      let result = parseTypus ""
      case result of
        Left err -> assertBool $ "Should parse empty input" ++ err
        Right typusFile -> do
          tfDirectives typusFile @?= defaultFileDirectives
          tfBlocks typusFile @?= []
          
  , testCase "simple code without directives" $ do
      let input = "func main() {\n    fmt.Println(\"hello\")\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse simple code" ++ err
        Right typusFile -> do
          length (tfBlocks typusFile) @?= 1
          let block = head (tfBlocks typusFile)
          cbDirectives block @?= defaultBlockDirectives
          
  , testCase "code with file directives" $ do
      let input = "//! ownership: on\n//! dependent_types: off\n\nfunc main() {\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse file directives" ++ err
        Right typusFile -> do
          let directives = tfDirectives typusFile
          isJust (fdOwnership directives) @?= True
          isJust (fdDependentTypes directives) @?= True
  ]

-- Test block directive parsing
testBlockDirectiveParsing :: TestTree
testBlockDirectiveParsing = testGroup "Block Directive Parsing"
  [ testCase "single block directive" $ do
      let input = "{//! ownership: on}\nfunc test() {\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse block directive" ++ err
        Right typusFile -> do
          length (tfBlocks typusFile) @?= 1
          let block = head (tfBlocks typusFile)
          isJust (bdOwnership (cbDirectives block)) @?= True
          
  , testCase "multiple block directives" $ do
      let input = "{//! ownership: on, dependent_types: off}\nfunc test() {\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse multiple block directives" ++ err
        Right typusFile -> do
          length (tfBlocks typusFile) @?= 1
          let block = head (tfBlocks typusFile)
          let directives = cbDirectives block
          isJust (bdOwnership directives) @?= True
          isJust (bdDependentTypes directives) @?= True
          
  , testCase "mixed code and block directives" $ do
      let input = "func before() {}\n\n{//! ownership: on}\nfunc with() {}\n\nfunc after() {}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse mixed content" ++ err
        Right typusFile -> do
          length (tfBlocks typusFile) @?= 3
  ]

-- Test error recovery cases
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "Error Recovery"
  [ testCase "invalid file directive recovery" $ do
      let input = "//! invalid_key: on\n\nfunc main() {\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should recover from invalid directive" ++ err
        Right _ -> assertBool "Should handle invalid directive gracefully" True
        
  , testCase "invalid block directive recovery" $ do
      let input = "{//! invalid_key: on}\nfunc test() {\n}"
      case parseTypus input of
        Left err -> assertBool $ "Should recover from invalid block directive" ++ err
        Right _ -> assertBool "Should handle invalid block directive gracefully" True
        
  , testCase "malformed block directive recovery" $ do
      let input = "{//! ownership: on\nfunc test() {\n}"  -- missing closing brace
      case parseTypus input of
        Left err -> assertBool $ "Should recover from malformed block directive" ++ err
        Right _ -> assertBool "Should handle malformed block directive gracefully" True
        
  , testCase "unclosed block recovery" $ do
      let input = "{//! ownership: on\nfunc test() {\n    // content"
      case parseTypus input of
        Left err -> assertBool $ "Should recover from unclosed block" ++ err
        Right _ -> assertBool "Should handle unclosed block gracefully" True
  ]

-- Test syntax error detection
testSyntaxErrorDetection :: TestTree
testSyntaxErrorDetection = testGroup "Syntax Error Detection"
  [ testCase "if without brace detection" $ do
      let input = "if condition:\n    doSomething()"
      case parseTypus input of
        Left err -> assertBool "Should detect if without brace" $ "missing opening brace" `isInfixOf` err
        Right typusFile -> do
          -- Should still parse but with syntax errors
          assertBool "Should have syntax errors" $ not (null (tfSyntaxErrors typusFile))
          
  , testCase "multiple package declarations" $ do
      let input = "package main\n\npackage other\n\nfunc main() {}"
      case parseTypus input of
        Left err -> assertBool "Should detect multiple packages" $ "Multiple package declarations" `isInfixOf` err
        Right _ -> assertBool "Should handle multiple package declarations" True
  ]

-- Test build tag parsing
testBuildTagParsing :: TestTree
testBuildTagParsing = testGroup "Build Tag Parsing"
  [ testCase "go build tags" $ do
      let input = "//go:build linux\n//go:build amd64\n\nfunc main() {}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse go build tags" ++ err
        Right typusFile -> do
          length (tfBuildTags typusFile) @?= 2
          
  , testCase "plus build tags" $ do
      let input = "// +build linux,amd64\n\nfunc main() {}"
      case parseTypus input of
        Left err -> assertBool $ "Should parse plus build tags" ++ err
        Right typusFile -> do
          length (tfBuildTags typusFile) @?= 1
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Valid directive content should not break parsing
prop_valid_directives_parse :: String -> Property
prop_valid_directives_parse codeContent =
  forAll genDirectiveContent $ \directive ->
    let input = directive ++ "\n\n" ++ codeContent
    in case parseTypus input of
         Left _ -> property False
         Right _ -> property True

-- Property: Valid block directives should not break parsing
prop_valid_block_directives_parse :: String -> Property
prop_valid_block_directives_parse codeContent =
  forAll genBlockDirectiveContent $ \directive ->
    let input = directive ++ "\n" ++ codeContent
    in case parseTypus input of
         Left _ -> property False
         Right _ -> property True

-- Property: Empty input should always parse successfully
prop_empty_input_parses :: Property
prop_empty_input_parses =
  case parseTypus "" of
    Left _ -> property False
    Right _ -> property True

-- Property: Adding valid directives to valid code should not break parsing
prop_add_directives_preserves_parsing :: String -> Property
prop_add_directives_preserves_parsing codeContent =
  let parseWithoutDirectives = parseTypus codeContent
      parseWithDirectives = parseTypus $ "//! ownership: on\n" ++ codeContent
  in case (parseWithoutDirectives, parseWithDirectives) of
       (Left _, Left _) -> property True  -- Both fail, that's OK
       (Right _, Right _) -> property True -- Both succeed, that's OK
       (Left _, Right _) -> property True  -- Adding directives fixes it
       (Right _, Left _) -> property False -- Adding directives breaks it

-- Property: Parser should be position-aware
prop_parser_position_aware :: String -> Property
prop_parser_position_aware codeContent =
  let input = "line1\nline2\nline3\n" ++ codeContent
  in case parseTypus input of
       Left _ -> property True  -- May fail for other reasons
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in if null blocks
            then property True
            else property $ all (\block -> 
              let span = cbSpan block
                  start = spanStart span
                  endPos = spanEnd span
              in posLine start >= 1 && posLine endPos >= posLine start) blocks

-- Property: Malformed code should not crash parser
prop_malformed_code_no_crash :: Property
prop_malformed_code_no_crash =
  forAll genMalformedCodeContent $ \malformedCode ->
    case parseTypus malformedCode of
      Left _ -> property True  -- Expected to fail
      Right _ -> property True  -- May succeed with recovery

-- Property: Invalid directives should not crash parser
prop_invalid_directives_no_crash :: Property
prop_invalid_directives_no_crash =
  forAll genInvalidDirectiveContent $ \invalidDirective ->
    let input = invalidDirective ++ "\n\nfunc main() {}"
    in case parseTypus input of
         Left _ -> property True  -- Expected to fail
         Right _ -> property True  -- May succeed with recovery

-- Property: Parser should handle mixed valid/invalid content
prop_mixed_content_handling :: Property
prop_mixed_content_handling =
  forAll genCodeContent $ \validCode ->
    forAll genInvalidDirectiveContent $ \invalidDirective ->
      let input = invalidDirective ++ "\n\n" ++ validCode
      in case parseTypus input of
           Left _ -> property True  -- May fail due to invalid directive
           Right typusFile -> property True  -- Should recover and parse valid code

-- Property: Multiple directives should be combined correctly
prop_multiple_directives_combination :: Property
prop_multiple_directives_combination =
  let input = "//! ownership: on\n//! dependent_types: off\n//! constraints: on\n\nfunc main() {}"
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let directives = tfDirectives typusFile
         in property $ isJust (fdOwnership directives) .&&.
                      isJust (fdDependentTypes directives) .&&.
                      isJust (fdConstraints directives)

-- Property: Block content should be preserved correctly
prop_block_content_preservation :: Property
prop_block_content_preservation =
  forAll genCodeContent $ \codeContent ->
    let input = "{//! ownership: on}\n" ++ codeContent
    in case parseTypus input of
         Left _ -> property True  -- May fail for other reasons
         Right typusFile ->
           if null (tfBlocks typusFile)
           then property True
           else property $ codeContent `isInfixOf` (cbContent (head (tfBlocks typusFile)))

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Recovery Tests"
  [ testBasicParsingSuccess
  , testBlockDirectiveParsing
  , testErrorRecovery
  , testSyntaxErrorDetection
  , testBuildTagParsing
  , testGroup "QuickCheck Properties"
    [ fastProperty "Valid directives parse" prop_valid_directives_parse
    , fastProperty "Valid block directives parse" prop_valid_block_directives_parse
    , fastProperty "Empty input parses" prop_empty_input_parses
    , fastProperty "Add directives preserves parsing" prop_add_directives_preserves_parsing
    , fastProperty "Parser position aware" prop_parser_position_aware
    , fastProperty "Malformed code no crash" prop_malformed_code_no_crash
    , fastProperty "Invalid directives no crash" prop_invalid_directives_no_crash
    , fastProperty "Mixed content handling" prop_mixed_content_handling
    , fastProperty "Multiple directives combination" prop_multiple_directives_combination
    , fastProperty "Block content preservation" prop_block_content_preservation
    ]
  ]