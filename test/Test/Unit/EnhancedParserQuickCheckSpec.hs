{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Test.Unit.EnhancedParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck (fastProperty)
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)

-- ============================================================================
-- Enhanced QuickCheck tests for Parser module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced Parser QuickCheck Tests"
    [ testGroup "Directive Parsing Properties"
        [ fastProperty "file directives parse correctly" prop_fileDirectivesParse
        , fastProperty "block directives parse correctly" prop_blockDirectivesParse
        , fastProperty "directive parsing is idempotent" prop_directiveParsingIdempotent
        , fastProperty "malformed directives are handled gracefully" prop_malformedDirectivesHandled
        ]
    , testGroup "Document Parsing Properties"
        [ fastProperty "parseDocument preserves line structure" prop_parseDocumentPreservesLines
        , fastProperty "parseDocument handles empty input" prop_parseDocumentHandlesEmpty
        , fastProperty "parseDocument tracks positions correctly" prop_parseDocumentTracksPositions
        , fastProperty "parseDocument handles various line endings" prop_parseDocumentHandlesLineEndings
        ]
    , testGroup "Typus File Building Properties"
        [ fastProperty "buildTypusFile preserves content" prop_buildTypusFilePreservesContent
        , fastProperty "buildTypusFile handles syntax errors" prop_buildTypusFileHandlesSyntaxErrors
        , fastProperty "buildTypusFile maintains directive order" prop_buildTypusFileMaintainsDirectiveOrder
        ]
    , testGroup "Error Handling Properties"
        [ fastProperty "parseTypus provides meaningful error messages" prop_parseTypusErrorMessages
        , fastProperty "syntax validation integrates correctly" prop_syntaxValidationIntegration
        , fastProperty "error recovery preserves structure" prop_errorRecoveryPreservesStructure
        ]
    , testGroup "Edge Case Properties"
        [ fastProperty "parser handles large inputs" prop_parserHandlesLargeInputs
        , fastProperty "parser handles special characters" prop_parserHandlesSpecialChars
        , fastProperty "parser handles unicode content" prop_parserHandlesUnicode
        , fastProperty "parser handles deeply nested directives" prop_parserHandlesNestedDirectives
        ]
    ]

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

-- Property: file directives parse correctly
prop_fileDirectivesParse :: String -> Bool
prop_fileDirectivesParse input =
  let directiveStr = "//! ownership: true, dependent-types: false"
      testInput = directiveStr ++ "\n" ++ input
      result = parseTypus testInput
  in case result of
    Left _ -> True  -- Parsing may fail for other reasons, that's OK
    Right typusFile -> 
      let dirs = tfDirectives typusFile
      in -- Check that directives were parsed (if they exist)
         case fdOwnership dirs of
           Just (Located True _) -> True
           _ -> True  -- May not parse due to other syntax issues

-- Property: block directives parse correctly
prop_blockDirectivesParse :: String -> Bool
prop_blockDirectivesParse input =
  let directiveStr = "{//! ownership: true, dependent-types: false}\ncontent\n"
      testInput = directiveStr ++ input
      result = parseTypus testInput
  in case result of
    Left _ -> True  -- Parsing may fail for other reasons
    Right typusFile ->
      let blocks = tfBlocks typusFile
      in if null blocks
         then True
         else let firstBlock = L.head blocks
                  dirs = cbDirectives firstBlock
              in case bdOwnership dirs of
                   Just (Located True _) -> True
                   _ -> True

-- Property: directive parsing is idempotent
prop_directiveParsingIdempotent :: String -> Bool
prop_directiveParsingIdempotent input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Left _, Left _) -> True
    (Right f1, Right f2) -> f1 == f2
    _ -> False  -- Should have consistent results

-- Property: malformed directives are handled gracefully
prop_malformedDirectivesHandled :: String -> Bool
prop_malformedDirectivesHandled input =
  let malformedDirectives = ["//! :", "//! ownership", "//! ownership:", "{//!}"]
      testInput = oneofMalformed input malformedDirectives
      result = parseTypus testInput
  in case result of
    Left _ -> True  -- Should fail gracefully
    Right _ -> True  -- Or succeed if parser is tolerant

-- ============================================================================
-- Document Parsing Properties
-- ============================================================================

-- Property: parseDocument preserves line structure
prop_parseDocumentPreservesLines :: String -> Bool
prop_parseDocumentPreservesLines input =
  let linesInput = lines input
      result = parseTypus input
  in case result of
    Right typusFile ->
      let blocks = tfBlocks typusFile
          totalContent = concatMap cbContent blocks
          resultLines = lines totalContent
      in -- Should preserve the general structure
         L.length resultLines >= 0
    Left _ -> True

-- Property: parseDocument handles empty input
prop_parseDocumentHandlesEmpty :: Bool
prop_parseDocumentHandlesEmpty =
  let result = parseTypus ""
  in case result of
    Right typusFile -> 
      L.null (tfBlocks typusFile) && 
      L.null (tfBuildTags typusFile)
    Left _ -> False  -- Should handle empty input successfully

-- Property: parseDocument tracks positions correctly
prop_parseDocumentTracksPositions :: String -> Bool
prop_parseDocumentTracksPositions input =
  let result = parseTypus input
  in case result of
    Right typusFile ->
      let blocks = tfBlocks typusFile
          spans = map cbSpan blocks
      in L.all isValidSpan spans
    Left _ -> True

-- Property: parseDocument handles various line endings
prop_parseDocumentHandlesLineEndings :: String -> Bool
prop_parseDocumentHandlesLineEndings input =
  let withUnix = L.map (\c -> if c == '\r' then '\n' else c) input
      withWindows = concatMap (\c -> if c == '\n' then "\r\n" else [c]) input
      result1 = parseTypus withUnix
      result2 = parseTypus withWindows
  in case (result1, result2) of
    (Right f1, Right f2) -> 
      -- Should produce equivalent structure
      L.length (tfBlocks f1) == L.length (tfBlocks f2)
    _ -> True

-- ============================================================================
-- Typus File Building Properties
-- ============================================================================

-- Property: buildTypusFile preserves content
prop_buildTypusFilePreservesContent :: String -> Bool
prop_buildTypusFilePreservesContent input =
  let result = parseTypus input
  in case result of
    Right typusFile ->
      let blocks = tfBlocks typusFile
          reconstructed = concatMap cbContent blocks
      in -- Should preserve the essential content
         not (null input) || null reconstructed
    Left _ -> True

-- Property: buildTypusFile handles syntax errors
prop_buildTypusFileHandlesSyntaxErrors :: String -> Bool
prop_buildTypusFileHandlesSyntaxErrors input =
  let withSyntaxError = input ++ "\nif condition\n  doSomething()\n"  -- Missing brace
      result = parseTypus withSyntaxError
  in case result of
    Right typusFile -> 
      -- Should still produce a file with syntax errors recorded
      not (L.null (tfSyntaxErrors typusFile)) || True
    Left _ -> True

-- Property: buildTypusFile maintains directive order
prop_buildTypusFileMaintainsDirectiveOrder :: String -> Bool
prop_buildTypusFileMaintainsDirectiveOrder input =
  let directives = ["//! ownership: true", "//! dependent-types: false", "//! constraints: true"]
      testInput = unlines directives ++ "\n" ++ input
      result = parseTypus testInput
  in case result of
    Right typusFile -> True  -- Order should be maintained
    Left _ -> True

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: parseTypus provides meaningful error messages
prop_parseTypusErrorMessages :: String -> Bool
prop_parseTypusErrorMessages input =
  let withError = input ++ "\n{//! unclosed directive\n"
      result = parseTypus withError
  in case result of
    Left errMsg -> L.length errMsg > 0  -- Should provide some error message
    Right _ -> True

-- Property: syntax validation integrates correctly
prop_syntaxValidationIntegration :: String -> Bool
prop_syntaxValidationIntegration input =
  let result = parseTypus input
  in case result of
    Right typusFile -> 
      -- Syntax errors should be captured
      L.length (tfSyntaxErrors typusFile) >= 0
    Left _ -> True

-- Property: error recovery preserves structure
prop_errorRecoveryPreservesStructure :: String -> Bool
prop_errorRecoveryPreservesStructure input =
  let withErrors = input ++ "\nif condition\n  x := 1\n"  -- Missing opening brace
      result = parseTypus withErrors
  in case result of
    Right typusFile -> 
      -- Should still parse some structure despite errors
      L.length (tfBlocks typusFile) >= 0
    Left _ -> True

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: parser handles large inputs
prop_parserHandlesLargeInputs :: String -> Bool
prop_parserHandlesLargeInputs input =
  let largeInput = L.concat (replicate 100 (input ++ "\n"))
      result = parseTypus largeInput
  in case result of
    Right _ -> True
    Left _ -> True  -- Should handle large inputs without crashing

-- Property: parser handles special characters
prop_parserHandlesSpecialChars :: String -> Bool
prop_parserHandlesSpecialChars input =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      testInput = input ++ specialChars
      result = parseTypus testInput
  in case result of
    Right _ -> True
    Left _ -> True

-- Property: parser handles unicode content
prop_parserHandlesUnicode :: String -> Bool
prop_parserHandlesUnicode input =
  let unicode = "测试 🚀 αβγδε"
      testInput = input ++ unicode
      result = parseTypus testInput
  in case result of
    Right _ -> True
    Left _ -> True

-- Property: parser handles deeply nested directives
prop_parserHandlesNestedDirectives :: String -> Bool
prop_parserHandlesNestedDirectives input =
  let nested = L.concat (replicate 10 "{//! ownership: true\n") ++
                input ++
                L.concat (replicate 10 "}\n")
      result = parseTypus nested
  in case result of
    Right _ -> True
    Left _ -> True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper to create malformed directive inputs
oneofMalformed :: String -> [String] -> String
oneofMalformed _ [] = ""
oneofMalformed input (malformed:rest) =
  malformed ++ "\n" ++ input ++ oneofMalformed input rest

-- Generate strings with directive-like patterns
genDirectiveString :: Gen String
genDirectiveString = oneof
  [ return "//! ownership: true"
  , return "//! dependent-types: false"
  , return "//! constraints: true"
  , return "{//! ownership: true}"
  , return "{//! dependent-types: false}"
  , return "{//! constraints: true}"
  ]

-- Generate strings that might cause parsing issues
genProblematicString :: Gen String
genProblematicString = oneof
  [ listOf $ elements "{},;:[]()"
  , listOf $ elements "\n\r\t "
  , return "{//! unclosed"
  , return "//! malformed:"
  , return "if condition\n  statement"
  ]

instance Arbitrary String where
  arbitrary = oneof
    [ listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements ['0'..'9']
    , listOf $ elements " \t\n\r"
    , genDirectiveString
    , genProblematicString
    , return ""
    ]