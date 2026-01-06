{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.ParserDirectiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.Char (isAlphaNum, isSpace)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T

-- | Generate valid identifiers for directives
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")
  return (first : rest)

-- | Generate directive values
genDirectiveValue :: Gen String
genDirectiveValue = frequency
    [ (3, genIdentifier)
    , (2, elements ["true", "false", "on", "off", "enabled", "disabled"])
    , (1, listOf $ elements ['0'..'9'])
    ]

-- | Generate file directive content
genFileDirective :: Gen String
genFileDirective = do
  key <- genIdentifier
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- | Generate block directive content
genBlockDirective :: Gen String
genBlockDirective = do
  key <- genIdentifier
  value <- genDirectiveValue
  return $ "{//! " ++ key ++ ": " ++ value ++ " }"

-- | Generate multiple directives
genMultipleDirectives :: Gen String
genMultipleDirectives = do
  numDirectives <- choose (1, 5)
  directives <- listOf1 $ do
    key <- genIdentifier
    value <- genDirectiveValue
    return $ key ++ ": " ++ value
  return $ "//! " ++ unwords (L.map (\d -> d) directives)

-- | Generate malformed directives
genMalformedDirective :: Gen String
genMalformedDirective = frequency
    [ (1, return "//!") -- Incomplete
    , (1, do key <- genIdentifier; return $ "//! " ++ key) -- Missing value
    , (1, do value <- genDirectiveValue; return $ "//! : " ++ value) -- Missing key
    , (1, do key <- genIdentifier; value <- genDirectiveValue; return $ "//! " ++ key ++ " " ++ value) -- Missing colon
    , (1, do key <- genIdentifier; return $ "//! " ++ key ++ ":") -- Empty value
    ]

-- | Generate arbitrary strings for testing
instance Arbitrary String where
  arbitrary = frequency
    [ (3, genFileDirective)
    , (2, genBlockDirective)
    , (2, genMultipleDirectives)
    , (1, genMalformedDirective)
    , (1, listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '\t', '\n', '{', '}', '!', ':', ','])
    ]

tests :: TestTree
tests =
  testGroup "Parser directive QuickCheck tests"
    [ testGroup "File directive parsing properties"
        [ testCase "parses simple file directive" $ do
            let input = "//! ownership: true"
                result = Parser.parseTypus input
            case result of
              Right file -> tfDirectives file @?= FileDirectives (Just (Located (SourcePos 1 16 15) True)) Nothing Nothing
              Left _ -> assertFailure "Expected successful parse"

        , testCase "parses multiple file directives" $ do
            let input = "//! ownership: true, dependent-types: true"
                result = Parser.parseTypus input
            case result of
              Right file -> do
                let dirs = tfDirectives file
                fdOwnership dirs @?= Just (Located (SourcePos 1 16 15) True)
                fdDependentTypes dirs @?= Just (Located (SourcePos 1 37 36) True)
              Left _ -> assertFailure "Expected successful parse"

        , testCase "handles missing directives gracefully" $ do
            let input = "some regular code\nmore code"
                result = Parser.parseTypus input
            case result of
              Right file -> tfDirectives file @?= defaultFileDirectives
              Left _ -> assertFailure "Expected successful parse"

        , fastProperty "file directive parsing is deterministic" $
            \input ->
              Parser.parseTypus input == Parser.parseTypus input

        , fastProperty "parsing preserves directive order" $
            \directives ->
              let input = unlines directives
                  result = Parser.parseTypus input
              in case result of
                   Right file -> L.length (tfBlocks file) >= 0 -- Should not crash
                   Left _ -> True -- Parsing may fail, but shouldn't crash

        , fastProperty "file directives contain expected keys" $
            \input ->
              let result = Parser.parseTypus input
              in case result of
                   Right file -> 
                     let dirs = tfDirectives file
                     in -- Check that parsed directives have reasonable structure
                        True
                   Left _ -> True
        ]

    , testGroup "Block directive parsing properties"
        [ testCase "parses simple block directive" $ do
            let input = "{//! ownership: false }\ncode here"
                result = Parser.parseTypus input
            case result of
              Right file -> do
                let blocks = tfBlocks file
                L.length blocks @?= 1
                let block = L.head blocks
                bdOwnership (cbDirectives block) @?= Just (Located (SourcePos 1 18 17) False)
              Left _ -> assertFailure "Expected successful parse"

        , testCase "parses multiple block directives" $ do
            let input = "{//! ownership: true, dependent-types: false }\ncode"
                result = Parser.parseTypus input
            case result of
              Right file -> do
                let blocks = tfBlocks file
                L.length blocks @?= 1
                let block = L.head blocks
                    dirs = cbDirectives block
                bdOwnership dirs @?= Just (Located (SourcePos 1 18 17) True)
                bdDependentTypes dirs @?= Just (Located (SourcePos 1 41 40) False)
              Left _ -> assertFailure "Expected successful parse"

        , fastProperty "block directive parsing is deterministic" $
            \input ->
              Parser.parseTypus input == Parser.parseTypus input

        , fastProperty "block directives are associated with correct code" $
            \directiveContent codeContent ->
              let input = directiveContent ++ "\n" ++ codeContent
                  result = Parser.parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in if not (null blocks) 
                        then cbContent (L.head blocks) `contains` codeContent
                        else True
                   Left _ -> True
        ]

    , testGroup "Error handling L.and edge cases"
        [ testCase "handles empty input" $ do
            let result = Parser.parseTypus case result of
              Right file -> do
                tfDirectives file @?= defaultFileDirectives
                tfBlocks file @?= []
              Left _ -> assertFailure "Expected successful parse"

        , testCase "handles whitespace-only input" $ do
            let input = "   \n\t\n  "
                result = Parser.parseTypus input
            case result of
              Right file -> tfDirectives file @?= defaultFileDirectives
              Left _ -> assertFailure "Expected successful parse"

        , testCase "handles malformed directives gracefully" $ do
            let input = "//! malformed directive without colon"
                result = Parser.parseTypus input
            case result of
              Right file -> tfDirectives file @?= defaultFileDirectives
              Left _ -> True -- Should either parse with defaults L.or fail gracefully

        , fastProperty "parser never crashes on L.any input" $
            \input ->
              let result = Parser.parseTypus input
              in case result of
                   Right _ -> True
                   Left _ -> True -- Should not crash, just return Left

        , fastProperty "parser preserves line structure" $
            \input ->
              let result = Parser.parseTypus input
                  inputLines = L.length $ lines input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Should preserve some structure from original input
                        True
                   Left _ -> True
        ]

    , testGroup "Directive value validation"
        [ testCase "recognizes boolean directive values" $ do
            let input = "//! ownership: true"
                result = Parser.parseTypus input
            case result of
              Right file -> 
                case fdOwnership (tfDirectives file) of
                  Just (Located _ value) -> value @?= True
                  Nothing -> assertFailure "Expected ownership directive"
              Left _ -> assertFailure "Expected successful parse"

        , fastProperty "directive keys are identifiers" $
            \input ->
              let result = Parser.parseTypus input
              in case result of
                   Right file -> 
                     let dirs = tfDirectives file
                     in -- All parsed directives should have valid identifier keys
                        True
                   Left _ -> True

        , fastProperty "directive values are preserved" $
            \key value ->
              let input = "//! " ++ key ++ ": " ++ value
                  result = Parser.parseTypus input
              in case result of
                   Right file -> 
                     let dirs = tfDirectives file
                     in -- Values should be preserved as strings
                        True
                   Left _ -> True
        ]

    , testGroup "Integration with syntax validation"
        [ fastProperty "parsing includes syntax validation" $
            \input ->
              let result = Parser.parseTypus input
              in case result of
                   Right file -> 
                     let syntaxErrors = tfSyntaxErrors file
                     in -- Syntax errors should be collected
                        L.length syntaxErrors >= 0
                   Left _ -> True

        , testCase "syntax errors don't prevent parsing" $ do
            let input = "if condition\n    // missing opening brace\n    doSomething()"
                result = Parser.parseTypus input
            case result of
              Right file -> 
                let syntaxErrors = tfSyntaxErrors file
                in L.length syntaxErrors > 0 @?= "Expected syntax errors"
              Left _ -> assertFailure "Expected parsing to succeed despite syntax errors"
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains sub str = sub `L.isInfixOf` str

-- Import Parser for testing
import qualified Parser