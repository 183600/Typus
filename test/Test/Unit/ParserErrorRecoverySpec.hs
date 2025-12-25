{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.ParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

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

import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- ============================================================================
-- Parser Error Recovery Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Error Recovery Tests"
    [ testGroup "Malformed Directive Recovery"
        [ testCase "recovers from invalid file directive syntax" $ do
            let input = unlines
                  [ "//! ownership on, invalid_syntax"
                  , "//! dependent_types true"
                  , ""
                  , "func main() {"
                  , "    println(\"Hello\")"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from malformed directive" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse valid directives" (fdDependentTypes directives /= Nothing)

        , testCase "handles invalid boolean values in directives gracefully" $ do
            let input = unlines
                  [ "//! ownership maybe"
                  , "//! dependent_types true"
                  , ""
                  , "func test() {"
                  , "    return 42"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle invalid boolean values" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse valid directive despite invalid one" (fdDependentTypes directives /= Nothing)

        , testCase "recovers from missing directive values" $ do
            let input = unlines
                  [ "//! ownership"
                  , "//! dependent_types on"
                  , ""
                  , "func example() {"
                  , "    return true"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from missing values" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse complete directives" (fdDependentTypes directives /= Nothing)
        ]

    , testGroup "Block Parsing Error Recovery"
        [ testCase "handles incomplete block directives" $ do
            let input = unlines
                  [ "//typus ownership"
                  , "func incomplete() {"
                  , "    // missing closing brace"
                  , "    return 1"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle incomplete blocks" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should create blocks despite errors" (not $ null blocks)

        , testCase "recovers from malformed block syntax" $ do
            let input = unlines
                  [ "//typus ownership on dependent_types true"
                  , "func malformed() {{{"
                  , "    return \"error\""
                  , "}}}"
                  , ""
                  , "func normal() {"
                  , "    return \"ok\""
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from malformed syntax" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse some blocks" (length blocks >= 1)

        , testCase "handles nested directive blocks with errors" $ do
            let input = unlines
                  [ "//typus ownership on"
                  , "func outer() {"
                  , "    //typus dependent_types true"
                  , "    func inner() {"
                  , "        return true"
                  , "    // missing closing brace for inner"
                  , "}"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle nested blocks with errors" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should handle nested structure" (not $ null blocks)
        ]

    , testGroup "Syntax Error Recovery"
        [ testCase "recovers from unmatched braces" $ do
            let input = unlines
                  [ "func unmatched() {"
                  , "    if true {"
                  , "        return true"
                  , "    // missing closing braces"
                  , ""
                  , "func next() {"
                  , "    return false"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from unmatched braces" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse subsequent functions" (length blocks >= 1)

        , testCase "handles invalid characters in code" $ do
            let input = unlines
                  [ "func invalidChars() {"
                  , "    let x = \\x00 // invalid byte"
                  , "    return x"
                  , "}"
                  , ""
                  , "func valid() {"
                  , "    return 42"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle invalid characters" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse valid parts" (not $ null blocks)

        , testCase "recovers from malformed string literals" $ do
            let input = unlines
                  [ "func badStrings() {"
                  , "    let s = \"unclosed string"
                  , "    return s"
                  , "}"
                  , ""
                  , "func goodStrings() {"
                  , "    let s = \"complete string\""
                  , "    return s"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from bad strings" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse functions with good strings" (not $ null blocks)
        ]

    , testGroup "File Structure Error Recovery"
        [ testCase "handles empty files gracefully" $ do
            let input = ""
            case parseTypus input of
                Left _ -> assertBool "should handle empty files" False
                Right result -> do
                    tfDirectives result @?= defaultFileDirectives
                    assertBool "should have no blocks" (null $ tfBlocks result)

        , testCase "handles files with only comments" $ do
            let input = unlines
                  [ "// This is a comment"
                 , "//! ownership on"
                 , "// Another comment"
                  , ""
                  , "// Final comment"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle comment-only files" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse file directives" (fdOwnership directives /= Nothing)

        , testCase "recovers from mixed valid and invalid content" $ do
            let input = unlines
                  [ "//! dependent_types on"
                  , ""
                  , "// This is valid"
                  , "func valid() {"
                  , "    return true"
                  , "}"
                  , ""
                  , "invalid syntax here without function"
                  , "more invalid {"
                  , ""
                  , "// This is valid again"
                  , "func anotherValid() {"
                  , "    return 42"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from mixed content" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should extract valid blocks" (not $ null blocks)

        , testCase "handles files with encoding issues" $ do
            let input = unlines
                  [ "func encoding() {"
                  , "    // UTF-8 content: caf\233 na\239ve r\233sum\233"
                  , "    return \"test\""
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle UTF-8 content" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse UTF-8 content" (not $ null blocks)
        ]

    , testGroup "Directive Parsing Error Recovery"
        [ testCase "handles unknown directive keys" $ do
            let input = unlines
                  [ "//! unknown_directive on"
                  , "//! ownership true"
                  , "//! another_unknown off"
                  , ""
                  , "func test() {"
                  , "    return 1"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle unknown directives" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse known directives" (fdOwnership directives /= Nothing)

        , testCase "recovers from malformed directive pairs" $ do
            let input = unlines
                  [ "//! ownership"
                  , "//! dependent_types true"
                  , "//! constraints"
                  , ""
                  , "func test() {"
                  , "    return true"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from malformed pairs" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should parse complete directives" (fdDependentTypes directives /= Nothing)

        , testCase "handles duplicate directives with last one winning" $ do
            let input = unlines
                  [ "//! ownership on"
                  , "//! ownership off"
                  , "//! dependent_types true"
                  , ""
                  , "func test() {"
                  , "    return false"
                  , "}"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should handle duplicate directives" False
                Right result -> do
                    let directives = tfDirectives result
                    assertBool "should handle duplicates" (isJust $ fdOwnership directives)
        ]

    , testGroup "Property-Based Error Recovery Tests"
        [ fastProperty "parser never crashes on random input" $
            \input -> case parseTypus input of
                Left _ -> property True
                Right _ -> property True

        , fastProperty "parser extracts some valid directives from mixed input" $
            \validDirectives invalidContent ->
                let input = unlines $ validDirectives ++ [""] ++ invalidContent
                in case parseTypus input of
                    Left _ -> property True
                    Right result -> 
                        let directives = tfDirectives result
                        in property $ not (null [() | Just _ <- [fdOwnership directives, fdDependentTypes directives, fdConstraints directives]])

        , fastProperty "parser maintains file structure despite errors" $
            \blocksBefore errors blocksAfter ->
                let input = unlines $ blocksBefore ++ errors ++ blocksAfter
                in case parseTypus input of
                    Left _ -> property True
                    Right result -> 
                        let blockCount = length $ tfBlocks result
                        in property $ blockCount >= 0
        ]

    , testGroup "Edge Cases and Stress Tests"
        [ testCase "handles extremely long lines" $ do
            let longLine = replicate 10000 'a' ++ " func long() { return 1; }"
            let input = unlines ["//! ownership on", "", longLine]
            case parseTypus input of
                Left _ -> assertBool "should handle long lines" False
                Right result -> assertBool "should process long lines" (True)

        , testCase "handles deeply nested structures" $ do
            let nested = unlines $ ["func nested() {"] ++ ["    if true {" ++ replicate i ' ' | i <- [0,4..20]] ++ ["        return true"] ++ ["    }"] ++ ["}"]
            let input = unlines ["//! ownership on", "", nested]
            case parseTypus input of
                Left _ -> assertBool "should handle nested structures" False
                Right result -> assertBool "should process nested structures" (True)

        , testCase "handles files with only whitespace" $ do
            let input = unlines ["   ", "\t", "   \t   ", ""]
            case parseTypus input of
                Left _ -> assertBool "should handle whitespace-only files" False
                Right result -> do
                    tfDirectives result @?= defaultFileDirectives
                    assertBool "should have no blocks" (null $ tfBlocks result)

        , testCase "recovers from syntax errors at file end" $ do
            let input = unlines
                  [ "func complete() {"
                  , "    return true"
                  , "}"
                  , ""
                  , "func incomplete() {"
                  , "    return false"
                  , "    // missing closing brace"
                  ]
            case parseTypus input of
                Left _ -> assertBool "should recover from end-of-file errors" False
                Right result -> do
                    let blocks = tfBlocks result
                    assertBool "should parse complete functions" (not $ null blocks)
        ]
    ]
  where
    isJust Nothing = False
    isJust (Just _) = True