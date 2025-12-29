{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf, Positive(..), NonNegative(..))

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), SourcePos(..), startPos)
import qualified SyntaxValidator
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- | 新的QuickCheck属性测试，针对Parser模块的错误恢复
tests :: TestTree
tests =
  testGroup "New Cabal Parser QuickCheck Tests"
    [ testGroup "Basic parsing properties"
        [ fastProperty "Empty input creates valid TypusFile" $
            \input ->
              input == "" ==>
              case parseTypus input of
                Left _ -> property False
                Right file -> tfDirectives file === defaultFileDirectives .&&.
                             null (tfBlocks file) .&&.
                             null (tfBuildTags file)

        , fastProperty "Valid directives are parsed correctly" $
            \directives ->
              let input = "//!" ++ directives
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> not (null (tfDirectives file)) || length (tfSyntaxErrors file) > 0

        , fastProperty "Multiple build tags are preserved" $
            \tags ->
              let input = unlines $ map (\tag -> "//go:build " ++ tag) tags
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfBuildTags file) === length tags
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "Parser recovers from syntax errors" $
            \prefix error suffix ->
              let input = prefix ++ "\n" ++ error ++ "\n" ++ suffix
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfSyntaxErrors file) > 0 ==> not (null (tfBlocks file))

        , fastProperty "Partial parsing continues after errors" $
            \validContent errorContent ->
              let input = validContent ++ "\n" ++ errorContent ++ "\nfunc main() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfSyntaxErrors file) > 0 ==> 
                                any (isPrefixOf "func" . cbContent) (tfBlocks file)

        , fastProperty "Malformed directives don't crash parser" $
            \directives ->
              let input = "//!" ++ directives ++ "\nfunc test() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True  -- Parser should not crash

        , fastProperty "Unclosed blocks are handled gracefully" $
            \content ->
              let input = "{//!\n" ++ content ++ "\nfunc test() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True
        ]

    , testGroup "Directive parsing robustness"
        [ fastProperty "Mixed valid and invalid directives" $
            \valid invalid ->
              let input = "//!" ++ valid ++ "\n//! " ++ invalid ++ "\nfunc main() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True  -- Should parse some parts

        , fastProperty "Duplicate directives are handled" $
            \directive ->
              let input = "//!" ++ directive ++ "\n//! " ++ directive ++ "\nfunc test() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True

        , fastProperty "Directive values with special characters" $
            \value ->
              let input = "//!ownership:" ++ value ++ "\nfunc main() {}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True

        , fastProperty "Block directives with various formats" $
            \content ->
              let input = "{//! ownership:true }\n" ++ content ++ "\n}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True
        ]

    , testGroup "Content parsing properties"
        [ fastProperty "Code blocks preserve content" $
            \content ->
              let input = "func test() {\n" ++ content ++ "\n}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> any (\block -> content `isInfixOf` cbContent block) (tfBlocks file)

        , fastProperty "Multiple code blocks are parsed" $
            \blocks ->
              let blockContents = map (\i -> "func block" ++ show i ++ "() {}") [1..blocks]
                  input = unlines blockContents
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfBlocks file) >= blocks

        , fastProperty "Nested structures are handled" $
            \depth ->
              depth < 10 ==>
              let nested = concat $ replicate depth "  "
                  input = nested ++ "func test() {\n" ++ nested ++ "  return 42\n" ++ nested ++ "}"
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> not (null (tfBlocks file))
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "Empty file parsing" $ do
            let input = ""
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should parse empty file: " ++ err
              Right file -> do
                tfDirectives file @?= defaultFileDirectives
                tfBuildTags file @?= []
                tfBlocks file @?= []

        , testCase "File with only comments" $ do
            let input = unlines ["// This is a comment", "// Another comment", "//! ownership:true"]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should parse comments: " ++ err
              Right file -> do
                length (tfSyntaxErrors file) @?= 0
                tfBlocks file @?= []

        , testCase "Malformed directive recovery" $ do
            let input = unlines ["//! ownership", "//! invalid:syntax", "func main() {}"]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should recover from malformed directive: " ++ err
              Right file -> do
                -- Should have parsed the function despite malformed directive
                any (isPrefixOf "func main" . cbContent) (tfBlocks file) @?= True

        , testCase "Unclosed block directive" $ do
            let input = unlines ["{//! ownership:true", "func test() {}", "// Should close here"]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should handle unclosed block: " ++ err
              Right file -> do
                -- Should still parse some content
                not (null (tfBlocks file)) @?= True

        , testCase "Multiple package declarations error" $ do
            let input = unlines ["package main", "package test", "func main() {}"]
                result = parseTypus input
            case result of
              Left err -> err @?= "Multiple package declarations found"
              Right _ -> assertFailure "Should detect multiple package declarations"

        , testCase "If statement without brace" $ do
            let input = "if condition\n    doSomething()\n"
                result = parseTypus input
            case result of
              Left err -> "missing opening brace" `isInfixOf` err @?= True
              Right _ -> assertFailure "Should detect missing brace"
        ]

    , testGroup "Complex error scenarios"
        [ testCase "Mixed valid and invalid syntax" $ do
            let input = unlines
                  [ "//! ownership:true"
                  , "func validFunction() {"
                  , "    return 42"
                  , "}"
                  , "if invalid syntax here"
                  , "func anotherValid() {"
                  , "    return 'hello'"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should parse partial content: " ++ err
              Right file -> do
                -- Should have parsed valid functions
                length (filter (isPrefixOf "func validFunction" . cbContent) (tfBlocks file)) @?= 1
                length (filter (isPrefixOf "func anotherValid" . cbContent) (tfBlocks file)) @?= 1
                -- Should have syntax errors
                length (tfSyntaxErrors file) @? (> 0)

        , testCase "Deeply nested malformed content" $ do
            let input = unlines
                  [ "{//! ownership:true"
                  , "func outer() {"
                  , "    if condition"
                  , "        malformed line here"
                  , "    func inner() {"
                  , "        return 42"
                  , "    }"
                  , "}"
                  , "}"
                  , "func separate() {"
                  , "    return 'separate'"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should handle deeply nested errors: " ++ err
              Right file -> do
                -- Should parse some valid parts
                any (isPrefixOf "func separate" . cbContent) (tfBlocks file) @?= True

        , testCase "Unicode and special characters" $ do
            let input = unlines
                  [ "//! 拥有权:true"
                  , "func 测试函数() {"
                  , "    message := '你好世界'"
                  , "    return message"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Left err -> assertFailure $ "Should handle unicode: " ++ err
              Right file -> do
                -- Should parse unicode content
                any ("测试函数" `isInfixOf` cbContent) (tfBlocks file) @?= True
        ]

    , testGroup "Performance and stress tests"
        [ fastProperty "Large file parsing" $
            \size ->
              size < 1000 ==>
              let lines' = replicate size "func test" ++ show (size) ++ "() { return " ++ show size ++ "; }"
                  input = unlines lines'
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfBlocks file) >= size `div` 2  -- Should parse most content

        , fastProperty "Many small blocks" $
            \count ->
              count < 100 ==>
              let blocks = map (\i -> "func block" ++ show i ++ "() {}") [1..count]
                  input = unlines blocks
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> length (tfBlocks file) >= count

        , fastProperty "Complex directive combinations" $
            \directives ->
              length directives < 20 ==>
              let directiveLines = map (\d -> "//!" ++ d) directives
                  functionLines = ["func main() {}", "return 0"]
                  input = unlines $ directiveLines ++ functionLines
              in case parseTypus input of
                   Left _ -> property False
                   Right file -> property True
        ]
    ]