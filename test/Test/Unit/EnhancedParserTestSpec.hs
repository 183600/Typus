module Test.Unit.EnhancedParserTestSpec (tests) where

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )

tests :: TestTree
tests =
  testGroup "Enhanced Parser Tests"
    [ testCase "parses empty file correctly" $ do
        let source = ""
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed for empty file: " <> err
          Right typusFile -> do
            tfDirectives typusFile @?= defaultFileDirectives
            tfBlocks typusFile @?= []
            tfBuildTags typusFile @?= []

    , testCase "handles file with only whitespace" $ do
        let source = "\n   \n\t\n   \n"
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed for whitespace-only file: " <> err
          Right typusFile -> do
            tfDirectives typusFile @?= defaultFileDirectives
            tfBlocks typusFile @?= []

    , testCase "parses mixed directive formats" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! dependent_types: off"
              , "//! constraints: on"
              , "package main"
              , "func main() {"
              , "    {//! ownership: off, dependent_types: on}"
              , "        println(\"mixed block\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes, fdConstraints = constraints } = tfDirectives typusFile
            locatedValue (fromMaybe (error "missing ownership") ownership) @?= True
            locatedValue (fromMaybe (error "missing dependent types") dependentTypes) @?= False
            locatedValue (fromMaybe (error "missing constraints") constraints) @?= True
            
            let blocks = tfBlocks typusFile
            assertBool "should have one code block" (L.length blocks == 1)
            let block = L.head blocks
                directives = cbDirectives block
            locatedValue (fromMaybe (error "missing block ownership") (bdOwnership directives)) @?= False
            locatedValue (fromMaybe (error "missing block dependent types") (bdDependentTypes directives)) @?= True

    , testCase "handles nested directives" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    {//! dependent_types: on"
              , "        {//! constraints: on"
              , "            println(\"deeply nested\")"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "should have multiple code blocks" (L.length blocks >= 2)
            
            -- Check that directives are properly parsed at each level
            let hasOwnershipDirective = L.any (maybe False locatedValue . bdOwnership . cbDirectives) blocks
            let hasDependentTypesDirective = L.any (maybe False locatedValue . bdDependentTypes . cbDirectives) blocks
            let hasConstraintsDirective = L.any (maybe False locatedValue . bdConstraints . cbDirectives) blocks
            
            assertBool "should find ownership directive" hasOwnershipDirective
            assertBool "should find dependent types directive" hasDependentTypesDirective
            assertBool "should find constraints directive" hasConstraintsDirective

    , testCase "handles malformed directives gracefully" $ do
        let source = unlines
              [ "//! ownership"
              , "//! dependent_types: maybe"
              , "//! constraints: 123"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention malformed directive: " <> err) 
                           (L.any (`L.isInfixOf` err) ["malformed", "invalid", "directive"])
          Right _ -> assertFailure "expected parse failure for malformed directives"

    , testCase "handles unicode characters in source" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    // Unicode test: 你好世界 🌍"
              , "    message := \"Hello 世界 🚀\""
              , "    println(message)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed with unicode: " <> err
          Right typusFile -> do
            assertBool "should parse unicode content" (not $ L.null $ tfBlocks typusFile)
            let block = L.head $ tfBlocks typusFile
            assertBool "should contain unicode characters" 
                     (L.any (`L.isInfixOf` cbContent block) ["世界", "🌍", "🚀"])

    , testCase "handles very long lines" $ do
        let longString = replicate 1000 'a'
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    long := \"" <> longString <> "\""
              , "    println(long)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed with long lines: " <> err
          Right typusFile -> do
            assertBool "should handle long lines" (not $ L.null $ tfBlocks typusFile)

    , testCase "detects unclosed string literals" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    message := \"unclosed string"
              , "    println(message)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention string: " <> err) 
                           (L.any (`L.isInfixOf` err) ["string", "unclosed", "literal"])
          Right _ -> assertFailure "expected parse failure for unclosed string"

    , testCase "handles complex go build tags" $ do
        let source = unlines
              [ "//go:build linux && amd64"
              , "// +build linux,amd64"
              , "//go:build !cgo"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed with complex build tags: " <> err
          Right typusFile -> do
            let buildTags = tfBuildTags typusFile
            assertBool "should have multiple build tags" (L.length buildTags >= 3)
            map locatedValue buildTags @?= 
              ["//go:build linux && amd64", "// +build linux,amd64", "//go:build !cgo"]

    , testCase "handles comments mixed with directives" $ do
        let source = unlines
              [ "// This is a regular comment"
              , "//! ownership: on"
              , "// Another comment"
              , "//! dependent_types: off"
              , "package main"
              , "func main() {"
              , "    // Inline comment"
              , "    {//! constraints: on  // trailing comment"
              , "        println(\"mixed\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed with mixed comments: " <> err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            locatedValue (fromMaybe (error "missing ownership") ownership) @?= True
            locatedValue (fromMaybe (error "missing dependent types") dependentTypes) @?= False
            
            let blocks = tfBlocks typusFile
            assertBool "should have one code block" (L.length blocks == 1)
            let block = L.head blocks
                directives = cbDirectives block
            locatedValue (fromMaybe (error "missing constraints") (bdConstraints directives)) @?= True
    ]