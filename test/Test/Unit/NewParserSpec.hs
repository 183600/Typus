{-# LANGUAGE CPP #-}
module Test.Unit.NewParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  , locatedValue
  , locSpan
  )

tests :: TestTree
tests =
  testGroup "New Parser Tests"
    [ testCase "parses empty file" $ do
        let source = ""
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            tfDirectives typusFile @?= defaultFileDirectives
            tfBuildTags typusFile @?= []
            tfBlocks typusFile @?= []

    , testCase "parses simple package declaration" $ do
        let source = "package main\n"
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            assertBool "should have one block" (length (tfBlocks typusFile) == 1)
            let firstBlock = head (tfBlocks typusFile)
            cbDirectives firstBlock @?= defaultBlockDirectives
            assertBool "block contains package declaration" ("package main" `isInfixOf` cbContent firstBlock)

    , testCase "parses file-level ownership directive" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> do
                locatedValue loc @?= True
                posLine (spanStart (locSpan loc)) @?= 1

    , testCase "parses file-level dependent_types directive" $ do
        let source = unlines
              [ "//! dependent_types: off"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case dependentTypes of
              Nothing -> assertFailure "expected dependent_types directive"
              Just loc -> do
                locatedValue loc @?= False
                posLine (spanStart (locSpan loc)) @?= 1

    , testCase "parses multiple file directives" $ do
        let source = unlines
              [ "//! ownership: on, dependent_types: on"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> locatedValue loc @?= True
            case dependentTypes of
              Nothing -> assertFailure "expected dependent_types directive"
              Just loc -> locatedValue loc @?= True

    , testCase "parses constraints directive as dependent_types alias" $ do
        let source = unlines
              [ "//! constraints: on"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdConstraints = constraints, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case constraints of
              Nothing -> assertFailure "expected constraints directive"
              Just loc -> locatedValue loc @?= True
            case dependentTypes of
              Nothing -> assertFailure "expected dependent_types alias"
              Just loc -> locatedValue loc @?= True

    , testCase "parses block-level ownership directive" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on}"
              , "        println(\"hello\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                ownershipBlock = filter (\block -> maybe False locatedValue (bdOwnership (cbDirectives block))) blocks
            case ownershipBlock of
              [] -> assertFailure "expected ownership block"
              (block:_) -> do
                case bdOwnership (cbDirectives block) of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True
                assertBool "block contains println" ("println(\"hello\")" `isInfixOf` cbContent block)

    , testCase "parses block with multiple directives" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on, dependent_types: off}"
              , "        x := 42"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                directedBlocks = filter (\block -> isJust (bdOwnership (cbDirectives block))) blocks
            case directedBlocks of
              [] -> assertFailure "expected directed block"
              (block:_) -> do
                let directives = cbDirectives block
                case bdOwnership directives of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True
                case bdDependentTypes directives of
                  Nothing -> assertFailure "expected dependent_types directive"
                  Just loc -> locatedValue loc @?= False

    , testCase "parses README-style block directive without closing brace" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"hello\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                ownershipBlock = filter (\block -> maybe False locatedValue (bdOwnership (cbDirectives block))) blocks
            case ownershipBlock of
              [] -> assertFailure "expected ownership block"
              (block:_) -> do
                case bdOwnership (cbDirectives block) of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True

    , testCase "parses build tags" $ do
        let source = unlines
              [ "//go:build ignore"
              , "// +build ignore"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let buildTags = tfBuildTags typusFile
            length buildTags @?= 2
            map locatedValue buildTags @?= ["//go:build ignore", "// +build ignore"]

    , testCase "rejects unknown file directive" $ do
        let source = unlines
              [ "//! unknown: on"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention unknown directive: " ++ err) ("Unknown file directive" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for unknown directive"

    , testCase "rejects unknown block directive" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! unknown: on}"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention unknown directive: " ++ err) ("Unknown block directive" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for unknown directive"

    , testCase "rejects invalid boolean values" $ do
        let source = unlines
              [ "//! ownership: maybe"
              , "package main"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention invalid boolean: " ++ err) ("Invalid boolean value" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for invalid boolean"

    , testCase "rejects multiple package declarations" $ do
        let source = unlines
              [ "package main"
              , "package secondary"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention multiple package: " ++ err) ("Multiple package declarations" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for multiple packages"

    , testCase "rejects unclosed directive block" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"hello\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention unclosed block: " ++ err) ("Unclosed directive block" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for unclosed block"

    , testCase "handles whitespace and empty lines" $ do
        let source = unlines
              [ ""
              , "   "
              , "//! ownership: on"
              , ""
              , "package main"
              , ""
              , "func main() {}"
              , ""
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> locatedValue loc @?= True
            assertBool "should have blocks" (not $ null $ tfBlocks typusFile)

    , testCase "parses complex nested structures" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        {//! ownership: on}"
              , "            x := 42"
              , "            if x > 0 {"
              , "                {//! dependent_types: on}"
              , "                    println(x)"
              , "            }"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                ownershipBlocks = filter (\block -> maybe False locatedValue (bdOwnership (cbDirectives block))) blocks
                dependentTypeBlocks = filter (\block -> maybe False locatedValue (bdDependentTypes (cbDirectives block))) blocks
            length ownershipBlocks @?= 1
            length dependentTypeBlocks @?= 1

    , testCase "preserves source location information" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    {//! dependent_types: off}"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
            case ownership of
              Just loc -> do
                posLine (spanStart (locSpan loc)) @?= 1
                posColumn (spanStart (locSpan loc)) @?= 1
            let blocks = tfBlocks typusFile
                dependentTypeBlocks = filter (\block -> maybe False (not . locatedValue) (bdDependentTypes (cbDirectives block))) blocks
            case dependentTypeBlocks of
              (block:_) -> case bdDependentTypes (cbDirectives block) of
                Just loc -> do
                  posLine (spanStart (locSpan loc)) @?= 4
                  posColumn (spanStart (locSpan loc)) @?= 5
                _ -> assertFailure "expected dependent_types directive location"

    , testCase "handles comments in code blocks" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    // This is a comment"
              , "    x := 42 // inline comment"
              , "    /* block comment */"
              , "    println(x)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            assertBool "should have blocks" (not $ null $ tfBlocks typusFile)
            let mainBlock = head (tfBlocks typusFile)
            assertBool "contains line comment" ("// This is a comment" `isInfixOf` cbContent mainBlock)
            assertBool "contains inline comment" ("// inline comment" `isInfixOf` cbContent mainBlock)
            assertBool "contains block comment" ("/* block comment */" `isInfixOf` cbContent mainBlock)

    , testCase "handles string literals with braces" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    s := \"{not a directive}\""
              , "    println(s)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                directedBlocks = filter (\block -> isJust (bdOwnership (cbDirectives block))) blocks
            length directedBlocks @?= 0  -- Should not parse string content as directive
            let mainBlock = head blocks
            assertBool "contains string literal" ("{not a directive}" `isInfixOf` cbContent mainBlock)
  ]