module Test.Unit.ParserSpec (tests) where

import Data.List (find, isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
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
  testGroup "Parser"
    [ testCase "parses file-level directives" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! dependent_types: off"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> do
                locatedValue loc @?= True
                posLine (spanStart (locSpan loc)) @?= 1
                posLine (spanEnd (locSpan loc)) @?= 2
            case dependentTypes of
              Nothing -> assertFailure "expected dependent types directive"
              Just loc -> do
                locatedValue loc @?= False
                posLine (spanStart (locSpan loc)) @?= 2
                posLine (spanEnd (locSpan loc)) @?= 3

    , testCase "captures block directives with associated code" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on, dependent_types: on}"
              , "        println(\"inside\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let ownershipBlock = find (maybe False locatedValue . bdOwnership . cbDirectives) (tfBlocks typusFile)
            case ownershipBlock of
              Nothing -> assertFailure "expected to find a block with ownership enabled"
              Just CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = blkSpan } -> do
                case bdOwnership directives of
                  Nothing -> assertFailure "expected ownership flag"
                  Just loc -> do
                    locatedValue loc @?= True
                    posLine (spanStart (locSpan loc)) @?= 3
                case bdDependentTypes directives of
                  Nothing -> assertFailure "expected dependent types flag"
                  Just loc -> locatedValue loc @?= True
                bdConstraints directives @?= Nothing
                assertBool "block content should include println call" ("println(\"inside\")" `isInfixOf` content)
                posLine (spanStart blkSpan) @?= 4
                posLine (spanEnd blkSpan) @?= 5

    , testCase "ignores trailing whitespace-only files" $ do
        let source :: String; source = "\n   \n\n"
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "collects build tags before the first code block" $ do
        let source = unlines
              [ "//go:build ignore"
              , "// +build ignore"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right TypusFile { tfBuildTags = buildTags } -> do
            map locatedValue buildTags @?= ["//go:build ignore", "// +build ignore"]
            case buildTags of
              (firstTag:secondTag:_) -> do
                posLine (spanStart (locSpan firstTag)) @?= 1
                posLine (spanStart (locSpan secondTag)) @?= 2
              _ -> assertFailure "expected two build tags"
    ]
