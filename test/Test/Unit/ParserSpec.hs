module Test.Unit.ParserSpec (tests) where

import Data.List (find)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
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
            ownership @?= Just True
            dependentTypes @?= Just False

    , testCase "captures block directives with associated code" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on, dependent_types: on"
              , "        println(\"inside\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let ownershipBlock = find (bdOwnership . cbDirectives) (tfBlocks typusFile)
            case ownershipBlock of
              Nothing -> assertFailure "expected to find a block with ownership enabled"
              Just CodeBlock { cbDirectives = directives, cbContent = content } -> do
                directives @?= BlockDirectives { bdOwnership = True, bdDependentTypes = True, bdConstraints = False }
                assertBool "block content should include println call" ("println(\"inside\")" `elem` lines content)

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
          Right TypusFile { tfBuildTags = buildTags } ->
            buildTags @?= ["//go:build ignore", "// +build ignore"]
    ]
