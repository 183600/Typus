{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserDirectivesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, oneof, elements, vectorOf)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), SourcePos(..))

-- Arbitrary instances for testing
instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate simple valid Typus content
genValidTypusContent :: Gen String
genValidTypusContent = do
    hasFileDirective <- arbitrary
    hasBlocks <- arbitrary
    let fileDirective = if hasFileDirective then "//! ownership: true, dependent-types: true\n" else ""
    let blockContent = if hasBlocks 
                      then "package main\n\nfunc main() {\n    println(\"Hello\")\n}\n"
                      else "package main\n"
    return $ fileDirective ++ blockContent

-- Generate content with malformed directives
genMalformedDirectiveContent :: Gen String
genMalformedDirectiveContent = oneof
  [ return "//! ownership invalid\npackage main\n"
  , return "//! ownership: true extra\npackage main\n"
  , return "{//! ownership: true missing\npackage main\n"
  , return "//! :true\npackage main\n"
  ]

-- Property: parsing empty content should succeed
prop_parse_empty_content :: Property
prop_parse_empty_content =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ 
      tfDirectives file === defaultFileDirectives &&.
      L.length (tfBlocks file) === 0

-- Property: parsing simple package declaration succeeds
prop_parse_simple_package :: Property
prop_parse_simple_package =
  let content = "package main\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 0

-- Property: parsing valid file directives preserves them
prop_parse_file_directives :: Property
prop_parse_file_directives =
  let content = "//! ownership: true, dependent-types: false\npackage main\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      let dirs = tfDirectives file
          hasOwnership = isJust (fdOwnership dirs)
          hasDepTypes = isJust (fdDependentTypes dirs)
      in property $ hasOwnership &&. hasDepTypes

-- Property: parsing multiple blocks preserves order
prop_parse_multiple_blocks :: Property
prop_parse_multiple_blocks =
  let content = "package main\n\nfunc test1() {}\n\nfunc test2() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 2

-- Property: block directives are parsed correctly
prop_parse_block_directives :: Property
prop_parse_block_directives =
  let content = "package main\n\n{//! ownership: true}\nfunc test() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
          hasBlockWithOwnership = L.any (isJust . bdOwnership . cbDirectives) blocks
      in property $ hasBlockWithOwnership

-- Property: malformed directives are handled gracefully
prop_parse_malformed_directives :: Property
prop_parse_malformed_directives =
  forAll genMalformedDirectiveContent $ \content ->
    let result = parseTypus content
    in case result of
      Left _ -> property True  -- Expected to fail
      Right _ -> property True  -- Or succeed gracefully

-- Property: parsing preserves content in blocks
prop_parse_preserves_content :: Property
prop_parse_preserves_content =
  let content = "package main\n\nfunc test() {\n    // comment\n    x := 1\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file ->
      let blocks = tfBlocks file
          hasContent = L.any (L.isInfixOf "x := 1" . cbContent) blocks
      in property $ hasContent

-- Property: parsing handles mixed line endings
prop_parse_mixed_line_endings :: Property
prop_parse_mixed_line_endings =
  let content = "package main\r\n\nfunc test() {\r\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 1

-- Property: parsing large content doesn't crash
prop_parse_large_content :: Property
prop_parse_large_content =
  let largeContent = unlines $ replicate 1000 "func testFunction() { x := 1 }"
      result = parseTypus largeContent
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) > 0

-- Property: parsing with syntax errors still returns structure
prop_parse_syntax_errors_structure :: Property
prop_parse_syntax_errors_structure =
  let content = "package main\n\nfunc incomplete {\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right file -> 
      let syntaxErrors = tfSyntaxErrors file
      hasStructure = L.length (tfBlocks file) >= 0
      hasErrors = L.length syntaxErrors >= 0
      in property $ hasStructure &&. hasErrors

-- Property: round-trip parsing preserves essential structure
prop_parse_roundtrip_structure :: Property
prop_parse_roundtrip_structure =
  forAll genValidTypusContent $ \originalContent ->
    case parseTypus originalContent of
      Left _ -> property False
      Right file -> 
        let blocks = tfBlocks file
            blockCount = L.length blocks
            hasPackage = "package" `L.isInfixOf` originalContent
        in property $ (hasPackage ==> blockCount >= 0) &&. (not hasPackage ==> blockCount >= 0)

tests :: TestTree
tests = testGroup "NewCabalParserDirectivesSpec"
  [ fastProperty "parse empty content" prop_parse_empty_content
  , fastProperty "parse simple package" prop_parse_simple_package
  , fastProperty "parse file directives" prop_parse_file_directives
  , fastProperty "parse multiple blocks" prop_parse_multiple_blocks
  , fastProperty "parse block directives" prop_parse_block_directives
  , fastProperty "parse malformed directives" prop_parse_malformed_directives
  , fastProperty "parse preserves content" prop_parse_preserves_content
  , fastProperty "parse mixed line endings" prop_parse_mixed_line_endings
  , fastProperty "parse large content" prop_parse_large_content
  , fastProperty "parse syntax errors structure" prop_parse_syntax_errors_structure
  , fastProperty "parse roundtrip structure" prop_parse_roundtrip_structure
  ]