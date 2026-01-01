{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, frequency, sized, resize, Positive(..), NonEmpty(..)
  )

import Parser
  ( parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives
  )

import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, spanStart, spanEnd)
import Utils (trim)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, sort)
import Data.Char (isSpace)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

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

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary
    span <- arbitrary
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- arbitrary
    blocks <- arbitrary
    syntaxErrors <- arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parseTypus with empty string returns valid TypusFile
prop_parseTypus_empty_string :: Property
prop_parseTypus_empty_string =
  case parseTypus "" of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parseTypus with only whitespace returns valid TypusFile
prop_parseTypus_whitespace_only :: Property
prop_parseTypus_whitespace_only =
  forAll (listOf (elements " \t\n\r")) $ \whitespace ->
    case parseTypus whitespace of
      Left _ -> property False
      Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parseTypus with simple package declaration succeeds
prop_parseTypus_simple_package :: Property
prop_parseTypus_simple_package =
  let input = "package main\nfunc main() {}\n"
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> property True

-- Property: parseTypus with file directives parses correctly
prop_parseTypus_file_directives :: String -> String -> String -> Property
prop_parseTypus_file_directives ownershipStr dependentTypesStr constraintsStr =
  let ownership = if null ownershipStr then "on" else ownershipStr
      dependentTypes = if null dependentTypesStr then "off" else dependentTypesStr
      constraints = if null constraintsStr then "on" else constraintsStr
      input = unlines
        [ "//! ownership: " ++ ownership
        , "//! dependent_types: " ++ dependentTypes
        , "//! constraints: " ++ constraints
        , "package main"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let dirs = tfDirectives typusFile
         in case fdOwnership dirs of
           Nothing -> property False
           Just loc -> locatedValue loc === (ownership `elem` ["on", "true"])

-- Property: parseTypus with block directives parses correctly
prop_parseTypus_block_directives :: Property
prop_parseTypus_block_directives =
  let input = unlines
        [ "package main"
        , "{//! ownership: on, dependent_types: off"
        , "func test() {"
        , "  return 42"
        , "}"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in case blocks of
           (block:_) ->
             let dirs = cbDirectives block
             in case bdOwnership dirs of
               Nothing -> property False
               Just loc -> locatedValue loc === True
           [] -> property False

-- Property: parseTypus with build tags parses correctly
prop_parseTypus_build_tags :: Property
prop_parseTypus_build_tags =
  let input = unlines
        [ "//go:build linux"
        , "// +build amd64"
        , "package main"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let buildTags = tfBuildTags typusFile
         in L.length buildTags === 2

-- Property: parseTypus preserves code content in blocks
prop_parseTypus_preserves_content :: String -> Property
prop_parseTypus_preserves_content code =
  not (null code) && not (L.any (`elem` ['{', '}']) code) ==>
  let input = unlines
        [ "package main"
        , code
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in L.any (\block -> code `L.isInfixOf` cbContent block) blocks

-- Property: parseTypus handles multiple code blocks
prop_parseTypus_multiple_blocks :: Property
prop_parseTypus_multiple_blocks =
  let input = unlines
        [ "package main"
        , "{//! ownership: on"
        , "func func1() { return 1 }"
        , "}"
        , "{//! ownership: off"
        , "func func2() { return 2 }"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in L.length blocks >= 2

-- Property: parseTypus with syntax errors still returns TypusFile
prop_parseTypus_syntax_errors :: Property
prop_parseTypus_syntax_errors =
  let input = unlines
        [ "package main"
        , "func broken("  -- Missing closing parenthesis
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let syntaxErrors = tfSyntaxErrors typusFile
         in not (null syntaxErrors)

-- Property: parseTypus handles if statements correctly
prop_parseTypus_if_statements :: Property
prop_parseTypus_if_statements =
  let input = unlines
        [ "package main"
        , "func test() {"
        , "  if x {"
        , "    return true"
        , "  }"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> property True

-- Property: parseTypus rejects if statements without braces
prop_parseTypus_if_without_braces :: Property
prop_parseTypus_if_without_braces =
  let input = unlines
        [ "package main"
        , "func test() {"
        , "  if x return true"  -- Missing braces
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False

-- Property: parseTypus handles multiple package declarations error
prop_parseTypus_multiple_packages :: Property
prop_parseTypus_multiple_packages =
  let input = unlines
        [ "package main"
        , "package other"  -- Second package declaration
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False

-- Property: parseTypus handles nested directives
prop_parseTypus_nested_directives :: Property
prop_parseTypus_nested_directives =
  let input = unlines
        [ "//! ownership: on"
        , "package main"
        , "{//! ownership: off"
        , "func outer() {"
        , "  {//! ownership: on"
        , "  func inner() { return 42 }"
        , "  }"
        , "}"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in L.length blocks >= 2

-- Property: parseTypus with comments handles correctly
prop_parseTypus_comments :: Property
prop_parseTypus_comments =
  let input = unlines
        [ "package main"
        , "// This is a comment"
        , "/* This is a block comment */"
        , "func main() {"
        , "  // Another comment"
        , "  return"
        , "}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> property True

-- ============================================================================
-- Directive Properties
-- ============================================================================

-- Property: defaultFileDirectives has L.all Nothing values
prop_defaultFileDirectives_nothing :: Property
prop_defaultFileDirectives_nothing =
  fdOwnership defaultFileDirectives === Nothing .&&.
  fdDependentTypes defaultFileDirectives === Nothing .&&.
  fdConstraints defaultFileDirectives === Nothing

-- Property: defaultBlockDirectives has L.all Nothing values
prop_defaultBlockDirectives_nothing :: Property
prop_defaultBlockDirectives_nothing =
  bdOwnership defaultBlockDirectives === Nothing .&&.
  bdDependentTypes defaultBlockDirectives === Nothing .&&.
  bdConstraints defaultBlockDirectives === Nothing

-- ============================================================================
-- Combined Properties
-- ============================================================================

-- Property: parseTypus round-trip with generated content
prop_parseTypus_roundtrip :: Property
prop_parseTypus_roundtrip =
  forAll (listOf $ elements ["func test() {}", "var x int", "const y = 42", "type MyType int"]) $ \lines ->
    let input = "package main\n" ++ unlines lines
    in case parseTypus input of
         Left _ -> property False
         Right _ -> property True

-- Property: parseTypus handles unicode characters
prop_parseTypus_unicode :: Property
prop_parseTypus_unicode =
  let input = unlines
        [ "package main"
        , "func 测试函数() {"
        , "  // 注释"
        , "  return 你好"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus input of
       Left _ -> property False
       Right typusFile -> property True

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Properties"
  [ fastProperty "parseTypus empty string" prop_parseTypus_empty_string
  , fastProperty "parseTypus whitespace only" prop_parseTypus_whitespace_only
  , fastProperty "parseTypus simple package" prop_parseTypus_simple_package
  , fastProperty "parseTypus file directives" prop_parseTypus_file_directives
  , fastProperty "parseTypus block directives" prop_parseTypus_block_directives
  , fastProperty "parseTypus build tags" prop_parseTypus_build_tags
  , fastProperty "parseTypus preserves content" prop_parseTypus_preserves_content
  , fastProperty "parseTypus multiple blocks" prop_parseTypus_multiple_blocks
  , fastProperty "parseTypus syntax errors" prop_parseTypus_syntax_errors
  , fastProperty "parseTypus if statements" prop_parseTypus_if_statements
  , fastProperty "parseTypus if without braces" prop_parseTypus_if_without_braces
  , fastProperty "parseTypus multiple packages" prop_parseTypus_multiple_packages
  , fastProperty "parseTypus nested directives" prop_parseTypus_nested_directives
  , fastProperty "parseTypus comments" prop_parseTypus_comments
  , fastProperty "defaultFileDirectives nothing" prop_defaultFileDirectives_nothing
  , fastProperty "defaultBlockDirectives nothing" prop_defaultBlockDirectives_nothing
  , fastProperty "parseTypus roundtrip" prop_parseTypus_roundtrip
  , fastProperty "parseTypus unicode" prop_parseTypus_unicode

  , testCase "parseTypus basic functionality" $ do
      let simpleInput = "package main\nfunc main() {}\n"
      case parseTypus simpleInput of
        Left err -> assertFailure $ "Failed to parse simple input: " ++ err
        Right typusFile -> do
          tfDirectives typusFile @?= defaultFileDirectives
          L.length (tfBlocks typusFile) @?= 1
          
  , testCase "parseTypus with file directives" $ do
      let directiveInput = unlines
            [ "//! ownership: on"
            , "//! dependent_types: off"
            , "package main"
            , "func main() {}"
            ]
      case parseTypus directiveInput of
        Left err -> assertFailure $ "Failed to parse directives: " ++ err
        Right typusFile -> do
          let dirs = tfDirectives typusFile
          case fdOwnership dirs of
            Nothing -> assertFailure "Expected ownership directive"
            Just loc -> locatedValue loc @?= True
          case fdDependentTypes dirs of
            Nothing -> assertFailure "Expected dependent_types directive"
            Just loc -> locatedValue loc @?= False
            
  , testCase "parseTypus error cases" $ do
      let badInput = unlines
            [ "package main"
            , "func broken("  -- Missing closing parenthesis
            ]
      case parseTypus badInput of
        Left _ -> assertFailure "Should parse even with syntax errors"
        Right typusFile -> do
          let syntaxErrors = tfSyntaxErrors typusFile
          assertBool "Should have syntax errors" (not (null syntaxErrors))
          
  , testCase "parseTypus build tags" $ do
      let buildTagInput = unlines
            [ "//go:build linux"
            , "// +build amd64"
            , "package main"
            , "func main() {}"
            ]
      case parseTypus buildTagInput of
        Left err -> assertFailure $ "Failed to parse build tags: " ++ err
        Right typusFile -> do
          let buildTags = tfBuildTags typusFile
          L.length buildTags @?= 2
  ]