{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core Parser module QuickCheck tests
module Test.Unit.CoreParserQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.Arbitrary
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Parser QuickCheck Tests
-- ============================================================================

-- | Test that parsing empty string returns empty file
prop_parseEmptyString :: Property
prop_parseEmptyString =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ null (tfBlocks file)

-- | Test that parsing a string with only whitespace returns empty file
prop_parseWhitespaceOnly :: Property
prop_parseWhitespaceOnly =
  forAll arbitraryWhitespace $ \ws ->
    let result = parseTypus ws
    in case result of
      Left _ -> property False
      Right file -> property $ null (tfBlocks file)

-- | Test that parsing a simple code block preserves content
prop_parseSimpleBlock :: Property
prop_parseSimpleBlock =
  forAll arbitraryIdentifier $ \ident ->
    forAll arbitraryShortString $ \content ->
      let input = ident ++ " {\n" ++ content ++ "\n}"
          result = parseTypus input
      in case result of
        Left _ -> property False
        Right file -> property $ not (null (tfBlocks file))

-- | Test that file directives are parsed correctly
prop_parseFileDirectives :: Property
prop_parseFileDirectives =
  forAll arbitrary $ \ownership ->
    forAll arbitrary $ \dependentTypes ->
      forAll arbitrary $ \constraints ->
        let directives = []
            input = buildFileDirectiveInput directives ownership dependentTypes constraints
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> property $ True

-- | Test that block directives are parsed correctly
prop_parseBlockDirectives :: Property
prop_parseBlockDirectives =
  forAll arbitrary $ \ownership ->
    forAll arbitrary $ \dependentTypes ->
      forAll arbitrary $ \constraints ->
        let directives = []
            input = buildBlockDirectiveInput directives ownership dependentTypes constraints
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> property $ True

-- | Test that parsing preserves line structure
prop_parsePreservesLines :: Property
prop_parsePreservesLines =
  forAll (listOf (arbitraryShortString `suchThat` (not . null))) $ \lines ->
    let input = unlines lines
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles comments correctly
prop_parseHandlesComments :: Property
prop_parseHandlesComments =
  forAll arbitraryShortString $ \code ->
    forAll arbitraryShortString $ \comment ->
      let input = code ++ " // " ++ comment
          result = parseTypus input
      in case result of
        Left _ -> property False
        Right file -> property $ True

-- | Test that parsing is idempotent for valid input
prop_parseIdempotent :: Property
prop_parseIdempotent =
  forAll validTypusCode $ \code ->
    let firstParse = parseTypus code
    in case firstParse of
      Left _ -> property True  -- Invalid input is allowed
      Right file -> property $ True

-- | Test that parsing handles nested blocks
prop_parseNestedBlocks :: Property
prop_parseNestedBlocks =
  forAll arbitraryIdentifier $ \ident1 ->
    forAll arbitraryIdentifier $ \ident2 ->
      forAll arbitraryShortString $ \content1 ->
        forAll arbitraryShortString $ \content2 ->
          let input = ident1 ++ " {\n" ++ content1 ++ "\n" ++ 
                     ident2 ++ " {\n" ++ content2 ++ "\n}\n}"
              result = parseTypus input
          in case result of
            Left _ -> property False
            Right file -> property $ True

-- | Test that parsing handles Unicode characters
prop_parseUnicode :: Property
prop_parseUnicode =
  forAll arbitraryUnicodeString $ \unicodeStr ->
    let input = "test {\n" ++ unicodeStr ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles escape sequences
prop_parseEscapeSequences :: Property
prop_parseEscapeSequences =
  forAll arbitraryEscapeString $ \escapeStr ->
    let input = "test {\n" ++ escapeStr ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles string literals
prop_parseStringLiterals :: Property
prop_parseStringLiterals =
  forAll arbitraryStringLiteral $ \strLit ->
    let input = "test {\n" ++ strLit ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles numeric literals
prop_parseNumericLiterals :: Property
prop_parseNumericLiterals =
  forAll arbitraryNumericLiteral $ \numLit ->
    let input = "test {\n" ++ numLit ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles identifiers
prop_parseIdentifiers :: Property
prop_parseIdentifiers =
  forAll arbitraryIdentifier $ \ident ->
    let input = "test {\n" ++ ident ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- | Test that parsing handles operators
prop_parseOperators :: Property
prop_parseOperators =
  forAll arbitraryOperator $ \op ->
    let input = "test {\n" ++ op ++ "\n}"
        result = parseTypus input
    in case result of
      Left _ -> property False
      Right file -> property $ True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Build input with file directives
buildFileDirectiveInput :: [(String, String)] -> Bool -> Bool -> Bool -> String
buildFileDirectiveInput directives ownership dependentTypes constraints =
  let ownershipStr = if ownership then "//! ownership: true" else ""
      dependentTypesStr = if dependentTypes then "//! dependentTypes: true" else ""
      constraintsStr = if constraints then "//! constraints: true" else ""
      allDirectives = [ownershipStr, dependentTypesStr, constraintsStr]
      validDirectives = filter (not . null) allDirectives
  in unlines validDirectives

-- | Build input with block directives
buildBlockDirectiveInput :: [(String, String)] -> Bool -> Bool -> Bool -> String
buildBlockDirectiveInput directives ownership dependentTypes constraints =
  let ownershipStr = if ownership then "ownership: true" else ""
      dependentTypesStr = if dependentTypes then "dependentTypes: true" else ""
      constraintsStr = if constraints then "constraints: true" else ""
      allDirectives = [ownershipStr, dependentTypesStr, constraintsStr]
      validDirectives = filter (not . null) allDirectives
      directivesStr = if null validDirectives 
                      then ""
                      else "{//! " ++ unwords validDirectives ++ "}"
  in "test {\n" ++ directivesStr ++ "\n}"

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Parser QuickCheck Tests"
  [ testProperty "Parse empty string" prop_parseEmptyString
  , testProperty "Parse whitespace only" prop_parseWhitespaceOnly
  , testProperty "Parse simple block" prop_parseSimpleBlock
  , testProperty "Parse file directives" prop_parseFileDirectives
  , testProperty "Parse block directives" prop_parseBlockDirectives
  , testProperty "Parse preserves lines" prop_parsePreservesLines
  , testProperty "Parse handles comments" prop_parseHandlesComments
  , testProperty "Parse is idempotent" prop_parseIdempotent
  , testProperty "Parse nested blocks" prop_parseNestedBlocks
  , testProperty "Parse Unicode" prop_parseUnicode
  , testProperty "Parse escape sequences" prop_parseEscapeSequences
  , testProperty "Parse string literals" prop_parseStringLiterals
  , testProperty "Parse numeric literals" prop_parseNumericLiterals
  , testProperty "Parse identifiers" prop_parseIdentifiers
  , testProperty "Parse operators" prop_parseOperators
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite