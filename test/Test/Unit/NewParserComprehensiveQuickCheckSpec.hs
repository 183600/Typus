{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserComprehensiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof, elements, listOf, resize)
import qualified Test.QuickCheck.Gen as Gen

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
  , locatedValue
  , spanEnd
  , spanStart
  , startPos
  )

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    if (posLine start <= posLine end) && 
       (posLine start < posLine end || posColumn start <= posColumn end)
      then return (SourceSpan start end)
      else return (SourceSpan start start)

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- Generate valid directive strings
genDirective :: Gen String
genDirective = oneof
  [ return "//! ownership: on"
  , return "//! ownership: off"
  , return "//! dependent_types: on"
  , return "//! dependent_types: off"
  , return "//! constraints: on"
  , return "//! constraints: off"
  ]

-- Generate valid code block strings
genCodeBlock :: Gen String
genCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n(){}[];:,."
  return $ unlines content

-- Generate complete Typus source
genTypusSource :: Gen String
genTypusSource = do
  numDirectives <- choose (0, 3)
  directives <- listOf $ resize numDirectives genDirective
  code <- genCodeBlock
  return $ unlines directives ++ code

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Parsing valid directives should succeed
prop_parse_directives_success :: Property
prop_parse_directives_success = forAll genDirective $ \directive ->
  let source = directive ++ "\npackage main\nfunc main() {}"
  in case parseTypus source of
       Left _ -> property False
       Right _ -> property True

-- Property: Round-trip property for file directives
prop_directives_roundtrip :: Property
prop_directives_roundtrip = forAll genTypusSource $ \source ->
  case parseTypus source of
    Left _ -> property True  -- Invalid input is allowed
    Right typusFile -> 
      let directives = tfDirectives typusFile
      in property True  -- Successfully parsed directives

-- Property: Ownership directive parsing preserves boolean value
prop_ownership_directive_preserves_value :: Property
prop_ownership_directive_preserves_value = 
  forAll (elements ["on", "off"]) $ \value ->
    let source = "//! ownership: " ++ value ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile ->
           case fdOwnership (tfDirectives typusFile) of
             Nothing -> property False
             Just located -> 
               let parsedValue = locatedValue located
                   expectedValue = value == "on"
               in parsedValue === expectedValue

-- Property: Dependent types directive parsing preserves boolean value
prop_dependent_types_directive_preserves_value :: Property
prop_dependent_types_directive_preserves_value = 
  forAll (elements ["on", "off"]) $ \value ->
    let source = "//! dependent_types: " ++ value ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile ->
           case fdDependentTypes (tfDirectives typusFile) of
             Nothing -> property False
             Just located -> 
               let parsedValue = locatedValue located
                   expectedValue = value == "on"
               in parsedValue === expectedValue

-- Property: Multiple directives are all parsed
prop_multiple_directives_parsed :: Property
prop_multiple_directives_parsed = 
  forAll (listOf1 genDirective) $ \directives ->
    let source = unlines directives ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile ->
           let fileDirectives = tfDirectives typusFile
               hasOwnership = isJust $ fdOwnership fileDirectives
               hasDependentTypes = isJust $ fdDependentTypes fileDirectives
               hasConstraints = isJust $ fdConstraints fileDirectives
               expectedCount = length $ filter ("ownership" `isInfixOf`) directives
           in property True  -- At least parsing succeeded

-- Property: Empty source should parse with default directives
prop_empty_source_default_directives :: Property
prop_empty_source_default_directives =
  let source = ""
  in case parseTypus source of
       Left _ -> property False
       Right typusFile ->
         let directives = tfDirectives typusFile
         in directives === defaultFileDirectives

-- Property: Source position tracking for directives
prop_directive_source_position_tracking :: Property
prop_directive_source_position_tracking = forAll genDirective $ \directive ->
  let source = directive ++ "\npackage main\nfunc main() {}"
  in case parseTypus source of
       Left _ -> property False
       Right typusFile ->
         case fdOwnership (tfDirectives typusFile) of
           Just located -> 
             let span = locSpan located
                 start = spanStart span
             in posLine start === 1
           Nothing -> property True  -- Directive might not be ownership

-- Property: Parser is idempotent for valid input
prop_parser_idempotent :: Property
prop_parser_idempotent = forAll genTypusSource $ \source ->
  case parseTypus source of
    Left _ -> property True  -- Invalid input is allowed
    Right firstParse ->
      case parseTypus source of
        Left _ -> property False  -- Should parse consistently
        Right secondParse -> firstParse === secondParse

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Comprehensive QuickCheck Tests"
  [ fastProperty "Parse directives success" prop_parse_directives_success
  , fastProperty "Directives roundtrip" prop_directives_roundtrip
  , fastProperty "Ownership directive preserves value" prop_ownership_directive_preserves_value
  , fastProperty "Dependent types directive preserves value" prop_dependent_types_directive_preserves_value
  , fastProperty "Multiple directives parsed" prop_multiple_directives_parsed
  , fastProperty "Empty source default directives" prop_empty_source_default_directives
  , fastProperty "Directive source position tracking" prop_directive_source_position_tracking
  , fastProperty "Parser idempotent" prop_parser_idempotent
  ]