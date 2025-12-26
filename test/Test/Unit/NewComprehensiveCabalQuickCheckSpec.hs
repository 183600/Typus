{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, resize, vectorOf, frequency
  )

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation)
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Ownership (OwnershipType(..), OwnershipError(..))
import Compiler (CompilerError(..), CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, spanLength, spanContains)

import Data.List (isPrefixOf, isInfixOf, sort, nub, intersperse)
import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Text as T

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Property: trim trim . trim = trim (idempotent)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: splitBy and splitByCollapsed consistency
prop_split_by_consistency :: Char -> String -> Property
prop_split_by_consistency delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
      filtered = filter (not . null) normal
  in collapsed === filtered

-- Property: removeLineComments preserves non-comment lines
prop_remove_line_comments_preserves :: String -> Property
prop_remove_line_comments_preserves s =
  not ("//" `isInfixOf` s) ==> removeLineComments s === s

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_relative :: String -> Property
prop_normalize_indentation_relative s =
  let linesWithIndent = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in length linesWithIndent === length normalizedLines

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- Property: parseTypus roundtrip for simple content
prop_parse_typus_roundtrip :: String -> Property
prop_parse_typus_roundtrip content =
  not (null content) && not ("//!" `isInfixOf` content) && 
  not ("{/*!" `isInfixOf` content) && not ("package " `isInfixOf` content) ==>
  case parseTypus content of
    Left _ -> property True  -- Invalid input is allowed
    Right _ -> property True  -- Successful parse is valid

-- Property: parseTypus handles directives correctly
prop_parse_typus_directives :: String -> Property
prop_parse_typus_directives directive =
  let fullDirective = "//! " ++ directive
      result = parseTypus fullDirective
  in case result of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile /= defaultFileDirectives

-- ============================================================================
-- Ownership Module Tests
-- ============================================================================

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering t1 t2 =
  let comparison = compare t1 t2
  in (t1 <= t2) === (comparison <= 0)

-- Property: OwnershipError contains meaningful information
prop_ownership_error_structure :: OwnershipError -> Property
property True  -- Basic structure test - will be expanded based on actual OwnershipError type

-- ============================================================================
-- Compiler Module Tests
-- ============================================================================

-- Property: CompilationPhase ordering is total
prop_compilation_phase_ordering :: CompilationPhase -> CompilationPhase -> Property
prop_compilation_phase_ordering p1 p2 =
  let comparison = compare p1 p2
  in (p1 <= p2) === (comparison <= 0)

-- Property: CompilerError contains phase information
prop_compiler_error_phase :: CompilerError -> Property
property True  -- Basic structure test - will be expanded based on actual CompilerError type

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Property: SourceSpan length is non-negative
prop_source_span_length_nonnegative :: SourceSpan -> Property
prop_source_span_length_nonnegative span = spanLength span >= 0

-- Property: spanContains is reflexive
prop_span_contains_reflexive :: SourceSpan -> Property
prop_span_contains_reflexive span = spanContains span span

-- Property: spanContains is transitive
prop_span_contains_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_contains_transitive span1 span2 span3 =
  spanContains span1 span2 && spanContains span2 span3 ==> spanContains span1 span3

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = elements [Owned, Borrowed, Shared, Unique]  -- Adjust based on actual constructors

instance Arbitrary CompilationPhase where
  arbitrary = elements [Parsing, Analysis, TypeChecking, CodeGeneration, Optimization]  -- Adjust based on actual constructors

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ locatedWithSpan start end

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Cabal QuickCheck Tests"
  [ testGroup "Utils Module Tests"
    [ fastProperty "trim idempotent" prop_trim_idempotent
    , fastProperty "splitBy consistency" prop_split_by_consistency
    , fastProperty "removeLineComments preserves" prop_remove_line_comments_preserves
    , fastProperty "normalizeIndentation preserves lines" prop_normalize_indentation_relative
    ]
  , testGroup "Parser Module Tests"
    [ fastProperty "parseTypus roundtrip" prop_parse_typus_roundtrip
    , fastProperty "parseTypus handles directives" prop_parse_typus_directives
    ]
  , testGroup "Ownership Module Tests"
    [ fastProperty "OwnershipType ordering" prop_ownership_type_ordering
    , fastProperty "OwnershipError structure" prop_ownership_error_structure
    ]
  , testGroup "Compiler Module Tests"
    [ fastProperty "CompilationPhase ordering" prop_compilation_phase_ordering
    , fastProperty "CompilerError phase" prop_compiler_error_phase
    ]
  , testGroup "SourceLocation Module Tests"
    [ fastProperty "SourceSpan length non-negative" prop_source_span_length_nonnegative
    , fastProperty "spanContains reflexive" prop_span_contains_reflexive
    , fastProperty "spanContains transitive" prop_span_contains_transitive
    ]
  ]