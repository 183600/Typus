{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeComments
  , normalizeIndentation
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , emptySpan
  , spanFrom
  , mergeSpans
  , isValidSpan
  )

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler (CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipError(..))

import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Utils Properties (3 tests)
-- ============================================================================

-- Property: trim is idempotent - applying trim twice gives same result
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy L.and join are inverse operations
prop_split_by_join_inverse :: String -> Char -> Property
prop_split_by_join_inverse str delim =
  let segments = splitBy delim str
      rejoined = L.concat $ L.map (\s -> s ++ [delim]) (init segments) ++ [last segments]
  in not (null segments) ==> property $ splitBy delim rejoined === segments

-- Property: removeComments preserves non-comment code structure
prop_remove_comments_preserve_structure :: String -> Property
prop_remove_comments_preserve_structure code =
  let hasNoComments = not ("//" `L.isInfixOf` code) && not ("/*" `L.isInfixOf` code)
      cleaned = removeComments code
  in hasNoComments ==> property $ cleaned === code

-- ============================================================================
-- SourceLocation Properties (3 tests)
-- ============================================================================

-- Property: SourcePos ordering is consistent
prop_sourcepos_ordering :: Int -> Int -> Int -> Int -> Property
prop_sourcepos_ordering line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
  in (line1 < line2 || (line1 == line2 && col1 < col2)) ==> 
     property $ (pos1 < pos2) === True

-- Property: mergeSpans is associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let left = mergeSpans span1 (mergeSpans span2 span3)
      right = mergeSpans (mergeSpans span1 span2) span3
  in property $ left === right

-- Property: emptySpan is identity element for mergeSpans
prop_empty_span_identity :: SourceSpan -> Property
prop_empty_span_identity span =
  let left = mergeSpans emptySpan span
      right = mergeSpans span emptySpan
  in property $ left === span .&&. right === span

-- ============================================================================
-- Parser Properties (2 tests)
-- ============================================================================

-- Property: Default directives have consistent structure
prop_default_directives_consistent :: Property
prop_default_directives_consistent =
  let fileDefaults = defaultFileDirectives
      blockDefaults = defaultBlockDirectives
  in property $ isNothing (fdOwnership fileDefaults) .&&. 
                isNothing (fdDependentTypes fileDefaults) .&&.
                isNothing (fdConstraints fileDefaults) .&&.
                isNothing (bdOwnership blockDefaults) .&&.
                isNothing (bdDependentTypes blockDefaults) .&&.
                isNothing (bdConstraints blockDefaults)

-- Property: FileDirectives equality is reflexive
prop_file_directives_reflexive :: FileDirectives -> Property
prop_file_directives_reflexive directives =
  property $ directives === directives

-- ============================================================================
-- Compiler Properties (1 test)
-- ============================================================================

-- Property: CompilerError ordering respects phases
prop_compiler_error_phase_ordering :: CompilationPhase -> CompilationPhase -> String -> Property
prop_compiler_error_phase_ordering phase1 phase2 errorMsg =
  let error1 = CompilerError phase1 errorMsg
      error2 = CompilerError phase2 errorMsg
  in (phase1 < phase2) ==> property $ error1 < error2

-- ============================================================================
-- Ownership Properties (1 test)
-- ============================================================================

-- Property: OwnershipType ordering is total
prop_ownership_type_total_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_total_ordering owntype1 owntype2 =
  let ordering = [Owned, Borrowed, Shared, Moved]
      idx1 = L.length $ takeWhile (/= owntype1) ordering
      idx2 = L.length $ takeWhile (/= owntype2) ordering
  in property $ (owntype1 < owntype2) === (idx1 < idx2)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Core QuickCheck Properties"
  [ testGroup "Utils Properties"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy L.and join are inverse" prop_split_by_join_inverse
    , fastProperty "removeComments preserves structure" prop_remove_comments_preserve_structure
    ]
  , testGroup "SourceLocation Properties"
    [ fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
    , fastProperty "mergeSpans is associative" prop_merge_spans_associative
    , fastProperty "emptySpan is identity element" prop_empty_span_identity
    ]
  , testGroup "Parser Properties"
    [ fastProperty "default directives are consistent" prop_default_directives_consistent
    , fastProperty "FileDirectives equality is reflexive" prop_file_directives_reflexive
    ]
  , testGroup "Compiler Properties"
    [ fastProperty "CompilerError respects phase ordering" prop_compiler_error_phase_ordering
    ]
  , testGroup "Ownership Properties"
    [ fastProperty "OwnershipType has total ordering" prop_ownership_type_total_ordering
    ]
  ]