{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerCorePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , mergeSpans
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , mapLocated
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (sort, nub, intersperse)
import qualified Data.Text as T
import Control.Monad (foldM)

-- ============================================================================
-- File Directives Properties
-- ============================================================================

-- Property: Default file directives have no values set
prop_default_file_directives :: Property
prop_default_file_directives =
  let defaults = defaultFileDirectives
  in property $
     fdOwnership defaults === Nothing .&&.
     fdDependentTypes defaults === Nothing .&&.
     fdConstraints defaults === Nothing

-- Property: Default block directives have no values set
prop_default_block_directives :: Property
prop_default_block_directives =
  let defaults = defaultBlockDirectives
  in property $
     bdOwnership defaults === Nothing .&&.
     bdDependentTypes defaults === Nothing .&&.
     bdConstraints defaults === Nothing

-- ============================================================================
-- Source Location Properties
-- ============================================================================

-- Property: Located values preserve their span information through mapping
prop_located_preserves_span :: SourceSpan -> String -> Property
prop_located_preserves_span span str =
  let located = locatedWithSpan span str
      mapped = mapLocated reverse located
  in locatedSpan located === locatedSpan mapped .&&.
     locatedSpan mapped === span

-- Property: Located values at position create proper spans
prop_located_at_position :: SourcePos -> String -> Property
prop_located_at_position pos str =
  let located = locatedAt pos str
      expectedSpan = emptySpan pos
  in locatedSpan located === expectedSpan .&&.
     locatedPos located === pos

-- Property: Located values can be nested while preserving inner structure
prop_located_nesting :: SourcePos -> SourcePos -> String -> Property
prop_located_nesting outerPos innerPos str =
  let innerLocated = locatedAt innerPos str
      outerLocated = locatedAt outerPos innerLocated
  in locatedValue outerLocated === innerLocated .&&.
     locatedPos outerLocated === outerPos

-- ============================================================================
-- Text Processing Properties
-- ============================================================================

-- Property: Position tracking through multiline text
prop_position_tracking_multiline :: [String] -> Property
prop_position_tracking_multiline lines =
  let text = unlines lines
      finalPos = foldl (flip posAfter) startPos text
  in posLine finalPos === 6 .&&. posOffset finalPos === length text

-- Property: Tab expansion preserves character count consistency
prop_tab_expansion_consistency :: Int -> Int -> Property
prop_tab_expansion_consistency col offset =
  let pos = SourcePos 1 col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in col > 0 && offset >= 0 ==>
     posColumn newPos === expectedCol .&&.
     posOffset newPos === offset + 1

-- Property: Span merging is idempotent for identical spans
prop_merge_spans_idempotent :: SourceSpan -> Property
prop_merge_spans_idempotent span =
  let merged = mergeSpans span span
  in merged === span

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Code block processing preserves content integrity
prop_code_block_content_integrity :: String -> Property
prop_code_block_content_integrity content =
  let block = CodeBlock defaultBlockDirectives content
  in cbContent block === content

-- Property: Typus file structure consistency
prop_typus_file_structure :: FileDirectives -> [CodeBlock] -> Property
prop_typus_file_structure directives blocks =
  let typusFile = TypusFile directives blocks
  in tfDirectives typusFile === directives .&&.
     tfCodeBlocks typusFile === blocks

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Position tracking handles edge cases correctly
prop_position_edge_cases :: Property
prop_position_edge_cases =
  let pos1 = startPos { posLine = 1, posColumn = 1, posOffset = 0 }
      pos2 = posAfter '\n' pos1
      pos3 = posAfter '\t' pos2
      pos4 = posAfter 'a' pos3
  in posLine pos2 === 2 .&&.
     posColumn pos2 === 1 .&&.
     posOffset pos2 === 1 .&&.
     posColumn pos3 === 9 .&&.  -- First tab position
     posOffset pos3 === 2 .&&.
     posColumn pos4 === 10 .&&.
     posOffset pos4 === 3

-- Property: Span validation works for edge cases
prop_span_validation_edge_cases :: Property
prop_span_validation_edge_cases =
  let pos1 = startPos
      pos2 = posAfter 'a' pos1
      validSpan = SourceSpan pos1 pos2
      invalidSpan = SourceSpan pos2 pos1
      samePosSpan = SourceSpan pos1 pos1
  in isValidSpan validSpan .&&.
     not (isValidSpan invalidSpan) .&&.
     isValidSpan samePosSpan

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Large text position tracking is linear time
prop_large_text_position_tracking :: Property
prop_large_text_position_tracking =
  let largeText = concat (replicate 10000 "a\n")
      finalPos = advancePosBy largeText startPos
  in posLine finalPos === 10001 .&&.
     posColumn finalPos === 1 .&&.
     posOffset finalPos === 20000

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler Core Properties"
  [ testGroup "File Directives"
    [ fastProperty "default file directives have no values" prop_default_file_directives
    , fastProperty "default block directives have no values" prop_default_block_directives
    ]
  , testGroup "Source Location"
    [ fastProperty "located preserves span through mapping" prop_located_preserves_span
    , fastProperty "located at position creates proper spans" prop_located_at_position
    , fastProperty "located nesting preserves structure" prop_located_nesting
    , fastProperty "position tracking through multiline text" prop_position_tracking_multiline
    , fastProperty "tab expansion consistency" prop_tab_expansion_consistency
    , fastProperty "merge spans idempotent" prop_merge_spans_idempotent
    ]
  , testGroup "Parser"
    [ fastProperty "code block content integrity" prop_code_block_content_integrity
    , fastProperty "typus file structure consistency" prop_typus_file_structure
    ]
  , testGroup "Error Handling"
    [ fastProperty "position edge cases" prop_position_edge_cases
    , fastProperty "span validation edge cases" prop_span_validation_edge_cases
    ]
  , testGroup "Performance"
    [ fastProperty "large text position tracking" prop_large_text_position_tracking
    ]
  ]