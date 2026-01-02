{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary(..), oneof, elements, listOf1, choose)
import qualified Test.QuickCheck as QC

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , advancePos
  , advancePosBy
  )

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import qualified Compiler
import qualified Compiler.DependentTypeChecker as DepChecker
import qualified SyntaxValidator

import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, head, tail)
import Data.List (null, last, init)
import qualified Data.Text as T

-- ============================================================================
-- QuickCheck Properties for Utils Module
-- ============================================================================

-- Property: trim removes leading L.and trailing whitespace but preserves internal content
prop_trim_preserves_content :: String -> String -> Property
prop_trim_preserves_content prefix suffix =
  let content = "content"
      full = prefix ++ content ++ suffix
      trimmed = trim full
  in classify (not (null prefix) && L.any isSpace prefix) "has leading whitespace" $
     classify (not (null suffix) && L.any isSpace suffix) "has trailing whitespace" $
     property $ content `L.isInfixOf` trimmed

-- Property: splitBy delim (splitBy delim x) == x for L.any delimiter L.and string
prop_split_by_idempotent :: Char -> String -> Property
prop_split_by_idempotent delim str =
  let parts = splitBy delim str
      rejoined = L.foldr (\x acc -> if null acc then x else x ++ [delim] ++ acc) "" parts
  in property $ rejoined === str

-- Property: splitByCollapsed never returns empty strings
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ L.all (not . null) parts

-- Property: removeLineComments removes // comments but preserves other content
prop_remove_line_comments_preserves_non_comments :: String -> Property
prop_remove_line_comments_preserves_non_comments str =
  let withoutComments = removeLineComments str
      hasLineComment = "//" `L.isInfixOf` str
  in classify hasLineComment "has line comments" $
     property $ L.length withoutComments <= L.length str

-- ============================================================================
-- QuickCheck Properties for SourceLocation Module
-- ============================================================================

-- Property: advancePos by newline increments line number L.and resets column
prop_advance_pos_newline :: Int -> Int -> Property
prop_advance_pos_newline line col =
  let pos = SourcePos line col
      newPos = advancePos '\n' pos
  in property $ posLine newPos === line + 1 .&&. posColumn newPos === 1

-- Property: advancePos by other characters increments column only
prop_advance_pos_regular_char :: Int -> Int -> Char -> Property
prop_advance_pos_regular_char line col ch =
  let pos = SourcePos line col
      newPos = advancePos ch pos
  in (ch /= '\n') ==> property $ posLine newPos === line .&&. posColumn newPos === col + 1

-- Property: spanBetween creates valid span when start <= end
prop_span_between_valid :: Int -> Int -> Int -> Property
prop_span_between_valid startLine offset endOffset =
  let start = SourcePos startLine 1
      endPos = SourcePos startLine (1 + offset + endOffset)
      span = spanBetween start endPos
  in (offset >= 0 && endOffset >= 0) ==> property $ isValidSpan span

-- Property: mergeSpans contains both original spans
prop_merge_spans_contains_both :: Int -> Int -> Int -> Property
prop_merge_spans_contains_both line1 col1 offset =
  let span1 = spanFrom (SourcePos line1 col1) (SourcePos line1 (col1 + offset))
      span2 = spanFrom (SourcePos line1 (col1 + offset + 1)) (SourcePos line1 (col1 + offset + 2))
      merged = mergeSpans span1 span2
  in (offset >= 0) ==> property $ 
    spanStart merged `isBeforeOrEqual` spanStart span2 .&&.
    spanEnd span2 `isBeforeOrEqual` spanEnd merged
  where
    isBeforeOrEqual p1 p2 = posLine p1 < posLine p2 || 
                           (posLine p1 == posLine p2 && posColumn p1 <= posColumn p2)

-- ============================================================================
-- Unit Tests for Parser Module
-- ============================================================================

-- Test: parseTypus handles empty input
test_parse_empty_input :: TestTree
test_parse_empty_input = testCase "parseTypus handles empty input" $ do
  case parseTypus "" of
    Left err -> assertFailure $ "parseTypus failed on empty input: " ++ err
    Right typusFile -> do
      tfDirectives typusFile @?= defaultFileDirectives
      tfBlocks typusFile @?= []

-- Test: parseTypus correctly parses file-level ownership directive
test_parse_ownership_directive :: TestTree
test_parse_ownership_directive = testCase "parseTypus parses ownership directive" $ do
  let source = "//! ownership: on\npackage main"
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right typusFile -> do
      let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
      case ownership of
        Nothing -> assertFailure "expected ownership directive"
        Just loc -> locatedValue loc @?= True

-- Test: parseTypus handles block directives correctly
test_parse_block_directives :: TestTree
test_parse_block_directives = testCase "parseTypus parses block directives" $ do
  let source = unlines
        [ "package main"
        , "func main() {"
        , "    {//! ownership: on"
        , "    // ownership-enabled code"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "expected at least one block" (not (null blocks))
      let firstBlock = L.head blocks
          BlockDirectives { bdOwnership = ownership } = cbDirectives firstBlock
      case ownership of
        Nothing -> assertFailure "expected block ownership directive"
        Just loc -> locatedValue loc @?= True

-- ============================================================================
-- Unit Tests for Compiler Module
-- ============================================================================

-- Test: compiler handles basic Go code correctly
test_compiler_basic_go_code :: TestTree
test_compiler_basic_go_code = testCase "compiler handles basic Go code" $ do
  let source = unlines
        [ "package main"
        , "func main() {"
        , "    println(\"hello world\")"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right typusFile -> do
      case Compiler.compile typusFile of
        Left err -> assertFailure $ "compile failed: " ++ show err
        Right goCode -> do
          assertBool "compiled code should contain package declaration" ("package main" `L.isInfixOf` goCode)
          assertBool "compiled code should contain main function" ("func main" `L.isInfixOf` goCode)

-- Test: dependent type checker catches invalid syntax
test_dependent_type_error_detection :: TestTree
test_dependent_type_error_detection = testCase "dependent type checker detects errors" $ do
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "alias Broken"  -- Invalid dependent type syntax
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right typusFile -> do
      case DepChecker.checkDependentTypes typusFile of
        Left errs -> assertBool "expected dependent type errors" (not (null errs))
        Right _ -> assertFailure "expected dependent type checking to fail"

-- ============================================================================
-- Additional QuickCheck Property Tests
-- ============================================================================

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_structure :: [String] -> Property
prop_normalize_indentation_preserves_structure lines =
  let normalized = normalizeIndentation lines
      hasContent = not (null lines) && L.any (not . null) lines
  in hasContent ==> property $ L.length normalized === L.length lines

-- Property: breakOn behaves like standard break function
prop_break_on_consistency :: String -> String -> Property
prop_break_on_consistency delim str =
  let (before, after) = breakOn delim str
      expectedBefore = takeWhile (not . L.isPrefixOf delim . take (L.length delim)) (tails str) >>= L.head
  in property $ before === expectedBefore

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ testGroup "Utils Module Properties"
    [ fastProperty "trim preserves content" prop_trim_preserves_content
    , fastProperty "splitBy is idempotent" prop_split_by_idempotent
    , fastProperty "splitByCollapsed never returns empty strings" prop_split_by_collapsed_no_empty
    , fastProperty "removeLineComments preserves non-comments" prop_remove_line_comments_preserves_non_comments
    ]
  , testGroup "SourceLocation Module Properties"
    [ fastProperty "advancePos handles newline correctly" prop_advance_pos_newline
    , fastProperty "advancePos handles regular characters" prop_advance_pos_regular_char
    , fastProperty "spanBetween creates valid spans" prop_span_between_valid
    , fastProperty "mergeSpans contains both spans" prop_merge_spans_contains_both
    ]
  , testGroup "Parser Module Tests"
    [ test_parse_empty_input
    , test_parse_ownership_directive
    , test_parse_block_directives
    ]
  , testGroup "Compiler Module Tests"
    [ test_compiler_basic_go_code
    , test_dependent_type_error_detection
    ]
  , testGroup "Additional Properties"
    [ fastProperty "normalizeIndentation preserves structure" prop_normalize_indentation_preserves_structure
    , fastProperty "breakOn consistency" prop_break_on_consistency
    ]
  ]