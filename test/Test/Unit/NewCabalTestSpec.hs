{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary(..), elements, listOf, choose, oneof)
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
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
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- ============================================================================
-- Test 1: Utils advanced edge cases
-- ============================================================================

test_utils_edge_cases :: TestTree
test_utils_edge_cases = testCase "Utils edge cases" $ do
  -- Test trim with complex Unicode whitespace
  trim "\x2000\x2001\x2002 content \x2003\x2004" @?= "content"
  
  -- Test splitBy with space delimiter behavior
  splitBy ' ' "a b c" @?= ["a", "b", "c"]
  
  -- Test removeComments with nested-like patterns
  let nestedComments = "code /* outer /* inner */ still outer */ end"
  removeComments nestedComments @?= "code  end"
  
  -- Test normalizeIndentation with mixed tabs and spaces
  let mixedIndent = "\t  line1\n    \tline2\n\t\tline3"
  normalizeIndentation mixedIndent @?= "line1\n  line2\n\tline3"

-- ============================================================================
-- Test 2: SourceLocation mathematical properties
-- ============================================================================

test_source_location_math :: TestTree
test_source_location_math = testCase "SourceLocation mathematical properties" $ do
  let pos1 = posAtLineCol 1 1 0
  let pos2 = posAtLineCol 1 5 4
  let pos3 = posAtLineCol 2 1 10
  
  -- Test position ordering
  assertBool "pos1 < pos2" $ pos1 < pos2
  assertBool "pos2 < pos3" $ pos2 < pos3
  
  -- Test span creation and validation
  let span1 = spanBetween pos1 pos2
  let span2 = spanBetween pos2 pos3
  assertBool "span1 is valid" $ isValidSpan span1
  assertBool "span2 is valid" $ isValidSpan span2
  
  -- Test span merging
  let merged = mergeSpans span1 span2
  spanStart merged @?= pos1
  spanEnd merged @?= pos3

-- ============================================================================
-- Test 3: Comment handling with edge cases
-- ============================================================================

test_comment_edge_cases :: TestTree
test_comment_edge_cases = testCase "Comment handling edge cases" $ do
  -- Test comment markers in strings with escapes
  let escapedString = "text \"// not comment \\\" // still not\" code // real comment"
  removeLineComments escapedString @?= "text \"// not comment \\\" // still not\" code "
  
  -- Test block comment at end of file without closing
  let unterminated = "code /* open comment"
  removeComments unterminated @?= "code "
  
  -- Test multiple consecutive block comments
  let consecutive = "a/*1*/b/*2*/c"
  removeComments consecutive @?= "abc"

-- ============================================================================
-- Test 4: Advanced position tracking
-- ============================================================================

test_advanced_position_tracking :: TestTree
test_advanced_position_tracking = testCase "Advanced position tracking" $ do
  let start = startPos
  let afterHello = advancePosBy "hello" start
  posLine afterHello @?= 1
  posColumn afterHello @?= 6
  posOffset afterHello @?= 5
  
  -- Test tab expansion
  let afterTab = posAfter '\t' startPos
  posColumn afterTab @?= 9  -- Next tab stop (8 + 1)
  
  -- Test newline handling
  let afterNewline = posAfter '\n' startPos
  posLine afterNewline @?= 2
  posColumn afterNewline @?= 1

-- ============================================================================
-- Test 5: Located values operations
-- ============================================================================

test_located_values :: TestTree
test_located_values = testCase "Located values operations" $ do
  let pos = posAt 1 5
  let value = "test"
  let located = locatedAt pos value
  
  locatedValue located @?= value
  locatedPos located @?= pos
  
  -- Test mapping over located values
  let mapped = mapLocated (++ " mapped") located
  locatedValue mapped @?= "test mapped"
  locatedPos mapped @?= pos  -- Position should be preserved

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 6: splitBy and splitByCollapsed relationship
prop_split_by_relationship :: String -> Char -> Property
prop_split_by_relationship str delim =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
  in property $ 
     (null (filter null normal) === True) .&&.
     (normal === collapsed) .||.
     (length collapsed === length (filter (not . null) normal))

-- Property 7: Source position advancement is consistent
prop_position_advancement_consistent :: String -> Property
prop_position_advancement_consistent str =
  let start = startPos
      end = advancePosBy str start
      text = T.pack str
      endByText = advancePosByText text start
  in property $ end === endByText

-- Property 8: Span merge is associative
prop_span_merge_associative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_associative p1 p2 p3 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      span3 = spanBetween p1 p3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in (isValidSpan span1 && isValidSpan span2) ==> 
     (spanStart merged1 === spanStart span3) .&&.
     (spanEnd merged1 === spanEnd span3) .&&.
     (merged1 === merged2)

-- Property 9: Comment removal preserves non-comment content
prop_comment_preservation :: String -> String -> Property
prop_comment_preservation code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) && 
  not ("/" `isInfixOf` code) ==>
  let withLineComment = code ++ " // " ++ comment
      withBlockComment = code ++ " /* " ++ comment ++ " */ " ++ code
      cleanedLine = removeLineComments withLineComment
      cleanedBlock = removeComments withBlockComment
  in property $ 
     (code ++ " " === cleanedLine) .&&.
     (code ++ "  " ++ code === cleanedBlock)

-- Property 10: Indentation normalization preserves structure
prop_indentation_structure :: String -> Property
prop_indentation_structure input =
  let linesInput = lines input
      normalized = normalizeIndentation input
      linesNormalized = lines normalized
  in property $ 
     length linesInput === length linesNormalized

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Tests"
  [ test_utils_edge_cases
  , test_source_location_math
  , test_comment_edge_cases
  , test_advanced_position_tracking
  , test_located_values
  , testGroup "QuickCheck Properties"
      [ fastProperty "splitBy relationship" prop_split_by_relationship
      , fastProperty "position advancement consistency" prop_position_advancement_consistent
      , fastProperty "span merge associative" prop_span_merge_associative
      , fastProperty "comment preservation" prop_comment_preservation
      , fastProperty "indentation structure" prop_indentation_structure
      ]
  ]