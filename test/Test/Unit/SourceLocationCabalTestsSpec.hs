{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
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
  )

import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Additional Cabal Tests for SourceLocation Module
-- ============================================================================

-- | Test case 1: Source position arithmetic with newlines and tabs
test_posAfter_special_chars :: TestTree
test_posAfter_special_chars = testCase "posAfter handles special characters correctly" $ do
    let pos = SourcePos 5 10 100
    assertEqual "newline increments line, resets column"
        (SourcePos 6 1 101)
        (posAfter '\n' pos)
    assertEqual "tab increments column by 1 (simple handling)"
        (SourcePos 5 11 101)
        (posAfter '\t' pos)
    assertEqual "regular char increments column"
        (SourcePos 5 11 101)
        (posAfter 'a' pos)

-- | Test case 2: Span validity edge cases
test_isValidSpan_edge_cases :: TestTree
test_isValidSpan_edge_cases = testCase "isValidSpan handles edge cases" $ do
    let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
    let invalidSpan = SourceSpan (SourcePos 2 1 10) (SourcePos 1 5 4)
    let samePosSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
    
    assertBool "valid span is valid" $ isValidSpan validSpan
    assertBool "invalid span (end before start) is not valid" $ not $ isValidSpan invalidSpan
    assertBool "same position span is valid" $ isValidSpan samePosSpan

-- | Test case 3: Span merging with overlapping ranges
test_mergeSpans_overlapping :: TestTree
test_mergeSpans_overlapping = testCase "mergeSpans handles overlapping spans" $ do
    let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
    let span2 = SourceSpan (SourcePos 1 5 4) (SourcePos 1 15 14)
    let expected = SourceSpan (SourcePos 1 1 0) (SourcePos 1 15 14)
    
    assertEqual "overlapping spans merge correctly" expected (mergeSpans span1 span2)

-- | Test case 4: Position advancement by text with Unicode
test_advancePosByText_unicode :: TestTree
test_advancePosByText_unicode = testCase "advancePosByText handles Unicode text" $ do
    let pos = startPos
    let unicodeText = "hello世界\nnext"
    let result = advancePosByText unicodeText pos
    
    assertEqual "Unicode text advances position correctly"
        (SourcePos 2 6 12) -- "hello世界" (8 chars) + "\n" (1) + "next" (4) = 13, but offset is 12-based
        result

-- | Test case 5: Located value mapping preserves location
test_mapLocated_preserves_location :: TestTree
test_mapLocated_preserves_location = testCase "mapLocated preserves location information" $ do
    let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
    let located = Located span "hello"
    let mapped = mapLocated (reverse . map toUpper) located
    
    assertEqual "mapping preserves span" span (locatedSpan mapped)
    assertEqual "mapping transforms value correctly" "OLLEH" (locatedValue mapped)
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Test case 6: Property test for span ordering
prop_span_ordering_consistency :: SourcePos -> SourcePos -> Property
prop_span_ordering_consistency pos1 pos2 =
    let span = SourceSpan pos1 pos2
        valid = isValidSpan span
    in property $ valid ==> (pos1 <= pos2)

-- | Test case 7: Property test for position advancement monotonicity
prop_advancePos_monotonic :: Char -> SourcePos -> Property
prop_advancePos_monotonic char pos =
    let newPos = posAfter char pos
    in property $ posOffset newPos > posOffset pos

-- | Test case 8: Property test for span merging commutativity
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
    let merge1 = mergeSpans span1 span2
        merge2 = mergeSpans span2 span1
    in property $ merge1 === merge2

-- | Test case 9: Property test for located value extraction
prop_located_value_roundtrip :: String -> SourceSpan -> Property
prop_located_value_roundtrip value span =
    let located = Located span value
        extractedValue = locatedValue located
        extractedSpan = locatedSpan located
    in property $ value === extractedValue .&&. span === extractedSpan

-- | Test case 10: Test position at specific line and column
test_posAtLineCol_accuracy :: TestTree
test_posAtLineCol_accuracy = testCase "posAtLineCol calculates correct offset" $ do
    let text = "hello\nworld\n\ntest"
    let pos1 = posAtLineCol 1 3 text
    let pos2 = posAtLineCol 2 2 text
    let pos3 = posAtLineCol 4 1 text
    
    assertEqual "line 1, column 3" (SourcePos 1 3 2) pos1
    assertEqual "line 2, column 2" (SourcePos 2 2 7) pos2
    assertEqual "line 4, column 1" (SourcePos 4 1 12) pos3

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Cabal Tests"
    [ testGroup "Unit Tests"
        [ test_posAfter_special_chars
        , test_isValidSpan_edge_cases
        , test_mergeSpans_overlapping
        , test_advancePosByText_unicode
        , test_mapLocated_preserves_location
        , test_posAtLineCol_accuracy
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "span ordering consistency" prop_span_ordering_consistency
        , fastProperty "position advancement monotonicity" prop_advancePos_monotonic
        , fastProperty "span merging commutativity" prop_mergeSpans_commutative
        , fastProperty "located value roundtrip" prop_located_value_roundtrip
        ]
    ]