{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, isValidSpan, mergeSpans)
import Utils (trim, splitBy, splitByCollapsed)
import Data.Char (isSpace)
import Data.List (sort)

-- ============================================================================
-- SourceLocation QuickCheck Properties
-- ============================================================================

-- Property: startPos always returns position (1, 1)
prop_startPos_always_1_1 :: Property
prop_startPos_always_1_1 =
  let pos = startPos
  in property $ (posLine pos === 1) .&&. (posColumn pos === 1)

-- Property: emptySpan is always invalid
prop_emptySpan_always_invalid :: Property
prop_emptySpan_always_invalid =
  property $ not (isValidSpan emptySpan)

-- Property: spanFrom creates valid spans from valid positions
prop_spanFrom_creates_valid_span :: Int -> Int -> Property
prop_spanFrom_creates_valid_span line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      span = spanFrom pos
  in property $ isValidSpan span

-- Property: Located values preserve their content
prop_located_preserves_content :: String -> SourcePos -> Property
prop_located_preserves_content value pos =
  let located = Located pos value
  in property $ locatedValue located === value

-- ============================================================================
-- Utils QuickCheck Properties
-- ============================================================================

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      hasLeading = L.any isSpace prefix
      hasTrailing = L.any isSpace suffix
      trimmed = trim content
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy preserves the number of delimiters + 1
prop_splitBy_preserves_count :: Char -> String -> Property
prop_splitBy_preserves_count delim str =
  let result = splitBy delim str
      expectedCount = L.length (L.filter (== delim) str) + 1
  in property $ L.length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim str =
  let result = splitByCollapsed delim str
  in property $ L.all (not . null) result

-- Property: splitByCollapsed result L.length <= splitBy result L.length
prop_splitByCollapsed_shorter_or_equal :: Char -> String -> Property
prop_splitByCollapsed_shorter_or_equal delim str =
  let splitResult = splitBy delim str
      collapsedResult = splitByCollapsed delim str
  in property $ L.length collapsedResult <= L.length splitResult

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    pos <- arbitrary
    value <- arbitrary
    return $ Located pos value

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Core QuickCheck Tests"
    [ testGroup "SourceLocation Properties"
        [ fastProperty "startPos always returns (1, 1)" prop_startPos_always_1_1
        , fastProperty "emptySpan is always invalid" prop_emptySpan_always_invalid
        , fastProperty "spanFrom creates valid spans from valid positions" prop_spanFrom_creates_valid_span
        , fastProperty "Located values preserve their content" prop_located_preserves_content
        ]
    , testGroup "Utils Properties"
        [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
        , fastProperty "splitBy preserves delimiter count + 1" prop_splitBy_preserves_count
        , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
        , fastProperty "splitByCollapsed result is shorter L.or equal to splitBy" prop_splitByCollapsed_shorter_or_equal
        ]
    ]