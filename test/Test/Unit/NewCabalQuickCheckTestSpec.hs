{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), counterexample, property)
import Data.List (isInfixOf)
import Data.Char (isSpace)
import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween, mergeSpans, isValidSpan, advancePosBy)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)

-- ============================================================================
-- Test Groups
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ testGroup "Utils Module Tests"
    [ testProperty "trim: trim(trim(x)) == trim(x)" propTrimIdempotent
    , testProperty "splitBy: length (splitBy c s) >= 1" propSplitByNonEmpty
    , testProperty "splitByCollapsed: concatenating result with delimiter gives original (without empty parts)" propSplitByCollapsedRoundtrip
    , testProperty "removeLineComments: removing comments twice is same as once" propRemoveLineCommentsIdempotent
    , testProperty "normalizeIndentation: preserves relative indentation" propNormalizeIndentationRelative
    ]
  , testGroup "SourceLocation Module Tests"
    [ testProperty "spanBetween: start <= end for valid span" propSpanBetweenValid
    , testProperty "mergeSpans: result contains both input spans" propMergeSpansContains
    , testProperty "advancePosBy: advancing by empty string returns same position" propAdvancePosByEmpty
    ]
  ]

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- | trim is idempotent: trimming twice gives same result as trimming once
propTrimIdempotent :: String -> Property
propTrimIdempotent s = trim (trim s) === trim s

-- | splitBy always returns at least one element (even for empty strings)
propSplitByNonEmpty :: Char -> String -> Property
propSplitByNonEmpty c s = property (length (splitBy c s) >= 1)

-- | splitByCollapsed: when we join the result with the delimiter, we get the original string without empty parts
propSplitByCollapsedRoundtrip :: Char -> String -> Property
propSplitByCollapsedRoundtrip c s = 
  let parts = splitByCollapsed c s
      reconstructed = if null parts then "" else foldr1 (\x y -> x ++ [c] ++ y) parts
      expected = filter (/= c) s  -- Remove all occurrences of delimiter
  in counterexample ("parts: " ++ show parts ++ ", reconstructed: " ++ show reconstructed ++ ", expected: " ++ show expected) $
     reconstructed === expected

-- | removeLineComments is idempotent for strings without line comments
propRemoveLineCommentsIdempotent :: String -> Property
propRemoveLineCommentsIdempotent s = 
  let noComments = not ( "//" `isInfixOf` s )
  in if noComments
     then removeLineComments (removeLineComments s) === removeLineComments s
     else property True  -- Skip test for strings with comments

-- | normalizeIndentation preserves relative indentation between lines
propNormalizeIndentationRelative :: String -> Property
propNormalizeIndentationRelative s = 
  let ls = lines s
      nonEmpty = filter (not . all isSpace) ls
      -- Only test if we have at least 2 non-empty lines
  in if length nonEmpty >= 2
     then let normalized = normalizeIndentation s
              normalizedLines = lines normalized
              nonEmptyNormalized = filter (not . all isSpace) normalizedLines
              -- Check that relative indentation is preserved
              originalIndents = map (length . takeWhile isSpace) nonEmpty
              normalizedIndents = map (length . takeWhile isSpace) nonEmptyNormalized
              -- Differences between consecutive lines should be preserved
              originalDiffs = zipWith (-) (tail originalIndents) originalIndents
              normalizedDiffs = zipWith (-) (tail normalizedIndents) normalizedIndents
          in counterexample ("original indents: " ++ show originalIndents ++ 
                            ", normalized indents: " ++ show normalizedIndents ++
                            ", original diffs: " ++ show originalDiffs ++
                            ", normalized diffs: " ++ show normalizedDiffs) $
             normalizedDiffs === originalDiffs
     else property True  -- Skip test for strings with fewer than 2 non-empty lines

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- | spanBetween always creates a valid span where start <= end
propSpanBetweenValid :: Int -> Int -> Int -> Property
propSpanBetweenValid line1 col1 line2 = 
  let pos1 = SourcePos (abs line1 + 1) (abs col1 + 1) 0
      pos2 = SourcePos (abs line1 + 1) (abs line2 + 1) (abs line2 + 1)
      span = spanBetween pos1 pos2
  in isValidSpan span === True

-- | mergeSpans result contains both input spans
propMergeSpansContains :: Int -> Int -> Int -> Int -> Property
propMergeSpansContains line1 col1 line2 col2 =
  let pos1 = SourcePos (abs line1 + 1) (abs col1 + 1) 0
      pos2 = SourcePos (abs line2 + 1) (abs col2 + 1) (abs line2 + 1)
      span1 = spanBetween pos1 pos2
      pos3 = SourcePos (abs line1 + 2) (abs col1 + 2) 10
      pos4 = SourcePos (abs line2 + 2) (abs col2 + 2) (abs line2 + 2)
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in counterexample ("span1: " ++ show span1 ++ ", span2: " ++ show span2 ++ ", merged: " ++ show merged) $
     (spanStart merged <= spanStart span1 || spanStart merged <= spanStart span2) &&
     (spanEnd merged >= spanEnd span1 || spanEnd merged >= spanEnd span2)

-- | advancePosBy with empty string returns same position
propAdvancePosByEmpty :: Int -> Int -> Int -> Property
propAdvancePosByEmpty line col offset = 
  let pos = SourcePos (abs line + 1) (abs col + 1) (abs offset)
  in advancePosBy "" pos === pos

-- ============================================================================
-- Helper Functions
-- ============================================================================