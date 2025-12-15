{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import SourceLocation
import TestSupport.Arbitrary ()

prop_sourcePos_offset_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcePos_offset_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos l c (o + 1)
  in property $ posOffset pos1 < posOffset pos2

prop_sourceSpan_start_before_end :: SourceSpan -> Property
prop_sourceSpan_start_before_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_sourceSpan_contains_start :: SourceSpan -> Property
prop_sourceSpan_contains_start span =
  let start = spanStart span
      end = spanEnd span
  in posLine start <= posLine end .&&.
     (posLine start < posLine end .||. posColumn start <= posColumn end)

prop_located_preserves_pos :: Int -> SourcePos -> SourceSpan -> Property
prop_located_preserves_pos val pos span =
  let located = Located val pos span
  in locPos located === pos

prop_located_preserves_span :: Int -> SourcePos -> SourceSpan -> Property
prop_located_preserves_span val pos span =
  let located = Located val pos span
  in locSpan located === span

tests :: TestTree
tests = testGroup "SourceLocation Properties QuickCheck"
  [ fastProperty "SourcePos offset is monotonic" prop_sourcePos_offset_monotonic
  , fastProperty "SourceSpan start before end" prop_sourceSpan_start_before_end
  , fastProperty "SourceSpan contains start position" prop_sourceSpan_contains_start
  , fastProperty "Located preserves pos" prop_located_preserves_pos
  , fastProperty "Located preserves span" prop_located_preserves_span
  ]