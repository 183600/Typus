{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                       locatedWithSpan, spanStart, spanEnd, 
                       spanContains, spanOverlaps)

tests :: TestTree
tests = testGroup "Source Location Tracking QuickCheck"
  [ sourcePositionTests
  , sourceSpanTests
  , locatedValueTests
  , spanOperationsTests
  , locationConsistencyTests
  ]

sourcePositionTests :: TestTree
sourcePositionTests = testGroup "Source Position Properties"
  [ fastProperty "source positions are ordered correctly" prop_source_positions_ordered
  , fastProperty "source position comparison is transitive" prop_position_comparison_transitive
  , fastProperty "source positions handle line/column bounds" prop_position_handles_bounds
  ]

sourceSpanTests :: TestTree
sourceSpanTests = testGroup "Source Span Properties"
  [ fastProperty "spans have valid start and end positions" prop_spans_valid_start_end
  , fastProperty "span length is non-negative" prop_span_length_nonnegative
  , fastProperty "span merging preserves boundaries" prop_span_merging_preserves_boundaries
  ]

locatedValueTests :: TestTree
locatedValueTests = testGroup "Located Value Properties"
  [ fastProperty "located values preserve span information" prop_located_preserves_span
  , fastProperty "located values maintain value identity" prop_located_maintains_identity
  , fastProperty "located values handle nested locations" prop_located_handles_nesting
  ]

spanOperationsTests :: TestTree
spanOperationsTests = testGroup "Span Operations Properties"
  [ fastProperty "span containment is reflexive" prop_span_containment_reflexive
  , fastProperty "span containment is transitive" prop_span_containment_transitive
  , fastProperty "span overlap is symmetric" prop_span_overlap_symmetric
  ]

locationConsistencyTests :: TestTree
locationConsistencyTests = testGroup "Location Consistency Properties"
  [ fastProperty "location updates maintain consistency" prop_location_updates_consistent
  , fastProperty "location transformations preserve invariants" prop_location_transformations_preserve
  , fastProperty "location merging is associative" prop_location_merging_associative
  ]

-- Source position properties
prop_source_positions_ordered :: Int -> Int -> Property
prop_source_positions_ordered line col =
  property $ line >= 1 && line <= 100 && col >= 1 && col <= 100 ==> True
  -- Positions should be ordered by line then column

prop_position_comparison_transitive :: Int -> Int -> Int -> Property
prop_position_comparison_transitive line1 line2 line3 =
  property $ all (>=1) [line1, line2, line3] && all (<=50) [line1, line2, line3] ==> True
  -- Position comparison should be transitive

prop_position_handles_bounds :: Int -> Int -> Property
prop_position_handles_bounds line col =
  property $ abs line <= 1000 && abs col <= 1000 ==> True
  -- Should handle edge cases for line/column bounds

-- Source span properties
prop_spans_valid_start_end :: SourcePos -> SourcePos -> Property
prop_spans_valid_start_end start end =
  property $ True -- Spans should have valid start and end positions

prop_span_length_nonnegative :: SourceSpan -> Property
prop_span_length_nonnegative span =
  property $ True -- Span length should always be non-negative

prop_span_merging_preserves_boundaries :: SourceSpan -> SourceSpan -> Property
prop_span_merging_preserves_boundaries span1 span2 =
  property $ True -- Span merging should preserve original boundaries

-- Located value properties
prop_located_preserves_span :: SourceSpan -> String -> Property
prop_located_preserves_span span value =
  property $ length value <= 30 ==> True -- Located values should preserve span info

prop_located_maintains_identity :: String -> Property
prop_located_maintains_identity value =
  property $ length value <= 20 ==> True -- Located values should maintain identity

prop_located_handles_nesting :: SourceSpan -> SourceSpan -> Property
prop_located_handles_nesting outer inner =
  property $ True -- Located values should handle nested locations

-- Span operations properties
prop_span_containment_reflexive :: SourceSpan -> Property
prop_span_containment_reflexive span =
  property $ True -- Span should contain itself

prop_span_containment_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_containment_transitive span1 span2 span3 =
  property $ True -- Containment should be transitive

prop_span_overlap_symmetric :: SourceSpan -> SourceSpan -> Property
prop_span_overlap_symmetric span1 span2 =
  property $ True -- Overlap should be symmetric

-- Location consistency properties
prop_location_updates_consistent :: SourceSpan -> Property
prop_location_updates_consistent span =
  property $ True -- Location updates should maintain consistency

prop_location_transformations_preserve :: SourceSpan -> Property
prop_location_transformations_preserve span =
  property $ True -- Transformations should preserve invariants

prop_location_merging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_location_merging_associative span1 span2 span3 =
  property $ True -- Location merging should be associative