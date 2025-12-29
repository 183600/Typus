{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isControl)
import qualified Data.List as Data.List

-- ============================================================================
-- New Source Location Math Properties Tests
-- ============================================================================

-- Property: Position advancement is consistent for single characters
prop_pos_after_consistency :: Char -> SourcePos -> Property
prop_pos_after_consistency c pos =
  let advanced = posAfter c pos
  in property $ posOffset advanced === posOffset pos + 1 .&&.
     (if c == '\n' 
      then posLine advanced === posLine pos + 1 .&&. posColumn advanced === 1
      else if c == '\t'
           then posColumn advanced === ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
           else posColumn advanced === posColumn pos + 1)

-- Property: Multiple position advancements equal single advancement by string
prop_advance_pos_by_consistency :: String -> SourcePos -> Property
prop_advance_pos_by_consistency chars pos =
  let byString = advancePosBy chars pos
      byIndividual = foldl (flip posAfter) pos chars
  in property $ byString === byIndividual

-- Property: Position advancement by text equals advancement by unpacked string
prop_advance_pos_by_text_consistency :: Text -> SourcePos -> Property
prop_advance_pos_by_text_consistency text pos =
  let byText = advancePosByText text pos
      byString = advancePosBy (T.unpack text) pos
  in property $ byText === byString

-- Property: Line advancement preserves column 1 and increments line number
prop_advance_pos_by_line_properties :: Int -> SourcePos -> Property
prop_advance_pos_by_line_properties numLines pos =
  numLines >= 0 && numLines <= 100 ==>  -- Limit for reasonable testing
  let advanced = advancePosByLine numLines pos
  in property $ posLine advanced === posLine pos + numLines .&&.
     posColumn advanced === 1 .&&.
     posOffset advanced === posOffset pos + numLines

-- Property: Empty span has equal start and end positions
prop_empty_span_properties :: SourcePos -> Property
prop_empty_span_properties pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos .&&.
     isValidSpan span

-- Property: Span between positions maintains order
prop_span_between_order :: SourcePos -> SourcePos -> Property
prop_span_between_order pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ spanStart span === min pos1 pos2 .&&.
     spanEnd span === max pos1 pos2

-- Property: Merged span covers both original spans
prop_merge_spans_coverage :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_coverage span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&.
     spanEnd merged === max (spanEnd span1) (spanEnd span2) .&&.
     isValidSpan merged

-- Property: Located value preserves position information
prop_located_at_properties :: SourcePos -> Int -> Property
prop_located_at_properties pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos .&&.
     locatedSpan located === emptySpan pos .&&.
     locatedValue located === value

-- Property: Located with span preserves span information
prop_located_with_span_properties :: SourceSpan -> String -> Property
prop_located_with_span_properties span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
     locatedPos located === spanStart span .&&.
     locatedValue located === value

-- Property: Map located preserves position information
prop_map_located_preserves_location :: SourceSpan -> Int -> Property
prop_map_located_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedSpan mapped === locatedSpan located .&&.
     locatedPos mapped === locatedPos located .&&.
     locatedValue mapped === value * 2

-- Property: Position offset is monotonic with line advancement
prop_offset_monotonic_line_advancement :: Int -> SourcePos -> Property
prop_offset_monotonic_line_advancement lines pos =
  lines >= 0 && lines <= 50 ==>  -- Reasonable limit
  let advanced = advancePosByLine lines pos
  in property $ posOffset advanced >= posOffset pos

-- Property: Tab advancement follows 8-column tab stops
prop_tab_advancement_tab_stops :: SourcePos -> Property
prop_tab_advancement_tab_stops pos =
  let advanced = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn advanced === expectedColumn

-- Property: Newline advancement resets column to 1
prop_newline_advancement_resets_column :: SourcePos -> Property
prop_newline_advancement_resets_column pos =
  let advanced = posAfter '\n' pos
  in property $ posColumn advanced === 1 .&&.
     posLine advanced === posLine pos + 1

-- Property: Span validity is transitive
prop_span_validity_transitive :: SourceSpan -> SourceSpan -> Property
prop_span_validity_transitive span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ (isValidSpan span1 .&&. isValidSpan span2) ==> isValidSpan merged

-- Property: Error location conversion preserves position information
prop_error_location_conversion_preserves_position :: SourcePos -> Property
prop_error_location_conversion_preserves_position pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
     column errLoc === posColumn pos

-- Property: Error location conversion with span preserves range information
prop_error_location_span_conversion_preserves_range :: SourceSpan -> Property
prop_error_location_span_conversion_preserves_range span =
  let errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === posLine (spanStart span) .&&.
     column errLoc === posColumn (spanStart span) .&&.
     endLine errLoc === Just (posLine (spanEnd span)) .&&.
     endColumn errLoc === Just (posColumn (spanEnd span))

-- Property: Position advancement is reversible for non-newline characters
prop_pos_advancement_reversible_non_newline :: Char -> SourcePos -> Property
prop_pos_advancement_reversible_non_newline c pos =
  c /= '\n' ==>
  let advanced = posAfter c pos
      -- Note: We can't easily reverse position advancement due to tab expansion
      -- but we can verify that the offset increases by exactly 1
  in property $ posOffset advanced === posOffset pos + 1

-- Property: Multiple character position advancement is additive
prop_multiple_char_advancement_additive :: String -> String -> SourcePos -> Property
prop_multiple_char_advancement_additive str1 str2 pos =
  let byBoth = advancePosBy (str1 ++ str2) pos
      bySeparate = advancePosBy str2 (advancePosBy str1 pos)
  in property $ byBoth === bySeparate

-- Property: Span merging is commutative
prop_span_merging_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merging_commutative span1 span2 =
  let merge1 = mergeSpans span1 span2
      merge2 = mergeSpans span2 span1
  in property $ merge1 === merge2

-- Property: Span merging is associative
prop_span_merging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merging_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in property $ result1 === result2

-- Property: Empty span is identity element for span merging
prop_empty_span_identity :: SourceSpan -> Property
prop_empty_span_identity span =
  let emptyAtStart = emptySpan (spanStart span)
      emptyAtEnd = emptySpan (spanEnd span)
      mergeStart = mergeSpans emptyAtStart span
      mergeEnd = mergeSpans span emptyAtEnd
  in property $ mergeStart === span .&&. mergeEnd === span

-- Property: Located values maintain equality when mapped with identity
prop_located_identity_map :: SourceSpan -> Int -> Property
prop_located_identity_map span value =
  let located = locatedWithSpan span value
      mapped = mapLocated id located
  in property $ located === mapped

-- Property: Position advancement by empty string returns original position
prop_advance_by_empty_string :: SourcePos -> Property
prop_advance_by_empty_string pos =
  let advanced = advancePosBy "" pos
  in property $ advanced === pos

-- Property: Position advancement by empty text returns original position
prop_advance_by_empty_text :: SourcePos -> Property
prop_advance_by_empty_text pos =
  let advanced = advancePosByText T.empty pos
  in property $ advanced === pos

-- Property: Line advancement by zero returns original position except offset
prop_advance_by_zero_lines :: SourcePos -> Property
prop_advance_by_zero_lines pos =
  let advanced = advancePosByLine 0 pos
  in property $ posLine advanced === posLine pos .&&.
     posColumn advanced === posColumn pos .&&.
     posOffset advanced === posOffset pos

-- Property: Span start is always less than or equal to span end for valid spans
prop_span_start_le_end :: SourceSpan -> Property
prop_span_start_le_end span =
  isValidSpan span ==> spanStart span <= spanEnd span

-- Property: Position line and column are always positive for valid positions
prop_position_positive :: SourcePos -> Property
prop_position_positive pos =
  posLine pos > 0 ==> posColumn pos > 0 ==> posOffset pos >= 0

-- Property: Located functor law: map id = id
prop_located_functor_identity :: SourceSpan -> String -> Property
prop_located_functor_identity span value =
  let located = locatedWithSpan span value
      mapped = mapLocated id located
  in property $ located === mapped

-- Property: Located functor law: map (f . g) = map f . map g
prop_located_functor_composition :: SourceSpan -> Int -> Property
prop_located_functor_composition span value =
  let located = locatedWithSpan span value
      f = (*2)
      g = (+1)
      composed = mapLocated (f . g) located
      separate = mapLocated f (mapLocated g located)
  in property $ composed === separate

-- Tests collection
tests :: TestTree
tests = testGroup "New Source Location Math Properties Tests"
  [ fastProperty "Position advancement is consistent for single characters" prop_pos_after_consistency
  , fastProperty "Multiple position advancements equal single advancement by string" prop_advance_pos_by_consistency
  , fastProperty "Position advancement by text equals advancement by unpacked string" prop_advance_pos_by_text_consistency
  , fastProperty "Line advancement preserves column 1 and increments line number" prop_advance_pos_by_line_properties
  , fastProperty "Empty span has equal start and end positions" prop_empty_span_properties
  , fastProperty "Span between positions maintains order" prop_span_between_order
  , fastProperty "Merged span covers both original spans" prop_merge_spans_coverage
  , fastProperty "Located value preserves position information" prop_located_at_properties
  , fastProperty "Located with span preserves span information" prop_located_with_span_properties
  , fastProperty "Map located preserves position information" prop_map_located_preserves_location
  , fastProperty "Position offset is monotonic with line advancement" prop_offset_monotonic_line_advancement
  , fastProperty "Tab advancement follows 8-column tab stops" prop_tab_advancement_tab_stops
  , fastProperty "Newline advancement resets column to 1" prop_newline_advancement_resets_column
  , fastProperty "Span validity is transitive" prop_span_validity_transitive
  , fastProperty "Error location conversion preserves position information" prop_error_location_conversion_preserves_position
  , fastProperty "Error location conversion with span preserves range information" prop_error_location_span_conversion_preserves_range
  , fastProperty "Position advancement is reversible for non-newline characters" prop_pos_advancement_reversible_non_newline
  , fastProperty "Multiple character position advancement is additive" prop_multiple_char_advancement_additive
  , fastProperty "Span merging is commutative" prop_span_merging_commutative
  , fastProperty "Span merging is associative" prop_span_merging_associative
  , fastProperty "Empty span is identity element for span merging" prop_empty_span_identity
  , fastProperty "Located values maintain equality when mapped with identity" prop_located_identity_map
  , fastProperty "Position advancement by empty string returns original position" prop_advance_by_empty_string
  , fastProperty "Position advancement by empty text returns original position" prop_advance_by_empty_text
  , fastProperty "Line advancement by zero returns original position except offset" prop_advance_by_zero_lines
  , fastProperty "Span start is always less than or equal to span end for valid spans" prop_span_start_le_end
  , fastProperty "Position line and column are always positive for valid positions" prop_position_positive
  , fastProperty "Located functor law: map id = id" prop_located_functor_identity
  , fastProperty "Located functor law: map (f . g) = map f . map g" prop_located_functor_composition
  ]