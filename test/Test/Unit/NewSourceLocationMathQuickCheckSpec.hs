{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewSourceLocationMathQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, advancePos, advancePosBy, advancePosByText
  , mergeOverlappingSpans, spanLength, posDistance, lineDistance
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)

-- Test mathematical properties of source positions
prop_position_addition_associative :: String -> String -> SourcePos -> Bool
prop_position_addition_associative s1 s2 pos = 
  let pos1 = advancePosBy s1 pos
      pos2 = advancePosBy s2 pos
      pos12 = advancePosBy s2 pos1
      combined = advancePosBy (s1 ++ s2) pos
  in pos12 == combined

prop_position_distance_symmetric :: SourcePos -> SourcePos -> Bool
prop_position_distance_symmetric pos1 pos2 = 
  let dist1 = posDistance pos1 pos2
      dist2 = posDistance pos2 pos1
  in dist1 == dist2

prop_position_distance_triangle_inequality :: SourcePos -> SourcePos -> SourcePos -> Bool
prop_position_distance_triangle_inequality pos1 pos2 pos3 = 
  let dist12 = posDistance pos1 pos2
      dist23 = posDistance pos2 pos3
      dist13 = posDistance pos1 pos3
  in dist13 <= dist12 + dist23

prop_position_distance_zero_identity :: SourcePos -> Bool
prop_position_distance_zero_identity pos = 
  posDistance pos pos == 0

prop_line_distance_properties :: SourcePos -> SourcePos -> Bool
prop_line_distance_properties pos1 pos2 = 
  let lineDiff = lineDistance pos1 pos2
      colDiff = abs (posColumn pos1 - posColumn pos2)
  in lineDiff >= 0 && 
     (lineDiff == 0) ==> (colDiff == posDistance pos1 pos2)

-- Test mathematical properties of source spans
prop_span_length_additivity :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_length_additivity pos1 pos2 pos3 = 
  pos1 <= pos2 && pos2 <= pos3 ==> 
  let span12 = spanBetween pos1 pos2
      span23 = spanBetween pos2 pos3
      span13 = spanBetween pos1 pos3
  in spanLength span12 + spanLength span23 == spanLength span13

prop_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_span_merge_associative span1 span2 span3 = 
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      merge123_1 = mergeSpans merge12 span3
      merge123_2 = mergeSpans span1 merge23
  in merge123_1 == merge123_2

prop_span_merge_commutative :: SourceSpan -> SourceSpan -> Bool
prop_span_merge_commutative span1 span2 = 
  let merge12 = mergeSpans span1 span2
      merge21 = mergeSpans span2 span1
  in merge12 == merge21

prop_span_merge_idempotent :: SourceSpan -> Bool
prop_span_merge_idempotent span = 
  let merged = mergeSpans span span
  in merged == span

-- Test span overlap mathematical properties
prop_span_overlap_reflexive :: SourceSpan -> Bool
prop_span_overlap_reflexive span = 
  spansOverlap span span

prop_span_overlap_symmetric :: SourceSpan -> SourceSpan -> Bool
prop_span_overlap_symmetric span1 span2 = 
  spansOverlap span1 span2 == spansOverlap span2 span1

prop_span_merge_overlapping_properties :: [SourceSpan] -> Bool
prop_span_merge_overlapping_properties spans = 
  let merged = mergeOverlappingSpans spans
  in all isValidSpan merged &&
     all (\span -> any (\mergedSpan -> spansOverlap span mergedSpan) merged) spans &&
     all (\(i, j) -> i < j ==> not (spansOverlap (merged !! i) (merged !! j)))
         (zip [0..] [0..length merged - 1])

-- Test located value mathematical properties
prop_located_value_position_preservation :: Int -> String -> SourcePos -> Bool
prop_located_value_position_preservation n value pos = 
  let located = locatedAt pos value
      doubled = mapLocated (*2) located
  in locatedPos doubled == pos && locatedSpan doubled == locatedSpan located

prop_located_value_functor_composition :: Int -> Int -> SourcePos -> Bool
prop_located_value_functor_composition n m pos = 
  let located = locatedAt pos n
      composed = mapLocated (*m) (mapLocated (*2) located)
      separate = mapLocated (* (2 * m)) located
  in composed == separate

prop_located_value_functor_identity :: Int -> SourcePos -> Bool
prop_located_value_functor_identity n pos = 
  let located = locatedAt pos n
      mapped = mapLocated id located
  in mapped == located

-- Test position advancement mathematical properties
prop_position_advancement_monotonic :: String -> SourcePos -> Bool
prop_position_advancement_monotonic s pos = 
  let advanced = advancePosBy s pos
  in posOffset advanced >= posOffset pos

prop_position_advancement_additive :: String -> String -> SourcePos -> Bool
prop_position_advancement_additive s1 s2 pos = 
  let advanced1 = advancePosBy s1 pos
      advanced2 = advancePosBy s2 advanced1
      combined = advancePosBy (s1 ++ s2) pos
  in advanced2 == combined

prop_position_advancement_identity :: SourcePos -> Bool
prop_position_advancement_identity pos = 
  advancePosBy "" pos == pos

-- Test span ordering mathematical properties
prop_span_ordering_total :: [SourceSpan] -> Bool
prop_span_ordering_total spans = 
  let sorted = sort spans
  in all (\(i, span1) -> 
            all (\(j, span2) -> 
                  i <= j || span1 <= span2) 
                (zip [0..] sorted))
         (zip [0..] sorted)

prop_span_ordering_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_span_ordering_transitive span1 span2 span3 = 
  (span1 <= span2 && span2 <= span3) ==> span1 <= span3

prop_span_ordering_antisymmetric :: SourceSpan -> SourceSpan -> Bool
prop_span_ordering_antisymmetric span1 span2 = 
  (span1 <= span2 && span2 <= span1) ==> span1 == span2

-- Test span coverage mathematical properties
prop_span_coverage_idempotent :: SourcePos -> SourcePos -> Bool
prop_span_coverage_idempotent pos1 pos2 = 
  let span1 = spanCovering pos1 pos2
      span2 = spanCovering (spanStart span1) (spanEnd span1)
  in span1 == span2

prop_span_coverage_commutative :: SourcePos -> SourcePos -> Bool
prop_span_coverage_commutative pos1 pos2 = 
  let span12 = spanCovering pos1 pos2
      span21 = spanCovering pos2 pos1
  in span12 == span21

prop_span_coverage_contains_inputs :: SourcePos -> SourcePos -> Bool
prop_span_coverage_contains_inputs pos1 pos2 = 
  let span = spanCovering pos1 pos2
  in spanContains span pos1 && spanContains span pos2

-- Test span expansion mathematical properties
prop_span_expansion_monotonic :: Int -> Int -> SourceSpan -> Bool
prop_span_expansion_monotonic before after span = 
  let expanded = expandSpan before after span
  in spanLength expanded >= spanLength span

prop_span_expansion_idempotent :: Int -> Int -> SourceSpan -> Property
prop_span_expansion_idempotent before after span = 
  before >= 0 && after >= 0 ==> 
  let expanded1 = expandSpan before after span
      expanded2 = expandSpan before after expanded1
  in expanded1 == expanded2

prop_span_expansion_additive :: Int -> Int -> Int -> Int -> SourceSpan -> Property
prop_span_expansion_additive b1 a1 b2 a2 span = 
  all (>= 0) [b1, a1, b2, a2] ==> 
  let expanded1 = expandSpan b1 a1 span
      expanded2 = expandSpan b2 a2 expanded1
      combined = expandSpan (b1 + b2) (a1 + a2) span
  in expanded2 == combined

-- Helper functions
spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap span1 span2 =
  spanStart span1 <= spanEnd span2 && spanEnd span1 >= spanStart span2

mergeOverlappingSpans :: [SourceSpan] -> [SourceSpan]
mergeOverlappingSpans = foldr merge []
  where
    merge current [] = [current]
    merge current (acc:rest)
        | spansOverlap current acc = merge (spanCovering (spanStart current) (spanEnd acc)) rest
        | otherwise = current : acc : rest

spanLength :: SourceSpan -> Int
spanLength srcSpan = posOffset (spanEnd srcSpan) - posOffset (spanStart srcSpan)

posDistance :: SourcePos -> SourcePos -> Int
posDistance p1 p2 = abs (posOffset p2 - posOffset p1)

lineDistance :: SourcePos -> SourcePos -> Int
lineDistance p1 p2 = abs (posLine p2 - posLine p1)

spanCovering :: SourcePos -> SourcePos -> SourceSpan
spanCovering p1 p2 = SourceSpan (minPos p1 p2) (maxPos p1 p2)

minPos :: SourcePos -> SourcePos -> SourcePos
minPos p1 p2 = if p1 <= p2 then p1 else p2

maxPos :: SourcePos -> SourcePos -> SourcePos
maxPos p1 p2 = if p1 >= p2 then p1 else p2

spanContains :: SourceSpan -> SourcePos -> Bool
spanContains srcSpan pos = pos >= spanStart srcSpan && pos <= spanEnd srcSpan

expandSpan :: Int -> Int -> SourceSpan -> SourceSpan
expandSpan before after srcSpan =
    let start = spanStart srcSpan
        end = spanEnd srcSpan
        newStart = posAt (posLine start) (max 1 (posColumn start - before))
        newEnd = posAt (posLine end) (posColumn end + after)
    in SourceSpan newStart newEnd

-- Arbitrary instances
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 1000)
    let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value (spanStart span) span

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests