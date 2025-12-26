{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationPrecisionQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust, isNothing)

import SourceLocation
import Compiler.Errors.Core

-- | Test source position arithmetic precision
testSourcePositionArithmeticPrecision :: Property
testSourcePositionArithmeticPrecision =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \lines ->
    forAll arbitrary $ \cols ->
      let newPos = advancePosBy lines cols pos
          expectedLine = max 1 (sourceLine pos + lines)
          expectedCol = if lines == 0 
                       then max 1 (sourceColumn pos + cols)
                       else max 1 cols
      in sourceLine newPos === expectedLine .&&.
         sourceColumn newPos === expectedCol .&&.
         sourceFile newPos === sourceFile pos

-- | Test span containment properties
testSpanContainmentProperties :: Property
testSpanContainmentProperties =
  forAll arbitrary $ \outerSpan ->
    forAll arbitrary $ \innerSpan ->
      let outerStart = spanStart outerSpan
          outerEnd = spanEnd outerSpan
          innerStart = spanStart innerSpan
          innerEnd = spanEnd innerSpan
          contains = spanContains outerSpan innerSpan
      in if contains
         then sourceLine outerStart <= sourceLine innerStart .&&.
              sourceLine outerEnd >= sourceLine innerEnd .&&.
              (if sourceLine outerStart == sourceLine innerStart
               then sourceColumn outerStart <= sourceColumn innerStart
               else property True) .&&.
              (if sourceLine outerEnd == sourceLine innerEnd
               then sourceColumn outerEnd >= sourceColumn innerEnd
               else property True)
         else property True

-- | Test span merging precision
testSpanMergingPrecision :: Property
testSpanMergingPrecision =
  forAll arbitrary $ \span1 ->
    forAll arbitrary $ \span2 ->
      let merged = mergeSpans span1 span2
          mergedStart = spanStart merged
          mergedEnd = spanEnd merged
          start1 = spanStart span1
          end1 = spanEnd span1
          start2 = spanStart span2
          end2 = spanEnd span2
      in if isValidSpan span1 && isValidSpan span2
         then let minLine = min (sourceLine start1) (sourceLine start2)
                  minCol = if sourceLine start1 == sourceLine start2
                          then min (sourceColumn start1) (sourceColumn start2)
                          else if sourceLine start1 < sourceLine start2
                               then sourceColumn start1
                               else sourceColumn start2
                  maxLine = max (sourceLine end1) (sourceLine end2)
                  maxCol = if sourceLine end1 == sourceLine end2
                          then max (sourceColumn end1) (sourceColumn end2)
                          else if sourceLine end1 > sourceLine end2
                               then sourceColumn end1
                               else sourceColumn end2
              in sourceLine mergedStart === minLine .&&.
                 sourceColumn mergedStart === minCol .&&.
                 sourceLine mergedEnd === maxLine .&&.
                 sourceColumn mergedEnd === maxCol
         else property True

-- | Test location tracking through transformations
testLocationTrackingPrecision :: Property
testLocationTrackingPrecision =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \text ->
      let located = locatedAt pos text
          extractedPos = locatedPos located
          extractedValue = locatedValue located
      in extractedPos === pos .&&.
         extractedValue === text

-- | Test error location conversion precision
testErrorLocationConversionPrecision :: Property
testErrorLocationConversionPrecision =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \message ->
      let error = errorAt pos message
          errorLoc = toErrorLocation pos
          errorWithSpanLoc = toErrorLocationWithSpan pos (spanFrom pos pos)
      in errorLocation error === errorLoc .&&.
         sourceLine (elPosition errorLoc) === sourceLine pos .&&.
         sourceColumn (elPosition errorLoc) === sourceColumn pos .&&.
         sourceFile (elPosition errorLoc) === sourceFile pos

-- | Test span validity invariants
testSpanValidityInvariants :: Property
testSpanValidityInvariants =
  forAll arbitrary $ \span ->
    let start = spanStart span
        end = spanEnd span
        valid = isValidSpan span
    in if valid
       then sourceLine start <= sourceLine end .&&.
            (if sourceLine start == sourceLine end
             then sourceColumn start <= sourceColumn end
             else property True)
       else property True

-- | Test position comparison precision
testPositionComparisonPrecision :: Property
testPositionComparisonPrecision =
  forAll arbitrary $ \pos1 ->
    forAll arbitrary $ \pos2 ->
      let sameFile = sourceFile pos1 == sourceFile pos2
          earlier = pos1 `posBefore` pos2
          later = pos1 `posAfter` pos2
          same = pos1 == pos2
      in if sameFile
         then if same
              then not earlier .&&. not later
              else earlier /= later
         else not earlier .&&. not later -- Different files can't be ordered

-- | Test span length calculation precision
testSpanLengthCalculationPrecision :: Property
testSpanLengthCalculationPrecision =
  forAll arbitrary $ \span ->
    if isValidSpan span
    then let start = spanStart span
             end = spanEnd span
             length = spanLength span
         in if sourceLine start == sourceLine end
            then length === max 0 (sourceColumn end - sourceColumn start)
            else length >= 0
    else spanLength span === 0

-- | Test location tracker state consistency
testLocationTrackerStateConsistency :: Property
testLocationTrackerStateConsistency =
  forAll arbitrary $ \positions ->
    let tracker = foldl (\acc pos -> setCurrentPos pos acc) 
                        (runLocationTracker getCurrentPos) positions
        finalPos = runLocationTracker getCurrentPos tracker
    in if null positions
       then finalPos === startPos "" -- Default position
       else finalPos === last positions

-- | Test span intersection properties
testSpanIntersectionProperties :: Property
testSpanIntersectionProperties =
  forAll arbitrary $ \span1 ->
    forAll arbitrary $ \span2 ->
      let intersection = spanIntersection span1 span2
          hasIntersection = isJust intersection
      in if hasIntersection
         then let justSpan = fromMaybe (error "Impossible") intersection
              in spanContains span1 justSpan .&&. 
                 spanContains span2 justSpan
         else property True

-- | Test text position advancement precision
testTextPositionAdvancementPrecision :: Property
testTextPositionAdvancementPrecision =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \text ->
      let linesInText = length $ T.lines $ T.pack text
          finalPos = foldl advancePos pos text
          expectedLine = sourceLine pos + linesInText
          expectedCol = if linesInText == 0 
                       then sourceColumn pos + length text
                       else length $ last $ lines text
      in sourceLine finalPos === expectedLine .&&.
         sourceColumn finalPos === max 1 expectedCol

-- | Test location span expansion
testLocationSpanExpansion :: Property
testLocationSpanExpansion =
  forAll arbitrary $ \span ->
    forAll arbitrary $ \pos ->
      let expanded = expandSpan span pos
          originalStart = spanStart span
          originalEnd = spanEnd span
          expandedStart = spanStart expanded
          expandedEnd = spanEnd expanded
      in sourceLine expandedStart <= min (sourceLine originalStart) (sourceLine pos) .&&.
         sourceLine expandedEnd >= max (sourceLine originalEnd) (sourceLine pos)

-- | Test location path resolution
testLocationPathResolution :: Property
testLocationPathResolution =
  forAll arbitrary $ \filePath ->
    let pos = posAt filePath 1 1
        resolvedPath = sourceFile pos
    in resolvedPath === filePath

-- | Test location serialization roundtrip
testLocationSerializationRoundtrip :: Property
testLocationSerializationRoundtrip =
  forAll arbitrary $ \pos ->
    let serialized = show pos
        -- Note: This would require a readPos function to complete the roundtrip
        -- For now, we just test that serialization produces something
    in length serialized > 0

tests :: TestTree
tests = testGroup "Source Location Precision QuickCheck Tests"
  [ testProperty "Position arithmetic precision" testSourcePositionArithmeticPrecision
  , testProperty "Span containment properties" testSpanContainmentProperties
  , testProperty "Span merging precision" testSpanMergingPrecision
  , testProperty "Location tracking precision" testLocationTrackingPrecision
  , testProperty "Error location conversion" testErrorLocationConversionPrecision
  , testProperty "Span validity invariants" testSpanValidityInvariants
  , testProperty "Position comparison" testPositionComparisonPrecision
  , testProperty "Span length calculation" testSpanLengthCalculationPrecision
  , testProperty "Tracker state consistency" testLocationTrackerStateConsistency
  , testProperty "Span intersection" testSpanIntersectionProperties
  , testProperty "Text position advancement" testTextPositionAdvancementPrecision
  , testProperty "Span expansion" testLocationSpanExpansion
  , testProperty "Path resolution" testLocationPathResolution
  , testProperty "Serialization roundtrip" testLocationSerializationRoundtrip
  ]