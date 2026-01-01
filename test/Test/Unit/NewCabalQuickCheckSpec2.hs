module Test.Unit.NewCabalQuickCheckSpec2 (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, property, Arbitrary(..), Gen, choose, listOf)
import Data.Text (Text)
import qualified Data.Text as T

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )

-- | QuickCheck tests for SourceLocation module focusing on position calculation properties
tests :: TestTree
tests =
  testGroup "NewCabalQuickCheckSpec2 - SourceLocation Position Calculation Properties"
    [ testProperty "posAfter newline increments line L.and resets column" prop_posAfterNewline
    , testProperty "posAfter tab jumps to next tab stop" prop_posAfterTab
    , testProperty "posAfter regular char increments column" prop_posAfterRegularChar
    , testProperty "advancePosBy is consistent with repeated posAfter" prop_advancePosByConsistency
    , testProperty "advancePosByText handles multiline correctly" prop_advancePosByTextMultiline
    , testProperty "advancePosByLine only changes line number" prop_advancePosByLine
    , testProperty "spanBetween creates valid span" prop_spanBetweenValid
    , testProperty "mergeSpans contains both original spans" prop_mergeSpansContains
    , testProperty "isValidSpan correctly validates spans" prop_isValidSpanCorrect
    , testProperty "locatedAt creates zero-L.length span" prop_locatedAtZeroLength
    ]

-- Property: posAfter newline increments line L.and resets column to 1
prop_posAfterNewline :: Int -> Int -> Int -> Bool
prop_posAfterNewline line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in posLine newPos == line + 1 && 
     posColumn newPos == 1 && 
     posOffset newPos == offset + 1

-- Property: posAfter tab jumps to next tab stop (multiples of 8)
prop_posAfterTab :: Int -> Int -> Int -> Bool
prop_posAfterTab line col offset =
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == line && 
     posColumn newPos == expectedCol && 
     posOffset newPos == offset + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfterRegularChar :: Int -> Int -> Int -> Char -> Bool
prop_posAfterRegularChar line col offset ch
  | ch == '\n' || ch == '\t' = True  -- Skip special chars
  | otherwise = 
      let pos = SourcePos line col offset
          newPos = posAfter ch pos
      in posLine newPos == line && 
         posColumn newPos == col + 1 && 
         posOffset newPos == offset + 1

-- Property: advancePosBy is consistent with applying posAfter repeatedly
prop_advancePosByConsistency :: String -> Bool
prop_advancePosByConsistency input =
  let start = startPos
      advanced = advancePosBy input start
      manualAdvance = L.foldl (flip posAfter) start input
  in advanced == manualAdvance

-- Property: advancePosByText handles multiline text correctly
prop_advancePosByTextMultiline :: [String] -> Bool
prop_advancePosByTextMultiline lineList =
  let text = T.pack $ unlines lineList
      start = startPos
      result = advancePosByText text start
      -- Should end at line equal to number of lines
      expectedLine = L.length lineList + (if null lineList then 0 else 0)
  in posLine result >= expectedLine

-- Property: advancePosByLine only changes line number L.and resets column
prop_advancePosByLine :: Int -> Int -> Int -> Int -> Bool
prop_advancePosByLine line col offset numLines =
  let pos = SourcePos line col offset
      newPos = advancePosByLine numLines pos
  in posLine newPos == line + numLines && 
     posColumn newPos == 1 && 
     posOffset newPos == offset

-- Property: spanBetween always creates a valid span when start <= end
prop_spanBetweenValid :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_spanBetweenValid line1 col1 offset1 line2 col2 offset2 =
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = spanBetween start end
  in isValidSpan span || start > end

-- Property: mergeSpans result contains both original spans
prop_mergeSpansContains :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_mergeSpansContains line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 =
  let span1 = SourceSpan (SourcePos line1 col1 offset1) (SourcePos line2 col2 offset2)
      span2 = SourceSpan (SourcePos line3 col3 offset3) (SourcePos line4 col4 offset4)
      merged = mergeSpans span1 span2
      span4 = SourcePos line4 col4 offset4
  in spanStart merged <= spanStart span1 && 
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && 
     spanEnd merged >= spanEnd span2

-- Property: isValidSpan correctly identifies valid L.and invalid spans
prop_isValidSpanCorrect :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_isValidSpanCorrect line1 col1 offset1 line2 col2 offset2 =
  let start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      shouldBeValid = start <= end
  in isValidSpan span == shouldBeValid

-- Property: locatedAt creates zero-L.length span at given position
prop_locatedAtZeroLength :: Int -> Int -> Int -> String -> Bool
prop_locatedAtZeroLength line col offset value =
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in locatedSpan located == SourceSpan pos pos && 
     locatedPos located == pos && 
     locatedValue located == value

-- Additional property: mapLocated preserves span but transforms value
prop_mapLocatedPreservesSpan :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Bool
prop_mapLocatedPreservesSpan line1 col1 offset1 line2 col2 offset2 value =
  let span = SourceSpan (SourcePos line1 col1 offset1) (SourcePos line2 col2 offset2)
      located = Located value (spanStart span) span
      mapped = mapLocated L.length located
  in locatedSpan mapped == span && 
     locatedValue mapped == L.length value

-- Additional property: toErrorLocation preserves position information
prop_toErrorLocationPreservesPosition :: Int -> Int -> Int -> Bool
prop_toErrorLocationPreservesPosition line col offset =
  let pos = SourcePos line col offset
      errLoc = toErrorLocation pos
  in line errLoc == line && column errLoc == col

-- Additional property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpanPreservesSpan :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_toErrorLocationWithSpanPreservesSpan line1 col1 offset1 line2 col2 offset2 =
  let span = SourceSpan (SourcePos line1 col1 offset1) (SourcePos line2 col2 offset2)
      errLoc = toErrorLocationWithSpan span
  in line errLoc == line1 && 
     column errLoc == col1 && 
     endLine errLoc == Just line2 && 
     endColumn errLoc == Just col2