{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)

-- | Test properties for SourceLocation module
tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testProperty "posAfter: newline advances line and resets column" propPosAfterNewline
  , testProperty "posAfter: tab advances to next tab position" propPosAfterTab
  , testProperty "posAfter: regular char advances column" propPosAfterRegularChar
  , testProperty "spanBetween: creates valid span" propSpanBetweenValid
  , testProperty "mergeSpans: creates span covering both" propMergeSpansCoversBoth
  , testProperty "isValidSpan: correctly validates spans" propIsValidSpanCorrect
  , testProperty "locatedAt: creates located value with empty span" propLocatedAtEmptySpan
  , testProperty "locatedWithSpan: creates located value with given span" propLocatedWithSpanCorrect
  , testProperty "mapLocated: preserves location" propMapLocatedPreservesLocation
  , testProperty "advancePosBy: correctly advances position" propAdvancePosByCorrect
  ]

-- | posAfter: Newline should advance line and reset column to 1
propPosAfterNewline :: Int -> Property
propPosAfterNewline line = 
  line >= 1 ==> 
    let pos = SourcePos line 5 10
        newPos = posAfter '\n' pos
    in posLine newPos == line + 1 && 
       posColumn newPos == 1 && 
       posOffset newPos == 11

-- | posAfter: Tab should advance to next tab position (8-character tabs)
propPosAfterTab :: Int -> Property
propPosAfterTab col = 
  col >= 1 && col <= 20 ==>
    let pos = SourcePos 5 col (col + 10)
        newPos = posAfter '\t' pos
        expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
    in posLine newPos == 5 && 
       posColumn newPos == expectedCol && 
       posOffset newPos == col + 11

-- | posAfter: Regular character should advance column by 1
propPosAfterRegularChar :: Int -> Int -> Property
propPosAfterRegularChar line col = 
  line >= 1 && col >= 1 ==>
    let pos = SourcePos line col (line * 100 + col)
        newPos = posAfter 'a' pos
    in posLine newPos == line && 
       posColumn newPos == col + 1 && 
       posOffset newPos == line * 100 + col + 1

-- | spanBetween: Should create valid span between positions
propSpanBetweenValid :: Int -> Int -> Int -> Int -> Property
propSpanBetweenValid line1 col1 line2 col2 = 
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 ==>
    let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
        pos2 = SourcePos line2 col2 (line2 * 100 + col2)
        span = spanBetween pos1 pos2
    in spanStart span == pos1 && spanEnd span == pos2

-- | mergeSpans: Should create span covering both spans
propMergeSpansCoversBoth :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
propMergeSpansCoversBoth line1 col1 line2 col2 line3 col3 line4 col4 = 
  all (>= 1) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
    let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
        pos2 = SourcePos line2 col2 (line2 * 100 + col2)
        pos3 = SourcePos line3 col3 (line3 * 100 + col3)
        pos4 = SourcePos line4 col4 (line4 * 100 + col4)
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos3 pos4
        merged = mergeSpans span1 span2
    in spanStart merged <= spanStart span1 && 
       spanStart merged <= spanStart span2 &&
       spanEnd merged >= spanEnd span1 && 
       spanEnd merged >= spanEnd span2

-- | isValidSpan: Should correctly validate spans
propIsValidSpanCorrect :: Int -> Int -> Int -> Int -> Property
propIsValidSpanCorrect line1 col1 line2 col2 = 
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 ==>
    let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
        pos2 = SourcePos line2 col2 (line2 * 100 + col2)
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos1
        valid1 = isValidSpan span1
        valid2 = isValidSpan span2
    in (pos1 <= pos2 && valid1 && not valid2) || 
       (pos1 > pos2 && not valid1 && valid2) ||
       (pos1 == pos2 && valid1 && valid2)

-- | locatedAt: Should create located value with empty span
propLocatedAtEmptySpan :: Int -> Int -> String -> Property
propLocatedAtEmptySpan line col value = 
  line >= 1 && col >= 1 ==>
    let pos = SourcePos line col (line * 100 + col)
        located = locatedAt pos value
    in locValue located == value && 
       locPos located == pos && 
       spanStart (locSpan located) == pos && 
       spanEnd (locSpan located) == pos

-- | locatedWithSpan: Should create located value with given span
propLocatedWithSpanCorrect :: Int -> Int -> Int -> Int -> String -> Property
propLocatedWithSpanCorrect line1 col1 line2 col2 value = 
  all (>= 1) [line1, col1, line2, col2] ==>
    let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
        pos2 = SourcePos line2 col2 (line2 * 100 + col2)
        span = spanBetween pos1 pos2
        located = locatedWithSpan span value
    in locValue located == value && 
       locPos located == pos1 && 
       locSpan located == span

-- | mapLocated: Should preserve location
propMapLocatedPreservesLocation :: Int -> Int -> Int -> Int -> String -> Property
propMapLocatedPreservesLocation line1 col1 line2 col2 value = 
  all (>= 1) [line1, col1, line2, col2] ==>
    let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
        pos2 = SourcePos line2 col2 (line2 * 100 + col2)
        span = spanBetween pos1 pos2
        located = locatedWithSpan span value
        mapped = mapLocated (++ " suffix") located
    in locPos mapped == locPos located && 
       locSpan mapped == locSpan located &&
       locValue mapped == value ++ " suffix"

-- | advancePosBy: Should correctly advance position
propAdvancePosByCorrect :: Int -> Int -> String -> Property
propAdvancePosByCorrect line col text = 
  line >= 1 && col >= 1 && not (null text) ==>
    let pos = SourcePos line col (line * 100 + col)
        advanced = advancePosBy text pos
        expectedOffset = posOffset pos + length text
        expectedLine = line + count '\n' text
        expectedCol = case (reverse text, dropWhile (/= '\n') (reverse text)) of
          (revText, afterLastNewline) -> 
            if '\n' `elem` text 
            then length (takeWhile (/= '\n') afterLastNewline) + 1
            else col + length text
    in posLine advanced == expectedLine && 
       posOffset advanced == expectedOffset

-- Helper function to count occurrences of a character in a string
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)