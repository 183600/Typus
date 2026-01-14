{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.List (minimum, maximum)
import qualified Data.Text as T

-- | 测试SourceLocation模块中的高级位置计算功能
tests :: TestTree
tests = testGroup "SourceLocationAdvancedQuickCheckSpec Tests"
  [ testGroup "SourcePos属性测试"
    [ testProperty "posAfter newline increments line and resets column" $
        \line col offset ->
          let pos = SourcePos { posLine = line, posColumn = col, posOffset = offset }
              newPos = posAfter '\n' pos
          in property (posLine newPos == line + 1 && posColumn newPos == 1 && posOffset newPos == offset + 1)
    
    , testProperty "posAfter tab advances to next tab stop" $
        \col offset ->
          let pos = SourcePos { posLine = 1, posColumn = col, posOffset = offset }
              newPos = posAfter '\t' pos
              expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
          in property (posColumn newPos == expectedCol && posOffset newPos == offset + 1)
    
    , testProperty "posAfter normal char increments column and offset" $
        \col offset c ->
          let pos = SourcePos { posLine = 1, posColumn = col, posOffset = offset }
              newPos = posAfter c pos
          in not (c `elem` ['\n', '\t']) ==> 
             property (posColumn newPos == col + 1 && posOffset newPos == offset + 1)
    
    , testProperty "posAt creates position at specified coordinates" $
        \line col ->
          let pos = posAt line col
          in property (posLine pos == line && posColumn pos == col && posOffset pos == 0)
    
    , testProperty "posAtLineCol creates position with specified offset" $
        \line col offset ->
          let pos = posAtLineCol line col offset
          in property (posLine pos == line && posColumn pos == col && posOffset pos == offset)
    ]
  
  , testGroup "SourceSpan属性测试"
    [ testProperty "emptySpan creates span with same start and end" $
        \pos ->
          let span = emptySpan pos
          in property (spanStart span == pos && spanEnd span == pos)
    
    , testProperty "spanFrom creates span with same start and end" $
        \pos ->
          let span = spanFrom pos
          in property (spanStart span == pos && spanEnd span == pos)
    
    , testProperty "spanTo creates span with same start and end" $
        \pos ->
          let span = spanTo pos
          in property (spanStart span == pos && spanEnd span == pos)
    
    , testProperty "spanBetween creates span with specified start and end" $
        \startPos endPos ->
          let span = spanBetween startPos endPos
          in property (spanStart span == startPos && spanEnd span == endPos)
    
    , testProperty "spanBetweenOrdered orders positions correctly" $
        \pos1 pos2 ->
          let span = spanBetweenOrdered pos1 pos2
              orderedStart = min pos1 pos2
              orderedEnd = max pos1 pos2
          in property (spanStart span == orderedStart && spanEnd span == orderedEnd)
    ]
  
  , testGroup "mergeSpans属性测试"
    [ testProperty "mergeSpans contains both original spans" $
        \start1 end1 start2 end2 ->
          let span1 = spanBetween start1 end1
              span2 = spanBetween start2 end2
              merged = mergeSpans span1 span2
          in property (spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
                      spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2)
    
    , testProperty "mergeSpans is commutative" $
        \start1 end1 start2 end2 ->
          let span1 = spanBetween start1 end1
              span2 = spanBetween start2 end2
              merged1 = mergeSpans span1 span2
              merged2 = mergeSpans span2 span1
          in property (merged1 == merged2)
    
    , testProperty "mergeSpans is associative" $
        \start1 end1 start2 end2 start3 end3 ->
          let span1 = spanBetween start1 end1
              span2 = spanBetween start2 end2
              span3 = spanBetween start3 end3
              merged1 = mergeSpans (mergeSpans span1 span2) span3
              merged2 = mergeSpans span1 (mergeSpans span2 span3)
          in property (merged1 == merged2)
    ]
  
  , testGroup "isValidSpan属性测试"
    [ testProperty "emptySpan is always valid" $
        \pos -> property (isValidSpan (emptySpan pos))
    
    , testProperty "span between valid positions is valid" $
        \line1 col1 line2 col2 ->
          let pos1 = SourcePos { posLine = line1, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line2, posColumn = col2, posOffset = 0 }
              span = spanBetweenOrdered pos1 pos2
          in property (isValidSpan span)
    
    , testProperty "span with start > end is invalid" $
        \line1 col1 line2 col2 ->
          let pos1 = SourcePos { posLine = line1, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line2, posColumn = col2, posOffset = 0 }
              span = spanBetween pos1 pos2
          in (pos1 > pos2) ==> not (isValidSpan span)
    ]
  
  , testGroup "isValidBlockSpan属性测试"
    [ testProperty "emptySpan is not a valid block span" $
        \pos -> not (isValidBlockSpan (emptySpan pos))
    
    , testProperty "span across multiple lines is a valid block span" $
        \line1 col1 line2 ->
          let pos1 = SourcePos { posLine = line1, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line1 + line2 + 1, posColumn = 1, posOffset = 0 }
              span = spanBetween pos1 pos2
          in property (isValidBlockSpan span)
    
    , testProperty "span on single line is not a valid block span" $
        \line col1 col2 ->
          let pos1 = SourcePos { posLine = line, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line, posColumn = col2, posOffset = 0 }
              span = spanBetweenOrdered pos1 pos2
          in property (not (isValidBlockSpan span))
    ]
  
  , testGroup "Located值属性测试"
    [ testProperty "locatedAt preserves position and value" $
        \pos (value :: String) ->
          let located = locatedAt pos value
          in property (locatedPos located == pos && locatedValue located == value)
    
    , testProperty "locatedWithSpan preserves span and value" $
        \span (value :: String) ->
          let located = locatedWithSpan span value
          in property (locatedSpan located == span && locatedValue located == value)
    
    , testProperty "mapLocated preserves position" $
        \pos value ->
          let located = locatedAt pos value
              mapped = mapLocated (++ " mapped") located
          in property (locatedPos mapped == pos)
    
    , testProperty "mapLocated applies function correctly" $
        \pos (value :: String) ->
          let located = locatedAt pos value
              mapped = mapLocated (++ " mapped") located
          in property (locatedValue mapped == value ++ " mapped")
    ]
  
  , testGroup "位置比较属性测试"
    [ testProperty "comparePos respects line ordering" $
        \line1 line2 col1 col2 ->
          let pos1 = SourcePos { posLine = line1, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line2, posColumn = col2, posOffset = 0 }
          in (line1 < line2) ==> property (comparePos pos1 pos2 == LT)
    
    , testProperty "comparePos respects column ordering for same line" $
        \line col1 col2 ->
          let pos1 = SourcePos { posLine = line, posColumn = col1, posOffset = 0 }
              pos2 = SourcePos { posLine = line, posColumn = col2, posOffset = 0 }
          in (col1 < col2) ==> property (comparePos pos1 pos2 == LT)
    
    , testProperty "comparePos respects offset ordering for same position" $
        \line col offset1 offset2 ->
          let pos1 = SourcePos { posLine = line, posColumn = col, posOffset = offset1 }
              pos2 = SourcePos { posLine = line, posColumn = col, posOffset = offset2 }
          in (offset1 < offset2) ==> property (comparePos pos1 pos2 == LT)
    ]
  
  , testGroup "位置推进属性测试"
    [ testProperty "advancePos advances correctly for normal characters" $
        \pos c ->
          not (c `elem` ['\n', '\t']) ==> 
            let newPos = advancePos c pos
            in property (posColumn newPos == posColumn pos + 1 && 
                        posOffset newPos == posOffset pos + 1 &&
                        posLine newPos == posLine pos)
    
    , testProperty "advancePosBy advances correctly for multiple characters" $
        \pos chars ->
          let newPos = advancePosBy chars pos
          in property (posOffset newPos == posOffset pos + length chars)
    
    , testProperty "advancePosByText advances correctly for text" $
        \pos text ->
          let newPos = advancePosByText text pos
          in property (posOffset newPos >= posOffset pos)
    
    , testProperty "advancePosByLine advances to next line" $
        \pos ->
          let newPos = advancePosByLine 1 pos
          in property (posLine newPos == posLine pos + 1 && posColumn newPos == 1)
    ]
  
  , testGroup "边界条件测试"
    [ testCase "handle very large positions" $ do
        let largePos = SourcePos { posLine = 1000000, posColumn = 1000000, posOffset = 1000000 }
            span = emptySpan largePos
        assertBool "Should handle large positions" (isValidSpan span)
    
    , testCase "handle negative positions gracefully" $ do
        let negPos = SourcePos { posLine = -1, posColumn = -1, posOffset = -1 }
            span = spanBetween negPos startPos
        assertBool "Should handle negative positions" (not (isValidSpan span))
    
    , testCase "handle zero-width spans" $ do
        let span = emptySpan startPos
        assertBool "Zero-width span should be valid" (isValidSpan span)
        assertBool "Zero-width span should not be a valid block span" (not (isValidBlockSpan span))
    ]
  ]

-- | Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- arbitrary `suchThat` (> 0)
    col <- arbitrary `suchThat` (> 0)
    offset <- arbitrary `suchThat` (>= 0)
    return $ SourcePos { posLine = line, posColumn = col, posOffset = offset }

-- | Arbitrary instance for Text
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- | Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end