{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseSourceLocationQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), choose)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , spanBetweenOrdered
  , mergeSpans
  , isValidSpan
  , isValidBlockSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , comparePos
  , sourceLine
  , sourceColumn
  )
import Data.Text (Text)
import qualified Data.Text as T

-- Arbitrary instances for QuickCheck
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
instance Arbitrary SourcePos where
  arbitrary = do
    lineNum' <- choose (1, 1000)
    columnNum' <- choose (1, 1000)
    offset' <- choose (0, 1000000)
    return $ SourcePos lineNum' columnNum' offset'

instance Arbitrary SourceSpan where
  arbitrary = do
    startPos' <- arbitrary
    endPos' <- arbitrary
    return $ SourceSpan startPos' endPos'

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value' <- arbitrary
    position' <- arbitrary
    span' <- arbitrary
    return $ Located value' position' span'

tests :: TestTree
tests = testGroup "Concise SourceLocation QuickCheck Tests"
  [ testProperties "SourcePos Properties"
    [ ("pos_properties", property pos_properties)
    , ("posAfter_properties", property posAfter_properties)
    , ("posAt_properties", property posAt_properties)
    , ("posAtLineCol_properties", property posAtLineCol_properties)
    , ("advancePos_properties", property advancePos_properties)
    , ("advancePosBy_properties", property advancePosBy_properties)
    , ("advancePosByText_properties", property advancePosByText_properties)
    , ("comparePos_properties", property comparePos_properties)
    , ("sourceLine_properties", property sourceLine_properties)
    , ("sourceColumn_properties", property sourceColumn_properties)
    ]
  , testProperties "SourceSpan Properties"
    [ ("span_properties", property span_properties)
    , ("emptySpan_properties", property emptySpan_properties)
    , ("spanFrom_properties", property spanFrom_properties)
    , ("spanTo_properties", property spanTo_properties)
    , ("spanBetween_properties", property spanBetween_properties)
    , ("spanBetweenOrdered_properties", property spanBetweenOrdered_properties)
    , ("mergeSpans_properties", property mergeSpans_properties)
    , ("isValidSpan_properties", property isValidSpan_properties)
    , ("isValidBlockSpan_properties", property isValidBlockSpan_properties)
    ]
  , testProperties "Located Properties"
    [ ("locatedAt_properties", property locatedAt_properties)
    , ("locatedWithSpan_properties", property locatedWithSpan_properties)
    , ("locatedValue_properties", property locatedValue_properties)
    , ("locatedSpan_properties", property locatedSpan_properties)
    , ("locatedPos_properties", property locatedPos_properties)
    , ("mapLocated_properties", property mapLocated_properties)
    ]
  ]

-- | Test basic SourcePos properties
pos_properties :: SourcePos -> Bool
pos_properties pos = 
  posLine pos > 0 && 
  posColumn pos > 0 && 
  posOffset pos >= 0

-- | Test posAfter properties
posAfter_properties :: Char -> SourcePos -> Bool
posAfter_properties c pos = 
  let newPos = posAfter c pos
  in if c == '\n'
     then posLine newPos == posLine pos + 1 && 
          posColumn newPos == 1 &&
          posOffset newPos == posOffset pos + 1
     else if c == '\t'
          then posColumn newPos == ((posColumn pos - 1) `div` 8 + 1) * 8 + 1 &&
               posOffset newPos == posOffset pos + 1
          else posColumn newPos == posColumn pos + 1 &&
               posOffset newPos == posOffset pos + 1

-- | Test posAt properties
posAt_properties :: Int -> Int -> Bool
posAt_properties line col = 
  let pos = posAt line col
  in posLine pos == line && 
      posColumn pos == col &&
      posOffset pos == 0

-- | Test posAtLineCol properties
posAtLineCol_properties :: Int -> Int -> Int -> Bool
posAtLineCol_properties line col offset = 
  let pos = posAtLineCol line col offset
  in posLine pos == line && 
      posColumn pos == col &&
      posOffset pos == offset

-- | Test advancePos properties
advancePos_properties :: Char -> SourcePos -> Bool
advancePos_properties c pos = posAfter c pos == advancePos c pos

-- | Test advancePosBy properties
advancePosBy_properties :: String -> SourcePos -> Bool
advancePosBy_properties s pos = 
  let result1 = advancePosBy s pos
      result2 = foldl (flip advancePos) pos s
  in result1 == result2

-- | Test advancePosByText properties
advancePosByText_properties :: Text -> SourcePos -> Bool
advancePosByText_properties text pos = 
  advancePosByText text pos == advancePosBy (T.unpack text) pos

-- | Test comparePos properties
comparePos_properties :: SourcePos -> SourcePos -> Bool
comparePos_properties pos1 pos2 = 
  let cmp = comparePos pos1 pos2
  in (cmp == EQ && pos1 == pos2) ||
     (cmp == LT && pos1 < pos2) ||
     (cmp == GT && pos1 > pos2)

-- | Test sourceLine properties
sourceLine_properties :: SourcePos -> Bool
sourceLine_properties pos = sourceLine pos == posLine pos

-- | Test sourceColumn properties
sourceColumn_properties :: SourcePos -> Bool
sourceColumn_properties pos = sourceColumn pos == posColumn pos

-- | Test basic SourceSpan properties
span_properties :: SourceSpan -> Bool
span_properties testSpan = 
  let start = spanStart testSpan
      end = spanEnd testSpan
  in posLine start >= 1 && 
      posColumn start >= 1 && 
      posOffset start >= 0 &&
      posLine end >= 1 && 
      posColumn end >= 1 && 
      posOffset end >= 0

-- | Test emptySpan properties
emptySpan_properties :: SourcePos -> Bool
emptySpan_properties pos = 
  let testSpan = emptySpan pos
  in spanStart testSpan == pos && spanEnd testSpan == pos

-- | Test spanFrom properties
spanFrom_properties :: SourcePos -> Bool
spanFrom_properties pos = 
  let testSpan = spanFrom pos
  in spanStart testSpan == pos && spanEnd testSpan == pos

-- | Test spanTo properties
spanTo_properties :: SourcePos -> Bool
spanTo_properties pos = 
  let testSpan = spanTo pos
  in spanStart testSpan == pos && spanEnd testSpan == pos

-- | Test spanBetween properties
spanBetween_properties :: SourcePos -> SourcePos -> Bool
spanBetween_properties pos1 pos2 = 
  let testSpan = spanBetween pos1 pos2
  in spanStart testSpan == pos1 && spanEnd testSpan == pos2

-- | Test spanBetweenOrdered properties
spanBetweenOrdered_properties :: SourcePos -> SourcePos -> Bool
spanBetweenOrdered_properties pos1 pos2 = 
  let testSpan = spanBetweenOrdered pos1 pos2
      start = spanStart testSpan
      end = spanEnd testSpan
  in (comparePos start end /= GT) && 
     (start == pos1 || start == pos2) &&
     (end == pos1 || end == pos2)

-- | Test mergeSpans properties
mergeSpans_properties :: SourceSpan -> SourceSpan -> Bool
mergeSpans_properties span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
      end1 = spanEnd span1
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in posLine mergedStart == min (posLine start1) (posLine start2) &&
     posColumn mergedStart == min (posColumn start1) (posColumn start2) &&
     posOffset mergedStart == min (posOffset start1) (posOffset start2) &&
     posLine mergedEnd == max (posLine end1) (posLine end2) &&
     posColumn mergedEnd == max (posColumn end1) (posColumn end2) &&
     posOffset mergedEnd == max (posOffset end1) (posOffset end2)

-- | Test isValidSpan properties
isValidSpan_properties :: SourceSpan -> Bool
isValidSpan_properties testSpan = 
  let start = spanStart testSpan
      end = spanEnd testSpan
  in isValidSpan testSpan == (comparePos start end /= GT)

-- | Test isValidBlockSpan properties
isValidBlockSpan_properties :: SourceSpan -> Bool
isValidBlockSpan_properties testSpan = isValidBlockSpan testSpan == isValidSpan testSpan

-- | Test locatedAt properties
locatedAt_properties :: SourcePos -> Int -> Bool
locatedAt_properties pos value = 
  let located = locatedAt pos value
  in locatedValue located == value &&
     locatedPos located == pos &&
     spanStart (locatedSpan located) == pos &&
     spanEnd (locatedSpan located) == pos

-- | Test locatedWithSpan properties
locatedWithSpan_properties :: SourceSpan -> Int -> Bool
locatedWithSpan_properties testSpan value = 
  let located = locatedWithSpan testSpan value
  in locatedValue located == value &&
     locatedSpan located == testSpan &&
     locatedPos located == spanStart testSpan

-- | Test locatedValue properties
locatedValue_properties :: Located Int -> Bool
locatedValue_properties located = locatedValue located == locValue located

-- | Test locatedSpan properties
locatedSpan_properties :: Located Int -> Bool
locatedSpan_properties located = locatedSpan located == locSpan located

-- | Test locatedPos properties
locatedPos_properties :: Located Int -> Bool
locatedPos_properties located = locatedPos located == spanStart (locSpan located)

-- | Test mapLocated properties
mapLocated_properties :: Located Int -> Bool
mapLocated_properties located = 
  let f = (*2)
      mapped = mapLocated f located
  in locatedValue mapped == f (locatedValue located) &&
     locatedSpan mapped == locatedSpan located &&
     locatedPos mapped == locatedPos located