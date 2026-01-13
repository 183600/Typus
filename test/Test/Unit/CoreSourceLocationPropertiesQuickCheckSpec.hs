{-# LANGUAGE ScopedTypeVariables #-}

module CoreSourceLocationPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, mergeSpans)
import qualified Data.Text as T

-- | Test source location properties with QuickCheck
coreSourceLocationPropertiesSpec :: TestTree
coreSourceLocationPropertiesSpec = testGroup "Core Source Location Properties"
  [ testProperty "Source positions are ordered correctly" $
      \line1 col1 line2 col2 -> 
        let pos1 = SourcePos line1 col1
            pos2 = SourcePos line2 col2
        in (line1 < line2 || (line1 == line2 && col1 < col2)) ==> pos1 < pos2

  , testProperty "Source spans contain their positions" $
      \startLine startCol endLine endCol -> 
        let start = SourcePos startLine startCol
            end = SourcePos endLine endCol
            span = SourceSpan start end
        in (startLine < endLine || (startLine == endLine && startCol <= endCol)) ==> 
           containsPosition span start && containsPosition span end

  , testProperty "Empty span has no length" $
      \line col -> 
        let pos = SourcePos line col
            span = emptySpan pos
        in spanStart span == spanEnd span

  , testCase "Source position arithmetic works correctly" $ do
    let pos = SourcePos 5 10
        nextPos = posAfter pos 5
    assertEqual "Position after 5 chars" (SourcePos 5 15) nextPos

  , testProperty "Located values preserve their location" $
      \value line col -> 
        let pos = SourcePos line col
            located = Located pos value
        in locatedPos located == pos
  ]

-- Helper functions for testing
containsPosition :: SourceSpan -> SourcePos -> Bool
containsPosition span pos = 
  let start = spanStart span
      end = spanEnd span
  in pos >= start && pos <= end

extractLocations :: T.Text -> [SourcePos]
extractLocations _ = []

countLines :: T.Text -> Int
countLines text = length $ filter (== '\n') (T.unpack text) + 1

calculatePositions :: T.Text -> [SourcePos]
calculatePositions _ = []

isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span