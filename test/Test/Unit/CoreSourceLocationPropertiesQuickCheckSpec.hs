{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreSourceLocationPropertiesQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty



import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, mergeSpans, posAfter, locatedValue)
import qualified Data.Text as T

-- | Test source location properties with QuickCheck
coreSourceLocationPropertiesSpec :: TestTree
coreSourceLocationPropertiesSpec = testGroup "Core Source Location Properties"
  [ testProperty "Source positions are ordered correctly" $
      \line1 col1 line2 col2 -> 
        let pos1 = SourcePos line1 col1 0
            pos2 = SourcePos line2 col2 0
        in (line1 < line2 || (line1 == line2 && col1 < col2)) ==> pos1 < pos2

  , testProperty "Source spans contain their positions" $
      \startLine startCol endLine endCol -> 
        let start = SourcePos startLine startCol 0
            end = SourcePos endLine endCol 0
            span = SourceSpan start end
        in (startLine < endLine || (startLine == endLine && startCol <= endCol)) ==> 
           containsPosition span start && containsPosition span end

  , testProperty "Empty span has no length" $
      \line col -> 
        let pos = SourcePos line col 0
            span = emptySpan pos
        in spanStart span == spanEnd span

  , testCase "Source position arithmetic works correctly" $ do
    let pos = SourcePos 5 10 0
        nextPos = SourcePos (posLine pos) (posColumn pos + 5) (posOffset pos + 5)
    assertEqual "Position after 5 chars" (SourcePos 5 15 0) nextPos

  , testProperty "Located values preserve their location" $
      \(value :: String) line col -> 
        let pos = SourcePos line col 0
            span = SourceSpan pos pos
            located = Located value pos span
        in locatedValue located == value && locPos located == pos
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
countLines text = length (filter (== '\n') (T.unpack text)) + 1

calculatePositions :: T.Text -> [SourcePos]
calculatePositions _ = []

isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span