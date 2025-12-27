{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationMathSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import SourceLocation

spec :: Spec
spec = describe "SourceLocation Mathematical Properties" $ do
  
  describe "SourcePos properties" $ do
    it "startPos has correct initial values" $ do
      posLine startPos `shouldBe` 1
      posColumn startPos `shouldBe` 1
      posOffset startPos `shouldBe` 0
    
    it "posAt creates position with correct values" $ do
      let pos = posAt 5 10
      posLine pos `shouldBe` 5
      posColumn pos `shouldBe` 10
      posOffset pos `shouldBe` 0
    
    it "posAtLineCol creates position with all values" $ do
      let pos = posAtLineCol 5 10 100
      posLine pos `shouldBe` 5
      posColumn pos `shouldBe` 10
      posOffset pos `shouldBe` 100

  describe "Position advancement" $ do
    it "newline advances line and resets column" $ do
      let start = posAt 1 5
          result = posAfter '\n' start
      posLine result `shouldBe` 2
      posColumn result `shouldBe` 1
      posOffset result `shouldBe` 6
    
    it "tab advances to next tab stop" $ do
      let start = posAt 1 3
          result = posAfter '\t' start
      posColumn result `shouldBe` 9  -- Next tab stop after column 3
      posLine result `shouldBe` 1
      posOffset result `shouldBe` 4
    
    it "tab at tab stop advances to next" $ do
      let start = posAt 1 9
          result = posAfter '\t' start
      posColumn result `shouldBe` 17  -- Next tab stop after column 9
      posLine result `shouldBe` 1
    
    it "regular character advances column" $ do
      let start = posAt 1 5
          result = posAfter 'a' start
      posLine result `shouldBe` 1
      posColumn result `shouldBe` 6
      posOffset result `shouldBe` 5
    
    property "advancePosBy advances correctly for multiple chars" $ do
      \chars startPos -> 
        let result = advancePosBy chars startPos
            expected = foldl (flip advancePos) startPos chars
        in result === expected

  describe "SourceSpan properties" $ do
    it "emptySpan has same start and end" $ do
      let pos = posAt 3 7
          span = emptySpan pos
      spanStart span `shouldBe` pos
      spanEnd span `shouldBe` pos
    
    it "spanBetween creates correct span" $ do
      let start = posAt 1 5
          end = posAt 2 10
          span = spanBetween start end
      spanStart span `shouldBe` start
      spanEnd span `shouldBe` end
    
    it "mergeSpans takes minimum start and maximum end" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 1 10)
          span2 = spanBetween (posAt 1 3) (posAt 1 8)
          merged = mergeSpans span1 span2
      spanStart merged `shouldBe` spanStart span2  -- Minimum start
      spanEnd merged `shouldBe` spanEnd span1      -- Maximum end
    
    it "isValidSpan checks start <= end" $ do
      let validSpan = spanBetween (posAt 1 5) (posAt 1 10)
          invalidSpan = spanBetween (posAt 1 10) (posAt 1 5)
      isValidSpan validSpan `shouldBe` True
      isValidSpan invalidSpan `shouldBe` False

  describe "Located values" $ do
    it "locatedAt creates value with empty span" $ do
      let pos = posAt 3 7
          value = "test"
          located = locatedAt pos value
      locValue located `shouldBe` value
      locPos located `shouldBe` pos
      spanStart (locSpan located) `shouldBe` pos
      spanEnd (locSpan located) `shouldBe` pos
    
    it "locatedWithSpan creates value with given span" $ do
      let span = spanBetween (posAt 1 5) (posAt 1 10)
          value = 42
          located = locatedWithSpan span value
      locValue located `shouldBe` value
      locSpan located `shouldBe` span
      locPos located `shouldBe` spanStart span
    
    it "mapLocated applies function to value" $ do
      let span = spanBetween (posAt 1 5) (posAt 1 10)
          located = locatedWithSpan span 5
          result = mapLocated (*2) located
      locValue result `shouldBe` 10
      locSpan result `shouldBe` span

  describe "Location tracking" $ do
    it "runLocationTracker starts at startPos" $ do
      let result = runLocationTracker getCurrentPos
      result `shouldBe` startPos
    
    it "setCurrentPos changes current position" $ do
      let newPos = posAt 5 10
          result = evalState (setCurrentPos newPos >> getCurrentPos) startPos
      result `shouldBe` newPos
    
    it "markSpanStart and markSpanEnd create correct span" $ do
      let start = posAt 1 5
          end = posAt 1 10
          result = evalState (do
            setCurrentPos start
            spanStart <- markSpanStart
            setCurrentPos end
            markSpanEnd spanStart) startPos
      spanStart result `shouldBe` start
      spanEnd result `shouldBe` end

  describe "Position advancement by text" $ do
    property "advancePosByText is consistent with advancePosBy" $ do
      \text startPos -> 
        let result1 = advancePosByText text startPos
            result2 = advancePosBy (T.unpack text) startPos
        in result1 === result
    
    it "advancePosByLine advances line count" $ do
      let start = posAt 3 7
          result = advancePosByLine 5 start
      posLine result `shouldBe` 8
      posColumn result `shouldBe` 1
      posOffset result `shouldBe` posOffset start  -- Offset unchanged in this implementation

  describe "Error location conversion" $ do
    it "toErrorLocation converts position correctly" $ do
      let pos = posAt 5 10
          errorLoc = toErrorLocation pos
      line errorLoc `shouldBe` 5
      column errorLoc `shouldBe` 10
      filePath errorLoc `shouldBe` Nothing
      endLine errorLoc `shouldBe` Nothing
      endColumn errorLoc `shouldBe` Nothing
    
    it "toErrorLocationWithSpan converts span correctly" $ do
      let span = spanBetween (posAt 5 10) (posAt 6 15)
          errorLoc = toErrorLocationWithSpan span
      line errorLoc `shouldBe` 5
      column errorLoc `shouldBe` 10
      endLine errorLoc `shouldBe` Just 6
      endColumn errorLoc `shouldBe` Just 15
      filePath errorLoc `shouldBe` Nothing

  describe "Span length and distance calculations" $ do
    it "span length is end offset minus start offset" $ do
      let start = posAtLineCol 1 1 100
          end = posAtLineCol 1 5 104
          span = spanBetween start end
      _spanLength span `shouldBe` 4
    
    it "position distance is absolute offset difference" $ do
      let pos1 = posAtLineCol 1 1 100
          pos2 = posAtLineCol 1 5 104
      _posDistance pos1 pos2 `shouldBe` 4
    
    it "line distance is absolute line difference" $ do
      let pos1 = posAt 5 10
          pos2 = posAt 8 15
      _lineDistance pos1 pos2 `shouldBe` 3

  describe "Span operations" $ do
    it "spanContains checks if position is within span" $ do
      let span = spanBetween (posAt 1 5) (posAt 1 10)
          inside = posAt 1 7
          outside = posAt 1 15
      _spanContains span inside `shouldBe` True
      _spanContains span outside `shouldBe` False
    
    it "spansOverlap detects overlapping spans" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 1 10)
          span2 = spanBetween (posAt 1 8) (posAt 1 15)
          span3 = spanBetween (posAt 1 12) (posAt 1 20)
      _spansOverlap span1 span2 `shouldBe` True
      _spansOverlap span1 span3 `shouldBe` False
    
    it "expandSpan expands span by given amounts" $ do
      let original = spanBetween (posAt 1 5) (posAt 1 10)
          expanded = _expandSpan 2 3 original
      spanStart expanded `shouldBe` posAt 1 3  -- 5 - 2
      spanEnd expanded `shouldBe` posAt 1 13    -- 10 + 3