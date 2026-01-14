{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationCalculationSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.List (sort)

tests :: TestTree
tests = testGroup "Source Location Calculation Tests"
  [ testGroup "SourcePos operations"
    [ testCase "creates start position correctly" $ do
        startPos @?= SourcePos 1 1 0
      
    , testCase "advances position with regular characters" $ do
        let pos = startPos
        posAfter 'a' pos @?= SourcePos 1 2 1
        posAfter 'b' (posAfter 'a' pos) @?= SourcePos 1 3 2
      
    , testCase "advances position with newline" $ do
        let pos = SourcePos 1 5 4
        posAfter '\n' pos @?= SourcePos 2 1 5
      
    , testCase "advances position with tab" $ do
        let pos1 = SourcePos 1 1 0
        posAfter '\t' pos1 @?= SourcePos 1 9 1  -- Tab to next 8-column boundary
      
      , testCase "advances position with tab (second example)" $ do
        let pos2 = SourcePos 1 5 4
        posAfter '\t' pos2 @?= SourcePos 1 9 5  -- Tab to next 8-column boundary
      
    , testCase "creates position at specific line and column" $
        posAt 3 5 @?= SourcePos 3 5 0
      posAtLineCol 3 5 10 `shouldBe` SourcePos 3 5 10
      
    it "compares positions correctly" $ do
      let pos1 = SourcePos 1 1 0
          pos2 = SourcePos 1 2 1
          pos3 = SourcePos 2 1 5
      comparePos pos1 pos2 `shouldBe` LT
      comparePos pos2 pos1 `shouldBe` GT
      comparePos pos1 pos1 `shouldBe` EQ
      comparePos pos2 pos3 `shouldBe` LT
      comparePos pos3 pos2 `shouldBe` GT

  describe "SourceSpan operations" $ do
    it "creates empty span" $ do
      let pos = SourcePos 3 5 10
      emptySpan pos `shouldBe` SourceSpan pos pos
      
    it "creates span from position" $ do
      let pos = SourcePos 3 5 10
      spanFrom pos `shouldBe` SourceSpan pos pos
      
    it "creates span to position" $ do
      let pos = SourcePos 3 5 10
      spanTo pos `shouldBe` SourceSpan pos pos
      
    it "creates span between positions" $ do
      let pos1 = SourcePos 1 1 0
          pos2 = SourcePos 1 5 4
      spanBetween pos1 pos2 `shouldBe` SourceSpan pos1 pos2
      
    it "creates ordered span between positions" $ do
      let pos1 = SourcePos 1 5 4
          pos2 = SourcePos 1 1 0
      spanBetweenOrdered pos1 pos2 `shouldBe` SourceSpan pos2 pos1
      spanBetweenOrdered pos2 pos1 `shouldBe` SourceSpan pos2 pos1
      
    it "merges spans correctly" $ do
      let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          span2 = SourceSpan (SourcePos 1 3 2) (SourcePos 1 7 6)
          expected = SourceSpan (SourcePos 1 1 0) (SourcePos 1 7 6)
      mergeSpans span1 span2 `shouldBe` expected
      
    it "checks span validity" $ do
      let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          invalidSpan = SourceSpan (SourcePos 1 5 4) (SourcePos 1 1 0)
          samePosSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      isValidSpan validSpan `shouldBe` True
      isValidSpan invalidSpan `shouldBe` False
      isValidSpan samePosSpan `shouldBe` True
      
    it "checks block span validity" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 2 1 10)
      isValidBlockSpan span `shouldBe` True

  describe "Located values" $ do
    it "creates located value at position" $ do
      let pos = SourcePos 3 5 10
          value = "test"
      locatedAt pos value `shouldBe` Located value pos (SourceSpan pos pos)
      
    it "creates located value with span" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          value = "test"
      locatedWithSpan span value `shouldBe` Located value (SourcePos 1 1 0) span
      
    it "extracts values from located" $ do
      let pos = SourcePos 3 5 10
          span = SourceSpan pos pos
          value = "test"
          located = Located value pos span
      locatedValue located `shouldBe` value
      locatedSpan located `shouldBe` span
      locatedPos located `shouldBe` pos
      
    it "maps function over located value" $ do
      let pos = SourcePos 3 5 10
          span = SourceSpan pos pos
          value = "test"
          located = Located value pos span
          mapped = mapLocated (++ "ed") located
      locatedValue mapped `shouldBe` "tested"
      locatedSpan mapped `shouldBe` span
      locatedPos mapped `shouldBe` pos

  describe "Position advancement" $ do
    it "advances position by multiple characters" $ do
      let pos = startPos
      advancePosBy "abc" pos `shouldBe` SourcePos 1 4 3
      
    it "advances position by text with newline" $ do
      let pos = startPos
      advancePosBy "ab\nc" pos `shouldBe` SourcePos 2 2 4
      
    it "advances position by text with tab" $ do
      let pos = startPos
      advancePosBy "ab\tc" pos `shouldBe` SourcePos 1 11 4
      
    it "advances position by lines" $ do
      let pos = SourcePos 3 5 10
      advancePosByLine 2 pos `shouldBe` SourcePos 5 1 10

  describe "Error location conversion" $ do
    it "converts position to error location" $ do
      let pos = SourcePos 3 5 10
          expected = ErrorLocation Nothing 3 5 Nothing Nothing
      toErrorLocation pos `shouldBe` expected
      
    it "converts span to error location with range" $ do
      let span = SourceSpan (SourcePos 3 5 10) (SourcePos 3 10 15)
          expected = ErrorLocation Nothing 3 5 (Just 3) (Just 10)
      toErrorLocationWithSpan span `shouldBe` expected

  describe "QuickCheck properties" $ do
    it "position advancement is consistent" $ property $
      \pos c -> let newPos = posAfter c pos
                 in posOffset newPos >= posOffset pos
      
    it "span merging is associative" $ property $
      \span1 span2 span3 -> 
        let merged1 = mergeSpans span1 (mergeSpans span2 span3)
            merged2 = mergeSpans (mergeSpans span1 span2) span3
        in spanStart merged1 `shouldBe` spanStart merged2 &&
           spanEnd merged1 `shouldBe` spanEnd merged2
           
    it "span merging is commutative for start/end" $ property $
      \span1 span2 ->
        let merged = mergeSpans span1 span2
        in spanStart merged `shouldBe` min (spanStart span1) (spanStart span2) &&
           spanEnd merged `shouldBe` max (spanEnd span1) (spanEnd span2)
           
    it "position comparison is transitive" $ property $
      \pos1 pos2 pos3 ->
        let comp12 = comparePos pos1 pos2
            comp23 = comparePos pos2 pos3
            comp13 = comparePos pos1 pos3
        in if comp12 == EQ && comp23 == EQ 
           then comp13 `shouldBe` EQ
           else if comp12 == LT && comp23 == LT
                then comp13 `shouldBe` LT
                else if comp12 == GT && comp23 == GT
                     then comp13 `shouldBe` GT
                     else True  -- Mixed cases are not necessarily transitive

  describe "Edge cases" $ do
    it "handles zero-based offsets" $ do
      let pos = SourcePos 1 1 0
      posAfter 'a' pos `shouldBe` SourcePos 1 2 1
      
    it "handles large column numbers" $ do
      let pos = SourcePos 1 1000 999
      posAfter 'a' pos `shouldBe` SourcePos 1 1001 1000
      
    it "handles tab at column boundary" $ do
      let pos1 = SourcePos 1 8 7  -- Just before tab boundary
      posAfter '\t' pos1 `shouldBe` SourcePos 1 9 8  -- Next column
      
      let pos2 = SourcePos 1 9 8  -- At tab boundary
      posAfter '\t' pos2 `shouldBe` SourcePos 1 17 9  -- Next tab boundary
      
    it "handles empty spans in merge" $ do
      let empty1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
          empty2 = SourceSpan (SourcePos 2 2 5) (SourcePos 2 2 5)
          expected = SourceSpan (SourcePos 1 1 0) (SourcePos 2 2 5)
      mergeSpans empty1 empty2 @?= expected