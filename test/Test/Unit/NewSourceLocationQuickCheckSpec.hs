{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewSourceLocationQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T

-- | Test source location tracking properties
spec :: Spec
spec = describe "NewSourceLocation QuickCheck Tests" $ do

  describe "SourcePos properties" $ do
    it "startPos has correct initial values" $ do
      posLine startPos `shouldBe` 1
      posColumn startPos `shouldBe` 1
      posOffset startPos `shouldBe` 0

    it "posAfter updates line number for newline" $ property $
      \line col offset ->
        let pos = SourcePos line col offset
            newPos = posAfter '\n' pos
        in posLine newPos === line + 1 &&
           posColumn newPos === 1 &&
           posOffset newPos === offset + 1

    it "posAfter updates column for tab" $ property $
      \line col offset ->
        let pos = SourcePos line col offset
            expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
            newPos = posAfter '\t' pos
        in posLine newPos === line &&
           posColumn newPos === expectedCol &&
           posOffset newPos === offset + 1

    it "posAfter increments column for regular characters" $ property $
      \line col offset c ->
        let pos = SourcePos line col offset
            newPos = posAfter c pos
        in c `notElem` "\n\t" ==> 
           posLine newPos === line &&
           posColumn newPos === col + 1 &&
           posOffset newPos === offset + 1

  describe "SourceSpan properties" $ do
    it "emptySpan has same start and end" $ property $
      \pos -> 
        let span = emptySpan pos
        in spanStart span === pos && spanEnd span === pos

    it "spanBetween creates correct span" $ property $
      \start end ->
        let span = spanBetween start end
        in spanStart span === start && spanEnd span === end

    it "mergeSpans contains both original spans" $ property $
      \start1 end1 start2 end2 ->
        let span1 = spanBetween start1 end1
            span2 = spanBetween start2 end2
            merged = mergeSpans span1 span2
        in spanStart merged === min (spanStart span1) (spanStart span2) &&
           spanEnd merged === max (spanEnd span1) (spanEnd span2)

    it "isValidSpan checks start <= end" $ property $
      \start end ->
        let span = spanBetween start end
        in isValidSpan span === (start <= end)

  describe "Located values properties" $ do
    it "locatedAt creates value with empty span" $ property $
      \pos value ->
        let located = locatedAt pos value
        in locValue located === value &&
           locPos located === pos &&
           locSpan located === emptySpan pos

    it "locatedWithSpan creates value with given span" $ property $
      \span value ->
        let located = locatedWithSpan span value
        in locValue located === value &&
           locSpan located === span &&
           locPos located === spanStart span

    it "mapLocated preserves location" $ property $
      \span value f ->
        let located = locatedWithSpan span value
            mapped = mapLocated f located
        in locSpan mapped === locSpan located &&
           locPos mapped === locPos located &&
           locValue mapped === f value

  describe "Position advancement properties" $ do
    it "advancePosBy handles empty string" $ property $
      \pos -> advancePosBy "" pos === pos

    it "advancePosBy is consistent with repeated advancePos" $ property $
      \pos chars ->
        let result1 = advancePosBy chars pos
            result2 = foldl (flip advancePos) pos chars
        in result1 === result2

    it "advancePosByText handles empty text" $ property $
      \pos -> advancePosByText T.empty pos === pos

    it "advancePosByLine updates line number correctly" $ property $
      \line col offset numLines ->
        let pos = SourcePos line col offset
            newPos = advancePosByLine numLines pos
        in posLine newPos === line + numLines &&
           posColumn newPos === 1

  describe "Error location conversion properties" $ do
    it "toErrorLocation converts position correctly" $ property $
      \pos ->
        let errLoc = toErrorLocation pos
        in line errLoc === posLine pos &&
           column errLoc === posColumn pos &&
           filePath errLoc === Nothing &&
           endLine errLoc === Nothing &&
           endColumn errLoc === Nothing

    it "toErrorLocationWithSpan converts span correctly" $ property $
      \start end ->
        let span = spanBetween start end
            errLoc = toErrorLocationWithSpan span
        in line errLoc === posLine start &&
           column errLoc === posColumn start &&
           endLine errLoc === Just (posLine end) &&
           endColumn errLoc === Just (posColumn end) &&
           filePath errLoc === Nothing

  describe "Location tracking properties" $ do
    it "runLocationTracker starts at startPos" $ property $
      \action ->
        let result = runLocationTracker action
        in result === result -- Basic sanity check

    it "position tracking is consistent" $ property $
      \chars ->
        let tracked = runLocationTracker $ do
                setCurrentPos startPos
                advancePosBy chars
                getCurrentPos
            expected = advancePosBy chars startPos
        in tracked === expected

  where
    -- Helper instances for QuickCheck
    instance Arbitrary SourcePos where
      arbitrary = SourcePos <$> arbitraryPositive <*> arbitraryPositive <*> arbitraryNonNegative
        where
          arbitraryPositive = getPositive <$> arbitrary
          arbitraryNonNegative = getNonNegative <$> arbitrary

    instance Arbitrary SourceSpan where
      arbitrary = do
        start <- arbitrary
        endOffset <- arbitrary
        let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
        return $ spanBetween start end