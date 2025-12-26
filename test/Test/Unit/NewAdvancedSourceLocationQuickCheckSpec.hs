{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewAdvancedSourceLocationQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T

-- | Test advanced source location tracking properties
spec :: Spec
spec = describe "NewAdvancedSourceLocation QuickCheck Tests" $ do

  describe "Advanced position arithmetic properties" $ do
    it "position advancement is consistent" $ property $
      \pos chars ->
        let result1 = advancePosBy chars pos
            result2 = foldl (flip advancePos) pos chars
        in result1 === result2

    it "position arithmetic is associative" $ property $
      \pos chars1 chars2 ->
        let result1 = advancePosBy (chars1 ++ chars2) pos
            result2 = advancePosBy chars2 (advancePosBy chars1 pos)
        in result1 === result2

    it "position advancement handles complex Unicode" $ property $
      \pos ->
        let unicodeChars = "αβγδεζηθ"
            newPos = advancePosBy unicodeChars pos
        in posOffset newPos === posOffset pos + length unicodeChars

  describe "Span manipulation properties" $ do
    it "span merging is commutative" $ property $
      \span1 span2 ->
        let merged1 = mergeSpans span1 span2
            merged2 = mergeSpans span2 span1
        in merged1 === merged2

    it "span merging is associative" $ property $
      \span1 span2 span3 ->
        let merged1 = mergeSpans span1 (mergeSpans span2 span3)
            merged2 = mergeSpans (mergeSpans span1 span2) span3
        in merged1 === merged2

    it "span contains its start and end positions" $ property $
      \start end ->
        let span = spanBetween start end
        in start <= end ==> 
           _isPosInSpan start span && _isPosInSpan end span

  describe "Complex location tracking" $ do
    it "location tracking preserves order" $ property $
      \positions ->
        let tracked = map (\pos -> (pos, pos)) positions
            sorted = sort tracked
        in map fst sorted === sort positions

    it "span calculations are accurate" $ property $
      \start end ->
        let span = spanBetween start end
            length = _spanLength span
        in start <= end ==> length >= 0

    it "nested spans are handled correctly" $ property $
      \outerStart outerEnd innerStart innerEnd ->
        let outerSpan = spanBetween outerStart outerEnd
            innerSpan = spanBetween innerStart innerEnd
        in outerStart <= innerStart && innerEnd <= outerEnd ==> 
           _spanContains outerSpan (spanStart innerSpan) &&
           _spanContains outerSpan (spanEnd innerSpan)

  describe "Error location conversion" $ do
    it "error location preserves essential information" $ property $
      \span ->
        let errLoc = toErrorLocationWithSpan span
            start = spanStart span
            end = spanEnd span
        in line errLoc === posLine start &&
           column errLoc === posColumn start &&
           endLine errLoc === Just (posLine end) &&
           endColumn errLoc === Just (posColumn end)

    it "single position error locations are consistent" $ property $
      \pos ->
        let errLoc = toErrorLocation pos
            spanErrLoc = toErrorLocationWithSpan (spanBetween pos pos)
        in line errLoc === line spanErrLoc &&
           column errLoc === column spanErrLoc

  describe "Advanced span operations" = do
    it "span overlap detection is symmetric" $ property $
      \span1 span2 ->
        let overlap1 = _doSpansOverlap span1 span2
            overlap2 = _doSpansOverlap span2 span1
        in overlap1 === overlap2

    it "span expansion works correctly" $ property $
      \span before after ->
        let expanded = _expandSpan before after span
            originalStart = spanStart span
            originalEnd = spanEnd span
            newStart = spanStart expanded
            newEnd = spanEnd expanded
        in posLine newStart <= posLine originalStart &&
           posLine newEnd >= posLine originalEnd

    it "span distance calculation is accurate" $ property $
      \start end ->
        let span = spanBetween start end
            distance = _posDistance start end
        in distance >= 0

  describe "Location tracking invariants" = do
    it "position invariants are maintained" $ property $
      \pos ->
        let afterNewline = posAfter '\n' pos
            afterTab = posAfter '\t' pos
            afterRegular = posAfter 'x' pos
        in posColumn afterNewline === 1 &&
           posLine afterNewline === posLine pos + 1 &&
           posColumn afterTab `mod` 8 === 1 &&
           posColumn afterRegular === posColumn pos + 1

    it "span invariants hold" $ property $
      \start end ->
        let span = spanBetween start end
        in start <= end ==> 
           spanStart span <= spanEnd span &&
           _spanLength span >= 0

    it "located value invariants" $ property $
      \pos value ->
        let located = locatedAt pos value
            locatedSpan = locatedSpan located
        in spanStart locatedSpan === pos &&
           spanEnd locatedSpan === pos &&
           locValue located === value

  where
    -- Helper functions for advanced testing
    _isPosInSpan :: SourcePos -> SourceSpan -> Bool
    _isPosInSpan pos srcSpan = pos >= spanStart srcSpan && pos <= spanEnd srcSpan

    _doSpansOverlap :: SourceSpan -> SourceSpan -> Bool
    _doSpansOverlap span1 span2 =
      spanStart span1 <= spanEnd span2 && spanEnd span1 >= spanStart span2

    _spanLength :: SourceSpan -> Int
    _spanLength srcSpan = posOffset (spanEnd srcSpan) - posOffset (spanStart srcSpan)

    _spanContains :: SourceSpan -> SourcePos -> Bool
    _spanContains srcSpan pos = pos >= spanStart srcSpan && pos <= spanEnd srcSpan

    _expandSpan :: Int -> Int -> SourceSpan -> SourceSpan
    _expandSpan before after srcSpan =
      let start = spanStart srcSpan
          end = spanEnd srcSpan
          newStart = SourcePos (posLine start) (max 1 (posColumn start - before)) (posOffset start)
          newEnd = SourcePos (posLine end) (posColumn end + after) (posOffset end)
      in SourceSpan newStart newEnd

    _posDistance :: SourcePos -> SourcePos -> Int
    _posDistance p1 p2 = abs (posOffset p2 - posOffset p1)

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