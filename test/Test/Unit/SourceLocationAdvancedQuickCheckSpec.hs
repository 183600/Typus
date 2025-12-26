{-# LANGUAGE CPP #-}
module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample)

import qualified Data.Text as T
import Data.Char (isSpace)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 200)
    offset <- choose (0, 50000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 1000)
    let end = start { posOffset = posOffset start + endOffset 
                    , posColumn = posColumn start + endOffset `mod` 200 + 1
                    , posLine = posLine start + (endOffset `div` 200)
                    }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Advanced QuickCheck Tests"
    [ testProperty "posAfter newline always increments line and resets column to 1" $
        \line column offset ->
          let pos = SourcePos line column offset
              newPos = posAfter '\n' pos
          in posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

    , testProperty "posAfter tab advances to next tab stop (8-character alignment)" $
        \line column offset ->
          let pos = SourcePos line column offset
              expectedCol = ((column - 1) `div` 8 + 1) * 8 + 1
              newPos = posAfter '\t' pos
          in posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1 .&&.
             posLine newPos === posLine pos

    , testProperty "posAfter ordinary character increments column and offset" $
        \pos c ->
          c `notElem` "\n\t" ==>
          let newPos = posAfter c pos
          in posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1 .&&.
             posLine newPos === posLine pos

    , testProperty "advancePosBy empty string returns same position" $
        \pos -> advancePosBy "" pos === pos

    , testProperty "advancePosByText empty text returns same position" $
        \pos -> advancePosByText T.empty pos === pos

    , testProperty "advancePosByLine preserves offset but changes line and resets column" $
        \pos numLines ->
          let newPos = advancePosByLine numLines pos
          in posLine newPos === posLine pos + numLines .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos

    , testProperty "emptySpan creates span with same start and end" $
        \pos ->
          let span = emptySpan pos
          in spanStart span === pos .&&.
             spanEnd span === pos

    , testProperty "spanFrom is equivalent to emptySpan" $
        \pos -> spanFrom pos === emptySpan pos

    , testProperty "spanTo creates zero-length span at position" $
        \pos ->
          let span = spanTo pos
          in spanStart span === pos .&&.
             spanEnd span === pos

    , testProperty "spanBetween preserves provided bounds" $
        \start end ->
          let span = spanBetween start end
          in spanStart span === start .&&.
             spanEnd span === end

    , testProperty "mergeSpans selects earliest start and latest end" $
        \span1 span2 ->
          let merged = mergeSpans span1 span2
          in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
             spanEnd merged === max (spanEnd span1) (spanEnd span2)

    , testProperty "mergeSpans is commutative" $
        \span1 span2 -> mergeSpans span1 span2 === mergeSpans span2 span1

    , testProperty "mergeSpans is idempotent" $
        \span -> mergeSpans span span === span

    , testProperty "locatedAt creates located value with empty span at position" $
        \pos value ->
          let located = locatedAt pos value
          in locatedPos located === pos .&&.
             locatedSpan located === emptySpan pos .&&.
             locatedValue located === value

    , testProperty "mapLocated preserves span but transforms value" $
        \loc ->
          let mapped = mapLocated length loc
          in locatedSpan mapped === locatedSpan loc .&&.
             locatedValue mapped === length (locatedValue loc)

    , testProperty "isValidSpan is true for spans created by spanBetween with valid positions" $
        \start end ->
          let span = spanBetween start end
              valid = start <= end
          in isValidSpan span === valid

    , testProperty "toErrorLocation converts position to error location correctly" $
        \pos ->
          let errLoc = toErrorLocation pos
          in line errLoc === posLine pos .&&.
             column errLoc === posColumn pos .&&.
             endLine errLoc === Nothing .&&.
             endColumn errLoc === Nothing

    , testProperty "toErrorLocationWithSpan preserves both start and end positions" $
        \span ->
          let errLoc = toErrorLocationWithSpan span
              start = spanStart span
              end = spanEnd span
          in line errLoc === posLine start .&&.
             column errLoc === posColumn start .&&.
             endLine errLoc === Just (posLine end) .&&.
             endColumn errLoc === Just (posColumn end)

    , testProperty "advancePosByText single character equals posAfter" $
        \pos c ->
          let text = T.singleton c
              pos1 = advancePosByText text pos
              pos2 = posAfter c pos
          in pos1 === pos2

    , testProperty "advancePosBy is consistent with advancePosByText for ASCII strings" $
        \pos str ->
          let text = T.pack str
              pos1 = advancePosBy str pos
              pos2 = advancePosByText text pos
          in pos1 === pos2

    , testProperty "SourcePos ordering is consistent with offset comparison" $
        \pos1 pos2 ->
          (pos1 <= pos2) === (posOffset pos1 <= posOffset pos2)

    , testProperty "SourceSpan ordering is lexicographic (start, then end)" $
        \span1 span2 ->
          (span1 <= span2) === 
            (spanStart span1 < spanStart span2 || 
             (spanStart span1 == spanStart span2 && spanEnd span1 <= spanEnd span2))
    ]