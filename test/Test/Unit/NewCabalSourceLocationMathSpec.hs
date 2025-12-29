{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, oneof, elements)
import Test.QuickCheck.Gen (vectorOf)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
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
  , advancePos
  , advancePosByText
  )

import Data.Text (Text)
import qualified Data.Text as T

-- Arbitrary instances for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    col <- choose (1, 100)
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

-- Arbitrary instances for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = SourcePos 
          { posLine = posLine start + (endOffset `div` 50)
          , posColumn = posColumn start + (endOffset `mod` 50)
          , posOffset = posOffset start + endOffset
          }
    return $ SourceSpan start end

-- Property: startPos has positive coordinates
prop_start_pos_positive :: Property
prop_start_pos_positive = 
  property $ posLine startPos > 0 &&. posColumn startPos > 0 &&. posOffset startPos >= 0

-- Property: posAfter newline increments line and resets column
prop_pos_after_newline :: SourcePos -> Property
prop_pos_after_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 &&. posColumn newPos === 1

-- Property: posAfter tab aligns to next tab stop (multiple of 8 + 1)
prop_pos_after_tab :: SourcePos -> Property
prop_pos_after_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedCol

-- Property: posAfter regular character increments column and offset
prop_pos_after_regular :: SourcePos -> Char -> Property
prop_pos_after_regular pos c =
  let notSpecial = c `notElem` ['\n', '\t']
      newPos = posAfter c pos
  in classify notSpecial "regular character" $
     property $ notSpecial ==> 
       posColumn newPos === posColumn pos + 1 &&. 
       posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with given line and column
prop_pos_at_creates_correct :: Int -> Int -> Property
prop_pos_at_creates_correct line col =
  let pos = posAt line col
  in property $ line > 0 && col > 0 ==> 
    posLine pos === line &&. posColumn pos === col &&. posOffset pos === 0

-- Property: emptySpan has same start and end
prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos &&. spanEnd span === pos

-- Property: spanBetween creates valid span when start <= end
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid start end =
  let span = spanBetween start end
      valid = posLine start < posLine end || 
              (posLine start == posLine end && posColumn start <= posColumn end)
  in classify valid "valid positions" $
     property $ valid ==> isValidSpan span

-- Property: mergeSpans contains both original spans
prop_merge_spans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ 
    spanStart merged <= spanStart span1 &&. 
    spanEnd merged >= spanEnd span1 &&.
    spanStart merged <= spanStart span2 &&. 
    spanEnd merged >= spanEnd span2

-- Property: mergeSpans is commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: locatedAt creates located value with correct position
prop_located_at_correct :: SourcePos -> String -> Property
prop_located_at_correct pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos &&. locatedValue located === value

-- Property: mapLocated preserves position but transforms value
prop_map_located_preserves_position :: SourcePos -> String -> Property
prop_map_located_preserves_position pos value =
  let located = locatedAt pos value
      mapped = mapLocated length located
  in property $ locatedPos mapped === locatedPos located &&. 
                 locatedValue mapped === length value

-- Property: advancePosByText correctly handles multi-line text
prop_advance_pos_by_text_multiline :: SourcePos -> Property
prop_advance_pos_by_text_multiline pos =
  let text = T.unlines ["line1", "line2", "line3"]
      finalPos = advancePosByText text pos
  in property $ posLine finalPos === posLine pos + 2 &&. posColumn finalPos === 5

-- Property: advancePosByText offset increases by text length
prop_advance_pos_by_text_offset :: SourcePos -> String -> Property
prop_advance_pos_by_text_offset pos str =
  let text = T.pack str
      finalPos = advancePosByText text pos
      expectedOffset = posOffset pos + T.length text
  in property $ posOffset finalPos === expectedOffset

tests :: TestTree
tests = testGroup "NewCabalSourceLocationMathSpec"
  [ fastProperty "startPos positive" prop_start_pos_positive
  , fastProperty "posAfter newline" prop_pos_after_newline
  , fastProperty "posAfter tab" prop_pos_after_tab
  , fastProperty "posAfter regular" prop_pos_after_regular
  , fastProperty "posAt creates correct" prop_pos_at_creates_correct
  , fastProperty "emptySpan same start end" prop_empty_span_same_start_end
  , fastProperty "spanBetween valid" prop_span_between_valid
  , fastProperty "mergeSpans contains both" prop_merge_spans_contains_both
  , fastProperty "mergeSpans commutative" prop_merge_spans_commutative
  , fastProperty "locatedAt correct" prop_located_at_correct
  , fastProperty "mapLocated preserves position" prop_map_located_preserves_position
  , fastProperty "advancePosByText multiline" prop_advance_pos_by_text_multiline
  , fastProperty "advancePosByText offset" prop_advance_pos_by_text_offset
  ]