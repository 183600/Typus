{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationPropertiesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, oneof, elements, Positive(..), NonNegative(..))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
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
import Data.Char (isSpace)

-- Helper to generate valid positions
genValidPos :: IO SourcePos
genValidPos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ posAtLineCol line col offset

-- Property: posAfter newline increments line and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&. posColumn newPos === 1

-- Property: posAfter tab advances to next tab stop
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol

-- Property: posAfter regular char increments column by 1
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos c =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in posColumn newPos === posColumn pos + 1 .&&. posLine newPos === posLine pos

-- Property: posAfter always increments offset
prop_posAfter_increments_offset :: SourcePos -> Char -> Property
prop_posAfter_increments_offset pos c =
  let newPos = posAfter c pos
  in posOffset newPos === posOffset pos + 1

-- Property: spanFrom creates valid span
prop_spanFrom_valid :: SourcePos -> Property
prop_spanFrom_valid pos = isValidSpan (spanFrom pos) === True

-- Property: spanFrom has same start and end
prop_spanFrom_same_start_end :: SourcePos -> Property
prop_spanFrom_same_start_end pos =
  let span = spanFrom pos
  in spanStart span === spanEnd span

-- Property: spanBetween creates span with correct endpoints
prop_spanBetween_correct_endpoints :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct_endpoints start end =
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: spanBetween is valid if start <= end
prop_spanBetween_valid_order :: Positive Int -> Positive Int -> Property
prop_spanBetween_valid_order (Positive line1) (Positive line2) =
  let start = posAtLineCol line1 1 0
      end = posAtLineCol (line1 + line2) 1 0
  in isValidSpan (spanBetween start end) === True

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_contains_both pos1 pos2 pos3 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in spanStart merged === spanStart span1 .&&. spanEnd merged === spanEnd span2

-- Property: locatedAt preserves value
prop_locatedAt_preserves_value :: SourcePos -> String -> Property
prop_locatedAt_preserves_value pos value =
  locatedValue (locatedAt pos value) === value

-- Property: locatedAt preserves position
prop_locatedAt_preserves_position :: SourcePos -> String -> Property
prop_locatedAt_preserves_position pos value =
  locatedPos (locatedAt pos value) === pos

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: SourcePos -> Int -> Property
prop_mapLocated_preserves_location pos value =
  let located = locatedAt pos value
      transformed = mapLocated (*2) located
  in locatedPos transformed === locatedPos located

-- Property: mapLocated applies function correctly
prop_mapLocated_applies_function :: SourcePos -> Int -> Property
prop_mapLocated_applies_function pos value =
  let located = locatedAt pos value
      transformed = mapLocated (*2) located
  in locatedValue transformed === value * 2

-- Property: advancePosByText empty text returns same position
prop_advancePosByText_empty :: SourcePos -> Property
prop_advancePosByText_empty pos = advancePosByText pos "" === pos

-- Property: advancePosByText single character
prop_advancePosByText_single_char :: SourcePos -> Char -> Property
prop_advancePosByText_single_char pos c =
  advancePosByText pos [c] === posAfter c pos

-- Property: advancePosByText newline handling
prop_advancePosByText_newline :: SourcePos -> String -> Property
prop_advancePosByText_newline pos text =
  '\n' `elem` text ==>
  let finalPos = advancePosByText pos text
      expectedLine = posLine pos + length (filter (== '\n') text)
  in posLine finalPos === expectedLine

-- Property: emptySpan is invalid
prop_emptySpan_invalid :: Property
prop_emptySpan_invalid = isValidSpan emptySpan === False

-- Property: spanTo creates valid span
prop_spanTo_valid :: SourcePos -> Property
prop_spanTo_valid pos = isValidSpan (spanTo pos) === True

-- Property: spanTo has same start and end
prop_spanTo_same_start_end :: SourcePos -> Property
prop_spanTo_same_start_end pos =
  let span = spanTo pos
  in spanStart span === spanEnd span

-- Property: locatedWithSpan preserves value and span
prop_locatedWithSpan_preserves_both :: SourceSpan -> String -> Property
prop_locatedWithSpan_preserves_both span value =
  let located = locatedWithSpan span value
  in locatedValue located === value .&&. locatedSpan located === span

tests :: TestTree
tests =
  testGroup "SourceLocation QuickCheck Property Tests"
    [ fastProperty "posAfter newline increments line and resets column" prop_posAfter_newline
    , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab
    , fastProperty "posAfter regular char increments column by 1" prop_posAfter_regular_char
    , fastProperty "posAfter always increments offset" prop_posAfter_increments_offset
    , fastProperty "spanFrom creates valid span" prop_spanFrom_valid
    , fastProperty "spanFrom has same start and end" prop_spanFrom_same_start_end
    , fastProperty "spanBetween creates span with correct endpoints" prop_spanBetween_correct_endpoints
    , fastProperty "spanBetween is valid if start <= end" prop_spanBetween_valid_order
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
    , fastProperty "locatedAt preserves value" prop_locatedAt_preserves_value
    , fastProperty "locatedAt preserves position" prop_locatedAt_preserves_position
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "mapLocated applies function correctly" prop_mapLocated_applies_function
    , fastProperty "advancePosByText empty text returns same position" prop_advancePosByText_empty
    , fastProperty "advancePosByText single character" prop_advancePosByText_single_char
    , fastProperty "advancePosByText newline handling" prop_advancePosByText_newline
    , fastProperty "emptySpan is invalid" prop_emptySpan_invalid
    , fastProperty "spanTo creates valid span" prop_spanTo_valid
    , fastProperty "spanTo has same start and end" prop_spanTo_same_start_end
    , fastProperty "locatedWithSpan preserves value and span" prop_locatedWithSpan_preserves_both
    ]