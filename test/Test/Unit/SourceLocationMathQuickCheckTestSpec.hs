{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Positive(Positive), getPositive, Arbitrary(..)
  , Gen, oneof, elements, listOf, listOf1, choose, sized, suchThat
  )

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , advancePosBy
  , advancePosByLine
  , advancePosByText
  , emptySpan
  , locatedAt
  , locatedPos
  , locatedSpan
  , locatedValue
  , mapLocated
  , markSpanEnd
  , markSpanStart
  , mergeSpans
  , posAfter
  , spanBetween
  , spanFrom
  , spanTo
  , startPos
  , toErrorLocation
  , toErrorLocationWithSpan
  , withLocationTracking
  , setCurrentPos
  , isValidSpan
  , spanStart
  , spanEnd
  )

import Compiler.Errors.Core (ErrorLocation(..))

import qualified Data.Text as T (pack, unpack)
import Data.Char (isSpace)

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- Positive <$> choose (1, 1000)
  col <- Positive <$> choose (1, 1000)
  offset <- Positive <$> choose (0, 10000)
  return $ SourcePos (getPositive line) (getPositive col) (getPositive offset)

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  -- Ensure end is not before start
  lineDiff <- choose (0, 10)
  colDiff <- choose (0, 50)
  let endLine = sourceLine start + lineDiff
      endCol = if lineDiff == 0 then sourceColumn start + colDiff else colDiff + 1
      endOffset = sourceOffset start + lineDiff * 100 + colDiff
  return $ SourceSpan start (SourcePos endLine (max 1 endCol) endOffset)

-- Generate text with various characters
genText :: Gen T.Text
genText = T.pack <$> listOf (oneof 
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , pure ' '
  , pure '\t'
  , pure '\n'
  , pure '\r'
  , elements ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=', '[', ']', '{', '}', '|', ';', ':', ',', '.', '<', '>', '/', '?']
  ])

-- Property: posAfter newline increments line L.and resets column
prop_pos_after_newline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_after_newline (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      next = posAfter '\n' pos
  in sourceLine next === line + 1 .&&.
     sourceColumn next === 1 .&&.
     sourceOffset next === offset + 1

-- Property: posAfter tab jumps to next tab stop (assuming 8-space tabs)
prop_pos_after_tab :: Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_after_tab (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      next = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in sourceLine next === line .&&.
     sourceColumn next === expectedCol .&&.
     sourceOffset next === offset + 1

-- Property: posAfter regular character increments column
prop_pos_after_regular_char :: Positive Int -> Positive Int -> Positive Int -> Char -> Property
prop_pos_after_regular_char (Positive line) (Positive col) (Positive offset) c =
  c `notElem` ['\n', '\t'] ==>
  let pos = SourcePos line col offset
      next = posAfter c pos
  in sourceLine next === line .&&.
     sourceColumn next === col + 1 .&&.
     sourceOffset next === offset + 1

-- Property: advancePosByText correctly handles empty text
prop_advance_pos_by_empty_text :: SourcePos -> Property
prop_advance_pos_by_empty_text pos = advancePosByText T.empty pos === pos

-- Property: advancePosByText is consistent with repeated posAfter
prop_advance_pos_by_text_consistency :: SourcePos -> T.Text -> Property
prop_advance_pos_by_text_consistency pos text =
  let finalPos = advancePosByText text pos
      chars = T.unpack text
      manualPos = foldl posAfter pos chars
  in finalPos === manualPos

-- Property: advancePosByLine preserves offset but changes line L.and column
prop_advance_pos_by_line :: SourcePos -> Positive Int -> Property
prop_advance_pos_by_line pos (Positive lines) =
  let advanced = advancePosByLine lines pos
  in sourceLine advanced === sourceLine pos + lines .&&.
     sourceColumn advanced === 1 .&&.
     sourceOffset advanced === sourceOffset pos

-- Property: spanFrom creates zero-L.length span
prop_span_from_creates_zero_length :: SourcePos -> Property
prop_span_from_creates_zero_length pos =
  let span = spanFrom pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates zero-L.length span ending at position
prop_span_to_creates_zero_length :: SourcePos -> Property
prop_span_to_creates_zero_length pos =
  let span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween preserves bounds
prop_span_between_preserves_bounds :: SourcePos -> SourcePos -> Property
prop_span_between_preserves_bounds start end =
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans selects earliest start L.and latest end
prop_merge_spans_selects_bounds :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_selects_bounds span1 span2 =
  let merged = mergeSpans span1 span2
      earliestStart = if sourceLine (spanStart span1) < sourceLine (spanStart span2) ||
                        (sourceLine (spanStart span1) == sourceLine (spanStart span2) && 
                         sourceColumn (spanStart span1) <= sourceColumn (spanStart span2))
                     then spanStart span1 else spanStart span2
      latestEnd = if sourceLine (spanEnd span1) > sourceLine (spanEnd span2) ||
                     (sourceLine (spanEnd span1) == sourceLine (spanEnd span2) && 
                      sourceColumn (spanEnd span1) >= sourceColumn (spanEnd span2))
                   then spanEnd span1 else spanEnd span2
  in spanStart merged === earliestStart .&&. spanEnd merged === latestEnd

-- Property: isValidSpan correctly identifies invalid spans
prop_is_valid_span_detection :: SourceSpan -> Property
prop_is_valid_span_detection span =
  let start = spanStart span
      end = spanEnd span
      valid = sourceLine start < sourceLine end ||
              (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end)
  in isValidSpan span === valid

-- Property: locatedAt creates correct location
prop_located_at_correct :: SourcePos -> String -> Property
prop_located_at_correct pos value =
  let loc = locatedAt pos value
  in locatedPos loc === pos .&&.
     locatedValue loc === value .&&.
     locatedSpan loc === SourceSpan pos pos

-- Property: mapLocated preserves span but transforms value
prop_map_located_preserves_span :: SourceSpan -> String -> Property
prop_map_located_preserves_span span value =
  let loc = Located value (spanStart span) span
      mapped = mapLocated L.length loc
  in locatedSpan mapped === span .&&.
     locatedValue mapped === L.length value

-- Property: toErrorLocation extracts point information
prop_to_error_location_point :: SourcePos -> Property
prop_to_error_location_point pos =
  let errLoc = toErrorLocation pos
  in line errLoc === sourceLine pos .&&.
     column errLoc === sourceColumn pos .&&.
     endLine errLoc === Nothing .&&.
     endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan preserves span information
prop_to_error_location_with_span :: SourceSpan -> Property
prop_to_error_location_with_span span =
  let start = spanStart span
      end = spanEnd span
      errLoc = toErrorLocationWithSpan span
  in line errLoc === sourceLine start .&&.
     column errLoc === sourceColumn start .&&.
     endLine errLoc === Just (sourceLine end) .&&.
     endColumn errLoc === Just (sourceColumn end)

-- Property: emptySpan creates zero-L.length span
prop_empty_span_creates_zero_length :: SourcePos -> Property
prop_empty_span_creates_zero_length pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: advancePosBy handles empty string
prop_advance_pos_by_empty_string :: SourcePos -> Property
prop_advance_pos_by_empty_string pos = advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with advancePosByText
prop_advance_pos_by_consistency :: SourcePos -> String -> Property
prop_advance_pos_by_consistency pos str =
  let byString = advancePosBy str pos
      byText = advancePosByText (T.pack str) pos
  in byString === byText

-- Property: startPos has expected values
prop_start_pos_values :: Property
prop_start_pos_values =
  sourceLine startPos === 1 .&&.
  sourceColumn startPos === 1 .&&.
  sourceOffset startPos === 0

tests :: TestTree
tests =
  testGroup "SourceLocation Math QuickCheck Tests"
    [ fastProperty "posAfter newline increments line L.and resets column" prop_pos_after_newline
    , fastProperty "posAfter tab jumps to next tab stop" prop_pos_after_tab
    , fastProperty "posAfter regular character increments column" prop_pos_after_regular_char
    , fastProperty "advancePosByText correctly handles empty text" prop_advance_pos_by_empty_text
    , fastProperty "advancePosByText is consistent with repeated posAfter" prop_advance_pos_by_text_consistency
    , fastProperty "advancePosByLine preserves offset but changes line L.and column" prop_advance_pos_by_line
    , fastProperty "spanFrom creates zero-L.length span" prop_span_from_creates_zero_length
    , fastProperty "spanTo creates zero-L.length span ending at position" prop_span_to_creates_zero_length
    , fastProperty "spanBetween preserves bounds" prop_span_between_preserves_bounds
    , fastProperty "mergeSpans selects earliest start L.and latest end" prop_merge_spans_selects_bounds
    , fastProperty "isValidSpan correctly identifies invalid spans" prop_is_valid_span_detection
    , fastProperty "locatedAt creates correct location" prop_located_at_correct
    , fastProperty "mapLocated preserves span but transforms value" prop_map_located_preserves_span
    , fastProperty "toErrorLocation extracts point information" prop_to_error_location_point
    , fastProperty "toErrorLocationWithSpan preserves span information" prop_to_error_location_with_span
    , fastProperty "emptySpan creates zero-L.length span" prop_empty_span_creates_zero_length
    , fastProperty "advancePosBy handles empty string" prop_advance_pos_by_empty_string
    , fastProperty "advancePosBy is consistent with advancePosByText" prop_advance_pos_by_consistency
    , fastProperty "startPos has expected values" prop_start_pos_values
    ]