{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.SourceLocationQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
                      startPos, posAfter, posAt, posAtLineCol, emptySpan, spanFrom, 
                      spanTo, spanBetween, spanBetweenOrdered, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      mapLocated, advancePos, advancePosBy, advancePosByText,
                      comparePos, toErrorLocation, toErrorLocationWithSpan)
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.Char (isSpace)

-- Helper generators for SourceLocation tests
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  column <- choose (1, 100)
  offset <- choose (0, 10000)
  return $ SourcePos line column offset

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"

genString :: Gen String
genString = do
  len <- choose (0, 20)
  vectorOf len genChar

genText :: Gen T.Text
genText = T.pack <$> genString

genInt :: Gen Int
genInt = choose (-100, 100)

-- Test properties for SourceLocation module

-- Property 1: startPos is the starting position
prop_startPos_properties :: Bool
prop_startPos_properties = 
  let pos = startPos
  in posLine pos == 1 && posColumn pos == 1 && posOffset pos == 0

-- Property 2: posAfter advances line for newline
prop_posAfter_advances_line_for_newline :: SourcePos -> Bool
prop_posAfter_advances_line_for_newline pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 && 
     posColumn newPos == 1 && 
     posOffset newPos == posOffset pos + 1

-- Property 3: posAfter advances column for regular character
prop_posAfter_advances_column_for_regular_char :: SourcePos -> Property
prop_posAfter_advances_column_for_regular_char pos = 
  forAll genChar $ \c ->
    c /= '\n' && c /= '\t' ==> 
      let newPos = posAfter c pos
      in posLine newPos == posLine pos && 
         posColumn newPos == posColumn pos + 1 && 
         posOffset newPos == posOffset pos + 1

-- Property 4: posAfter handles tab correctly
prop_posAfter_handles_tab :: SourcePos -> Bool
prop_posAfter_handles_tab pos = 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine pos && 
     posColumn newPos == expectedColumn && 
     posOffset newPos == posOffset pos + 1

-- Property 5: posAt creates position with given line and column
prop_posAt_creates_position :: Int -> Int -> Property
prop_posAt_creates_position line col =
  line > 0 && col > 0 ==> 
    let pos = posAt line col
    in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- Property 6: posAtLineCol creates position with given line, column, and offset
prop_posAtLineCol_creates_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_position line col offset =
  line > 0 && col > 0 && offset >= 0 ==> 
    let pos = posAtLineCol line col offset
    in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- Property 7: emptySpan has same start and end
prop_emptySpan_same_start_end :: SourcePos -> Bool
prop_emptySpan_same_start_end pos = 
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos

-- Property 8: spanFrom creates empty span at position
prop_spanFrom_creates_empty_span :: SourcePos -> Bool
prop_spanFrom_creates_empty_span pos = 
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- Property 9: spanTo creates empty span at position
prop_spanTo_creates_empty_span :: SourcePos -> Bool
prop_spanTo_creates_empty_span pos = 
  let span = spanTo pos
  in spanStart span == pos && spanEnd span == pos

-- Property 10: spanBetween creates span with given start and end
prop_spanBetween_creates_span :: SourcePos -> SourcePos -> Bool
prop_spanBetween_creates_span pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in spanStart span == pos1 && spanEnd span == pos2

-- Property 11: spanBetweenOrdered creates ordered span
prop_spanBetweenOrdered_creates_ordered_span :: SourcePos -> SourcePos -> Bool
prop_spanBetweenOrdered_creates_ordered_span pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in comparePos start end /= GT

-- Property 12: mergeSpans creates span covering both spans
prop_mergeSpans_covers_both_spans :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_covers_both_spans span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergeStart = spanStart merged
      mergeEnd = spanEnd merged
  in comparePos mergeStart start1 /= GT && 
     comparePos mergeStart start2 /= GT &&
     comparePos end1 mergeEnd /= GT && 
     comparePos end2 mergeEnd /= GT

-- Property 13: isValidSpan checks if start <= end
prop_isValidSpan_checks_order :: SourceSpan -> Bool
prop_isValidSpan_checks_order span = 
  let start = spanStart span
      end = spanEnd span
  in isValidSpan span == (comparePos start end /= GT)

-- Property 14: locatedAt creates located value at position
prop_locatedAt_creates_located_value :: SourcePos -> Int -> Bool
prop_locatedAt_creates_located_value pos value = 
  let located = locatedAt pos value
  in locatedValue located == value && 
     locatedPos located == pos && 
     spanStart (locatedSpan located) == pos

-- Property 15: locatedWithSpan creates located value with span
prop_locatedWithSpan_creates_located_value :: SourceSpan -> String -> Bool
prop_locatedWithSpan_creates_located_value span value = 
  let located = locatedWithSpan span value
  in locatedValue located == value && 
     locatedSpan located == span && 
     locatedPos located == spanStart span

-- Property 16: mapLocated applies function to value
prop_mapLocated_applies_function :: SourceSpan -> Int -> Bool
prop_mapLocated_applies_function span value = 
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in locatedValue mapped == value * 2 && 
     locatedSpan mapped == span

-- Property 17: advancePos advances position by character
prop_advancePos_advances_by_char :: SourcePos -> Char -> Bool
prop_advancePos_advances_by_char pos c = 
  advancePos c pos == posAfter c pos

-- Property 18: advancePosBy advances position by string
prop_advancePosBy_advances_by_string :: SourcePos -> String -> Bool
prop_advancePosBy_advances_by_string pos s = 
  let finalPos = advancePosBy s pos
      expectedPos = foldl (flip advancePos) pos s
  in finalPos == expectedPos

-- Property 19: advancePosByText advances position by text
prop_advancePosByText_advances_by_text :: SourcePos -> T.Text -> Bool
prop_advancePosByText_advances_by_text pos text = 
  advancePosByText text pos == advancePosBy (T.unpack text) pos

-- Property 20: comparePos orders positions correctly
prop_comparePos_orders_correctly :: SourcePos -> SourcePos -> Bool
prop_comparePos_orders_correctly pos1 pos2 = 
  let result = comparePos pos1 pos2
      line1 = posLine pos1
      line2 = posLine pos2
      col1 = posColumn pos1
      col2 = posColumn pos2
  in if line1 < line2 then result == LT
     else if line1 > line2 then result == GT
     else if col1 < col2 then result == LT
     else if col1 > col2 then result == GT
     else result == EQ

-- Property 21: toErrorLocation converts position to error location
prop_toErrorLocation_converts_position :: SourcePos -> Bool
prop_toErrorLocation_converts_position pos = 
  let errLoc = toErrorLocation pos
  in line errLoc == posLine pos && 
     column errLoc == posColumn pos && 
     filePath errLoc == Nothing

-- Property 22: toErrorLocationWithSpan converts span to error location
prop_toErrorLocationWithSpan_converts_span :: SourceSpan -> Bool
prop_toErrorLocationWithSpan_converts_span span = 
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errLoc == posLine start && 
     column errLoc == posColumn start && 
     endLine errLoc == Just (posLine end) && 
     endColumn errLoc == Just (posColumn end) &&
     filePath errLoc == Nothing

-- Property 23: Located values implement HasLocation
prop_located_implements_HasLocation :: SourceSpan -> Int -> Bool
prop_located_implements_HasLocation span value = 
  let located = locatedWithSpan span value
  in getLocation located == span

-- Property 24: Located functor preserves span
prop_located_functor_preserves_span :: SourceSpan -> Int -> Bool
prop_located_functor_preserves_span span value = 
  let located = locatedWithSpan span value
      doubled = fmap (*2) located
  in locatedSpan doubled == span

-- Property 25: advancePosBy empty string returns original position
prop_advancePosBy_empty_string :: SourcePos -> Bool
prop_advancePosBy_empty_string pos = advancePosBy "" pos == pos

-- Property 26: advancePosByText empty text returns original position
prop_advancePosByText_empty_text :: SourcePos -> Bool
prop_advancePosByText_empty_text pos = advancePosByText T.empty pos == pos

-- Property 27: posAfter newline resets column to 1
prop_posAfter_newline_resets_column :: SourcePos -> Bool
prop_posAfter_newline_resets_column pos = 
  let newPos = posAfter '\n' pos
  in posColumn newPos == 1

-- Property 28: posAfter newline increments line
prop_posAfter_newline_increments_line :: SourcePos -> Bool
prop_posAfter_newline_increments_line pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1

-- Property 29: posAfter newline increments offset
prop_posAfter_newline_increments_offset :: SourcePos -> Bool
prop_posAfter_newline_increments_offset pos = 
  let newPos = posAfter '\n' pos
  in posOffset newPos == posOffset pos + 1

-- Property 30: posAfter tab aligns to next tab stop
prop_posAfter_tab_aligns_to_tab_stop :: SourcePos -> Bool
prop_posAfter_tab_aligns_to_tab_stop pos = 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos == expectedColumn

-- Property 31: posAfter regular char increments column
prop_posAfter_regular_char_increments_column :: SourcePos -> Property
prop_posAfter_regular_char_increments_column pos = 
  forAll genChar $ \c ->
    c /= '\n' && c /= '\t' ==> 
      let newPos = posAfter c pos
      in posColumn newPos == posColumn pos + 1

-- Property 32: posAfter regular char increments offset
prop_posAfter_regular_char_increments_offset :: SourcePos -> Property
prop_posAfter_regular_char_increments_offset pos = 
  forAll genChar $ \c ->
    c /= '\n' && c /= '\t' ==> 
      let newPos = posAfter c pos
      in posOffset newPos == posOffset pos + 1

-- Property 33: spanBetweenOrdered always has valid span
prop_spanBetweenOrdered_always_valid :: SourcePos -> SourcePos -> Bool
prop_spanBetweenOrdered_always_valid pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
  in isValidSpan span

-- Property 34: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_commutative span1 span2 = 
  mergeSpans span1 span2 == mergeSpans span2 span1

-- Property 35: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_associative span1 span2 span3 = 
  mergeSpans span1 (mergeSpans span2 span3) == mergeSpans (mergeSpans span1 span2) span3

-- Property 36: mergeSpans with empty span returns other span
prop_mergeSpans_with_empty :: SourceSpan -> SourcePos -> Bool
prop_mergeSpans_with_empty span pos = 
  let empty = emptySpan pos
  in mergeSpans span empty == span && mergeSpans empty span == span

-- Property 37: locatedAt and locatedWithSpan are consistent
prop_locatedAt_withSpan_consistent :: SourcePos -> Int -> Bool
prop_locatedAt_withSpan_consistent pos value = 
  let located1 = locatedAt pos value
      span = emptySpan pos
      located2 = locatedWithSpan span value
  in locatedValue located1 == locatedValue located2 && 
     locatedSpan located1 == locatedSpan located2

-- Property 38: mapLocated preserves position
prop_mapLocated_preserves_position :: SourceSpan -> Int -> Bool
prop_mapLocated_preserves_position span value = 
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in locatedPos mapped == locatedPos located

-- Property: mapLocated preserves span
prop_mapLocated_preserves_span :: SourceSpan -> Int -> Bool
prop_mapLocated_preserves_span span value = 
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in locatedSpan mapped == locatedSpan located

-- Property 39: advancePosBy is consistent with repeated posAfter
prop_advancePosBy_consistent_with_posAfter :: SourcePos -> String -> Bool
prop_advancePosBy_consistent_with_posAfter pos s = 
  let finalPos = advancePosBy s pos
      expectedPos = foldl (flip posAfter) pos s
  in finalPos == expectedPos

-- Property 40: SourcePos ordering is transitive
prop_sourcePos_ordering_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcePos_ordering_transitive pos1 pos2 pos3 = 
  comparePos pos1 pos2 == LT && comparePos pos2 pos3 == LT ==> 
    comparePos pos1 pos3 == LT

sourceLocationQuickCheckTests :: TestTree
sourceLocationQuickCheckTests = testGroup "SourceLocation QuickCheck Tests"
  [ testProperties "SourcePos Properties"
    [ ("startPos is the starting position", property prop_startPos_properties)
    , ("posAfter advances line for newline", property prop_posAfter_advances_line_for_newline)
    , ("posAfter advances column for regular character", property prop_posAfter_advances_column_for_regular_char)
    , ("posAfter handles tab correctly", property prop_posAfter_handles_tab)
    , ("posAt creates position with given line and column", property prop_posAt_creates_position)
    , ("posAtLineCol creates position with given line, column, and offset", property prop_posAtLineCol_creates_position)
    , ("posAfter newline resets column to 1", property prop_posAfter_newline_resets_column)
    , ("posAfter newline increments line", property prop_posAfter_newline_increments_line)
    , ("posAfter newline increments offset", property prop_posAfter_newline_increments_offset)
    , ("posAfter tab aligns to next tab stop", property prop_posAfter_tab_aligns_to_tab_stop)
    , ("posAfter regular char increments column", property prop_posAfter_regular_char_increments_column)
    , ("posAfter regular char increments offset", property prop_posAfter_regular_char_increments_offset)
    , ("comparePos orders positions correctly", property prop_comparePos_orders_correctly)
    , ("SourcePos ordering is transitive", property prop_sourcePos_ordering_transitive)
    ]
  , testProperties "SourceSpan Properties"
    [ ("emptySpan has same start and end", property prop_emptySpan_same_start_end)
    , ("spanFrom creates empty span at position", property prop_spanFrom_creates_empty_span)
    , ("spanTo creates empty span at position", property prop_spanTo_creates_empty_span)
    , ("spanBetween creates span with given start and end", property prop_spanBetween_creates_span)
    , ("spanBetweenOrdered creates ordered span", property prop_spanBetweenOrdered_creates_ordered_span)
    , ("spanBetweenOrdered always has valid span", property prop_spanBetweenOrdered_always_valid)
    , ("mergeSpans creates span covering both spans", property prop_mergeSpans_covers_both_spans)
    , ("mergeSpans is commutative", property prop_mergeSpans_commutative)
    , ("mergeSpans is associative", property prop_mergeSpans_associative)
    , ("mergeSpans with empty span returns other span", property prop_mergeSpans_with_empty)
    , ("isValidSpan checks if start <= end", property prop_isValidSpan_checks_order)
    ]
  , testProperties "Located Properties"
    [ ("locatedAt creates located value at position", property prop_locatedAt_creates_located_value)
    , ("locatedWithSpan creates located value with span", property prop_locatedWithSpan_creates_located_value)
    , ("locatedAt and locatedWithSpan are consistent", property prop_locatedAt_withSpan_consistent)
    , ("mapLocated applies function to value", property prop_mapLocated_applies_function)
    , ("mapLocated preserves span", property prop_mapLocated_preserves_span)
    , ("mapLocated preserves position", property prop_mapLocated_preserves_position)
    , ("Located values implement HasLocation", property prop_located_implements_HasLocation)
    , ("Located functor preserves span", property prop_located_functor_preserves_span)
    ]
  , testProperties "Position Advancement Properties"
    [ ("advancePos advances position by character", property prop_advancePos_advances_by_char)
    , ("advancePosBy advances position by string", property prop_advancePosBy_advances_by_string)
    , ("advancePosByText advances position by text", property prop_advancePosByText_advances_by_text)
    , ("advancePosBy empty string returns original position", property prop_advancePosBy_empty_string)
    , ("advancePosByText empty text returns original position", property prop_advancePosByText_empty_text)
    , ("advancePosBy is consistent with repeated posAfter", property prop_advancePosBy_consistent_with_posAfter)
    ]
  , testProperties "Error Location Conversion Properties"
    [ ("toErrorLocation converts position to error location", property prop_toErrorLocation_converts_position)
    , ("toErrorLocationWithSpan converts span to error location", property prop_toErrorLocationWithSpan_converts_span)
    ]
  ]

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

instance Arbitrary T.Text where
  arbitrary = genText