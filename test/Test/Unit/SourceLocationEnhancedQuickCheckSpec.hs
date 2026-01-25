{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.SourceLocationEnhancedQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.List (sort)
import Control.Monad.State (runState)

-- Arbitrary instances are now defined in SourceLocation module

-- ============================================================================
-- SourceLocation Module QuickCheck Tests
-- ============================================================================

-- | Test SourcePos properties
prop_sourcePos_startPos_properties :: Bool
prop_sourcePos_startPos_properties = 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

prop_sourcePos_posAfter_newline :: SourcePos -> Bool
prop_sourcePos_posAfter_newline pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 && 
     posColumn newPos == 1 && 
     posOffset newPos == posOffset pos + 1

prop_sourcePos_posAfter_tab :: SourcePos -> Bool
prop_sourcePos_posAfter_tab pos = 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos == expectedColumn && 
     posOffset newPos == posOffset pos + 1

prop_sourcePos_posAfter_regular_char :: SourcePos -> Char -> Property
prop_sourcePos_posAfter_regular_char pos c = 
  c /= '\n' && c /= '\t' ==> 
    let newPos = posAfter c pos
    in posColumn newPos == posColumn pos + 1 && 
       posOffset newPos == posOffset pos + 1

prop_sourcePos_posAt :: Int -> Int -> Bool
prop_sourcePos_posAt lineNum colNum = 
  let pos = posAt lineNum colNum
  in posLine pos == lineNum && posColumn pos == colNum && posOffset pos == 0

prop_sourcePos_posAtLineCol :: Int -> Int -> Int -> Bool
prop_sourcePos_posAtLineCol lineNum colNum offsetVal = 
  let pos = posAtLineCol lineNum colNum offsetVal
  in posLine pos == lineNum && posColumn pos == colNum && posOffset pos == offsetVal

-- | Test SourceSpan properties
prop_sourceSpan_emptySpan :: SourcePos -> Bool
prop_sourceSpan_emptySpan pos = 
  let sourceSpan = emptySpan pos
  in spanStart sourceSpan == pos && spanEnd sourceSpan == pos

prop_sourceSpan_spanFrom :: SourcePos -> Bool
prop_sourceSpan_spanFrom pos = 
  let sourceSpan = spanFrom pos
  in spanStart sourceSpan == pos && spanEnd sourceSpan == pos

prop_sourceSpan_spanTo :: SourcePos -> Bool
prop_sourceSpan_spanTo pos = 
  let sourceSpan = spanTo pos
  in spanStart sourceSpan == pos && spanEnd sourceSpan == pos

prop_sourceSpan_spanBetween :: SourcePos -> SourcePos -> Bool
prop_sourceSpan_spanBetween pos1 pos2 = 
  let sourceSpan = spanBetween pos1 pos2
  in spanStart sourceSpan == pos1 && spanEnd sourceSpan == pos2

prop_sourceSpan_spanBetween_ordered :: SourcePos -> SourcePos -> Bool
prop_sourceSpan_spanBetween_ordered pos1 pos2 = 
  let sourceSpan = spanBetweenOrdered pos1 pos2
      (start, end) = if pos1 <= pos2 then (pos1, pos2) else (pos2, pos1)
  in spanStart sourceSpan == start && spanEnd sourceSpan == end

prop_sourceSpan_mergeSpans :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_sourceSpan_mergeSpans start1 end1 start2 end2 = 
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      merged = mergeSpans span1 span2
      expectedStart = SourcePos
        { posLine = min (posLine start1) (posLine start2)
        , posColumn = min (posColumn start1) (posColumn start2)
        , posOffset = min (posOffset start1) (posOffset start2)
        }
      expectedEnd = SourcePos
        { posLine = max (posLine end1) (posLine end2)
        , posColumn = max (posColumn end1) (posColumn end2)
        , posOffset = max (posOffset end1) (posOffset end2)
        }
  in spanStart merged == expectedStart && spanEnd merged == expectedEnd

prop_sourceSpan_isValidSpan :: SourcePos -> SourcePos -> Bool
prop_sourceSpan_isValidSpan start end = 
  let sourceSpan = SourceSpan start end
  in isValidSpan sourceSpan == (start <= end)

prop_sourceSpan_isValidBlockSpan :: SourcePos -> SourcePos -> Bool
prop_sourceSpan_isValidBlockSpan start end = 
  let sourceSpan = SourceSpan start end
  in isValidBlockSpan sourceSpan == isValidSpan sourceSpan

-- | Test Located properties
prop_located_at :: SourcePos -> Int -> Bool
prop_located_at pos value = 
  let located = locatedAt pos value
  in locValue located == value && 
     locPos located == pos && 
     locSpan located == emptySpan pos

prop_located_with_span :: SourceSpan -> Int -> Bool
prop_located_with_span sourceSpan value = 
  let located = locatedWithSpan sourceSpan value
  in locValue located == value && 
     locPos located == spanStart sourceSpan && 
     locSpan located == sourceSpan

prop_located_value :: SourceSpan -> Int -> Bool
prop_located_value sourceSpan value = 
  let located = locatedWithSpan sourceSpan value
  in locatedValue located == value

prop_located_span :: SourceSpan -> Int -> Bool
prop_located_span sourceSpan value = 
  let located = locatedWithSpan sourceSpan value
  in locatedSpan located == sourceSpan

prop_located_pos :: SourceSpan -> Int -> Bool
prop_located_pos sourceSpan value = 
  let located = locatedWithSpan sourceSpan value
  in locatedPos located == spanStart sourceSpan

prop_map_located :: SourceSpan -> Int -> Bool
prop_map_located sourceSpan value = 
  let located = locatedWithSpan sourceSpan value
      doubled = mapLocated (*2) located
  in locValue doubled == value * 2 && 
     locPos doubled == locPos located && 
     locSpan doubled == locSpan located

-- | Test LocationTracker properties
prop_location_tracker_run :: Int -> Bool
prop_location_tracker_run value = 
  let action = return value
  in runLocationTracker action == value

prop_location_tracker_get_set :: SourcePos -> Bool
prop_location_tracker_get_set pos = 
  let action = do
        setCurrentPos pos
        getCurrentPos
  in runLocationTracker action == pos

prop_location_tracker_mark_span :: SourcePos -> String -> Bool
prop_location_tracker_mark_span startPos text = 
  let action = do
        setCurrentPos startPos
        start <- markSpanStart
        _ <- mapM_ (\c -> setCurrentPos (posAfter c startPos)) text
        end <- markSpanEnd start
        return end
      sourceSpan = runLocationTracker action
      expectedEnd = foldl (flip posAfter) startPos text
  in spanStart sourceSpan == startPos && spanEnd sourceSpan == expectedEnd

-- | Test position advancement properties
prop_advance_pos_equals_posAfter :: Char -> SourcePos -> Bool
prop_advance_pos_equals_posAfter c pos = advancePos c pos == posAfter c pos

prop_advance_pos_by :: String -> SourcePos -> Bool
prop_advance_pos_by chars pos = 
  advancePosBy chars pos == foldl (flip posAfter) pos chars

prop_advance_pos_by_line :: SourcePos -> Int -> Bool
prop_advance_pos_by_line pos numLines = 
  let newPos = advancePosByLine numLines pos
  in posLine newPos == posLine pos + numLines && 
     posColumn newPos == 1

-- | Test error location conversion properties
prop_to_error_location :: SourcePos -> Bool
prop_to_error_location pos = 
  let errLoc = toErrorLocation pos
  in line errLoc == posLine pos && 
     column errLoc == posColumn pos && 
     filePath errLoc == Nothing && 
     endLine errLoc == Nothing && 
     endColumn errLoc == Nothing

prop_to_error_location_with_span :: SourceSpan -> Bool
prop_to_error_location_with_span sourceSpan = 
  let errLoc = toErrorLocationWithSpan sourceSpan
      start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in line errLoc == posLine start && 
     column errLoc == posColumn start && 
     endLine errLoc == Just (posLine end) && 
     endColumn errLoc == Just (posColumn end) && 
     filePath errLoc == Nothing

-- | Test ordering properties
prop_source_pos_ordering :: SourcePos -> SourcePos -> Bool
prop_source_pos_ordering pos1 pos2 = 
  let result = compare pos1 pos2
      lineCompare = compare (posLine pos1) (posLine pos2)
      colCompare = compare (posColumn pos1) (posColumn pos2)
      offsetCompare = compare (posOffset pos1) (posOffset pos2)
  in if lineCompare /= EQ 
     then result == lineCompare
     else if colCompare /= EQ 
          then result == colCompare
          else result == offsetCompare

prop_source_span_ordering :: SourceSpan -> SourceSpan -> Bool
prop_source_span_ordering span1 span2 = 
  let result = compare span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
  in result == compare start1 start2

-- | Test span combination properties
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_merge_spans_associative span1 span2 span3 = 
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in result1 == result2

prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Bool
prop_merge_spans_commutative span1 span2 = 
  let result1 = mergeSpans span1 span2
      result2 = mergeSpans span2 span1
  in result1 == result2

prop_merge_spans_idempotent :: SourceSpan -> Bool
prop_merge_spans_idempotent sourceSpan = 
  let result = mergeSpans sourceSpan sourceSpan
  in result == sourceSpan

-- | Test location tracking properties
prop_with_location_tracking :: SourcePos -> Int -> Bool
prop_with_location_tracking pos value = 
  let action = do
        setCurrentPos (advancePosByLine 1 pos)
        return value
      (result, finalPos) = withLocationTracking pos action
      expectedFinalPos = advancePosByLine 1 pos
  in result == value && finalPos == expectedFinalPos

-- | Test span coverage properties
prop_span_between_ordered_covers_both :: SourcePos -> SourcePos -> Bool
prop_span_between_ordered_covers_both pos1 pos2 = 
  let sourceSpan = spanBetweenOrdered pos1 pos2
  in spanStart sourceSpan <= spanEnd sourceSpan && 
     (spanStart sourceSpan == pos1 || spanStart sourceSpan == pos2) && 
     (spanEnd sourceSpan == pos1 || spanEnd sourceSpan == pos2)

-- | Test position distance properties
prop_pos_distance_symmetric :: SourcePos -> SourcePos -> Bool
prop_pos_distance_symmetric pos1 pos2 = 
  let dist1 = abs (posOffset pos2 - posOffset pos1)
      dist2 = abs (posOffset pos1 - posOffset pos2)
  in dist1 == dist2

prop_pos_distance_non_negative :: SourcePos -> SourcePos -> Bool
prop_pos_distance_non_negative pos1 pos2 = 
  let dist = abs (posOffset pos2 - posOffset pos1)
  in dist >= 0

prop_line_distance_non_negative :: SourcePos -> SourcePos -> Bool
prop_line_distance_non_negative pos1 pos2 = 
  let dist = abs (posLine pos2 - posLine pos1)
  in dist >= 0

-- | Test span validity properties
prop_valid_span_start_end :: SourceSpan -> Bool
prop_valid_span_start_end sourceSpan = 
  let start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in if isValidSpan sourceSpan 
     then start <= end
     else True  -- Invalid spans can have any order

prop_merge_spans_preserves_validity :: SourceSpan -> SourceSpan -> Bool
prop_merge_spans_preserves_validity span1 span2 = 
  let merged = mergeSpans span1 span2
      bothValid = isValidSpan span1 && isValidSpan span2
  in if bothValid 
     then isValidSpan merged
     else True  -- If inputs are invalid, output validity is not guaranteed

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "SourceLocation Module QuickCheck Properties"
  [ -- SourcePos tests
    testProperty "startPos has correct properties" prop_sourcePos_startPos_properties,
    testProperty "posAfter handles newline correctly" prop_sourcePos_posAfter_newline,
    testProperty "posAfter handles tab correctly" prop_sourcePos_posAfter_tab,
    testProperty "posAfter handles regular characters correctly" prop_sourcePos_posAfter_regular_char,
    testProperty "posAt creates position with correct line and column" prop_sourcePos_posAt,
    testProperty "posAtLineCol creates position with correct line, column, and offset" prop_sourcePos_posAtLineCol,
    
    -- SourceSpan tests
    testProperty "emptySpan creates span with same start and end" prop_sourceSpan_emptySpan,
    testProperty "spanFrom creates span starting at position" prop_sourceSpan_spanFrom,
    testProperty "spanTo creates span ending at position" prop_sourceSpan_spanTo,
    testProperty "spanBetween creates span between two positions" prop_sourceSpan_spanBetween,
    testProperty "spanBetweenOrdered creates ordered span" prop_sourceSpan_spanBetween_ordered,
    testProperty "mergeSpans combines spans correctly" prop_sourceSpan_mergeSpans,
    testProperty "isValidSpan checks span validity" prop_sourceSpan_isValidSpan,
    testProperty "isValidBlockSpan is equivalent to isValidSpan" prop_sourceSpan_isValidBlockSpan,
    
    -- Located tests
    testProperty "locatedAt creates located value at position" prop_located_at,
    testProperty "locatedWithSpan creates located value with span" prop_located_with_span,
    testProperty "locatedValue extracts value correctly" prop_located_value,
    testProperty "locatedSpan extracts span correctly" prop_located_span,
    testProperty "locatedPos extracts position correctly" prop_located_pos,
    testProperty "mapLocated applies function to value" prop_map_located,
    
    -- LocationTracker tests
    testProperty "runLocationTracker executes action" prop_location_tracker_run,
    testProperty "get/set position works correctly" prop_location_tracker_get_set,
    testProperty "markSpan tracks span correctly" prop_location_tracker_mark_span,
    
    -- Position advancement tests
    testProperty "advancePos equals posAfter" prop_advance_pos_equals_posAfter,
    testProperty "advancePosBy advances by multiple characters" prop_advance_pos_by,
    testProperty "advancePosByLine advances by lines" prop_advance_pos_by_line,
    
    -- Error location conversion tests
    testProperty "toErrorLocation converts position correctly" prop_to_error_location,
    testProperty "toErrorLocationWithSpan converts span correctly" prop_to_error_location_with_span,
    
    -- Ordering tests
    testProperty "SourcePos ordering is consistent" prop_source_pos_ordering,
    testProperty "SourceSpan ordering is based on start position" prop_source_span_ordering,
    
    -- Span combination tests
    testProperty "mergeSpans is associative" prop_merge_spans_associative,
    testProperty "mergeSpans is commutative" prop_merge_spans_commutative,
    testProperty "mergeSpans is idempotent" prop_merge_spans_idempotent,
    
    -- Location tracking tests
    testProperty "withLocationTracking tracks position correctly" prop_with_location_tracking,
    
    -- Span coverage tests
    testProperty "spanBetweenOrdered covers both positions" prop_span_between_ordered_covers_both,
    
    -- Distance tests
    testProperty "position distance is symmetric" prop_pos_distance_symmetric,
    testProperty "position distance is non-negative" prop_pos_distance_non_negative,
    testProperty "line distance is non-negative" prop_line_distance_non_negative,
    
    -- Validity tests
    testProperty "valid span has start <= end" prop_valid_span_start_end,
    testProperty "mergeSpans preserves validity" prop_merge_spans_preserves_validity
  ]