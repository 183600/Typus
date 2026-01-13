module Test.Unit.SourceLocationMathQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T

-- | Test that startPos has correct initial values
prop_start_pos_values :: Property
prop_start_pos_values = property $
  posLine startPos == 1 &&
  posColumn startPos == 1 &&
  posOffset startPos == 0

-- | Test that posAfter correctly updates line for newline
prop_pos_after_newline :: SourcePos -> Property
prop_pos_after_newline pos = 
  let newPos = posAfter '\n' pos
  in property $ 
    posLine newPos == posLine pos + 1 &&
    posColumn newPos == 1 &&
    posOffset newPos == posOffset pos + 1

-- | Test that posAfter correctly updates column for tab
prop_pos_after_tab :: SourcePos -> Property
prop_pos_after_tab pos = 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ 
    posLine newPos == posLine pos &&
    posColumn newPos == expectedColumn &&
    posOffset newPos == posOffset pos + 1

-- | Test that posAfter correctly updates column for regular character
prop_pos_after_regular :: Char -> SourcePos -> Property
prop_pos_after_regular c pos = 
  let notSpecial = c `notElem` ['\n', '\t']
      newPos = posAfter c pos
  in property $ 
    notSpecial ==> 
    (posLine newPos == posLine pos &&
     posColumn newPos == posColumn pos + 1 &&
     posOffset newPos == posOffset pos + 1)

-- | Test that posAt creates correct position
prop_pos_at_correct :: Int -> Int -> Int -> Property
prop_pos_at_correct line col offset = 
  let pos = posAt line col offset
  in property $ 
    posLine pos == line &&
    posColumn pos == col &&
    posOffset pos == offset

-- | Test that emptySpan has valid properties
prop_empty_span_properties :: Property
prop_empty_span_properties = 
  let span = emptySpan startPos
  in property $ 
    spanStart span == startPos &&
    spanEnd span == startPos

-- | Test that spanFrom creates correct span
prop_span_from_correct :: SourcePos -> Int -> Property
prop_span_from_correct pos len = 
  let span = spanFrom pos len
      endPos = advancePosBy pos len
  in property $ 
    spanStart span == pos &&
    spanEnd span == endPos

-- | Test that mergeSpans correctly combines spans
prop_merge_spans_correct :: SourcePos -> SourcePos -> Property
prop_merge_spans_correct pos1 pos2 = 
  let span1 = spanFrom pos1 5
      span2 = spanFrom pos2 3
      merged = mergeSpans span1 span2
      start = min pos1 pos2
      end1 = advancePosBy pos1 5
      end2 = advancePosBy pos2 3
      end = max end1 end2
  in property $ 
    spanStart merged == start &&
    spanEnd merged == end

-- | Test that advancePosBy correctly updates position
prop_advance_pos_by_correct :: SourcePos -> Int -> Property
prop_advance_pos_by_correct pos steps = 
  let newPos = advancePosBy pos steps
  in property $ 
    posOffset newPos == posOffset pos + steps

-- | Test that advancePosByText handles newlines correctly
prop_advance_pos_by_text_newlines :: SourcePos -> String -> Property
prop_advance_pos_by_text_newlines pos s = 
  let withNewlines = s ++ "\n\n"
      newPos = advancePosByText pos withNewlines
      newlineCount = length (filter (== '\n') withNewlines)
  in property $ 
    posLine newPos == posLine pos + newlineCount

-- | Test that advancePosByText handles tabs correctly
prop_advance_pos_by_text_tabs :: SourcePos -> Int -> Property
prop_advance_pos_by_text_tabs pos count = 
  let tabs = replicate count '\t'
      newPos = advancePosByText pos tabs
      expectedCol = foldl (\col _ -> ((col - 1) `div` 8 + 1) * 8 + 1) (posColumn pos) tabs
  in property $ 
    posColumn newPos == expectedCol

-- | Test that toErrorLocation creates correct location
prop_to_error_location_correct :: SourcePos -> Property
prop_to_error_location_correct pos = 
  let errLoc = toErrorLocation pos
  in property $ 
    getErrorLine errLoc == posLine pos &&
    getErrorColumn errLoc == posColumn pos

-- | Test that locatedAt creates correct located value
prop_located_at_correct :: String -> SourcePos -> Property
prop_located_at_correct val pos = 
  let located = locatedAt val pos
  in property $ 
    locatedValue located == val &&
    locatedPos located == pos

-- | Test that isValidSpan correctly identifies valid spans
prop_is_valid_span_correct :: SourcePos -> SourcePos -> Property
prop_is_valid_span_correct pos1 pos2 = 
  let span = spanBetween pos1 pos2
      isValid = isValidSpan span
      shouldBeValid = pos1 <= pos2
  in property $ 
    isValid == shouldBeValid

-- | Test that withLocationTracking tracks positions correctly
prop_with_location_tracking_correct :: String -> Property
prop_with_location_tracking_correct s = 
  let (result, finalPos) = runLocationTracker $ do
        setCurrentPos startPos
        mapM_ (\c -> do
          current <- getCurrentPos
          setCurrentPos (posAfter c current)
        ) s
        getCurrentPos
  in property $ 
    posOffset finalPos == length s

tests :: TestTree
tests = testGroup "SourceLocation Math QuickCheck Tests"
  [ testProperty "startPos values" prop_start_pos_values
  , testProperty "posAfter newline" prop_pos_after_newline
  , testProperty "posAfter tab" prop_pos_after_tab
  , testProperty "posAfter regular" prop_pos_after_regular
  , testProperty "posAt correct" prop_pos_at_correct
  , testProperty "emptySpan properties" prop_empty_span_properties
  , testProperty "spanFrom correct" prop_span_from_correct
  , testProperty "mergeSpans correct" prop_merge_spans_correct
  , testProperty "advancePosBy correct" prop_advance_pos_by_correct
  , testProperty "advancePosByText newlines" prop_advance_pos_by_text_newlines
  , testProperty "advancePosByText tabs" prop_advance_pos_by_text_tabs
  , testProperty "toErrorLocation correct" prop_to_error_location_correct
  , testProperty "locatedAt correct" prop_located_at_correct
  , testProperty "isValidSpan correct" prop_is_valid_span_correct
  , testProperty "withLocationTracking correct" prop_with_location_tracking_correct
  ]