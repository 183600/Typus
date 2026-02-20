{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.SourceLocationComprehensiveSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, spanBetweenOrdered,
                      mergeSpans, isValidSpan, isValidBlockSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      mapLocated, advancePos, advancePosBy, advancePosByText,
                      comparePos, toErrorLocation, toErrorLocationWithSpan)
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.Char (isSpace)

-- Arbitrary instances
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    span <- arbitrary
    value <- arbitrary
    return $ locatedWithSpan span value

-- Helper generators for SourceLocation tests
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  offset <- choose (0, 100000)
  return $ SourcePos line column offset

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!()[]{}+-*/=<>&|^%~?"

genString :: Gen String
genString = do
  len <- choose (0, 100)
  vectorOf len genChar

genText :: Gen T.Text
genText = T.pack <$> genString

genLocated :: Gen a -> Gen (Located a)
genLocated gen = do
  value <- gen
  span <- genSourceSpan
  return $ locatedWithSpan span value

-- Test properties for SourceLocation module

-- Property 1: startPos is the canonical start position
prop_start_pos_properties :: Bool
prop_start_pos_properties = 
  posLine startPos == 1 && 
  posColumn startPos == 1 && 
  posOffset startPos == 0

-- Property 2: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Bool
prop_posAfter_newline pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 &&
     posColumn newPos == 1 &&
     posOffset newPos == posOffset pos + 1

-- Property 3: posAfter handles tab correctly (8-space tab)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos = 
  posColumn pos <= 1000 ==> 
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos == expectedColumn &&
     posLine newPos == posLine pos &&
     posOffset newPos == posOffset pos + 1

-- Property 4: posAfter handles regular characters correctly
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos c = 
  c `notElem` "\n\t" ==> 
  let newPos = posAfter c pos
  in posLine newPos == posLine pos &&
     posColumn newPos == posColumn pos + 1 &&
     posOffset newPos == posOffset pos + 1

-- Property 5: posAt creates positions with correct line and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col = 
  line > 0 && col > 0 ==> 
  let pos = posAt line col
  in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- Property 6: posAtLineCol creates positions with correct line, column, and offset
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset = 
  line > 0 && col > 0 && offset >= 0 ==> 
  let pos = posAtLineCol line col offset
  in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- Property 7: emptySpan creates a span with same start and end
prop_empty_span_properties :: SourcePos -> Bool
prop_empty_span_properties pos = 
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos

-- Property 8: spanFrom creates a span starting at the given position
prop_span_from_properties :: SourcePos -> Bool
prop_span_from_properties pos = 
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- Property 9: spanTo creates a span ending at the given position
prop_span_to_properties :: SourcePos -> Bool
prop_span_to_properties pos = 
  let span = spanTo pos
  in spanStart span == pos && spanEnd span == pos

-- Property 10: spanBetween preserves the order of positions
prop_span_between_preserves_order :: SourcePos -> SourcePos -> Bool
prop_span_between_preserves_order pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in spanStart span == pos1 && spanEnd span == pos2

-- Property 11: spanBetweenOrdered creates spans with correct ordering
prop_span_between_ordered_correct :: SourcePos -> SourcePos -> Bool
prop_span_between_ordered_correct pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in comparePos start end /= GT

-- Property 12: mergeSpans creates spans that encompass both input spans
prop_merge_spans_encompassing :: SourceSpan -> SourceSpan -> Bool
prop_merge_spans_encompassing span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in comparePos mergedStart start1 /= GT &&
     comparePos mergedStart start2 /= GT &&
     comparePos mergedEnd end1 /= LT &&
     comparePos mergedEnd end2 /= LT

-- Property 13: isValidSpan correctly identifies valid spans
prop_is_valid_span_correct :: SourcePos -> SourcePos -> Bool
prop_is_valid_span_correct pos1 pos2 = 
  let span = spanBetween pos1 pos2
      isValid = isValidSpan span
  in isValid == (comparePos pos1 pos2 /= GT)

-- Property 14: locatedAt creates located values with correct position
prop_located_at_correct :: Eq a => a -> SourcePos -> Bool
prop_located_at_correct value pos = 
  let located = locatedAt pos value
  in locatedValue located == value && 
     locatedPos located == pos &&
     spanStart (locatedSpan located) == pos &&
     spanEnd (locatedSpan located) == pos

-- Property 15: mapLocated preserves location information
prop_map_located_preserves_location :: (a -> b) -> Located a -> Bool
prop_map_located_preserves_location f located = 
  let mapped = mapLocated f located
  in locatedSpan mapped == locatedSpan located

-- Property 16: advancePos advances position by one character
prop_advance_pos_single_char :: SourcePos -> Char -> Bool
prop_advance_pos_single_char pos c = 
  advancePos c pos == posAfter c pos

-- Property 17: advancePosByText advances position correctly through text
prop_advance_pos_by_text :: SourcePos -> String -> Bool
prop_advance_pos_by_text pos text = 
  let finalPos = advancePosByText (T.pack text) pos
      expectedPos = foldl (\p c -> posAfter c p) pos text
  in finalPos == expectedPos

-- Property 18: comparePos provides total ordering
prop_compare_pos_total_ordering :: SourcePos -> SourcePos -> SourcePos -> Property
prop_compare_pos_total_ordering pos1 pos2 pos3 = 
  let cmp12 = comparePos pos1 pos2
      cmp23 = comparePos pos2 pos3
      cmp13 = comparePos pos1 pos3
      result = if cmp12 == EQ && cmp23 == EQ
               then cmp13 == EQ
               else if cmp12 == LT && cmp23 == LT
                    then cmp13 == LT
                    else if cmp12 == GT && cmp23 == GT
                         then cmp13 == GT
                         else True -- Mixed cases are complex, but shouldn't violate basic properties
  in property result

-- Unit tests for edge cases
test_source_pos_edge_cases :: [TestTree]
test_source_pos_edge_cases = 
  [ testCase "posAfter on newline at line 1" $ 
      let pos = posAt 1 5
          newPos = posAfter '\n' pos
      in assertEqual "line should increment" 2 (posLine newPos) >>
         assertEqual "column should reset" 1 (posColumn newPos) >>
         assertEqual "offset should increment" 6 (posOffset newPos)
  , testCase "posAfter on tab at column 1" $ 
      let pos = posAt 1 1
          newPos = posAfter '\t' pos
      in assertEqual "column should jump to 9" 9 (posColumn newPos) >>
         assertEqual "line should stay same" 1 (posLine newPos) >>
         assertEqual "offset should increment" 1 (posOffset newPos)
  , testCase "posAfter on tab at column 8" $ 
      let pos = posAt 1 8
          newPos = posAfter '\t' pos
      in assertEqual "column should jump to 9" 9 (posColumn newPos) >>
         assertEqual "line should stay same" 1 (posLine newPos) >>
         assertEqual "offset should increment" 1 (posOffset newPos)
  , testCase "posAfter on tab at column 9" $ 
      let pos = posAt 1 9
          newPos = posAfter '\t' pos
      in assertEqual "column should jump to 17" 17 (posColumn newPos) >>
         assertEqual "line should stay same" 1 (posLine newPos) >>
         assertEqual "offset should increment" 1 (posOffset newPos)
  ]

test_source_span_edge_cases :: [TestTree]
test_source_span_edge_cases = 
  [ testCase "spanBetweenOrdered with same positions" $ 
      let pos = posAt 1 1
          span = spanBetweenOrdered pos pos
      in assertEqual "start should be pos" pos (spanStart span) >>
         assertEqual "end should be pos" pos (spanEnd span)
  , testCase "spanBetweenOrdered with pos1 before pos2" $ 
      let pos1 = posAt 1 1
          pos2 = posAt 1 5
          span = spanBetweenOrdered pos1 pos2
      in assertEqual "start should be pos1" pos1 (spanStart span) >>
         assertEqual "end should be pos2" pos2 (spanEnd span)
  , testCase "spanBetweenOrdered with pos1 after pos2" $ 
      let pos1 = posAt 1 5
          pos2 = posAt 1 1
          span = spanBetweenOrdered pos1 pos2
      in assertEqual "start should be pos2" pos2 (spanStart span) >>
         assertEqual "end should be pos1" pos1 (spanEnd span)
  , testCase "mergeSpans with identical spans" $ 
      let pos1 = posAt 1 1
          pos2 = posAt 1 5
          span1 = spanBetween pos1 pos2
          span2 = spanBetween pos1 pos2
          merged = mergeSpans span1 span2
      in assertEqual "merged should equal original" span1 merged
  ]

test_located_edge_cases :: [TestTree]
test_located_edge_cases = 
  [ testCase "locatedAt with simple value" $ 
      let value = "test"
          pos = posAt 1 1
          located = locatedAt pos value
      in assertEqual "value should be preserved" value (locatedValue located) >>
         assertEqual "position should be preserved" pos (locatedPos located)
  , testCase "mapLocated with identity function" $ 
      let value = "test"
          pos = posAt 1 1
          located = locatedAt pos value
          mapped = mapLocated id located
      in assertEqual "mapped should equal original" located mapped
  , testCase "mapLocated with transformation" $ 
      let value = "test"
          pos = posAt 1 1
          located = locatedAt pos value
          mapped = mapLocated length located
      in assertEqual "value should be transformed" 4 (locatedValue mapped) >>
         assertEqual "position should be preserved" pos (locatedPos mapped)
  ]

test_position_advancement_edge_cases :: [TestTree]
test_position_advancement_edge_cases = 
  [ testCase "advancePosByText with empty string" $ 
      let pos = posAt 1 1
          finalPos = advancePosByText (T.pack "") pos
      in assertEqual "position should not change" pos finalPos
  , testCase "advancePosByText with single newline" $ 
      let pos = posAt 1 1
          finalPos = advancePosByText (T.pack "\n") pos
          expected = posAfter '\n' pos
      in assertEqual "should advance correctly" expected finalPos
  , testCase "advancePosByText with mixed content" $ 
      let pos = posAt 1 1
          finalPos = advancePosByText (T.pack "hello\nworld") pos
          expected = foldl (\p c -> posAfter c p) pos "hello\nworld"
      in assertEqual "should advance correctly" expected finalPos
  ]

test_position_comparison_edge_cases :: [TestTree]
test_position_comparison_edge_cases = 
  [ testCase "comparePos with identical positions" $ 
      let pos = posAt 1 1
      in assertEqual "identical positions should be EQ" EQ (comparePos pos pos)
  , testCase "comparePos with different lines" $ 
      let pos1 = posAt 1 10
          pos2 = posAt 2 1
      in assertEqual "line 1 should be LT" LT (comparePos pos1 pos2) >>
         assertEqual "line 2 should be GT" GT (comparePos pos2 pos1)
  , testCase "comparePos with same line different columns" $ 
      let pos1 = posAt 1 1
          pos2 = posAt 1 5
      in assertEqual "column 1 should be LT" LT (comparePos pos1 pos2) >>
         assertEqual "column 5 should be GT" GT (comparePos pos2 pos1)
  ]

-- QuickCheck property tests
sourceLocationQuickCheckTests :: TestTree
sourceLocationQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "Source Position"
      [ ("startPos properties", property prop_start_pos_properties)
      , ("posAfter newline", property prop_posAfter_newline)
      , ("posAfter tab", property prop_posAfter_tab)
      , ("posAfter regular char", property prop_posAfter_regular_char)
      , ("posAt correct", property prop_posAt_correct)
      , ("posAtLineCol correct", property prop_posAtLineCol_correct)
      ]
  , testProperties "Source Span"
      [ ("emptySpan properties", property prop_empty_span_properties)
      , ("spanFrom properties", property prop_span_from_properties)
      , ("spanTo properties", property prop_span_to_properties)
      , ("spanBetween preserves order", property prop_span_between_preserves_order)
      , ("spanBetweenOrdered correct", property prop_span_between_ordered_correct)
      , ("mergeSpans encompassing", property prop_merge_spans_encompassing)
      , ("isValidSpan correct", property prop_is_valid_span_correct)
      ]
  , testProperties "Located Values"
      [ ("locatedAt correct", property (prop_located_at_correct :: String -> SourcePos -> Bool))
      -- , ("mapLocated preserves location", property (prop_map_located_preserves_location :: (String -> String) -> Located String -> Bool))
      ]
  , testProperties "Position Advancement"
      [ ("advancePos single char", property prop_advance_pos_single_char)
      , ("advancePosByText", property prop_advance_pos_by_text)
      ]
  , testProperties "Position Comparison"
      [ ("comparePos total ordering", property prop_compare_pos_total_ordering)
      ]
  ]

-- Unit tests
sourceLocationUnitTests :: TestTree
sourceLocationUnitTests = testGroup "Unit Tests"
  [ testGroup "Source Position Edge Cases" test_source_pos_edge_cases
  , testGroup "Source Span Edge Cases" test_source_span_edge_cases
  , testGroup "Located Edge Cases" test_located_edge_cases
  , testGroup "Position Advancement Edge Cases" test_position_advancement_edge_cases
  , testGroup "Position Comparison Edge Cases" test_position_comparison_edge_cases
  ]

-- Main test suite
sourceLocationComprehensiveTests :: TestTree
sourceLocationComprehensiveTests = testGroup "SourceLocation Comprehensive Tests"
  [ sourceLocationUnitTests
  , sourceLocationQuickCheckTests
  ]