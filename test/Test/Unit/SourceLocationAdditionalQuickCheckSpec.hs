{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.SourceLocationAdditionalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, vectorOf, suchThat, choose, resize, forAll, (==>))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import SourceLocation (
    SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
    startPos, posAfter, posAt, posAtLineCol,
    emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
    locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
    advancePos, advancePosBy, advancePosByText, advancePosByLine,
    toErrorLocation, toErrorLocationWithSpan
  )
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = SourcePos 
          { posLine = posLine start + (endOffset `div` 100)
          , posColumn = posColumn start + (endOffset `mod` 100)
          , posOffset = posOffset start + endOffset
          }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Helper Generators
-- ============================================================================

-- Generate valid positions where start <= end
genValidSpan :: Gen SourceSpan
genValidSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  startOffset <- choose (0, 10000)
  let start = SourcePos startLine startCol startOffset
  
  endOffset <- choose (0, 1000)
  let end = SourcePos 
        { posLine = startLine + (endOffset `div` 100)
        , posColumn = if endOffset `mod` 100 == 0 then startCol else startCol + (endOffset `mod` 100)
        , posOffset = startOffset + endOffset
        }
  
  return $ SourceSpan start end

-- Generate characters for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?"

-- Generate strings for position advancement
genString :: Gen String
genString = listOf genChar

-- Generate text for position advancement
genText :: Gen Text
genText = T.pack <$> genString

-- ============================================================================
-- QuickCheck Tests for SourcePos
-- ============================================================================

-- Test SourcePos properties
prop_source_pos_valid :: SourcePos -> Bool
prop_source_pos_valid pos = 
  posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

prop_pos_after_newline :: SourcePos -> Bool
prop_pos_after_newline pos = 
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 &&
     posColumn newPos == 1 &&
     posOffset newPos == posOffset pos + 1

prop_pos_after_tab :: SourcePos -> Bool
prop_pos_after_tab pos = 
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine pos &&
     posColumn newPos == expectedCol &&
     posOffset newPos == posOffset pos + 1

prop_pos_after_regular_char :: SourcePos -> Property
prop_pos_after_regular_char pos = 
  forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,.;:!?") $ \c ->
    let newPos = posAfter c pos
    in posLine newPos == posLine pos &&
       posColumn newPos == posColumn pos + 1 &&
       posOffset newPos == posOffset pos + 1

prop_pos_at_creates_valid_position :: Int -> Int -> Bool
prop_pos_at_creates_valid_position line col =
  line > 0 && col > 0 ==> 
  let pos = posAt line col
  in posLine pos == line && posColumn pos == col && posOffset pos == 0

prop_pos_at_line_col_creates_valid_position :: Int -> Int -> Int -> Bool
prop_pos_at_line_col_creates_valid_position line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- ============================================================================
-- QuickCheck Tests for SourceSpan
-- ============================================================================

-- Test SourceSpan properties
prop_empty_span_valid :: SourcePos -> Bool
prop_empty_span_valid pos = 
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_span_from_valid :: SourcePos -> Bool
prop_span_from_valid pos = 
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_span_to_valid :: SourcePos -> Bool
prop_span_to_valid pos = 
  let span = spanTo pos
  in spanStart span == pos && spanEnd span == pos && isValidSpan span

prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid start end = 
  start <= end ==> 
  let span = spanBetween start end
  in spanStart span == start && spanEnd span == end && isValidSpan span

prop_merge_spans_valid :: SourceSpan -> SourceSpan -> Bool
prop_merge_spans_valid span1 span2 = 
  let merged = mergeSpans span1 span2
      expectedStart = min (spanStart span1) (spanStart span2)
      expectedEnd = max (spanEnd span1) (spanEnd span2)
  in spanStart merged == expectedStart && 
     spanEnd merged == expectedEnd && 
     isValidSpan merged

prop_is_valid_span_check :: SourceSpan -> Bool
prop_is_valid_span_check span = 
  isValidSpan span == (spanStart span <= spanEnd span)

-- ============================================================================
-- QuickCheck Tests for Located Values
-- ============================================================================

-- Test Located properties
prop_located_at_creates_valid_location :: SourcePos -> Int -> Bool
prop_located_at_creates_valid_location pos value = 
  let located = locatedAt pos value
  in locatedValue located == value &&
     locatedPos located == pos &&
     locatedSpan located == emptySpan pos

prop_located_with_span_creates_valid_location :: SourceSpan -> String -> Bool
prop_located_with_span_creates_valid_location span value = 
  let located = locatedWithSpan span value
  in locatedValue located == value &&
     locatedSpan located == span &&
     locatedPos located == spanStart span

prop_map_located_preserves_location :: SourceSpan -> String -> Bool
prop_map_located_preserves_location span value = 
  let located = locatedWithSpan span value
      mapped = mapLocated length located
  in locatedValue mapped == length value &&
     locatedPos mapped == locatedPos located &&
     locatedSpan mapped == locatedSpan located

-- ============================================================================
-- QuickCheck Tests for Position Advancement
-- ============================================================================

-- Test position advancement properties
prop_advance_pos_matches_pos_after :: SourcePos -> Char -> Bool
prop_advance_pos_matches_pos_after pos c = 
  advancePos c pos == posAfter c pos

prop_advance_pos_by_empty_string :: SourcePos -> Bool
prop_advance_pos_by_empty_string pos = 
  advancePosBy "" pos == pos

prop_advance_pos_by_single_char :: SourcePos -> Char -> Bool
prop_advance_pos_by_single_char pos c = 
  advancePosBy [c] pos == posAfter c pos

prop_advance_pos_by_multiple_chars :: SourcePos -> Property
prop_advance_pos_by_multiple_chars pos = 
  forAll (listOf1 genChar) $ \chars ->
    let result = advancePosBy chars pos
        expected = foldl (flip posAfter) pos chars
    in result == expected

prop_advance_pos_by_text_matches_string :: SourcePos -> Property
prop_advance_pos_by_text_matches_string pos = 
  forAll genText $ \text ->
    advancePosByText text pos == advancePosBy (T.unpack text) pos

prop_advance_pos_by_line :: SourcePos -> Int -> Property
prop_advance_pos_by_line pos numLines = 
  numLines >= 0 ==>
  let result = advancePosByLine numLines pos
  in posLine result == posLine pos + numLines &&
     posColumn result == 1

-- ============================================================================
-- QuickCheck Tests for Error Location Conversion
-- ============================================================================

-- Test error location conversion properties
prop_to_error_location_from_pos :: SourcePos -> Bool
prop_to_error_location_from_pos pos = 
  let errLoc = toErrorLocation pos
  in filePath errLoc == Nothing &&
     line errLoc == posLine pos &&
     column errLoc == posColumn pos &&
     endLine errLoc == Nothing &&
     endColumn errLoc == Nothing

prop_to_error_location_with_span :: SourceSpan -> Bool
prop_to_error_location_with_span span = 
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in filePath errLoc == Nothing &&
     line errLoc == posLine start &&
     column errLoc == posColumn start &&
     endLine errLoc == Just (posLine end) &&
     endColumn errLoc == Just (posColumn end)

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

test_start_pos_properties :: TestTree
test_start_pos_properties = testCase "startPos properties" $
  assertBool "startPos should be valid" $
    posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

test_pos_after_special_chars :: TestTree
test_pos_after_special_chars = testCase "posAfter special characters" $ do
  let pos = startPos
  assertEqual "newline" (SourcePos 2 1 1) (posAfter '\n' pos)
  assertEqual "tab from column 1" (SourcePos 1 9 1) (posAfter '\t' pos)
  assertEqual "regular char" (SourcePos 1 2 1) (posAfter 'a' pos)

test_span_operations :: TestTree
test_span_operations = testCase "span operations" $ do
  let pos1 = posAt 1 5
  let pos2 = posAt 2 10
  let span1 = emptySpan pos1
  let span2 = spanBetween pos1 pos2
  let merged = mergeSpans span1 span2
  
  assertEqual "emptySpan start" pos1 (spanStart span1)
  assertEqual "emptySpan end" pos1 (spanEnd span1)
  assertEqual "spanBetween start" pos1 (spanStart span2)
  assertEqual "spanBetween end" pos2 (spanEnd span2)
  assertEqual "merged start" pos1 (spanStart merged)
  assertEqual "merged end" pos2 (spanEnd merged)

test_located_value_operations :: TestTree
test_located_value_operations = testCase "located value operations" $ do
  let pos = posAt 3 7
  let value = "test"
  let located = locatedAt pos value
  let mapped = mapLocated reverse located
  
  assertEqual "located value" value (locatedValue located)
  assertEqual "located position" pos (locatedPos located)
  assertEqual "mapped value" (reverse value) (locatedValue mapped)
  assertEqual "mapped position" pos (locatedPos mapped)

test_advance_position_operations :: TestTree
test_advance_position_operations = testCase "advance position operations" $ do
  let pos = posAt 1 1
  let advanced1 = advancePosBy "hello" pos
  let advanced2 = advancePosByLine 2 pos
  
  assertEqual "advance by string" (SourcePos 1 6 5) advanced1
  assertEqual "advance by line" (SourcePos 3 1) advanced2

test_error_location_conversion :: TestTree
test_error_location_conversion = testCase "error location conversion" $ do
  let pos = posAt 10 20
  let span = spanBetween pos (posAt 11 5)
  let errLoc1 = toErrorLocation pos
  let errLoc2 = toErrorLocationWithSpan span
  
  assertEqual "pos error location line" 10 (line errLoc1)
  assertEqual "pos error location column" 20 (column errLoc1)
  assertEqual "span error location line" 10 (line errLoc2)
  assertEqual "span error location column" 20 (column errLoc2)
  assertEqual "span error location end line" (Just 11) (endLine errLoc2)
  assertEqual "span error location end column" (Just 5) (endColumn errLoc2)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Additional QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ testProperty "source pos valid" prop_source_pos_valid
    , testProperty "posAfter newline" prop_pos_after_newline
    , testProperty "posAfter tab" prop_pos_after_tab
    , testProperty "posAfter regular char" prop_pos_after_regular_char
    , testProperty "posAt creates valid position" prop_pos_at_creates_valid_position
    , testProperty "posAtLineCol creates valid position" prop_pos_at_line_col_creates_valid_position
    ]
  , testGroup "SourceSpan Properties"
    [ testProperty "empty span valid" prop_empty_span_valid
    , testProperty "span from valid" prop_span_from_valid
    , testProperty "span to valid" prop_span_to_valid
    , testProperty "span between valid" prop_span_between_valid
    , testProperty "merge spans valid" prop_merge_spans_valid
    , testProperty "is valid span check" prop_is_valid_span_check
    ]
  , testGroup "Located Value Properties"
    [ testProperty "locatedAt creates valid location" prop_located_at_creates_valid_location
    , testProperty "locatedWithSpan creates valid location" prop_located_with_span_creates_valid_location
    , testProperty "mapLocated preserves location" prop_map_located_preserves_location
    ]
  , testGroup "Position Advancement Properties"
    [ testProperty "advancePos matches posAfter" prop_advance_pos_matches_pos_after
    , testProperty "advancePosBy empty string" prop_advance_pos_by_empty_string
    , testProperty "advancePosBy single char" prop_advance_pos_by_single_char
    , testProperty "advancePosBy multiple chars" prop_advance_pos_by_multiple_chars
    , testProperty "advancePosByText matches string" prop_advance_pos_by_text_matches_string
    , testProperty "advancePosByLine" prop_advance_pos_by_line
    ]
  , testGroup "Error Location Conversion Properties"
    [ testProperty "toErrorLocation from pos" prop_to_error_location_from_pos
    , testProperty "toErrorLocationWithSpan" prop_to_error_location_with_span
    ]
  , testGroup "Unit Tests"
    [ test_start_pos_properties
    , test_pos_after_special_chars
    , test_span_operations
    , test_located_value_operations
    , test_advance_position_operations
    , test_error_location_conversion
    ]
  ]