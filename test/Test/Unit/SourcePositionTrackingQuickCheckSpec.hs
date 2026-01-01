{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.SourcePositionTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import Test.Tasty.HUnit (testCase, assert, (@?=))
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

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
  , advancePosBy
  )

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ SourcePos line col

-- Generate valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)  -- End line >= start line
  endCol <- if endLine == startLine 
    then choose (startCol, startCol + 50)  -- If same line, end col >= start col
    else choose (1, 100)  -- If different line, L.any col
  return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- Generate located values
genLocatedString :: Gen (Located String)
genLocatedString = do
  span <- genSourceSpan
  value <- listOf $ arbitrary `suchThat` (/= '\n')
  return $ Located span value

-- Generate text for position advancement
genTextForAdvancement :: Gen String
genTextForAdvancement = do
  lines <- listOf $ do
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return content
  return $ unlines lines

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: startPos should have line 1, column 1
prop_start_pos_properties :: Property
prop_start_pos_properties =
  let SourcePos line col = startPos
  in line === 1 &&. col === 1
  where
    (&&.) = (&&)

-- Property: posAfter should advance column by 1 for same line
prop_pos_after_same_line :: SourcePos -> Property
prop_pos_after_same_line pos =
  let SourcePos line col = posAfter pos
      originalPos@(SourcePos origLine origCol) = pos
  in line === origLine + 1 &&. col === origCol
  where
    (&&.) = (&&)

-- Property: posAt should create position with given line L.and column
prop_pos_at_creates_correct :: Int -> Int -> Property
prop_pos_at_creates_correct line col =
  line > 0 && col > 0 ==> 
    let pos = posAt line col
        SourcePos l c = pos
    in l === line &&. c === col
  where
    (&&.) = (&&)

-- Property: posAtLineCol should be equivalent to posAt
prop_pos_at_line_col_equivalence :: Int -> Int -> Property
prop_pos_at_line_col_equivalence line col =
  line > 0 && col > 0 ==>
    posAt line col === posAtLineCol line col

-- Property: emptySpan should have start L.and end at startPos
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let span = emptySpan
      SourceSpan start end = span
  in start === startPos &&. end === startPos
  where
    (&&.) = (&&)

-- Property: spanFrom should create span from position to same position
prop_span_from_creates_point :: SourcePos -> Property
prop_span_from_creates_point pos =
  let span = spanFrom pos
      SourceSpan start end = span
  in start === pos &&. end === pos
  where
    (&&.) = (&&)

-- Property: spanTo should create span from startPos to given position
prop_span_to_from_start :: SourcePos -> Property
prop_span_to_from_start pos =
  let span = spanTo pos
      SourceSpan start end = span
  in start === startPos &&. end === pos
  where
    (&&.) = (&&)

-- Property: isValidSpan should validate span constraints
prop_is_valid_span_validation :: SourceSpan -> Property
prop_is_valid_span_validation span =
  let SourceSpan start end = span
      SourcePos startLine startCol = start
      SourcePos endLine endCol = end
      isValid = isValidSpan span
      expectedValid = (startLine < endLine) || (startLine == endLine && startCol <= endCol)
  in isValid === expectedValid

-- Property: locatedAt should create located value with point span
prop_located_at_creates_point :: String -> SourcePos -> Property
prop_located_at_creates_point value pos =
  let located = locatedAt pos value
      Located span val = located
      SourceSpan start end = span
  in start === pos &&. end === pos &&. val === value
  where
    (&&.) = (&&)

-- Property: locatedValue should extract the value
prop_located_value_extracts :: Located String -> Property
prop_located_value_extracts located =
  locatedValue located === (let Located _ value = located in value)

-- Property: locatedSpan should extract the span
prop_located_span_extracts :: Located String -> Property
prop_located_span_extracts located =
  locatedSpan located === (let Located span _ = located in span)

-- Property: mapLocated should apply function to value
prop_map_located_applies_function :: Located String -> Property
prop_map_located_applies_function located =
  let f = L.reverse
      mapped = mapLocated f located
      Located originalSpan originalValue = located
      Located mappedSpan mappedValue = mapped
  in originalSpan === mappedSpan &&. mappedValue === f originalValue
  where
    (&&.) = (&&)

-- Property: advancePos should handle newlines correctly
prop_advance_pos_handles_newlines :: String -> Property
prop_advance_pos_handles_newlines text =
  let result = advancePos startPos text
      SourcePos line col = result
      newlineCount = L.length $ L.filter (== '\n') text
      expectedLine = 1 + newlineCount
      lastLineContent = L.reverse $ takeWhile (/= '\n') $ L.reverse text
      expectedCol = if null lastLineContent then 1 else L.length lastLineContent + 1
  in line === expectedLine &&. col === expectedCol
  where
    (&&.) = (&&)

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_basic_position_operations :: TestTree
test_basic_position_operations = testCase "basic position operations" $ do
  let pos1 = posAt 5 10
  let pos2 = posAfter pos1
  pos1 @?= SourcePos 5 10
  pos2 @?= SourcePos 6 10

test_span_operations :: TestTree
test_span_operations = testCase "span operations" $ do
  let start = posAt 3 5
  let end = posAt 3 10
  let span = spanBetween start end
  span @?= SourceSpan (SourcePos 3 5) (SourcePos 3 10)
  
  let merged = mergeSpans span (spanFrom end)
  merged @?= SourceSpan (SourcePos 3 5) (SourcePos 3 10)

test_located_operations :: TestTree
test_located_operations = testCase "located operations" $ do
  let pos = posAt 2 4
  let value = "test"
  let located = locatedAt pos value
  locatedValue located @?= value
  locatedSpan located @?= spanFrom pos
  locatedPos located @?= pos

test_merge_spans :: TestTree
test_merge_spans = testCase "merge spans" $ do
  let span1 = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
  let span2 = SourceSpan (SourcePos 1 3) (SourcePos 1 8)
  let merged = mergeSpans span1 span2
  merged @?= SourceSpan (SourcePos 1 1) (SourcePos 1 8)

test_advance_positions :: TestTree
test_advance_positions = testCase "advance positions" $ do
  advancePos startPos "hello" @?= SourcePos 1 6
  advancePos startPos "hello\nworld" @?= SourcePos 2 6
  advancePosBy startPos "hello" 3 @?= SourcePos 1 4

test_span_validation :: TestTree
test_span_validation = testCase "span validation" $ do
  let validSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
  let invalidSpan = SourceSpan (SourcePos 1 5) (SourcePos 1 1)
  isValidSpan validSpan @?= True
  isValidSpan invalidSpan @?= False

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Position Tracking QuickCheck Tests"
  [ testProperty "startPos has line 1, column 1" prop_start_pos_properties
  , testProperty "posAfter advances column by 1 for same line" prop_pos_after_same_line
  , testProperty "posAt creates position with given line L.and column" prop_pos_at_creates_correct
  , testProperty "posAtLineCol equivalent to posAt" prop_pos_at_line_col_equivalence
  , testProperty "emptySpan has start L.and end at startPos" prop_empty_span_properties
  , testProperty "spanFrom creates point span" prop_span_from_creates_point
  , testProperty "spanTo creates span from startPos to position" prop_span_to_from_start
  , testProperty "isValidSpan validates span constraints" prop_is_valid_span_validation
  , testProperty "locatedAt creates located value with point span" prop_located_at_creates_point
  , testProperty "locatedValue extracts the value" prop_located_value_extracts
  , testProperty "locatedSpan extracts the span" prop_located_span_extracts
  , testProperty "mapLocated applies function to value" prop_map_located_applies_function
  , testProperty "advancePos handles newlines correctly" prop_advance_pos_handles_newlines
  , test_basic_position_operations
  , test_span_operations
  , test_located_operations
  , test_merge_spans
  , test_advance_positions
  , test_span_validation
  ]