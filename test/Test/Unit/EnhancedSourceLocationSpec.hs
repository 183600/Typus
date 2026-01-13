module Test.Unit.EnhancedSourceLocationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import SourceLocation module
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..), 
                      locatedWithSpan, spanStart, spanEnd, 
                      posLine, posColumn, posOffset,
                      emptySpan, spanFrom, locatedAt, locatedValue, 
                      locatedSpan, locatedPos)

-- Test properties for source location

-- Property 1: Creating source position should not crash
prop_create_source_pos :: Int -> Int -> Property
prop_create_source_pos line col = 
  line >= 0 && col >= 0 ==>
  let pos = SourcePos { posLine = line, posColumn = col, posOffset = 0 }
  in property $ posLine pos == line && 
               posColumn pos == col

-- Property 2: Creating source span should not crash
prop_create_source_span :: Int -> Int -> Int -> Int -> Property
prop_create_source_span startLine startCol endLine endCol = 
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
  in property $ spanStart span == start && 
               spanEnd span == end

-- Property 3: Creating located value should not crash
prop_create_located :: String -> Int -> Int -> Int -> Int -> Property
prop_create_located value startLine startCol endLine endCol = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located = Located { locValue = value, locPos = start, locSpan = span }
  in property $ locatedValue located == value && 
               locatedSpan located == span

-- Property 4: Located values should be comparable
prop_located_equality :: String -> Int -> Int -> Int -> Int -> Property
prop_located_equality value startLine startCol endLine endCol = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located1 = Located { locValue = value, locPos = start, locSpan = span }
      located2 = Located { locValue = value, locPos = start, locSpan = span }
  in property $ located1 == located2

-- Property 5: Source positions with different values should be different
prop_source_pos_inequality :: Int -> Int -> Int -> Int -> Property
prop_source_pos_inequality line1 col1 line2 col2 = 
  line1 >= 0 && col1 >= 0 &&
  line2 >= 0 && col2 >= 0 &&
  (line1 /= line2 || col1 /= col2) ==>
  let pos1 = SourcePos { posLine = line1, posColumn = col1, posOffset = 0 }
      pos2 = SourcePos { posLine = line2, posColumn = col2, posOffset = 0 }
  in property $ pos1 /= pos2

-- Property 6: Source spans with different values should be different
prop_source_span_inequality :: Int -> Int -> Int -> Int -> 
                             Int -> Int -> Int -> Int -> Property
prop_source_span_inequality sl1 sc1 el1 ec1 sl2 sc2 el2 ec2 = 
  sl1 >= 0 && sc1 >= 0 && el1 >= 0 && ec1 >= 0 &&
  sl2 >= 0 && sc2 >= 0 && el2 >= 0 && ec2 >= 0 &&
  (sl1 /= sl2 || sc1 /= sc2 || 
   el1 /= el2 || ec1 /= ec2) ==>
  let start1 = SourcePos { posLine = sl1, posColumn = sc1, posOffset = 0 }
      end1 = SourcePos { posLine = el1, posColumn = ec1, posOffset = 0 }
      span1 = SourceSpan { spanStart = start1, spanEnd = end1 }
      start2 = SourcePos { posLine = sl2, posColumn = sc2, posOffset = 0 }
      end2 = SourcePos { posLine = el2, posColumn = ec2, posOffset = 0 }
      span2 = SourceSpan { spanStart = start2, spanEnd = end2 }
  in property $ span1 /= span2

-- Property 7: Located values with different spans should be different
prop_located_span_inequality :: String -> Int -> Int -> Int -> Int -> 
                               Int -> Int -> Int -> Int -> Property
prop_located_span_inequality value sl1 sc1 el1 ec1 sl2 sc2 el2 ec2 = 
  not (null value) && sl1 >= 0 && sc1 >= 0 && el1 >= 0 && ec1 >= 0 &&
  sl2 >= 0 && sc2 >= 0 && el2 >= 0 && ec2 >= 0 &&
  (sl1 /= sl2 || sc1 /= sc2 || el1 /= el2 || ec1 /= ec2) ==>
  let start1 = SourcePos { posLine = sl1, posColumn = sc1, posOffset = 0 }
      end1 = SourcePos { posLine = el1, posColumn = ec1, posOffset = 0 }
      span1 = SourceSpan { spanStart = start1, spanEnd = end1 }
      start2 = SourcePos { posLine = sl2, posColumn = sc2, posOffset = 0 }
      end2 = SourcePos { posLine = el2, posColumn = ec2, posOffset = 0 }
      span2 = SourceSpan { spanStart = start2, spanEnd = end2 }
      located1 = Located { locValue = value, locPos = start1, locSpan = span1 }
      located2 = Located { locValue = value, locPos = start2, locSpan = span2 }
  in property $ located1 /= located2

-- Property 8: Located values with different values should be different
prop_located_value_inequality :: String -> String -> Int -> Int -> Int -> Int -> Property
prop_located_value_inequality value1 value2 startLine startCol endLine endCol = 
  not (null value1) && not (null value2) && value1 /= value2 &&
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located1 = Located { locValue = value1, locPos = start, locSpan = span }
      located2 = Located { locValue = value2, locPos = start, locSpan = span }
  in property $ located1 /= located2

-- Property 9: Span start and end should be accessible
prop_span_accessors :: Int -> Int -> Int -> Int -> Property
prop_span_accessors startLine startCol endLine endCol = 
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
  in property $ spanStart span == start && spanEnd span == end

-- Property 10: Located with span should work correctly
prop_located_with_span :: String -> Int -> Int -> Int -> Int -> Property
prop_located_with_span value startLine startCol endLine endCol = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 ==>
  let start = SourcePos { posLine = startLine, posColumn = startCol, posOffset = 0 }
      end = SourcePos { posLine = endLine, posColumn = endCol, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located = locatedWithSpan span value
  in property $ locatedValue located == value && 
               locatedSpan located == span

-- Unit tests for specific source location functionality

test_create_source_pos :: Assertion
test_create_source_pos = 
  let pos = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
  in do
    assertEqual "sourcePosLine" 1 (posLine pos)
    assertEqual "sourcePosColumn" 2 (posColumn pos)

test_create_source_span :: Assertion
test_create_source_span = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
  in do
    assertEqual "spanStart" start (spanStart span)
    assertEqual "spanEnd" end (spanEnd span)

test_create_located :: Assertion
test_create_located = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located = Located { locValue = "test value", locPos = start, locSpan = span }
  in do
    assertEqual "locValue" "test value" (locValue located)
    assertEqual "locSpan" span (locSpan located)

test_located_equality :: Assertion
test_located_equality = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located1 = Located { locValue = "test value", locPos = start, locSpan = span }
      located2 = Located { locValue = "test value", locPos = start, locSpan = span }
  in assertEqual "located values should be equal" located1 located2

test_source_pos_inequality :: Assertion
test_source_pos_inequality = 
  let pos1 = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      pos2 = SourcePos { posLine = 1, posColumn = 3, posOffset = 0 }
  in assertBool "source positions should be different" $ pos1 /= pos2

test_source_span_inequality :: Assertion
test_source_span_inequality = 
  let start1 = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end1 = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span1 = SourceSpan { spanStart = start1, spanEnd = end1 }
      start2 = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end2 = SourcePos { posLine = 3, posColumn = 5, posOffset = 0 }
      span2 = SourceSpan { spanStart = start2, spanEnd = end2 }
  in assertBool "source spans should be different" $ span1 /= span2

test_located_span_inequality :: Assertion
test_located_span_inequality = 
  let start1 = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end1 = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span1 = SourceSpan { spanStart = start1, spanEnd = end1 }
      start2 = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end2 = SourcePos { posLine = 3, posColumn = 5, posOffset = 0 }
      span2 = SourceSpan { spanStart = start2, spanEnd = end2 }
      located1 = Located { locValue = "test value", locPos = start1, locSpan = span1 }
      located2 = Located { locValue = "test value", locPos = start2, locSpan = span2 }
  in assertBool "located values with different spans should be different" $ located1 /= located2

test_located_value_inequality :: Assertion
test_located_value_inequality = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located1 = Located { locValue = "value1", locPos = start, locSpan = span }
      located2 = Located { locValue = "value2", locPos = start, locSpan = span }
  in assertBool "located values with different values should be different" $ located1 /= located2

test_span_accessors :: Assertion
test_span_accessors = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
  in do
    assertEqual "spanStart" start (spanStart span)
    assertEqual "spanEnd" end (spanEnd span)

test_located_with_span :: Assertion
test_located_with_span = 
  let start = SourcePos { posLine = 1, posColumn = 2, posOffset = 0 }
      end = SourcePos { posLine = 3, posColumn = 4, posOffset = 0 }
      span = SourceSpan { spanStart = start, spanEnd = end }
      located = locatedWithSpan span "test value"
  in do
    assertEqual "locatedValue" "test value" (locValue located)
    assertEqual "locatedSpan" span (locSpan located)

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedSourceLocationSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "create source pos" prop_create_source_pos
    , testProperty "create source span" prop_create_source_span
    , testProperty "create located" prop_create_located
    , testProperty "located equality" prop_located_equality
    , testProperty "source pos inequality" prop_source_pos_inequality
    , testProperty "source span inequality" prop_source_span_inequality
    , testProperty "located span inequality" prop_located_span_inequality
    , testProperty "located value inequality" prop_located_value_inequality
    , testProperty "span accessors" prop_span_accessors
    , testProperty "located with span" prop_located_with_span
    ]
  , testGroup "Unit Tests"
    [ testCase "create source pos" test_create_source_pos
    , testCase "create source span" test_create_source_span
    , testCase "create located" test_create_located
    , testCase "located equality" test_located_equality
    , testCase "source pos inequality" test_source_pos_inequality
    , testCase "source span inequality" test_source_span_inequality
    , testCase "located span inequality" test_located_span_inequality
    , testCase "located value inequality" test_located_value_inequality
    , testCase "span accessors" test_span_accessors
    , testCase "located with span" test_located_with_span
    ]
  ]