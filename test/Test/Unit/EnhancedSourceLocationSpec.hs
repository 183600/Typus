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
                      sourcePosLine, sourcePosColumn, sourcePosFile,
                      sourceSpanStart, sourceSpanEnd, sourceSpanFile,
                      mkSourcePos, mkSourceSpan, mkLocated)

-- Test properties for source location

-- Property 1: Creating source position should not crash
prop_create_source_pos :: Int -> Int -> String -> Property
prop_create_source_pos line col file = 
  line >= 0 && col >= 0 && not (null file) ==>
  let pos = mkSourcePos line col file
  in property $ sourcePosLine pos == line && 
               sourcePosColumn pos == col && 
               sourcePosFile pos == file

-- Property 2: Creating source span should not crash
prop_create_source_span :: Int -> Int -> Int -> Int -> String -> Property
prop_create_source_span startLine startCol endLine endCol file = 
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 && 
  not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
  in property $ sourceSpanStart span == start && 
               sourceSpanEnd span == end && 
               sourceSpanFile span == file

-- Property 3: Creating located value should not crash
prop_create_located :: String -> Int -> Int -> Int -> Int -> String -> Property
prop_create_located value startLine startCol endLine endCol file = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 && not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
      located = mkLocated value span
  in property $ locatedValue located == value && 
               locatedSpan located == span

-- Property 4: Located values should be comparable
prop_located_equality :: String -> Int -> Int -> Int -> Int -> String -> Property
prop_located_equality value startLine startCol endLine endCol file = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 && not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
      located1 = mkLocated value span
      located2 = mkLocated value span
  in property $ located1 == located2

-- Property 5: Source positions with different values should be different
prop_source_pos_inequality :: Int -> Int -> String -> Int -> Int -> String -> Property
prop_source_pos_inequality line1 col1 file1 line2 col2 file2 = 
  line1 >= 0 && col1 >= 0 && not (null file1) &&
  line2 >= 0 && col2 >= 0 && not (null file2) &&
  (line1 /= line2 || col1 /= col2 || file1 /= file2) ==>
  let pos1 = mkSourcePos line1 col1 file1
      pos2 = mkSourcePos line2 col2 file2
  in property $ pos1 /= pos2

-- Property 6: Source spans with different values should be different
prop_source_span_inequality :: Int -> Int -> Int -> Int -> String -> 
                             Int -> Int -> Int -> Int -> String -> Property
prop_source_span_inequality startLine1 startCol1 endLine1 endCol1 file1 
                             startLine2 startCol2 endLine2 endCol2 file2 = 
  startLine1 >= 0 && startCol1 >= 0 && endLine1 >= 0 && endCol1 >= 0 && 
  not (null file1) &&
  startLine2 >= 0 && startCol2 >= 0 && endLine2 >= 0 && endCol2 >= 0 && 
  not (null file2) &&
  (startLine1 /= startLine2 || startCol1 /= startCol2 || 
   endLine1 /= endLine2 || endCol1 /= endCol2 || file1 /= file2) ==>
  let start1 = mkSourcePos startLine1 startCol1 file1
      end1 = mkSourcePos endLine1 endCol1 file1
      span1 = mkSourceSpan start1 end1
      start2 = mkSourcePos startLine2 startCol2 file2
      end2 = mkSourcePos endLine2 endCol2 file2
      span2 = mkSourceSpan start2 end2
  in property $ span1 /= span2

-- Property 7: Located values with different spans should be different
prop_located_span_inequality :: String -> Int -> Int -> Int -> Int -> String -> 
                               Int -> Int -> Int -> Int -> String -> Property
prop_located_span_inequality value startLine1 startCol1 endLine1 endCol1 file1 
                               startLine2 startCol2 endLine2 endCol2 file2 = 
  not (null value) && startLine1 >= 0 && startCol1 >= 0 && endLine1 >= 0 && 
  endCol1 >= 0 && not (null file1) &&
  startLine2 >= 0 && startCol2 >= 0 && endLine2 >= 0 && endCol2 >= 0 && 
  not (null file2) &&
  (startLine1 /= startLine2 || startCol1 /= startCol2 || 
   endLine1 /= endLine2 || endCol1 /= endCol2 || file1 /= file2) ==>
  let start1 = mkSourcePos startLine1 startCol1 file1
      end1 = mkSourcePos endLine1 endCol1 file1
      span1 = mkSourceSpan start1 end1
      start2 = mkSourcePos startLine2 startCol2 file2
      end2 = mkSourcePos endLine2 endCol2 file2
      span2 = mkSourceSpan start2 end2
      located1 = mkLocated value span1
      located2 = mkLocated value span2
  in property $ located1 /= located2

-- Property 8: Located values with different values should be different
prop_located_value_inequality :: String -> String -> Int -> Int -> Int -> Int -> String -> Property
prop_located_value_inequality value1 value2 startLine startCol endLine endCol file = 
  not (null value1) && not (null value2) && value1 /= value2 &&
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 && 
  not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
      located1 = mkLocated value1 span
      located2 = mkLocated value2 span
  in property $ located1 /= located2

-- Property 9: Span start and end should be accessible
prop_span_accessors :: Int -> Int -> Int -> Int -> String -> Property
prop_span_accessors startLine startCol endLine endCol file = 
  startLine >= 0 && startCol >= 0 && endLine >= 0 && endCol >= 0 && 
  not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
  in property $ spanStart span == start && spanEnd span == end

-- Property 10: Located with span should work correctly
prop_located_with_span :: String -> Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span value startLine startCol endLine endCol file = 
  not (null value) && startLine >= 0 && startCol >= 0 && endLine >= 0 && 
  endCol >= 0 && not (null file) ==>
  let start = mkSourcePos startLine startCol file
      end = mkSourcePos endLine endCol file
      span = mkSourceSpan start end
      located = locatedWithSpan value span
  in property $ locatedValue located == value && 
               locatedSpan located == span

-- Unit tests for specific source location functionality

test_create_source_pos :: Assertion
test_create_source_pos = 
  let pos = mkSourcePos 1 2 "test.txt"
  in do
    assertEqual "sourcePosLine" 1 (sourcePosLine pos)
    assertEqual "sourcePosColumn" 2 (sourcePosColumn pos)
    assertEqual "sourcePosFile" "test.txt" (sourcePosFile pos)

test_create_source_span :: Assertion
test_create_source_span = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
  in do
    assertEqual "sourceSpanStart" start (sourceSpanStart span)
    assertEqual "sourceSpanEnd" end (sourceSpanEnd span)
    assertEqual "sourceSpanFile" "test.txt" (sourceSpanFile span)

test_create_located :: Assertion
test_create_located = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
      located = mkLocated "test value" span
  in do
    assertEqual "locatedValue" "test value" (locatedValue located)
    assertEqual "locatedSpan" span (locatedSpan located)

test_located_equality :: Assertion
test_located_equality = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
      located1 = mkLocated "test value" span
      located2 = mkLocated "test value" span
  in assertEqual "located values should be equal" located1 located2

test_source_pos_inequality :: Assertion
test_source_pos_inequality = 
  let pos1 = mkSourcePos 1 2 "test.txt"
      pos2 = mkSourcePos 1 3 "test.txt"
  in assertBool "source positions should be different" $ pos1 /= pos2

test_source_span_inequality :: Assertion
test_source_span_inequality = 
  let start1 = mkSourcePos 1 2 "test.txt"
      end1 = mkSourcePos 3 4 "test.txt"
      span1 = mkSourceSpan start1 end1
      start2 = mkSourcePos 1 2 "test.txt"
      end2 = mkSourcePos 3 5 "test.txt"
      span2 = mkSourceSpan start2 end2
  in assertBool "source spans should be different" $ span1 /= span2

test_located_span_inequality :: Assertion
test_located_span_inequality = 
  let start1 = mkSourcePos 1 2 "test.txt"
      end1 = mkSourcePos 3 4 "test.txt"
      span1 = mkSourceSpan start1 end1
      start2 = mkSourcePos 1 2 "test.txt"
      end2 = mkSourcePos 3 5 "test.txt"
      span2 = mkSourceSpan start2 end2
      located1 = mkLocated "test value" span1
      located2 = mkLocated "test value" span2
  in assertBool "located values with different spans should be different" $ located1 /= located2

test_located_value_inequality :: Assertion
test_located_value_inequality = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
      located1 = mkLocated "value1" span
      located2 = mkLocated "value2" span
  in assertBool "located values with different values should be different" $ located1 /= located2

test_span_accessors :: Assertion
test_span_accessors = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
  in do
    assertEqual "spanStart" start (spanStart span)
    assertEqual "spanEnd" end (spanEnd span)

test_located_with_span :: Assertion
test_located_with_span = 
  let start = mkSourcePos 1 2 "test.txt"
      end = mkSourcePos 3 4 "test.txt"
      span = mkSourceSpan start end
      located = locatedWithSpan "test value" span
  in do
    assertEqual "locatedValue" "test value" (locatedValue located)
    assertEqual "locatedSpan" span (locatedSpan located)

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