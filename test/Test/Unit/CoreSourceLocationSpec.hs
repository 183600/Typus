{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing  -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CoreSourceLocationSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
  ( SourcePos(..), posAt
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
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
  , mapLocated
  , posLine
  , posColumn
  )
import Data.Char (isSpace)

-- Test properties for SourceLocation module

-- | startPos should have line 1 and column 1
prop_startPos_values :: Property
prop_startPos_values = 
  let pos = startPos
  in property $ posLine pos == 1 && posColumn pos == 1

-- | posAfter should increment column by 1 for basic characters
prop_posAfter_basic_column :: Positive Int -> Property
prop_posAfter_basic_column (Positive n) = 
  let pos = posAt 1 n
      pos' = posAfter 'a' pos
  in property $ posColumn pos' == n + 1

-- | posAfter should move to next line for newline character
prop_posAfter_newline :: Positive Int -> Property
prop_posAfter_newline (Positive line) = 
  let pos = posAt line 5
      pos' = posAfter '\n' pos
  in property $ posLine pos' == line + 1 && posColumn pos' == 1

-- | emptySpan should have start and end at the same position
prop_emptySpan_consistency :: Property
prop_emptySpan_consistency = 
  let span = emptySpan startPos
  in property $ spanStart span == spanEnd span

-- | spanFrom should create a span with given start and end at same position
prop_spanFrom_consistency :: Positive Int -> Positive Int -> Property
prop_spanFrom_consistency (Positive line) (Positive col) = 
  let pos = posAt line col
      span = spanFrom pos
  in property $ spanStart span == pos && spanEnd span == pos

-- | spanTo should create a span with given start and end
prop_spanTo_consistency :: Positive Int -> Positive Int -> Positive Int -> Property
prop_spanTo_consistency (Positive line1) (Positive col1) (Positive len) = 
  let start = posAt line1 col1
      end = posAt line1 (col1 + len)
      span = spanBetween start end
  in property $ spanStart span == start && spanEnd span == end

-- | mergeSpans should create a span that encompasses both input spans
prop_mergeSpans_encompassing :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_mergeSpans_encompassing (Positive line1) (Positive col1) (Positive line2) (Positive col2) = 
  let start1 = posAt line1 col1
      end1 = posAt line1 (col1 + 5)
      start2 = posAt line2 col2
      end2 = posAt line2 (col2 + 5)
      span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in property $ spanStart merged `isBeforeOrEqual` spanStart span1 && 
                spanEnd merged `isAfterOrEqual` spanEnd span2
  where
    isBeforeOrEqual p1 p2 = posLine p1 < posLine p2 || 
                           (posLine p1 == posLine p2 && posColumn p1 <= posColumn p2)
    isAfterOrEqual p1 p2 = posLine p1 > posLine p2 || 
                          (posLine p1 == posLine p2 && posColumn p1 >= posColumn p2)

-- | isValidSpan should return True for spans with valid start and end
prop_isValidSpan_valid :: Positive Int -> Positive Int -> Positive Int -> Property
prop_isValidSpan_valid (Positive line) (Positive col1) (Positive col2) = 
  let start = posAt line col1
      end = posAt line (col1 + col2)
      span = spanBetween start end
  in property $ isValidSpan span

-- | locatedAt should create a Located value with empty span at given position
prop_locatedAt_consistency :: Positive Int -> Positive Int -> String -> Property
prop_locatedAt_consistency (Positive line) (Positive col) value = 
  let pos = posAt line col
      located = locatedAt pos value
  in property $ locatedValue located == value && 
                spanStart (locatedSpan located) == pos &&
                spanEnd (locatedSpan located) == pos

-- | mapLocated should apply function to the value while preserving location
prop_mapLocated_preserves_location :: Positive Int -> Positive Int -> String -> Property
prop_mapLocated_preserves_location (Positive line) (Positive col) value = 
  let pos = posAt line col
      located = locatedAt pos value
      mapped = mapLocated length located
  in property $ locatedValue located == value &&
                locatedValue mapped == length value &&
                locatedSpan located == locatedSpan mapped

-- Unit tests
test_startPos :: Assertion
test_startPos = assertEqual "startPos values" (posAt 1 1) startPos

test_posAfter_char :: Assertion
test_posAfter_char = assertEqual "posAfter char" (posAt 1 2) (posAfter 'a' (posAt 1 1))

test_posAfter_newline :: Assertion
test_posAfter_newline = assertEqual "posAfter newline" (posAt 2 1) (posAfter '\n' (posAt 1 5))

test_posAfter_tab :: Assertion
test_posAfter_tab = assertEqual "posAfter tab" (posAt 1 5) (posAfter '\t' (posAt 1 1))

test_emptySpan :: Assertion
test_emptySpan = do
  let span = emptySpan startPos
  assertEqual "emptySpan start" startPos (spanStart span)
  assertEqual "emptySpan end" startPos (spanEnd span)

test_spanFrom :: Assertion
test_spanFrom = do
  let pos = SourcePos 3 5 0
  let span = spanFrom pos
  assertEqual "spanFrom start" pos (spanStart span)
  assertEqual "spanFrom end" pos (spanEnd span)

test_spanTo :: Assertion
test_spanTo = do
  let start = posAt 2 3
  let end = posAt 2 8
  let span = spanBetween start end
  assertEqual "spanTo start" start (spanStart span)
  assertEqual "spanTo end" end (spanEnd span)

test_mergeSpans :: Assertion
test_mergeSpans = do
  let start1 = posAt 1 1
  let end1 = posAt 1 5
  let start2 = posAt 1 3
  let end2 = posAt 1 8
  let span1 = spanBetween start1 end1
  let span2 = spanBetween start2 end2
  let merged = mergeSpans span1 span2
  assertEqual "mergeSpans start" start1 (spanStart merged)
  assertEqual "mergeSpans end" end2 (spanEnd merged)

test_isValidSpan :: Assertion
test_isValidSpan = do
  let validSpan = spanBetween (posAt 1 1) (posAt 1 5)
  assertEqual "valid span" True (isValidSpan validSpan)

test_locatedAt :: Assertion
test_locatedAt = do
  let pos = posAt 2 4
  let value = "test"
  let located = locatedAt pos value
  assertEqual "locatedAt value" value (locatedValue located)
  assertEqual "locatedAt span start" pos (spanStart (locatedSpan located))
  assertEqual "locatedAt span end" pos (spanEnd (locatedSpan located))

test_locatedWithSpan :: Assertion
test_locatedWithSpan = do
  let span = spanBetween (posAt 1 1) (posAt 1 5)
  let value = "test"
  let located = locatedWithSpan span value
  assertEqual "locatedWithSpan value" value (locatedValue located)
  assertEqual "locatedWithSpan span" span (locatedSpan located)

test_mapLocated :: Assertion
test_mapLocated = do
  let pos = posAt 3 2
  let value = "hello"
  let located = locatedAt pos value
  let mapped = mapLocated reverse located
  assertEqual "mapLocated value" "olleh" (locatedValue mapped)
  assertEqual "mapLocated span" (locatedSpan located) (locatedSpan mapped)

-- Test suite
tests :: TestTree
tests = testGroup "Core SourceLocation Tests"
  [ testProperties "QuickCheck Properties"
    [ ("startPos_values", prop_startPos_values)
    , ("posAfter_basic_column", property $ prop_posAfter_basic_column (Positive 5))
    , ("posAfter_newline", property $ prop_posAfter_newline (Positive 5))
    , ("mergeSpans_encompassing", property $ prop_mergeSpans_encompassing (Positive 1) (Positive 5) (Positive 2) (Positive 10))
    ]
  , testCase "startPos" test_startPos
  , testCase "posAfter char" test_posAfter_char
  , testCase "posAfter newline" test_posAfter_newline
  , testCase "emptySpan" test_emptySpan
  , testCase "mergeSpans" test_mergeSpans
  , testCase "locatedAt" test_locatedAt
  ]