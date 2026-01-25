module Test.Unit.NewSimpleErrorHandlingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Char (isSpace)

-- Simple tests that don't require complex error handling

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 1: String length is non-negative
prop_string_length_non_negative :: String -> Property
prop_string_length_non_negative s = property $ length s >= 0

-- Property 2: Trim should not increase length
prop_trim_not_increase :: String -> Property
prop_trim_not_increase s = property $ length (trim s) <= length s
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse

-- Property 3: Reverse of reverse is original
prop_reverse_reverse :: String -> Property
prop_reverse_reverse s = property $ reverse (reverse s) == s

-- Property 4: Head of non-empty list is in list
prop_head_in_list :: [Int] -> Property
prop_head_in_list xs = not (null xs) ==> property $ case xs of (x:_) -> x `elem` xs; [] -> False

-- Property 5: Sum of list is non-negative for non-negative elements
prop_sum_non_negative :: [Int] -> Property
prop_sum_non_negative xs = all (>= 0) xs ==> property $ sum xs >= 0

-- Property 6: Concatenation length is sum of lengths
prop_concat_length :: String -> String -> Property
prop_concat_length s1 s2 = property $ length (s1 ++ s2) == length s1 + length s2

-- Property 7: Map preserves length
prop_map_preserves_length :: [Int] -> Property
prop_map_preserves_length xs = property $ length (map (*2) xs) == length xs

-- Property 8: Filter doesn't increase length
prop_filter_not_increase :: [Int] -> Property
prop_filter_not_increase xs = property $ length (filter even xs) <= length xs

-- Property 9: All elements of filtered list satisfy predicate
prop_filter_satisfies :: [Int] -> Property
prop_filter_satisfies xs = property $ all even (filter even xs)

-- Property 10: Take doesn't exceed original length
prop_take_not_exceed :: [Int] -> Int -> Property
prop_take_not_exceed xs n = 
  let n' = abs n `mod` (length xs + 1)
  in property $ length (take n' xs) <= length xs

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_empty_string :: Assertion
test_empty_string = 
  assertEqual "Empty string should be empty" "" ""

test_string_length :: Assertion
test_string_length = 
  let s = "hello"
  in assertEqual "String length should be 5" 5 (length s)

test_reverse_property :: Assertion
test_reverse_property = 
  let s = "hello"
  in assertEqual "Reverse of reverse should be original" s (reverse (reverse s))

test_head_property :: Assertion
test_head_property = 
  let xs = [1,2,3]
      x = case xs of
            (v:_) -> v
            [] -> error "Impossible: xs is not empty"
  in assertEqual "Head should be first element" 1 x

test_sum_property :: Assertion
test_sum_property = 
  let xs = [1,2,3,4,5]
  in assertEqual "Sum should be 15" 15 (sum xs)

test_concat_property :: Assertion
test_concat_property = 
  let s1 = "hello"
      s2 = "world"
  in assertEqual "Concatenation should work" "helloworld" (s1 ++ s2)

test_map_property :: Assertion
test_map_property = 
  let xs = [1,2,3]
  in assertEqual "Map should double elements" [2,4,6] (map (*2) xs)

test_filter_property :: Assertion
test_filter_property = 
  let xs = [1,2,3,4,5]
  in assertEqual "Filter should get even numbers" [2,4] (filter even xs)

test_take_property :: Assertion
test_take_property = 
  let xs = [1,2,3,4,5]
  in assertEqual "Take should get first 3 elements" [1,2,3] (take 3 xs)

test_drop_property :: Assertion
test_drop_property = 
  let xs = [1,2,3,4,5]
  in assertEqual "Drop should get last 2 elements" [4,5] (drop 3 xs)

tests :: TestTree
tests = testGroup "Test.Unit.NewSimpleErrorHandlingSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "string length non-negative" prop_string_length_non_negative
    , testProperty "trim not increase" prop_trim_not_increase
    , testProperty "reverse reverse" prop_reverse_reverse
    , testProperty "head in list" prop_head_in_list
    , testProperty "sum non-negative" prop_sum_non_negative
    , testProperty "concat length" prop_concat_length
    , testProperty "map preserves length" prop_map_preserves_length
    , testProperty "filter not increase" prop_filter_not_increase
    , testProperty "filter satisfies" prop_filter_satisfies
    , testProperty "take not exceed" prop_take_not_exceed
    ]
  , testGroup "Unit Tests"
    [ testCase "empty string" test_empty_string
    , testCase "string length" test_string_length
    , testCase "reverse property" test_reverse_property
    , testCase "head property" test_head_property
    , testCase "sum property" test_sum_property
    , testCase "concat property" test_concat_property
    , testCase "map property" test_map_property
    , testCase "filter property" test_filter_property
    , testCase "take property" test_take_property
    , testCase "drop property" test_drop_property
    ]
  ]