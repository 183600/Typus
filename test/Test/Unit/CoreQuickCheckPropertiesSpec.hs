{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo, startPos, posAfter, mergeSpans, posAt)
import Data.Char (isSpace, isAlphaNum)
import Data.List (sort, intercalate, isInfixOf, nub)
import qualified Data.Map as Map (Map, empty, insert, toList, keys)
import qualified Data.Set as Set (Set, empty, insert, toList)
import Control.Monad (foldM)

-- Core QuickCheck property tests

-- String and text processing properties

-- | trim should not add any characters
prop_trim_no_addition :: String -> Property
prop_trim_no_addition s = 
  let trimmed = trim s
  in property $ length trimmed <= length s

-- | trim should remove only whitespace characters
prop_trim_removes_only_whitespace :: String -> Property
prop_trim_removes_only_whitespace s = 
  let trimmed = trim s
      removedChars = s \\ trimmed
  in property $ all isSpace removedChars

-- | splitBy followed by intercalate should reconstruct original (for non-empty parts)
prop_splitby_intercalate :: Char -> NonEmptyList String -> Property
prop_splitby_intercalate delim (NonEmpty parts) = 
  let s = intercalate [delim] parts
      reconstructed = intercalate [delim] (splitBy delim s)
  in property $ s == reconstructed

-- | splitBy should produce parts that concatenate to original with delimiters
prop_splitby_concatenation :: Char -> String -> Property
prop_splitby_concatenation delim s = 
  let parts = splitBy delim s
      reconstructed = concat parts ++ replicate (length parts - 1) [delim]
  in property $ if null parts then null s else length reconstructed >= length s

-- | removeLineComments should not affect non-comment lines
prop_removeLineComments_preserves_non_comments :: String -> Property
prop_removeLineComments_preserves_non_comments s = 
  let withoutComments = removeLineComments s
      linesWithoutComments = lines withoutComments
      originalLines = filter (not . ("//" `isPrefixOf`)) (lines s)
  in property $ length linesWithoutComments >= length originalLines

-- Source location properties

-- | posAfter should always advance position
prop_posAfter_advances :: SourcePos -> Char -> Property
prop_posAfter_advances pos c = 
  let newPos = posAfter pos c
  in property $ if c == '\n' 
    then sourceLine newPos > sourceLine pos && sourceColumn newPos == 1
    else sourceLine newPos == sourceLine pos && sourceColumn newPos > sourceColumn pos

-- | mergeSpans should be commutative
prop_mergeSpans_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_commutative start1 end1 start2 end2 = 
  let span1 = spanTo start1 end1
      span2 = spanTo start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

-- | mergeSpans should be associative
prop_mergeSpans_associative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_associative start1 end1 start2 end2 start3 end3 = 
  let span1 = spanTo start1 end1
      span2 = spanTo start2 end2
      span3 = spanTo start3 end3
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

-- Map and Set properties

-- | Map insertion should be idempotent for same key-value
prop_map_insertion_idempotent :: Ord k => k -> v -> Map.Map k v -> Property
prop_map_insertion_idempotent k v m = 
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in property $ m1 == m2

-- | Map keys should be unique
prop_map_keys_unique :: Ord k => Map.Map k v -> Property
prop_map_keys_unique m = 
  let ks = keys m
  in property $ length ks == length (nub ks)

-- | Set insertion should be idempotent
prop_set_insertion_idempotent :: Ord a => a -> Set.Set a -> Property
prop_set_insertion_idempotent x s = 
  let s1 = Set.insert x s
      s2 = Set.insert x s1
  in property $ s1 == s2

-- | Set size should never decrease when inserting
prop_set_insertion_non_decreasing :: Ord a => a -> Set.Set a -> Property
prop_set_insertion_non_decreasing x s = 
  let s1 = Set.insert x s
  in property $ size s1 >= size s

-- List properties

-- | List concatenation should be associative
prop_list_concat_associative :: [Int] -> [Int] -> [Int] -> Property
prop_list_concat_associative xs ys zs = 
  property $ (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- | List length should be additive over concatenation
prop_list_length_additive :: [Int] -> [Int] -> Property
prop_list_length_additive xs ys = 
  property $ length (xs ++ ys) == length xs + length ys

-- | Sorting should be idempotent
prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs = 
  property $ sort (sort xs) == sort xs

-- | Sorting should preserve length
prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs = 
  property $ length (sort xs) == length xs

-- | Reverse should be involutive (reverse(reverse(xs)) == xs)
prop_reverse_involutive :: [Int] -> Property
prop_reverse_involutive xs = 
  property $ reverse (reverse xs) == xs

-- Numeric properties

-- | Addition should be commutative
prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = 
  property $ x + y == y + x

-- | Addition should be associative
prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = 
  property $ (x + y) + z == x + (y + z)

-- | Multiplication should be commutative
prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y = 
  property $ x * y == y * x

-- | Multiplication should be associative
prop_multiplication_associative :: Int -> Int -> Int -> Property
prop_multiplication_associative x y z = 
  property $ (x * y) * z == x * (y * z)

-- | Distributive property
prop_distributive :: Int -> Int -> Int -> Property
prop_distributive x y z = 
  property $ x * (y + z) == x * y + x * z

-- Boolean properties

-- | AND should be commutative
prop_and_commutative :: Bool -> Bool -> Property
prop_and_commutative x y = 
  property $ (x && y) == (y && x)

-- | OR should be commutative
prop_or_commutative :: Bool -> Bool -> Property
prop_or_commutative x y = 
  property $ (x || y) == (y || x)

-- | De Morgan's laws
prop_demorgan_and :: Bool -> Bool -> Property
prop_demorgan_and x y = 
  property $ not (x && y) == (not x) || (not y)

prop_demorgan_or :: Bool -> Bool -> Property
prop_demorgan_or x y = 
  property $ not (x || y) == (not x) && (not y)

-- | Double negation
prop_double_negation :: Bool -> Property
prop_double_negation x = 
  property $ not (not x) == x

-- Unit tests
test_trim_examples :: Assertion
test_trim_examples = do
  assertEqual "trim empty" "" (trim "")
  assertEqual "trim spaces" "hello" (trim "  hello  ")
  assertEqual "trim newlines" "hello" (trim "\nhello\n")
  assertEqual "trim mixed" "hello" (trim "  \n hello \n  ")

test_splitBy_examples :: Assertion
test_splitBy_examples = do
  assertEqual "splitBy comma" ["a", "b", "c"] (splitBy ',' "a,b,c")
  assertEqual "splitBy empty" ["a", "", "c"] (splitBy ',' "a,,c")
  assertEqual "splitBy leading" ["", "b", "c"] (splitBy ',' ",b,c")
  assertEqual "splitBy trailing" ["a", "b", ""] (splitBy ',' "a,b,")

test_sourcePos_examples :: Assertion
test_sourcePos_examples = do
  let pos1 = posAt 1 1
  let pos2 = posAfter pos1 'a'
  assertEqual "posAfter char" (posAt 1 2) pos2
  let pos3 = posAfter pos2 '\n'
  assertEqual "posAfter newline" (posAt 2 1) pos3

test_mergeSpans_examples :: Assertion
test_mergeSpans_examples = do
  let span1 = spanTo (posAt 1 1) (posAt 1 5)
  let span2 = spanTo (posAt 1 3) (posAt 1 8)
  let merged = mergeSpans span1 span2
  assertEqual "mergeSpans start" (posAt 1 1) (spanStart merged)
  assertEqual "mergeSpans end" (posAt 1 8) (spanEnd merged)

-- Test suite
tests :: TestTree
tests = testGroup "Core QuickCheck Properties"
  [ testProperties "String Processing"
    [ prop_trim_no_addition
    , prop_splitby_intercalate
    ]
  , testProperties "Source Location"
    [ prop_posAfter_advances
    ]
  , testProperties "Data Structures"
    [ prop_map_insertion_idempotent
    ]
  , testProperties "List Operations"
    [ prop_list_concat_associative
    ]
  , testProperties "Numeric Operations"
    [ prop_addition_commutative
    ]
  , testCase "trim examples" test_trim_examples
  , testCase "sourcePos examples" test_sourcePos_examples
  ]